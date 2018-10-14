%% Copyright 2018, Noel Cower <ncower@gmail.com>.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(erespsv_proto).

-behaviour(ranch_protocol).

-export([start_link/4]).
-export([init/3]).

-export_type([db_ref/0]).

-type db_ref() :: {Module :: atom(), Ref :: any()}.

-record(state, {
          db :: db_ref(),
          % Packet decoder
          decoder :: erespsv_msg:decoder(),
          buffer = <<>> :: binary()
         }).

start_link(Ref, _, Transport, Opts) ->
    Pid = spawn_link(?MODULE, init, [Ref, Transport, Opts]),
    {ok, Pid}.

init(Ref, Transport, Opts) ->
    DB = maps:get(db, Opts),
    State = #state{
               db = DB,
               decoder = erespsv_msg:new()
              },
    {ok, Socket} = ranch:handshake(Ref),
    loop(Socket, Transport, State).

loop(Socket, Transport, State0) ->
    case Transport:recv(Socket, 0, 5000) of
        {ok, Packet} ->
            handle_packet(Socket, Transport, State0, Packet);
        {error, closed} ->
            ok;
        {error, timeout} ->
            loop(Socket, Transport, State0);
        {error, Reason} ->
            lager:warn("RECV error => ~p", [Reason]),
            Transport:close(Socket)
    end.

handle_packet(Socket, Transport, State0, Packet) ->
    {ok, Dec} = erespsv_msg:decode(State0#state.decoder, Packet),
    State1 = State0#state{decoder = Dec},
    DecResult = erespsv_msg:finalize(Dec),
    handle_decoder(Socket, Transport, State1, DecResult).

handle_decoder(Socket, Transport, State0, {ok, Term, Rest}) ->
    lager:debug("MSG ~p", [Term]),
    State1 = State0#state{
               decoder = erespsv_msg:new(),
               buffer = Rest
              },
    {Cmd, Args} = format(Term),
    Rep = reply(State1, Cmd, Args),
    handle_response(Socket, Transport, Rep);
handle_decoder(Socket, Transport, State0, {error, invalid, _}) ->
    % Invalid protocol encountered -- kill the connection.
    handle_response(Socket, Transport, {close, {error, {'PROTO', <<"invalid message">>}}, State0});
handle_decoder(Socket, Transport, State0, {error, incomplete}) ->
    % Need more data to parse a message
    loop(Socket, Transport, State0).

handle_response(Socket, Transport, {ok, Msg, State}) ->
    {ok, Rep} = eresp:encode_server(Msg),
    Transport:send(Socket, Rep),
    loop(Socket, Transport, State);
handle_response(Socket, Transport, {close, Msg, _}) ->
    {ok, Rep} = eresp:encode_server(Msg),
    Transport:send(Socket, Rep),
    Transport:close(Socket),
    ok;
handle_response(Socket, Transport, {error, Reason}) ->
    lager:error("RESP Error => ~p", [Reason]),
    Transport:close(Socket),
    ok.

% Casefold any command names so they can be handled easily.
format([]) ->
    [];
format([Cmd | Args]) ->
    {string:casefold(Cmd), Args}.

% Reply to a message -- returns unencoded responses.
% Clients can respond to a handful of messages on their own without consulting
% a DB -- they're pretty simple.
reply(State, <<"ping">>, []) ->
    {ok, 'PONG', State};
reply(State, <<"ping">>, [Pong]) when is_binary(Pong) ->
    {ok, Pong, State};
reply(State, <<"echo">>, Args) ->
    {ok, Args, State};
reply(State, <<"quit">>, []) ->
    {close, ok, State};
reply(#state{db = DB} = State, Cmd, Args) when is_binary(Cmd) ->
    % Forward to DB
    case call_db(DB, Cmd, Args) of
        {ok, Term} -> {ok, Term, State};
        Error -> Error
    end.

call_db({DB, Ref}, Cmd, Args) ->
    DB:handle_request(Ref, Cmd, Args).
