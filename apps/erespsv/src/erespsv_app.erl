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

-module(erespsv_app).

-behaviour(application).

-export([start/2, stop/1]).

-record(state, {
          ranch_ref :: any()
         }).

%% API

start(_StartType, _StartArgs) ->
    {ok, Ref} = start_listener(),
    {ok, SupPid} = erespsv_sup:start_link(),
    {ok, SupPid, #state{ranch_ref = Ref}}.

stop(#state{ranch_ref = Ref}) ->
    ok = ranch:stop_listener(Ref),
    lager:info("Listener stopped"),
    ok.

%% Internal

start_listener() ->
    Ref = ref(),
    {Transport, TransportOpts} = transport(),
    {Proto, ProtoOpts} = protocol(),
    {ok, _} = ranch:start_listener(Ref, Transport, TransportOpts, Proto, ProtoOpts),
    log_start(Ref),
    {ok, Ref}.

ref() ->
    erespsv_tcp.

transport() ->
    {ranch_tcp,
     #{
       num_acceptors => 4,
       keepalive => true,
       socket_opts => [{port, 6379}]
      }}.

protocol() ->
    DB = {erespsv_db, 'unimplemented'},
    {erespsv_proto,
     #{
       db => DB
      }}.

log_start(Ref) ->
    {Addr, Port} = ranch:get_addr(Ref),
    AddrStr = inet:ntoa(Addr),
    lager:info("Listening on ~s:~b", [AddrStr, Port]).
