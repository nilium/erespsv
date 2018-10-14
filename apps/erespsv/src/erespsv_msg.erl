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

-module(erespsv_msg).

% TODO(ncower): Port iterative decoder to eresp, including support for the full
% set of decodable message types.

-export([new/0, decode/2, finalize/1]).

-export_type([decoder/0]).

-record(state, {
          mode   = init :: atom(),
          buffer = <<>> :: binary(),
          n      = 0    :: integer(),
          term   = []   :: 'nil' | [binary(), ...]
         }).

-type decoder() :: #state{}.

-spec new() -> decoder().
new() ->
    #state{}.

-spec decode(decoder(), binary()) -> {ok, decoder()}.
decode(#state{
          mode = init,
          buffer = Buf0
         } = State0, Bin) ->
    Buf1 = concat(Buf0, Bin),
    State1 = State0#state{buffer = Buf1},
    case Buf1 of
        <<>> ->
            {ok, State1};
        <<"*">> ->
            {ok, State1};
        <<"*", Rest0/binary>> ->
            case to_integer(Rest0) of
                {error, _} ->
                    {ok, State1#state{mode = invalid}};
                {N, <<"\r", _/binary>> = Rest} ->
                    Next = {crlf, readbulk},
                    State2 = State1#state{mode = Next, n = N, buffer = Rest},
                    decode(State2, <<>>);
                {_, _} ->
                    {ok, State1}
            end;
        <<"PING\r\n", Rest/binary>> ->
            {ok, State1#state{
                   mode = done,
                   term = [<<"PING">>],
                   buffer = Rest
                  }};
        _ ->
            {ok, State1#state{mode = invalid}}
    end;

decode(#state{
          mode = {crlf, Next},
          buffer = Buf0
         } = State0, Bin) ->
    Buf1 = concat(Buf0, Bin),
    case Buf1 of
        <<"\r\n", Rest/binary>> ->
            State1 = State0#state{mode = Next, buffer = Rest},
            decode(State1, <<>>);
        Bin1 when byte_size(Bin1) > 2 ->
            State1 = State0#state{mode = invalid, buffer = Buf1},
            {ok, State1};
        _ ->
            State1 = State0#state{buffer = Buf1},
            {ok, State1}
    end;

decode(#state{
          mode = readbulk,
          buffer = Buf0,
          term = Term,
          n = 0
         } = State0, Bin) ->
    Buf1 = concat(Buf0, Bin),
    {ok, State0#state{
           mode = done,
           term = lists:reverse(Term),
           buffer = Buf1
          }};

decode(#state{
          mode = readbulk,
          buffer = Buf0,
          n = -1
         } = State0, Bin) ->
    Buf1 = concat(Buf0, Bin),
    {ok, State0#state{
           mode = done,
           term = nil,
           buffer = Buf1
          }};

decode(#state{
          mode = readbulk,
          buffer = Buf0,
          n = N
         } = State0, Bin)
  when N < -1 ->
    Buf1 = concat(Buf0, Bin),
    {ok, State0#state{
           mode = invalid,
           buffer = Buf1
          }};

decode(#state{
          mode = readbulk,
          buffer = Buf0
         } = State0, Bin) ->
    Buf1 = concat(Buf0, Bin),
    State1 = State0#state{buffer = Buf1},
    case Buf1 of
        <<>> ->
            {ok, State1};
        <<"$">> ->
            {ok, State1};
        <<"$", Rest0/binary>> ->
            case to_integer(Rest0) of
                {error, _} ->
                    {ok, State1#state{mode = invalid}};
                {BN, <<"\r", _/binary>> = Rest} ->
                    Next = {crlf, {bulk, BN}},
                    State2 = State1#state{
                               mode = Next,
                               buffer = Rest
                              },
                    decode(State2, <<>>);
                {_, _} ->
                    {ok, State1}
            end;
        _ ->
            {ok, State1#state{mode = invalid}}
    end;

decode(#state{
          mode = {bulk, 0},
          n = N,
          term = Term
         } = State0, Bin) ->
    State1 = State0#state{
               mode = {crlf, readbulk},
               n = N - 1,
               term = [<<>> | Term]
              },
    decode(State1, Bin);

decode(#state{
          mode = {bulk, BN},
          buffer = Buf0,
          n = N,
          term = Term
         } = State0, Bin)
  when N > 0 ->
    case concat(Buf0, Bin) of
        <<Bulk:BN/binary, Rest/binary>> ->
            State1 = State0#state{
                       mode = {crlf, readbulk},
                       n = N - 1,
                       term = [Bulk | Term],
                       buffer = Rest
                      },
            decode(State1, <<>>);
        Rest ->
            {ok, State0#state{buffer = Rest}}
    end;


decode(#state{mode = done}, _) ->
    {error, complete};

decode(#state{mode = invalid}, _) ->
    {error, invalid}.

finalize(#state{mode = done, buffer = Buf0, term = Term}) ->
    {ok, Term, Buf0};
finalize(#state{mode = invalid, buffer = Buf0}) ->
    {error, invalid, Buf0};
finalize(_) ->
    {error, incomplete}.

to_integer(<<$-, Rest/binary>>) ->
    to_integer(Rest, -1, 0, 0);
to_integer(Bin) ->
    to_integer(Bin, 1, 0, 0).

to_integer(<<C:8, Rest/binary>>, Sign, Len, N) when C >= $0, C =< $9 ->
    to_integer(Rest, Sign, Len + 1, N * 10 + (C - $0));
to_integer(_, _, 0, _) ->
    {error, no_integer};
to_integer(Rest, Sign, _, N) ->
    {N * Sign, Rest}.

concat(BinA, <<>>) -> BinA;
concat(<<>>, BinB) -> BinB;
concat(BinA, BinB) -> <<BinA/binary, BinB/binary>>.
