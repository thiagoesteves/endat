%%%-------------------------------------------------------------------
%%% Created : 3 Jan 2020 by Thiago Esteves <thiagocalori@gmail.com>
%%%
%%% @doc
%%% This file contains the crc function calculation using NIFS because
%%% the c code is more efficient.
%%% @end
%%%-------------------------------------------------------------------

-module(endat_crc).

-author('Thiago Esteves').

%% NIF definition
-on_load(init/0).

%% ENDAT CRC API (using lookup table)
-export([makeCrcNormLt/2, makeCrcPosLt/6]).

%% ENDAT CRC API (using heidenhain functions)
-export([makeCrcNorm/2, makeCrcPos/6]).

%% Auxiliar functions
-export([reverse32Bits/1, reverse64Bits/1]).

%%--------------------------------------------------------------------
%% DEFINITIONS
%%--------------------------------------------------------------------

-define(APPNAME, endat).

%%--------------------------------------------------------------------
%% PUBLIC FUNCTIONS IMPLEMENTATION
%%
%% Access endat_crc.c and endat_nif.c for more details
%%--------------------------------------------------------------------

makeCrcNormLt(_, _) ->
  not_loaded(?LINE).

makeCrcPosLt(_, _, _, _, _, _) ->
  not_loaded(?LINE).

makeCrcNorm(_, _) ->
  not_loaded(?LINE).

makeCrcPos(_, _, _, _, _, _) ->
  not_loaded(?LINE).

reverse32Bits(_) ->
  not_loaded(?LINE).

reverse64Bits(_) ->
  not_loaded(?LINE).

%%%===================================================================
%%% Internal functions
%%%===================================================================

init() ->
  SoDir = code:priv_dir(?APPNAME),
  erlang:load_nif(filename:join(SoDir, ?MODULE), 0).

%% This function shouldn't ever be called unless there was an error while
%% loading the NIF shared library.
not_loaded(Line) ->
  exit({not_loaded, [{module, ?MODULE}, {line, Line}]}).








