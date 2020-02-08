%%%-------------------------------------------------------------------
%%% Created : 3 Jan 2020 by Thiago Esteves <thiagocalori@gmail.com>
%%%
%%% @doc
%%% This is the ENDAT driver implementation and it is responsible to
%%% access the ENDAT low level functions using Port.
%%% @end
%%%-------------------------------------------------------------------

-module(endat_driver).

-behaviour(gen_server).

-author('Thiago Esteves').

%% gen_server exports
-export([init/1,
         start_link/0,
         terminate/2,
         handle_cast/2,
         handle_info/2,
         handle_call/3,
         code_change/3]).

%% ENDAT Driver API
-export([write_command/3,
         read_position/4]).

%% Endat Start/Stop to read position continuously
-export([start_read_position/3,
         stop_read_position/1]).

%% ENDAT TEST API, these functions won't be used by the Endat gen_server
%% but can be used to test and compare the calculation when using
%% Port (endat_driver) and NIFs (endat_crc).
-export([makeCrcNorm/2,
         makeCrcNormLt/2,
         makeCrcPos/6,
         makeCrcPosLt/6]).

%%%===================================================================
%%% Defines
%%%===================================================================

-define(SERVER,  ?MODULE).
-define(TIMEOUT, 5000). % in milliseconds

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
init([]) ->
  process_flag(trap_exit, true),
  Port = open_port(),
  {ok, _} = talk_to_port(Port , {open_endat_driver}),
  {ok, Port}.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
terminate(_, Port) ->
  talk_to_port(Port, {close_endat_driver}).

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
handle_info({Port, {data,Msg}}, Port) ->
  io:format("\n\r ~p", [binary_to_term(Msg)]),
  {noreply, Port};

handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
handle_call(Msg, _From, Port) ->
  Res = talk_to_port(Port , Msg),
  {reply, Res, Port}.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
open_port() ->
    E = filename:join([code:priv_dir(endat), "endat"]),
    open_port({spawn, E},[{packet, 2}, binary, exit_status]).

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
talk_to_port(Port,Msg) ->
    try
        erlang:port_command(Port, term_to_binary(Msg)),
        receive
            {Port, {data, D = <<131, 104, 2, _/binary>>}} ->
                binary_to_term(D)
        after ?TIMEOUT ->
                throw(talk_to_port_timeout)
        end
    catch
        _:R ->
            throw({talking_to_port_failed, {R, Port, Msg}})
    end.

%%%===================================================================
%%% Public API
%%%===================================================================

read_position(Instance, EndatVersion, Command, PositionBits) ->
  gen_server:call(?MODULE, {read_position, Instance, EndatVersion, Command, PositionBits}).

write_command(Instance, Command, Timeout) ->
  gen_server:call(?MODULE, {write_command, Instance, Command, Timeout}).

start_read_position(Instance, EndatVersion, PositionBits) ->
  gen_server:call(?MODULE, {start_read_position, Instance, EndatVersion, PositionBits}).

stop_read_position(Instance) ->
  gen_server:call(?MODULE, {stop_read_position, Instance}).

%%%===================================================================
%%% Public TEST API
%%%===================================================================

makeCrcNorm(Param8, Param16) ->
  gen_server:call(?MODULE, {makeCrcNorm, Param8, Param16}).

makeCrcNormLt(Param8, Param16) ->
  gen_server:call(?MODULE, {makeCrcNormLt, Param8, Param16}).

makeCrcPos(Clocks, Endat, Error1, Error2, Highpos, Lowpos) ->
  gen_server:call(?MODULE, {makeCrcPos, Clocks, Endat, Error1, Error2, Highpos, Lowpos}).

makeCrcPosLt(Clocks, Endat, Error1, Error2, Highpos, Lowpos) ->
  gen_server:call(?MODULE, {makeCrcPosLt, Clocks, Endat, Error1, Error2, Highpos, Lowpos}).

