%%%-------------------------------------------------------------------
%%% Created : 3 Jan 2020 by Thiago Esteves <thiagocalori@gmail.com>
%%%
%%% @doc
%%% This is the endat implementation as a gen_server. All the high-level
%%% functions are located here and the low level functions (i2c read 
%%% and gpio read) are accessed by PORT.
%%%
%%% This gen-server supports multiple devices connected and can be
%%% dynamically created/removed by the endat_sup.
%%% @end
%%%-------------------------------------------------------------------

-module(endat).

-behaviour(gen_server).

-author('Thiago Esteves').

-include("endat.hrl").

%% gen_server exports
-export([init/1,
         start_link/1,
         terminate/2,
         handle_cast/2,
         handle_info/2,
         handle_call/3,
         code_change/3]).

%% Public API export where the Instance is required
-export([get_state/1,
         get_clocks/1,
         set_reset/1]).

%% Public API export where the Instance is the default defined value
-export([get_state/0,
         get_clocks/0,
         set_reset/0]).

-compile([export_all]).

%%%===================================================================
%%% Global Defines
%%%===================================================================

-define(DEFAULT_INSTANCE, 0).

%%%===================================================================
%%% Mode Defines (6-bits)
%%%===================================================================

-define(MODE_ENCODER_SEND_POS,   2#000111).
-define(MODE_SELECT_MEMORY,      2#001110).
-define(MODE_ENCODER_RCV_PARAM,  2#011100).
-define(MODE_ENCODER_SEND_PARAM, 2#100011).
-define(MODE_ENCODER_RCV_RESET,  2#101010).

%%%===================================================================
%%% MRS Defines (8-bits)
%%%===================================================================

-define(MRS_OPERATION,       2#10111001).
-define(MRS_PARAM_MANUF_1,   2#10100001).
-define(MRS_PARAM_MANUF_2,   2#10100011).
-define(MRS_PARAM_MANUF_3,   2#10100101).
-define(MRS_OPERATING_PARAM, 2#10100111).
-define(MRS_OEM_PARAM_1,     2#10101001).
-define(MRS_OEM_PARAM_2,     2#10101011).
-define(MRS_OEM_PARAM_3,     2#10101101).
-define(MRS_OEM_PARAM_4,     2#10101111).

%%%===================================================================
%%% MEMORY DEFINES
%%%===================================================================

-define(ADDRESS_ENDAT_INTERFACE,    16#8).
-define(ADDRESS_NUMBER_OF_CLOCKS,   16#D).

%%%===================================================================
%%% Timeout defines in ms
%%%===================================================================

-define(RESET_TIMEOUT,       16).
-define(CMD_TIMEOUT,         5).

%%%===================================================================
%%% Register Defines
%%%===================================================================

%% Reset delay im ms
-define(ENDAT_RESET_DELAY,           100).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
start_link([EndatName, Instance]) ->
  gen_server:start_link({local, EndatName}, ?MODULE, [Instance], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
init([Instance]) ->
  State = #endat_info { instance = Instance },
  %% Register Gproc name
  gproc:reg({p, l, {?MODULE, Instance}}),
  %% comment this line to stop trapping exits
  process_flag(trap_exit, true),
  %% Update State
  NewState = update_endat_state(State),
  {ok, NewState}.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
terminate(_, _LD) ->
  gproc:goodbye().

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
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
%% Get operations with no arguments
handle_call({get, state}, _From, State) ->
  {reply, State, State};

handle_call({get, Operation}, _From, State) ->
  Res = send_endat(Operation, State#endat_info.instance),
  {reply, Res, State};

%% Set operations with no arguments
handle_call({set, Operation}, _From, State) ->
  Res = send_endat(Operation, State#endat_info.instance),
  {reply, Res, State};

%% Get operations with arguments

%% Set operations with arguments

handle_call(_Request, _From, State) ->
  {reply, ignored, State}.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Exported ENDAT functions
%%%===================================================================
-spec get_state() -> { ok | error , #endat_info{} }.
get_state() ->
  get_state(?DEFAULT_INSTANCE).

-spec get_state(endatInstance()) -> { ok | error , #endat_info{} }.
get_state(Instance) ->
  gproc_call(Instance, {get, state}).

-spec set_reset() -> { ok | error , integer() }.
set_reset() ->
  set_reset(?DEFAULT_INSTANCE).

-spec set_reset(endatInstance()) -> { ok | error , integer() }.
set_reset(Instance) ->
  gproc_call(Instance, {set, reset}).


-spec get_clocks() -> { ok | error , integer() }.
get_clocks() ->
  get_clocks(?DEFAULT_INSTANCE).

-spec get_clocks(endatInstance()) -> { ok | error , integer() }.
get_clocks(Instance) ->
  gproc_call(Instance, {get, get_clocks}).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private Update endat static data
%%--------------------------------------------------------------------
update_endat_state(S) ->
  %% Update all information but instance
  S.

%%--------------------------------------------------------------------
%% @private Write endat information
%%--------------------------------------------------------------------
send_endat(reset, Instance) ->
  %% Send command and analyse the encoder response
  {ok, <<0:34, 1:1, Param8:8, Param16:16, Crc:5>>} = 
      endat_driver:write_command(Instance, 
                                 compose_send_cmd(?MODE_ENCODER_RCV_RESET, 0, 0), 
                                 ?RESET_TIMEOUT),
  {ok, Crc} = endat_crc:makeCrcNormLt(Param8,Param16),
  {ok, 0};

send_endat(get_clocks, Instance) ->
  %% Send command and analyse the encoder response
  {ok, <<0:34, 1:1, Param8:8, Param16:16, Crc:5>>} = 
      endat_driver:write_command(Instance, 
                                 compose_send_cmd(?MODE_ENCODER_SEND_PARAM, ?ADDRESS_NUMBER_OF_CLOCKS, 0), 
                                 ?RESET_TIMEOUT),
  {ok, Crc} = endat_crc:makeCrcNormLt(Param8,Param16),
  {ok, Param16}.

%%--------------------------------------------------------------------
%% @private Send a gen_server:call message if the PID is found
%%--------------------------------------------------------------------
gproc_call(Instance, Msg) ->
  Key = {?MODULE, Instance},
  case gproc:lookup_pids({p, l, Key}) of
    [Pid] -> gen_server:call(Pid, Msg);
    _ -> {error, invalid_endat}
  end.

compose_send_cmd(Mode, Address, Data) ->
  b2u32(<<0:1,Mode:6, 0:1, Address:8, Data:16>>).

b2u32(Bin) ->
  binary:decode_unsigned(Bin).









