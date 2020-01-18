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
         set_reset/1]).

%% Public API export where the Instance is the default defined value
-export([get_state/0,
         set_reset/0]).

-compile([export_all]).

%%%===================================================================
%%% Global Defines
%%%===================================================================

-define(DEFAULT_INSTANCE, 0).

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

%% Set operations with no arguments
handle_call({set, Operation}, _From, State) ->
  Res = write_priv(Operation, State#endat_info.instance),
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

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private Update endat static data
%%--------------------------------------------------------------------
update_endat_state(S) ->
  %% 1 byte information data
  {ok, ChipId0} = endat_driver:read_register(S#endat_info.instance, 0),
  {ok, ChipId1} = endat_driver:read_register(S#endat_info.instance, 1),
  %% Update all information but instance
  S.

%%--------------------------------------------------------------------
%% @private Write endat information
%%--------------------------------------------------------------------
write_priv(reset, Instance) ->
  endat_driver:write_register(Instance, 0, 1).

%%--------------------------------------------------------------------
%% @private Send a gen_server:call message if the PID is found
%%--------------------------------------------------------------------
gproc_call(Instance, Msg) ->
  Key = {?MODULE, Instance},
  case gproc:lookup_pids({p, l, Key}) of
    [Pid] -> gen_server:call(Pid, Msg);
    _ -> {error, invalid_endat}
  end.








