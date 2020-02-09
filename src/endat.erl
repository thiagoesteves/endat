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
         get_position/1,
         set_reset/1,
         start_read_position/1,
         stop_read_position/1]).

%% Public API export where the Instance is the default defined value
-export([get_state/0,
         get_position/0,
         set_reset/0,
         start_read_position/0,
         stop_read_position/0]).

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

-define(ADDRESS_ENDAT_INTERFACE_VER, 16#8).
-define(ADDRESS_NUMBER_OF_CLOCKS,    16#D).
-define(ADDRESS_ID_NUMBER,           16#8).
-define(ADDRESS_SERIAL_NUMBER,       16#B).

%%%===================================================================
%%% Timeout defines in ms
%%%===================================================================

-define(RESET_TIMEOUT,       16).
-define(CMD_TIMEOUT,         5).

%%%===================================================================
%%% Encoder Defines
%%%===================================================================

-define(ENDAT_2_1,           0).
-define(ENDAT_2_2,           1).

%% 64bits (answer sizeof) - (Start + Error1 + X + Crc)
-define(ENDAT_2_1_POSITION_HEADER, (64 - 7)).
%% 64bits (answer sizeof) - (Start + Error1 + Error2 + X + Crc)
-define(ENDAT_2_2_POSITION_HEADER, (64 - 8)).

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
%% Handle the position if the driver started to receive constantly
handle_info({endat_event, <<Bin:64>>}, State) ->
  {ok, Pos} = analyse_received_position(State#endat_info.endat_version,
                                        Bin,
                                        State#endat_info.bits_pos1),
  %% Here you have the Position being sent for analysis each 10ms
  io:format("\rP: ~p", [Pos]),
  {noreply, State};

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
handle_call(state, _From, State) ->
  {reply, State, State};

handle_call(get_position, _From, State) ->
  Res = read_position(State#endat_info.instance,
                      State#endat_info.endat_version,
                      State#endat_info.bits_pos1),
  {reply, Res, State};

handle_call(start_read_position, _From, State) ->
  Res = endat_driver:start_read_position(State#endat_info.instance,
                                         State#endat_info.endat_version,
                                         State#endat_info.bits_pos1),
  {reply, Res, State};

handle_call(stop_read_position, _From, State) ->
  Res = endat_driver:stop_read_position(State#endat_info.instance),
  {reply, Res, State};

handle_call(Operation, _From, State) ->
  Res = send_endat(Operation, State#endat_info.instance),
  {reply, Res, State}.

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
  gproc_call(Instance, state).

-spec set_reset() -> { ok | error , integer() }.
set_reset() ->
  set_reset(?DEFAULT_INSTANCE).

-spec set_reset(endatInstance()) -> { ok | error , integer() }.
set_reset(Instance) ->
  gproc_call(Instance, reset).

-spec get_position() -> { ok | error , integer() }.
get_position() ->
  get_position(?DEFAULT_INSTANCE).

-spec get_position(endatInstance()) -> { ok | error , integer() }.
get_position(Instance) ->
  gproc_call(Instance, get_position).

-spec start_read_position() -> { ok | error , integer() }.
start_read_position() ->
  start_read_position(?DEFAULT_INSTANCE).

-spec start_read_position(endatInstance()) -> { ok | error , integer() }.
start_read_position(Instance) ->
  gproc_call(Instance, start_read_position).

-spec stop_read_position() -> { ok | error , integer() }.
stop_read_position() ->
  stop_read_position(?DEFAULT_INSTANCE).

-spec stop_read_position(endatInstance()) -> { ok | error , integer() }.
stop_read_position(Instance) ->
  gproc_call(Instance, stop_read_position).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private Update endat static data
%%--------------------------------------------------------------------
update_endat_state(S) ->
  %% Reset endat encoder
  {ok, _} = send_endat(reset, S#endat_info.instance),
  %% Update all information but instance
  {ok, Clocks}  = send_endat(get_clocks,  S#endat_info.instance),
  {ok, Id}      = send_endat(get_id,      S#endat_info.instance),
  {ok, Serial}  = send_endat(get_serial,  S#endat_info.instance),
  {ok, Version} = send_endat(get_version, S#endat_info.instance),
  S#endat_info{ 
    bits_pos1 = Clocks,
    id = Id,
    serial = Serial,
    endat_version = Version}.

%%--------------------------------------------------------------------
%% @private Write endat information
%%--------------------------------------------------------------------
send_endat(reset, Instance) ->
  send_cmd_and_check_crc(Instance, ?MODE_ENCODER_RCV_RESET, 
                                   0,
                                   0,
                                   ?RESET_TIMEOUT);

send_endat(get_clocks, Instance) ->
  %% Select the target memory address
  {ok, _} = endat_select_memory(Instance, ?MRS_PARAM_MANUF_1),
  %% Send command and analyse the encoder response
  send_cmd_and_check_crc(Instance, ?MODE_ENCODER_SEND_PARAM, 
                                   ?ADDRESS_NUMBER_OF_CLOCKS, 
                                   0, 
                                   ?CMD_TIMEOUT);

send_endat(get_version, Instance) ->
  %% Select the target memory address
  {ok, _} = endat_select_memory(Instance, ?MRS_PARAM_MANUF_1),
  %% Send command and analyse the encoder response
  send_cmd_and_check_crc(Instance, ?MODE_ENCODER_SEND_PARAM, 
                                   ?ADDRESS_ENDAT_INTERFACE_VER, 
                                   0, 
                                   ?CMD_TIMEOUT);

send_endat(get_id, Instance) ->
  %% Select the target memory address
  {ok, _} = endat_select_memory(Instance, ?MRS_PARAM_MANUF_2),
  %% Send command and analyse the encoder response
  {ok, ID1} = send_cmd_and_check_crc(Instance, ?MODE_ENCODER_SEND_PARAM, 
                                               ?ADDRESS_ID_NUMBER, 
                                               0, 
                                               ?CMD_TIMEOUT),
  {ok, ID2} = send_cmd_and_check_crc(Instance, ?MODE_ENCODER_SEND_PARAM, 
                                               ?ADDRESS_ID_NUMBER + 1, 
                                               0, 
                                               ?CMD_TIMEOUT),
  {ok, ID3} = send_cmd_and_check_crc(Instance, ?MODE_ENCODER_SEND_PARAM, 
                                               ?ADDRESS_ID_NUMBER + 2, 
                                               0, 
                                               ?CMD_TIMEOUT),
{ok, b2u32(<<0:8, ID3:8, ID2:8, ID1:8>>)};

send_endat(get_serial, Instance) ->
  %% Select the target memory address
  {ok, _} = endat_select_memory(Instance, ?MRS_PARAM_MANUF_2),
  %% Send command and analyse the encoder response
  {ok, ID1} = send_cmd_and_check_crc(Instance, ?MODE_ENCODER_SEND_PARAM, 
                                               ?ADDRESS_SERIAL_NUMBER, 
                                               0, 
                                               ?CMD_TIMEOUT),
  {ok, ID2} = send_cmd_and_check_crc(Instance, ?MODE_ENCODER_SEND_PARAM, 
                                               ?ADDRESS_SERIAL_NUMBER + 1, 
                                               0, 
                                               ?CMD_TIMEOUT),
  {ok, ID3} = send_cmd_and_check_crc(Instance, ?MODE_ENCODER_SEND_PARAM, 
                                               ?ADDRESS_SERIAL_NUMBER + 2, 
                                               0, 
                                               ?CMD_TIMEOUT),
{ok, b2u32(<<0:8, ID3:8, ID2:8, ID1:8>>)}.

%% Send the command to select memory and check the answer
endat_select_memory(Instance, MRS) ->
  send_cmd_and_check_crc(Instance, ?MODE_SELECT_MEMORY, 
                                   MRS, 
                                   0, 
                                   ?CMD_TIMEOUT).

send_cmd_and_check_crc(Instance, MODE, MRS, DATA, Timeout)->
  %% Send command and analyse the encoder response
  {ok, <<0:34, 1:1, Param8:8, Param16:16, Crc:5>>} = 
      endat_driver:write_command(Instance, 
                                 compose_send_cmd(MODE, MRS, DATA), 
                                 Timeout),
  {ok, Crc} = endat_crc:makeCrcNormLt(Param8,Param16),
  {ok, Param16}.

%% Read position for Endat 2.1 and Endat 2.2
read_position(Instance, Endat, PositionBits) ->
  {ok, <<Answer:64>>} = endat_driver:read_position(Instance,
                                 Endat,
                                 compose_read_cmd(), 
                                 PositionBits),
  analyse_received_position(Endat, Answer, PositionBits).

analyse_received_position(Endat, Bin, PositionBits) ->
  %% Check type of Endat
  case Endat of
    ?ENDAT_2_2 ->
      Rest = ?ENDAT_2_2_POSITION_HEADER-PositionBits,
      <<0:Rest, S:1, Err1:1, Err2:1, InvPos:PositionBits, Crc:5>> = <<Bin:64>>;
    ?ENDAT_2_1 ->
      Rest = ?ENDAT_2_1_POSITION_HEADER-PositionBits,
      <<0:Rest, S:1, Err1:1, InvPos:PositionBits, Crc:5>> = <<Bin:64>>,
      Err2=1
  end,
  %% Revert Position
  {ok, P} = endat_crc:reverse64Bits(InvPos),
  Rest2 = 64 - PositionBits,
  %% Extract position
  << Pos:PositionBits, _:Rest2 >> = padTo64(binary:encode_unsigned(P)),
  %% Extract MSB and LSB position
  <<High:32, Low:32>> = padTo64(binary:encode_unsigned(Pos)),
  %% Check CRC
  {ok, CalculatedCrc} = 
    endat_crc:makeCrcPosLt(PositionBits,Endat,Err1,Err2,High,Low),
  %% Check Answers
  case {Crc, S, Err1, Err2} of
    {CalculatedCrc, 1, 0, 1} -> {ok, Pos};
    {CalculatedCrc, _, _, _} -> {error, encoder_error};
    {_,_,_,_}                -> {error, invalid_crc}
  end.

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

compose_read_cmd() ->
  b2u32(<<0:1,?MODE_ENCODER_SEND_POS:6, 0:1>>).

b2u32(Bin) ->
  binary:decode_unsigned(Bin).

padTo64(Bin) ->
  case (8 - size(Bin)) of
    0 -> Bin;
    N -> <<0:(N*8), Bin/binary>>
  end.









