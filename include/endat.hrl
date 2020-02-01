%%%=============================================================================
%%% Created : 3 Jan 2020 by Thiago Esteves <thiagocalori@gmail.com>
%%%=============================================================================

-ifndef(endat).
-define(endat, true).

%% Endat encoder defines
-type endatInstance() :: 0..4.

%% static information records
-record(endat_info, {
  instance,
  endat_version, % 0 for Endat 2.1 and 1 for Endat 2.2
  id,
  serial,
  bits_pos1
}).

-endif. %% endat