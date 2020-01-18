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
  serial,
  bits_pos1,
  revision_id
}).

-endif. %% endat