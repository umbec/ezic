-module(ezic).
-include("ezic.hrl").

-export([
   localtime/1
   , utc_to_local/2
   , local_to_utc/2
   , has_dst_utc/2
   , has_dst_local/2
   , offset_utc/2
   , offset_local/2
   , timezone_exists/1
   ]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PUBLIC API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% returns local time in given timezone
localtime(TzName) ->
    utc_to_local(erlang:universaltime(), TzName).

%% ---------------------------------------------------------------------------

%% returns time in given timezone for corresponding utc time
utc_to_local(UTCDatetime, TzName) ->
    NormalDatetime= ezic_date:normalize(UTCDatetime, u),
    utc_to_local_handleFlatzone(UTCDatetime, ezic_db:flatzone(NormalDatetime, TzName)).

%% ---------------------------------------------------------------------------

%% returns utc time for corresponding time in given timezone utc
local_to_utc(LocalDatetime, TzName) ->
    NormalDatetime= ezic_date:normalize(LocalDatetime, w),
    local_to_utc_handleFlatzone(LocalDatetime, ezic_db:flatzone(NormalDatetime, TzName)).

%% ---------------------------------------------------------------------------

-spec has_dst_utc(calendar:datetime(), list()) -> boolean().

has_dst_utc(Datetime, TzName) ->
    has_dst(Datetime, TzName, u).

%% ---------------------------------------------------------------------------

-spec has_dst_local(calendar:datetime(), list()) -> boolean().

has_dst_local(Datetime, TzName) ->
    has_dst(Datetime, TzName, w).

%% ---------------------------------------------------------------------------

-spec offset_utc(Localtime, Timezone) -> {ok, {UtcOffset, DstOffset}} when
  Localtime :: calendar:datetime(),
  Timezone :: list(),
  UtcOffset :: calendar:time(),
  DstOffset :: calendar:time().

offset_utc(Datetime, TzName) ->
  offset_lookup(Datetime, TzName, u).

%% ---------------------------------------------------------------------------

-spec offset_local(Localtime, Timezone) -> {ok, {UtcOffset, DstOffset}} when
  Localtime :: calendar:datetime(),
  Timezone :: list(),
  UtcOffset :: calendar:time(),
  DstOffset :: calendar:time().

offset_local(Datetime, TzName) ->
  offset_lookup(Datetime, TzName, w).

%% ---------------------------------------------------------------------------

-spec timezone_exists(Timezone) -> Res when
  Timezone :: list(),
  Res :: boolean().

timezone_exists(Timezone) when is_list(Timezone) ->
  length(ezic_db:zones(Timezone)) > 0;
timezone_exists(_Timezone) ->
  false.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PRIVATE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

utc_to_local_handleFlatzone(_, X={error,_}) ->
    X;
utc_to_local_handleFlatzone(UTCDatetime, #flatzone{offset=Offset, dstoffset=DSTOffset}) ->
    ezic_date:add_offset(
      ezic_date:add_offset(
        UTCDatetime
        , Offset)
      , DSTOffset).


local_to_utc_handleFlatzone(_, X={error, _}) ->
    X;
local_to_utc_handleFlatzone(LocalDatetime, #flatzone{offset=Offset, dstoffset=DSTOffset}) ->
    ezic_date:add_offset(
      ezic_date:add_offset(
        LocalDatetime
        , Offset, {0,0,0})
      , DSTOffset, {0,0,0}).

has_dst(Datetime, TzName, Flag) ->
    NormalDatetime = ezic_date:normalize(Datetime, Flag),
    case ezic_db:flatzone(NormalDatetime, TzName) of
        #flatzone{dstoffset={0,0,0}} ->
            false;
        #flatzone{dstoffset={_,_,_}} ->
            true
    end.

offset_lookup(Datetime, TzName, Flag) ->
  NormalDatetime = ezic_date:normalize(Datetime, Flag),
  #flatzone{offset = UtcOffset, dstoffset = DstOffset} = ezic_db:flatzone(NormalDatetime, TzName),
  {ok, {UtcOffset, DstOffset}}.

