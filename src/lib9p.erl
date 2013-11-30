-module(lib9p).

-include("9p.hrl").

-export([parse_message/2,
         pack_message/3]).

%% Version

-spec parse_message(byte(), binary()) -> false | tuple() | binary().
parse_message(?Tversion, <<MSize:32/little-integer,
                           S:16/little-integer,
                           Version:S/binary>>) ->
  {MSize, Version};

parse_message(?Rversion, <<MSize:32/little-integer,
                           S:16/little-integer,
                           Version:S/binary>>) ->
  {MSize, Version};

parse_message(?Tauth, <<AFid:32/little-integer,
                        Su:16/little-integer,
                        Uname:Su/binary,
                        Sa:16/little-integer,
                        Aname:Sa/binary>>) ->
  {AFid, Uname, Aname};

parse_message(?Rauth, <<AQid:13/binary>>) ->
  binary_to_qid(AQid);

parse_message(?Rerror, <<S:16/little-integer,
                         Ename:S/binary>>) ->
  Ename;

parse_message(?Tflush, <<OldTag:2/binary>>) ->
  OldTag;

parse_message(?Rflush, <<>>) ->
  <<>>;

parse_message(?Tattach, <<Fid:32/little-integer,
                          AFid:32/little-integer,
                          Su:16/little-integer,
                          Uname:Su/binary,
                          Sa:16/little-integer,
                          Aname:Sa/binary>>) ->
  {Fid, AFid, Uname, Aname};

parse_message(?Rattach, <<Qid:13/binary>>) ->
  binary_to_qid(Qid);

parse_message(?Twalk, <<Fid:32/little-integer,
                        NewFid:32/little-integer,
                        Nwname:16/little-integer,
                        Rest/binary>>) ->
  Wnames = binary_to_wnames(Nwname, Rest),
  {Fid, NewFid, Wnames};

parse_message(?Rwalk, <<Nwqid:16/little-integer,
                        Rest/binary>>) ->
  binary_to_wqids(Nwqid, Rest);

parse_message(?Topen, <<Fid:32/little-integer,
                        Mode:1/binary>>) ->
  {Fid, Mode};

parse_message(?Ropen, <<Qid:13/binary,
                        IOunit:32/little-integer>>) ->
  {binary_to_qid(Qid), IOunit};

parse_message(?Tcreate, <<Fid:32/little-integer,
                          S:16/little-integer,
                          Name:S/binary,
                          Perm:32/little-integer,
                          Mode:1/binary>>) ->
  {Fid, Name, Perm, Mode};

parse_message(?Rcreate, <<Qid:13/binary,
                          IOunit:32/little-integer>>) ->
  {binary_to_qid(Qid), IOunit};

parse_message(?Tread, <<Fid:32/little-integer,
                        Offset:64/little-integer,
                        Count:32/little-integer>>) ->
  {Fid, Offset, Count};

parse_message(?Rread, <<Count:32/little-integer,
                        Data:Count/binary>>) ->
  Data;

parse_message(?Twrite, <<Fid:32/little-integer,
                         Offset:64/little-integer,
                         Count:32/little-integer,
                         Data:Count/binary>>) ->
  {Fid, Offset, Data};

parse_message(?Rwrite, <<Count:32/little-integer>>) ->
  Count;

parse_message(?Tclunk, <<Fid:32/little-integer>>) ->
  Fid;

parse_message(?Rclunk, <<>>) ->
  <<>>;

parse_message(?Tremove, <<Fid:32/little-integer>>) ->
  Fid;

parse_message(?Rremove, <<>>) ->
  <<>>;

parse_message(?Tstat, <<Fid:32/little-integer>>) ->
  Fid;

parse_message(?Rstat, <<N:16/little-integer,
                        Stat:N/binary>>) ->
  Stat;

parse_message(?Twstat, <<Fid:32/little-integer,
                         N:16/little-integer,
                         Stat:N/binary>>) ->
  {Fid, Stat};

parse_message(?Rwstat, <<>>) ->
  <<>>;

parse_message(_, _) ->
  false.

%% Packing

-spec pack_message(byte(), binary(), tuple() | binary()) -> binary().
pack_message(Type, Tag, Data) ->
  BinData = pack_message(Type, Data),
  DSize = size(BinData) + 7,
  <<DSize:32/little-integer,
    Type:8/little-integer,
    Tag:2/binary,
    BinData/binary>>.

-spec pack_message(integer(), any()) -> binary().
pack_message(?Tversion, {MSize, Version}) ->
  S = size(Version),
  <<MSize:32/little-integer,
    S:16/little-integer,
    Version/binary>>;

pack_message(?Rversion, {MSize, Version}) ->
  S = size(Version),
  <<MSize:32/little-integer,
    S:16/little-integer,
    Version/binary>>;

%% Auth

pack_message(?Tauth, {AFid, Uname, Aname}) ->
  Su = size(Uname),
  Sa = size(Aname),
  <<AFid:32/little-integer,
    Su:16/little-integer,
    Uname/binary,
    Sa:16/little-integer,
    Aname/binary>>;

pack_message(?Rauth, AQid) ->
  qid_to_binary(AQid);

%% Error

pack_message(?Rerror, Name) ->
  S = size(Name),
  <<S:16/little-integer,
    Name/binary>>;

%% Flush

pack_message(?Tflush, OldTag) ->
  <<OldTag:2/binary>>;

pack_message(?Rflush, _) ->
  <<>>;

%% Attach

pack_message(?Tattach, {Fid, AFid, Uname, Aname}) ->
  Su = size(Uname),
  Sa = size(Aname),
  <<Fid:32/little-integer,
    AFid:32/little-integer,
    Su:16/little-integer,
    Uname/binary,
    Sa:16/little-integer,
    Aname/binary>>;

pack_message(?Rattach, Qid) ->
  qid_to_binary(Qid);

%% Walk

pack_message(?Twalk, {Fid, NewFid, Wnames}) ->
  Nwname = length(Wnames),
  BinNames = wnames_to_binary(Wnames),
  <<Fid:32/little-integer,
    NewFid:32/little-integer,
    Nwname:16/little-integer,
    BinNames/binary>>;

pack_message(?Rwalk, Wqids) ->
  Nwqids = length(Wqids),
  BinQids = wqids_to_binary(Wqids),
  <<Nwqids:16/little-integer,
    BinQids/binary>>;

%% Open

pack_message(?Topen, {Fid, Mode}) ->
  <<Fid:32/little-integer,
    Mode:1/binary>>;

pack_message(?Ropen, {Qid, IOunit}) ->
  BQid = qid_to_binary(Qid),
  <<BQid:13/binary,
    IOunit:32/little-integer>>;

%% Create

pack_message(?Tcreate, {Fid, Name, Perm, Mode}) ->
  S = size(Name),
  <<Fid:32/little-integer,
    S:16/little-integer,
    Name/binary,
    Perm:32/little-integer,
    Mode:1/binary>>;

pack_message(?Rcreate, {Qid, IOunit}) ->
  BQid = qid_to_binary(Qid),
  <<BQid:13/binary,
    IOunit:32/little-integer>>;

%% read

pack_message(?Tread, {Fid, Offset, Count}) ->
  <<Fid:32/little-integer,
    Offset:64/little-integer,
    Count:32/little-integer>>;

pack_message(?Rread, RData) ->
  Count = size(RData),
  <<Count:32/little-integer,
    RData/binary>>;

%% Write

pack_message(?Twrite, {Fid, Offset, WData}) ->
  Count = size(WData),
  <<Fid:32/little-integer,
    Offset:64/little-integer,
    Count:32/little-integer,
    WData/binary>>;

pack_message(?Rwrite, Count) ->
  <<Count:32/little-integer>>;

%% Clunk

pack_message(?Tclunk, Fid) ->
  <<Fid:32/little-integer>>;

pack_message(?Rclunk, _) ->
  <<>>;

%% Remove

pack_message(?Tremove, Fid) ->
  <<Fid:32/little-integer>>;

pack_message(?Rremove, _) ->
  <<>>;

%% Stat

pack_message(?Tstat, Fid) ->
  <<Fid:32/little-integer>>;

pack_message(?Rstat, Stat) ->
  N = size(Stat),
  <<N:16/little-integer,
    Stat/binary>>;

%% WStat

pack_message(?Twstat, {Fid, Stat}) ->
  N = size(Stat),
  <<Fid:32/little-integer,
    N:16/little-integer,
    Stat/binary>>;

pack_message(?Rwstat, _) ->
  <<>>.

%% Utils

-spec wnames_to_binary([binary()]) -> binary().
wnames_to_binary(Names) ->
  wnames_to_binary(Names, <<>>).

-spec wnames_to_binary([binary()], binary()) -> binary().
wnames_to_binary([], Acc) ->
  Acc;

wnames_to_binary([H|T], Acc) ->
  S = size(H),
  Data = <<S:16/little-integer,
           H/binary>>,
  wnames_to_binary(T, <<Acc/binary, Data/binary>>).

-spec binary_to_wnames(integer(), binary()) -> [binary()].
binary_to_wnames(0, _) ->
  [];

binary_to_wnames(N, <<S:16/little-integer,
                      Wname:S/binary,
                      Rest/binary>>) ->
  [Wname, binary_to_wnames(N-1, Rest)].

-spec wqids_to_binary([binary()]) -> binary().
wqids_to_binary(Qids) ->
  wqids_to_binary(Qids, <<>>).

-spec wqids_to_binary([binary()], binary()) -> binary().
wqids_to_binary([], Acc) ->
  Acc;

wqids_to_binary([H|T], Acc) ->
  BQid = qid_to_binary(H),
  wqids_to_binary(T, <<Acc/binary, BQid:13/binary>>).

-spec binary_to_wqids(integer(), binary()) -> [{byte(), integer(), integer()}].
binary_to_wqids(0, _) ->
  [];

binary_to_wqids(N, <<Wqid:13/binary,
                     Rest/binary>>) ->
  [binary_to_qid(Wqid) | binary_to_wqids(N-1, Rest)].

-spec binary_to_qid(binary()) -> {byte(), integer(), integer()}.
binary_to_qid(<<Type:8/little-integer,
                Vers:32/little-integer,
                Path:64/little-integer>>) ->
  {Type, Vers, Path}.

-spec qid_to_binary({qid_type(), qid_version(), qid_path()}) ->
                       binary().
qid_to_binary({Type, Vers, Path}) ->
  <<Type:8/little-integer,
    Vers:32/little-integer,
    Path:8/binary>>.
