-module(lib9p).

-include("9p.hrl").

-export([parse_message/2,
         pack_message/3]).

%% Version

-spec parse_message(integer(), binary()) -> false | any().
parse_message(?TVersion, <<MSize:32/little-integer,
                           S:16/little-integer,
                           Version:S/binary>>) ->
  {MSize, Version};

parse_message(?RVersion, <<MSize:32/little-integer,
                           S:16/little-integer,
                           Version:S/binary>>) ->
  {MSize, Version};

parse_message(?TAuth, <<AFid:32/little-integer,
                        Su:16/little-integer,
                        Uname:Su/binary,
                        Sa:16/little-integer,
                        Aname:Sa/binary>>) ->
  {AFid, Uname, Aname};

parse_message(?RAuth, <<AQid:13/binary>>) ->
  binary_to_qid(AQid);

parse_message(?RError, <<S:16/little-integer,
                         Ename:S/binary>>) ->
  Ename;

parse_message(?TFlush, <<OldTag:2/binary>>) ->
  OldTag;

parse_message(?RFlush, <<>>) ->
  <<>>;

parse_message(?TAttach, <<Fid:32/little-integer,
                          AFid:32/little-integer,
                          Su:16/little-integer,
                          Uname:Su/binary,
                          Sa:16/little-integer,
                          Aname:Sa/binary>>) ->
  {Fid, AFid, Uname, Aname};

parse_message(?RAttach, <<Qid:13/binary>>) ->
  binary_to_qid(Qid);

parse_message(?TWalk, <<Fid:32/little-integer,
                        NewFid:32/little-integer,
                        Nwname:16/little-integer,
                        Rest/binary>>) ->
  Wnames = binary_to_wnames(Nwname, Rest),
  {Fid, NewFid, Wnames};

parse_message(?RWalk, <<Nwqid:16/little-integer,
                        Rest/binary>>) ->
  binary_to_wqids(Nwqid, Rest);

parse_message(?TOpen, <<Fid:32/little-integer,
                        Mode:1/binary>>) ->
  {Fid, Mode};

parse_message(?ROpen, <<Qid:13/binary,
                        IOunit:32/little-integer>>) ->
  {binary_to_qid(Qid), IOunit};

parse_message(?TCreate, <<Fid:32/little-integer,
                          S:16/little-integer,
                          Name:S/binary,
                          Perm:32/little-integer,
                          Mode:1/binary>>) ->
  {Fid, Name, Perm, Mode};

parse_message(?RCreate, <<Qid:13/binary,
                          IOunit:32/little-integer>>) ->
  {qid_to_binary(Qid), IOunit};

parse_message(?TRead, <<Fid:32/little-integer,
                        Offset:64/little-integer,
                        Count:32/little-integer>>) ->
  {Fid, Offset, Count};

parse_message(?RRead, <<Count:32/little-integer,
                        Data:Count/binary>>) ->
  Data;

parse_message(?TWrite, <<Fid:32/little-integer,
                         Offset:64/little-integer,
                         Count:32/little-integer,
                         Data:Count/binary>>) ->
  {Fid, Offset, Data};

parse_message(?RWrite, <<Count:32/little-integer>>) ->
  Count;

parse_message(?TClunk, <<Fid:32/little-integer>>) ->
  Fid;

parse_message(?RClunk, <<>>) ->
  <<>>;

parse_message(?TRemove, <<Fid:32/little-integer>>) ->
  Fid;

parse_message(?RRemove, <<>>) ->
  <<>>;

parse_message(?TStat, <<Fid:32/little-integer>>) ->
  Fid;

parse_message(?RStat, <<N:16/little-integer,
                        Stat:N/binary>>) ->
  Stat;

parse_message(?TWstat, <<Fid:32/little-integer,
                         N:16/little-integer,
                         Stat:N/binary>>) ->
  {Fid, Stat};

parse_message(?RWstat, <<>>) ->
  <<>>;

parse_message(_, _) ->
  false.

%% Packing

-spec pack_message(integer(), binary(), any()) -> binary().
pack_message(Type, Tag, Data) ->
  BinData = pack_message(Type, Data),
  DSize = size(BinData) + 7,
  <<DSize:32/little-integer,
    Type:8/little-integer,
    Tag:2/binary,
    BinData/binary>>.

-spec pack_message(integer(), any()) -> binary().
pack_message(?TVersion, {MSize, Version}) ->
  S = size(Version),
  <<MSize:32/little-integer,
    S:16/little-integer,
    Version/binary>>;

pack_message(?RVersion, {MSize, Version}) ->
  S = size(Version),
  <<MSize:32/little-integer,
    S:16/little-integer,
    Version/binary>>;

%% Auth

pack_message(?TAuth, {AFid, Uname, Aname}) ->
  Su = size(Uname),
  Sa = size(Aname),
  <<AFid:32/little-integer,
    Su:16/little-integer,
    Uname/binary,
    Sa:16/little-integer,
    Aname/binary>>;

pack_message(?RAuth, AQid) ->
  qid_to_binary(AQid);

%% Error

pack_message(?RError, Name) ->
  S = size(Name),
  <<S:16/little-integer,
    Name/binary>>;

%% Flush

pack_message(?TFlush, OldTag) ->
  <<OldTag:2/binary>>;

pack_message(?RFlush, _) ->
  <<>>;

%% Attach

pack_message(?TAttach, {Fid, AFid, Uname, Aname}) ->
  Su = size(Uname),
  Sa = size(Aname),
  <<Fid:32/little-integer,
    AFid:32/little-integer,
    Su:16/little-integer,
    Uname/binary,
    Sa:16/little-integer,
    Aname/binary>>;

pack_message(?RAttach, Qid) ->
  qid_to_binary(Qid);

%% Walk

pack_message(?TWalk, {Fid, NewFid, Wnames}) ->
  Nwname = length(Wnames),
  BinNames = wnames_to_binary(Wnames),
  <<Fid:32/little-integer,
    NewFid:32/little-integer,
    Nwname:16/little-integer,
    BinNames/binary>>;

pack_message(?RWalk, Wqids) ->
  Nwqids = length(Wqids),
  BinQids = wqids_to_binary(Wqids),
  <<Nwqids:16/little-integer,
    BinQids/binary>>;

%% Open

pack_message(?TOpen, {Fid, Mode}) ->
  <<Fid:32/little-integer,
    Mode:1/binary>>;

pack_message(?ROpen, {Qid, IOunit}) ->
  BQid = qid_to_binary(Qid),
  <<BQid:13/binary,
    IOunit:32/little-integer>>;

%% Create

pack_message(?TCreate, {Fid, Name, Perm, Mode}) ->
  S = size(Name),
  <<Fid:32/little-integer,
    S:16/little-integer,
    Name/binary,
    Perm:32/little-integer,
    Mode:1/binary>>;

pack_message(?RCreate, {Qid, IOunit}) ->
  BQid = qid_to_binary(Qid),
  <<BQid:13/binary,
    IOunit:32/little-integer>>;

%% read

pack_message(?TRead, {Fid, Offset, Count}) ->
  <<Fid:32/little-integer,
    Offset:64/little-integer,
    Count:32/little-integer>>;

pack_message(?RRead, RData) ->
  Count = size(RData),
  <<Count:32/little-integer,
    RData/binary>>;

%% Write

pack_message(?TWrite, {Fid, Offset, WData}) ->
  Count = size(WData),
  <<Fid:32/little-integer,
    Offset:64/little-integer,
    Count:32/little-integer,
    WData/binary>>;

pack_message(?RWrite, Count) ->
  <<Count:32/little-integer>>;

%% Clunk

pack_message(?TClunk, Fid) ->
  <<Fid:32/little-integer>>;

pack_message(?RClunk, _) ->
  <<>>;

%% Remove

pack_message(?TRemove, Fid) ->
  <<Fid:32/little-integer>>;

pack_message(?RRemove, _) ->
  <<>>;

%% Stat

pack_message(?TStat, Fid) ->
  <<Fid:32/little-integer>>;

pack_message(?RStat, Stat) ->
  N = size(Stat),
  <<N:16/little-integer,
    Stat/binary>>;

%% WStat

pack_message(?TWstat, {Fid, Stat}) ->
  N = size(Stat),
  <<Fid:32/little-integer,
    N:16/little-integer,
    Stat/binary>>;

pack_message(?RWstat, _) ->
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

-spec binary_to_wqids(integer(), [binary()]) -> [binary()].
binary_to_wqids(0, _) ->
  [];

binary_to_wqids(N, <<Wqid:13/binary,
                     Rest/binary>>) ->
  [binary_to_qid(Wqid), binary_to_wqids(N-1, Rest)].

-spec binary_to_qid(binary()) -> {integer(), integer(), integer()}.
binary_to_qid(<<Type:8/little-integer,
                Vers:32/little-integer,
                Path:64/little-integer>>) ->
  {Type, Vers, Path}.

-spec qid_to_binary({integer(), integer(), integer()}) -> binary().
qid_to_binary({Type, Vers, Path}) ->
  <<Type:8/little-integer,
    Vers:32/little-integer,
    Path:64/little-integer>>.
