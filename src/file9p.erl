-module(file9p).

-export([make/5   , is_file9p/1,
         atime/1  , atime/2,
         mtime/1  , mtime/2,
         length/1 , length/2,
         name/1   , name/2,
         uid/1    , uid/2,
         gid/1    , gid/2,
         muid/1   , muid/2,
         type/1, version/1, path/1,
         qid/1,
         is_directory/1,
         accessible/4,
         readable/3,
         writable/3,
         executable/3,
         set_mode/2,
         stat/1]).

-include("9p.hrl").

-record(file9p, {type = 0 :: integer(),           % for kernel use?
                 dev  = 0 :: integer(),           % for kernel use?
                 qid      :: qid(),               % unique qid
                 mode     :: integer(),           % access mode
                 atime    :: pos_integer(),       % last read time
                 mtime    :: pos_integer(),       % last write time
                 length   :: integer(),           % data size
                 name     :: binary(),            % file name
                 uid      :: binary(),            % owner id
                 gid      :: binary(),            % group id
                 muid     :: binary()}).          % last modifier uid

-type file9p() :: #file9p{}.
-export_type([file9p/0]).

-spec make(Type::qid_type(), Name::binary(),
           Mode::integer(), Uid::binary(),
           Gid::binary()) -> file9p().
make(Type, Name, Mode, Uid, Gid) ->
  Qid = make_qid(Type),
  Ts = unix_now(),
  #file9p{qid=Qid, mode=Mode, atime=Ts,
          mtime=Ts, length=0, name=Name,
          uid=Uid, gid=Gid, muid=Uid}.

-spec is_file9p(any()) -> boolean().
is_file9p(#file9p{}) ->
  true;
is_file9p(_) ->
  false.

-spec atime(file9p()) -> pos_integer().
atime(#file9p{atime=Atime}) ->
  Atime.

-spec atime(file9p(), pos_integer()) -> file9p().
atime(#file9p{}=File, Atime) ->
  File#file9p{atime=Atime}.

-spec mtime(file9p()) -> pos_integer().
mtime(#file9p{mtime=Mtime}) ->
  Mtime.

-spec mtime(file9p(), pos_integer()) -> file9p().
mtime(#file9p{}=File, Mtime) ->
  File#file9p{mtime=Mtime}.

-spec length(file9p()) -> integer().
length(#file9p{length=Len}) ->
  Len.

-spec length(file9p(), integer()) -> file9p().
length(#file9p{}=File, Len) ->
  File#file9p{length=Len}.

-spec name(file9p()) -> binary().
name(#file9p{name=Name}) ->
  Name.

-spec name(file9p(), binary()) -> file9p().
name(#file9p{}=File, Name) ->
  File#file9p{name=Name}.

-spec uid(file9p()) -> binary().
uid(#file9p{uid=Uid}) ->
  Uid.

-spec uid(file9p(), binary()) -> file9p().
uid(#file9p{}=File, Uid) ->
  File#file9p{uid=Uid}.

-spec gid(file9p()) -> binary().
gid(#file9p{gid=Gid}) ->
  Gid.

-spec gid(file9p(), binary()) -> file9p().
gid(#file9p{}=File, Gid) ->
  File#file9p{gid=Gid}.

-spec muid(file9p()) -> binary().
muid(#file9p{muid=Muid}) ->
  Muid.

-spec muid(file9p(), binary()) -> file9p().
muid(#file9p{}=File, Muid) ->
  File#file9p{muid=Muid}.

-spec type(file9p()) -> qid_type().
type(#file9p{qid=Qid}) ->
  Qid#qid.type.

-spec version(file9p()) -> integer().
version(#file9p{qid=Qid}) ->
  Qid#qid.version.

-spec path(file9p()) -> binary().
path(#file9p{qid=Qid}) ->
  Qid#qid.path.

-spec qid(file9p()) -> {qid_type(), qid_version(), qid_path()}.
qid(#file9p{qid=#qid{type=T, version=V, path=P}}) ->
  {T, V, P}.

-spec stat(file9p()) -> binary().
stat(#file9p{type=Type, dev=Dev,
             qid=#qid{type=QType, version=QVers, path=QPath},
             mode=Mode, atime=Atime, mtime=Mtime,
             length=Length, name=Name, uid=Uid,
             gid=Gid, muid=Muid}) ->
  NameSize = byte_size(Name),
  UidSize = byte_size(Uid),
  GidSize = byte_size(Gid),
  MuidSize = byte_size(Muid),
  <<Type:16, Dev:32, QType:8, QVers:32/little-integer,QPath:8/binary,
    Mode:32/little-integer, Atime:32, Mtime:32, Length:64,
    NameSize:16/little-integer, Name/binary,
    UidSize:16/little-integer, Uid/binary,
    GidSize:16/little-integer, Gid/binary,
    MuidSize:16/little-integer, Muid/binary>>.


-spec is_directory(file9p()) -> boolean().
is_directory(#file9p{qid=Qid}) ->
  Qid#qid.type == ?DirType.

-spec accessible(File::file9p(), User::binary(),
                 Groups::[binary()], Mode::byte()) -> boolean().
accessible(#file9p{uid=Uid, gid=Gid, mode=Mode}, User, Groups, AMode) ->
  InGroup = lists:member(Gid, Groups),
  Mask = if User == Uid ->
             8#777;
            InGroup == true ->
             8#77;
            true ->
             8#7
         end,
  (Mode band Mask band AMode) == AMode.

-spec readable(File::file9p(), User::binary(), Groups::[binary()]) ->
                   boolean().
readable(#file9p{uid=Uid, gid=Gid, mode=Mode}, User, Groups) ->
  InGroup = lists:member(Gid, Groups),
  Mask = if Uid == User ->
             8#444;
            InGroup == true ->
             8#44;
            true ->
             8#4
         end,
  (Mode band Mask) /= 0.

-spec writable(File::file9p(), User::binary(), Groups::[binary()]) ->
                    boolean().
writable(#file9p{uid=Uid, gid=Gid, mode=Mode}, User, Groups) ->
  InGroup = lists:member(Gid, Groups),
  Mask = if Uid == User ->
             8#222;
            InGroup == true ->
             8#22;
            true ->
             8#2
         end,
  (Mode band Mask) /= 0.

-spec executable(File::file9p(), User::binary(), Groups::[binary()]) ->
                   boolean().
executable(#file9p{uid=Uid, gid=Gid, mode=Mode}, User, Groups) ->
  InGroup = lists:member(Gid, Groups),
  Mask = if Uid == User ->
             8#111;
            InGroup == true ->
             8#11;
            true ->
             8#1
         end,
  (Mode band Mask) /= 0.

-spec set_mode(File::file9p(), SMode::integer()) -> file9p().
set_mode(#file9p{mode=Mode}=File, SMode)
  when is_integer(SMode) andalso SMode >= 0 andalso SMode =< 8#777 ->
  File#file9p{mode=(Mode bor SMode)}.

%% Utils

-spec make_qid(Type::qid_type()) -> qid().
make_qid(Type) ->
  #qid{type=Type,
       version=1,
       path=crypto:rand_bytes(8)}.

-spec unix_now() -> integer().
unix_now() ->
  {Mega, Sec, _} = now(),
  Mega * 1000000 + Sec.
