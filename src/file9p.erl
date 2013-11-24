-module(file9p).

-export([make/5,
         mode/1   , mode/2,
         atime/1  , atime/2,
         mtime/1  , mtime/2,
         length/1 , length/2,
         name/1   , name/2,
         uid/1    , uid/2,
         gid/1    , gid/2,
         muid/1   , muid/2,
         type/1, version/1, path/1,
         is_directory/1]).

-include("9p.hrl").

-record(file9p, {type,                          % for kernel use?
               dev,                           % for kernel use?
               qid :: qid(),                  % unique qid
               mode,                          % access mode
               atime :: pos_integer(),        % access time
               mtime :: pos_integer(),        % modification time
               length :: integer(),           % file data size
               name :: binary(),              % file name
               uid :: binary(),               % owner id
               gid :: binary(),               % group id
               muid :: binary()}).            % last modifier uid

-type file9p() :: #file9p{}.
-export_type([file9p/0]).

-spec make(Type::qid_type(), Name::binary(),
           Mode::pos_integer(), Uid::binary(),
           Gid::binary()) -> file9p().
make(Type, Name, Mode, Uid, Gid) ->
  Qid = make_qid(Type),
  #file9p{name=Name, qid=Qid, mode=Mode, uid=Uid, gid=Gid}.

-spec mode(file9p()) -> pos_integer().
mode(#file9p{mode=Mode}) ->
  Mode.

-spec mode(file9p(), pos_integer()) -> file9p().
mode(#file9p{}=File, Mode) ->
  File#file9p{mode=Mode}.

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

-spec is_directory(file9p()) -> boolean().
is_directory(#file9p{qid=Qid}) ->
  Qid#qid.type == ?DirType.

%% Utils

-spec make_qid(Type::qid_type()) -> qid().
make_qid(Type) ->
  #qid{type=Type,
       version=1,
       path=crypto:rand_bytes(8)}.
