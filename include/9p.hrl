
%% 9p message tags

-define(Tversion , 100).
-define(Rversion , 101).

-define(Tauth    , 102).
-define(Rauth    , 103).

-define(Tattach  , 104).
-define(Rattach  , 105).

-define(Rerror   , 107).

-define(Tflush   , 108).
-define(Rflush   , 109).

-define(Twalk    , 110).
-define(Rwalk    , 111).

-define(Topen    , 112).
-define(Ropen    , 113).

-define(Tcreate  , 114).
-define(Rcreate  , 115).

-define(Tread    , 116).
-define(Rread    , 117).

-define(Twrite   , 118).
-define(Rwrite   , 119).

-define(Tclunk   , 120).
-define(Rclunk   , 121).

-define(Tremove  , 122).
-define(Rremove  , 123).

-define(Tstat    , 124).
-define(Rstat    , 125).

-define(Twstat   , 126).
-define(Rwstat   , 127).

-define(NoTag, <<255,255>>).
-define(NoFid, <<255,255,255,255>>).
-define(NoUid, <<255,255,255,255>>).


%% Qid Types

-define(DirType    , 16#80).
-define(AppendType , 16#40).
-define(ExclType   , 16#20).
-define(MountType  , 16#10).
-define(AuthType   , 16#08).
-define(TmpType    , 16#04).
-define(LinkType   , 16#02).
-define(FileType   , 16#00).

-type qid_type() :: ?DirType    |
                    ?AppendType |
                    ?ExclType   |
                    ?MountType  |
                    ?AuthType   |
                    ?TmpType    |
                    ?LinkType   |
                    ?FileType.

-type qid_version() :: non_neg_integer().
-type qid_path() :: <<_:64>>.

-record(qid, {type    :: qid_type(),
              version :: qid_version(),
              path    :: qid_path()}).

-type qid() :: #qid{}.

-define(DirMode       , 16#80000000).
-define(AppendMode    , 16#40000000).
-define(ExclMode      , 16#20000000).
-define(MountMode     , 16#10000000).
-define(AuthMode      , 16#08000000).
-define(TmpMode       , 16#04000000).
%% 9P2000.u extensions
-define(LinkMode      , 16#02000000).
-define(DeviceMode    , 16#00800000).
-define(NamedPipeMode , 16#00200000).
-define(SocketMode    , 16#00100000).
-define(SetUIDMode    , 16#00080000).
-define(SetGIDMode    , 16#00040000).

-type mod_type() :: ?DirMode       |
                    ?AppendMode    |
                    ?ExclMode      |
                    ?MountMode     |
                    ?AuthMode      |
                    ?TmpMode       |
                    ?LinkMode      |
                    ?DeviceMode    |
                    ?NamedPipeMode |
                    ?SocketMode    |
                    ?SetUIDMode    |
                    ?SetGIDMode.

-define(IOUnit, 8168).
