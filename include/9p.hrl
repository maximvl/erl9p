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


%% Qid Types

-define(QTDIR    , 80).
-define(QTAPPEND , 40).
-define(QTEXCL   , 20).
-define(QTMOUNT  , 10).
-define(QTAUTH   , 8).
-define(QTTMP    , 4).
-define(QTLINK   , 2).
-define(QTFILE   , 0).
