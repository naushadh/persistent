{-# LANGUAGE CPP #-}

#if !MIN_VERSION_mysql_haskell(0,8,1)
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
#endif

{-|
Module      : Database.Persist.MySQLConnectInfoShowInstance
Description : Conditionally adding Show instance for mysql-haskell's ConnectInfo

As of mysql_haskell-0.8.1.0, mysql-haskell's ConnectInfo ships _with_ a Show instance.
However, for earlier versions, we must supply our own instance.
We have a stand-alone package since CPP flags do not like multi-line string literals (widely used in sibling module).
-}

module Database.Persist.MySQLConnectInfoShowInstance where

#if !MIN_VERSION_mysql_haskell(0,8,1)
import qualified Database.MySQL.Base    as MySQL
deriving instance Show MySQL.ConnectInfo
#endif