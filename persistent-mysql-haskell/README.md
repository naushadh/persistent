# persistent-mysql-haskell

[![hackage version](https://img.shields.io/hackage/v/persistent-mysql-haskell.svg)](https://hackage.haskell.org/package/persistent-mysql-haskell)
[![Build Status](https://travis-ci.org/naushadh/persistent.svg?branch=persistent-mysql-haskell)](https://travis-ci.org/naushadh/persistent)

A pure haskell backend for [persistent](https://github.com/yesodweb/persistent) using the MySQL database server.
Internally it uses the [mysql-haskell](https://github.com/winterland1989/mysql-haskell) driver in order to access the database.

See [example/Main.hs](https://github.com/naushadh/persistent/blob/persistent-mysql-haskell/persistent-mysql-haskell/example/Main.hs) for how this MySQL backend can be used with Persistent.

### Motivation

`persistent-mysql` uses [mysql](https://hackage.haskell.org/package/mysql) (via [mysql-simple](https://hackage.haskell.org/package/mysql-simple)) as the database driver. `mysql` is a haskell FFI wrapper for `mysqlclient` written in C.

Reasons to use a pure haskell driver:

- `mysql` has [concurrency issues](https://ro-che.info/articles/2015-04-17-safe-concurrent-mysql-haskell) as noted by [@feuerbach](https://github.com/feuerbach).

- [mysql-haskell](https://hackage.haskell.org/package/mysql-haskell), a pure haskell driver by [@winterland1989](https://github.com/winterland1989), outperforms `mysql-simple` in benchmarks (see hackage or project repo).

- better portability and possible static compilation of an entire project that uses `persistent-mysql`.

- result streaming support means persistent [`selectSource` streams data from database](http://www.jakubkonka.com/2014/01/23/conduit-haskell.html). Effectively addressing [#657 selectSource does not stream results](https://github.com/yesodweb/persistent/issues/657).

- a newtype-d `MySQLConnectInfo` allows adding configuring _how_ persistent and the underlying driver are glued. Ex: [#679](https://github.com/yesodweb/persistent/issues/679) can be elegantly addressed in this library.

Personal experience on replacing `mysql-simple` with `mysql-haskell` in a project:

- Performance gains consistent with benchmark.

- Smoother deployment to [AWS](https://en.wikipedia.org/wiki/Amazon_Machine_Image), since `mysql` appears to have a hard dependency on the oracle version of `libmysqlclient` that does not work with the open source variant that is available by default on Amazon Linux (and possibly on other Linux distros).

### Potential issues moving from persistent-mysql to persistent-mysql-haskell

`ConnectInfo` and `defaultConnectInfo` are not the same between `mysql` and `mysql-haskell`, therefore this package is not a 100% drop in replacement for persistent-mysql from the connection configuration perspective.

- `mysql-haskell` does not allow provide an API for the entirety of [mysqlclient options](https://hackage.haskell.org/package/mysql-0.1.4/docs/Database-MySQL-Base.html#t:Option). Therefore neither can this package.

- Given the inevitable incompatibility with `persistent-mysql`, and in the interest of [providing a forward-compatible API](http://www.snoyman.com/blog/2016/11/designing-apis-for-extensibility), `ConnectInfo` internals and `defaultConnectInfo` have been deprecated. However the similar utility can be achieved like so:

    ```diff
    import Database.Persist.MySQL

    connectInfo :: MySQLConnectInfo
    - connectInfo = defaultConnectInfo
    -             { connectHost     = "localhost"
    -             , connectUser     = "test"
    -             , connectPassword = "test"
    -             , connectDatabase = "test"
    -             }
    + connectInfo = mkMySQLConnectInfo "localhost" "test" "test" "test"

    connectInfoNewPort :: MySQLConnectInfo
    - connectInfoNewPort = connectInfo { connectPort = 3307 }
    + connectInfoNewPort = setMySQLConnectInfoPort 3307 connectInfo

    connectInfoNewCharSet :: MySQLConnectInfo
    - connectInfoNewCharSet = connectInfo { connectOptions = [CharsetName "utf8"] }
    + connectInfoNewCharSet = setMySQLConnectInfoCharset 33 connectInfo

    ```

- `mysql-haskell` and `mysql` have different APIs/mechanisms for securing the
connection to MySQL. `persistent-mysql-haskell` exposes an API to utilize
[TLS client params](https://hackage.haskell.org/package/mysql-haskell/docs/Database-MySQL-TLS.html)
that ships with `mysql-haskell`.

    ```diff
    connectInfoCustomCaStore :: MySQLConnectInfo
    - connectInfoCustomCaStore = connectInfo { connectSSL = Just customCaParams }
    + connectInfoCustomCaStore = setMySQLConnectInfoTLS customCaParams connectInfo
        where
    -         customCaParams = defaultSSLInfo { sslCAPath = "foobar.pem" }
    +         customCaParams = makeClientParams $ CustomCAStore "foobar.pem"
    ```


Aside from connection configuration, persistent-mysql-haskell is functionally on par with persistent-mysql (as of writing this). This can be seen by [comparing persistent-test between this fork and upstream](https://github.com/yesodweb/persistent/compare/master...naushadh:persistent-mysql-haskell#diff-028f5df7b2b9c5c8b0fa670fc8c69bff).

#### Yesod

In order to use `persistent-mysql-haskell` with `yesod` you have to modify `Settings.hs`:

  ```diff
  - import Database.Persist.MySQL     (MySQLConf (..))
  + import Database.Persist.MySQL     (MySQLConf, mkMySQLConf, myConnInfo, myPoolSize, setMySQLConnectInfoCharset)
  ```

  ```diff
  - import qualified Database.MySQL.Base as MySQL
  ```

  ```diff
  -         -- This code enables MySQL's strict mode, without which MySQL will truncate data.
  -         -- See https://github.com/yesodweb/persistent/wiki/Database-Configuration#strict-mode for details
  -         -- If you choose to keep strict mode enabled, it's recommended that you enable it in your my.cnf file so that it's also enabled for your MySQL console sessions.
  -         -- (If you enable it in your my.cnf file, you can delete this code).
  -         let appDatabaseConf = fromYamlAppDatabaseConf { myConnInfo = (myConnInfo fromYamlAppDatabaseConf) {
  -                 MySQL.connectOptions =
  -                   ( MySQL.connectOptions (myConnInfo fromYamlAppDatabaseConf)) ++ [MySQL.InitCommand "SET SESSION sql_mode = 'STRICT_ALL_TABLES';\0"]
  -               }
  -             }
  ```

And in `Application.hs`:

  ```diff
  - import qualified Database.MySQL.Base as MySQL
  ```

  ```diff
    import Network.Wai.Handler.Warp             (Settings, defaultSettings,
                                                 defaultShouldDisplayException,
                                                 runSettings, setHost,
  -                                              setFork, setOnOpen, setOnClose,
  +                                              setFork,
                                                 setOnException, setPort, getPort)
  ```

  ```diff
  -     -- See http://www.yesodweb.com/blog/2016/11/use-mysql-safely-in-yesod
  -     MySQL.initLibrary
  ```

  ```diff
  -     $ setOnOpen (const $ MySQL.initThread >> return True)
  -     $ setOnClose (const MySQL.endThread)
  ```

Optionally you may enable the MYSQL strict mode (in each transaction)
by modifying `Foundation.hs` (or editing the `my.cnf` server configuration):

  ```diff
  - import Database.Persist.Sql (ConnectionPool, runSqlPool)
  + import Database.Persist.Sql (ConnectionPool, rawExecute, runSqlPool)
  ```

  ```diff
  -         runSqlPool action $ appConnPool master
  +         runSqlPool
  +           (rawExecute "SET SESSION sql_mode = 'STRICT_ALL_TABLES'" [] >> action)
  +           (appConnPool master)
  ```

### FAQs

#### Why isn't this part of the main/upstream persistent repo?

- TLDR: Upstream wants to gauge community interest before absorbing this backend into the main repo.
- Long version: See [issue yesodweb/persistent/issues/659](https://github.com/yesodweb/persistent/issues/659).

#### persistent-mysql supports X but persistent-mysql-haskell API doesn't. Why?

- Internals (getters/setters) of MySQLConnectInfo and `defaultConnectInfo` are intentionally masked for [forward compatibility](http://www.snoyman.com/blog/2016/11/designing-apis-for-extensibility).

- For all others, feel free to open an issue and/or submit a PR.

#### Does persistent-mysql-haskell ship with tests?

- It does! :) `persistent-test` is fully re-used with an additional flag to specifically test persistent-mysql-haskell.

    - [CI/Travis](https://travis-ci.org/naushadh/persistent), see [.travis.yml](../.travis.yml).

    - Local,
    ```bash
    stack test persistent-test --flag persistent-test:mysql_haskell --exec persistent-test
    ```