# persistent-mysql-haskell

![hackage version](https://img.shields.io/hackage/v/persistent-mysql-haskell.svg)

A pure haskell backend for [persistent](https://github.com/yesodweb/persistent) using the MySQL database server.
Internally it uses the [mysql-haskell](https://github.com/winterland1989/mysql-haskell) driver in order to access the database.

See [example/Main.hs](example/Main.hs) for how this MySQL backend can be used with Persistent.

### Motivation

`persistent-mysql` uses [mysql](https://hackage.haskell.org/package/mysql) (via [mysql-simple](https://hackage.haskell.org/package/mysql-simple)) as the database driver. `mysql` is a haskell FFI wrapper for `mysqlclient` written in C.

Reasons to use a pure haskell driver:

- `mysql` has [concurrency issues](https://ro-che.info/articles/2015-04-17-safe-concurrent-mysql-haskell) as noted by [@feuerbach](https://github.com/feuerbach).

- [mysql-haskell](https://hackage.haskell.org/package/mysql-haskell), a pure haskell driver by [@winterland1989](https://github.com/winterland1989), outperforms `mysql-simple` in benchmarks (see hackage or project repo).

- better portability and possible static compilation of an entire project that uses `persistent-mysql`.


Personal experience on replacing `mysql-simple` with `mysql-haskell` in a project:

- Performance gains consistent with benchmark.

- Smoother deployment to [Amazon AMI/EC2](https://en.wikipedia.org/wiki/Amazon_Machine_Image), since `mysql` appears to have a hard dependency on the oracle version of `libmysqlclient` that does not work with the open source variant that is available by default on EC2 (and possibly on other cloud providers).

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

Aside from connection configuration, persistent-mysql-haskell is functionally on par with persistent-mysql (as of writing this). This can be seen by [comparing persistent-test between this fork and upstream](https://github.com/yesodweb/persistent/compare/master...naushadh:persistent-mysql-haskell#diff-028f5df7b2b9c5c8b0fa670fc8c69bff).

### FAQs

#### Why isn't this part of the main/upstream persistent repo?

- TLDR: Upstream wants to gauge community interest before absorbing this backend into the main repo.
- Long version: See [issue yesodweb/persistent/issues/659](https://github.com/yesodweb/persistent/issues/659).

#### persistent-mysql supports X but persistent-mysql-haskell API doesn't. Why?

- Internals (getters/setters) of MySQLConnectInfo and `defaultConnectInfo` are intentionally masked for [forward compatibility](http://www.snoyman.com/blog/2016/11/designing-apis-for-extensibility).

- For all others, feel free to open an issue and/or submit a PR.

#### Does persistent-mysql-haskell ship with tests?

- It does! :) `persistent-test` is fully re-used with an additional flag to specifically test persistent-mysql-haskell.

    ```bash
    stack test persistent-test --flag persistent-test:mysql_haskell
    ```