# ChangeLog for `persistent-mysql-haskell`

## 0.6.0

- Port [#977](https://github.com/yesodweb/persistent/pull/977) from `persistent-mysql`: Support Stackage Nightly
- Added `constraint=` attribute to allow users to specify foreign reference constraint names.
- Port [#894](https://github.com/yesodweb/persistent/pull/894) from `persistent-mysql`: Remove deprecated `SomeField` type and pattern synonym. Use `HandleUpdateCollision` type instead and the `copyField` function instead of `SomeField`   constructor/pattern.

## 0.5.2

- Fix [stackage#4312](https://github.com/commercialhaskell/stackage/issues/4312): Relax `network` version bound.

## 0.5.1

- [#9](https://github.com/naushadh/persistent/pull/9) Add `insertEntityOnDuplicateKeyUpdate` and `insertEntityManyOnDuplicateKeyUpdate` functions

## 0.5.0

- Port [#812](https://github.com/yesodweb/persistent/pull/812) from `persistent-mysql`: Add support for SQL isolation levels
- Port [#833](https://github.com/yesodweb/persistent/pull/833) from `persistent-mysql`: `repsertMany` now matches `mapM_ (uncurry repsert)` and is atomic.

## 0.4.2

- [#7](https://github.com/naushadh/persistent/pull/7) Fix boolean conversion bug.

## 0.4.1

- Fix [#2](https://github.com/naushadh/persistent/issues/2): Better compatibility with `yesod` scaffold.

## 0.4.0

- Port [#770](https://github.com/yesodweb/persistent/pull/770) from `persistent-mysql`: Performance enhancements for bulk writes.
- Port [#773](https://github.com/yesodweb/persistent/pull/773) from `persistent-mysql`: Support new conduit release. Includes bundled changes from other PRs.
  - [#723](https://github.com/yesodweb/persistent/pull/723) More BackendCompatible generalizations.
  - [#760](https://github.com/yesodweb/persistent/pull/760) Rename SomeField type to HandleUpdateCollision.

## 0.3.6

- Port [#752](https://github.com/yesodweb/persistent/pull/754) from `persistent-mysql`: Fix mysql sqltype migrations.

## 0.3.5

- Updated `selectSource` implementation to stream results instead of loading everything into memory.

## 0.3.4.1

- Fix a haddock issue down-streamed from [#693](https://github.com/yesodweb/persistent/pull/693).

## 0.3.4

- Port [#693](https://github.com/yesodweb/persistent/pull/693) from `persistent-mysql`: Extend the `SomeField` type to allow `insertManyOnDuplicateKeyUpdate` to conditionally copy values.
- Port [#702](https://github.com/yesodweb/persistent/pull/702) from `persistent-mysql`: Fix behavior of `insertManyOnDuplicateKeyUpdate` to ignore duplicate key exceptions when no updates specified.
- Bumped TLS bounds to be in [sync with `mysql-haskell`](https://github.com/winterland1989/mysql-haskell/pull/15) and land ourselves [back on stackage](https://github.com/fpco/stackage/pull/2956).

## 0.3.3

- Port from `persistent-mysql`: MySQL on duplicate key update [#674](https://github.com/yesodweb/persistent/pull/674).

## 0.3.2.1

- Port from `persistent-mysql`: Prevent spurious no-op migrations when `default=NULL` is specified - revised version [#672](https://github.com/yesodweb/persistent/pull/672) (which fixes bug [#671](https://github.com/yesodweb/persistent/issues/671) introduced by the earlier attempt [#641](https://github.com/yesodweb/persistent/pull/641)).

## 0.3.2.0

- Added conditional declaration of `Show` instance for mysql-haskell's `ConnectInfo` for compatibility with `mysql-haskell-0.8.1.0+`.

## 0.3.1.0

- Fixed compiler warnings in `stack --pedantic` mode so the project can run upstream tests on Travis.
- Minor README enhancements for badges and fixed URL for example when viewing outside of Github.

## 0.3.0.0

- Added API for setting [TLS client parameters](https://hackage.haskell.org/package/mysql-haskell-0.8.0.0/docs/Database-MySQL-TLS.html) for secure MySQL connections.
- Exported [Data.TLSSetting](https://hackage.haskell.org/package/tcp-streams-1.0.0.0/docs/Data-TLSSetting.html) for convenient usage of TLS.

## 0.2.1.0

- Bumped up version to update README.

## 0.2.0.0

- Added APIs for setting port number and character encoding.
- Updated type signature for mkMySQLConnectInfo to align with mysql-haskell.

## 0.1.1.0

- Bumped up version to include README and example.

## 0.1.0.0

- Ported persistent-mysql 2.6 to use mysql-haskell as the underlying database driver.
- Deprecated MySQLConf and ConnectInfo native constructor and default instance in favor of mk functions for better forward compatibility. See http://www.snoyman.com/blog/2016/11/designing-apis-for-extensibility.
