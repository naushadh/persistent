## 0.2.1.0
- Bumped up version to update README.

## 0.2.0.0
- Added APIs for setting port number and character encoding.
- Updated type signature for mkMySQLConnectInfo to align with mysql-haskell.

## 0.1.1.0
- Bumped up version to include README and example.

## 0.1.0.0

* Ported persistent-mysql 2.6 to use mysql-haskell as the underlying database driver.
* Deprecated MySQLConf and ConnectInfo native constructor and default instance in favor of mk functions for better forward compatibility. See http://www.snoyman.com/blog/2016/11/designing-apis-for-extensibility.
