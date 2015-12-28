corecursion.net repository
==========================

This is [www.corecursion.net](http://www.corecursion.net) code.

While it's possible to build this webapp only with Cabal, it's strongly recommended to use [stack](http://docs.haskellstack.org/en/stable/README.html) instead.

Purpose
-------
The goal of this project is to provide RealWorld yet simple [Yesod](http://www.yesodweb.com/) and [EventStore](https://geteventstore.com/) backed web application.

Project layout
--------------

1. boostrap: a command-line utility that initializes a  [EventStore](https://geteventstore.com/) database. Currently, it only creates an author user.
2. webapp: the main Yesod web application.

Requirements
------------
Normally, this code *should* work on any OS supported by GHC. Nonetheless, the website runs on a Linux distro.

The webapp uses an [EventStore](https://geteventstore.com/) database. Any version greater or equal to 3.0.0 would work.

How to build
------------

```
$ git clone https://github.com/YoEight/corecursion-net.git
$ cd corecursion-net
$ stack setup
$ stack build corecursion
```

Development mode
----------------
In order to have a cheerful Yesod experience, it's strongly advise to install [yesod-bin](https://hackage.haskell.org/package/yesod-bin). `stack` makes everything simple, this is how to proceed:

```
$ stack build yesod-bin
$ cd webapp
$ stack exec yesod devel
```

Now every change made on webapp files will trigger compilation. It makes web developement in Haskell interactive.

How to use Bootstrap
--------------------

First you need to compile `bootstrap`:

```
$ stack build bootstrap
```

You could either call `bootstrap` directly (where `stack` puts binaries which depends on your setup) or use `stack` itself.

```
$ ./bootstrap --help
bootstrap - Operates on corecursion.net database.

Usage: bootstrap [--ip IP] [--port PORT] [-l|--store-login STORE_LOGIN]
                 [-p|--store-passw STORE_PASSW] COMMAND
  Initialize corecursion.net database.

Available options:
  -h,--help                Show this help text
  --ip IP                  EventStore's IP address. (default: "127.0.0.1")
  --port PORT              EventStore's port. (default: 1113)
  -l,--store-login STORE_LOGIN
                           Store user login. (default: No admin login used.)
  -p,--store-passw STORE_PASSW
                           Store user
                           password. (default: No admin password used.)

Available commands:
  useradd                  Add a new author user.
  maxage                   Set stream $maxAge property
```

For example, to add an author user:
```
$ ./bootstrap useradd --username author@email.com
```

or `stack` version:

```
$ stack exec -- bootstrap useradd --username author@email.com
```

Notes
-----
Contributions and bug reports are welcome!

GPL License

-Yorick Laupa
