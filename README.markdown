This is a testing ground for programs that play [go](http://en.wikipedia.org/wiki/Go_(game)), creating a bridge via the [Go Text Protocol](http://www.lysator.liu.se/~gunnar/gtp/gtp2-spec-draft2/gtp2-spec.html).

### Usage

Duel assumes that [GnuGo](http://www.gnu.org/software/gnugo/) is installed.  To install GnuGo, follow these steps:

```
wget https://github.com/downloads/ztellman/duel/gnugo-3.9.1.tar.gz
tar xvf gnugo-3.9.1.tar.gz
cd gnugo-3.9.1
./configure
make
make install
```

### License

Copyright (C) 2012 Zachary Tellman

Distributed under the Eclipse Public License, the same as Clojure.
