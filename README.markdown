[![Build Status](https://travis-ci.org/zbskii/haskell-bcrypt.svg?branch=master)](https://travis-ci.org/zbskii/haskell-bcrypt)

haskell-bcrypt
==============

A simple haskell wrapper around the OpenBSD bcrypt implementation.

Building
--------

cabal configure; cabal build; cabal install

Example
-------

See examples/Main.hs


To reduce external dependencies, a secure random number generator wasn't
included.  genSalt needs a random 16 byte string as a seed.  Using a known
secure random number generator such as OpenSSL.Random is recommended.