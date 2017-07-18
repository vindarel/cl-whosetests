# Clwhosconnected

Are my buddies connected to this site ?


## Usage

    (get-all-connected)

## Configuration

A lisp init file: `init.lisp` (at the project root as for now). It must start with

    (in-package :clwhosconnected)

A text config file, `data.txt`, that accepts space-separated
strings. The first one is either an url either a name that will be
appended to `*base-url*`, then come keywords:

    http://mysite.com/foo/player awesome
    otherplayer keyword

## Installation
