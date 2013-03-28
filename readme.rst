Drake The Runtime Library for gcc-Ada (GNAT)
============================================

What's this?
------------

Drake is a runtime library written in 100% Ada to replace GNAT runtime.

Environment
-----------

- MacOSX 10.5/10.6 (x86, 32bit/64bit)
- FreeBSD 7 (x86, 32bit only)
- Linux (x86, 32bit only) [#experimental]_
- Windows (x86, 32bit only) [#experimental]_

Depending tools and libraries
-----------------------------

gcc
 http://gcc.gnu.org/

headmaster (or pre-translated headers)
 headmaster
  http://github.com/ytomino/headmaster
 pre-translated headers
  - `for MacOSX 10.6 (x86, 64bit)
    <https://raw.github.com/wiki/ytomino/drake/import-x86_64-apple-darwin10.tar.bz2>`_
  - `for MacOSX 10.5 (x86, 32bit)
    <https://raw.github.com/wiki/ytomino/drake/import-i686-apple-darwin9.tar.bz2>`_
  - `for FreeBSD 7 (x86, 32bit)
    <https://raw.github.com/wiki/ytomino/drake/import-i686-pc-freebsd7.tar.bz2>`_
  - **experimental** `for Linux (x86, 32bit)
    <https://raw.github.com/wiki/ytomino/drake/import-i686-pc-linux-gnu.tar.bz2>`_
  - **experimental** `for Windows (x86, 32bit, from mingw-w64 headers)
    <https://raw.github.com/wiki/ytomino/drake/import-i686-w64-mingw32.tar.bz2>`_

Downloads
---------

for gcc-4.8
 ``git clone git://github.com/ytomino/drake.git``

for gcc-4.7
 ``git clone git://github.com/ytomino/drake.git -b gcc-4.7``

for gcc-4.6
 ``git clone git://github.com/ytomino/drake.git -b gcc-4.6``

for gcc-4.5
 ``git clone git://github.com/ytomino/drake.git -b gcc-4.5``

Short Example
-------------

1. Ready source code of a sample application. ::
   
    $ cat > hello.adb
    with Ada.Text_IO;
    procedure hello is
    begin
       Ada.Text_IO.Put_Line ("Hello, Drake runtime!");
    end hello;
    ^D

2. Build drake. ::
   
    $ make -C ~/Downloads/drake/source \ # drake source path
      IMPORTDIR=~/Downloads/import-i686-apple-darwin9 \ # translated headers path
      RTSDIR=$PWD/rts-drake # destination path

3. Build the sample application. ::
   
    $ gnatmake hello.adb --RTS=rts-drake

4. Run the sample application. ::
   
    $ ./hello
    Hello, Drake runtime!

Limitations
-----------

Many many features are unimplemented !!!

And there are several intentional ACATS violations for usability.
See incompatibility_.

.. _incompatibility: https://github.com/ytomino/drake/wiki/Incompatibility
.. [#experimental] Linux and Windows support is under construction,
                   limited and experimental.
