Drake The Runtime Library for gcc-Ada
=====================================

What's this?
------------

Drake is a runtime library written in 100% Ada to replace GNAT runtime.

Please, read wiki_ for more information.

Environment
-----------

- Darwin (from 10.5, x86, 32bit/64bit)
- FreeBSD (from 7, x86, 32bit/64bit)
- Linux (from 2.6, x86, 32bit/64bit) [#experimental]_
- Windows (from XP, x86, 32bit only) [#experimental]_

Depending tools and libraries
-----------------------------

gcc
 Use gcc_ *instead of GNAT GPL Edition*.
translated headers
 Install headmaster_ to convert the headers on your system,
 or download them from `pre-translated headers page`_.

Downloads
---------

for gcc-4.9 ::

 $ git clone git://github.com/ytomino/drake.git

for old gcc (from 4.5 until 4.8) ::

 $ VERSION=$(expr $(gcc -dumpversion) : '^\([0-9]*\.[0-9]*\)') # X.Y
 $ git clone git://github.com/ytomino/drake.git -b gcc-$VERSION

How to build
------------

Run ``make`` command with some variables, specify a translated headers path
to ``IMPORTDIR`` and a destination path to ``RTSDIR``. ::

 $ make IMPORTDIR=... RTSDIR=...

Read the `build page`_ for details.

Short example
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
      IMPORTDIR=~/Downloads/i686-apple-darwin9 \ # translated headers path
      RTSDIR=$PWD/rts-drake # destination path

3. Build the sample application. ::
   
    $ gnatmake hello.adb --RTS=rts-drake

   Specify ``--RTS`` option of ``gnatmake`` to use the alternative runtime.

4. Run the sample application. ::
   
    $ ./hello
    Hello, Drake runtime!

Limitations
-----------

Many many features are unimplemented !!!

And there are several intentional ACATS violations for usability.
See `incompatibility features page`_.

.. _gcc: http://gcc.gnu.org/
.. _headmaster: http://github.com/ytomino/headmaster
.. _wiki: https://github.com/ytomino/drake/wiki
.. _`pre-translated headers page`: https://github.com/ytomino/drake/wiki/Pre-translated-headers
.. _`build page`: https://github.com/ytomino/drake/wiki/Build
.. _`incompatibility features page`: https://github.com/ytomino/drake/wiki/Incompatibility
.. [#experimental] Linux and Windows support is under construction,
                   limited and experimental.
