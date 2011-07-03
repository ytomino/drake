Drake The Runtime Library for gcc-Ada (GNAT)
============================================

What's this?
------------

Drake is a runtime library written in 100% Ada to replace GNAT runtime.

Environment
-----------

- MacOSX 10.5 (x86, 32bit only)
- FreeBSD 7 (x86, 32bit only)

Depending tools and libraries
-----------------------------

gcc
 http://gcc.gnu.org/

 | gcc should be configured with
 |  ``--enable-languages=c,ada``
 |  ``--target=i686-apple-darwin9`` or ``--target=i686-pc-freebsd7``

headmaster (or pre-translated headers)
 headmaster
  http://github.com/ytomino/headmaster
 pre-translated headers
  - `for MacOSX 10.5 (x86, 32bit)
    <https://github.com/downloads/ytomino/drake/import-i686-apple-darwin9.zip>`_
  - `for FreeBSD 7 (x86, 32bit)
    <https://github.com/downloads/ytomino/drake/import-i686-pc-freebsd7.zip>`_

Downloads
---------

for gcc-4.6
 ``git clone git://github.com/ytomino/drake.git``

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
For example, all functions treat String as UTF-8, Wide_String as UTF-16,
and Wide_Wide_String as UTF-32.
This behavior violates standard, but useful for real-world applications.
