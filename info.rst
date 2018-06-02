===================
License Information
===================

The most files are distributed under `MIT License`_.

This repository also includes some files derived from other softwares.
Such files have inherited their original licenses.

* gcc_, distributed under `GPL version 3`_ with `GCC Runtime Library Exception`_
  
  * ``source/zcx/unwind-pe.h``
  * ``source/zcx/c-unwind_pe.ads``, ``c-unwind_pe.adb`` \
    (translated from ``unwind-pe.h``)
  
* ICU_, distributed under `ICU License`_
  
  * ``source/nls/machine-apple-darwin/c-icucore.ads`` \
    (translated from the header files of ICU)
  
* MT19937_, SFMT_, and dSFMT_, distributed under `BSD-like LICENSE.txt`__
  
  * ``examples/random_dsfmt.adb`` \
    (translated from ``test.c`` of dSFMT)
  * ``examples/random_mt19937.adb`` \
    (translated from ``mtTest.c`` of MT19937)
  * ``examples/random_sfmt.adb`` \
    (translated from ``test.c`` of SFMT)
  * ``source/numerics/a-nuds19.ads`` \
    (translated from ``dSFMT-params19937.h``)
  * ``source/numerics/a-nuds21.ads`` \
    (translated from ``dSFMT-params216091.h``)
  * ``source/numerics/a-numdsf.ads``, ``source/numerics/a-numdsf.adb``
    (translated from ``dSFMT.h``, ``dSFMT-params.h``, ``dSFMT.c``)
  * ``source/numerics/a-nummt1.ads``, ``a-nummt1.adb`` \
    (translated from ``mt19937ar.h``, ``mt19937ar.c``)
  * ``source/numerics/a-numsfm.ads``, ``source/numerics/a-numsfm.adb``
    (translated from ``SFMT.h``, ``SFMT.c``)
  * ``source/numerics/a-nusf19.ads`` \
    (translated from ``SFMT-params19937.h``)
  * ``source/numerics/a-nusf21.ads`` \
    (translated from ``SFMT-params216091.h``)
  * ``source/numerics/generic/a-nudsge.ads``, ``a-nudsge.adb``
    (translated from ``dSFMT-common.h``)
  * ``source/numerics/generic/a-nusfge.ads``, ``a-nusfge.adb``
    (translated from ``SFMT-common.h``)
  * ``source/numerics/x86_64/a-nudsge.ads``, ``a-nudsge.adb``
    (translated from ``dSFMT-common.h``)
  * ``source/numerics/x86_64/a-nusfge.ads``, ``a-nusfge.adb``
    (translated from ``SFMT-sse2.h``)
  
* MurmurHash3_, distributed under public domain
  
  * ``source/hash/a-cmuha3.ads``, ``a-cmuha3.adb`` \
    (translated from ``MurmurHash3.cpp``)
  
* XNU_, distributed under `Apple Public Source License Version 2.0`_
  
  * ``source/directories/machine-apple-darwin/hfs_CaseTables.h``
  * ``source/directories/machine-apple-darwin/c-hfs_casetables.ads`` \
    (translated from ``hfs_CaseTables.h``)
  * ``source/directories/machine-apple-darwin/vfs_utfconvdata.h``
  * ``source/directories/machine-apple-darwin/c-vfs_utfconvdata.ads`` \
    (translated from ``vfs_utfconvdata.h``)

In addition, automatic translated headers by headmaster have same license of
their original header files because preprocessor does not change any license.

__ `MT LICENSE.txt`_

.. _`MIT License`: https://github.com/ytomino/drake/blob/master/LICENSE
.. _gcc: https://gcc.gnu.org/
.. _`GPL version 3`: https://gcc.gnu.org/svn/gcc/trunk/COPYING3
.. _`GCC Runtime Library Exception`: https://gcc.gnu.org/svn/gcc/trunk/COPYING.RUNTIME
.. _ICU: http://site.icu-project.org/
.. _`ICU License`: http://www.unicode.org/copyright.html#License
.. _MT19937: http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/MT2002/emt19937ar.html
.. _SFMT: http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/SFMT/
.. _dSFMT: http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/SFMT/#dSFMT
.. _`MT LICENSE.txt`: http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/SFMT/LICENSE.txt
.. _MurmurHash3: https://github.com/aappleby/smhasher
.. _XNU: https://opensource.apple.com/
.. _`Apple Public Source License Version 2.0`: https://opensource.apple.com/apsl
