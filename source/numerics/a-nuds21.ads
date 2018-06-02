pragma License (Unrestricted); -- BSD 3-Clause
--  translated unit from dSFMT (dSFMT-params216091.h)
with Ada.Numerics.dSFMT;
package Ada.Numerics.dSFMT_216091 is
   new dSFMT (
      MEXP => 216091,
      POS1 => 1890,
      SL1 => 23,
      MSK1 => 16#000bf7df7fefcfff#,
      MSK2 => 16#000e7ffffef737ff#,
      FIX1 => 16#d7f95a04764c27d7#,
      FIX2 => 16#6a483861810bebc2#,
      PCV1 => 16#3af0a8f3d5600000#,
      PCV2 => 16#0000000000000001#);
--  The largest periods.
pragma Preelaborate (Ada.Numerics.dSFMT_216091);
