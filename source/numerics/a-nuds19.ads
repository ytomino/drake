pragma License (Unrestricted); -- BSD 3-Clause
--  translated unit from dSFMT (dSFMT-params19937.h)
with Ada.Numerics.dSFMT;
package Ada.Numerics.dSFMT_19937 is
   new dSFMT (
      MEXP => 19937,
      POS1 => 117,
      SL1 => 19,
      MSK1 => 16#000ffafffffffb3f#,
      MSK2 => 16#000ffdfffc90fffd#,
      FIX1 => 16#90014964b32f4329#,
      FIX2 => 16#3b8d12ac548a7c7a#,
      PCV1 => 16#3d84e1ac0dc82880#,
      PCV2 => 16#0000000000000001#);
--  For normal use.
pragma Preelaborate (Ada.Numerics.dSFMT_19937);
