pragma License (Unrestricted); -- BSD 3-Clause
--  translated unit from dSFMT (dSFMT-common.h, dSFMT.c)
private generic
package Ada.Numerics.dSFMT.Generating is
   --  SSE2 version
   pragma Preelaborate;

   procedure do_recursion (
      r : aliased out w128_t;
      a, b : aliased w128_t;
      lung : aliased in out w128_t)
      with Convention => Intrinsic;
   pragma Inline_Always (do_recursion);

   procedure convert_c0o1 (w : aliased in out w128_t)
      with Convention => Intrinsic;
   procedure convert_o0c1 (w : aliased in out w128_t)
      with Convention => Intrinsic;
   procedure convert_o0o1 (w : aliased in out w128_t)
      with Convention => Intrinsic;

   pragma Inline_Always (convert_c0o1);
   pragma Inline_Always (convert_o0c1);
   pragma Inline_Always (convert_o0o1);

end Ada.Numerics.dSFMT.Generating;
