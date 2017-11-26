pragma License (Unrestricted); -- BSD 3-Clause
--  translated unit from SFMT (SFMT-common.h)
private generic
package Ada.Numerics.SFMT.Generating is
   --  no SIMD version
   pragma Preelaborate;

   procedure gen_rand_all (
      sfmt : in out w128_t_Array_N);
   pragma Inline (gen_rand_all);

   procedure gen_rand_array (
      sfmt : in out w128_t_Array_N;
      Item : in out w128_t_Array_1; -- w128_t_Array (0 .. size - 1)
      size : Integer);
   pragma Inline (gen_rand_array);

end Ada.Numerics.SFMT.Generating;
