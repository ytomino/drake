pragma License (Unrestricted);
--  implementation unit
private generic
package Ada.Numerics.SFMT.Random.Inside is
   --  no SIMD version
   pragma Preelaborate;

   procedure gen_rand_all (
      sfmt : in out w128_t_Array_N);
   pragma Inline (gen_rand_all);

   procedure gen_rand_array (
      sfmt : in out w128_t_Array_N;
      Item : in out w128_t_Array_Fixed;
      size : Integer);
   pragma Inline (gen_rand_array);

end Ada.Numerics.SFMT.Random.Inside;
