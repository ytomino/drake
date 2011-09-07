pragma License (Unrestricted);
package Ada.Containers is
   pragma Pure;

   type Hash_Type is mod 2 ** 32; -- implementation-defined

   --  extended to calculate hash value
   function Rotate_Left (Left : Hash_Type; Right : Natural) return Hash_Type;
   pragma Import (Intrinsic, Rotate_Left);

--  type Count_Type is range 0 .. implementation-defined;
   subtype Count_Type is Natural; --  extended

   Capacity_Error : exception;

end Ada.Containers;
