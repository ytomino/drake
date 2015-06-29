pragma License (Unrestricted);
package Ada.Containers is
   pragma Pure;

   type Hash_Type is mod 2 ** 32; -- implementation-defined

   --  extended
   --  A convenience to make hash functions.
   function Shift_Left (Value : Hash_Type; Amount : Natural) return Hash_Type
      with Import, Convention => Intrinsic;
   function Shift_Right (Value : Hash_Type; Amount : Natural) return Hash_Type
      with Import, Convention => Intrinsic;
   function Rotate_Left (Value : Hash_Type; Amount : Natural) return Hash_Type
      with Import, Convention => Intrinsic;
   function Rotate_Right (Value : Hash_Type; Amount : Natural)
      return Hash_Type
      with Import, Convention => Intrinsic;

   --  modified
   --  Count_Type is essentially same as Natural.
--  type Count_Type is range 0 .. implementation-defined;
   subtype Count_Type is Natural;

   Capacity_Error : exception;

end Ada.Containers;
