pragma License (Unrestricted);
package Ada.Containers is
   pragma Pure;

   type Hash_Type is mod 2 ** 32; -- implementation-defined

   --  extended
   --  Shift_Left, Shift_Right, Shift_Right_Arithmetic, Rotate_Left,
   --    and Rotate_Right.
   --  These subprograms are convenience to make hash functions.
   pragma Provide_Shift_Operators (Hash_Type);

   --  modified
   --  Count_Type is essentially same as Natural.
--  type Count_Type is range 0 .. implementation-defined;
   subtype Count_Type is Natural;

   Capacity_Error : exception;

end Ada.Containers;
