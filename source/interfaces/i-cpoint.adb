with Ada.Unchecked_Conversion;
package body Interfaces.C.Pointers is
   pragma Suppress (All_Checks);

   generic function C renames Ada.Unchecked_Conversion;
   subtype A is Element_Array;
   Storage_Unit : constant := Standard'Storage_Unit;

   --  implementation

   procedure Decrement (Ref : in out Pointer) is
   begin
      Ref := Ref - 1;
   end Decrement;

   procedure Decrement (Ref : in out Constant_Pointer) is
   begin
      Ref := Ref - 1;
   end Decrement;

   procedure Increment (Ref : in out Pointer) is
   begin
      Ref := Ref + 1;
   end Increment;

   procedure Increment (Ref : in out Constant_Pointer) is
   begin
      Ref := Ref + 1;
   end Increment;

   function "+" (Left : Pointer; Right : ptrdiff_t) return Pointer is
      function I is new C (Pointer, ptrdiff_t);
      function P is new C (ptrdiff_t, Pointer);
   begin
      return P (I (Left) + Right * (A'Component_Size / Storage_Unit));
   end "+";

   function "+" (Left : Constant_Pointer; Right : ptrdiff_t)
      return Constant_Pointer
   is
      function I is new C (Constant_Pointer, ptrdiff_t);
      function P is new C (ptrdiff_t, Constant_Pointer);
   begin
      return P (I (Left) + Right * (A'Component_Size / Storage_Unit));
   end "+";

   function "+" (Left : ptrdiff_t; Right : Pointer) return Pointer is
   begin
      return Right + Left;
   end "+";

   function "+" (Left : ptrdiff_t; Right : Constant_Pointer)
      return Constant_Pointer is
   begin
      return Right + Left;
   end "+";

   function "-" (Left : Pointer; Right : ptrdiff_t) return Pointer is
      function I is new C (Pointer, ptrdiff_t);
      function P is new C (ptrdiff_t, Pointer);
   begin
      return P (I (Left) - Right * (A'Component_Size / Storage_Unit));
   end "-";

   function "-" (Left : Constant_Pointer; Right : ptrdiff_t)
      return Constant_Pointer
   is
      function I is new C (Constant_Pointer, ptrdiff_t);
      function P is new C (ptrdiff_t, Constant_Pointer);
   begin
      return P (I (Left) - Right * (A'Component_Size / Storage_Unit));
   end "-";

   function "-" (Left : Pointer; Right : not null access constant Element)
      return ptrdiff_t is
   begin
      return Constant_Pointer (Left) - Right;
   end "-";

   function "-" (Left : Constant_Pointer;
      Right : not null access constant Element)
      return ptrdiff_t
   is
      function I is new C (Constant_Pointer, ptrdiff_t);
   begin
      return (I (Left) - I (Constant_Pointer (Right))) /
         (A'Component_Size / Storage_Unit);
   end "-";

end Interfaces.C.Pointers;
