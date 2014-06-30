--  with System.Address_To_Constant_Access_Conversions; -- Preelaborate unit
--  with System.Address_To_Named_Access_Conversions;
with System.Storage_Elements;
package body Interfaces.C.Pointers is
   pragma Suppress (All_Checks);
   use type System.Storage_Elements.Storage_Offset;

--  package Conv is
--    new System.Address_To_Named_Access_Conversions (Element, Pointer);
--  package const_Conv is
--    new System.Address_To_Constant_Access_Conversions (
--       Element,
--       Constant_Pointer);

   function To_Pointer (Value : System.Address) return access Element;
   pragma Import (Intrinsic, To_Pointer);
   function To_Address (Value : access constant Element) return System.Address;
   pragma Import (Intrinsic, To_Address);

   --  implementation

   procedure Decrement (Ref : in out Pointer) is
   begin
      Ref := Ref - 1;
   end Decrement;

   procedure Decrement (Ref : in out not null Constant_Pointer) is
   begin
      Ref := Ref - 1;
   end Decrement;

   procedure Increment (Ref : in out not null Pointer) is
   begin
      Ref := Ref + 1;
   end Increment;

   procedure Increment (Ref : in out not null Constant_Pointer) is
   begin
      Ref := Ref + 1;
   end Increment;

   function "+" (
      Left : Pointer;
      Right : ptrdiff_t)
      return not null Pointer is
   begin
      if not Standard'Fast_Math and then Left = null then
         raise Pointer_Error; -- CXB3015
      end if;
      return To_Pointer (
         To_Address (Left)
         + System.Storage_Elements.Storage_Offset (Right)
            * (Element_Array'Component_Size / Standard'Storage_Unit));
   end "+";

   function "+" (
      Left : not null Constant_Pointer;
      Right : ptrdiff_t)
      return not null Constant_Pointer is
   begin
      return To_Pointer (
         To_Address (Left)
         + System.Storage_Elements.Storage_Offset (Right)
            * (Element_Array'Component_Size / Standard'Storage_Unit));
   end "+";

   function "+" (
      Left : ptrdiff_t;
      Right : not null Pointer)
      return not null Pointer is
   begin
      return Right + Left;
   end "+";

   function "+" (
      Left : ptrdiff_t;
      Right : not null Constant_Pointer)
      return not null Constant_Pointer is
   begin
      return Right + Left;
   end "+";

   function "-" (
      Left : Pointer;
      Right : ptrdiff_t)
      return not null Pointer is
   begin
      if not Standard'Fast_Math and then Left = null then
         raise Pointer_Error; -- CXB3015
      end if;
      return To_Pointer (
         To_Address (Left)
         - System.Storage_Elements.Storage_Offset (Right)
            * (Element_Array'Component_Size / Standard'Storage_Unit));
   end "-";

   function "-" (
      Left : not null Constant_Pointer;
      Right : ptrdiff_t)
      return not null Constant_Pointer　is
   begin
      return To_Pointer (
         To_Address (Left)
         - System.Storage_Elements.Storage_Offset (Right)
            * (Element_Array'Component_Size / Standard'Storage_Unit));
   end "-";

   function "-" (
      Left : not null Pointer;
      Right : not null access constant Element)
      return ptrdiff_t is
   begin
      return Constant_Pointer (Left) - Right;
   end "-";

   function "-" (
      Left : not null Constant_Pointer;
      Right : not null access constant Element)
      return ptrdiff_t is
   begin
      return ptrdiff_t (
         (To_Address (Left) - To_Address (Right))
         / (Element_Array'Component_Size / Standard'Storage_Unit));
   end "-";

end Interfaces.C.Pointers;
