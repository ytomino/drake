pragma License (Unrestricted);
generic
   type Index is (<>);
   type Element is private;
   type Element_Array is array (Index range <>) of aliased Element;
   Default_Terminator : Element;
package Interfaces.C.Pointers is
--  pragma Preelaborate;
   pragma Pure;

   type Pointer is access all Element;
   --  modified
   for Pointer'Storage_Size use 0;
   pragma No_Strict_Aliasing (Pointer);

   --  extended
   type Constant_Pointer is access constant Element;
   for Constant_Pointer'Storage_Size use 0;
   pragma No_Strict_Aliasing (Constant_Pointer);

--  function Value (Ref : Pointer; Terminator : Element := Default_Terminator)
--    return Element_Array;
   function Value (
      Ref : access constant Element; -- CXB3014 requires null
      Terminator : Element := Default_Terminator)
      return Element_Array;

--  function Value (Ref : Pointer; Length : ptrdiff_t)
--    return Element_Array;
   function Value (
      Ref : access constant Element; -- CXB3014 requires null
      Length : ptrdiff_t)
      return Element_Array;

   Pointer_Error : exception
      renames C.Pointer_Error;

   --  C-style Pointer arithmetic

   function "+" (
      Left : Pointer; -- CXB3015 requires null
      Right : ptrdiff_t)
      return not null Pointer
      with Convention => Intrinsic;
   --  modified from here
   function "+" (
      Left : ptrdiff_t;
      Right : not null Pointer) -- null exclusion
      return not null Pointer
      with Convention => Intrinsic;
   --  to here
   function "-" (
      Left : Pointer; -- CXB3015 requires null
      Right : ptrdiff_t)
      return not null Pointer
      with Convention => Intrinsic;
   --  modified from here
--  function "-" (Left : Pointer; Right : Pointer) return ptrdiff_t;
   function "-" (
      Left : not null Pointer; -- null exclusion
      Right : not null access constant Element)
      return ptrdiff_t
      with Convention => Intrinsic;
   --  to here

   pragma Pure_Function ("+");
   pragma Pure_Function ("-");
   pragma Inline_Always ("+");
   pragma Inline_Always ("-");

   --  modified from here
   procedure Increment (
      Ref : in out not null Pointer) -- null exclusion
      with Convention => Intrinsic;
   --  to here
   procedure Decrement (
      Ref : in out Pointer) -- CXB3015 requires null
      with Convention => Intrinsic;

   pragma Inline_Always (Increment);
   pragma Inline_Always (Decrement);

   --  extended from here

   function "+" (Left : not null Constant_Pointer; Right : ptrdiff_t)
      return not null Constant_Pointer
      with Convention => Intrinsic;
   function "+" (Left : ptrdiff_t; Right : not null Constant_Pointer)
      return not null Constant_Pointer
      with Convention => Intrinsic;
   function "-" (Left : not null Constant_Pointer; Right : ptrdiff_t)
      return not null Constant_Pointer
      with Convention => Intrinsic;
   function "-" (
      Left : not null Constant_Pointer;
      Right : not null access constant Element)
      return ptrdiff_t
      with Convention => Intrinsic;

   pragma Pure_Function ("+");
   pragma Pure_Function ("-");
   pragma Inline_Always ("+");
   pragma Inline_Always ("-");

   procedure Increment (Ref : in out not null Constant_Pointer)
      with Convention => Intrinsic;
   procedure Decrement (Ref : in out not null Constant_Pointer)
      with Convention => Intrinsic;

   pragma Inline_Always (Increment);
   pragma Inline_Always (Decrement);

   --  to here

--  function Virtual_Length (
--    Ref : Pointer;
--    Terminator : Element := Default_Terminator)
--    return ptrdiff_t;
   function Virtual_Length (
      Ref : access constant Element; -- CXB3016 requires null
      Terminator : Element := Default_Terminator)
      return ptrdiff_t;

   --  extended
   --  This overloaded version Virtual_Length gets the length of Ref
   --    less than or equal to Limit.
   function Virtual_Length (
      Ref : not null access constant Element;
      Limit : ptrdiff_t;
      Terminator : Element := Default_Terminator)
      return ptrdiff_t;

--  procedure Copy_Terminated_Array (
--    Source : Pointer;
--    Target : Pointer;
--    Limit : ptrdiff_t := ptrdiff_t'Last;
--    Terminator : Element := Default_Terminator);
   procedure Copy_Terminated_Array (
      Source : access constant Element; -- CXB3016 requires null
      Target : access Element; -- same as above
      Limit : ptrdiff_t := ptrdiff_t'Last;
      Terminator : Element := Default_Terminator);

   --  Note: Copy_Terminated_Array (..., Limit) produces an unterminated Target
   --    if there is no Terminator in the first Limit elements of Source.
   --  This behavior is danger similar to strncpy.
   --  Use Virtual_Length and Copy_Array, or imported strlcpy, instead of it.

--  procedure Copy_Array (
--    Source : Pointer;
--    Target : Pointer;
--    Length : ptrdiff_t);
   procedure Copy_Array (
      Source : access constant Element; -- CXB3016 requires null
      Target : access Element; -- same as above
      Length : ptrdiff_t);

end Interfaces.C.Pointers;
