pragma License (Unrestricted);
--  extended unit
package Ada.References is
   --  There are helpers for returning sliced pointer of array from a function.
   pragma Pure;

   --  magic to carry out ARRAY (F .. L)'Access to out of subprogram
   generic
      type Index_Type is range <>;
      type Element_Type is limited private;
      type Array_Type is array (Index_Type range <>) of Element_Type;
   package Generic_Slicing is

      type Constant_Reference_Type (
         Element : not null access constant Array_Type) is
         limited private;
--       with Implicit_Dereference => Element; -- [gcc 4.6]

      function Constant_Slice (
         Item : not null access constant Array_Type;
         First : Index_Type;
         Last : Index_Type'Base)
         return Constant_Reference_Type;

      type Reference_Type (Element : not null access Array_Type) is
         limited private;
--       with Implicit_Dereference => Element; -- [gcc 4.6]

      function Slice (
         Item : not null access Array_Type;
         First : Index_Type;
         Last : Index_Type'Base)
         return Reference_Type;

   private

      type Constant_Reference_Type (
         Element : not null access constant Array_Type) is limited
      record
         First : Index_Type;
         Last : Index_Type'Base;
      end record;
      pragma Suppress_Initialization (Constant_Reference_Type);

      type Reference_Type (Element : not null access Array_Type) is
         limited
      record
         First : Index_Type;
         Last : Index_Type'Base;
      end record;
      pragma Suppress_Initialization (Reference_Type);

   end Generic_Slicing;

end Ada.References;
