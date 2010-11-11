pragma License (Unrestricted);
--  extended package
package System.Arrays is
   pragma Pure;

   --  magic to carry out ARRAY (F .. L)'Access to out of subprogram
   generic
      type Index_Type is range <>;
      type Element_Type is limited private;
      type Array_Type is array (Index_Type range <>) of Element_Type;
   package Generic_Slicing is

      type Reference_Type (Element : not null access Array_Type) is
         limited private;

      function Slice (
         Item : not null access Array_Type;
         First : Index_Type;
         Last : Index_Type'Base)
         return Reference_Type;

   private

      type Reference_Type (Element : not null access Array_Type) is
         limited
      record
         First : Index_Type;
         Last : Index_Type'Base;
      end record;
      pragma Suppress_Initialization (Reference_Type);

   end Generic_Slicing;

end System.Arrays;
