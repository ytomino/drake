with Ada.Unchecked_Conversion;
with System;
package body Ada.References is
   pragma Suppress (All_Checks);

   package body Generic_Slicing is

      function Constant_Slice (
         Item : not null access constant Array_Type;
         First : Index_Type;
         Last : Index_Type'Base)
         return Constant_Reference_Type is
      begin
         return Result : Constant_Reference_Type := (
            Element => Item, -- dummy, be overwritten
            First => First,
            Last => Last)
         do
            declare
               type Repr is record
                  Data : System.Address;
                  Constraints : System.Address;
               end record;
               pragma Suppress_Initialization (Repr);
               R : Repr;
               for R'Address use Result.Element'Address;
            begin
               R.Data := Item (First)'Address;
               R.Constraints := Result.First'Address;
            end;
         end return;
      end Constant_Slice;

      function Slice (
         Item : not null access Array_Type;
         First : Index_Type;
         Last : Index_Type'Base)
         return Reference_Type
      is
         function Cast is new Unchecked_Conversion (
            Constant_Reference_Type,
            Reference_Type);
      begin
         return Cast (Constant_Slice (Item, First, Last));
      end Slice;

   end Generic_Slicing;

end Ada.References;
