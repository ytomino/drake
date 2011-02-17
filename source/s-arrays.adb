package body System.Arrays is
   pragma Suppress (All_Checks);

   package body Generic_Slicing is

      function Constant_Slice (
         Item : not null access constant Array_Type;
         First : Index_Type;
         Last : Index_Type'Base)
         return Constant_Reference_Type is
      begin
         return Result : aliased Constant_Reference_Type := (
            Element => Item, --  dummy, be overwritten
            First => First,
            Last => Last)
         do
            declare
               type Repr is record
                  Data : Address;
                  Constraints : Address;
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
         return Reference_Type is
      begin
         return Result : aliased Reference_Type := (
            Element => Item, --  dummy, be overwritten
            First => First,
            Last => Last)
         do
            declare
               type Repr is record
                  Data : Address;
                  Constraints : Address;
               end record;
               pragma Suppress_Initialization (Repr);
               R : Repr;
               for R'Address use Result.Element'Address;
            begin
               R.Data := Item (First)'Address;
               R.Constraints := Result.First'Address;
            end;
         end return;
      end Slice;

   end Generic_Slicing;

end System.Arrays;
