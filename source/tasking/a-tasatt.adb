with Ada.Exceptions.Finally;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with System.Tasks;
package body Ada.Task_Attributes is
   use type System.Address;

   procedure Finally (Item : not null access System.Tasks.Attribute_Index);
   procedure Finally (Item : not null access System.Tasks.Attribute_Index) is
   begin
      System.Tasks.Free (Item.all); -- Item.all indicates Index
   end Finally;

   package Holder is
      new Exceptions.Finally.Scoped_Holder (
         System.Tasks.Attribute_Index,
         Finally);

   Index : aliased System.Tasks.Attribute_Index;

   function Cast is
      new Unchecked_Conversion (
         Task_Identification.Task_Id,
         System.Tasks.Task_Id);
   function Cast is
      new Unchecked_Conversion (System.Address, Attribute_Handle);
   function Cast is
      new Unchecked_Conversion (Attribute_Handle, System.Address);

   procedure Free is new Unchecked_Deallocation (Attribute, Attribute_Handle);

   procedure Finalize (Item : System.Address);
   procedure Finalize (Item : System.Address) is
      P : Attribute_Handle := Cast (Item);
   begin
      Free (P);
   end Finalize;

   --  implementation

   function Value (
      T : Task_Identification.Task_Id := Task_Identification.Current_Task)
      return Attribute is
   begin
      if Task_Identification.Is_Terminated (T) then
         raise Tasking_Error;
      end if;
      return Result : Attribute do
         declare
            procedure Process (Item : System.Address);
            procedure Process (Item : System.Address) is
            begin
               if Item = System.Null_Address then
                  Result := Initial_Value;
               else
                  Result := Cast (Item).all;
               end if;
            end Process;
         begin
            System.Tasks.Query (Cast (T), Index, Process'Access);
         end;
      end return;
   end Value;

   function Reference (
      T : Task_Identification.Task_Id := Task_Identification.Current_Task)
      return not null Attribute_Handle is
   begin
      if Task_Identification.Is_Terminated (T) then
         raise Tasking_Error;
      end if;
      declare
         function New_Item return System.Address;
         function New_Item return System.Address is
         begin
            return Cast (new Attribute'(Initial_Value));
         end New_Item;
         Result : System.Address;
      begin
         System.Tasks.Reference (
            Cast (T),
            Index,
            New_Item'Access,
            Finalize'Access,
            Result);
         return Cast (Result);
      end;
   end Reference;

   procedure Set_Value (
      Val : Attribute;
      T : Task_Identification.Task_Id := Task_Identification.Current_Task) is
   begin
      if Task_Identification.Is_Terminated (T) then
         raise Tasking_Error;
      end if;
      declare
         function New_Item return System.Address;
         function New_Item return System.Address is
         begin
            return Cast (new Attribute'(Val));
         end New_Item;
      begin
         System.Tasks.Set (
            Cast (T),
            Index,
            New_Item'Access,
            Finalize'Access);
      end;
   end Set_Value;

   procedure Reinitialize (
      T : Task_Identification.Task_Id := Task_Identification.Current_Task) is
   begin
      if Task_Identification.Is_Terminated (T) then
         raise Tasking_Error;
      end if;
      System.Tasks.Clear (Cast (T), Index);
   end Reinitialize;

begin
   System.Tasks.Allocate (Index);
   Holder.Assign (Index'Access);
end Ada.Task_Attributes;
