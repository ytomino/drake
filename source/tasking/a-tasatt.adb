with Ada.Finalization;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with System.Tasks;
package body Ada.Task_Attributes is
   use type System.Address;

   Index : System.Tasks.Attribute_Index;

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

   type Finalizer_Type is new Finalization.Limited_Controlled with null record;
   pragma Unreferenced_Objects (Finalizer_Type);
   overriding procedure Finalize (Object : in out Finalizer_Type);
   overriding procedure Finalize (Object : in out Finalizer_Type) is
      pragma Unreferenced (Object);
   begin
      System.Tasks.Free (Index);
   end Finalize;

   Finalizer : Finalizer_Type;

   procedure Check (T : Task_Identification.Task_Id);
   procedure Check (T : Task_Identification.Task_Id) is
   begin
      if Task_Identification.Is_Terminated (T) then
         raise Tasking_Error;
      end if;
   end Check;

   --  implementation

   function Value (
      T : Task_Identification.Task_Id := Task_Identification.Current_Task)
      return Attribute is
   begin
      Check (T);
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
      return not null Attribute_Handle
   is
      function New_Item return System.Address;
      function New_Item return System.Address is
      begin
         return Cast (new Attribute'(Initial_Value));
      end New_Item;
      Result : System.Address;
   begin
      Check (T);
      System.Tasks.Reference (
         Cast (T),
         Index,
         New_Item'Access,
         Finalize'Access,
         Result);
      return Cast (Result);
   end Reference;

   procedure Set_Value (
      Val : Attribute;
      T : Task_Identification.Task_Id := Task_Identification.Current_Task)
   is
      function New_Item return System.Address;
      function New_Item return System.Address is
      begin
         return Cast (new Attribute'(Val));
      end New_Item;
   begin
      Check (T);
      System.Tasks.Set (
         Cast (T),
         Index,
         New_Item'Access,
         Finalize'Access);
   end Set_Value;

   procedure Reinitialize (
      T : Task_Identification.Task_Id := Task_Identification.Current_Task) is
   begin
      Check (T);
      System.Tasks.Clear (Cast (T), Index);
   end Reinitialize;

begin
   System.Tasks.Allocate (Index);
end Ada.Task_Attributes;
