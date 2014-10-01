pragma License (Unrestricted);
--  implementation unit required by compiler
with Ada.Interrupts;
with System.Interrupt_Handlers;
with System.Tasking.Protected_Objects;
with System.Tasking.Protected_Objects.Entries;
package System.Interrupts is

   type Previous_Handler_Item is record
      Interrupt : Ada.Interrupts.Interrupt_Id;
      Handler : Ada.Interrupts.Parameterless_Handler;
--    Static : Boolean;
   end record;
   pragma Suppress_Initialization (Previous_Handler_Item);

   type Previous_Handler_Array is
      array (Positive range <>) of Previous_Handler_Item;
   pragma Suppress_Initialization (Previous_Handler_Array);

   type New_Handler_Item is record
      Interrupt : Ada.Interrupts.Interrupt_Id;
      Handler : Ada.Interrupts.Parameterless_Handler;
   end record;
   pragma Suppress_Initialization (New_Handler_Item);

   type New_Handler_Array is array (Positive range <>) of New_Handler_Item;
   pragma Suppress_Initialization (New_Handler_Array);

   --  required by compiler

   subtype System_Interrupt_Id is Ada.Interrupts.Interrupt_Id;

   Default_Interrupt_Priority : constant Interrupt_Priority :=
      Interrupt_Priority'Last;

   --  required to attach a protected handler by compiler

   type Static_Interrupt_Protection (
      Num_Entries : Tasking.Protected_Objects.Protected_Entry_Index;
      Num_Attach_Handler : Natural) is
      limited new Tasking.Protected_Objects.Entries.Protection_Entries with
      private;

   procedure Register_Interrupt_Handler (Handler_Addr : Address)
      renames Interrupt_Handlers.Register_Interrupt_Handler;

   procedure Install_Handlers (
      Object : not null access Static_Interrupt_Protection;
      New_Handlers : New_Handler_Array);

   type Dynamic_Interrupt_Protection is
      limited new Tasking.Protected_Objects.Entries.Protection_Entries with
      null record;

   --  unimplemented subprograms required by compiler
   --  Bind_Interrupt_To_Entry
   --  Install_Restricted_Handlers

private

   type Static_Interrupt_Protection (
      Num_Entries : Tasking.Protected_Objects.Protected_Entry_Index;
      Num_Attach_Handler : Natural) is
      limited new Tasking.Protected_Objects.Entries.Protection_Entries (
         Num_Entries) with
    record
       Previous_Handlers : Previous_Handler_Array (1 .. Num_Attach_Handler);
    end record;

   overriding procedure Finalize (
      Object : in out Static_Interrupt_Protection);

end System.Interrupts;
