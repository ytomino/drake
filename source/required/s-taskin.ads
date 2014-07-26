pragma License (Unrestricted);
--  implementation unit required by compiler
with System.Storage_Elements;
package System.Tasking is
   pragma Preelaborate;

   --  required for local tagged types by compiler (s-taskin.ads)
   subtype Master_Level is Integer;
   subtype Master_ID is Master_Level;

   Foreign_Task_Level : constant Master_Level := 0;
   Environment_Task_Level : constant Master_Level := 1;
   Independent_Task_Level : constant Master_Level := 2;
   Library_Task_Level : constant Master_Level := 3;

   --  required for task by compiler (s-taskin.ads)
   type Task_Procedure_Access is access procedure (Params : Address);

   --  required for task by compiler (s-taskin.ads)
   subtype Task_Id is Address; -- Inside.Task_Id

   --  required for task by compiler (s-taskin.ads)
   --  "limited" is important to force pass-by-reference for Stages.Create_Task
   type Activation_Chain is limited record
      Data : Address := Null_Address;
   end record;
   for Activation_Chain'Size use Standard'Address_Size;

   --  required for built-in-place of task by compiler (s-taskin.ads)
   type Activation_Chain_Access is access all Activation_Chain;
   for Activation_Chain_Access'Storage_Size use 0;
   pragma No_Strict_Aliasing (Activation_Chain_Access);

   --  required for task by compiler (s-taskin.ads)
   Unspecified_Priority : constant Integer := Priority'First - 1;
   Unspecified_CPU : constant := -1;

   --  required for entry of task by compiler (s-taskin.ads)
   Interrupt_Entry : constant := -2;
   Cancelled_Entry : constant := -1;
   Null_Entry : constant := 0;
   Max_Entry : constant := Integer'Last;
   subtype Entry_Index is Integer range Interrupt_Entry .. Max_Entry;
   subtype Task_Entry_Index is Entry_Index range Null_Entry .. Max_Entry;

   --  required for select when or by compiler (s-taskin.ads)
   Null_Task_Entry : constant := Null_Entry;

   --  required for protected entry by compiler (s-taskin.ads)
   --  note, compiler may crash if Call_Modes is not declared as enum.
   type Call_Modes is (
      Simple_Call, -- 0
      Conditional_Call, -- 1
      Asynchronous_Call); -- 2
--    Timed_Call); -- 3
   pragma Discard_Names (Call_Modes);

   --  required for abort statement by compiler (s-taskin.ads)
   type Task_List is array (Positive range <>) of Task_Id;
   pragma Suppress_Initialization (Task_List);

   --  required for abort statement by compiler (s-taskin.ads)
   function Self return Task_Id;
   pragma Import (Ada, Self, "__drake_current_task");

   type Storage_Size_Handler is access function (T : Task_Id)
      return Storage_Elements.Storage_Count;
      --  System.Parameters.Size_Type ???
   pragma Suppress (Access_Check, Storage_Size_Handler);

   --  required for 'Storage_Size by compiler (s-taskin.ads)
   Storage_Size : Storage_Size_Handler := null;
   pragma Suppress (Access_Check, Storage_Size);

   --  equivalent to String_Access (s-taskin.ads)
   type Entry_Name_Access is access all String;

   --  dispatching domain (s-taskin.ads)
   type Dispatching_Domain is array (
--    System.Multiprocessors.CPU range <>)
      Positive) of Boolean;
   pragma Suppress_Initialization (Dispatching_Domain);
   type Dispatching_Domain_Access is access Dispatching_Domain;

   --  gdb knows below names, but those could not be provided
--  type Ada_Task_Control_Block;
--  type Common_ATCB;
--  type Entry_Call_Record;
--  Debug.Known_Tasks : array (0 .. 999) of Task_Id;
--  Debug.First_Task : ???
--  procedure Restricted.Stages.Task_Wrapper (Self_ID : Task_Id);
--  type System.Task_Primitives.Private_Data;

end System.Tasking;
