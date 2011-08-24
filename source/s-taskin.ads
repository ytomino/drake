pragma License (Unrestricted);
--  implementation package required by compiler
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
   type Activation_Chain is limited record
      Data : Address := Null_Address;
   end record;

   --  required for build-in-place of task by compiler (s-taskin.ads)
   type Activation_Chain_Access is access all Activation_Chain;

   --  required for task by compiler (s-taskin.ads)
   Unspecified_Priority : constant Integer := System.Priority'First - 1;
   Unspecified_CPU : constant := -1;

   --  required for entry of task by compiler (s-taskin.ads)
   Interrupt_Entry : constant := -2;
   Cancelled_Entry : constant := -1;
   Null_Entry : constant := 0;
   Max_Entry : constant := Integer'Last;
   type Entry_Index is range Interrupt_Entry .. Max_Entry;
   type Task_Entry_Index is new Entry_Index range Null_Entry .. Max_Entry;

   --  equivalent to String_Access (s-taskin.ads)
   type Entry_Name_Access is access all String;

end System.Tasking;
