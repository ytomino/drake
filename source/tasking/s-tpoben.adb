package body System.Tasking.Protected_Objects.Entries is

   procedure Cancel_Call (Call : not null Node_Access);
   procedure Cancel_Call (Call : not null Node_Access) is
   begin
      begin
         raise Program_Error; -- C940016, not Tasking_Error
      exception
         when E : Program_Error =>
            Ada.Exceptions.Save_Occurrence (Call.X, E);
      end;
      Synchronous_Objects.Set (Call.Waiting);
   end Cancel_Call;

   procedure Cancel_Call (X : in out Synchronous_Objects.Queue_Node_Access);
   procedure Cancel_Call (X : in out Synchronous_Objects.Queue_Node_Access) is
      Call : constant not null Node_Access := Downcast (X);
   begin
      Cancel_Call (Call);
   end Cancel_Call;

   --  implementation

   procedure Initialize_Protection_Entries (
      Object : not null access Protection_Entries'Class;
      Ceiling_Priority : Integer;
      Compiler_Info : Address;
      Entry_Bodies : Protected_Entry_Body_Access;
      Find_Body_Index : Find_Body_Index_Access;
      Build_Entry_Names : Boolean)
   is
      pragma Unreferenced (Ceiling_Priority);
      pragma Unreferenced (Build_Entry_Names);
   begin
      Synchronous_Objects.Initialize (Object.Mutex);
      Synchronous_Objects.Initialize (Object.Calling, Object.Mutex'Access);
      Object.Compiler_Info := Compiler_Info;
      Object.Entry_Bodies := Entry_Bodies;
      Object.Find_Body_Index := Find_Body_Index;
      Object.Raised_On_Barrier := False;
   end Initialize_Protection_Entries;

   overriding procedure Finalize (Object : in out Protection_Entries) is
   begin
      Cancel_Calls (Object);
      Synchronous_Objects.Finalize (Object.Calling);
      Synchronous_Objects.Finalize (Object.Mutex);
   end Finalize;

   procedure Lock_Entries (
      Object : not null access Protection_Entries'Class) is
   begin
      Synchronous_Objects.Enter (Object.Mutex);
   end Lock_Entries;

   procedure Unlock_Entries (
      Object : not null access Protection_Entries'Class) is
   begin
      Synchronous_Objects.Leave (Object.Mutex);
   end Unlock_Entries;

   procedure Cancel_Calls (Object : in out Protection_Entries'Class) is
   begin
      Synchronous_Objects.Cancel (Object.Calling, Cancel_Call'Access);
   end Cancel_Calls;

end System.Tasking.Protected_Objects.Entries;
