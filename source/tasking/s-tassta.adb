with System.Soft_Links;
with System.Address_To_Named_Access_Conversions;
with System.Tasking.Inside;
with System.Termination;
package body System.Tasking.Stages is
   pragma Suppress (All_Checks);

   package Task_Record_Conv is new Address_To_Named_Access_Conversions (
      Inside.Task_Record,
      Inside.Task_Id);

   procedure Unregister;
   procedure Unregister is
   begin
      Soft_Links.Current_Master := Soft_Links.Zero'Access;
      Soft_Links.Enter_Master := Soft_Links.Nop'Access;
      Soft_Links.Complete_Master := Soft_Links.Nop'Access;
   end Unregister;

   function Current_Master return Master_Level
      renames Inside.Master_Within;

   procedure Enter_Master
      renames Inside.Enter_Master;

   procedure Complete_Master
      renames Inside.Leave_Master;

   function Storage_Size (T : Task_Id) return Storage_Elements.Storage_Count;
   function Storage_Size (T : Task_Id) return Storage_Elements.Storage_Count is
      Addr : Address;
      Size : Storage_Elements.Storage_Count;
   begin
      Inside.Get_Stack (Task_Record_Conv.To_Pointer (T), Addr, Size);
      return Size;
   end Storage_Size;

   --  implementation

   procedure Create_Task (
      Priority : Integer;
      Size : Parameters.Size_Type;
      Task_Info : System.Task_Info.Task_Info_Type;
      Relative_Deadline : Ada.Real_Time.Time_Span;
      Num_Entries : Task_Entry_Index;
      Master : Master_Level;
      State : Task_Procedure_Access;
      Discriminants : Address;
      Elaborated : not null access Boolean;
      Chain : in out Activation_Chain;
      Task_Image : String;
      Created_Task : out Task_Id;
      Build_Entry_Names : Boolean)
   is
      pragma Unreferenced (Priority);
      pragma Unreferenced (Size);
      pragma Unreferenced (Task_Info);
      pragma Unreferenced (Relative_Deadline);
      pragma Unreferenced (Build_Entry_Names);
      Master_Of_Parent : constant Inside.Master_Access :=
         Inside.Master_Of_Parent (Master);
      New_Task_Id : Inside.Task_Id;
   begin
      Inside.Create (
         New_Task_Id,
         Discriminants,
         State,
         Name => Task_Image,
         Chain => Chain'Unrestricted_Access,
         Elaborated => Elaborated,
         Master => Master_Of_Parent,
         Entry_Last_Index => Num_Entries);
      Created_Task := Task_Record_Conv.To_Address (New_Task_Id);
   end Create_Task;

   procedure Complete_Activation is
      Aborted : Boolean; -- abort or elaboration error
   begin
      Inside.Accept_Activation (Aborted);
      if Aborted then
         raise Standard'Abort_Signal;
      end if;
   end Complete_Activation;

   procedure Complete_Task is
   begin
      Inside.Cancel_Calls;
      --  compiler omits final one calling Complete_Master, so do it here
      --  do it before returning task body, since descriptors and discriminants
      --  of child tasks are placed on the stack of task body
      --  do it before set 'Terminated to False required by C94001A
      Inside.Leave_All_Masters;
   end Complete_Task;

   procedure Activate_Tasks (
      Chain_Access : not null access Activation_Chain)
   is
      Aborted : Boolean; -- ignore abort
      pragma Unreferenced (Aborted);
   begin
      Inside.Activate (Chain_Access, Aborted => Aborted);
   end Activate_Tasks;

   procedure Free_Task (T : Task_Id) is
      Id : Inside.Task_Id := Task_Record_Conv.To_Pointer (T);
   begin
      Inside.Set_Entry_Names_To_Deallocate (Id);
      case Inside.Preferred_Free_Mode (Id) is
         when Inside.Wait =>
            declare
               Aborted : Boolean; -- ignored
            begin
               Inside.Wait (Id, Aborted => Aborted);
            end;
         when Inside.Detach =>
            Inside.Detach (Id);
      end case;
   end Free_Task;

   procedure Move_Activation_Chain (
      From, To : Activation_Chain_Access;
      New_Master : Master_ID)
   is
      New_Master_Of_Parent : constant Inside.Master_Access :=
         Inside.Master_Of_Parent (New_Master);
   begin
      Inside.Move (From, To, New_Master_Of_Parent);
   end Move_Activation_Chain;

   procedure Set_Entry_Name (
      T : Task_Id;
      Pos : Task_Entry_Index;
      Val : Entry_Name_Access) is
   begin
      Inside.Set_Entry_Name (Task_Record_Conv.To_Pointer (T), Pos, Val);
   end Set_Entry_Name;

   procedure Abort_Tasks (Tasks : Task_List) is
   begin
      for I in Tasks'Range loop
         Inside.Send_Abort (Task_Record_Conv.To_Pointer (Tasks (I)));
      end loop;
   end Abort_Tasks;

   function Terminated (T : Task_Id) return Boolean is
   begin
      return Inside.Terminated (Task_Record_Conv.To_Pointer (T));
   end Terminated;

begin
   Soft_Links.Current_Master := Current_Master'Access;
   Soft_Links.Enter_Master := Enter_Master'Access;
   Soft_Links.Complete_Master := Complete_Master'Access;
   Tasking.Storage_Size := Storage_Size'Access;
   Termination.Register_Exit (Unregister'Access);
end System.Tasking.Stages;
