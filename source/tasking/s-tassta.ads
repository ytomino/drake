pragma License (Unrestricted);
--  implementation package required by compiler
with Ada.Real_Time;
with System.Parameters;
with System.Task_Info;
package System.Tasking.Stages is

   --  required for task by compiler (s-tassta.ads)
   procedure Create_Task (
      Priority : Integer; -- range -1 .. Any_Priority'Last;
      Size : Parameters.Size_Type;
      Task_Info : System.Task_Info.Task_Info_Type;
      Relative_Deadline : Ada.Real_Time.Time_Span;
      Num_Entries : Task_Entry_Index;
      Master : Master_Level;
      State : Task_Procedure_Access;
      Discriminants : Address; -- discriminants and Task_Id
      Elaborated : not null access Boolean;
      Chain : in out Activation_Chain;
      Task_Image : String;
      Created_Task : out Task_Id;
      Build_Entry_Names : Boolean);

   --  required (optional?) for task by compiler (s-tassta.ads)
   procedure Complete_Activation;
   procedure Complete_Task;

   --  required for task by compiler (s-tassta.ads)
   procedure Activate_Tasks (Chain_Access : not null access Activation_Chain);

   --  required for dynamic allocation of task by compiler (s-tassta.ads)
   procedure Expunge_Unactivated_Tasks (Chain : in out Activation_Chain) is
      null;

   --  required for dynamic deallocation of task by compiler (s-tassta.ads)
   procedure Free_Task (T : Task_Id);

   --  required for build-in-place of task by compiler (s-tassta.ads)
   procedure Move_Activation_Chain (
      From, To : Activation_Chain_Access;
      New_Master : Master_ID);

   --  required for entry of task by compiler (s-tassta.ads)
   procedure Set_Entry_Name (
      T : Task_Id;
      Pos : Task_Entry_Index;
      Val : Entry_Name_Access);

   --  required for abort statement by compiler (s-tassta.ads)
   procedure Abort_Tasks (Tasks : Task_List);

   --  required for 'Terminated by compiler (s-tassta.ads)
   function Terminated (T : Task_Id) return Boolean;

   --  task type be expanded below:
   --
   --  _chain : aliased Activation_Chain;
   --  xE : aliased Boolean := False;
   --  xZ : Size_Type := Unspecified_Size;
   --  type xV (... discriminants ...) is limited record
   --     _task_id : Task_Id;
   --  end record;
   --  procedure xvip (
   --     _init : in out xV;
   --     _master : Master_Id;
   --     _chain : in out Activation_Chain;
   --     _task_name : String;
   --     ... discriminant parameters ...);
   --  procedure xt (_task : access xV);
   --
   --  xE := True;
   --  x1 : xV (... discriminants ...);
   --  _master : constant Master_Id := Current_Master.all;
   --  xS : String := ...task name...;
   --  Activate_Tasks (_chain'Access);
   --
   --  by -fdump-tree-all, task type be expanded below:
   --
   --  /* initialization */
   --  xvip (
   --    struct xV &_init,
   --    system__tasking__master_id _master,
   --    struct system__tasking__activation_chain & _chain,
   --    struct _task_name,
   --    ... discriminant parameters ...)
   --  {
   --    ... store discriminant parameters into _init ...
   --    _init->_task_id = 0B;
   --    _init->_task_id = system.tasking.stages.create_task (
   --      -1, /* priority */
   --      xZ, /* size */
   --      2, /* pragma Task_Info */
   --      -1, /* CPU */
   --      0, /* deadline */
   --      0, /* entry count */
   --      _master,
   --      xt, /* task body */
   --      _init,
   --      &xE, /* in out, elaboration flag */
   --      _chain, /* Activation_Chain */
   --      _task_name,
   --      &_init->_task_id, /* out */
   --      0); /* entry names */
   --  }
   --
   --  /* body */
   --  xt (struct xV * const _task)
   --  {
   --    if(_task == 0) { .gnat_rcheck_00 (...location...) };
   --    try
   --      {
   --        system.soft_links.abort_undefer ();
   --        system.tasking.stages.complete_activation ();
   --        ... user code ...
   --      }
   --    finally
   --      {
   --        system.soft_links.abort_defer ();
   --        system.tasking.stages.complete_task ();
   --        system.soft_links.abort_undefer ();
   --      }
   --  }

end System.Tasking.Stages;
