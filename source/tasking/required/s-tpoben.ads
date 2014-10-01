pragma License (Unrestricted);
--  implementation unit required by compiler
with Ada.Exceptions;
with Ada.Finalization;
with Ada.Unchecked_Conversion;
package System.Tasking.Protected_Objects.Entries is

   type Node is limited record
      Super : aliased Synchronous_Objects.Queue_Node;
      E : Protected_Entry_Index;
      Uninterpreted_Data : Address;
      Requeued : Boolean;
      Waiting : aliased Synchronous_Objects.Event;
      X : Ada.Exceptions.Exception_Occurrence;
   end record;
   pragma Suppress_Initialization (Node);
   type Node_Access is access all Node;

   function Downcast is
      new Ada.Unchecked_Conversion (
         Synchronous_Objects.Queue_Node_Access,
         Node_Access);

   type Find_Body_Index_Access is access function (
      O : Address;
      E : Protected_Entry_Index)
      return Protected_Entry_Index;

   --  required by compiler
   type Protected_Entry_Body_Array is
      array (Positive_Protected_Entry_Index range <>) of Entry_Body;
   pragma Suppress_Initialization (Protected_Entry_Body_Array);

   type Protected_Entry_Body_Access is access all Protected_Entry_Body_Array;

   --  required by compiler
   type Protected_Entry_Names_Array is
      array (Entry_Index range <>) of Entry_Name_Access;
   pragma Suppress_Initialization (Protected_Entry_Names_Array);

   --  required by compiler
   --  (if it is not controlled type, compiler may be crashed!)
   type Protection_Entries (Num_Entries : Protected_Entry_Index) is
      limited new Ada.Finalization.Limited_Controlled with
   record
      Mutex : aliased Synchronous_Objects.Mutex;
      Calling : aliased Synchronous_Objects.Queue;
      Compiler_Info : Address;
      Entry_Bodies : Protected_Entry_Body_Access;
      Find_Body_Index : Find_Body_Index_Access;
      Raised_On_Barrier : Boolean;
      Current_Calling : access Node;
   end record;

   --  required for synchronized interface by compiler
   type Protection_Entries_Access is access all Protection_Entries'Class;
   for Protection_Entries_Access'Storage_Size use 0;

   --  required by compiler
   procedure Initialize_Protection_Entries (
      Object : not null access Protection_Entries'Class;
      Ceiling_Priority : Integer;
      Compiler_Info : Address;
      Entry_Bodies : Protected_Entry_Body_Access;
      Find_Body_Index : Find_Body_Index_Access);

   overriding procedure Finalize (Object : in out Protection_Entries);

   --  required by compiler
   procedure Set_Entry_Names (
      Object : not null access Protection_Entries'Class;
      Names : access Protected_Entry_Names_Array) is
      null;

   --  required by compiler
   procedure Lock_Entries (
      Object : not null access Protection_Entries'Class);
   procedure Unlock_Entries (
      Object : not null access Protection_Entries'Class);

   --  cancel all callings of entries
   procedure Cancel_Calls (Object : in out Protection_Entries'Class);

   --  required by compiler (s-tpoben.ads)
   function Get_Ceiling (Object : not null access Protection_Entries'Class)
      return Any_Priority;

   --  unimplemented subprograms required by compiler
   --  Set_Ceiling

end System.Tasking.Protected_Objects.Entries;
