pragma License (Unrestricted);
--  implementation unit required by compiler
with Ada.Finalization;
package System.Tasking.Protected_Objects.Entries is

   type Find_Body_Index_Access is access function (
      O : Address;
      E : Protected_Entry_Index)
      return Protected_Entry_Index;

   --  required by compiler
   type Protected_Entry_Body_Array is array (Positive range <>) of Entry_Body;
   pragma Suppress_Initialization (Protected_Entry_Body_Array);

   type Protected_Entry_Body_Access is access all Protected_Entry_Body_Array;

   --  required by compiler
   --  (if it is not controlled type, compiler may be crashed!)
   type Protection_Entries (Num_Entries : Protected_Entry_Index) is
      new Ada.Finalization.Limited_Controlled with null record;

   --  required by compiler
   type Protection_Entries_Access is access all Protection_Entries'Class;
   for Protection_Entries_Access'Storage_Size use 0;

   --  required by compiler
   procedure Initialize_Protection_Entries (
      Object : Protection_Entries_Access;
      Ceiling_Priority : Integer;
      Compiler_Info : Address;
      Entry_Bodies : Protected_Entry_Body_Access;
      Find_Body_Index : Find_Body_Index_Access;
      Build_Entry_Names : Boolean) is
      null;

   --  required by compiler
   procedure Set_Entry_Name (
      Object : Protection_Entries'Class;
      Pos : Protected_Entry_Index;
      Val : Entry_Name_Access) is
      null;

   --  required by compiler
   procedure Lock_Entries (Object : Protection_Entries_Access) is null;
   procedure Unlock_Entries (Object : Protection_Entries_Access) is null;

   --  unimplemented subprograms required by compiler
   --  Get_Ceiling
   --  Set_Ceiling

end System.Tasking.Protected_Objects.Entries;
