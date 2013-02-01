with Ada.Command_Line.Inside;
with System.Standard_Library;
package body Ada.Command_Line is
   pragma Suppress (All_Checks);

   --  implementation

   function Argument (Number : Positive) return String is
   begin
      if Number > Inside.Argument_Count then
         raise Constraint_Error;
      else
         return Inside.Argument (Number);
      end if;
   end Argument;

   function Argument_Count return Natural is
   begin
      return Inside.Argument_Count;
   end Argument_Count;

   function Command_Name return String is
   begin
      return Inside.Argument (0);
   end Command_Name;

   procedure Set_Exit_Status (Code : Exit_Status) is
   begin
      System.Standard_Library.gnat_exit_status := Integer (Code);
   end Set_Exit_Status;

end Ada.Command_Line;
