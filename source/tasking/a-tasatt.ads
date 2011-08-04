pragma License (Unrestricted);
with Ada.Task_Identification; -- use Ada.Task_Identification;
generic
   type Attribute is private;
   Initial_Value : Attribute;
package Ada.Task_Attributes is

   type Attribute_Handle is access all Attribute;

   function Value (
      T : Task_Identification.Task_Id := Task_Identification.Current_Task)
      return Attribute;

   function Reference (
      T : Task_Identification.Task_Id := Task_Identification.Current_Task)
      return not null Attribute_Handle;

   procedure Set_Value (
      Val : Attribute;
      T : Task_Identification.Task_Id := Task_Identification.Current_Task);
   procedure Reinitialize (
      T : Task_Identification.Task_Id := Task_Identification.Current_Task);

end Ada.Task_Attributes;
