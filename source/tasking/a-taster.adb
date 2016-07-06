with Ada.Unchecked_Conversion;
with System.Tasks;
package body Ada.Task_Termination is
   use type Task_Identification.Task_Id;

   function To_Private is
      new Unchecked_Conversion (
         Task_Identification.Task_Id,
         System.Tasks.Task_Id);

   function To_Private is
      new Unchecked_Conversion (
         Termination_Handler,
         System.Tasks.Termination_Handler);
   function To_Public is
      new Unchecked_Conversion (
         System.Tasks.Termination_Handler,
         Termination_Handler);

   --  implementation

   procedure Set_Dependents_Fallback_Handler (Handler : Termination_Handler) is
   begin
      System.Tasks.Set_Dependents_Fallback_Handler (
         System.Tasks.Current_Task_Id,
         To_Private (Handler));
   end Set_Dependents_Fallback_Handler;

   function Current_Task_Fallback_Handler return Termination_Handler is
   begin
      return To_Public (
         System.Tasks.Dependents_Fallback_Handler (
            System.Tasks.Current_Task_Id));
   end Current_Task_Fallback_Handler;

   procedure Set_Specific_Handler (
      T : Task_Identification.Task_Id;
      Handler : Termination_Handler)
   is
      pragma Check (Pre,
         Check =>
            T /= Task_Identification.Null_Task_Id
            or else raise Program_Error); -- RM C.7.3(15/2)
   begin
      System.Tasks.Set_Specific_Handler (To_Private (T), To_Private (Handler));
   end Set_Specific_Handler;

   function Specific_Handler (T : Task_Identification.Task_Id)
      return Termination_Handler
   is
      pragma Check (Pre,
         Check =>
            T /= Task_Identification.Null_Task_Id
            or else raise Program_Error); -- RM C.7.3(15/2)
   begin
      return To_Public (System.Tasks.Specific_Handler (To_Private (T)));
   end Specific_Handler;

end Ada.Task_Termination;
