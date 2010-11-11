with System.Termination;
package body System.Debug is
   pragma Suppress (All_Checks);

   function Default_Put (
      S : String;
      Source_Location : String;
      Enclosing_Entity : String)
      return Boolean is
   begin
      Termination.Error_Put (Source_Location);
      Termination.Error_Put (": (");
      Termination.Error_Put (Enclosing_Entity);
      Termination.Error_Put (") ");
      Termination.Error_Put (S);
      Termination.Error_New_Line;
      return True;
   end Default_Put;

   function Put_Impl (
      S : String;
      Source_Location : String := Debug.Source_Location;
      Enclosing_Entity : String := Debug.Enclosing_Entity)
      return Boolean is
   begin
      return Put_Hook (S, Source_Location, Enclosing_Entity);
   end Put_Impl;

   procedure Runtime_Error (
      Condition : Boolean;
      S : String;
      Source_Location : String := Debug.Source_Location;
      Enclosing_Entity : String := Debug.Enclosing_Entity)
   is
      Dummy : Boolean;
      pragma Unreferenced (Dummy);
   begin
      if Condition then
         Dummy := Put (S, Source_Location, Enclosing_Entity);
         Termination.Force_Abort;
      end if;
   end Runtime_Error;

end System.Debug;
