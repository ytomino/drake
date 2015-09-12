with System.Termination;
package body System.Debug is
   pragma Suppress (All_Checks);

   function Default_Put (
      S : String;
      Source_Location : String;
      Enclosing_Entity : String)
      return Boolean
   is
      L_Length : constant Natural := Source_Location'Length;
      E_Length : constant Natural := Enclosing_Entity'Length;
      Buffer : String (
         1 ..
         S'Length + L_Length + E_Length + 5); -- ": (" and ") "
   begin
      Buffer (1 .. L_Length) := Source_Location;
      Buffer (L_Length + 1 .. L_Length + 3) := ": (";
      Buffer (L_Length + 4 .. L_Length + E_Length + 3) := Enclosing_Entity;
      Buffer (L_Length + E_Length + 4 .. L_Length + E_Length + 5) := ") ";
      Buffer (L_Length + E_Length + 6 .. Buffer'Last) := S;
      Termination.Error_Put_Line (Buffer);
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

   function Runtime_Error (
      S : String;
      Source_Location : String := Debug.Source_Location;
      Enclosing_Entity : String := Debug.Enclosing_Entity)
      return Boolean
   is
      Dummy : Boolean;
   begin
      Dummy := Put (S, Source_Location, Enclosing_Entity);
      Termination.Force_Abort;
      return False;
   end Runtime_Error;

end System.Debug;
