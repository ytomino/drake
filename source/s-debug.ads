pragma License (Unrestricted);
--  runtime unit
package System.Debug is
   pragma Preelaborate;

   --  implementation of Ada.Debug

   function File return String;
   pragma Import (Intrinsic, File);

   function Line return Positive;
   pragma Import (Intrinsic, Line);

   function Source_Location return String;
   pragma Import (Intrinsic, Source_Location);

   function Enclosing_Entity return String;
   pragma Import (Intrinsic, Enclosing_Entity);

   function Default_Put (
      S : String;
      Source_Location : String;
      Enclosing_Entity : String)
      return Boolean;
   pragma Export (Ada, Default_Put, "__drake_debug_default_put");

   type Put_Handler is access function (
      S : String;
      Source_Location : String;
      Enclosing_Entity : String)
      return Boolean;
   pragma Suppress (Access_Check, Put_Handler);

   Put_Hook : Put_Handler := Default_Put'Access; -- not null
   pragma Suppress (Access_Check, Put_Hook);
   pragma Export (Ada, Put_Hook, "__drake_debug_put_hook");

   procedure Put (
      S : String;
      Source_Location : String := Debug.Source_Location;
      Enclosing_Entity : String := Debug.Enclosing_Entity);
   function Put (
      S : String;
      Source_Location : String := Debug.Source_Location;
      Enclosing_Entity : String := Debug.Enclosing_Entity)
      return Boolean;
   pragma Import (Ada, Put, "__drake_debug_put");

   --  for compiler-units
   procedure Runtime_Error (
      Condition : Boolean;
      S : String;
      Source_Location : String := Debug.Source_Location;
      Enclosing_Entity : String := Debug.Enclosing_Entity);
   pragma Export (Ada, Runtime_Error, "__drake_runtime_error");

private

   function Put_Impl (
      S : String;
      Source_Location : String := Debug.Source_Location;
      Enclosing_Entity : String := Debug.Enclosing_Entity)
      return Boolean;
   pragma Export (Ada, Put_Impl, "__drake_debug_put");

end System.Debug;
