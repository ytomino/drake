pragma License (Unrestricted);
--  runtime unit
package System.Debug is
   pragma Preelaborate;

   --  implementation of Ada.Debug

   function File return String
      with Import, Convention => Intrinsic;

   function Line return Positive
      with Import, Convention => Intrinsic;

   function Source_Location return String
      with Import, Convention => Intrinsic;

   function Enclosing_Entity return String
      with Import, Convention => Intrinsic;

   function Default_Put (
      S : String;
      Source_Location : String;
      Enclosing_Entity : String)
      return Boolean
      with Export,
         Convention => Ada, External_Name => "__drake_debug_default_put";

   type Put_Handler is access function (
      S : String;
      Source_Location : String;
      Enclosing_Entity : String)
      return Boolean;

   Put_Hook : not null Put_Handler := Default_Put'Access
      with Export,
         Convention => Ada, External_Name => "__drake_debug_put_hook";

   procedure Put (
      S : String;
      Source_Location : String := Debug.Source_Location;
      Enclosing_Entity : String := Debug.Enclosing_Entity)
      with Import, Convention => Ada, External_Name => "__drake_debug_put";
   function Put (
      S : String;
      Source_Location : String := Debug.Source_Location;
      Enclosing_Entity : String := Debug.Enclosing_Entity)
      return Boolean
      with Import, Convention => Ada, External_Name => "__drake_debug_put";

   --  for compiler-units
   function Runtime_Error (
      S : String;
      Source_Location : String := Debug.Source_Location;
      Enclosing_Entity : String := Debug.Enclosing_Entity)
      return Boolean
      with Export, Convention => Ada, External_Name => "__drake_runtime_error";
   pragma Machine_Attribute (Runtime_Error, "noreturn");
   pragma Inline_Always (Runtime_Error);
      --  [gcc-7] can not skip calling Raise_Assertion_Error after "noreturn"

private

   function Put_Impl (
      S : String;
      Source_Location : String := Debug.Source_Location;
      Enclosing_Entity : String := Debug.Enclosing_Entity)
      return Boolean
      with Export, Convention => Ada, External_Name => "__drake_debug_put";

end System.Debug;
