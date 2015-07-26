pragma License (Unrestricted);
package Ada is
   pragma Pure;

   --  extended
   --  This package creates usable debug output.
   --  It is placed in the package Ada directly,
   --    therefore it does not need to write any additional "with" clause.
   package Debug is
      function File return String
         with Import, Convention => Intrinsic;
      function Line return Positive
         with Import, Convention => Intrinsic;
      function Source_Location return String
         with Import, Convention => Intrinsic;
      function Enclosing_Entity return String
         with Import, Convention => Intrinsic;
      procedure Put (
         S : String;
         Source_Location : String := Debug.Source_Location;
         Enclosing_Entity : String := Debug.Enclosing_Entity)
         with Import, Convention => Ada, External_Name => "__drake_debug_put";
      function Put (
         S : String;
         Source_Location : String := Debug.Source_Location;
         Enclosing_Entity : String := Debug.Enclosing_Entity)
         return Boolean -- always True to use in pragma Assert/Check
         with Import, Convention => Ada, External_Name => "__drake_debug_put";
   end Debug;

end Ada;
