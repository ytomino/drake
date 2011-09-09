pragma License (Unrestricted);
package Ada is
   pragma Pure;

   --  extended
   --  This is usable debug output.
   --  It placed in package Ada directly,
   --    therefore it makes no necessary additonal "with" clause.
   package Debug is
      function File return String;
      pragma Import (Intrinsic, File);
      function Line return Positive;
      pragma Import (Intrinsic, Line);
      function Source_Location return String;
      pragma Import (Intrinsic, Source_Location);
      function Enclosing_Entity return String;
      pragma Import (Intrinsic, Enclosing_Entity);
      procedure Put (
         S : String;
         Source_Location : String := Debug.Source_Location;
         Enclosing_Entity : String := Debug.Enclosing_Entity);
      function Put (
         S : String;
         Source_Location : String := Debug.Source_Location;
         Enclosing_Entity : String := Debug.Enclosing_Entity)
         return Boolean; -- always True to use in pragma Assert/Check
      pragma Import (Ada, Put, "__drake_debug_put");
   end Debug;

end Ada;
