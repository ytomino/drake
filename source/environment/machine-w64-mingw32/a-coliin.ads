pragma License (Unrestricted);
--  implementation unit
package Ada.Command_Line.Inside is
   pragma Preelaborate;

   function Argument_Count return Natural;
   pragma Inline (Argument_Count);

   --  Number => 0 means Command_Name
   function Argument (Number : Natural) return String;

end Ada.Command_Line.Inside;
