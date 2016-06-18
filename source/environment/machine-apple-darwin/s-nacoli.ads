pragma License (Unrestricted);
--  implementation unit specialized for POSIX (Darwin, FreeBSD, or Linux)
package System.Native_Command_Line is
   pragma Preelaborate;

   function Argument_Count return Natural;
   pragma Inline (Argument_Count);

   function Argument (Number : Natural) return String;
      --  Returns Command_Name if Number = 0.

end System.Native_Command_Line;
