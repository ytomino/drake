pragma License (Unrestricted);
--  runtime unit specialized for Windows
package System.Termination is
   pragma Preelaborate;

   --  write to standard error output
   procedure Error_Put_Line (S : String);

   --  force to abort
   procedure Force_Abort;
   pragma No_Return (Force_Abort);

   --  register exit handler

   type Exit_Handler is access procedure;
   pragma Favor_Top_Level (Exit_Handler);

   procedure Register_Exit (Handler : not null Exit_Handler);

end System.Termination;
