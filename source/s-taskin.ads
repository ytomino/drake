pragma License (Unrestricted);
--  implementation package required by compiler
package System.Tasking is

   --  required for local tagged types by compiler (s-taskin.ads)
   subtype Master_Level is Integer;
   subtype Master_ID is Master_Level;

end System.Tasking;
