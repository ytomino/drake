pragma License (Unrestricted);
--  extended unit
pragma Style_Checks (Off, Standard.Ascii);
--  package ASCII is "ASCII" in standard, but "Ascii" in GNAT style...
package Ada.Characters.ASCII is
   --  This is an alternative package of Standard.ASCII.
   pragma Pure;

   NUL : Character renames Standard.ASCII.NUL;

end Ada.Characters.ASCII;
