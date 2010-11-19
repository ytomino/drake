pragma License (Unrestricted);
pragma Style_Checks (Off, Standard.Ascii);
--  package ASCII is "ASCII" in standard, but "Ascii" in GNAT style...
--  extended package
package Ada.Characters.ASCII is
   pragma Pure;

   NUL : Character renames Standard.ASCII.NUL;

end Ada.Characters.ASCII;
