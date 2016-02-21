pragma License (Unrestricted);
--  implementation unit
with Ada.Naked_Text_IO;
package Ada.Text_IO.Naked is

   function Non_Controlled (File : File_Type)
      return not null access Naked_Text_IO.Non_Controlled_File_Type;
   pragma Inline (Non_Controlled); -- renamed

private

   function Non_Controlled (File : File_Type)
      return not null access Naked_Text_IO.Non_Controlled_File_Type
      renames Controlled.Reference;

end Ada.Text_IO.Naked;
