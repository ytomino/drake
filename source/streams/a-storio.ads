pragma License (Unrestricted);
with Ada.IO_Exceptions;
with System.Storage_Elements;
generic
   type Element_Type is private;
package Ada.Storage_IO is
   pragma Preelaborate;

   Buffer_Size : constant System.Storage_Elements.Storage_Count :=
      Element_Type'Max_Size_In_Storage_Elements; --  implementation-defined
   subtype Buffer_Type is
      System.Storage_Elements.Storage_Array (1 .. Buffer_Size);

   --  Input and output operations

   procedure Read (Buffer : Buffer_Type; Item : out Element_Type);

   procedure Write (Buffer : out Buffer_Type; Item : Element_Type);

   --  Exceptions

   Data_Error : exception renames IO_Exceptions.Data_Error;

end Ada.Storage_IO;
