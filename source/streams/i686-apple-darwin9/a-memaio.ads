pragma License (Unrestricted);
--  extended unit
with Ada.IO_Exceptions;
with Ada.Streams.Stream_IO;
with System.Storage_Elements;
private with Ada.Finalization;
private with Ada.Streams.Stream_IO.Inside;
package Ada.Memory_Mapped_IO is
   --  This package provides memory-mapped I/O.

   subtype File_Mode is Streams.Stream_IO.File_Mode;
   function In_File return File_Mode
      renames Streams.Stream_IO.In_File;
   function Inout_File return File_Mode
      renames Streams.Stream_IO.Append_File;
   function "=" (Left, Right : File_Mode) return Boolean
      renames Streams.Stream_IO."=";

   type Mapping is limited private;

   function Is_Map (Object : Mapping) return Boolean;
   pragma Inline (Is_Map);

   procedure Map (
      Object : out Mapping;
      File : Streams.Stream_IO.File_Type;
      Offset : Streams.Stream_IO.Positive_Count := 1;
      Size : Streams.Stream_IO.Count := 0);

   procedure Map (
      Object : out Mapping;
      Mode : File_Mode := In_File;
      Name : String;
      Form : String := "";
      Offset : Streams.Stream_IO.Positive_Count := 1;
      Size : Streams.Stream_IO.Count := 0);

   procedure Unmap (Object : in out Mapping);

   function Address (Object : Mapping) return System.Address;
   pragma Inline (Address);
   function Size (Object : Mapping)
      return System.Storage_Elements.Storage_Count;
   pragma Inline (Size);

   Status_Error : exception renames IO_Exceptions.Status_Error;
   Use_Error : exception renames IO_Exceptions.Use_Error;

private

   type Mapping is new Ada.Finalization.Limited_Controlled with record
      Address : System.Address := System.Null_Address;
      Size : System.Storage_Elements.Storage_Count;
      File : aliased Streams.Stream_IO.Inside.Non_Controlled_File_Type;
   end record;

   overriding procedure Finalize (Object : in out Mapping);

end Ada.Memory_Mapped_IO;
