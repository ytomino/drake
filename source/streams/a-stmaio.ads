pragma License (Unrestricted);
--  extended unit
with Ada.IO_Exceptions;
with Ada.IO_Modes;
with Ada.Streams.Stream_IO;
with System.Storage_Elements;
private with Ada.Finalization;
private with Ada.Streams.Naked_Stream_IO;
private with System.Native_IO;
package Ada.Storage_Mapped_IO is
   --  Memory-mapped I/O.

   type File_Mode is new IO_Modes.Inout_File_Mode;

   type Storage_Type is limited private;

--  subtype Mapped_Storage_Type is Storage_Type
--    with
--       Dynamic_Predicate => Is_Mapped (Mapped_Storage_Type),
--       Predicate_Failure => raise Status_Error;

   function Is_Mapped (Object : Storage_Type) return Boolean;
   pragma Inline (Is_Mapped);

   procedure Map (
      Object : in out Storage_Type;
      File : Streams.Stream_IO.File_Type;
      Offset : Streams.Stream_IO.Positive_Count := 1;
      Size : Streams.Stream_IO.Count := 0);
   function Map (
      File : Streams.Stream_IO.File_Type;
      Offset : Streams.Stream_IO.Positive_Count := 1;
      Size : Streams.Stream_IO.Count := 0)
      return Storage_Type;

   procedure Map (
      Object : in out Storage_Type;
      Mode : File_Mode := In_File;
      Name : String;
      Form : String; -- removed default
      Offset : Streams.Stream_IO.Positive_Count := 1;
      Size : Streams.Stream_IO.Count := 0);
   procedure Map (
      Object : in out Storage_Type;
      Mode : File_Mode := In_File;
      Name : String;
      Shared : IO_Modes.File_Shared_Spec := IO_Modes.By_Mode;
      Wait : Boolean := False;
      Overwrite : Boolean := True;
      Offset : Streams.Stream_IO.Positive_Count := 1;
      Size : Streams.Stream_IO.Count := 0);
   function Map (
      Mode : File_Mode := In_File;
      Name : String;
      Shared : IO_Modes.File_Shared_Spec := IO_Modes.By_Mode;
      Wait : Boolean := False;
      Overwrite : Boolean := True;
      Offset : Streams.Stream_IO.Positive_Count := 1;
      Size : Streams.Stream_IO.Count := 0)
      return Storage_Type;

   procedure Unmap (Object : in out Storage_Type);

   function Storage_Address (
      Object : Storage_Type) -- Mapped_Storage_Type
      return System.Address;
   function Storage_Size (
      Object : Storage_Type) -- Mapped_Storage_Type
      return System.Storage_Elements.Storage_Count;

   pragma Inline (Storage_Address);
   pragma Inline (Storage_Size);

   Status_Error : exception
      renames IO_Exceptions.Status_Error;
   Use_Error : exception
      renames IO_Exceptions.Use_Error;

private

   type Non_Controlled_Mapping is record
      Mapping : System.Native_IO.Mapping_Type;
      File : aliased Streams.Naked_Stream_IO.Non_Controlled_File_Type;
   end record;
   pragma Suppress_Initialization (Non_Controlled_Mapping);

   package Controlled is

      type Storage_Type is limited private;

      function Reference (Object : Storage_Mapped_IO.Storage_Type)
         return not null access Non_Controlled_Mapping;
      pragma Inline (Reference);

   private

      type Storage_Type is
         limited new Finalization.Limited_Controlled with
      record
         Data : aliased Non_Controlled_Mapping := (
            Mapping => (
               Storage_Address => System.Null_Address,
               others => <>),
            File => null);
      end record;

      overriding procedure Finalize (Object : in out Storage_Type);

   end Controlled;

   type Storage_Type is new Controlled.Storage_Type;

end Ada.Storage_Mapped_IO;
