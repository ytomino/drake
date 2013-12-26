pragma License (Unrestricted);
--  extended unit
with Ada.Iterator_Interfaces;
with Ada.References.Strings;
private with Ada.Finalization;
private with Ada.Streams;
package Ada.Text_IO.Iterators is
   --  Iterators for Ada.Text_IO.File_Type.

   --  per line

   type Lines_Type is tagged limited private
      with
         Constant_Indexing => Constant_Reference,
         Default_Iterator => Iterate,
         Iterator_Element => String;

   function Lines (File : File_Type) return Lines_Type;

   type Line_Cursor is private;
   pragma Preelaborable_Initialization (Line_Cursor);

   function Has_Element (Position : Line_Cursor) return Boolean;

   function Constant_Reference (
      Container : aliased Lines_Type;
      Position : Line_Cursor)
      return References.Strings.Constant_Reference_Type;

   package Lines_Iterator_Interfaces is
      new Iterator_Interfaces (Line_Cursor, Has_Element);

   function Iterate (Container : Lines_Type)
      return Lines_Iterator_Interfaces.Forward_Iterator'Class;

private

   type Lines_Type is tagged limited record
      File : File_Access;
   end record;

   package Line_Cursors is

      type String_Access is access String;
      type Line_Cursor is private;

      function Reference (Position : Line_Cursor) return String_Access;
      pragma Inline (Reference);

      procedure Assign (Position : out Line_Cursor; Line : String_Access);
      pragma Inline (Assign);

      procedure Step (Position : in out Line_Cursor);
      pragma Inline (Step);

   private

      type Line_Cursor_Access is access all Line_Cursor;
      for Line_Cursor_Access'Storage_Size use 0;
      type Line_Cursor is new Finalization.Controlled with record
         Line : String_Access;
         Owner : Line_Cursor_Access;
         Last : Boolean;
      end record;

      overriding procedure Adjust (Object : in out Line_Cursor);
      overriding procedure Finalize (Object : in out Line_Cursor);

      package Streaming is

         procedure Missing_Read (
            Stream : not null access Streams.Root_Stream_Type'Class;
            Item : out Line_Cursor);
         function Missing_Input (
            Stream : not null access Streams.Root_Stream_Type'Class)
            return Line_Cursor;
         procedure Missing_Write (
            Stream : not null access Streams.Root_Stream_Type'Class;
            Item : Line_Cursor);

         pragma Import (Ada, Missing_Read, "__drake_program_error");
         pragma Import (Ada, Missing_Input, "__drake_program_error");
         pragma Import (Ada, Missing_Write, "__drake_program_error");

      end Streaming;

      for Line_Cursor'Read use Streaming.Missing_Read;
      for Line_Cursor'Input use Streaming.Missing_Input;
      for Line_Cursor'Write use Streaming.Missing_Write;
      for Line_Cursor'Output use Streaming.Missing_Write;

   end Line_Cursors;

   type Line_Cursor is new Line_Cursors.Line_Cursor;

   type Line_Iterator is new Lines_Iterator_Interfaces.Forward_Iterator
      with
   record
      File : File_Access;
   end record;

   overriding function First (Object : Line_Iterator) return Line_Cursor;
   overriding function Next (Object : Line_Iterator; Position : Line_Cursor)
      return Line_Cursor;

   package Streaming is

      procedure Missing_Read (
         Stream : not null access Streams.Root_Stream_Type'Class;
         Item : out Line_Iterator);
      function Missing_Input (
         Stream : not null access Streams.Root_Stream_Type'Class)
         return Line_Iterator;
      procedure Missing_Write (
         Stream : not null access Streams.Root_Stream_Type'Class;
         Item : Line_Iterator);

      pragma Import (Ada, Missing_Read, "__drake_program_error");
      pragma Import (Ada, Missing_Input, "__drake_program_error");
      pragma Import (Ada, Missing_Write, "__drake_program_error");

   end Streaming;

   for Line_Iterator'Read use Streaming.Missing_Read;
   for Line_Iterator'Input use Streaming.Missing_Input;
   for Line_Iterator'Write use Streaming.Missing_Write;
   for Line_Iterator'Output use Streaming.Missing_Write;

end Ada.Text_IO.Iterators;
