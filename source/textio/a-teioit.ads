pragma License (Unrestricted);
--  extended unit
with Ada.Iterator_Interfaces;
with Ada.References.String;
private with Ada.Finalization;
package Ada.Text_IO.Iterators is
   --  Iterators for Ada.Text_IO.File_Type.

   --  per line

   type Line_Cursor is private;
   pragma Preelaborable_Initialization (Line_Cursor);

   function Has_Element (Position : Line_Cursor) return Boolean;

   type Lines_Type is tagged limited private
      with
         Constant_Indexing => Constant_Reference,
         Default_Iterator => Iterate,
         Iterator_Element => String;

   function Lines (File : File_Type) return Lines_Type;

   function Constant_Reference (
      Container : aliased Lines_Type;
      Position : Line_Cursor)
      return References.String.Constant_Reference_Type;

   package Lines_Iterator_Interfaces is
      new Iterator_Interfaces (Line_Cursor, Has_Element);

   function Iterate (Container : Lines_Type)
      return Lines_Iterator_Interfaces.Forward_Iterator'Class;

private

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
      type Line_Cursor is new Finalization.Controlled with record
         Line : String_Access;
         Owner : Line_Cursor_Access;
         Last : Boolean;
      end record;

      overriding procedure Adjust (Object : in out Line_Cursor);
      overriding procedure Finalize (Object : in out Line_Cursor);

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

   type Lines_Type is tagged limited record
      File : File_Access;
   end record;

end Ada.Text_IO.Iterators;
