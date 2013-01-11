pragma License (Unrestricted);
--  extended unit
--  with Ada.Iterator_Interfaces; -- [gcc 4.6] can not instantiate it
with Ada.References.String;
private with Ada.Finalization;
package Ada.Text_IO.Iterators is
   --  Iterators for Ada.Text_IO.File_Type.

   --  per line

   type Line_Cursor is private;
   pragma Preelaborable_Initialization (Line_Cursor);

   function Has_Element (Position : Line_Cursor) return Boolean;

   type Lines_Type is tagged limited private;
--    with -- [gcc 4.6]
--       Constant_Indexing => Constant_Reference,
--       Default_Iterator => Iterate,
--       Iterator_Element => String;

   function Lines (File : File_Type) return Lines_Type;

   function Constant_Reference (
      Container : not null access constant Lines_Type; -- [gcc 4.6] aliased
      Position : Line_Cursor)
      return References.String.Constant_Reference_Type;

   package Lines_Iterator_Interfaces is
--    new Iterator_Interfaces (Line_Cursor, Has_Element);
      --  [gcc 4.6] Cursor is incomplete type
      type Forward_Iterator is limited interface;
      function First (Object : Forward_Iterator)
         return Line_Cursor is abstract;
      function Next (Object : Forward_Iterator; Position : Line_Cursor)
         return Line_Cursor is abstract;
   end Lines_Iterator_Interfaces;

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
      for Line_Cursor_Access'Storage_Size use 0;
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
