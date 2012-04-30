with Ada.Unchecked_Deallocation;
package body Ada.Text_IO.Iterators is
   use type Line_Cursors.String_Access;

   --  implementation

   function Has_Element (Position : Line_Cursor) return Boolean is
   begin
      return Reference (Position) /= null;
   end Has_Element;

   function Lines (File : File_Type) return Lines_Type is
   begin
      return (File => File'Unrestricted_Access);
   end Lines;

   function Constant_Reference (
      Container : not null access constant Lines_Type; -- [gcc 4.6] aliased
      Position : Line_Cursor)
      return References.String.Constant_Reference_Type
   is
      pragma Unreferenced (Container);
   begin
      return (Element => Reference (Position));
   end Constant_Reference;

   function Iterate (Container : Lines_Type)
      return Lines_Iterator_Interfaces.Forward_Iterator'Class is
   begin
      return Line_Iterator'(File => Container.File);
   end Iterate;

   package body Line_Cursors is

      procedure Free is new Unchecked_Deallocation (String, String_Access);

      --  implementation

      function Reference (Position : Line_Cursor) return String_Access is
      begin
         return Position.Line;
      end Reference;

      procedure Assign (Position : out Line_Cursor; Line : String_Access) is
      begin
         Position.Line := Line;
         Position.Owner := Position'Unchecked_Access;
         Position.Last := True;
      end Assign;

      procedure Step (Position : in out Line_Cursor) is
      begin
         if not Position.Last then
            raise Program_Error;
         else
            Position.Last := False;
         end if;
      end Step;

      overriding procedure Adjust (Object : in out Line_Cursor) is
      begin
         if Object.Line /= null then
            Object.Owner.Line := null;
            Object.Owner := Object'Unrestricted_Access;
         end if;
      end Adjust;

      overriding procedure Finalize (Object : in out Line_Cursor) is
      begin
         pragma Assert (Object.Line = null
            or else Object.Owner = Object'Unrestricted_Access);
         Free (Object.Line);
      end Finalize;

   end Line_Cursors;

   overriding function First (Object : Line_Iterator) return Line_Cursor is
   begin
      return Result : Line_Cursor do
         Assign (Result, new String'(Get_Line (Object.File.all)));
      end return;
   end First;

   overriding function Next (Object : Line_Iterator; Position : Line_Cursor)
      return Line_Cursor is
   begin
      Step (Position'Unrestricted_Access.all);
      return Result : Line_Cursor do
         if not End_Of_File (Object.File.all) then
            Assign (Result, new String'(Get_Line (Object.File.all)));
         end if;
      end return;
   end Next;

end Ada.Text_IO.Iterators;
