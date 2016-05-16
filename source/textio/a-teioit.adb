package body Ada.Text_IO.Iterators is

   function Unchecked_Next (Object : Line_Iterator) return Line_Cursor;
   function Unchecked_Next (Object : Line_Iterator) return Line_Cursor is
   begin
      if End_Of_File (Object.Lines.File.all) then
         return 0; -- No_Element
      else
         Free (Object.Lines.Item);
         Object.Lines.Count := Object.Lines.Count + 1;
         Overloaded_Get_Line (
            Object.Lines.File.all,
            Object.Lines.Item); -- allocation
         return Line_Cursor (Object.Lines.Count);
      end if;
   end Unchecked_Next;

   --  implementation

   function Lines (
      File : File_Type)
      return Lines_Type
   is
      pragma Check (Dynamic_Predicate,
         Check => Is_Open (File) or else raise Status_Error);
      pragma Check (Dynamic_Predicate,
         Check => Mode (File) = In_File or else raise Mode_Error);
   begin
      return (Finalization.Limited_Controlled with
         File => File'Unrestricted_Access,
         Item => null,
         Count => 0);
   end Lines;

   function Has_Element (Position : Line_Cursor) return Boolean is
   begin
      return Position > 0;
   end Has_Element;

   function Element (Container : Lines_Type'Class; Position : Line_Cursor)
      return String is
   begin
      return Constant_Reference (Lines_Type (Container), Position).Element.all;
   end Element;

   function Constant_Reference (
      Container : aliased Lines_Type;
      Position : Line_Cursor)
      return References.Strings.Constant_Reference_Type
   is
      pragma Check (Pre,
         Check =>
            Integer (Position) = Container.Count or else raise Status_Error);
   begin
      return (Element => Container.Item);
   end Constant_Reference;

   function Iterate (Container : Lines_Type'Class)
      return Lines_Iterator_Interfaces.Forward_Iterator'Class is
   begin
      return Line_Iterator'(Lines => Container'Unrestricted_Access);
   end Iterate;

   overriding procedure Finalize (Object : in out Lines_Type) is
   begin
      Free (Object.Item);
   end Finalize;

   overriding function First (Object : Line_Iterator) return Line_Cursor is
      pragma Check (Pre, Object.Lines.Count = 0 or else raise Status_Error);
   begin
      return Unchecked_Next (Object);
   end First;

   overriding function Next (Object : Line_Iterator; Position : Line_Cursor)
      return Line_Cursor
   is
      pragma Check (Pre,
         Check =>
            Integer (Position) = Object.Lines.Count
            or else raise Status_Error);
   begin
      return Unchecked_Next (Object);
   end Next;

end Ada.Text_IO.Iterators;
