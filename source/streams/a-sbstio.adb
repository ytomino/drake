with Ada.Unchecked_Deallocation;
package body Ada.Streams.Buffer_Storage_IO is

   procedure Free is new Unchecked_Deallocation (
      Stream_Element_Array,
      Stream_Element_Array_Access);

   procedure Free is new Unchecked_Deallocation (
      Stream_Type,
      Stream_Access);

   procedure Set_Size (
      Storage : in out Stream_Element_Array_Access;
      New_Size : Stream_Element_Count);
   procedure Set_Size (
      Storage : in out Stream_Element_Array_Access;
      New_Size : Stream_Element_Count)
   is
      Old : Stream_Element_Array_Access := Storage;
      subtype Copy_Range is Stream_Element_Count range
         1 ..
         Stream_Element_Count'Min (New_Size, Old'Last);
   begin
      Storage := new Stream_Element_Array (
         1 ..
         Stream_Element_Count'Max (256, New_Size));
      Storage (Copy_Range) := Old (Copy_Range);
      Free (Old);
   end Set_Size;

   --  implementation

   function Size (Object : Buffer) return Stream_Element_Count is
   begin
      return Object.Stream.Last;
   end Size;

   procedure Set_Size (
      Object : in out Buffer;
      New_Size : Stream_Element_Count) is
   begin
      Set_Size (Object.Stream.Storage, New_Size);
      if Object.Stream.Last > New_Size then
         Object.Stream.Last := New_Size;
         if Object.Stream.Index > New_Size + 1 then
            Object.Stream.Index := New_Size + 1;
         end if;
      end if;
   end Set_Size;

   function Address (Object : Buffer) return System.Address is
   begin
      return Object.Stream.Storage.all'Address;
   end Address;

   procedure Query_Elements (
      Object : Buffer;
      Process : not null access procedure (
         Item : System.Storage_Elements.Storage_Array))
   is
      Item : System.Storage_Elements.Storage_Array (
         1 ..
         System.Storage_Elements.Storage_Offset (Object.Stream.Last));
      for Item'Address use Object.Stream.Storage.all'Address;
   begin
      Process (Item);
   end Query_Elements;

   procedure Update_Elements (
      Object : in out Buffer;
      Process : not null access procedure (
         Item : in out System.Storage_Elements.Storage_Array))
   is
      Item : System.Storage_Elements.Storage_Array (
         1 ..
         System.Storage_Elements.Storage_Offset (Object.Stream.Last));
      for Item'Address use Object.Stream.Storage.all'Address;
   begin
      Process (Item);
   end Update_Elements;

   function Stream (Object : Buffer)
      return not null access Root_Stream_Type'Class is
   begin
      return Object.Stream;
   end Stream;

   overriding procedure Read (
      Stream : in out Stream_Type;
      Item : out Stream_Element_Array;
      Last : out Stream_Element_Offset)
   is
      Length : Stream_Element_Count := Item'Length;
      Rest : constant Stream_Element_Count := Stream.Last - Stream.Index + 1;
   begin
      if Length > Rest then
         Length := Rest;
      end if;
      Last := Item'First + Length - 1;
      Item (Item'First .. Last) := Stream.Storage (
         Stream.Index ..
         Stream.Index + Length - 1);
      Stream.Index := Stream.Index + Length;
   end Read;

   overriding procedure Write (
      Stream : in out Stream_Type;
      Item : Stream_Element_Array)
   is
      Length : constant Stream_Element_Count := Item'Length;
      New_Index : constant Stream_Element_Count := Stream.Index + Length;
      Copy_Last : constant Stream_Element_Offset := New_Index - 1;
   begin
      if Copy_Last > Stream.Storage'Last then
         Set_Size (
            Stream.Storage,
            Stream_Element_Count'Max (
               Copy_Last,
               Stream.Storage'Length * 2));
      end if;
      Stream.Storage (Stream.Index .. Copy_Last) := Item;
      Stream.Index := New_Index;
      if Stream.Last < Copy_Last then
         Stream.Last := Copy_Last;
      end if;
   end Write;

   overriding procedure Set_Index (
      Stream : in out Stream_Type;
      To : Stream_Element_Positive_Count) is
   begin
      if To > Stream.Last + 1 then
         raise Constraint_Error;
      end if;
      Stream.Index := To;
   end Set_Index;

   overriding function Index (Stream : Stream_Type)
      return Stream_Element_Positive_Count is
   begin
      return Stream.Index;
   end Index;

   overriding function Size (Stream : Stream_Type)
      return Stream_Element_Count is
   begin
      return Stream.Last;
   end Size;

   overriding procedure Initialize (Object : in out Buffer) is
   begin
      Object.Stream := new Stream_Type;
      Object.Stream.Storage := new Stream_Element_Array (1 .. 256);
      Object.Stream.Last := 0;
      Object.Stream.Index := 1;
   end Initialize;

   overriding procedure Finalize (Object : in out Buffer) is
   begin
      if Object.Stream /= null then
         Free (Object.Stream.Storage);
         Free (Object.Stream);
      end if;
   end Finalize;

   overriding procedure Adjust (Object : in out Buffer) is
      Old_Stream : constant Stream_Access := Object.Stream;
   begin
      Object.Stream := new Stream_Type;
      Object.Stream.Storage :=
         new Stream_Element_Array'(Object.Stream.Storage.all);
      Object.Stream.Last := Old_Stream.Last;
      Object.Stream.Index := Old_Stream.Index;
   end Adjust;

   package body No_Primitives is

      procedure Write (
         Stream : not null access Root_Stream_Type'Class;
         Object : Buffer) is
      begin
         Streams.Write (
            Stream.all,
            Object.Stream.Storage (1 .. Object.Stream.Last));
      end Write;

   end No_Primitives;

end Ada.Streams.Buffer_Storage_IO;
