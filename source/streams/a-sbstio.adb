with Ada.Exceptions;
with Ada.Unchecked_Deallocation;
with System.Memory.Allocated_Size;
package body Ada.Streams.Buffer_Storage_IO is

   procedure Free is new Unchecked_Deallocation (
      Stream_Type,
      Stream_Access);

   procedure Allocate (
      Stream : in out Stream_Type;
      Size : Stream_Element_Count);
   procedure Allocate (
      Stream : in out Stream_Type;
      Size : Stream_Element_Count) is
   begin
      Stream.Data := System.Memory.Allocate (
         System.Storage_Elements.Storage_Count (Size));
      Stream.Capacity := Stream_Element_Offset (
         System.Memory.Allocated_Size (Stream.Data));
   end Allocate;

   procedure Reallocate (
      Stream : in out Stream_Type;
      Size : Stream_Element_Count);
   procedure Reallocate (
      Stream : in out Stream_Type;
      Size : Stream_Element_Count) is
   begin
      Stream.Data := System.Memory.Reallocate (
         Stream.Data,
         System.Storage_Elements.Storage_Count (Size));
      Stream.Capacity := Stream_Element_Offset (
         System.Memory.Allocated_Size (Stream.Data));
   end Reallocate;

   procedure Deallocate (Stream : in out Stream_Type);
   procedure Deallocate (Stream : in out Stream_Type) is
   begin
      System.Memory.Free (Stream.Data);
      Stream.Data := System.Null_Address;
      Stream.Capacity := 0;
   end Deallocate;

   --  implementation

   function Size (Object : Buffer) return Stream_Element_Count is
   begin
      return Object.Stream.Last;
   end Size;

   procedure Set_Size (
      Object : in out Buffer;
      New_Size : Stream_Element_Count) is
   begin
      Reallocate (Object.Stream.all, New_Size);
      if Object.Stream.Last > New_Size then
         Object.Stream.Last := New_Size;
         if Object.Stream.Index > New_Size + 1 then
            Object.Stream.Index := New_Size + 1;
         end if;
      end if;
   end Set_Size;

   function Address (Object : Buffer) return System.Address is
   begin
      return Object.Stream.Data;
   end Address;

   function Size (Object : Buffer)
      return System.Storage_Elements.Storage_Count is
   begin
      return System.Storage_Elements.Storage_Count (Object.Stream.Last);
   end Size;

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
      if Length > 0 and then Rest = 0 then
         Exceptions.Raise_Exception_From_Here (End_Error'Identity);
      end if;
      if Length > Rest then
         Length := Rest;
      end if;
      Last := Item'First + Length - 1;
      declare
         Stream_Item : Stream_Element_Array (1 .. Stream.Last);
         for Stream_Item'Address use Stream.Data;
      begin
         Item (Item'First .. Last) := Stream_Item (
            Stream.Index ..
            Stream.Index + Length - 1);
      end;
      Stream.Index := Stream.Index + Length;
   end Read;

   overriding procedure Write (
      Stream : in out Stream_Type;
      Item : Stream_Element_Array)
   is
      Length : constant Stream_Element_Count := Item'Length;
      New_Index : constant Stream_Element_Count := Stream.Index + Length;
      Copy_Last : constant Stream_Element_Offset := New_Index - 1;
      New_Last : constant Stream_Element_Offset :=
         Stream_Element_Offset'Max (Stream.Last, Copy_Last);
   begin
      if Copy_Last > Stream.Capacity then
         Reallocate (
            Stream,
            Stream_Element_Count'Max (
               Copy_Last,
               Stream.Capacity * 2));
      end if;
      declare
         Stream_Item : Stream_Element_Array (1 .. New_Last);
         for Stream_Item'Address use Stream.Data;
      begin
         Stream_Item (Stream.Index .. Copy_Last) := Item;
      end;
      Stream.Index := New_Index;
      Stream.Last := New_Last;
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
      Object.Stream := new Stream_Type'(
         Data => System.Null_Address,
         Capacity => 0,
         Last => 0,
         Index => 1);
   end Initialize;

   overriding procedure Finalize (Object : in out Buffer) is
   begin
      if Object.Stream /= null then
         Deallocate (Object.Stream.all);
         Free (Object.Stream);
      end if;
   end Finalize;

   overriding procedure Adjust (Object : in out Buffer) is
      Old_Stream : constant Stream_Access := Object.Stream;
   begin
      Object.Stream := new Stream_Type;
      Allocate (Object.Stream.all, Old_Stream.Last);
      Object.Stream.Last := Old_Stream.Last;
      Object.Stream.Index := Old_Stream.Index;
   end Adjust;

   package body Streaming is

      procedure Write (
         Stream : not null access Root_Stream_Type'Class;
         Item : Buffer)
      is
         Stream_Item : Stream_Element_Array (1 .. Item.Stream.Last);
         for Stream_Item'Address use Item.Stream.Data;
      begin
         Streams.Write (Stream.all, Stream_Item);
      end Write;

   end Streaming;

end Ada.Streams.Buffer_Storage_IO;
