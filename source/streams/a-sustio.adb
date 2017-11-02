with Ada.Exception_Identification.From_Here;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with System.Address_To_Named_Access_Conversions;
with System.Standard_Allocators;
with System.System_Allocators.Allocated_Size;
package body Ada.Streams.Unbounded_Storage_IO is
   use Exception_Identification.From_Here;
   use type System.Storage_Elements.Storage_Offset;

   procedure Free is
      new Unchecked_Deallocation (Stream_Type, Stream_Access);

   package DA_Conv is
      new System.Address_To_Named_Access_Conversions (Data, Data_Access);

   subtype Nonnull_Data_Access is not null Data_Access;

   function Upcast is
      new Unchecked_Conversion (
         Nonnull_Data_Access,
         System.Reference_Counting.Container);
   function Downcast is
      new Unchecked_Conversion (
         System.Reference_Counting.Container,
         Nonnull_Data_Access);

   type Data_Access_Access is access all Nonnull_Data_Access;
   type Container_Access is access all System.Reference_Counting.Container;

   function Upcast is
      new Unchecked_Conversion (Data_Access_Access, Container_Access);

   function Allocation_Size (Capacity : System.Reference_Counting.Length_Type)
      return System.Storage_Elements.Storage_Count;
   function Allocation_Size (Capacity : System.Reference_Counting.Length_Type)
      return System.Storage_Elements.Storage_Count
   is
      Dividable : constant Boolean :=
         Stream_Element_Array'Component_Size rem Standard'Storage_Unit = 0;
      Header_Size : constant System.Storage_Elements.Storage_Count :=
         Data'Size / Standard'Storage_Unit;
      Use_Size : System.Storage_Elements.Storage_Count;
   begin
      if Dividable then -- optimized for packed
         Use_Size :=
            Capacity
            * (Stream_Element_Array'Component_Size / Standard'Storage_Unit);
      else -- unpacked
         Use_Size :=
            (Capacity * Stream_Element_Array'Component_Size
               + (Standard'Storage_Unit - 1))
            / Standard'Storage_Unit;
      end if;
      return Header_Size + Use_Size;
   end Allocation_Size;

   procedure Adjust_Allocated (Data : not null Data_Access);
   procedure Adjust_Allocated (Data : not null Data_Access) is
      Dividable : constant Boolean :=
         Stream_Element_Array'Component_Size rem Standard'Storage_Unit = 0;
      Header_Size : constant System.Storage_Elements.Storage_Count :=
         Unbounded_Storage_IO.Data'Size / Standard'Storage_Unit;
      M : constant System.Address := DA_Conv.To_Address (Data);
      Usable_Size : constant System.Storage_Elements.Storage_Count :=
         System.System_Allocators.Allocated_Size (M) - Header_Size;
      Allocated_Capacity : Stream_Element_Count;
   begin
      if Dividable then -- optimized for packed
         Allocated_Capacity := Stream_Element_Offset (
            Usable_Size
               / (
                  Stream_Element_Array'Component_Size
                  / Standard'Storage_Unit));
      else -- unpacked
         Allocated_Capacity := Stream_Element_Offset (
            Usable_Size
               * Standard'Storage_Unit
               / Stream_Element_Array'Component_Size);
      end if;
      Data.Capacity := Allocated_Capacity;
      Data.Storage := M + Header_Size;
   end Adjust_Allocated;

   function Allocate_Data (
      Max_Length : System.Reference_Counting.Length_Type;
      Capacity : System.Reference_Counting.Length_Type)
      return not null Data_Access;
   function Allocate_Data (
      Max_Length : System.Reference_Counting.Length_Type;
      Capacity : System.Reference_Counting.Length_Type)
      return not null Data_Access
   is
      M : constant System.Address :=
         System.Standard_Allocators.Allocate (Allocation_Size (Capacity));
      Result : constant not null Data_Access := DA_Conv.To_Pointer (M);
   begin
      Result.Reference_Count := 1;
      Result.Max_Length := Max_Length;
      Adjust_Allocated (Result);
      return Result;
   end Allocate_Data;

   procedure Free_Data (Data : in out System.Reference_Counting.Data_Access);
   procedure Free_Data (Data : in out System.Reference_Counting.Data_Access) is
   begin
      System.Standard_Allocators.Free (DA_Conv.To_Address (Downcast (Data)));
      Data := null;
   end Free_Data;

   procedure Reallocate_Data (
      Data : aliased in out not null System.Reference_Counting.Data_Access;
      Length : System.Reference_Counting.Length_Type;
      Max_Length : System.Reference_Counting.Length_Type;
      Capacity : System.Reference_Counting.Length_Type);
   procedure Reallocate_Data (
      Data : aliased in out not null System.Reference_Counting.Data_Access;
      Length : System.Reference_Counting.Length_Type;
      Max_Length : System.Reference_Counting.Length_Type;
      Capacity : System.Reference_Counting.Length_Type)
   is
      pragma Unreferenced (Length);
      M : constant System.Address :=
         System.Standard_Allocators.Reallocate (
            DA_Conv.To_Address (Downcast (Data)),
            Allocation_Size (Capacity));
   begin
      Data := Upcast (DA_Conv.To_Pointer (M));
      Downcast (Data).Max_Length := Max_Length;
      Adjust_Allocated (Downcast (Data));
   end Reallocate_Data;

   procedure Copy_Data (
      Target : out not null System.Reference_Counting.Data_Access;
      Source : not null System.Reference_Counting.Data_Access;
      Length : System.Reference_Counting.Length_Type;
      Max_Length : System.Reference_Counting.Length_Type;
      Capacity : System.Reference_Counting.Length_Type);
   procedure Copy_Data (
      Target : out not null System.Reference_Counting.Data_Access;
      Source : not null System.Reference_Counting.Data_Access;
      Length : System.Reference_Counting.Length_Type;
      Max_Length : System.Reference_Counting.Length_Type;
      Capacity : System.Reference_Counting.Length_Type)
   is
      Data : constant not null Data_Access :=
         Allocate_Data (Max_Length, Capacity);
      Source_Storage_All : Stream_Element_Array (
         1 .. Stream_Element_Offset (Length));
      for Source_Storage_All'Address use Downcast (Source).Storage;
      Target_Storage_All : Stream_Element_Array (
         1 .. Stream_Element_Offset (Length));
      for Target_Storage_All'Address use Data.Storage;
   begin
      Target_Storage_All := Source_Storage_All;
      Target := Upcast (Data);
   end Copy_Data;

   procedure Reallocate (
      Buffer : in out Non_Controlled_Buffer_Type;
      Length : Stream_Element_Count;
      Size : Stream_Element_Count);
   procedure Reallocate (
      Buffer : in out Non_Controlled_Buffer_Type;
      Length : Stream_Element_Count;
      Size : Stream_Element_Count) is
   begin
      System.Reference_Counting.Unique (
         Target => Upcast (Buffer.Data'Unchecked_Access),
         Target_Length => System.Reference_Counting.Length_Type (Buffer.Last),
         Target_Capacity =>
            System.Reference_Counting.Length_Type (Buffer.Data.Capacity),
         New_Length => System.Reference_Counting.Length_Type (Length),
         New_Capacity => System.Reference_Counting.Length_Type (Size),
         Sentinel => Upcast (Empty_Data'Unrestricted_Access),
         Reallocate => Reallocate_Data'Access,
         Copy => Copy_Data'Access,
         Free => Free_Data'Access);
   end Reallocate;

   procedure Unique (Buffer : in out Non_Controlled_Buffer_Type);
   procedure Unique (Buffer : in out Non_Controlled_Buffer_Type) is
   begin
      if System.Reference_Counting.Shared (Upcast (Buffer.Data)) then
         Reallocate (
            Buffer,
            Buffer.Last,
            Buffer.Data.Capacity); -- not shrinking
      end if;
   end Unique;

   procedure Set_Size (
      Buffer : in out Non_Controlled_Buffer_Type;
      Size : Stream_Element_Count);
   procedure Set_Size (
      Buffer : in out Non_Controlled_Buffer_Type;
      Size : Stream_Element_Count)
   is
      Old_Capacity : constant Stream_Element_Count := Buffer.Data.Capacity;
      Failure : Boolean;
   begin
      System.Reference_Counting.In_Place_Set_Length (
         Target_Data => Upcast (Buffer.Data),
         Target_Length => System.Reference_Counting.Length_Type (Buffer.Last),
         Target_Max_Length => Buffer.Data.Max_Length,
         Target_Capacity =>
            System.Reference_Counting.Length_Type (Old_Capacity),
         New_Length => System.Reference_Counting.Length_Type (Size),
         Failure => Failure);
      if Failure then
         declare
            New_Capacity : Stream_Element_Count;
         begin
            if Old_Capacity >= Size then
               New_Capacity := Old_Capacity; -- not shrinking
            else
               New_Capacity :=
                  Stream_Element_Offset'Max (Old_Capacity * 2, Size);
            end if;
            Reallocate (Buffer, Size, New_Capacity);
         end;
      end if;
      Buffer.Last := Size;
   end Set_Size;

   --  implementation

   procedure Reset (Object : in out Buffer_Type) is
      NC_Object : Non_Controlled_Buffer_Type
         renames Controlled.Reference (Object).all;
   begin
      NC_Object.Index := 1;
   end Reset;

   function Size (Object : Buffer_Type) return Stream_Element_Count is
      NC_Object : Non_Controlled_Buffer_Type
         renames Controlled.Reference (Object).all;
   begin
      return NC_Object.Last;
   end Size;

   procedure Set_Size (
      Object : in out Buffer_Type;
      Size : Stream_Element_Count)
   is
      NC_Object : Non_Controlled_Buffer_Type
         renames Controlled.Reference (Object).all;
   begin
      Set_Size (NC_Object, Size);
      if NC_Object.Index > Size + 1 then
         NC_Object.Index := Size + 1;
      end if;
   end Set_Size;

   function Capacity (Object : Buffer_Type) return Stream_Element_Count is
      NC_Object : Non_Controlled_Buffer_Type
         renames Controlled.Reference (Object).all;
   begin
      return NC_Object.Data.Capacity;
   end Capacity;

   procedure Reserve_Capacity (
      Object : in out Buffer_Type;
      Capacity : Stream_Element_Count)
   is
      NC_Object : Non_Controlled_Buffer_Type
         renames Controlled.Reference (Object).all;
      New_Capacity : constant Stream_Element_Count :=
         Stream_Element_Offset'Max (Capacity, NC_Object.Last);
   begin
      Reallocate (NC_Object, NC_Object.Last, New_Capacity);
   end Reserve_Capacity;

   function Storage_Address (Object : aliased in out Buffer_Type)
      return System.Address
   is
      NC_Object : Non_Controlled_Buffer_Type
         renames Controlled.Reference (Object).all;
   begin
      Unique (NC_Object);
      return NC_Object.Data.Storage;
   end Storage_Address;

   function Storage_Size (Object : Buffer_Type)
      return System.Storage_Elements.Storage_Count
   is
      NC_Object : Non_Controlled_Buffer_Type
         renames Controlled.Reference (Object).all;
   begin
      return System.Storage_Elements.Storage_Offset (NC_Object.Last);
   end Storage_Size;

   function Stream (Object : Buffer_Type)
      return not null access Root_Stream_Type'Class
   is
      NC_Object_Ref : constant not null access Non_Controlled_Buffer_Type :=
         Controlled.Reference (Object);
   begin
      if NC_Object_Ref.Stream = null then
         NC_Object_Ref.Stream := new Stream_Type'(Buffer => NC_Object_Ref);
      end if;
      return NC_Object_Ref.Stream;
   end Stream;

   procedure Write_To_Stream (
      Stream : not null access Root_Stream_Type'Class;
      Item : Buffer_Type)
   is
      NC_Item : Non_Controlled_Buffer_Type
         renames Controlled.Reference (Item).all;
      Stream_Storage_All : Stream_Element_Array (1 .. NC_Item.Last);
      for Stream_Storage_All'Address use NC_Item.Data.Storage;
   begin
      Write (Stream.all, Stream_Storage_All);
   end Write_To_Stream;

   overriding procedure Read (
      Stream : in out Stream_Type;
      Item : out Stream_Element_Array;
      Last : out Stream_Element_Offset)
   is
      Length : Stream_Element_Count := Item'Length;
      Rest : constant Stream_Element_Count :=
         Stream.Buffer.Last - Stream.Buffer.Index + 1;
   begin
      if Length > Rest then
         Length := Rest;
         if Length = 0 and then Item'First = Stream_Element_Offset'First then
            raise Constraint_Error; -- AARM 13.13.1(11/2)
         end if;
      end if;
      Last := Item'First + (Length - 1);
      declare
         Stream_Storage_All : Stream_Element_Array (1 .. Stream.Buffer.Last);
         for Stream_Storage_All'Address use Stream.Buffer.Data.Storage;
      begin
         Item (Item'First .. Last) :=
            Stream_Storage_All (
               Stream.Buffer.Index ..
               Stream.Buffer.Index + Length - 1);
      end;
      Stream.Buffer.Index := Stream.Buffer.Index + Length;
   end Read;

   overriding procedure Write (
      Stream : in out Stream_Type;
      Item : Stream_Element_Array)
   is
      New_Index : constant Stream_Element_Count :=
         Stream.Buffer.Index + Item'Length;
      Copy_Last : constant Stream_Element_Offset := New_Index - 1;
      New_Last : constant Stream_Element_Offset :=
         Stream_Element_Offset'Max (Stream.Buffer.Last, Copy_Last);
   begin
      Set_Size (Stream.Buffer.all, New_Last);
      if Stream.Buffer.Index <= Stream.Buffer.Last then -- overwriting
         Unique (Stream.Buffer.all);
      end if;
      declare
         Stream_Storage_All : Stream_Element_Array (1 .. New_Last);
         for Stream_Storage_All'Address use Stream.Buffer.Data.Storage;
      begin
         Stream_Storage_All (Stream.Buffer.Index .. Copy_Last) := Item;
      end;
      Stream.Buffer.Index := New_Index;
      Stream.Buffer.Last := New_Last;
   end Write;

   overriding procedure Set_Index (
      Stream : in out Stream_Type;
      To : Stream_Element_Positive_Count) is
   begin
      if To > Stream.Buffer.Last + 1 then
         raise Constraint_Error;
      end if;
      Stream.Buffer.Index := To;
   end Set_Index;

   overriding function Index (Stream : Stream_Type)
      return Stream_Element_Positive_Count is
   begin
      return Stream.Buffer.Index;
   end Index;

   overriding function Size (Stream : Stream_Type)
      return Stream_Element_Count is
   begin
      return Stream.Buffer.Last;
   end Size;

   package body Controlled is

      procedure Clear (Data : aliased in out Non_Controlled_Buffer_Type);
      procedure Clear (Data : aliased in out Non_Controlled_Buffer_Type) is
      begin
         System.Reference_Counting.Clear (
            Upcast (Data.Data'Unchecked_Access),
            Free => Free_Data'Access);
      end Clear;

      --  implementation

      function Reference (Object : Unbounded_Storage_IO.Buffer_Type)
         return not null access Non_Controlled_Buffer_Type is
      begin
         return Buffer_Type (Object).Data'Unrestricted_Access;
      end Reference;

      overriding procedure Adjust (Object : in out Buffer_Type) is
      begin
         Object.Data.Stream := null;
         System.Reference_Counting.Adjust (
            Upcast (Object.Data.Data'Unchecked_Access));
      end Adjust;

      overriding procedure Finalize (Object : in out Buffer_Type) is
      begin
         Clear (Object.Data);
         Free (Object.Data.Stream);
      end Finalize;

      package body Streaming is

         procedure Read (
            Stream : not null access Root_Stream_Type'Class;
            Item : out Buffer_Type)
         is
            Size : Stream_Element_Offset;
         begin
            Stream_Element_Offset'Read (Stream, Size);
            Item.Data.Last := 0;
            Item.Data.Index := 1;
            Set_Size (Item.Data, Size);
            if Size > 0 then
               declare
                  Stream_Storage_All : Stream_Element_Array (
                     1 .. Item.Data.Last);
                  for Stream_Storage_All'Address use Item.Data.Data.Storage;
                  Last : Stream_Element_Offset;
               begin
                  Read (Stream.all, Stream_Storage_All, Last);
                  if Last < Stream_Storage_All'Last then
                     Raise_Exception (End_Error'Identity);
                  end if;
               end;
            end if;
         end Read;

         procedure Write (
            Stream : not null access Root_Stream_Type'Class;
            Item : Buffer_Type) is
         begin
            Stream_Element_Offset'Write (Stream, Item.Data.Last);
            Write_To_Stream (Stream, Unbounded_Storage_IO.Buffer_Type (Item));
         end Write;

      end Streaming;

   end Controlled;

end Ada.Streams.Unbounded_Storage_IO;
