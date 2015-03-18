with Ada.Exception_Identification.From_Here;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with System.Address_To_Named_Access_Conversions;
with System.Standard_Allocators.Allocated_Size;
package body Ada.Streams.Unbounded_Storage_IO is
   use Exception_Identification.From_Here;
   use type System.Storage_Elements.Storage_Offset;

   procedure Free is
      new Unchecked_Deallocation (Stream_Type, Stream_Access);

   package Data_Cast is
      new System.Address_To_Named_Access_Conversions (Data, Data_Access);

   subtype Not_Null_Data_Access is not null Data_Access;

   function Upcast is
      new Unchecked_Conversion (
         Not_Null_Data_Access,
         System.Reference_Counting.Container);
   function Downcast is
      new Unchecked_Conversion (
         System.Reference_Counting.Container,
         Not_Null_Data_Access);

   type Data_Access_Access is access all Not_Null_Data_Access;
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
      M : constant System.Address := Data_Cast.To_Address (Data);
      Usable_Size : constant System.Storage_Elements.Storage_Count :=
         System.Standard_Allocators.Allocated_Size (M) - Header_Size;
      Allocated_Capacity : Stream_Element_Count;
   begin
      if Dividable then -- optimized for packed
         Allocated_Capacity := Stream_Element_Offset (
            Usable_Size
            / (Stream_Element_Array'Component_Size
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
      return not null Data_Access is
   begin
      if Capacity = 0 then
         return Empty_Data'Unrestricted_Access;
      else
         declare
            M : constant System.Address :=
               System.Standard_Allocators.Allocate (
                  Allocation_Size (Capacity));
            Result : constant not null Data_Access := Data_Cast.To_Pointer (M);
         begin
            Result.Reference_Count := 1;
            Result.Max_Length := Max_Length;
            Adjust_Allocated (Result);
            return Result;
         end;
      end if;
   end Allocate_Data;

   procedure Free_Data (Data : in out System.Reference_Counting.Data_Access);
   procedure Free_Data (Data : in out System.Reference_Counting.Data_Access) is
   begin
      System.Standard_Allocators.Free (Data_Cast.To_Address (Downcast (Data)));
      Data := null;
   end Free_Data;

   procedure Reallocate_Data (
      Data : aliased in out System.Reference_Counting.Data_Access;
      Length : System.Reference_Counting.Length_Type;
      Max_Length : System.Reference_Counting.Length_Type;
      Capacity : System.Reference_Counting.Length_Type);
   procedure Reallocate_Data (
      Data : aliased in out System.Reference_Counting.Data_Access;
      Length : System.Reference_Counting.Length_Type;
      Max_Length : System.Reference_Counting.Length_Type;
      Capacity : System.Reference_Counting.Length_Type)
   is
      pragma Unreferenced (Length);
      M : constant System.Address :=
         System.Standard_Allocators.Reallocate (
            Data_Cast.To_Address (Downcast (Data)),
            Allocation_Size (Capacity));
   begin
      Data := Upcast (Data_Cast.To_Pointer (M));
      Downcast (Data).Max_Length := Max_Length;
      Adjust_Allocated (Downcast (Data));
   end Reallocate_Data;

   procedure Copy_Data (
      Target : out System.Reference_Counting.Data_Access;
      Source : not null System.Reference_Counting.Data_Access;
      Length : System.Reference_Counting.Length_Type;
      Max_Length : System.Reference_Counting.Length_Type;
      Capacity : System.Reference_Counting.Length_Type);
   procedure Copy_Data (
      Target : out System.Reference_Counting.Data_Access;
      Source : not null System.Reference_Counting.Data_Access;
      Length : System.Reference_Counting.Length_Type;
      Max_Length : System.Reference_Counting.Length_Type;
      Capacity : System.Reference_Counting.Length_Type)
   is
      Data : constant not null Data_Access :=
         Allocate_Data (Max_Length, Capacity);
      Source_Item : Stream_Element_Array (1 .. Stream_Element_Offset (Length));
      for Source_Item'Address use Downcast (Source).Storage;
      Target_Item : Stream_Element_Array (1 .. Stream_Element_Offset (Length));
      for Target_Item'Address use Data.Storage;
   begin
      Target_Item := Source_Item;
      Target := Upcast (Data);
   end Copy_Data;

   procedure Reallocate (
      Stream : in out Stream_Type;
      Size : Stream_Element_Count);
   procedure Reallocate (
      Stream : in out Stream_Type;
      Size : Stream_Element_Count) is
   begin
      System.Reference_Counting.Unique (
         Target => Upcast (Stream.Data'Unchecked_Access),
         Target_Length => System.Reference_Counting.Length_Type (Stream.Last),
         Target_Capacity => System.Reference_Counting.Length_Type (
            Stream.Data.Capacity),
         Max_Length => System.Reference_Counting.Length_Type (Stream.Last),
         Capacity => System.Reference_Counting.Length_Type (Size),
         Sentinel => Upcast (Empty_Data'Unrestricted_Access),
         Reallocate => Reallocate_Data'Access,
         Copy => Copy_Data'Access,
         Free => Free_Data'Access);
   end Reallocate;

   procedure Unique (Stream : in out Stream_Type);
   procedure Unique (Stream : in out Stream_Type) is
   begin
      Reallocate (Stream, Stream.Data.Capacity);
   end Unique;

   procedure Set_Size (
      Stream : in out Stream_Type;
      Size : Stream_Element_Count);
   procedure Set_Size (
      Stream : in out Stream_Type;
      Size : Stream_Element_Count) is
   begin
      System.Reference_Counting.Set_Length (
         Target => Upcast (Stream.Data'Unchecked_Access),
         Target_Length => System.Reference_Counting.Length_Type (Stream.Last),
         Target_Max_Length => Stream.Data.Max_Length,
         Target_Capacity => System.Reference_Counting.Length_Type (
            Stream.Data.Capacity),
         New_Length => System.Reference_Counting.Length_Type (Size),
         Sentinel => Upcast (Empty_Data'Unrestricted_Access),
         Reallocate => Reallocate_Data'Access,
         Copy => Copy_Data'Access,
         Free => Free_Data'Access);
      Stream.Last := Size;
   end Set_Size;

   --  implementation

   procedure Reset (Object : in out Buffer_Type) is
      S : constant not null Stream_Access := Stream (Object);
   begin
      S.Index := 1;
   end Reset;

   function Size (Object : Buffer_Type) return Stream_Element_Count is
      S : constant not null Stream_Access := Stream (Object);
   begin
      return S.Last;
   end Size;

   procedure Set_Size (
      Object : in out Buffer_Type;
      Size : Stream_Element_Count)
   is
      S : constant not null Stream_Access := Stream (Object);
   begin
      Set_Size (S.all, Size);
      if S.Index > Size + 1 then
         S.Index := Size + 1;
      end if;
   end Set_Size;

   function Capacity (Object : Buffer_Type) return Stream_Element_Count is
      S : constant not null Stream_Access := Stream (Object);
   begin
      return S.Data.Capacity;
   end Capacity;

   procedure Reserve_Capacity (
      Object : in out Buffer_Type;
      Capacity : Stream_Element_Count)
   is
      S : constant not null Stream_Access := Stream (Object);
      New_Capacity : constant Stream_Element_Count :=
         Stream_Element_Offset'Max (Capacity, S.Last);
   begin
      Reallocate (S.all, New_Capacity);
   end Reserve_Capacity;

   function Address (Object : aliased in out Buffer_Type)
      return System.Address
   is
      S : constant not null Stream_Access := Stream (Object);
   begin
      Unique (S.all);
      return S.Data.Storage;
   end Address;

   function Size (Object : Buffer_Type)
      return System.Storage_Elements.Storage_Count
   is
      S : constant not null Stream_Access := Stream (Object);
   begin
      return System.Storage_Elements.Storage_Count (S.Last);
   end Size;

   function Stream (Object : Buffer_Type)
      return not null access Root_Stream_Type'Class
   is
      S : constant not null Stream_Access := Stream (Object);
   begin
      return S;
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
         Raise_Exception (End_Error'Identity);
      end if;
      if Length > Rest then
         Length := Rest;
      end if;
      Last := Item'First + Length - 1;
      declare
         Stream_Item : Stream_Element_Array (1 .. Stream.Last);
         for Stream_Item'Address use Stream.Data.Storage;
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
      if Stream.Index > Stream.Last then
         Set_Size (Stream, New_Last);
      elsif New_Last > Stream.Data.Capacity then
         Reallocate (
            Stream,
            Stream_Element_Count'Max (
               New_Last,
               Stream.Data.Capacity * 2));
      else
         Unique (Stream);
      end if;
      declare
         Stream_Item : Stream_Element_Array (1 .. New_Last);
         for Stream_Item'Address use Stream.Data.Storage;
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

   package body Controlled is

      function Stream (Object : Buffer_Type) return not null Stream_Access is
      begin
         return Object.Stream;
      end Stream;

      overriding procedure Initialize (Object : in out Buffer_Type) is
      begin
         Object.Stream := new Stream_Type'(
            Data => Empty_Data'Unrestricted_Access,
            Last => 0,
            Index => 1);
      end Initialize;

      overriding procedure Finalize (Object : in out Buffer_Type) is
      begin
         if Object.Stream /= null then
            System.Reference_Counting.Clear (
               Upcast (Object.Stream.Data'Access),
               Free => Free_Data'Access);
            Free (Object.Stream);
         end if;
      end Finalize;

      overriding procedure Adjust (Object : in out Buffer_Type) is
         Old_Stream : constant Stream_Access := Object.Stream;
      begin
         Object.Stream := new Stream_Type'(
            Data => Old_Stream.Data,
            Last => Old_Stream.Last,
            Index => Old_Stream.Index);
         System.Reference_Counting.Adjust (Upcast (Object.Stream.Data'Access));
      end Adjust;

      package body Streaming is

         procedure Write (
            Stream : not null access Root_Stream_Type'Class;
            Item : Buffer_Type)
         is
            Stream_Item : Stream_Element_Array (1 .. Item.Stream.Last);
            for Stream_Item'Address use Item.Stream.Data.Storage;
         begin
            Streams.Write (Stream.all, Stream_Item);
         end Write;

      end Streaming;

   end Controlled;

end Ada.Streams.Unbounded_Storage_IO;
