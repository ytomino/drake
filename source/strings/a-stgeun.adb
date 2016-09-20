with Ada.Unchecked_Conversion;
with System.Address_To_Named_Access_Conversions;
with System.Standard_Allocators;
with System.Storage_Elements;
with System.System_Allocators.Allocated_Size;
package body Ada.Strings.Generic_Unbounded is
   use type Streams.Stream_Element_Offset;
   use type System.Address;
   use type System.Storage_Elements.Storage_Offset;

   package FSA_Conv is
      new System.Address_To_Named_Access_Conversions (
         Fixed_String,
         Fixed_String_Access);

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

   function Allocation_Size (
      Capacity : System.Reference_Counting.Length_Type)
      return System.Storage_Elements.Storage_Count;
   function Allocation_Size (
      Capacity : System.Reference_Counting.Length_Type)
      return System.Storage_Elements.Storage_Count
   is
      Header_Size : constant System.Storage_Elements.Storage_Count :=
         Data'Size / Standard'Storage_Unit;
      Item_Size : System.Storage_Elements.Storage_Count;
   begin
      if String_Type'Component_Size rem Standard'Storage_Unit = 0 then
         --  optimized for packed
         Item_Size :=
            Capacity * (String_Type'Component_Size / Standard'Storage_Unit);
      else
         --  unpacked
         Item_Size :=
            (Capacity * String_Type'Component_Size
               + (Standard'Storage_Unit - 1))
            / Standard'Storage_Unit;
      end if;
      return Header_Size + Item_Size;
   end Allocation_Size;

   procedure Adjust_Allocated (Data : not null Data_Access);
   procedure Adjust_Allocated (Data : not null Data_Access) is
      Header_Size : constant System.Storage_Elements.Storage_Count :=
         Generic_Unbounded.Data'Size / Standard'Storage_Unit;
      M : constant System.Address := DA_Conv.To_Address (Data);
      Usable_Size : constant System.Storage_Elements.Storage_Count :=
         System.System_Allocators.Allocated_Size (M) - Header_Size;
   begin
      if String_Type'Component_Size
         rem Standard'Storage_Unit = 0
      then -- optimized for packed
         Data.Capacity :=
            Integer (
               Usable_Size
                  / (String_Type'Component_Size / Standard'Storage_Unit));
      else -- unpacked
         Data.Capacity :=
            Integer (
               Usable_Size
                  * Standard'Storage_Unit
                  / String_Type'Component_Size);
      end if;
      Data.Items := FSA_Conv.To_Pointer (M + Header_Size);
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
      subtype R is Integer range 1 .. Integer (Length);
   begin
      declare
         pragma Suppress (Access_Check);
      begin
         Data.Items (R) := Downcast (Source).Items (R);
      end;
      Target := Upcast (Data);
   end Copy_Data;

   procedure Reallocate (
      Source : in out Unbounded_String;
      Length : Natural;
      Capacity : Natural);
   procedure Reallocate (
      Source : in out Unbounded_String;
      Length : Natural;
      Capacity : Natural) is
   begin
      System.Reference_Counting.Unique (
         Target => Upcast (Source.Data'Unchecked_Access),
         Target_Length =>
            System.Reference_Counting.Length_Type (Source.Length),
         Target_Capacity =>
            System.Reference_Counting.Length_Type (
               Generic_Unbounded.Capacity (Source)),
         New_Length => System.Reference_Counting.Length_Type (Length),
         New_Capacity => System.Reference_Counting.Length_Type (Capacity),
         Sentinel => Upcast (Empty_Data'Unrestricted_Access),
         Reallocate => Reallocate_Data'Access,
         Copy => Copy_Data'Access,
         Free => Free_Data'Access);
   end Reallocate;

   function Create (Data : not null Data_Access; Length : Natural)
      return Unbounded_String;
   function Create (Data : not null Data_Access; Length : Natural)
      return Unbounded_String is
   begin
      return (Finalization.Controlled with Data => Data, Length => Length);
   end Create;

   --  implementation

   function Null_Unbounded_String return Unbounded_String is
   begin
      return Create (Data => Empty_Data'Unrestricted_Access, Length => 0);
   end Null_Unbounded_String;

   function Is_Null (Source : Unbounded_String) return Boolean is
   begin
      return Source.Length = 0;
   end Is_Null;

   function Length (Source : Unbounded_String) return Natural is
   begin
      return Source.Length;
   end Length;

   procedure Set_Length (Source : in out Unbounded_String; Length : Natural) is
      pragma Suppress (Access_Check); -- dereferencing Source.Data
      Old_Capacity : constant Natural := Capacity (Source);
      Failure : Boolean;
   begin
      System.Reference_Counting.In_Place_Set_Length (
         Target_Data => Upcast (Source.Data),
         Target_Length =>
            System.Reference_Counting.Length_Type (Source.Length),
         Target_Max_Length => Source.Data.Max_Length,
         Target_Capacity =>
            System.Reference_Counting.Length_Type (Old_Capacity),
         New_Length => System.Reference_Counting.Length_Type (Length),
         Failure => Failure);
      if Failure then
         declare
            New_Capacity : Natural;
         begin
            if Old_Capacity >= Length then
               --  Old_Capacity is possibly a large value by Generic_Constant
               New_Capacity := Length; -- shrinking
            else
               New_Capacity := Integer'Max (Old_Capacity * 2, Length);
            end if;
            Reallocate (Source, Length, New_Capacity);
         end;
      end if;
      Source.Length := Length;
   end Set_Length;

   function Capacity (Source : Unbounded_String) return Natural is
      pragma Suppress (Access_Check);
   begin
      return Source.Data.Capacity;
   end Capacity;

   procedure Reserve_Capacity (
      Source : in out Unbounded_String;
      Capacity : Natural)
   is
      New_Capacity : constant Natural := Integer'Max (Capacity, Source.Length);
   begin
      Reallocate (Source, Source.Length, New_Capacity);
   end Reserve_Capacity;

   function To_Unbounded_String (Source : String_Type)
      return Unbounded_String
   is
      Length : constant Natural := Source'Length;
      New_Data : constant not null Data_Access :=
         Allocate_Data (
            System.Reference_Counting.Length_Type (Length),
            System.Reference_Counting.Length_Type (Length));
   begin
      declare
         pragma Suppress (Access_Check);
      begin
         New_Data.Items (1 .. Length) := Source;
      end;
      return Create (Data => New_Data, Length => Length);
   end To_Unbounded_String;

   function To_Unbounded_String (Length : Natural)
      return Unbounded_String
   is
      New_Data : constant not null Data_Access :=
         Allocate_Data (
            System.Reference_Counting.Length_Type (Length),
            System.Reference_Counting.Length_Type (Length));
   begin
      return Create (Data => New_Data, Length => Length);
   end To_Unbounded_String;

   function To_String (Source : Unbounded_String) return String_Type is
      pragma Suppress (Access_Check);
   begin
      return Source.Data.Items (1 .. Source.Length);
   end To_String;

   procedure Set_Unbounded_String (
      Target : out Unbounded_String;
      Source : String_Type)
   is
      pragma Suppress (Access_Check);
      Length : constant Natural := Source'Length;
   begin
      Target.Length := 0;
      Set_Length (Target, Length);
      Target.Data.Items (1 .. Length) := Source;
   end Set_Unbounded_String;

   procedure Append (
      Source : in out Unbounded_String;
      New_Item : Unbounded_String)
   is
      pragma Suppress (Access_Check);
      New_Item_Length : constant Natural := New_Item.Length;
      Old_Length : constant Natural := Source.Length;
   begin
      if Old_Length = 0 and then Capacity (Source) < New_Item_Length then
         Assign (Source, New_Item);
      else
         declare
            Total_Length : constant Natural := Old_Length + New_Item_Length;
         begin
            Set_Length (Source, Total_Length);
            Source.Data.Items (Old_Length + 1 .. Total_Length) :=
               New_Item.Data.Items (1 .. New_Item_Length);
            --  Do not use New_Item.Length in here for Append (X, X).
         end;
      end if;
   end Append;

   procedure Append (
      Source : in out Unbounded_String;
      New_Item : String_Type)
   is
      pragma Suppress (Access_Check);
      Old_Length : constant Natural := Source.Length;
      Total_Length : constant Natural := Old_Length + New_Item'Length;
   begin
      Set_Length (Source, Total_Length);
      Source.Data.Items (Old_Length + 1 .. Total_Length) := New_Item;
   end Append;

   procedure Append_Element (
      Source : in out Unbounded_String;
      New_Item : Character_Type)
   is
      pragma Suppress (Access_Check);
      Old_Length : constant Natural := Source.Length;
      Total_Length : constant Natural := Old_Length + 1;
   begin
      Set_Length (Source, Total_Length);
      Source.Data.Items (Total_Length) := New_Item;
   end Append_Element;

   function "&" (Left, Right : Unbounded_String) return Unbounded_String is
   begin
      return Result : Unbounded_String := Left do
         Append (Result, Right);
      end return;
   end "&";

   function "&" (Left : Unbounded_String; Right : String_Type)
      return Unbounded_String is
   begin
      return Result : Unbounded_String := Left do
         Append (Result, Right);
      end return;
   end "&";

   function "&" (Left : String_Type; Right : Unbounded_String)
      return Unbounded_String is
   begin
      return Result : Unbounded_String do
         if Left'Length > 0 then
            Reallocate (Result, 0, Left'Length + Right.Length);
            Append (Result, Left);
         end if;
         Append (Result, Right);
      end return;
   end "&";

   function "&" (Left : Unbounded_String; Right : Character_Type)
      return Unbounded_String is
   begin
      return Result : Unbounded_String := Left do
         Append_Element (Result, Right);
      end return;
   end "&";

   function "&" (Left : Character_Type; Right : Unbounded_String)
      return Unbounded_String is
   begin
      return Result : Unbounded_String do
         Reallocate (Result, 0, 1 + Right.Length);
         Append_Element (Result, Left);
         Append (Result, Right);
      end return;
   end "&";

   function Element (Source : Unbounded_String; Index : Positive)
      return Character_Type
   is
      pragma Check (Pre, Index <= Source.Length or else raise Index_Error);
      pragma Suppress (Access_Check);
   begin
      return Source.Data.Items (Index);
   end Element;

   procedure Replace_Element (
      Source : in out Unbounded_String;
      Index : Positive;
      By : Character_Type)
   is
      pragma Check (Pre, Index <= Source.Length or else raise Index_Error);
      pragma Suppress (Access_Check);
   begin
      Unique (Source);
      Source.Data.Items (Index) := By;
   end Replace_Element;

   function Slice (
      Source : Unbounded_String;
      Low : Positive;
      High : Natural)
      return String_Type
   is
      pragma Check (Pre,
         Check =>
            (Low <= Source.Length + 1 and then High <= Source.Length)
            or else raise Index_Error);
      pragma Suppress (Access_Check);
   begin
      return Source.Data.Items (Low .. High);
   end Slice;

   function Unbounded_Slice (
      Source : Unbounded_String;
      Low : Positive;
      High : Natural)
      return Unbounded_String is
   begin
      return Result : Unbounded_String do
         Unbounded_Slice (Source, Result, Low, High); -- checking Index_Error
      end return;
   end Unbounded_Slice;

   procedure Unbounded_Slice (
      Source : Unbounded_String;
      Target : out Unbounded_String;
      Low : Positive;
      High : Natural)
   is
      pragma Check (Pre,
         Check =>
            (Low <= Source.Length + 1 and then High <= Source.Length)
            or else raise Index_Error);
      pragma Suppress (Access_Check);
   begin
      if Low = 1 then
         Assign (Target, Source);
         Set_Length (Target, High);
      else
         Set_Unbounded_String (Target, Source.Data.Items (Low .. High));
      end if;
   end Unbounded_Slice;

   overriding function "=" (Left, Right : Unbounded_String) return Boolean is
      pragma Suppress (Access_Check);
   begin
      return Left.Data.Items (1 .. Left.Length) =
         Right.Data.Items (1 .. Right.Length);
   end "=";

   function "=" (Left : Unbounded_String; Right : String_Type)
      return Boolean
   is
      pragma Suppress (Access_Check);
   begin
      return Left.Data.Items (1 .. Left.Length) = Right;
   end "=";

   function "=" (Left : String_Type; Right : Unbounded_String)
      return Boolean
   is
      pragma Suppress (Access_Check);
   begin
      return Left = Right.Data.Items (1 .. Right.Length);
   end "=";

   function "<" (Left, Right : Unbounded_String) return Boolean is
      pragma Suppress (Access_Check);
   begin
      return Left.Data.Items (1 .. Left.Length) <
         Right.Data.Items (1 .. Right.Length);
   end "<";

   function "<" (Left : Unbounded_String; Right : String_Type)
      return Boolean
   is
      pragma Suppress (Access_Check);
   begin
      return Left.Data.Items (1 .. Left.Length) < Right;
   end "<";

   function "<" (Left : String_Type; Right : Unbounded_String)
      return Boolean
   is
      pragma Suppress (Access_Check);
   begin
      return Left < Right.Data.Items (1 .. Right.Length);
   end "<";

   function "<=" (Left, Right : Unbounded_String) return Boolean is
   begin
      return not (Right < Left);
   end "<=";

   function "<=" (Left : Unbounded_String; Right : String_Type)
      return Boolean is
   begin
      return not (Right < Left);
   end "<=";

   function "<=" (Left : String_Type; Right : Unbounded_String)
      return Boolean is
   begin
      return not (Right < Left);
   end "<=";

   function ">" (Left, Right : Unbounded_String) return Boolean is
   begin
      return Right < Left;
   end ">";

   function ">" (Left : Unbounded_String; Right : String_Type)
      return Boolean is
   begin
      return Right < Left;
   end ">";

   function ">" (Left : String_Type; Right : Unbounded_String)
      return Boolean is
   begin
      return Right < Left;
   end ">";

   function ">=" (Left, Right : Unbounded_String) return Boolean is
   begin
      return not (Left < Right);
   end ">=";

   function ">=" (Left : Unbounded_String; Right : String_Type)
      return Boolean is
   begin
      return not (Left < Right);
   end ">=";

   function ">=" (Left : String_Type; Right : Unbounded_String)
      return Boolean is
   begin
      return not (Left < Right);
   end ">=";

   procedure Assign (
      Target : in out Unbounded_String;
      Source : Unbounded_String) is
   begin
      System.Reference_Counting.Assign (
         Upcast (Target.Data'Unchecked_Access),
         Upcast (Source.Data'Unrestricted_Access),
         Free => Free_Data'Access);
      Target.Length := Source.Length;
   end Assign;

   procedure Move (
      Target : in out Unbounded_String;
      Source : in out Unbounded_String) is
   begin
      System.Reference_Counting.Move (
         Upcast (Target.Data'Unchecked_Access),
         Upcast (Source.Data'Unrestricted_Access),
         Sentinel => Upcast (Empty_Data'Unrestricted_Access),
         Free => Free_Data'Access);
      Target.Length := Source.Length;
      Source.Length := 0;
   end Move;

   function Constant_Reference (
      Source : aliased Unbounded_String)
      return Slicing.Constant_Reference_Type
   is
      pragma Suppress (Access_Check);
   begin
      return Slicing.Constant_Slice (
         String_Access'(Source.Data.Items.all'Unrestricted_Access).all,
         1,
         Source.Length);
   end Constant_Reference;

   function Reference (
      Source : aliased in out Unbounded_String)
      return Slicing.Reference_Type
   is
      pragma Suppress (Access_Check);
   begin
      Unique (Source);
      return Slicing.Slice (
         String_Access'(Source.Data.Items.all'Unrestricted_Access).all,
         1,
         Source.Length);
   end Reference;

   procedure Unique (Source : in out Unbounded_String'Class) is
   begin
      if System.Reference_Counting.Shared (Upcast (Source.Data)) then
         Reallocate (
            Unbounded_String (Source),
            Source.Length,
            Source.Length); -- shrinking
      end if;
   end Unique;

   procedure Unique_And_Set_Length (
      Source : in out Unbounded_String'Class;
      Length : Natural) is
   begin
      if System.Reference_Counting.Shared (Upcast (Source.Data)) then
         Reallocate (Unbounded_String (Source), Length, Length); -- shrinking
      else
         Set_Length (Unbounded_String (Source), Length);
      end if;
   end Unique_And_Set_Length;

   overriding procedure Adjust (Object : in out Unbounded_String) is
   begin
      System.Reference_Counting.Adjust (Upcast (Object.Data'Unchecked_Access));
   end Adjust;

   overriding procedure Finalize (Object : in out Unbounded_String) is
   begin
      System.Reference_Counting.Clear (
         Upcast (Object.Data'Unchecked_Access),
         Free => Free_Data'Access);
      Object.Data := Empty_Data'Unrestricted_Access;
      Object.Length := 0;
   end Finalize;

   package body Generic_Constant is

      S_Data : aliased constant Data := (
         Reference_Count => System.Reference_Counting.Static,
         Capacity => Integer'Last,
         Max_Length => System.Reference_Counting.Length_Type (Integer'Last),
         Items => S.all'Unrestricted_Access);

      function Value return Unbounded_String is
      begin
         return Create (
            Data => S_Data'Unrestricted_Access,
            Length => S'Length);
      end Value;

   end Generic_Constant;

   package body Streaming is

      procedure Read (
         Stream : not null access Streams.Root_Stream_Type'Class;
         Item : out Unbounded_String)
      is
         pragma Suppress (Access_Check);
         First : Integer;
         Last : Integer;
      begin
         Integer'Read (Stream, First);
         Integer'Read (Stream, Last);
         declare
            Length : constant Integer := Last - First + 1;
         begin
            Item.Length := 0;
            Set_Length (Item, Length);
            Read (Stream, Item.Data.Items (1 .. Length));
         end;
      end Read;

      procedure Write (
         Stream : not null access Streams.Root_Stream_Type'Class;
         Item : Unbounded_String)
      is
         pragma Suppress (Access_Check);
      begin
         Integer'Write (Stream, 1);
         Integer'Write (Stream, Item.Length);
         Write (Stream, Item.Data.Items (1 .. Item.Length));
      end Write;

   end Streaming;

end Ada.Strings.Generic_Unbounded;
