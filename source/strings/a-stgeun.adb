with Ada.Unchecked_Conversion;
with System.Address_To_Named_Access_Conversions;
with System.Standard_Allocators.Allocated_Size;
with System.Storage_Elements;
with System.Strings.Stream_Ops;
package body Ada.Strings.Generic_Unbounded is
   use type Streams.Stream_Element_Offset;
   use type System.Address;
   use type System.Storage_Elements.Storage_Offset;

   package Data_Cast is
      new System.Address_To_Named_Access_Conversions (Data, Data_Access);

   subtype Not_Null_Data_Access is not null Data_Access;
   type Data_Access_Access is access all Not_Null_Data_Access;
   type System_Address_Access is access all System.Address;
   function Upcast is
      new Unchecked_Conversion (Data_Access_Access, System_Address_Access);

   function Allocate_Data (Max_Length, Capacity : Natural)
      return not null Data_Access;
   function Allocate_Data (Max_Length, Capacity : Natural)
      return not null Data_Access is
   begin
      if Capacity = 0 then
         return Empty_Data'Unrestricted_Access;
      else
         declare
            Header_Size : constant System.Storage_Elements.Storage_Count :=
               Data'Size / Standard'Storage_Unit
               + Integer'Size / Standard'Storage_Unit * 2; -- constraints
            Required_Size : constant System.Storage_Elements.Storage_Count :=
               Header_Size
               + Character_Type'Size / Standard'Storage_Unit
                  * System.Storage_Elements.Storage_Count (Capacity);
            M : constant System.Address :=
               System.Standard_Allocators.Allocate (Required_Size);
         begin
            return Result : not null Data_Access := Data_Cast.To_Pointer (M) do
               Result.Reference_Count := 1;
               Result.Max_Length := Max_Length;
               declare
                  type Repr is record
                     Data : System.Address;
                     Constraints : System.Address;
                  end record;
                  pragma Suppress_Initialization (Repr);
                  R : Repr;
                  for R'Address use Result.Items'Address;
                  First : Integer;
                  for First'Address use
                     M + Data'Size / Standard'Storage_Unit;
                  Last : Integer;
                  for Last'Address use
                     First'Address + Integer'Size / Standard'Storage_Unit;
                  Data : Character_Type;
                  for Data'Address use
                     Last'Address + Integer'Size / Standard'Storage_Unit;
               begin
                  First := 1;
                  Last := Integer (
                     (System.Standard_Allocators.Allocated_Size (M)
                        - Header_Size)
                        / (Character_Type'Size / Standard'Storage_Unit));
                  R.Constraints := First'Address;
                  R.Data := Data'Address;
               end;
            end return;
         end;
      end if;
   end Allocate_Data;

   procedure Free_Data (Data : System.Address);
   procedure Free_Data (Data : System.Address) is
   begin
      System.Standard_Allocators.Free (Data);
   end Free_Data;

   procedure Copy_Data (
      Target : out System.Address;
      Source : System.Address;
      Length : Natural;
      Max_Length : Natural;
      Capacity : Natural);
   procedure Copy_Data (
      Target : out System.Address;
      Source : System.Address;
      Length : Natural;
      Max_Length : Natural;
      Capacity : Natural)
   is
      Data : not null Data_Access := Allocate_Data (Max_Length, Capacity);
      subtype R is Integer range 1 .. Length;
   begin
      Data.Items (R) := Data_Cast.To_Pointer (Source).Items (R);
      Target := Data_Cast.To_Address (Data);
   end Copy_Data;

   procedure Reserve_Capacity (
      Item : in out Unbounded_String;
      Capacity : Natural);
   procedure Reserve_Capacity (
      Item : in out Unbounded_String;
      Capacity : Natural) is
   begin
      System.Reference_Counting.Unique (
         Target => Upcast (Item.Data'Unchecked_Access),
         Target_Reference_Count => Item.Data.Reference_Count'Access,
         Target_Length => Item.Length,
         Target_Capacity => Generic_Unbounded.Capacity (Item),
         Max_Length => Item.Length,
         Capacity => Capacity,
         Sentinel => Empty_Data'Address,
         Copy => Copy_Data'Access,
         Free => Free_Data'Access);
   end Reserve_Capacity;

   procedure Unique (Item : in out Unbounded_String);
   procedure Unique (Item : in out Unbounded_String) is
   begin
      Reserve_Capacity (Item, Capacity (Item));
   end Unique;

   procedure Assign (
      Target : in out Unbounded_String;
      Source : Unbounded_String);
   procedure Assign (
      Target : in out Unbounded_String;
      Source : Unbounded_String) is
   begin
      System.Reference_Counting.Assign (
         Upcast (Target.Data'Unchecked_Access),
         Target.Data.Reference_Count'Access,
         Upcast (Source.Data'Unrestricted_Access),
         Source.Data.Reference_Count'Access,
         Free => Free_Data'Access);
      Target.Length := Source.Length;
   end Assign;

   --  implementation

   function Null_Unbounded_String return Unbounded_String is
   begin
      return (Finalization.Controlled with
         Data => Empty_Data'Unrestricted_Access,
         Length => 0);
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
   begin
      System.Reference_Counting.Set_Length (
         Target => Upcast (Source.Data'Unchecked_Access),
         Target_Reference_Count => Source.Data.Reference_Count'Access,
         Target_Length => Source.Length,
         Target_Max_Length => Source.Data.Max_Length'Access,
         Target_Capacity => Capacity (Source),
         New_Length => Length,
         Sentinel => Empty_Data'Address,
         Copy => Copy_Data'Access,
         Free => Free_Data'Access);
      Source.Length := Length;
   end Set_Length;

   function Capacity (Source : Unbounded_String'Class) return Natural is
   begin
      return Source.Data.Items'Last;
   end Capacity;

   function To_Unbounded_String (Source : String_Type)
      return Unbounded_String
   is
      Length : constant Natural := Source'Length;
      New_Data : constant not null Data_Access :=
         Allocate_Data (Length, Length);
   begin
      New_Data.Items (1 .. Length) := Source;
      return (Finalization.Controlled with Data => New_Data, Length => Length);
   end To_Unbounded_String;

   function To_Unbounded_String (Length : Natural)
      return Unbounded_String
   is
      New_Data : constant not null Data_Access :=
         Allocate_Data (Length, Length);
   begin
      return (Finalization.Controlled with Data => New_Data, Length => Length);
   end To_Unbounded_String;

   function To_String (Source : Unbounded_String) return String_Type is
   begin
      return Source.Data.Items (1 .. Source.Length);
   end To_String;

   procedure Set_Unbounded_String (
      Target : out Unbounded_String;
      Source : String_Type)
   is
      Length : constant Natural := Source'Length;
   begin
      Target.Length := 0;
      Set_Length (Target, Length);
      Target.Data.Items (1 .. Length) := Source;
   end Set_Unbounded_String;

   procedure Append (
      Source : in out Unbounded_String;
      New_Item : Unbounded_String) is
   begin
      if Source.Length = 0 then
         Assign (Source, New_Item);
      else
         Append (Source, New_Item.Data.Items (1 .. New_Item.Length));
      end if;
   end Append;

   procedure Append (
      Source : in out Unbounded_String;
      New_Item : String_Type)
   is
      Last : constant Natural := Source.Length;
      Total_Length : constant Natural := Last + New_Item'Length;
   begin
      Set_Length (Source, Total_Length);
      Source.Data.Items (Last + 1 .. Total_Length) := New_Item;
   end Append;

   procedure Append (
      Source : in out Unbounded_String;
      New_Item : Character_Type)
   is
      Last : constant Natural := Source.Length;
      Total_Length : constant Natural := Last + 1;
   begin
      Set_Length (Source, Total_Length);
      Source.Data.Items (Total_Length) := New_Item;
   end Append;

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
         Reserve_Capacity (Result, Left'Length + Right.Length);
         Append (Result, Left);
         Append (Result, Right);
      end return;
   end "&";

   function "&" (Left : Unbounded_String; Right : Character_Type)
      return Unbounded_String is
   begin
      return Result : Unbounded_String := Left do
         Append (Result, Right);
      end return;
   end "&";

   function "&" (Left : Character_Type; Right : Unbounded_String)
      return Unbounded_String is
   begin
      return Result : Unbounded_String do
         Reserve_Capacity (Result, 1 + Right.Length);
         Append (Result, Left);
         Append (Result, Right);
      end return;
   end "&";

   function Element (Source : Unbounded_String; Index : Positive)
      return Character_Type is
   begin
      return Source.Data.Items (Index);
   end Element;

   procedure Replace_Element (
      Source : in out Unbounded_String;
      Index : Positive;
      By : Character_Type) is
   begin
      Unique (Source);
      Source.Data.Items (Index) := By;
   end Replace_Element;

   function Slice (
      Source : Unbounded_String;
      Low : Positive;
      High : Natural)
      return String_Type is
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
         Unbounded_Slice (Source, Result, Low, High);
      end return;
   end Unbounded_Slice;

   procedure Unbounded_Slice (
      Source : Unbounded_String;
      Target : out Unbounded_String;
      Low : Positive;
      High : Natural) is
   begin
      if Low = 1 then
         Target := Source;
         Set_Length (Target, High);
      else
         Set_Unbounded_String (Target, Source.Data.Items (Low .. High));
      end if;
   end Unbounded_Slice;

   function "=" (Left, Right : Unbounded_String) return Boolean is
   begin
      return Left.Data.Items (1 .. Left.Length) =
         Right.Data.Items (1 .. Right.Length);
   end "=";

   function "=" (Left : Unbounded_String; Right : String_Type)
      return Boolean is
   begin
      return Left.Data.Items (1 .. Left.Length) = Right;
   end "=";

   function "=" (Left : String_Type; Right : Unbounded_String)
      return Boolean is
   begin
      return Left = Right.Data.Items (1 .. Right.Length);
   end "=";

   function "<" (Left, Right : Unbounded_String) return Boolean is
   begin
      return Left.Data.Items (1 .. Left.Length) <
         Right.Data.Items (1 .. Right.Length);
   end "<";

   function "<" (Left : Unbounded_String; Right : String_Type)
      return Boolean is
   begin
      return Left.Data.Items (1 .. Left.Length) < Right;
   end "<";

   function "<" (Left : String_Type; Right : Unbounded_String)
      return Boolean is
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

   function Constant_Reference (
      Source : aliased Unbounded_String)
      return Slicing.Constant_Reference_Type is
   begin
      return Constant_Reference (Source, 1, Source.Length);
   end Constant_Reference;

   function Constant_Reference (
      Source : aliased Unbounded_String;
      First_Index : Positive;
      Last_Index : Natural)
      return Slicing.Constant_Reference_Type is
   begin
      return Slicing.Constant_Slice (
         Source.Data.Items,
         First_Index,
         Last_Index);
   end Constant_Reference;

   function Reference (
      Source : aliased in out Unbounded_String)
      return Slicing.Reference_Type is
   begin
      return Reference (Source, 1, Source.Length);
   end Reference;

   function Reference (
      Source : aliased in out Unbounded_String;
      First_Index : Positive;
      Last_Index : Natural)
      return Slicing.Reference_Type is
   begin
      Unique (Source);
      return Slicing.Slice (
         Source.Data.Items,
         First_Index,
         Last_Index);
   end Reference;

   overriding procedure Adjust (Object : in out Unbounded_String) is
   begin
      System.Reference_Counting.Adjust (Object.Data.Reference_Count'Access);
   end Adjust;

   overriding procedure Finalize (Object : in out Unbounded_String) is
   begin
      System.Reference_Counting.Clear (
         Upcast (Object.Data'Unchecked_Access),
         Object.Data.Reference_Count'Access,
         Free => Free_Data'Access);
      Object.Data := Empty_Data'Unrestricted_Access;
      Object.Length := 0;
   end Finalize;

   package body Generic_Functions is

      function Index (
         Source : Unbounded_String;
         Pattern : String_Type;
         From : Positive;
         Going : Direction := Forward)
         return Natural is
      begin
         return Fixed_Index_From (
            Source.Data.Items (1 .. Source.Length),
            Pattern,
            From,
            Going);
      end Index;

      function Index (
         Source : Unbounded_String;
         Pattern : String_Type;
         Going : Direction := Forward)
         return Natural is
      begin
         return Fixed_Index (
            Source.Data.Items (1 .. Source.Length),
            Pattern,
            Going);
      end Index;

      function Index_Non_Blank (
         Source : Unbounded_String;
         From : Positive;
         Going : Direction := Forward)
         return Natural is
      begin
         return Fixed_Index_Non_Blank_From (
            Source.Data.Items (1 .. Source.Length),
            From,
            Going);
      end Index_Non_Blank;

      function Index_Non_Blank (
         Source : Unbounded_String;
         Going : Direction := Forward)
         return Natural is
      begin
         return Fixed_Index_Non_Blank (
            Source.Data.Items (1 .. Source.Length),
            Going);
      end Index_Non_Blank;

      function Count (
         Source : Unbounded_String;
         Pattern : String_Type)
         return Natural is
      begin
         return Fixed_Count (
            Source.Data.Items (1 .. Source.Length),
            Pattern);
      end Count;

      function Replace_Slice (
         Source : Unbounded_String;
         Low : Positive;
         High : Natural;
         By : String_Type)
         return Unbounded_String is
      begin
         return To_Unbounded_String (
            Fixed_Replace_Slice (
               Source.Data.Items (1 .. Source.Length),
               Low,
               High,
               By));
      end Replace_Slice;

      procedure Replace_Slice (
         Source : in out Unbounded_String;
         Low : Positive;
         High : Natural;
         By : String_Type) is
      begin
         Set_Unbounded_String (
            Source,
            Fixed_Replace_Slice (
               Source.Data.Items (1 .. Source.Length),
               Low,
               High,
               By));
      end Replace_Slice;

      function Insert (
         Source : Unbounded_String;
         Before : Positive;
         New_Item : String_Type)
         return Unbounded_String is
      begin
         return To_Unbounded_String (
            Fixed_Insert (
               Source.Data.Items (1 .. Source.Length),
               Before,
               New_Item));
      end Insert;

      procedure Insert (
         Source : in out Unbounded_String;
         Before : Positive;
         New_Item : String_Type) is
      begin
         Set_Unbounded_String (
            Source,
            Fixed_Insert (
               Source.Data.Items (1 .. Source.Length),
               Before,
               New_Item));
      end Insert;

      function Overwrite (
         Source : Unbounded_String;
         Position : Positive;
         New_Item : String_Type)
         return Unbounded_String is
      begin
         return To_Unbounded_String (
            Fixed_Overwrite (
               Source.Data.Items (1 .. Source.Length),
               Position,
               New_Item));
      end Overwrite;

      procedure Overwrite (
         Source : in out Unbounded_String;
         Position : Positive;
         New_Item : String_Type) is
      begin
         Set_Unbounded_String (
            Source,
            Fixed_Overwrite (
               Source.Data.Items (1 .. Source.Length),
               Position,
               New_Item));
      end Overwrite;

      function Delete (
         Source : Unbounded_String;
         From : Positive;
         Through : Natural)
         return Unbounded_String is
      begin
         return Result : Unbounded_String := Source do
            Delete (Result, From, Through);
         end return;
      end Delete;

      procedure Delete (
         Source : in out Unbounded_String;
         From : Positive;
         Through : Natural) is
      begin
         if From <= Through then
            declare
               Old_Length : constant Natural := Source.Length;
               New_Length : Natural;
            begin
               if Through >= Old_Length then
                  New_Length := From - 1;
               else
                  New_Length := Old_Length;
                  Unique (Source); -- for overwriting
                  Fixed_Delete (
                     Source.Data.Items (1 .. Old_Length),
                     New_Length,
                     From,
                     Through);
               end if;
               Set_Length (Source, New_Length);
            end;
         end if;
      end Delete;

      function Trim (
         Source : Unbounded_String;
         Side : Trim_End;
         Blank : Character_Type := Space)
         return Unbounded_String
      is
         First : Positive;
         Last : Natural;
      begin
         Fixed_Trim (
            Source.Data.Items (1 .. Source.Length),
            Side,
            Blank,
            First,
            Last);
         return Unbounded_Slice (Source, First, Last);
      end Trim;

      procedure Trim (
         Source : in out Unbounded_String;
         Side : Trim_End;
         Blank : Character_Type := Space)
      is
         First : Positive;
         Last : Natural;
      begin
         Fixed_Trim (
            Source.Data.Items (1 .. Source.Length),
            Side,
            Blank,
            First,
            Last);
         Unbounded_Slice (Source, Source, First, Last);
      end Trim;

      function Head (
         Source : Unbounded_String;
         Count : Natural;
         Pad : Character_Type := Space)
         return Unbounded_String is
      begin
         return To_Unbounded_String (
            Fixed_Head (
               Source.Data.Items (1 .. Source.Length),
               Count,
               Pad));
      end Head;

      procedure Head (
         Source : in out Unbounded_String;
         Count : Natural;
         Pad : Character_Type := Space) is
      begin
         Set_Unbounded_String (
            Source,
            Fixed_Head (
               Source.Data.Items (1 .. Source.Length),
               Count,
               Pad));
      end Head;

      function Tail (
         Source : Unbounded_String;
         Count : Natural;
         Pad : Character_Type := Space)
         return Unbounded_String is
      begin
         return To_Unbounded_String (
            Fixed_Tail (
               Source.Data.Items (1 .. Source.Length),
               Count,
               Pad));
      end Tail;

      procedure Tail (
         Source : in out Unbounded_String;
         Count : Natural;
         Pad : Character_Type := Space) is
      begin
         Set_Unbounded_String (
            Source,
            Fixed_Tail (
               Source.Data.Items (1 .. Source.Length),
               Count,
               Pad));
      end Tail;

      function "*" (Left : Natural; Right : Character_Type)
         return Unbounded_String is
      begin
         return Result : Unbounded_String := To_Unbounded_String (Left) do
            for I in 1 .. Left loop
               Result.Data.Items (I) := Right;
            end loop;
         end return;
      end "*";

      function "*" (Left : Natural; Right : String_Type)
         return Unbounded_String is
      begin
         return Result : Unbounded_String :=
            To_Unbounded_String (Left * Right'Length)
         do
            declare
               First : Positive := 1;
            begin
               for I in 1 .. Left loop
                  Result.Data.Items (First .. First + Right'Length - 1) :=
                     Right;
                  First := First + Right'Length;
               end loop;
            end;
         end return;
      end "*";

      function "*" (Left : Natural; Right : Unbounded_String)
         return Unbounded_String is
      begin
         return Left * Right.Data.Items (1 .. Right.Length);
      end "*";

      package body Generic_Maps is

         function Index (
            Source : Unbounded_String;
            Pattern : String_Type;
            From : Positive;
            Going : Direction := Forward;
            Mapping : Character_Mapping)
            return Natural is
         begin
            return Fixed_Index_Mapping_From (
               Source.Data.Items (1 .. Source.Length),
               Pattern,
               From,
               Going,
               Mapping);
         end Index;

         function Index (
            Source : Unbounded_String;
            Pattern : String_Type;
            Going : Direction := Forward;
            Mapping : Character_Mapping)
            return Natural is
         begin
            return Fixed_Index_Mapping (
               Source.Data.Items (1 .. Source.Length),
               Pattern,
               Going,
               Mapping);
         end Index;

         function Index (
            Source : Unbounded_String;
            Pattern : String_Type;
            From : Positive;
            Going : Direction := Forward;
            Mapping : not null access function (From : Wide_Wide_Character)
               return Wide_Wide_Character)
            return Natural is
         begin
            return Fixed_Index_Mapping_Function_From (
               Source.Data.Items (1 .. Source.Length),
               Pattern,
               From,
               Going,
               Mapping);
         end Index;

         function Index (
            Source : Unbounded_String;
            Pattern : String_Type;
            Going : Direction := Forward;
            Mapping : not null access function (From : Wide_Wide_Character)
               return Wide_Wide_Character)
            return Natural is
         begin
            return Fixed_Index_Mapping_Function (
               Source.Data.Items (1 .. Source.Length),
               Pattern,
               Going,
               Mapping);
         end Index;

         function Index_Per_Element (
            Source : Unbounded_String;
            Pattern : String_Type;
            From : Positive;
            Going : Direction := Forward;
            Mapping : not null access function (From : Character_Type)
               return Character_Type)
            return Natural is
         begin
            return Fixed_Index_Mapping_Function_Per_Element_From (
               Source.Data.Items (1 .. Source.Length),
               Pattern,
               From,
               Going,
               Mapping);
         end Index_Per_Element;

         function Index_Per_Element (
            Source : Unbounded_String;
            Pattern : String_Type;
            Going : Direction := Forward;
            Mapping : not null access function (From : Character_Type)
               return Character_Type)
            return Natural is
         begin
            return Fixed_Index_Mapping_Function_Per_Element (
               Source.Data.Items (1 .. Source.Length),
               Pattern,
               Going,
               Mapping);
         end Index_Per_Element;

         function Index (
            Source : Unbounded_String;
            Set : Character_Set;
            From : Positive;
            Test : Membership := Inside;
            Going : Direction := Forward)
            return Natural is
         begin
            return Fixed_Index_Set_From (
               Source.Data.Items (1 .. Source.Length),
               Set,
               From,
               Test,
               Going);
         end Index;

         function Index (
            Source : Unbounded_String;
            Set : Character_Set;
            Test : Membership := Inside;
            Going : Direction := Forward)
            return Natural is
         begin
            return Fixed_Index_Set (
               Source.Data.Items (1 .. Source.Length),
               Set,
               Test,
               Going);
         end Index;

         function Count (
            Source : Unbounded_String;
            Pattern : String_Type;
            Mapping : Character_Mapping)
            return Natural is
         begin
            return Fixed_Count_Mapping (
               Source.Data.Items (1 .. Source.Length),
               Pattern,
               Mapping);
         end Count;

         function Count (
            Source : Unbounded_String;
            Pattern : String_Type;
            Mapping : not null access function (From : Wide_Wide_Character)
               return Wide_Wide_Character)
            return Natural is
         begin
            return Fixed_Count_Mapping_Function (
               Source.Data.Items (1 .. Source.Length),
               Pattern,
               Mapping);
         end Count;

         function Count_Per_Element (
            Source : Unbounded_String;
            Pattern : String_Type;
            Mapping : not null access function (From : Character_Type)
               return Character_Type)
            return Natural is
         begin
            return Fixed_Count_Mapping_Function_Per_Element (
               Source.Data.Items (1 .. Source.Length),
               Pattern,
               Mapping);
         end Count_Per_Element;

         function Count (Source : Unbounded_String; Set : Character_Set)
            return Natural is
         begin
            return Fixed_Count_Set (
               Source.Data.Items (1 .. Source.Length),
               Set);
         end Count;

         procedure Find_Token (
            Source : Unbounded_String;
            Set : Character_Set;
            From : Positive;
            Test : Membership;
            First : out Positive;
            Last : out Natural) is
         begin
            Fixed_Find_Token_From (
               Source.Data.Items (1 .. Source.Length),
               Set,
               From,
               Test,
               First,
               Last);
         end Find_Token;

         procedure Find_Token (
            Source : Unbounded_String;
            Set : Character_Set;
            Test : Membership;
            First : out Positive;
            Last : out Natural) is
         begin
            Fixed_Find_Token (
               Source.Data.Items (1 .. Source.Length),
               Set,
               Test,
               First,
               Last);
         end Find_Token;

         function Translate (
            Source : Unbounded_String;
            Mapping : Character_Mapping)
            return Unbounded_String is
         begin
            return To_Unbounded_String (
               Fixed_Translate_Mapping (
                  Source.Data.Items (1 .. Source.Length),
                  Mapping));
         end Translate;

         procedure Translate (
            Source : in out Unbounded_String;
            Mapping : Character_Mapping) is
         begin
            Set_Unbounded_String (
               Source,
               Fixed_Translate_Mapping (
                  Source.Data.Items (1 .. Source.Length),
                  Mapping));
         end Translate;

         function Translate (
            Source : Unbounded_String;
            Mapping : not null access function (From : Wide_Wide_Character)
               return Wide_Wide_Character)
            return Unbounded_String is
         begin
            return To_Unbounded_String (
               Fixed_Translate_Mapping_Function (
                  Source.Data.Items (1 .. Source.Length),
                  Mapping));
         end Translate;

         procedure Translate (
            Source : in out Unbounded_String;
            Mapping : not null access function (From : Wide_Wide_Character)
               return Wide_Wide_Character) is
         begin
            Set_Unbounded_String (
               Source,
               Fixed_Translate_Mapping_Function (
                  Source.Data.Items (1 .. Source.Length),
                  Mapping));
         end Translate;

         function Translate_Per_Element (
            Source : Unbounded_String;
            Mapping : not null access function (From : Character_Type)
               return Character_Type)
            return Unbounded_String is
         begin
            return To_Unbounded_String (
               Fixed_Translate_Mapping_Function_Per_Element (
                  Source.Data.Items (1 .. Source.Length),
                  Mapping));
         end Translate_Per_Element;

         procedure Translate_Per_Element (
            Source : in out Unbounded_String;
            Mapping : not null access function (From : Character_Type)
               return Character_Type) is
         begin
            Set_Unbounded_String (
               Source,
               Fixed_Translate_Mapping_Function_Per_Element (
                  Source.Data.Items (1 .. Source.Length),
                  Mapping));
         end Translate_Per_Element;

         function Trim (
            Source : Unbounded_String;
            Left : Character_Set;
            Right : Character_Set)
            return Unbounded_String
         is
            First : Positive;
            Last : Natural;
         begin
            Fixed_Trim_Set (
               Source.Data.Items (1 .. Source.Length),
               Left,
               Right,
               First,
               Last);
            return Unbounded_Slice (Source, First, Last);
         end Trim;

         procedure Trim (
            Source : in out Unbounded_String;
            Left : Character_Set;
            Right : Character_Set)
         is
            First : Positive;
            Last : Natural;
         begin
            Fixed_Trim_Set (
               Source.Data.Items (1 .. Source.Length),
               Left,
               Right,
               First,
               Last);
            Unbounded_Slice (Source, Source, First, Last);
         end Trim;

      end Generic_Maps;

   end Generic_Functions;

   function Generic_Hash (Key : Unbounded_String) return Hash_Type is
   begin
      return Fixed_Hash (Key.Data.Items (1 .. Key.Length));
   end Generic_Hash;

   package body Generic_Constant is

      S_Data : aliased constant Data := (
         Reference_Count => System.Reference_Counting.Static,
         Max_Length => Integer'Last,
         Items => S.all'Unrestricted_Access);

      function Value return Unbounded_String is
      begin
         if S'First /= 1 then
            raise Constraint_Error;
         end if;
         return (Finalization.Controlled with
            Data => S_Data'Unrestricted_Access,
            Length => S'Last);
      end Value;

   end Generic_Constant;

   package body Streaming is

      procedure Read (
         Stream : not null access Streams.Root_Stream_Type'Class;
         Item : out Unbounded_String)
      is
         First : Integer;
         Last : Integer;
      begin
         Integer'Read (Stream, First);
         Integer'Read (Stream, Last);
         declare
            package String_Access_Conv is
               new System.Address_To_Named_Access_Conversions (
                  String_Type,
                  String_Access);
            Length : constant Integer := Last - First + 1;
         begin
            Item.Length := 0;
            Set_Length (Item, Length);
            case Character_Type'Size is
               when Character'Size =>
                  declare
                     Item_As_String : String (1 .. Length);
                     for Item_As_String'Address use
                        String_Access_Conv.To_Address (Item.Data.Items);
                  begin
                     System.Strings.Stream_Ops.String_Read_Blk_IO (
                        Stream,
                        Item_As_String);
                  end;
               when Wide_Character'Size =>
                  declare
                     Item_As_WString : Wide_String (1 .. Length);
                     for Item_As_WString'Address use
                        String_Access_Conv.To_Address (Item.Data.Items);
                  begin
                     System.Strings.Stream_Ops.Wide_String_Read_Blk_IO (
                        Stream,
                        Item_As_WString);
                  end;
               when Wide_Wide_Character'Size =>
                  declare
                     Item_As_WWString : Wide_Wide_String (1 .. Length);
                     for Item_As_WWString'Address use
                        String_Access_Conv.To_Address (Item.Data.Items);
                  begin
                     System.Strings.Stream_Ops.Wide_Wide_String_Read_Blk_IO (
                        Stream,
                        Item_As_WWString);
                  end;
               when others =>
                  String_Type'Read (Stream, Item.Data.Items (1 .. Length));
            end case;
         end;
      end Read;

      procedure Write (
         Stream : not null access Streams.Root_Stream_Type'Class;
         Item : Unbounded_String)
      is
         package String_Access_Conv is
            new System.Address_To_Named_Access_Conversions (
               String_Type,
               String_Access);
      begin
         Integer'Write (Stream, 1);
         Integer'Write (Stream, Item.Length);
         case Character_Type'Size is
            when Character'Size =>
               declare
                  Item_As_String : String (1 .. Item.Length);
                  for Item_As_String'Address use
                     String_Access_Conv.To_Address (Item.Data.Items);
               begin
                  System.Strings.Stream_Ops.String_Write_Blk_IO (
                     Stream,
                     Item_As_String);
               end;
            when Wide_Character'Size =>
               declare
                  Item_As_WString : Wide_String (1 .. Item.Length);
                  for Item_As_WString'Address use
                     String_Access_Conv.To_Address (Item.Data.Items);
               begin
                  System.Strings.Stream_Ops.Wide_String_Write_Blk_IO (
                     Stream,
                     Item_As_WString);
               end;
            when Wide_Wide_Character'Size =>
               declare
                  Item_As_WWString : Wide_Wide_String (1 .. Item.Length);
                  for Item_As_WWString'Address use
                     String_Access_Conv.To_Address (Item.Data.Items);
               begin
                  System.Strings.Stream_Ops.Wide_Wide_String_Write_Blk_IO (
                     Stream,
                     Item_As_WWString);
               end;
            when others =>
               String_Type'Write (Stream, Item.Data.Items (1 .. Item.Length));
         end case;
      end Write;

   end Streaming;

end Ada.Strings.Generic_Unbounded;
