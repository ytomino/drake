package body Ada.Strings.Generic_Unbounded is
   use type Streams.Stream_Element_Offset;
   use type Interfaces.Integer_32;

   procedure Free is new Unchecked_Deallocation (Data, Data_Access);

   procedure Release (Data : in out Data_Access);
   procedure Release (Data : in out Data_Access) is
   begin
      if Data /= Empty_Data'Unrestricted_Access then
         if Interfaces.sync_sub_and_fetch (
            Data.Reference_Count'Access,
            1) = 0
         then
            Free (Data);
         end if;
      end if;
   end Release;

   procedure Reserve_Capacity (
      Item : in out Unbounded_String;
      Capacity : Integer);
   procedure Reserve_Capacity (
      Item : in out Unbounded_String;
      Capacity : Integer) is
   begin
      if Capacity /= Item.Data.Capacity
         or else (Item.Data /= Empty_Data'Unrestricted_Access
            and then Item.Data.Reference_Count > 1)
      then
         declare
            New_Capacity : constant Natural := Integer'Max (
               Capacity,
               Item.Length);
            Old_Data : Data_Access := Item.Data;
         begin
            if New_Capacity = 0 then
               Item.Data := Empty_Data'Unrestricted_Access;
            else
               Item.Data := new Data'(
                  Capacity => New_Capacity,
                  Reference_Count => 1,
                  Max_Length => Interfaces.Integer_32 (Item.Length),
                  Items => <>);
               declare
                  subtype R is Integer range 1 .. Item.Length;
               begin
                  Item.Data.Items (R) := Old_Data.Items (R);
               end;
            end if;
            Release (Old_Data);
         end;
      end if;
   end Reserve_Capacity;

   procedure Unique (Item : in out Unbounded_String);
   procedure Unique (Item : in out Unbounded_String) is
   begin
      Reserve_Capacity (Item, Item.Data.Capacity);
   end Unique;

   procedure Set_Length (Item : in out Unbounded_String; Length : Natural);
   procedure Set_Length (Item : in out Unbounded_String; Length : Natural) is
   begin
      if Length > Item.Length then
         declare
            Old_Capacity : constant Natural := Item.Data.Capacity;
         begin
            if Length > Old_Capacity then
               declare
                  New_Capacity : constant Natural :=
                     Integer'Max (Old_Capacity * 2, Length);
               begin
                  Reserve_Capacity (Item, New_Capacity);
                  Item.Data.Max_Length := Interfaces.Integer_32 (Length);
               end;
            else
               if Interfaces.sync_bool_compare_and_swap (
                  Item.Data.Max_Length'Access,
                  Interfaces.Integer_32 (Item.Length),
                  Interfaces.Integer_32 (Length))
               then
                  null;
               elsif Item.Data.Reference_Count > 1 then
                  Reserve_Capacity (Item, Old_Capacity);
                  Item.Data.Max_Length := Interfaces.Integer_32 (Length);
               end if;
            end if;
         end;
      end if;
      Item.Length := Length;
   end Set_Length;

   procedure Assign (
      Target : in out Unbounded_String;
      Source : Unbounded_String);
   procedure Assign (
      Target : in out Unbounded_String;
      Source : Unbounded_String) is
   begin
      Target.Length := Source.Length;
      if Target.Data /= Source.Data then
         Target.Data := Source.Data;
         Adjust (Target);
      end if;
   end Assign;

   overriding procedure Adjust (Object : in out Unbounded_String) is
   begin
      if Object.Data /= Empty_Data'Unrestricted_Access then
         Interfaces.sync_add_and_fetch (Object.Data.Reference_Count'Access, 1);
      end if;
   end Adjust;

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

   function Constant_Reference (
      Source : not null access constant Unbounded_String)
      return Slicing.Constant_Reference_Type is
   begin
      return Constant_Reference (Source, 1, Source.Length);
   end Constant_Reference;

   function Constant_Reference (
      Source : not null access constant Unbounded_String;
      First_Index : Positive;
      Last_Index : Natural)
      return Slicing.Constant_Reference_Type is
   begin
      return Slicing.Constant_Slice (
         Source.Data.Items'Unrestricted_Access,
         First_Index,
         Last_Index);
   end Constant_Reference;

   function Element (Source : Unbounded_String; Index : Positive)
      return Character_Type is
   begin
      return Source.Data.Items (Index);
   end Element;

   overriding procedure Finalize (Object : in out Unbounded_String) is
   begin
      Release (Object.Data);
      Object.Data := Empty_Data'Unrestricted_Access;
      Object.Length := 0;
   end Finalize;

   function Length (Source : Unbounded_String) return Natural is
   begin
      return Source.Length;
   end Length;

   function Null_Unbounded_String return Unbounded_String is
   begin
      return (Finalization.Controlled with
         Data => Empty_Data'Unrestricted_Access,
         Length => 0);
   end Null_Unbounded_String;

   function Reference (Source : not null access Unbounded_String)
      return Slicing.Reference_Type is
   begin
      return Reference (Source, 1, Source.Length);
   end Reference;

   function Reference (
      Source : not null access Unbounded_String;
      First_Index : Positive;
      Last_Index : Natural)
      return Slicing.Reference_Type is
   begin
      Unique (Source.all);
      return Slicing.Slice (
         Source.Data.Items'Unrestricted_Access,
         First_Index,
         Last_Index);
   end Reference;

   procedure Replace_Element (
      Source : in out Unbounded_String;
      Index : Positive;
      By : Character_Type) is
   begin
      Unique (Source);
      Source.Data.Items (Index) := By;
   end Replace_Element;

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

   function Slice (
      Source : Unbounded_String;
      Low : Positive;
      High : Natural)
      return String_Type is
   begin
      return Source.Data.Items (Low .. High);
   end Slice;

   function To_String (Source : Unbounded_String) return String_Type is
   begin
      return Source.Data.Items (1 .. Source.Length);
   end To_String;

   function To_Unbounded_String (Source : String_Type)
      return Unbounded_String
   is
      Length : constant Natural := Source'Length;
      New_Data : Data_Access;
   begin
      if Length = 0 then
         New_Data := Empty_Data'Unrestricted_Access;
      else
         New_Data := new Data'(
            Capacity => Length,
            Reference_Count => 1,
            Max_Length => Interfaces.Integer_32 (Length),
            Items => Source);
      end if;
      return (Finalization.Controlled with Data => New_Data, Length => Length);
   end To_Unbounded_String;

   function To_Unbounded_String (Length : Natural)
      return Unbounded_String
   is
      New_Data : Data_Access;
   begin
      if Length = 0 then
         New_Data := Empty_Data'Unrestricted_Access;
      else
         New_Data := new Data'(
            Capacity => Length,
            Reference_Count => 1,
            Max_Length => Interfaces.Integer_32 (Length),
            Items => <>);
      end if;
      return (Finalization.Controlled with Data => New_Data, Length => Length);
   end To_Unbounded_String;

   function Unbounded_Slice (
      Source : Unbounded_String;
      Low : Positive;
      High : Natural)
      return Unbounded_String
   is
      pragma Suppress (All_Checks);
   begin
      return To_Unbounded_String (Source.Data.Items (Low .. High));
   end Unbounded_Slice;

   procedure Unbounded_Slice (
      Source : Unbounded_String;
      Target : out Unbounded_String;
      Low : Positive;
      High : Natural)
   is
      pragma Suppress (All_Checks);
      Source_Data : constant Data_Access := Source.Data;
      Target_Data : Data_Access := Target.Data;
   begin
      Target.Data := Empty_Data'Unrestricted_Access;
      Set_Unbounded_String (Target, Source_Data.Items (Low .. High));
      Release (Target_Data);
   end Unbounded_Slice;

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

   function "=" (Left, Right : Unbounded_String) return Boolean is
      pragma Suppress (All_Checks);
   begin
      return Left.Data.Items (1 .. Left.Length) =
         Right.Data.Items (1 .. Right.Length);
   end "=";

   function "=" (Left : Unbounded_String; Right : String_Type)
      return Boolean
   is
      pragma Suppress (All_Checks);
   begin
      return Left.Data.Items (1 .. Left.Length) = Right;
   end "=";

   function "=" (Left : String_Type; Right : Unbounded_String)
      return Boolean
   is
      pragma Suppress (All_Checks);
   begin
      return Left = Right.Data.Items (1 .. Right.Length);
   end "=";

   function "<" (Left, Right : Unbounded_String) return Boolean is
      pragma Suppress (All_Checks);
   begin
      return Left.Data.Items (1 .. Left.Length) <
         Right.Data.Items (1 .. Right.Length);
   end "<";

   function "<" (Left : Unbounded_String; Right : String_Type)
      return Boolean
   is
      pragma Suppress (All_Checks);
   begin
      return Left.Data.Items (1 .. Left.Length) < Right;
   end "<";

   function "<" (Left : String_Type; Right : Unbounded_String)
      return Boolean
   is
      pragma Suppress (All_Checks);
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

   package body Generic_Functions is

      function Index (
         Source : Unbounded_String;
         Pattern : String_Type;
         From : Positive;
         Going : Direction := Forward)
         return Natural is
      begin
         return Fixed_Index_From (
            Constant_Reference (Source'Access).Element.all,
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
            Constant_Reference (Source'Access).Element.all,
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
            Constant_Reference (Source'Access).Element.all,
            From,
            Going);
      end Index_Non_Blank;

      function Index_Non_Blank (
         Source : Unbounded_String;
         Going : Direction := Forward)
         return Natural is
      begin
         return Fixed_Index_Non_Blank (
            Constant_Reference (Source'Access).Element.all,
            Going);
      end Index_Non_Blank;

      function Count (
         Source : Unbounded_String;
         Pattern : String_Type)
         return Natural is
      begin
         return Fixed_Count (
            Constant_Reference (Source'Access).Element.all,
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
               Constant_Reference (Source'Access).Element.all,
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
               Constant_Reference (Source'Access).Element.all,
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
               Constant_Reference (Source'Access).Element.all,
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
               Constant_Reference (Source'Access).Element.all,
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
               Constant_Reference (Source'Access).Element.all,
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
               Constant_Reference (Source'Access).Element.all,
               Position,
               New_Item));
      end Overwrite;

      function Delete (
         Source : Unbounded_String;
         From : Positive;
         Through : Natural)
         return Unbounded_String is
      begin
         return To_Unbounded_String (
            Fixed_Delete (
               Constant_Reference (Source'Access).Element.all,
               From,
               Through));
      end Delete;

      procedure Delete (
         Source : in out Unbounded_String;
         From : Positive;
         Through : Natural) is
      begin
         Set_Unbounded_String (
            Source,
            Fixed_Delete (
               Constant_Reference (Source'Access).Element.all,
               From,
               Through));
      end Delete;

      function Trim (
         Source : Unbounded_String;
         Side : Trim_End;
         Left : Character_Type := Space;
         Right : Character_Type := Space)
         return Unbounded_String
      is
         S : String_Type renames
            Constant_Reference (Source'Access).Element.all;
         First : Positive;
         Last : Natural;
      begin
         Fixed_Trim (S, Side, Left, Right, First, Last);
         return To_Unbounded_String (S (First .. Last));
      end Trim;

      procedure Trim (
         Source : in out Unbounded_String;
         Side : Trim_End;
         Left : Character_Type := Space;
         Right : Character_Type := Space)
      is
         First : Positive;
         Last : Natural;
      begin
         Fixed_Trim (
            Constant_Reference (Source'Access).Element.all,
            Side,
            Left,
            Right,
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
               Constant_Reference (Source'Access).Element.all,
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
               Constant_Reference (Source'Access).Element.all,
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
               Constant_Reference (Source'Access).Element.all,
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
               Constant_Reference (Source'Access).Element.all,
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
         return Left * Constant_Reference (Right'Access).Element.all;
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
               Constant_Reference (Source'Access).Element.all,
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
               Constant_Reference (Source'Access).Element.all,
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
               Constant_Reference (Source'Access).Element.all,
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
               Constant_Reference (Source'Access).Element.all,
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
               Constant_Reference (Source'Access).Element.all,
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
               Constant_Reference (Source'Access).Element.all,
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
               Constant_Reference (Source'Access).Element.all,
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
               Constant_Reference (Source'Access).Element.all,
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
               Constant_Reference (Source'Access).Element.all,
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
               Constant_Reference (Source'Access).Element.all,
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
               Constant_Reference (Source'Access).Element.all,
               Pattern,
               Mapping);
         end Count_Per_Element;

         function Count (Source : Unbounded_String; Set : Character_Set)
            return Natural is
         begin
            return Fixed_Count_Set (
               Constant_Reference (Source'Access).Element.all,
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
               Constant_Reference (Source'Access).Element.all,
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
               Constant_Reference (Source'Access).Element.all,
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
                  Constant_Reference (Source'Access).Element.all,
                  Mapping));
         end Translate;

         procedure Translate (
            Source : in out Unbounded_String;
            Mapping : Character_Mapping) is
         begin
            Set_Unbounded_String (
               Source,
               Fixed_Translate_Mapping (
                  Constant_Reference (Source'Access).Element.all,
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
                  Constant_Reference (Source'Access).Element.all,
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
                  Constant_Reference (Source'Access).Element.all,
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
                  Constant_Reference (Source'Access).Element.all,
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
                  Constant_Reference (Source'Access).Element.all,
                  Mapping));
         end Translate_Per_Element;

         function Trim (
            Source : Unbounded_String;
            Left : Character_Set;
            Right : Character_Set)
            return Unbounded_String
         is
            S : String_Type renames
               Constant_Reference (Source'Access).Element.all;
            First : Positive;
            Last : Natural;
         begin
            Fixed_Trim_Set (S, Left, Right, First, Last);
            return To_Unbounded_String (S (First .. Last));
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
               Constant_Reference (Source'Access).Element.all,
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

   package body No_Primitives is

      procedure Read (
         Stream : not null access Streams.Root_Stream_Type'Class;
         Item : out Unbounded_String)
      is
         pragma Suppress (All_Checks);
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
         pragma Suppress (All_Checks);
      begin
         Integer'Write (Stream, 1);
         Integer'Write (Stream, Item.Length);
         Write (Stream, Item.Data.Items (1 .. Item.Length));
      end Write;

   end No_Primitives;

end Ada.Strings.Generic_Unbounded;
