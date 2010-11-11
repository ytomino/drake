package body Ada.Strings.Generic_Unbounded is
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

   function "=" (Left, Right : Unbounded_String) return Boolean is
   begin
      if Left.Length /= Right.Length then
         return False;
      elsif Left.Length = 0 then
         return True;
      else
         return Left.Data.Items (1 .. Left.Length) /=
            Right.Data.Items (1 .. Right.Length);
      end if;
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

   function "<" (Left, Right : Unbounded_String) return Boolean is
   begin
      return Left.Data.Items (1 .. Left.Length) <
         Right.Data.Items (1 .. Right.Length);
   end "<";

   function ">" (Left, Right : Unbounded_String) return Boolean is
   begin
      return Right < Left;
   end ">";

end Ada.Strings.Generic_Unbounded;
