package body Ada.Strings.Generic_Bounded is

   function Length (Source : Bounded_String) return Natural is
   begin
      return Source.Length;
   end Length;

   function To_String (Source : Bounded_String) return String_Type is
   begin
      return Source.Element (1 .. Source.Length);
   end To_String;

   procedure Set_Bounded_String (
      Target : out Bounded_String;
      Source : String_Type;
      Drop : Truncation := Error) is
   begin
      if Source'Length > Target.Capacity then
         case Drop is
            when Right =>
               Target.Length := Target.Capacity;
               Target.Element := Source (
                  Source'First ..
                  Source'First + Target.Capacity - 1);
            when Left =>
               Target.Length := Target.Capacity;
               Target.Element := Source (
                  Source'Last - Target.Capacity + 1 ..
                  Source'Last);
            when Error =>
               raise Length_Error;
         end case;
      else
         Target.Length := Source'Length;
         Target.Element (1 .. Target.Length) := Source;
      end if;
   end Set_Bounded_String;

   procedure Append (
      Source : in out Bounded_String;
      New_Item : Bounded_String;
      Drop : Truncation := Error) is
   begin
      Append (Source, New_Item.Element (1 .. New_Item.Length), Drop);
   end Append;

   procedure Append (
      Source : in out Bounded_String;
      New_Item : String_Type;
      Drop : Truncation := Error)
   is
      Old_Length : constant Natural := Source.Length;
      Rest : constant Natural := Source.Capacity - Old_Length;
   begin
      if New_Item'Length > Rest then
         case Drop is
            when Right =>
               Source.Length := Source.Capacity;
               Source.Element (Old_Length + 1 .. Source.Capacity) :=
                  New_Item (New_Item'First .. New_Item'First + Rest - 1);
            when Left =>
               Source.Length := Source.Capacity;
               if New_Item'Length < Source.Capacity then
                  declare
                     Moving : constant Natural :=
                        Source.Capacity - New_Item'Length;
                  begin
                     Source.Element (1 .. Moving) :=
                        Source.Element (Old_Length - Moving + 1 .. Old_Length);
                     Source.Element (Moving + 1 .. Source.Capacity) :=
                        New_Item;
                  end;
               else
                  Source.Element := New_Item (
                     New_Item'Last - Source.Capacity + 1 ..
                     New_Item'Last);
               end if;
            when Error =>
               raise Length_Error;
         end case;
      else
         Source.Length := Old_Length + New_Item'Length;
         Source.Element (Old_Length + 1 .. Source.Length) := New_Item;
      end if;
   end Append;

   procedure Append (
      Source : in out Bounded_String;
      New_Item : Character_Type;
      Drop : Truncation := Error) is
   begin
      Append (Source, String_Type'(1 => New_Item), Drop);
   end Append;

   function Element (
      Source : Bounded_String;
      Index : Positive)
      return Character_Type is
   begin
      return Source.Element (Index);
   end Element;

   procedure Replace_Element (
      Source : in out Bounded_String;
      Index : Positive;
      By : Character_Type) is
   begin
      Source.Element (Index) := By;
   end Replace_Element;

   function Slice (
      Source : Bounded_String;
      Low : Positive;
      High : Natural)
      return String_Type is
   begin
      if Low > Source.Length + 1 or else High > Source.Length then
         raise Index_Error;
      end if;
      return Source.Element (Low .. High);
   end Slice;

   procedure Bounded_Slice (
      Source : Bounded_String;
      Target : out Bounded_String;
      Low : Positive;
      High : Natural) is
   begin
      if Low > Source.Length + 1 or else High > Source.Length then
         raise Index_Error;
      end if;
      Set_Bounded_String (Target, Source.Element (Low .. High));
   end Bounded_Slice;

   function "=" (Left, Right : Bounded_String) return Boolean is
   begin
      return Left.Element (1 .. Left.Length) =
         Right.Element (1 .. Right.Length);
   end "=";

   function "=" (Left : Bounded_String; Right : String_Type) return Boolean is
   begin
      return Left.Element (1 .. Left.Length) = Right;
   end "=";

   function "=" (Left : String_Type; Right : Bounded_String) return Boolean is
   begin
      return Left = Right.Element (1 .. Right.Length);
   end "=";

   function "<" (Left, Right : Bounded_String) return Boolean is
   begin
      return Left.Element (1 .. Left.Length) <
         Right.Element (1 .. Right.Length);
   end "<";

   function "<" (Left : Bounded_String; Right : String_Type) return Boolean is
   begin
      return Left.Element (1 .. Left.Length) < Right;
   end "<";

   function "<" (Left : String_Type; Right : Bounded_String) return Boolean is
   begin
      return Left < Right.Element (1 .. Right.Length);
   end "<";

   function "<=" (Left, Right : Bounded_String) return Boolean is
   begin
      return not (Right < Left);
   end "<=";

   function "<=" (Left : Bounded_String; Right : String_Type) return Boolean is
   begin
      return not (Right < Left);
   end "<=";

   function "<=" (Left : String_Type; Right : Bounded_String) return Boolean is
   begin
      return not (Right < Left);
   end "<=";

   function ">" (Left, Right : Bounded_String) return Boolean is
   begin
      return Right < Left;
   end ">";

   function ">" (Left : Bounded_String; Right : String_Type) return Boolean is
   begin
      return Right < Left;
   end ">";

   function ">" (Left : String_Type; Right : Bounded_String) return Boolean is
   begin
      return Right < Left;
   end ">";

   function ">=" (Left, Right : Bounded_String) return Boolean is
   begin
      return not (Left < Right);
   end ">=";

   function ">=" (Left : Bounded_String; Right : String_Type) return Boolean is
   begin
      return not (Left < Right);
   end ">=";

   function ">=" (Left : String_Type; Right : Bounded_String) return Boolean is
   begin
      return not (Left < Right);
   end ">=";

   function Constant_Reference (
      Source : not null access constant Bounded_String)
      return Slicing.Constant_Reference_Type is
   begin
      return Constant_Reference (Source, 1, Source.Length);
   end Constant_Reference;

   function Constant_Reference (
      Source : not null access constant Bounded_String;
      First_Index : Positive;
      Last_Index : Natural)
      return Slicing.Constant_Reference_Type is
   begin
      return Slicing.Constant_Slice (
         Source.Element'Unrestricted_Access,
         First_Index,
         Last_Index);
   end Constant_Reference;

   function Reference (Source : not null access Bounded_String)
      return Slicing.Reference_Type is
   begin
      return Reference (Source, 1, Source.Length);
   end Reference;

   function Reference (
      Source : not null access Bounded_String;
      First_Index : Positive;
      Last_Index : Natural)
      return Slicing.Reference_Type is
   begin
      return Slicing.Slice (
         Source.Element'Unrestricted_Access,
         First_Index,
         Last_Index);
   end Reference;

   package body Generic_Bounded_Length is

      function Null_Bounded_String return Bounded_String is
      begin
         return (Capacity => Max, Length => 0, Element => <>);
      end Null_Bounded_String;

      function To_Bounded_String (
         Source : String_Type;
         Drop : Truncation := Error)
         return Bounded_String is
      begin
         return Result : Bounded_String do
            Set_Bounded_String (Result, Source, Drop);
         end return;
      end To_Bounded_String;

      function "+" (Source : String_Type) return Bounded_String is
      begin
         return To_Bounded_String (Source);
      end "+";

      function Append (
         Left, Right : Bounded_String;
         Drop : Truncation := Error)
         return Bounded_String is
      begin
         return Result : Bounded_String do
            Set_Bounded_String (Result, Left.Element (1 .. Left.Length), Drop);
            Append (Result, Right, Drop);
         end return;
      end Append;

      function Append (
         Left : Bounded_String;
         Right : String_Type;
         Drop : Truncation := Error)
         return Bounded_String is
      begin
         return Result : Bounded_String do
            Set_Bounded_String (Result, Left.Element (1 .. Left.Length), Drop);
            Append (Result, Right, Drop);
         end return;
      end Append;

      function Append (
         Left : String_Type;
         Right : Bounded_String;
         Drop : Truncation := Error)
         return Bounded_String is
      begin
         return Result : Bounded_String do
            Set_Bounded_String (Result, Left, Drop);
            Append (Result, Right, Drop);
         end return;
      end Append;

      function Append (
         Left : Bounded_String;
         Right : Character_Type;
         Drop : Truncation := Error)
         return Bounded_String is
      begin
         return Result : Bounded_String do
            Set_Bounded_String (Result, Left.Element (1 .. Left.Length), Drop);
            Append (Result, Right, Drop);
         end return;
      end Append;

      function Append (
         Left : Character_Type;
         Right : Bounded_String;
         Drop : Truncation := Error)
         return Bounded_String is
      begin
         return Result : Bounded_String do
            Set_Bounded_String (Result, (1 => Left), Drop);
            Append (Result, Right, Drop);
         end return;
      end Append;

      function "&" (Left, Right : Bounded_String)
         return Bounded_String is
      begin
         return Append (Left, Right);
      end "&";

      function "&" (Left : Bounded_String; Right : String_Type)
         return Bounded_String is
      begin
         return Append (Left, Right);
      end "&";

      function "&" (Left : String_Type; Right : Bounded_String)
         return Bounded_String is
      begin
         return Append (Left, Right);
      end "&";

      function "&" (Left : Bounded_String; Right : Character_Type)
         return Bounded_String is
      begin
         return Append (Left, Right);
      end "&";

      function "&" (Left : Character_Type; Right : Bounded_String)
         return Bounded_String is
      begin
         return Append (Left, Right);
      end "&";

      function Bounded_Slice (
         Source : Bounded_String;
         Low : Positive;
         High : Natural)
         return Bounded_String is
      begin
         return Result : Bounded_String do
            Bounded_Slice (Source, Result, Low, High);
         end return;
      end Bounded_Slice;

      function "*" (Left : Natural; Right : Character_Type)
         return Bounded_String is
      begin
         return Replicate (Left, Right, Error);
      end "*";

      function "*" (Left : Natural; Right : String_Type)
         return Bounded_String is
      begin
         return Replicate (Left, Right, Error);
      end "*";

      function "*" (Left : Natural; Right : Bounded_String)
         return Bounded_String is
      begin
         return Replicate (Left, Right, Error);
      end "*";

      function Replicate (
         Count : Natural;
         Item : Character_Type;
         Drop : Truncation := Error)
         return Bounded_String is
      begin
         if Count > Max and then Drop = Error then
            raise Length_Error;
         else
            return (
               Capacity => Max,
               Length => Natural'Min (Count, Max),
               Element => (others => Item));
         end if;
      end Replicate;

      function Replicate (
         Count : Natural;
         Item : String_Type;
         Drop : Truncation := Error)
         return Bounded_String is
      begin
         if Count * Item'Length > Max and then Drop = Error then
            raise Length_Error;
         else
            return Result : Bounded_String := (
               Capacity => Max,
               Length => Natural'Min (Count * Item'Length, Max),
               Element => <>)
            do
               case Drop is
                  when Right | Error =>
                     declare
                        First : Positive := Result.Element'First;
                     begin
                        for I in 1 .. Count loop
                           if First + Item'Length > Max then
                              Result.Element (First .. Max) :=
                                 Item (Item'First .. Item'First + Max - First);
                              exit;
                           else
                              Result.Element (
                                 First ..
                                 First + Item'Length - 1) := Item;
                              First := First + Item'Length;
                           end if;
                        end loop;
                     end;
                  when Left =>
                     declare
                        Last : Positive := Result.Element'Last;
                     begin
                        for I in 1 .. Count loop
                           if Last <= Item'Length then
                              Result.Element (1 .. Last) :=
                                 Item (Item'Last - Last + 1 .. Item'Last);
                              exit;
                           else
                              Result.Element (
                                 Last - Item'Length + 1 ..
                                 Last) := Item;
                              Last := Last - Item'Length;
                           end if;
                        end loop;
                     end;
               end case;
            end return;
         end if;
      end Replicate;

      function Replicate (
         Count : Natural;
         Item : Bounded_String;
         Drop : Truncation := Error)
         return Bounded_String is
      begin
         return Replicate (Count, Item.Element (1 .. Item.Length), Drop);
      end Replicate;

      package body Generic_Functions is

         function Index (
            Source : Bounded_String;
            Pattern : String_Type;
            From : Positive;
            Going : Direction := Forward)
            return Natural is
         begin
            return Fixed_Index_From (
               Source.Element (1 .. Source.Length),
               Pattern,
               From,
               Going);
         end Index;

         function Index (
            Source : Bounded_String;
            Pattern : String_Type;
            Going : Direction := Forward)
            return Natural is
         begin
            return Fixed_Index (
               Source.Element (1 .. Source.Length),
               Pattern,
               Going);
         end Index;

         function Index_Non_Blank (
            Source : Bounded_String;
            From : Positive;
            Going : Direction := Forward)
            return Natural is
         begin
            return Fixed_Index_Non_Blank_From (
               Source.Element (1 .. Source.Length),
               From,
               Going);
         end Index_Non_Blank;

         function Index_Non_Blank (
            Source : Bounded_String;
            Going : Direction := Forward)
            return Natural is
         begin
            return Fixed_Index_Non_Blank (
               Source.Element (1 .. Source.Length),
               Going);
         end Index_Non_Blank;

         function Count (
            Source : Bounded_String;
            Pattern : String_Type)
            return Natural is
         begin
            return Fixed_Count (
               Source.Element (1 .. Source.Length),
               Pattern);
         end Count;

         function Replace_Slice (
            Source : Bounded_String;
            Low : Positive;
            High : Natural;
            By : String_Type;
            Drop : Truncation := Error)
            return Bounded_String is
         begin
            return To_Bounded_String (
               Fixed_Replace_Slice (
                  Source.Element (1 .. Source.Length),
                  Low,
                  High,
                  By),
               Drop);
         end Replace_Slice;

         procedure Replace_Slice (
            Source : in out Bounded_String;
            Low : Positive;
            High : Natural;
            By : String_Type;
            Drop : Truncation := Error) is
         begin
            Set_Bounded_String (
               Source,
               Fixed_Replace_Slice (
                  Source.Element (1 .. Source.Length),
                  Low,
                  High,
                  By),
               Drop);
         end Replace_Slice;

         function Insert (
            Source : Bounded_String;
            Before : Positive;
            New_Item : String_Type;
            Drop : Truncation := Error)
            return Bounded_String is
         begin
            return To_Bounded_String (
               Fixed_Insert (
                  Source.Element (1 .. Source.Length),
                  Before,
                  New_Item),
               Drop);
         end Insert;

         procedure Insert (
            Source : in out Bounded_String;
            Before : Positive;
            New_Item : String_Type;
            Drop : Truncation := Error) is
         begin
            Set_Bounded_String (
               Source,
               Fixed_Insert (
                  Source.Element (1 .. Source.Length),
                  Before,
                  New_Item),
               Drop);
         end Insert;

         function Overwrite (
            Source : Bounded_String;
            Position : Positive;
            New_Item : String_Type;
            Drop : Truncation := Error)
            return Bounded_String is
         begin
            return To_Bounded_String (
               Fixed_Overwrite (
                  Source.Element (1 .. Source.Length),
                  Position,
                  New_Item),
               Drop);
         end Overwrite;

         procedure Overwrite (
            Source : in out Bounded_String;
            Position : Positive;
            New_Item : String_Type;
            Drop : Truncation := Error) is
         begin
            Set_Bounded_String (
               Source,
               Fixed_Overwrite (
                  Source.Element (1 .. Source.Length),
                  Position,
                  New_Item),
               Drop);
         end Overwrite;

         function Delete (
            Source : Bounded_String;
            From : Positive;
            Through : Natural)
            return Bounded_String is
         begin
            return To_Bounded_String (
               Fixed_Delete (
                  Source.Element (1 .. Source.Length),
                  From,
                  Through),
               Error);
         end Delete;

         procedure Delete (
            Source : in out Bounded_String;
            From : Positive;
            Through : Natural) is
         begin
            Set_Bounded_String (
               Source,
               Fixed_Delete (
                  Source.Element (1 .. Source.Length),
                  From,
                  Through),
               Error);
         end Delete;

         function Trim (
            Source : Bounded_String;
            Side : Trim_End;
            Left : Character_Type := Space;
            Right : Character_Type := Space)
            return Bounded_String
         is
            S : String_Type renames Source.Element (1 .. Source.Length);
            First : Positive;
            Last : Natural;
         begin
            Fixed_Trim (S, Side, Left, Right, First, Last);
            return To_Bounded_String (S (First .. Last), Error);
         end Trim;

         procedure Trim (
            Source : in out Bounded_String;
            Side : Trim_End;
            Left : Character_Type := Space;
            Right : Character_Type := Space)
         is
            S : String_Type renames Source.Element (1 .. Source.Length);
            First : Positive;
            Last : Natural;
         begin
            Fixed_Trim (S, Side, Left, Right, First, Last);
            Set_Bounded_String (Source, S (First .. Last), Error);
         end Trim;

         function Head (
            Source : Bounded_String;
            Count : Natural;
            Pad : Character_Type := Space;
            Drop : Truncation := Error)
            return Bounded_String is
         begin
            return To_Bounded_String (
               Fixed_Head (
                  Source.Element (1 .. Source.Length),
                  Count,
                  Pad),
               Drop);
         end Head;

         procedure Head (
            Source : in out Bounded_String;
            Count : Natural;
            Pad : Character_Type := Space;
            Drop : Truncation := Error) is
         begin
            Set_Bounded_String (
               Source,
               Fixed_Head (
                  Source.Element (1 .. Source.Length),
                  Count,
                  Pad),
               Drop);
         end Head;

         function Tail (
            Source : Bounded_String;
            Count : Natural;
            Pad : Character_Type := Space;
            Drop : Truncation := Error)
            return Bounded_String is
         begin
            return To_Bounded_String (
               Fixed_Tail (
                  Source.Element (1 .. Source.Length),
                  Count,
                  Pad),
               Drop);
         end Tail;

         procedure Tail (
            Source : in out Bounded_String;
            Count : Natural;
            Pad : Character_Type := Space;
            Drop : Truncation := Error) is
         begin
            Set_Bounded_String (
               Source,
               Fixed_Tail (
                  Source.Element (1 .. Source.Length),
                  Count,
                  Pad),
               Drop);
         end Tail;

         package body Generic_Maps is

            function Index (
               Source : Bounded_String;
               Pattern : String_Type;
               From : Positive;
               Going : Direction := Forward;
               Mapping : Character_Mapping)
               return Natural is
            begin
               return Fixed_Index_Mapping_From (
                  Source.Element (1 .. Source.Length),
                  Pattern,
                  From,
                  Going,
                  Mapping);
            end Index;

            function Index (
               Source : Bounded_String;
               Pattern : String_Type;
               Going : Direction := Forward;
               Mapping : Character_Mapping)
               return Natural is
            begin
               return Fixed_Index_Mapping (
                  Source.Element (1 .. Source.Length),
                  Pattern,
                  Going,
                  Mapping);
            end Index;

            function Index (
               Source : Bounded_String;
               Pattern : String_Type;
               From : Positive;
               Going : Direction := Forward;
               Mapping : not null access function (From : Wide_Wide_Character)
                  return Wide_Wide_Character)
               return Natural is
            begin
               return Fixed_Index_Mapping_Function_From (
                  Source.Element (1 .. Source.Length),
                  Pattern,
                  From,
                  Going,
                  Mapping);
            end Index;

            function Index (
               Source : Bounded_String;
               Pattern : String_Type;
               Going : Direction := Forward;
               Mapping : not null access function (From : Wide_Wide_Character)
                  return Wide_Wide_Character)
               return Natural is
            begin
               return Fixed_Index_Mapping_Function (
                  Source.Element (1 .. Source.Length),
                  Pattern,
                  Going,
                  Mapping);
            end Index;

            function Index_Per_Element (
               Source : Bounded_String;
               Pattern : String_Type;
               From : Positive;
               Going : Direction := Forward;
               Mapping : not null access function (From : Character_Type)
                  return Character_Type)
               return Natural is
            begin
               return Fixed_Index_Mapping_Function_Per_Element_From (
                  Source.Element (1 .. Source.Length),
                  Pattern,
                  From,
                  Going,
                  Mapping);
            end Index_Per_Element;

            function Index_Per_Element (
               Source : Bounded_String;
               Pattern : String_Type;
               Going : Direction := Forward;
               Mapping : not null access function (From : Character_Type)
                  return Character_Type)
               return Natural is
            begin
               return Fixed_Index_Mapping_Function_Per_Element (
                  Source.Element (1 .. Source.Length),
                  Pattern,
                  Going,
                  Mapping);
            end Index_Per_Element;

            function Index (
               Source : Bounded_String;
               Set : Character_Set;
               From : Positive;
               Test : Membership := Inside;
               Going : Direction := Forward)
               return Natural is
            begin
               return Fixed_Index_Set_From (
                  Source.Element (1 .. Source.Length),
                  Set,
                  From,
                  Test,
                  Going);
            end Index;

            function Index (
               Source : Bounded_String;
               Set : Character_Set;
               Test : Membership := Inside;
               Going : Direction := Forward)
               return Natural is
            begin
               return Fixed_Index_Set (
                  Source.Element (1 .. Source.Length),
                  Set,
                  Test,
                  Going);
            end Index;

            function Count (
               Source : Bounded_String;
               Pattern : String_Type;
               Mapping : Character_Mapping)
               return Natural is
            begin
               return Fixed_Count_Mapping (
                  Source.Element (1 .. Source.Length),
                  Pattern,
                  Mapping);
            end Count;

            function Count (
               Source : Bounded_String;
               Pattern : String_Type;
               Mapping : not null access function (From : Wide_Wide_Character)
                  return Wide_Wide_Character)
               return Natural is
            begin
               return Fixed_Count_Mapping_Function (
                  Source.Element (1 .. Source.Length),
                  Pattern,
                  Mapping);
            end Count;

            function Count_Per_Element (
               Source : Bounded_String;
               Pattern : String_Type;
               Mapping : not null access function (From : Character_Type)
                  return Character_Type)
               return Natural is
            begin
               return Fixed_Count_Mapping_Function_Per_Element (
                  Source.Element (1 .. Source.Length),
                  Pattern,
                  Mapping);
            end Count_Per_Element;

            function Count (Source : Bounded_String; Set : Character_Set)
               return Natural is
            begin
               return Fixed_Count_Set (
                  Source.Element (1 .. Source.Length),
                  Set);
            end Count;

            procedure Find_Token (
               Source : Bounded_String;
               Set : Character_Set;
               From : Positive;
               Test : Membership;
               First : out Positive;
               Last : out Natural) is
            begin
               Fixed_Find_Token_From (
                  Source.Element (1 .. Source.Length),
                  Set,
                  From,
                  Test,
                  First,
                  Last);
            end Find_Token;

            procedure Find_Token (
               Source : Bounded_String;
               Set : Character_Set;
               Test : Membership;
               First : out Positive;
               Last : out Natural) is
            begin
               Fixed_Find_Token (
                  Source.Element (1 .. Source.Length),
                  Set,
                  Test,
                  First,
                  Last);
            end Find_Token;

            function Translate (
               Source : Bounded_String;
               Mapping : Character_Mapping;
               Drop : Truncation := Error)
               return Bounded_String is
            begin
               return To_Bounded_String (
                  Fixed_Translate_Mapping (
                     Source.Element (1 .. Source.Length),
                     Mapping),
                  Drop);
            end Translate;

            procedure Translate (
               Source : in out Bounded_String;
               Mapping : Character_Mapping;
               Drop : Truncation := Error) is
            begin
               Set_Bounded_String (
                  Source,
                  Fixed_Translate_Mapping (
                     Source.Element (1 .. Source.Length),
                     Mapping),
                  Drop);
            end Translate;

            function Translate (
               Source : Bounded_String;
               Mapping : not null access function (From : Wide_Wide_Character)
                  return Wide_Wide_Character;
               Drop : Truncation := Error)
               return Bounded_String is
            begin
               return To_Bounded_String (
                  Fixed_Translate_Mapping_Function (
                     Source.Element (1 .. Source.Length),
                     Mapping),
                  Drop);
            end Translate;

            procedure Translate (
               Source : in out Bounded_String;
               Mapping : not null access function (From : Wide_Wide_Character)
                  return Wide_Wide_Character;
               Drop : Truncation := Error) is
            begin
               Set_Bounded_String (
                  Source,
                  Fixed_Translate_Mapping_Function (
                     Source.Element (1 .. Source.Length),
                     Mapping),
                  Drop);
            end Translate;

            function Translate_Per_Element (
               Source : Bounded_String;
               Mapping : not null access function (From : Character_Type)
                  return Character_Type)
               return Bounded_String is
            begin
               return To_Bounded_String (
                  Fixed_Translate_Mapping_Function_Per_Element (
                     Source.Element (1 .. Source.Length),
                     Mapping));
            end Translate_Per_Element;

            procedure Translate_Per_Element (
               Source : in out Bounded_String;
               Mapping : not null access function (From : Character_Type)
                  return Character_Type) is
            begin
               Set_Bounded_String (
                  Source,
                  Fixed_Translate_Mapping_Function_Per_Element (
                     Source.Element (1 .. Source.Length),
                     Mapping));
            end Translate_Per_Element;

            function Trim (
               Source : Bounded_String;
               Left : Character_Set;
               Right : Character_Set)
               return Bounded_String
            is
               S : String_Type renames Source.Element (1 .. Source.Length);
               First : Positive;
               Last : Natural;
            begin
               Fixed_Trim_Set (S, Left, Right, First, Last);
               return To_Bounded_String (S (First .. Last), Error);
            end Trim;

            procedure Trim (
               Source : in out Bounded_String;
               Left : Character_Set;
               Right : Character_Set)
            is
               S : String_Type renames Source.Element (1 .. Source.Length);
               First : Positive;
               Last : Natural;
            begin
               Fixed_Trim_Set (S, Left, Right, First, Last);
               Set_Bounded_String (Source, S (First .. Last), Error);
            end Trim;

         end Generic_Maps;

      end Generic_Functions;

      package body No_Primitives is

         procedure Read (
            Stream : not null access Streams.Root_Stream_Type'Class;
            Item : out Bounded_String)
         is
            First : Integer;
            Last : Integer;
         begin
            Integer'Read (Stream, First);
            Integer'Read (Stream, Last);
            Item.Length := Last - First + 1;
            Read (Stream, Item.Element (1 .. Item.Length));
         end Read;

         function Input (
            Stream : not null access Streams.Root_Stream_Type'Class)
            return Bounded_String is
         begin
            return Result : Bounded_String do
               Read (Stream, Result);
            end return;
         end Input;

         procedure Write (
            Stream : not null access Streams.Root_Stream_Type'Class;
            Item : Bounded_String) is
         begin
            Integer'Write (Stream, 1);
            Integer'Write (Stream, Item.Length);
            Write (Stream, Item.Element (1 .. Item.Length));
         end Write;

      end No_Primitives;

   end Generic_Bounded_Length;

end Ada.Strings.Generic_Bounded;
