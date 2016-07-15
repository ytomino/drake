with Ada.Unchecked_Conversion;
with System.Address_To_Named_Access_Conversions;
with System.Storage_Elements;
package body Ada.Strings.Generic_Functions is
   use type System.Address;
   use type System.Storage_Elements.Storage_Offset;

   procedure memset (
      b : System.Address;
      c : Integer;
      n : System.Storage_Elements.Storage_Count)
      with Import,
         Convention => Intrinsic, External_Name => "__builtin_memset";

   function memchr (
      s : System.Address;
      c : Integer;
      n : System.Storage_Elements.Storage_Count)
      return System.Address
      with Import,
         Convention => Intrinsic, External_Name => "__builtin_memchr";

   procedure Fill (
      Target : out String_Type;
      Pad : Character_Type := Space);
   procedure Fill (
      Target : out String_Type;
      Pad : Character_Type := Space) is
   begin
      if Character_Type'Size = Standard'Storage_Unit
         and then String_Type'Component_Size = Standard'Storage_Unit
      then
         memset (Target'Address, Character_Type'Pos (Pad), Target'Length);
      else
         for I in Target'Range loop
            Target (I) := Pad;
         end loop;
      end if;
   end Fill;

   --  implementation

   procedure Move (
      Source : String_Type;
      Target : out String_Type;
      Drop : Truncation := Error;
      Justify : Alignment := Left;
      Pad : Character_Type := Space)
   is
      Target_Offset : constant Integer := Target'Length - 1;
      Triming_Limit : Integer;
      Source_First : Positive := Source'First;
      Source_Last : Natural := Source'Last;
      Target_First : Positive;
      Target_Last : Natural;
   begin
      if Justify = Center then
         Triming_Limit := -1;
      else
         Triming_Limit := Target_Offset;
      end if;
      if Source_Last - Source_First > Triming_Limit then
         if Justify /= Right then -- Left or Center
            while Source_Last - Source_First > Triming_Limit
               and then Source (Source_Last) = Pad
            loop
               Source_Last := Source_Last - 1;
            end loop;
         end if;
         if Justify /= Left then -- Center or Right
            while Source_Last - Source_First > Triming_Limit
               and then Source (Source_First) = Pad
            loop
               Source_First := Source_First + 1;
            end loop;
         end if;
         if Source_Last - Source_First > Target_Offset then
            case Drop is
               when Left =>
                  Source_First := Source_Last - Target_Offset;
               when Right =>
                  Source_Last := Source_First + Target_Offset;
               when Error =>
                  raise Length_Error;
            end case;
         end if;
      end if;
      case Justify is
         when Left =>
            Target_First := Target'First;
            Target_Last := Target_First + (Source_Last - Source_First);
         when Center =>
            Target_First :=
               (Target'First + Target'Last - (Source_Last - Source_First)) / 2;
            Target_Last := Target_First + (Source_Last - Source_First);
         when Right =>
            Target_Last := Target'Last;
            Target_First := Target_Last - (Source_Last - Source_First);
      end case;
      --  contents
      declare
         Source_Contents : String_Type
            renames Source (Source_First .. Source_Last);
         Target_Contents : String_Type
            renames Target (Target_First .. Target_Last);
      begin
         if Source_Contents'Address /= Target_Contents'Address then
            Target_Contents := Source_Contents;
         end if;
      end;
      --  left padding
      if Target_First /= Target'First then
         Fill (Target (Target'First .. Target_First - 1), Pad);
      end if;
      --  right padding
      if Target_Last /= Target'Last then
         Fill (Target (Target_Last + 1 .. Target'Last), Pad);
      end if;
   end Move;

   function Index_Element (
      Source : String_Type;
      Pattern : Character_Type;
      From : Positive;
      Going : Direction := Forward)
      return Natural is
   begin
      case Going is
         when Forward =>
            return Index_Element_Forward (
               Source (From .. Source'Last),
               Pattern);
         when Backward =>
            return Index_Element_Backward (
               Source (Source'First .. From),
               Pattern);
      end case;
   end Index_Element;

   function Index_Element (
      Source : String_Type;
      Pattern : Character_Type;
      Going : Direction := Forward)
      return Natural is
   begin
      case Going is
         when Forward =>
            return Index_Element_Forward (Source, Pattern);
         when Backward =>
            return Index_Element_Backward (Source, Pattern);
      end case;
   end Index_Element;

   function Index_Element_Forward (
      Source : String_Type;
      Pattern : Character_Type)
      return Natural is
   begin
      if Character_Type'Size = Standard'Storage_Unit
         and then String_Type'Component_Size = Standard'Storage_Unit
      then
         declare
            P : constant System.Address :=
               memchr (
                  Source'Address,
                  Character_Type'Pos (Pattern),
                  Source'Length);
         begin
            if P = System.Null_Address then
               return 0;
            else
               return Source'First + Integer (P - Source'Address);
            end if;
         end;
      else
         for I in Source'Range loop
            if Source (I) = Pattern then
               return I;
            end if;
         end loop;
         return 0;
      end if;
   end Index_Element_Forward;

   function Index_Element_Backward (
      Source : String_Type;
      Pattern : Character_Type)
      return Natural is
   begin
      --  __builtin_memrchr does not exist...
      for I in reverse Source'Range loop
         if Source (I) = Pattern then
            return I;
         end if;
      end loop;
      return 0;
   end Index_Element_Backward;

   function Index (
      Source : String_Type;
      Pattern : String_Type;
      From : Positive;
      Going : Direction := Forward)
      return Natural is
   begin
      case Going is
         when Forward =>
            return Index_Forward (Source (From .. Source'Last), Pattern);
         when Backward =>
            return Index_Backward (
               Source (
                  Source'First ..
                  Natural'Min (From + (Pattern'Length - 1), Source'Last)),
               Pattern);
      end case;
   end Index;

   function Index (
      Source : String_Type;
      Pattern : String_Type;
      Going : Direction := Forward)
      return Natural is
   begin
      case Going is
         when Forward =>
            return Index_Forward (Source, Pattern);
         when Backward =>
            return Index_Backward (Source, Pattern);
      end case;
   end Index;

   function Index_Forward (Source : String_Type; Pattern : String_Type)
      return Natural is
   begin
      if Pattern'Length = 0 then
         raise Pattern_Error;
      else
         declare
            Pattern_Length : constant Natural := Pattern'Length;
            Last : constant Integer := Source'Last - (Pattern_Length - 1);
            Searched_Last : Natural := Source'First - 1;
         begin
            while Searched_Last < Last loop
               declare
                  Position : constant Natural :=
                     Index_Element_Forward (
                        Source (Searched_Last + 1 .. Last),
                        Pattern (Pattern'First));
               begin
                  exit when Position = 0;
                  if Source (Position .. Position + (Pattern_Length - 1)) =
                     Pattern
                  then
                     return Position;
                  end if;
                  Searched_Last := Position;
               end;
            end loop;
            return 0;
         end;
      end if;
   end Index_Forward;

   function Index_Backward (Source : String_Type; Pattern : String_Type)
      return Natural is
   begin
      if Pattern'Length = 0 then
         raise Pattern_Error;
      else
         declare
            Pattern_Length : constant Natural := Pattern'Length;
            Unsearched_Last : Natural := Source'Last - (Pattern_Length - 1);
         begin
            while Unsearched_Last >= Source'First loop
               declare
                  Position : constant Natural :=
                     Index_Element_Backward (
                        Source (Source'First .. Unsearched_Last),
                        Pattern (Pattern'First));
               begin
                  exit when Position = 0;
                  if Source (Position .. Position + (Pattern_Length - 1)) =
                     Pattern
                  then
                     return Position;
                  end if;
                  Unsearched_Last := Position - 1;
               end;
            end loop;
            return 0;
         end;
      end if;
   end Index_Backward;

   function Index_Non_Blank (
      Source : String_Type;
      From : Positive;
      Going : Direction := Forward)
      return Natural is
   begin
      case Going is
         when Forward =>
            return Index_Non_Blank_Forward (Source (From .. Source'Last));
         when Backward =>
            return Index_Non_Blank_Backward (Source (Source'First .. From));
      end case;
   end Index_Non_Blank;

   function Index_Non_Blank (
      Source : String_Type;
      Going : Direction := Forward)
      return Natural is
   begin
      case Going is
         when Forward =>
            return Index_Non_Blank_Forward (Source);
         when Backward =>
            return Index_Non_Blank_Backward (Source);
      end case;
   end Index_Non_Blank;

   function Index_Non_Blank_Forward (
      Source : String_Type;
      Blank : Character_Type := Space)
      return Natural is
   begin
      for I in Source'Range loop
         if Source (I) /= Blank then
            return I;
         end if;
      end loop;
      return 0;
   end Index_Non_Blank_Forward;

   function Index_Non_Blank_Backward (
      Source : String_Type;
      Blank : Character_Type := Space)
      return Natural is
   begin
      for I in reverse Source'Range loop
         if Source (I) /= Blank then
            return I;
         end if;
      end loop;
      return 0;
   end Index_Non_Blank_Backward;

   function Count (
      Source : String_Type;
      Pattern : String_Type)
      return Natural
   is
      Searched_Last : Natural := Source'First - 1;
      Result : Natural := 0;
   begin
      while Searched_Last < Source'Last loop
         declare
            Position : constant Natural :=
               Index_Forward (
                  Source (Searched_Last + 1 .. Source'Last),
                  Pattern);
         begin
            exit when Position = 0;
            Searched_Last := Position + (Pattern'Length - 1);
         end;
         Result := Result + 1;
      end loop;
      return Result;
   end Count;

   function Replace_Slice (
      Source : String_Type;
      Low : Positive;
      High : Natural;
      By : String_Type)
      return String_Type
   is
      pragma Check (Pre,
         Check =>
            (Low in
                  Source'First ..
                  Integer'Min (Source'Last, Integer'Last - 1) + 1
               and then High in Source'First - 1 .. Source'Last)
            or else raise Index_Error);
   begin
      return Result : String_Type (
         1 ..
         Source'Length + By'Length - Integer'Max (High - Low + 1, 0))
      do
         declare
            Dummy_Last : Natural;
         begin
            Replace_Slice (Source, Low, High, By, Result, Dummy_Last);
         end;
      end return;
   end Replace_Slice;

   procedure Replace_Slice (
      Source : in out String_Type;
      Low : Positive;
      High : Natural;
      By : String_Type;
      Drop : Truncation := Error;
      Justify : Alignment := Left;
      Pad : Character_Type := Space)
   is
      pragma Check (Pre,
         Check =>
            (Low in
                  Source'First ..
                  Integer'Min (Source'Last, Integer'Last - 1) + 1
               and then High in Source'First - 1 .. Source'Last)
            or else raise Index_Error); -- CXA4005, CXA4016
      Offset : constant Integer := By'Length - Integer'Max (High - Low + 1, 0);
   begin
      if Offset > 0 then -- growing
         declare
            S : String_Type (1 .. Source'Length + Offset);
            S_Last : Natural;
         begin
            Replace_Slice (Source, Low, High, By, S, S_Last); -- copying
            Move (S (1 .. S_Last), Source, Drop, Justify, Pad);
         end;
      else
         declare
            Last : Natural := Source'Last;
         begin
            Replace_Slice (Source, Last, Low, High, By);
            Move (Source (Source'First .. Last), Source, Drop, Justify, Pad);
         end;
      end if;
   end Replace_Slice;

   procedure Replace_Slice (
      Source : String_Type;
      Low : Positive;
      High : Natural;
      By : String_Type;
      Target : out String_Type;
      Target_Last : out Natural)
   is
      By_Length : constant Natural := By'Length;
      Slice_Last : constant Natural := Integer'Max (High, Low - 1);
      Target_Low : constant Positive := Low - Source'First + Target'First;
      Target_Slice_Last : constant Natural := Target_Low + (By_Length - 1);
   begin
      Target_Last :=
         Target'First
         + (Source'Length - 1)
         - (Slice_Last - Low)
         + (By_Length - 1);
      Target (Target'First .. Target_Low - 1) :=
         Source (Source'First .. Low - 1);
      Target (Target_Low .. Target_Slice_Last) := By;
      if Target_Slice_Last < Target_Last then
         Target (Target_Slice_Last + 1 .. Target_Last) :=
            Source (Slice_Last + 1 .. Source'Last);
      end if;
   end Replace_Slice;

   procedure Replace_Slice (
      Source : in out String_Type;
      Last : in out Natural;
      Low : Positive;
      High : Natural;
      By : String_Type)
   is
      Slice_Last : constant Natural := Integer'Max (High, Low - 1);
      New_Slice_Last : constant Natural := Low + (By'Length - 1);
      New_Last : constant Natural := Last + (New_Slice_Last - Slice_Last);
   begin
      if New_Slice_Last < New_Last and then New_Slice_Last /= Slice_Last then
         Source (New_Slice_Last + 1 .. New_Last) :=
            Source (Slice_Last + 1 .. Last);
      end if;
      Source (Low .. New_Slice_Last) := By;
      Last := New_Last;
   end Replace_Slice;

   function Insert (
      Source : String_Type;
      Before : Positive;
      New_Item : String_Type)
      return String_Type
   is
      pragma Check (Pre,
         Check =>
            Before in
               Source'First ..
               Integer'Min (Source'Last, Integer'Last - 1) + 1
            or else raise Index_Error); -- CXA4005, CXA4016
   begin
      return Result : String_Type (1 .. Source'Length + New_Item'Length) do
         declare
            Dummy_Last : Natural;
         begin
            Insert (Source, Before, New_Item, Result, Dummy_Last);
         end;
      end return;
   end Insert;

   procedure Insert (
      Source : in out String_Type;
      Before : Positive;
      New_Item : String_Type;
      Drop : Truncation := Error)
   is
      pragma Check (Pre,
         Check =>
            Before in
               Source'First ..
               Integer'Min (Source'Last, Integer'Last - 1) + 1
            or else raise Index_Error);
   begin
      if New_Item'Length > 0 then -- growing
         declare
            S : String_Type (1 .. Source'Length + New_Item'Length);
            S_Last : Natural;
         begin
            Insert (Source, Before, New_Item, S, S_Last); -- copying
            Move (S (1 .. S_Last), Source, Drop);
         end;
      end if;
   end Insert;

   procedure Insert (
      Source : String_Type;
      Before : Positive;
      New_Item : String_Type;
      Target : out String_Type;
      Target_Last : out Natural)
   is
      New_Item_Length : constant Natural := New_Item'Length;
      Target_Before : constant Positive :=
         Before - Source'First + Target'First;
      Target_Slice_Last : constant Natural :=
         Target_Before + (New_Item_Length - 1);
   begin
      Target_Last := Target'First + (Source'Length - 1) + New_Item_Length;
      Target (Target'First .. Target_Before - 1) :=
         Source (Source'First .. Before - 1);
      Target (Target_Before .. Target_Slice_Last) := New_Item;
      Target (Target_Slice_Last + 1 .. Target_Last) :=
         Source (Before .. Source'Last);
   end Insert;

   procedure Insert (
      Source : in out String_Type;
      Last : in out Natural;
      Before : Positive;
      New_Item : String_Type)
   is
      New_Slice_Last : constant Natural := Before + (New_Item'Length - 1);
      New_Last : constant Natural := New_Slice_Last + 1 + Last - Before;
   begin
      if New_Slice_Last + 1 /= Before then
         Source (New_Slice_Last + 1 .. New_Last) := Source (Before .. Last);
      end if;
      Source (Before .. New_Slice_Last) := New_Item;
      Last := New_Last;
   end Insert;

   function Overwrite (
      Source : String_Type;
      Position : Positive;
      New_Item : String_Type)
      return String_Type is
   begin
      return Replace_Slice (
         Source,
         Position, -- checking Index_Error
         Integer'Min (Position + (New_Item'Length - 1), Source'Last),
         New_Item);
   end Overwrite;

   procedure Overwrite (
      Source : in out String_Type;
      Position : Positive;
      New_Item : String_Type;
      Drop : Truncation := Right) is
   begin
      Replace_Slice (
         Source,
         Position, -- checking Index_Error
         Integer'Min (Position + (New_Item'Length - 1), Source'Last),
         New_Item,
         Drop);
   end Overwrite;

   function Delete (
      Source : String_Type;
      From : Positive;
      Through : Natural)
      return String_Type
   is
      pragma Check (Pre,
         Check =>
            (From <= Integer'Min (Source'Last, Integer'Last - 1) + 1
               and then Through <= Source'Last)
            or else raise Index_Error);
   begin
      return Result : String_Type (
         1 .. Source'Length - Integer'Max (0, Through - From + 1))
      do
         declare
            Dummy_Last : Natural;
         begin
            Delete (Source, From, Through, Result, Dummy_Last);
         end;
      end return;
   end Delete;

   procedure Delete (
      Source : in out String_Type;
      From : Positive;
      Through : Natural;
      Justify : Alignment := Left;
      Pad : Character_Type := Space)
   is
      pragma Check (Pre,
         Check =>
            (From <= Integer'Min (Source'Last, Integer'Last - 1) + 1
               and then Through <= Source'Last)
            or else raise Index_Error);
      Last : Natural := Source'Last;
   begin
      Delete (Source, Last, From, Through);
      Move (
         Source (Source'First .. Last),
         Source,
         Error, -- no raising because Source'Length be not growing
         Justify,
         Pad);
   end Delete;

   procedure Delete (
      Source : String_Type;
      From : Positive;
      Through : Natural;
      Target : out String_Type;
      Target_Last : out Natural)
   is
      Source_Slice_Last : Natural;
      Target_Slice_Last : Natural;
      Following_Length : Natural;
   begin
      if From <= Through then
         Target_Slice_Last := Target'First - 1 + (From - Source'First);
         Target (Target'First .. Target_Slice_Last) :=
            Source (Source'First .. From - 1);
         Source_Slice_Last := Through;
         Following_Length := Source'Last - Through;
      else
         Source_Slice_Last := Source'First - 1;
         Target_Slice_Last := Target'First - 1;
         Following_Length := Source'Length;
      end if;
      Target_Last := Target_Slice_Last + Following_Length;
      if Target_Slice_Last < Target_Last then
         Target (Target_Slice_Last + 1 .. Target_Last) :=
            Source (Source_Slice_Last + 1 .. Source'Last);
      end if;
   end Delete;

   procedure Delete (
      Source : in out String_Type;
      Last : in out Natural;
      From : Positive;
      Through : Natural) is
   begin
      if From <= Through then
         declare
            Old_Last : constant Natural := Last;
         begin
            Last := Last - (Through - From + 1);
            Source (From .. Last) := Source (Through + 1 .. Old_Last);
         end;
      end if;
   end Delete;

   function Trim (
      Source : String_Type;
      Side : Trim_End;
      Blank : Character_Type := Space)
      return String_Type
   is
      First : Positive;
      Last : Natural;
   begin
      Trim (Source, Side, Blank, First, Last);
      declare
         subtype T is String_Type (1 .. Last - First + 1);
      begin
         return T (Source (First .. Last));
      end;
   end Trim;

   procedure Trim (
      Source : in out String_Type;
      Side : Trim_End;
      Justify : Alignment := Left;
      Pad : Character_Type := Space) is
   begin
      Trim (Source, Side, Space, Justify, Pad);
   end Trim;

   procedure Trim (
      Source : in out String_Type;
      Side : Trim_End;
      Blank : Character_Type;
      Justify : Alignment := Left;
      Pad : Character_Type := Space)
   is
      First : Positive;
      Last : Natural;
   begin
      Trim (Source, Side, Blank, First, Last);
      Move (
         Source (First .. Last),
         Source,
         Error, -- no raising because Source'Length be not growing
         Justify,
         Pad);
   end Trim;

   procedure Trim (
      Source : String_Type;
      Side : Trim_End;
      Blank : Character_Type := Space;
      First : out Positive;
      Last : out Natural) is
   begin
      First := Source'First;
      Last := Source'Last;
      case Side is
         when Left | Both =>
            while First < Last and then Source (First) = Blank loop
               First := First + 1;
            end loop;
         when Right =>
            null;
      end case;
      case Side is
         when Right | Both =>
            while Last > First and then Source (Last) = Blank loop
               Last := Last - 1;
            end loop;
         when Left =>
            null;
      end case;
      if First = Last and then Source (First) = Blank then
         First := Source'First;
         Last := Source'Last - 1;
      end if;
   end Trim;

   function Head (
      Source : String_Type;
      Count : Natural;
      Pad : Character_Type := Space)
      return String_Type is
   begin
      return Result : String_Type (1 .. Count) do
         declare
            Dummy_Last : Natural;
         begin
            Head (Source, Count, Pad, Result, Dummy_Last);
         end;
      end return;
   end Head;

   procedure Head (
      Source : in out String_Type;
      Count : Natural;
      Justify : Alignment := Left;
      Pad : Character_Type := Space) is
   begin
      if Count > Source'Length then
         declare
            S : String_Type (1 .. Count);
            S_Last : Natural;
         begin
            Head (Source, Count, Pad, S, S_Last); -- copying
            Move (S (1 .. S_Last), Source, Error, Justify, Pad);
         end;
      else
         declare
            Last : Natural := Source'Last;
         begin
            Head (Source, Last, Count, Pad);
            Move (Source (Source'First .. Last), Source, Error, Justify, Pad);
         end;
      end if;
   end Head;

   procedure Head (
      Source : String_Type;
      Count : Natural;
      Pad : Character_Type := Space;
      Target : out String_Type;
      Target_Last : out Natural)
   is
      Taking : constant Natural := Integer'Min (Source'Length, Count);
   begin
      Target (Target'First .. Target'First - 1 + Taking) :=
         Source (Source'First .. Source'First - 1 + Taking);
      Target_Last := Target'First - 1 + Count;
      Fill (Target (Target'First + Taking .. Target_Last), Pad);
   end Head;

   procedure Head (
      Source : in out String_Type;
      Last : in out Natural;
      Count : Natural;
      Pad : Character_Type := Space)
   is
      New_Last : constant Natural := Source'First - 1 + Count;
   begin
      if Last < New_Last then
         Fill (Source (Last + 1 .. New_Last), Pad);
      end if;
      Last := New_Last;
   end Head;

   function Tail (
      Source : String_Type;
      Count : Natural;
      Pad : Character_Type := Space)
      return String_Type is
   begin
      return Result : String_Type (1 .. Count) do
         declare
            Dummy_Last : Natural;
         begin
            Tail (Source, Count, Pad, Result, Dummy_Last);
         end;
      end return;
   end Tail;

   procedure Tail (
      Source : in out String_Type;
      Count : Natural;
      Justify : Alignment := Left;
      Pad : Character_Type := Space) is
   begin
      if Count /= Source'Length then
         declare
            S : String_Type (1 .. Count);
            S_Last : Natural;
         begin
            Tail (Source, Count, Pad, S, S_Last); -- copying
            Move (S (1 .. S_Last), Source, Error, Justify, Pad);
         end;
      end if;
   end Tail;

   procedure Tail (
      Source : String_Type;
      Count : Natural;
      Pad : Character_Type := Space;
      Target : out String_Type;
      Target_Last : out Natural)
   is
      Taking : constant Natural := Natural'Min (Source'Length, Count);
   begin
      Target_Last := Target'First - 1 + Count;
      Target (Target_Last - Taking + 1 .. Target_Last) :=
         Source (Source'Last - Taking + 1 .. Source'Last);
      Fill (Target (Target'First .. Target_Last - Taking), Pad);
   end Tail;

   function "*" (Left : Natural; Right : Character_Type)
      return String_Type is
   begin
      return (1 .. Left => Right);
   end "*";

   function "*" (Left : Natural; Right : String_Type)
      return String_Type
   is
      Right_Length : constant Natural := Right'Length;
   begin
      return Result : String_Type (1 .. Left * Right_Length) do
         declare
            Last : Natural := Result'First - 1;
         begin
            for I in 1 .. Left loop
               Result (Last + 1 .. Last + Right_Length) := Right;
               Last := Last + Right_Length;
            end loop;
         end;
      end return;
   end "*";

   package body Generic_Maps is

      function Last_Of_Index_Backward (
         Source : String_Type;
         Pattern : String_Type;
         From : Positive)
         return Natural;
      function Last_Of_Index_Backward (
         Source : String_Type;
         Pattern : String_Type;
         From : Positive)
         return Natural
      is
         Pattern_Count : Natural := 0;
         Result : Natural := From - 1;
      begin
         declare
            Pattern_Last : Natural := Pattern'First - 1;
         begin
            while Pattern_Last < Pattern'Last loop
               Pattern_Count := Pattern_Count + 1;
               declare
                  Code : Wide_Wide_Character;
                  Error : Boolean; -- ignore
               begin
                  Get (
                     Pattern (Pattern_Last + 1 .. Pattern'Last),
                     Pattern_Last,
                     Code,
                     Error);
               end;
            end loop;
         end;
         while Pattern_Count > 0 and then Result < Source'Last loop
            declare
               Code : Wide_Wide_Character;
               Error : Boolean; -- ignore
            begin
               Get (Source (Result + 1 .. Source'Last), Result, Code, Error);
            end;
            Pattern_Count := Pattern_Count - 1;
         end loop;
         return Result;
      end Last_Of_Index_Backward;

      function Index_Forward (
         Source : String_Type;
         Pattern : String_Type;
         Params : System.Address;
         Mapping : not null access function (
            From : Wide_Wide_Character;
            Params : System.Address)
            return Wide_Wide_Character)
         return Natural;
      function Index_Forward (
         Source : String_Type;
         Pattern : String_Type;
         Params : System.Address;
         Mapping : not null access function (
            From : Wide_Wide_Character;
            Params : System.Address)
            return Wide_Wide_Character)
         return Natural is
      begin
         if Pattern'Length = 0 then
            raise Pattern_Error;
         else
            declare
               Searched_Last : Natural := Source'First - 1;
            begin
               while Searched_Last < Source'Last loop
                  declare
                     Source_Index : constant Positive := Searched_Last + 1;
                     Buffer : String_Type (1 .. Expanding);
                     J, J_Last, Pattern_Last, Character_Length : Positive;
                     Code : Wide_Wide_Character;
                     Error : Boolean;
                  begin
                     Get (
                        Source (Source_Index .. Source'Last),
                        Searched_Last,
                        Code,
                        Error);
                     if Error then
                        Character_Length := Searched_Last - Source_Index + 1;
                        Buffer (1 .. Character_Length) :=
                           Source (Source_Index .. Searched_Last);
                     else
                        Code := Mapping (Code, Params);
                        Put (Code, Buffer, Character_Length);
                     end if;
                     Pattern_Last := Pattern'First - 1 + Character_Length;
                     if Buffer (1 .. Character_Length) =
                        Pattern (Pattern'First .. Pattern_Last)
                     then
                        J_Last := Searched_Last;
                        loop
                           if Pattern_Last >= Pattern'Last then
                              return Source_Index;
                           end if;
                           J := J_Last + 1;
                           exit when J > Source'Last;
                           Get (
                              Source (J .. Source'Last),
                              J_Last,
                              Code,
                              Error);
                           if Error then
                              Character_Length := J_Last - J + 1;
                              Buffer (1 .. Character_Length) :=
                                 Source (J .. J_Last);
                           else
                              Code := Mapping (Code, Params);
                              Put (Code, Buffer, Character_Length);
                           end if;
                           exit when Buffer (1 .. Character_Length) /=
                              Pattern (
                                 Pattern_Last + 1 ..
                                 Pattern_Last + Character_Length);
                           Pattern_Last := Pattern_Last + Character_Length;
                        end loop;
                     end if;
                  end;
               end loop;
               return 0;
            end;
         end if;
      end Index_Forward;

      function Index_Backward (
         Source : String_Type;
         Pattern : String_Type;
         Params : System.Address;
         Mapping : not null access function (
            From : Wide_Wide_Character;
            Params : System.Address)
            return Wide_Wide_Character)
         return Natural;
      function Index_Backward (
         Source : String_Type;
         Pattern : String_Type;
         Params : System.Address;
         Mapping : not null access function (
            From : Wide_Wide_Character;
            Params : System.Address)
            return Wide_Wide_Character)
         return Natural is
      begin
         if Pattern'Length = 0 then
            raise Pattern_Error;
         else
            declare
               Unsearched_Last : Natural := Source'Last;
            begin
               while Unsearched_Last >= Source'First loop
                  declare
                     Source_Index : constant Positive := Unsearched_Last;
                     Buffer : String_Type (1 .. Expanding);
                     J, J_First, Pattern_First, Character_Length : Natural;
                     Code : Wide_Wide_Character;
                     Error : Boolean;
                  begin
                     Get_Reverse (
                        Source (Source'First .. Source_Index),
                        J_First,
                        Code,
                        Error);
                     Unsearched_Last := J_First - 1;
                     if Error then
                        Character_Length := Source_Index - J_First + 1;
                        Buffer (1 .. Character_Length) :=
                           Source (J_First .. Source_Index);
                     else
                        Code := Mapping (Code, Params);
                        Put (Code, Buffer, Character_Length);
                     end if;
                     Pattern_First := Pattern'Last - Character_Length + 1;
                     if Buffer (1 .. Character_Length) =
                        Pattern (Pattern_First .. Pattern'Last)
                     then
                        loop
                           if Pattern_First <= Pattern'First then
                              return J_First;
                           end if;
                           J := J_First - 1;
                           exit when J < Source'First;
                           Get_Reverse (
                              Source (Source'First .. J),
                              J_First,
                              Code,
                              Error);
                           if Error then
                              Character_Length := J - J_First + 1;
                              Buffer (1 .. Character_Length) :=
                                 Source (J_First .. J);
                           else
                              Code := Mapping (Code, Params);
                              Put (Code, Buffer, Character_Length);
                           end if;
                           exit when Buffer (1 .. Character_Length) /=
                              Pattern (
                                 Pattern_First - Character_Length ..
                                 Pattern_First - 1);
                           Pattern_First := Pattern_First - Character_Length;
                        end loop;
                     end if;
                  end;
               end loop;
               return 0;
            end;
         end if;
      end Index_Backward;

      procedure Translate (
         Source : String_Type;
         Params : System.Address;
         Mapping : not null access function (
            From : Wide_Wide_Character;
            Params : System.Address)
            return Wide_Wide_Character;
         Target : out String_Type;
         Target_Last : out Natural);
      procedure Translate (
         Source : String_Type;
         Params : System.Address;
         Mapping : not null access function (
            From : Wide_Wide_Character;
            Params : System.Address)
            return Wide_Wide_Character;
         Target : out String_Type;
         Target_Last : out Natural)
      is
         Source_Last : Natural := Source'First - 1;
      begin
         Target_Last := Target'First - 1;
         while Source_Last < Source'Last loop
            declare
               Source_Index : constant Positive := Source_Last + 1;
               Target_Index : constant Positive := Target_Last + 1;
               Code : Wide_Wide_Character;
               Error : Boolean;
            begin
               --  get single unicode character
               Get (
                  Source (Source_Index .. Source'Last),
                  Source_Last,
                  Code,
                  Error);
               if Error then
                  --  keep illegal sequence
                  Target_Last := Target_Index + (Source_Last - Source_Index);
                  Target (Target_Index .. Target_Last) :=
                     Source (Source_Index .. Source_Last);
               else
                  --  map it
                  Code := Mapping (Code, Params);
                  --  put it
                  Put (
                     Code,
                     Target (Target_Index .. Target'Last),
                     Target_Last);
               end if;
            end;
         end loop;
      end Translate;

      function By_Mapping (From : Wide_Wide_Character; Params : System.Address)
         return Wide_Wide_Character;
      function By_Mapping (From : Wide_Wide_Character; Params : System.Address)
         return Wide_Wide_Character
      is
         type Character_Mapping_Access is access all Character_Mapping;
         for Character_Mapping_Access'Storage_Size use 0;
         package Conv is
            new System.Address_To_Named_Access_Conversions (
               Character_Mapping,
               Character_Mapping_Access);
      begin
         return Value (Conv.To_Pointer (Params).all, From);
      end By_Mapping;

      function By_Func (From : Wide_Wide_Character; Params : System.Address)
         return Wide_Wide_Character;
      function By_Func (From : Wide_Wide_Character; Params : System.Address)
         return Wide_Wide_Character
      is
         type T is access function (From : Wide_Wide_Character)
            return Wide_Wide_Character;
         function Cast is new Unchecked_Conversion (System.Address, T);
      begin
         return Cast (Params) (From);
      end By_Func;

      function Find_Non_Token_Last (
         Source : String_Type;
         Set : Character_Set;
         Test : Membership)
         return Positive;
      function Find_Non_Token_Last (
         Source : String_Type;
         Set : Character_Set;
         Test : Membership)
         return Positive
      is
         Unsearched_Last : Natural := Source'Last;
      begin
         while Unsearched_Last >= Source'First loop
            declare
               Code_First : Positive;
               Code : Wide_Wide_Character;
               Error : Boolean;
            begin
               Get_Reverse (
                  Source (Source'First .. Unsearched_Last),
                  Code_First,
                  Code,
                  Error);
               if Error then
                  exit when Test = Inside; -- illegal sequence is outside
               else
                  exit when Is_In (Code, Set) /= (Test = Inside);
               end if;
               Unsearched_Last := Code_First - 1;
            end;
         end loop;
         return Unsearched_Last;
      end Find_Non_Token_Last;

      --  implementation

      function Index (
         Source : String_Type;
         Pattern : String_Type;
         From : Positive;
         Going : Direction := Forward;
         Mapping : Character_Mapping)
         return Natural is
      begin
         case Going is
            when Forward =>
               return Index_Forward (
                  Source (From .. Source'Last),
                  Pattern,
                  Mapping);
            when Backward =>
               return Index_Backward (
                  Source (
                     Source'First ..
                     Last_Of_Index_Backward (Source, Pattern, From)),
                  Pattern,
                  Mapping);
         end case;
      end Index;

      function Index (
         Source : String_Type;
         Pattern : String_Type;
         Going : Direction := Forward;
         Mapping : Character_Mapping)
         return Natural is
      begin
         case Going is
            when Forward =>
               return Index_Forward (Source, Pattern, Mapping);
            when Backward =>
               return Index_Backward (Source, Pattern, Mapping);
         end case;
      end Index;

      function Index_Forward (
         Source : String_Type;
         Pattern : String_Type;
         Mapping : Character_Mapping)
         return Natural is
      begin
         return Index_Forward (
            Source,
            Pattern,
            Mapping'Address,
            By_Mapping'Access);
      end Index_Forward;

      function Index_Backward (
         Source : String_Type;
         Pattern : String_Type;
         Mapping : Character_Mapping)
         return Natural is
      begin
         return Index_Backward (
            Source,
            Pattern,
            Mapping'Address,
            By_Mapping'Access);
      end Index_Backward;

      function Index (
         Source : String_Type;
         Pattern : String_Type;
         From : Positive;
         Going : Direction := Forward;
         Mapping : not null access function (From : Wide_Wide_Character)
            return Wide_Wide_Character)
         return Natural is
      begin
         case Going is
            when Forward =>
               return Index_Forward (
                  Source (From .. Source'Last),
                  Pattern,
                  Mapping);
            when Backward =>
               return Index_Backward (
                  Source (
                     Source'First ..
                     Last_Of_Index_Backward (Source, Pattern, From)),
                  Pattern,
                  Mapping);
         end case;
      end Index;

      function Index (
         Source : String_Type;
         Pattern : String_Type;
         Going : Direction := Forward;
         Mapping : not null access function (From : Wide_Wide_Character)
            return Wide_Wide_Character)
         return Natural is
      begin
         case Going is
            when Forward =>
               return Index_Forward (Source, Pattern, Mapping);
            when Backward =>
               return Index_Backward (Source, Pattern, Mapping);
         end case;
      end Index;

      function Index_Forward (
         Source : String_Type;
         Pattern : String_Type;
         Mapping : not null access function (From : Wide_Wide_Character)
            return Wide_Wide_Character)
         return Natural
      is
         type T is access function (From : Wide_Wide_Character)
            return Wide_Wide_Character;
         function Cast is new Unchecked_Conversion (T, System.Address);
      begin
         return Index_Forward (
            Source,
            Pattern,
            Cast (Mapping),
            By_Func'Access);
      end Index_Forward;

      function Index_Backward (
         Source : String_Type;
         Pattern : String_Type;
         Mapping : not null access function (From : Wide_Wide_Character)
            return Wide_Wide_Character)
         return Natural
      is
         type T is access function (From : Wide_Wide_Character)
            return Wide_Wide_Character;
         function Cast is new Unchecked_Conversion (T, System.Address);
      begin
         return Index_Backward (
            Source,
            Pattern,
            Cast (Mapping),
            By_Func'Access);
      end Index_Backward;

      function Index_Element (
         Source : String_Type;
         Pattern : String_Type;
         From : Positive;
         Going : Direction := Forward;
         Mapping : not null access function (From : Character_Type)
            return Character_Type)
         return Natural is
      begin
         case Going is
            when Forward =>
               return Index_Element_Forward (
                  Source (From .. Source'Last),
                  Pattern,
                  Mapping);
            when Backward =>
               return Index_Element_Backward (
                  Source (
                     Source'First ..
                     Natural'Min (From + (Pattern'Length - 1), Source'Last)),
                  Pattern,
                  Mapping);
         end case;
      end Index_Element;

      function Index_Element (
         Source : String_Type;
         Pattern : String_Type;
         Going : Direction := Forward;
         Mapping : not null access function (From : Character_Type)
            return Character_Type)
         return Natural is
      begin
         case Going is
            when Forward =>
               return Index_Element_Forward (Source, Pattern, Mapping);
            when Backward =>
               return Index_Element_Backward (Source, Pattern, Mapping);
         end case;
      end Index_Element;

      function Index_Element_Forward (
         Source : String_Type;
         Pattern : String_Type;
         Mapping : not null access function (From : Character_Type)
            return Character_Type)
         return Natural is
      begin
         if Pattern'Length = 0 then
            raise Pattern_Error;
         else
            for I in Source'First .. Source'Last - Pattern'Length + 1 loop
               declare
                  J, P : Positive;
                  Code : Character_Type;
               begin
                  Code := Mapping (Source (I));
                  if Code = Pattern (Pattern'First) then
                     P := Pattern'First;
                     J := I;
                     loop
                        if P >= Pattern'Last then
                           return I;
                        end if;
                        P := P + 1;
                        J := J + 1;
                        Code := Mapping (Source (J));
                        exit when Code /= Pattern (P);
                     end loop;
                  end if;
               end;
            end loop;
            return 0;
         end if;
      end Index_Element_Forward;

      function Index_Element_Backward (
         Source : String_Type;
         Pattern : String_Type;
         Mapping : not null access function (From : Character_Type)
            return Character_Type)
         return Natural is
      begin
         if Pattern'Length = 0 then
            raise Pattern_Error;
         else
            for I in reverse
               Source'First + (Pattern'Length - 1) .. Source'Last
            loop
               declare
                  J, P : Natural;
                  Code : Character_Type;
               begin
                  Code := Mapping (Source (I));
                  if Code = Pattern (Pattern'Last) then
                     P := Pattern'Last;
                     J := I;
                     loop
                        if P <= Pattern'First then
                           return J;
                        end if;
                        P := P - 1;
                        J := J - 1;
                        Code := Mapping (Source (J));
                        exit when Code /= Pattern (P);
                     end loop;
                  end if;
               end;
            end loop;
            return 0;
         end if;
      end Index_Element_Backward;

      function Index (
         Source : String_Type;
         Set : Character_Set;
         From : Positive;
         Test : Membership := Inside;
         Going : Direction := Forward)
         return Natural is
      begin
         case Going is
            when Forward =>
               return Index_Forward (Source (From .. Source'Last), Set, Test);
            when Backward =>
               return Index_Backward (
                  Source (Source'First .. From),
                  Set,
                  Test);
         end case;
      end Index;

      function Index (
         Source : String_Type;
         Set : Character_Set;
         Test : Membership := Inside;
         Going : Direction := Forward)
         return Natural is
      begin
         case Going is
            when Forward =>
               return Index_Forward (Source, Set, Test);
            when Backward =>
               return Index_Backward (Source, Set, Test);
         end case;
      end Index;

      function Index_Forward (
         Source : String_Type;
         Set : Character_Set;
         Test : Membership := Inside)
         return Natural
      is
         Searched_Last : Natural := Source'First - 1;
      begin
         while Searched_Last < Source'Last loop
            declare
               Source_Index : constant Positive := Searched_Last + 1;
               Code : Wide_Wide_Character;
               Error : Boolean;
            begin
               Get (
                  Source (Source_Index .. Source'Last),
                  Searched_Last,
                  Code,
                  Error);
               if Error then
                  if Test /= Inside then -- illegal sequence is outside
                     return Source_Index;
                  end if;
               else
                  if Is_In (Code, Set) = (Test = Inside) then
                     return Source_Index;
                  end if;
               end if;
            end;
         end loop;
         return 0;
      end Index_Forward;

      function Index_Backward (
         Source : String_Type;
         Set : Character_Set;
         Test : Membership := Inside)
         return Natural
      is
         Unsearched_Last : Natural := Source'Last;
      begin
         while Unsearched_Last >= Source'First loop
            declare
               Code_First : Positive;
               Code : Wide_Wide_Character;
               Error : Boolean;
            begin
               Get_Reverse (
                  Source (Source'First .. Unsearched_Last),
                  Code_First,
                  Code,
                  Error);
               if Error then
                  if Test /= Inside then -- illegal sequence is outside
                     return Code_First;
                  end if;
               else
                  if Is_In (Code, Set) = (Test = Inside) then
                     return Code_First;
                  end if;
               end if;
               Unsearched_Last := Code_First - 1;
            end;
         end loop;
         return 0;
      end Index_Backward;

      function Count (
         Source : String_Type;
         Pattern : String_Type;
         Mapping : Character_Mapping)
         return Natural is
      begin
         return Count (Translate (Source, Mapping), Pattern);
      end Count;

      function Count (
         Source : String_Type;
         Pattern : String_Type;
         Mapping : not null access function (From : Wide_Wide_Character)
            return Wide_Wide_Character)
         return Natural is
      begin
         return Count (Translate (Source, Mapping), Pattern);
      end Count;

      function Count_Element (
         Source : String_Type;
         Pattern : String_Type;
         Mapping : not null access function (From : Character_Type)
            return Character_Type)
         return Natural
      is
         Mapped_Source : String_Type (Source'Range);
      begin
         Translate_Element (Source, Mapping, Mapped_Source);
         return Count (Mapped_Source, Pattern);
      end Count_Element;

      function Count (
         Source : String_Type;
         Set : Character_Set)
         return Natural
      is
         Last : Natural := Source'First - 1;
         Result : Natural := 0;
      begin
         while Last < Source'Last loop
            declare
               Code : Wide_Wide_Character;
               Error : Boolean;
            begin
               Get (Source (Last + 1 .. Source'Last), Last, Code, Error);
               if not Error and then Is_In (Code, Set) then
                  Result := Result + 1;
               end if;
            end;
         end loop;
         return Result;
      end Count;

      procedure Find_Token (
         Source : String_Type;
         Set : Character_Set;
         From : Positive;
         Test : Membership;
         First : out Positive;
         Last : out Natural) is
      begin
         Find_Token (Source (From .. Source'Last), Set, Test, First, Last);
      end Find_Token;

      procedure Find_Token (
         Source : String_Type;
         Set : Character_Set;
         Test : Membership;
         First : out Positive;
         Last : out Natural)
      is
         Position : constant Natural := Index_Forward (Source, Set, Test);
      begin
         if Position >= Source'First then
            First := Position;
            Last := Find_Token_Last (Source (First .. Source'Last), Set, Test);
         else
            First := Source'First;
            Last := Source'First - 1;
         end if;
      end Find_Token;

      function Find_Token_Last (
         Source : String_Type;
         Set : Character_Set;
         Test : Membership)
         return Natural
      is
         Last : Natural := Source'First - 1;
      begin
         while Last < Source'Last loop
            declare
               New_Last : Natural;
               Code : Wide_Wide_Character;
               Error : Boolean; -- ignore
            begin
               Get (Source (Last + 1 .. Source'Last), New_Last, Code, Error);
               if Error then
                  exit when Test = Inside; -- illegal sequence is outside
               else
                  exit when Is_In (Code, Set) /= (Test = Inside);
               end if;
               Last := New_Last;
            end;
         end loop;
         return Last;
      end Find_Token_Last;

      function Find_Token_First (
         Source : String_Type;
         Set : Character_Set;
         Test : Membership)
         return Positive
      is
         Unsearched_Last : constant Natural :=
            Find_Non_Token_Last (Source, Set, Test);
      begin
         if Unsearched_Last = Integer'Last then
            raise Constraint_Error;
         end if;
         return Unsearched_Last + 1;
      end Find_Token_First;

      function Translate (
         Source : String_Type;
         Mapping : Character_Mapping)
         return String_Type
      is
         Result : String_Type (1 .. Source'Length * Expanding);
         Result_Last : Natural;
      begin
         Translate (Source, Mapping, Result, Result_Last);
         return Result (1 .. Result_Last);
      end Translate;

      procedure Translate (
         Source : in out String_Type;
         Mapping : Character_Mapping;
         Drop : Truncation := Error;
         Justify : Alignment := Left;
         Pad : Character_Type := Space)
      is
         S : String_Type (1 .. Source'Length * Expanding);
         S_Last : Natural;
      begin
         Translate (Source, Mapping, S, S_Last);
         Move (S (1 .. S_Last), Source, Drop, Justify, Pad);
      end Translate;

      procedure Translate (
         Source : String_Type;
         Mapping : Character_Mapping;
         Target : out String_Type;
         Target_Last : out Natural) is
      begin
         Translate (
            Source,
            Mapping'Address,
            By_Mapping'Access,
            Target,
            Target_Last);
      end Translate;

      function Translate (
         Source : String_Type;
         Mapping : not null access function (From : Wide_Wide_Character)
            return Wide_Wide_Character)
         return String_Type
      is
         Result : String_Type (1 .. Source'Length * Expanding);
         Result_Last : Natural;
      begin
         Translate (Source, Mapping, Result, Result_Last);
         return Result (1 .. Result_Last);
      end Translate;

      procedure Translate (
         Source : in out String_Type;
         Mapping : not null access function (From : Wide_Wide_Character)
            return Wide_Wide_Character;
         Drop : Truncation := Error;
         Justify : Alignment := Left;
         Pad : Character_Type := Space)
      is
         S : String_Type (1 .. Source'Length * Expanding);
         S_Last : Natural;
      begin
         Translate (Source, Mapping, S, S_Last);
         Move (S (1 .. S_Last), Source, Drop, Justify, Pad);
      end Translate;

      procedure Translate (
         Source : String_Type;
         Mapping : not null access function (From : Wide_Wide_Character)
            return Wide_Wide_Character;
         Target : out String_Type;
         Target_Last : out Natural)
      is
         type T is access function (From : Wide_Wide_Character)
            return Wide_Wide_Character;
         function Cast is new Unchecked_Conversion (T, System.Address);
      begin
         Translate (
            Source,
            Cast (Mapping),
            By_Func'Access,
            Target,
            Target_Last);
      end Translate;

      function Translate_Element (
         Source : String_Type;
         Mapping : not null access function (From : Character_Type)
            return Character_Type)
         return String_Type is
      begin
         return Result : String_Type (1 .. Source'Length) do
            Translate_Element (Source, Mapping, Result);
         end return;
      end Translate_Element;

      procedure Translate_Element (
         Source : in out String_Type;
         Mapping : not null access function (From : Character_Type)
            return Character_Type) is
      begin
         Translate_Element (Source, Mapping, Source);
      end Translate_Element;

      procedure Translate_Element (
         Source : String_Type;
         Mapping : not null access function (From : Character_Type)
            return Character_Type;
         Target : out String_Type)
      is
         Length : constant Natural := Source'Length;
      begin
         for I in 0 .. Length - 1 loop
            Target (Target'First + I) := Mapping (Source (Source'First + I));
         end loop;
      end Translate_Element;

      function Trim (
         Source : String_Type;
         Left : Character_Set;
         Right : Character_Set)
         return String_Type
      is
         First : Positive;
         Last : Natural;
      begin
         Trim (Source, Left, Right, First, Last);
         declare
            subtype T is String_Type (1 .. Last - First + 1);
         begin
            return T (Source (First .. Last));
         end;
      end Trim;

      procedure Trim (
         Source : in out String_Type;
         Left : Character_Set;
         Right : Character_Set;
         Justify : Alignment := Strings.Left;
         Pad : Character_Type := Space)
      is
         First : Positive;
         Last : Natural;
      begin
         Trim (Source, Left, Right, First, Last);
         Move (
            Source (First .. Last),
            Source,
            Error, -- no raising because Source'Length be not growing
            Justify,
            Pad);
      end Trim;

      procedure Trim (
         Source : String_Type;
         Left : Character_Set;
         Right : Character_Set;
         First : out Positive;
         Last : out Natural)
      is
         Left_Last : constant Natural :=
            Find_Token_Last (Source, Left, Inside);
      begin
         if Left_Last = Source'Last then
            First := Source'First;
            Last := Source'Last - 1;
         else
            First := Left_Last + 1;
            Last :=
               Find_Non_Token_Last (
                  Source (First .. Source'Last),
                  Right,
                  Inside);
         end if;
      end Trim;

   end Generic_Maps;

end Ada.Strings.Generic_Functions;
