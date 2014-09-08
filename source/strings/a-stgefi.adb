with Ada.Unchecked_Conversion;
with System.Address_To_Access_Conversions;
with System.Address_To_Named_Access_Conversions;
with System.Storage_Elements;
package body Ada.Strings.Generic_Fixed is
   use type System.Address;
   use type System.Storage_Elements.Storage_Offset;

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
         declare
            package Conv is
               new System.Address_To_Access_Conversions (Character);
            --  gcc's builtin-function
            procedure memset (
               b : Conv.Object_Pointer;
               c : Integer;
               n : System.Storage_Elements.Storage_Count);
            pragma Import (Intrinsic, memset, "__builtin_memset");
         begin
            memset (
               Conv.To_Pointer (Target'Address),
               Character_Type'Pos (Pad),
               Target'Length);
         end;
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
      Side : constant array (Alignment) of Trim_End := (
         Left => Right,
         Center => Both,
         Right => Left);
      Source_First : Positive;
      Source_Last : Natural;
      Target_First : Positive;
      Target_Last : Natural;
   begin
      Trim (Source, Side (Justify), Pad, Source_First, Source_Last);
      if Source_Last - Source_First + 1 > Target'Length then
         case Drop is
            when Left =>
               Source_First := Source_Last - (Target'Length - 1);
            when Right =>
               Source_Last := Source_First + (Target'Length - 1);
            when Error =>
               raise Length_Error;
         end case;
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
      Target (Target_First .. Target_Last) :=
         Source (Source_First .. Source_Last);
      Fill (Target (Target'First .. Target_First - 1), Pad);
      Fill (Target (Target_Last + 1 .. Target'Last), Pad);
   end Move;

   function Index (
      Source : String_Type;
      Pattern : Character_Type;
      From : Positive;
      Going : Direction := Forward)
      return Natural is
   begin
      case Going is
         when Forward =>
            return Index_Forward (Source (From .. Source'Last), Pattern);
         when Backward =>
            return Index_Backward (Source (Source'First .. From), Pattern);
      end case;
   end Index;

   function Index (
      Source : String_Type;
      Pattern : Character_Type;
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

   function Index_Forward (Source : String_Type; Pattern : Character_Type)
      return Natural is
   begin
      if Character_Type'Size = Standard'Storage_Unit
         and then String_Type'Component_Size = Standard'Storage_Unit
      then
         declare
            package Conv is
               new System.Address_To_Access_Conversions (Character);
            --  gcc's builtin-function
            function memchr (
               s : Conv.Object_Pointer;
               c : Integer;
               n : System.Storage_Elements.Storage_Count)
               return Conv.Object_Pointer;
            pragma Import (Intrinsic, memchr, "__builtin_memchr");
            Result : constant System.Address := Conv.To_Address (
               memchr (
                  Conv.To_Pointer (Source'Address),
                  Character_Type'Pos (Pattern),
                  Source'Length));
         begin
            if Result = System.Null_Address then
               return 0;
            else
               return Source'First + Integer (Result - Source'Address);
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
   end Index_Forward;

   function Index_Backward (Source : String_Type; Pattern : Character_Type)
      return Natural is
   begin
      --  __builtin_memrchr does not exist...
      for I in reverse Source'Range loop
         if Source (I) = Pattern then
            return I;
         end if;
      end loop;
      return 0;
   end Index_Backward;

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
            Current : Natural := Source'First;
            Last : constant Integer := Source'Last - Pattern'Length + 1;
         begin
            while Current <= Last loop
               Current := Index_Forward (
                  Source (Current .. Last),
                  Pattern (Pattern'First));
               exit when Current = 0;
               if Source (Current .. Current + Pattern'Length - 1) =
                  Pattern
               then
                  return Current;
               end if;
               Current := Current + 1;
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
            Current : Integer := Source'Last - Pattern'Length + 1;
         begin
            while Current >= Source'First loop
               Current := Index_Backward (
                  Source (Source'First .. Current),
                  Pattern (Pattern'First));
               exit when Current = 0;
               if Source (Current .. Current + Pattern'Length - 1) =
                  Pattern
               then
                  return Current;
               end if;
               Current := Current - 1;
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
      Position : Natural := Source'First;
      Result : Natural := 0;
   begin
      loop
         Position := Index_Forward (Source (Position .. Source'Last), Pattern);
         exit when Position = 0;
         Position := Position + Pattern'Length;
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
      Previous_Length : constant Integer := Low - Source'First;
      Actual_High : Natural;
   begin
      if Previous_Length < 0
         or else Low > Source'Last + 1
         or else High < Source'First - 1
         or else High > Source'Last
      then
         raise Index_Error;
      end if;
      if High < Low then
         Actual_High := Low - 1;
      else
         Actual_High := High;
      end if;
      return Result : String_Type (
         1 ..
         Source'Length - (Actual_High - Low + 1) + By'Length)
      do
         Result (1 .. Previous_Length) :=
            Source (Source'First .. Low - 1);
         Result (Previous_Length + 1 .. Previous_Length + By'Length) :=
            By;
         Result (Previous_Length + By'Length + 1 .. Result'Last) :=
            Source (Actual_High + 1 .. Source'Last);
      end return;
   end Replace_Slice;

   procedure Replace_Slice (
      Source : in out String_Type;
      Low : Positive;
      High : Natural;
      By : String_Type;
      Drop : Truncation := Error;
      Justify : Alignment := Left;
      Pad : Character_Type := Space) is
   begin
      Move (
         Replace_Slice (Source, Low, High, By),
         Source,
         Drop,
         Justify,
         Pad);
   end Replace_Slice;

   function Insert (
      Source : String_Type;
      Before : Positive;
      New_Item : String_Type)
      return String_Type
   is
      Previous_Length : constant Integer := Before - Source'First;
   begin
      if Previous_Length < 0 or else Before > Source'Last + 1 then
         raise Index_Error;
      end if;
      return Result : String_Type (1 .. Source'Length + New_Item'Length) do
         Result (1 .. Previous_Length) :=
            Source (Source'First .. Before - 1);
         Result (Previous_Length + 1 .. Previous_Length + New_Item'Length) :=
            New_Item;
         Result (Previous_Length + New_Item'Length + 1 .. Result'Last) :=
            Source (Before .. Source'Last);
      end return;
   end Insert;

   procedure Insert (
      Source : in out String_Type;
      Before : Positive;
      New_Item : String_Type;
      Drop : Truncation := Error) is
   begin
      Move (
         Insert (Source, Before, New_Item),
         Source,
         Drop,
         Justify => Left,
         Pad => Space);
   end Insert;

   function Overwrite (
      Source : String_Type;
      Position : Positive;
      New_Item : String_Type)
      return String_Type
   is
      Previous_Length : constant Integer := Position - Source'First;
   begin
      if Previous_Length < 0 or else Position > Source'Last + 1 then
         raise Index_Error;
      end if;
      return Result : String_Type (
         1 ..
         Natural'Max (Source'Length, Previous_Length + New_Item'Length))
      do
         Result (1 .. Previous_Length) :=
            Source (Source'First .. Position - 1);
         Result (Previous_Length + 1 .. Previous_Length + New_Item'Length) :=
            New_Item;
         Result (Previous_Length + New_Item'Length + 1 .. Result'Length) :=
            Source (Position + New_Item'Length .. Source'Last);
      end return;
   end Overwrite;

   procedure Overwrite (
      Source : in out String_Type;
      Position : Positive;
      New_Item : String_Type;
      Drop : Truncation := Right) is
   begin
      Move (
         Overwrite (Source, Position, New_Item),
         Source,
         Drop,
         Justify => Left,
         Pad => Space);
   end Overwrite;

   function Delete (
      Source : String_Type;
      From : Positive;
      Through : Natural)
      return String_Type is
   begin
      if From > Through then
         return Source;
      else
         return Result : String_Type (
            1 ..
            Source'Length - (Through - From + 1))
         do
            Result (1 .. From - Source'First) :=
               Source (Source'First .. From - 1);
            Result (From - Source'First + 1 .. Result'Last) :=
               Source (Through + 1 .. Source'Last);
         end return;
      end if;
   end Delete;

   procedure Delete (
      Source : in out String_Type;
      From : Positive;
      Through : Natural;
      Justify : Alignment := Left;
      Pad : Character_Type := Space)
   is
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
      Justify : Alignment := Strings.Left;
      Pad : Character_Type := Space) is
   begin
      Trim (Source, Side, Space, Justify, Pad);
   end Trim;

   procedure Trim (
      Source : in out String_Type;
      Side : Trim_End;
      Blank : Character_Type;
      Justify : Alignment := Strings.Left;
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
         when Strings.Left | Both =>
            while First <= Last and then Source (First) = Blank loop
               First := First + 1;
            end loop;
         when Strings.Right =>
            null;
      end case;
      case Side is
         when Strings.Right | Both =>
            while Last >= First and then Source (Last) = Blank loop
               Last := Last - 1;
            end loop;
         when Strings.Left =>
            null;
      end case;
   end Trim;

   function Head (
      Source : String_Type;
      Count : Natural;
      Pad : Character_Type := Space)
      return String_Type
   is
      Taking : constant Natural := Natural'Min (Source'Length, Count);
   begin
      return Result : String_Type (1 .. Count) do
         Result (1 .. Taking) :=
            Source (Source'First .. Source'First + Taking - 1);
         Fill (Result (Taking + 1 .. Count), Pad);
      end return;
   end Head;

   procedure Head (
      Source : in out String_Type;
      Count : Natural;
      Justify : Alignment := Left;
      Pad : Character_Type := Space) is
   begin
      Move (
         Head (Source, Count, Pad),
         Source,
         Error, -- no raising because Source'Length be not growing
         Justify,
         Pad);
   end Head;

   function Tail (
      Source : String_Type;
      Count : Natural;
      Pad : Character_Type := Space)
      return String_Type
   is
      Taking : constant Natural := Natural'Min (Source'Length, Count);
   begin
      return Result : String_Type (1 .. Count) do
         Result (Count - Taking + 1 .. Count) :=
            Source (Source'Last - Taking + 1 .. Source'Last);
         Fill (Result (1 .. Count - Taking), Pad);
      end return;
   end Tail;

   procedure Tail (
      Source : in out String_Type;
      Count : Natural;
      Justify : Alignment := Left;
      Pad : Character_Type := Space) is
   begin
      Move (
         Tail (Source, Count, Pad),
         Source,
         Error, -- no raising because Source'Length be not growing
         Justify,
         Pad);
   end Tail;

   function "*" (Left : Natural; Right : Character_Type)
      return String_Type is
   begin
      return (1 .. Left => Right);
   end "*";

   function "*" (Left : Natural; Right : String_Type)
      return String_Type is
   begin
      return Result : String_Type (1 .. Left * Right'Length) do
         declare
            First : Positive := Result'First;
         begin
            for I in 1 .. Left loop
               Result (First .. First + Right'Length - 1) := Right;
               First := First + Right'Length;
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
            P : Positive := Pattern'First;
         begin
            while P <= Pattern'Last loop
               Pattern_Count := Pattern_Count + 1;
               declare
                  Next : Positive;
                  Code : Wide_Wide_Character;
                  Error : Boolean; -- ignore
               begin
                  Get (Pattern (P .. Pattern'Last), Next, Code, Error);
                  P := Next + 1;
               end;
            end loop;
         end;
         while Pattern_Count > 0 and then Result < Source'Last loop
            declare
               Next : Positive;
               Code : Wide_Wide_Character;
               Error : Boolean; -- ignore
            begin
               Get (Source (Result + 1 .. Source'Last), Next, Code, Error);
               Result := Next;
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
               Buffer : String_Type (1 .. Expanding);
               Current : Natural := Source'First;
            begin
               while Current <= Source'Last loop
                  declare
                     Next : Positive;
                     J, J_Next, P, Character_Length : Positive;
                     Code : Wide_Wide_Character;
                     Error : Boolean;
                  begin
                     Get (Source (Current .. Source'Last), Next, Code, Error);
                     if Error then
                        Character_Length := Next - Current + 1;
                        Buffer (1 .. Character_Length) :=
                           Source (Current .. Next);
                     else
                        Code := Mapping (Code, Params);
                        Put (Code, Buffer, Character_Length);
                     end if;
                     P := Pattern'First + Character_Length;
                     if Buffer (1 .. Character_Length) =
                        Pattern (Pattern'First .. P - 1)
                     then
                        J_Next := Next;
                        loop
                           if P > Pattern'Last then
                              return Current;
                           end if;
                           J := J_Next + 1;
                           exit when J > Source'Last;
                           Get (
                              Source (J .. Source'Last),
                              J_Next,
                              Code,
                              Error);
                           if Error then
                              Character_Length := J_Next - J + 1;
                              Buffer (1 .. Character_Length) :=
                                 Source (J .. J_Next);
                           else
                              Code := Mapping (Code, Params);
                              Put (Code, Buffer, Character_Length);
                           end if;
                           exit when Buffer (1 .. Character_Length) /=
                              Pattern (P .. P + Character_Length - 1);
                           P := P + Character_Length;
                        end loop;
                     end if;
                     Current := Next + 1;
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
               Buffer : String_Type (1 .. Expanding);
               Current : Natural := Source'Last;
            begin
               while Current >= Source'First loop
                  declare
                     Previous : Natural;
                     J, J_Previous, P, Character_Length : Natural;
                     Code : Wide_Wide_Character;
                     Error : Boolean;
                  begin
                     Get_Reverse (
                        Source (Source'First .. Current),
                        Previous,
                        Code,
                        Error);
                     if Error then
                        Character_Length := Current - Previous + 1;
                        Buffer (1 .. Character_Length) :=
                           Source (Previous .. Current);
                     else
                        Code := Mapping (Code, Params);
                        Put (Code, Buffer, Character_Length);
                     end if;
                     P := Pattern'Last - Character_Length;
                     if Buffer (1 .. Character_Length) =
                        Pattern (P + 1 .. Pattern'Last)
                     then
                        J_Previous := Previous;
                        loop
                           if P < Pattern'First then
                              return J_Previous;
                           end if;
                           J := J_Previous - 1;
                           exit when J < Source'First;
                           Get_Reverse (
                              Source (Source'First .. J),
                              J_Previous,
                              Code,
                              Error);
                           if Error then
                              Character_Length := J - J_Previous + 1;
                              Buffer (1 .. Character_Length) :=
                                 Source (J_Previous .. J);
                           else
                              Code := Mapping (Code, Params);
                              Put (Code, Buffer, Character_Length);
                           end if;
                           exit when Buffer (1 .. Character_Length) /=
                              Pattern (P - Character_Length + 1 .. P);
                           P := P - Character_Length;
                        end loop;
                     end if;
                     Current := Previous - 1;
                  end;
               end loop;
               return 0;
            end;
         end if;
      end Index_Backward;

      function Translate (
         Source : String_Type;
         Params : System.Address;
         Mapping : not null access function (
            From : Wide_Wide_Character;
            Params : System.Address)
            return Wide_Wide_Character)
         return String_Type;
      function Translate (
         Source : String_Type;
         Params : System.Address;
         Mapping : not null access function (
            From : Wide_Wide_Character;
            Params : System.Address)
            return Wide_Wide_Character)
         return String_Type
      is
         Result : String_Type (
            1 ..
            Source'Length * Expanding);
         Last : Natural;
         I : Natural := Source'First;
         J : Natural := Result'First;
      begin
         while I <= Source'Last loop
            declare
               Code : Wide_Wide_Character;
               I_Next : Natural;
               J_Next : Natural;
               Error : Boolean;
            begin
               --  get single unicode character
               Get (Source (I .. Source'Last), I_Next, Code, Error);
               if Error then
                  --  keep illegal sequence
                  J_Next := J + (I_Next - I);
                  Result (J .. J_Next) := Source (I .. I_Next);
               else
                  --  map it
                  Code := Mapping (Code, Params);
                  --  put it
                  Put (Code, Result (J .. Result'Last), J_Next);
               end if;
               --  forwarding
               I := I_Next + 1;
               J := J_Next + 1;
            end;
         end loop;
         Last := J - 1;
         return Result (1 .. Last);
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

      function Index_Per_Element (
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
               return Index_Per_Element_Forward (
                  Source (From .. Source'Last),
                  Pattern,
                  Mapping);
            when Backward =>
               return Index_Per_Element_Backward (
                  Source (
                     Source'First ..
                     Natural'Min (From + (Pattern'Length - 1), Source'Last)),
                  Pattern,
                  Mapping);
         end case;
      end Index_Per_Element;

      function Index_Per_Element (
         Source : String_Type;
         Pattern : String_Type;
         Going : Direction := Forward;
         Mapping : not null access function (From : Character_Type)
            return Character_Type)
         return Natural is
      begin
         case Going is
            when Forward =>
               return Index_Per_Element_Forward (Source, Pattern, Mapping);
            when Backward =>
               return Index_Per_Element_Backward (Source, Pattern, Mapping);
         end case;
      end Index_Per_Element;

      function Index_Per_Element_Forward (
         Source : String_Type;
         Pattern : String_Type;
         Mapping : not null access function (From : Character_Type)
            return Character_Type)
         return Natural is
      begin
         if Pattern'Length = 0 then
            raise Pattern_Error;
         else
            for Current in
               Source'First ..
               Source'Last - Pattern'Length + 1
            loop
               declare
                  J, P : Positive;
                  Code : Character_Type;
               begin
                  Code := Mapping (Source (Current));
                  if Code = Pattern (Pattern'First) then
                     P := Pattern'First;
                     J := Current;
                     loop
                        P := P + 1;
                        if P > Pattern'Last then
                           return Current;
                        end if;
                        J := J + 1;
                        exit when J > Source'Last;
                        Code := Mapping (Source (J));
                        exit when Code /= Pattern (P);
                     end loop;
                  end if;
               end;
            end loop;
            return 0;
         end if;
      end Index_Per_Element_Forward;

      function Index_Per_Element_Backward (
         Source : String_Type;
         Pattern : String_Type;
         Mapping : not null access function (From : Character_Type)
            return Character_Type)
         return Natural is
      begin
         if Pattern'Length = 0 then
            raise Pattern_Error;
         else
            for Current in reverse
               Source'First + Pattern'Length - 1 ..
               Source'Last
            loop
               declare
                  J, P : Natural;
                  Code : Character_Type;
               begin
                  Code := Mapping (Source (Current));
                  if Code = Pattern (Pattern'Last) then
                     P := Pattern'Last;
                     J := Current;
                     loop
                        P := P - 1;
                        if P < Pattern'First then
                           return J;
                        end if;
                        J := J - 1;
                        exit when J < Source'First;
                        Code := Mapping (Source (J));
                        exit when Code /= Pattern (P);
                     end loop;
                  end if;
               end;
            end loop;
            return 0;
         end if;
      end Index_Per_Element_Backward;

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
         I : Positive := Source'First;
      begin
         while I <= Source'Last loop
            declare
               I_Next : Positive;
               Code : Wide_Wide_Character;
               Error : Boolean;
            begin
               Get (Source (I .. Source'Last), I_Next, Code, Error);
               if Error then
                  if Test /= Inside then -- illegal sequence is outside
                     return I;
                  end if;
               else
                  if Is_In (Code, Set) = (Test = Inside) then
                     return I;
                  end if;
               end if;
               I := I_Next + 1;
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
         First : Positive := Source'Last + 1;
      begin
         while First > Source'First loop
            declare
               Code : Wide_Wide_Character;
               Error : Boolean;
            begin
               Get_Reverse (
                  Source (Source'First .. First - 1),
                  First,
                  Code,
                  Error);
               if Error then
                  if Test /= Inside then -- illegal sequence is outside
                     return First;
                  end if;
               else
                  if Is_In (Code, Set) = (Test = Inside) then
                     return First;
                  end if;
               end if;
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

      function Count_Per_Element (
         Source : String_Type;
         Pattern : String_Type;
         Mapping : not null access function (From : Character_Type)
            return Character_Type)
         return Natural is
      begin
         return Count (Translate_Per_Element (Source, Mapping), Pattern);
      end Count_Per_Element;

      function Count (
         Source : String_Type;
         Set : Character_Set)
         return Natural
      is
         I : Positive := Source'First;
         Result : Natural := 0;
      begin
         while I <= Source'Last loop
            declare
               I_Next : Positive;
               Code : Wide_Wide_Character;
               Error : Boolean;
            begin
               Get (Source (I .. Source'Last), I_Next, Code, Error);
               if not Error and then Is_In (Code, Set) then
                  Result := Result + 1;
               end if;
               I := I_Next + 1;
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
         F : constant Natural := Index_Forward (Source, Set, Test);
      begin
         if F >= Source'First then
            First := F;
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
               Next : Positive;
               Code : Wide_Wide_Character;
               Error : Boolean; -- ignore
            begin
               Get (Source (Last + 1 .. Source'Last), Next, Code, Error);
               if Error then
                  exit when Test = Inside; -- illegal sequence is outside
               else
                  exit when Is_In (Code, Set) /= (Test = Inside);
               end if;
               Last := Next;
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
         First : Positive := Source'Last + 1;
      begin
         while First > Source'First loop
            declare
               Previous : Positive;
               Code : Wide_Wide_Character;
               Error : Boolean;
            begin
               Get_Reverse (
                  Source (Source'First .. First - 1),
                  Previous,
                  Code,
                  Error);
               if Error then
                  exit when Test = Inside; -- illegal sequence is outside
               else
                  exit when Is_In (Code, Set) /= (Test = Inside);
               end if;
               First := Previous;
            end;
         end loop;
         return First;
      end Find_Token_First;

      function Translate (
         Source : String_Type;
         Mapping : Character_Mapping)
         return String_Type is
      begin
         return Translate (
            Source,
            Mapping'Address,
            By_Mapping'Access);
      end Translate;

      procedure Translate (
         Source : in out String_Type;
         Mapping : Character_Mapping;
         Drop : Truncation := Error;
         Justify : Alignment := Left;
         Pad : Character_Type := Space) is
      begin
         Move (
            Translate (Source, Mapping),
            Source,
            Drop,
            Justify,
            Pad);
      end Translate;

      function Translate (
         Source : String_Type;
         Mapping : not null access function (From : Wide_Wide_Character)
            return Wide_Wide_Character)
         return String_Type
      is
         type T is access function (From : Wide_Wide_Character)
            return Wide_Wide_Character;
         function Cast is new Unchecked_Conversion (T, System.Address);
      begin
         return Translate (
            Source,
            Cast (Mapping),
            By_Func'Access);
      end Translate;

      procedure Translate (
         Source : in out String_Type;
         Mapping : not null access function (From : Wide_Wide_Character)
            return Wide_Wide_Character;
         Drop : Truncation := Error;
         Justify : Alignment := Left;
         Pad : Character_Type := Space) is
      begin
         Move (
            Translate (Source, Mapping),
            Source,
            Drop,
            Justify,
            Pad);
      end Translate;

      function Translate_Per_Element (
         Source : String_Type;
         Mapping : not null access function (From : Character_Type)
            return Character_Type)
         return String_Type
      is
         Length : constant Integer := Source'Length;
      begin
         return Result : String_Type (1 .. Length) do
            for I in 0 .. Length - 1 loop
               Result (Result'First + I) :=
                  Mapping (Source (Source'First + I));
            end loop;
         end return;
      end Translate_Per_Element;

      procedure Translate_Per_Element (
         Source : in out String_Type;
         Mapping : not null access function (From : Character_Type)
            return Character_Type) is
      begin
         for I in Source'Range loop
            Source (I) := Mapping (Source (I));
         end loop;
      end Translate_Per_Element;

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
         Last : out Natural) is
      begin
         First := Find_Token_Last (
            Source,
            Left,
            Inside) + 1;
         Last := Find_Token_First (
            Source (First .. Source'Last),
            Right,
            Inside) - 1;
      end Trim;

   end Generic_Maps;

end Ada.Strings.Generic_Fixed;
