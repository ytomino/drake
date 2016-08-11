package body Ada.Strings.Generic_Unbounded.Generic_Functions is

   function Index (
      Source : Unbounded_String;
      Pattern : String_Type;
      From : Positive;
      Going : Direction := Forward)
      return Natural
   is
      pragma Suppress (Access_Check);
   begin
      return Fixed_Functions.Index (
         Source.Data.Items (1 .. Source.Length),
         Pattern,
         From,
         Going);
   end Index;

   function Index (
      Source : Unbounded_String;
      Pattern : String_Type;
      Going : Direction := Forward)
      return Natural
   is
      pragma Suppress (Access_Check);
   begin
      return Fixed_Functions.Index (
         Source.Data.Items (1 .. Source.Length),
         Pattern,
         Going);
   end Index;

   function Index_Non_Blank (
      Source : Unbounded_String;
      From : Positive;
      Going : Direction := Forward)
      return Natural
   is
      pragma Suppress (Access_Check);
   begin
      return Fixed_Functions.Index_Non_Blank (
         Source.Data.Items (1 .. Source.Length),
         From,
         Going);
   end Index_Non_Blank;

   function Index_Non_Blank (
      Source : Unbounded_String;
      Going : Direction := Forward)
      return Natural
   is
      pragma Suppress (Access_Check);
   begin
      return Fixed_Functions.Index_Non_Blank (
         Source.Data.Items (1 .. Source.Length),
         Going);
   end Index_Non_Blank;

   function Count (Source : Unbounded_String; Pattern : String_Type)
      return Natural
   is
      pragma Suppress (Access_Check);
   begin
      return Fixed_Functions.Count (
         Source.Data.Items (1 .. Source.Length),
         Pattern);
   end Count;

   function Replace_Slice (
      Source : Unbounded_String;
      Low : Positive;
      High : Natural;
      By : String_Type)
      return Unbounded_String
   is
      pragma Check (Pre,
         Check =>
            (Low <= Source.Length + 1 and then High <= Source.Length)
            or else raise Index_Error);
      pragma Suppress (Access_Check);
   begin
      return Result : Unbounded_String do
         if By'Length > 0 or else Low <= High then
            if By'Length = 0 and then High = Source.Length then
               Assign (Result, Source); -- shared
               Set_Length (Result, Low - 1);
            elsif Low > Source.Length then
               Assign (Result, Source); -- shared
               Append (Result, By);
            else
               Set_Length (
                  Result,
                  Source.Length + By'Length - Integer'Max (High - Low + 1, 0));
               declare
                  Dummy_Last : Natural;
               begin
                  Fixed_Functions.Replace_Slice (
                     Source.Data.Items (1 .. Source.Length),
                     Low,
                     High,
                     By,
                     Target => Result.Data.Items.all,
                     Target_Last => Dummy_Last);
               end;
            end if;
         else
            Assign (Result, Source); -- shared
         end if;
      end return;
   end Replace_Slice;

   procedure Replace_Slice (
      Source : in out Unbounded_String;
      Low : Positive;
      High : Natural;
      By : String_Type)
   is
      pragma Check (Pre,
         Check =>
            (Low <= Source.Length + 1 and then High <= Source.Length)
            or else raise Index_Error); -- CXA4032
      pragma Suppress (Access_Check);
   begin
      if By'Length > 0 or else Low <= High then
         if By'Length = 0 and then High = Source.Length then
            Set_Length (Source, Low - 1);
         elsif Low > Source.Length then
            Append (Source, By);
         else
            declare
               Old_Length : constant Natural := Source.Length;
               New_Length : Natural;
            begin
               Unique_And_Set_Length (
                  Source,
                  Old_Length
                     + Integer'Max (
                        By'Length - Integer'Max (High - Low + 1, 0),
                        0));
               New_Length := Old_Length;
               Fixed_Functions.Replace_Slice (
                  Source.Data.Items.all, -- (1 .. Source.Length)
                  New_Length,
                  Low,
                  High,
                  By);
               Set_Length (Source, New_Length);
            end;
         end if;
      end if;
   end Replace_Slice;

   function Insert (
      Source : Unbounded_String;
      Before : Positive;
      New_Item : String_Type)
      return Unbounded_String
   is
      pragma Check (Pre,
         Check => Before <= Source.Length + 1 or else raise Index_Error);
      pragma Suppress (Access_Check);
   begin
      return Result : Unbounded_String do
         if New_Item'Length > 0 then
            if Before > Source.Length then
               Assign (Result, Source); -- shared
               Append (Result, New_Item);
            else
               Set_Length (Result, Source.Length + New_Item'Length);
               declare
                  Dummy_Last : Natural;
               begin
                  Fixed_Functions.Insert (
                     Source.Data.Items (1 .. Source.Length),
                     Before,
                     New_Item,
                     Target => Result.Data.Items.all,
                     Target_Last => Dummy_Last);
               end;
            end if;
         else
            Assign (Result, Source); -- shared
         end if;
      end return;
   end Insert;

   procedure Insert (
      Source : in out Unbounded_String;
      Before : Positive;
      New_Item : String_Type)
   is
      pragma Check (Pre,
         Check =>
            Before <= Source.Length + 1
            or else raise Index_Error); -- CXA4032
      pragma Suppress (Access_Check);
   begin
      if New_Item'Length > 0 then
         if Before > Source.Length then
            Append (Source, New_Item);
         else
            declare
               Old_Length : constant Natural := Source.Length;
               New_Length : Natural;
            begin
               Unique_And_Set_Length (Source, Old_Length + New_Item'Length);
               New_Length := Old_Length;
               Fixed_Functions.Insert (
                  Source.Data.Items.all, -- (1 .. Source.Length)
                  New_Length,
                  Before,
                  New_Item);
               Set_Length (Source, New_Length);
            end;
         end if;
      end if;
   end Insert;

   function Overwrite (
      Source : Unbounded_String;
      Position : Positive;
      New_Item : String_Type)
      return Unbounded_String is
   begin
      return Replace_Slice (
         Source,
         Position, -- checking Index_Error
         Integer'Min (Position + New_Item'Length - 1, Source.Length),
         New_Item);
   end Overwrite;

   procedure Overwrite (
      Source : in out Unbounded_String;
      Position : Positive;
      New_Item : String_Type) is
   begin
      Replace_Slice (
         Source,
         Position, -- checking Index_Error, CXA4032
         Integer'Min (Position + New_Item'Length - 1, Source.Length),
         New_Item);
   end Overwrite;

   function Delete (
      Source : Unbounded_String;
      From : Positive;
      Through : Natural)
      return Unbounded_String
   is
      pragma Check (Pre,
         Check =>
            (From <= Source.Length + 1 and then Through <= Source.Length)
            or else raise Index_Error);
      pragma Suppress (Access_Check);
   begin
      return Result : Unbounded_String do
         if From <= Through then
            if Through >= Source.Length then
               Assign (Result, Source); -- shared
               Set_Length (Result, From - 1);
            else
               Set_Length (Result, Source.Length - (Through - From + 1));
               declare
                  Dummy_Last : Natural;
               begin
                  Fixed_Functions.Delete (
                     Source.Data.Items (1 .. Source.Length),
                     From,
                     Through,
                     Target => Result.Data.Items.all,
                     Target_Last => Dummy_Last);
               end;
            end if;
         else
            Assign (Result, Source); -- shared
         end if;
      end return;
   end Delete;

   procedure Delete (
      Source : in out Unbounded_String;
      From : Positive;
      Through : Natural)
   is
      pragma Check (Pre,
         Check =>
            (From <= Source.Length + 1 and then Through <= Source.Length)
            or else raise Index_Error);
      pragma Suppress (Access_Check);
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
               Fixed_Functions.Delete (
                  Source.Data.Items.all, -- (1 .. Old_Length)
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
      Blank : Character_Type := Fixed_Functions.Space)
      return Unbounded_String
   is
      pragma Suppress (Access_Check);
      First : Positive;
      Last : Natural;
   begin
      Fixed_Functions.Trim (
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
      Blank : Character_Type := Fixed_Functions.Space)
   is
      pragma Suppress (Access_Check);
      First : Positive;
      Last : Natural;
   begin
      Fixed_Functions.Trim (
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
      Pad : Character_Type := Fixed_Functions.Space)
      return Unbounded_String
   is
      pragma Suppress (Access_Check);
   begin
      return Result : Unbounded_String do
         if Count > Source.Length then
            Set_Length (Result, Count);
            declare
               Dummy_Last : Natural;
            begin
               Fixed_Functions.Head (
                  Source.Data.Items (1 .. Source.Length),
                  Count,
                  Pad,
                  Target => Result.Data.Items.all,
                  Target_Last => Dummy_Last);
            end;
         else
            Assign (Result, Source); -- shared
            Set_Length (Result, Count);
         end if;
      end return;
   end Head;

   procedure Head (
      Source : in out Unbounded_String;
      Count : Natural;
      Pad : Character_Type := Fixed_Functions.Space)
   is
      pragma Suppress (Access_Check);
   begin
      if Count > Source.Length then
         declare
            New_Last : Natural := Source.Length;
         begin
            Set_Length (Source, Count);
            Fixed_Functions.Head (
               Source.Data.Items.all, -- (1 .. Count)
               New_Last,
               Count,
               Pad);
         end;
      else
         Set_Length (Source, Count);
      end if;
   end Head;

   function Tail (
      Source : Unbounded_String;
      Count : Natural;
      Pad : Character_Type := Fixed_Functions.Space)
      return Unbounded_String
   is
      pragma Suppress (Access_Check);
   begin
      return Result : Unbounded_String do
         if Count /= Source.Length then
            Set_Length (Result, Count);
            declare
               Dummy_Last : Natural;
            begin
               Fixed_Functions.Tail (
                  Source.Data.Items (1 .. Source.Length),
                  Count,
                  Pad,
                  Target => Result.Data.Items.all,
                  Target_Last => Dummy_Last);
            end;
         else
            Assign (Result, Source); -- shared
         end if;
      end return;
   end Tail;

   procedure Tail (
      Source : in out Unbounded_String;
      Count : Natural;
      Pad : Character_Type := Fixed_Functions.Space)
   is
      pragma Suppress (Access_Check);
   begin
      if Count /= Source.Length then
         declare
            Old_Length : constant Natural := Source.Length;
            Dummy_Last : Natural;
         begin
            Unique_And_Set_Length (Source, Integer'Max (Count, Old_Length));
            Fixed_Functions.Tail (
               Source.Data.Items (1 .. Old_Length),
               Count,
               Pad,
               Target => Source.Data.Items.all, -- copying
               Target_Last => Dummy_Last);
            Set_Length (Source, Count);
         end;
      end if;
   end Tail;

   function "*" (Left : Natural; Right : Character_Type)
      return Unbounded_String
   is
      pragma Suppress (Access_Check);
   begin
      return Result : Unbounded_String do
         Set_Length (Result, Left);
         for I in 1 .. Left loop
            Result.Data.Items (I) := Right;
         end loop;
      end return;
   end "*";

   function "*" (Left : Natural; Right : String_Type)
      return Unbounded_String
   is
      pragma Suppress (Access_Check);
      Right_Length : constant Natural := Right'Length;
   begin
      return Result : Unbounded_String do
         Set_Length (Result, Left * Right_Length);
         declare
            Last : Natural := 0;
         begin
            for I in 1 .. Left loop
               Result.Data.Items (Last + 1 .. Last + Right_Length) :=
                  Right;
               Last := Last + Right_Length;
            end loop;
         end;
      end return;
   end "*";

   function "*" (Left : Natural; Right : Unbounded_String)
      return Unbounded_String
   is
      pragma Suppress (Access_Check);
   begin
      return Left * Right.Data.Items (1 .. Right.Length);
   end "*";

   package body Generic_Maps is

      function Index (
         Source : Unbounded_String;
         Pattern : String_Type;
         From : Positive;
         Going : Direction := Forward;
         Mapping : Fixed_Maps.Character_Mapping)
         return Natural
      is
         pragma Suppress (Access_Check);
      begin
         return Fixed_Maps.Index (
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
         Mapping : Fixed_Maps.Character_Mapping)
         return Natural
      is
         pragma Suppress (Access_Check);
      begin
         return Fixed_Maps.Index (
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
         return Natural
      is
         pragma Suppress (Access_Check);
      begin
         return Fixed_Maps.Index (
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
         return Natural
      is
         pragma Suppress (Access_Check);
      begin
         return Fixed_Maps.Index (
            Source.Data.Items (1 .. Source.Length),
            Pattern,
            Going,
            Mapping);
      end Index;

      function Index_Element (
         Source : Unbounded_String;
         Pattern : String_Type;
         From : Positive;
         Going : Direction := Forward;
         Mapping : not null access function (From : Character_Type)
            return Character_Type)
         return Natural
      is
         pragma Suppress (Access_Check);
      begin
         return Fixed_Maps.Index_Element (
            Source.Data.Items (1 .. Source.Length),
            Pattern,
            From,
            Going,
            Mapping);
      end Index_Element;

      function Index_Element (
         Source : Unbounded_String;
         Pattern : String_Type;
         Going : Direction := Forward;
         Mapping : not null access function (From : Character_Type)
            return Character_Type)
         return Natural
      is
         pragma Suppress (Access_Check);
      begin
         return Fixed_Maps.Index_Element (
            Source.Data.Items (1 .. Source.Length),
            Pattern,
            Going,
            Mapping);
      end Index_Element;

      function Index (
         Source : Unbounded_String;
         Set : Fixed_Maps.Character_Set;
         From : Positive;
         Test : Membership := Inside;
         Going : Direction := Forward)
         return Natural
      is
         pragma Suppress (Access_Check);
      begin
         return Fixed_Maps.Index (
            Source.Data.Items (1 .. Source.Length),
            Set,
            From,
            Test,
            Going);
      end Index;

      function Index (
         Source : Unbounded_String;
         Set : Fixed_Maps.Character_Set;
         Test : Membership := Inside;
         Going : Direction := Forward)
         return Natural
      is
         pragma Suppress (Access_Check);
      begin
         return Fixed_Maps.Index (
            Source.Data.Items (1 .. Source.Length),
            Set,
            Test,
            Going);
      end Index;

      function Count (
         Source : Unbounded_String;
         Pattern : String_Type;
         Mapping : Fixed_Maps.Character_Mapping)
         return Natural
      is
         pragma Suppress (Access_Check);
      begin
         return Fixed_Maps.Count (
            Source.Data.Items (1 .. Source.Length),
            Pattern,
            Mapping);
      end Count;

      function Count (
         Source : Unbounded_String;
         Pattern : String_Type;
         Mapping : not null access function (From : Wide_Wide_Character)
            return Wide_Wide_Character)
         return Natural
      is
         pragma Suppress (Access_Check);
      begin
         return Fixed_Maps.Count (
            Source.Data.Items (1 .. Source.Length),
            Pattern,
            Mapping);
      end Count;

      function Count_Element (
         Source : Unbounded_String;
         Pattern : String_Type;
         Mapping : not null access function (From : Character_Type)
            return Character_Type)
         return Natural
      is
         pragma Suppress (Access_Check);
      begin
         return Fixed_Maps.Count_Element (
            Source.Data.Items (1 .. Source.Length),
            Pattern,
            Mapping);
      end Count_Element;

      function Count (
         Source : Unbounded_String;
         Set : Fixed_Maps.Character_Set)
         return Natural
      is
         pragma Suppress (Access_Check);
      begin
         return Fixed_Maps.Count (Source.Data.Items (1 .. Source.Length), Set);
      end Count;

      procedure Find_Token (
         Source : Unbounded_String;
         Set : Fixed_Maps.Character_Set;
         From : Positive;
         Test : Membership;
         First : out Positive;
         Last : out Natural)
      is
         pragma Suppress (Access_Check);
      begin
         Fixed_Maps.Find_Token (
            Source.Data.Items (1 .. Source.Length),
            Set,
            From,
            Test,
            First,
            Last);
      end Find_Token;

      procedure Find_Token (
         Source : Unbounded_String;
         Set : Fixed_Maps.Character_Set;
         Test : Membership;
         First : out Positive;
         Last : out Natural)
      is
         pragma Suppress (Access_Check);
      begin
         Fixed_Maps.Find_Token (
            Source.Data.Items (1 .. Source.Length),
            Set,
            Test,
            First,
            Last);
      end Find_Token;

      function Translate (
         Source : Unbounded_String;
         Mapping : Fixed_Maps.Character_Mapping)
         return Unbounded_String
      is
         pragma Suppress (Access_Check);
      begin
         return Result : Unbounded_String do
            Set_Length (Result, Source.Length * Fixed_Maps.Expanding);
            declare
               New_Length : Natural;
            begin
               Fixed_Maps.Translate (
                  Source.Data.Items (1 .. Source.Length),
                  Mapping,
                  Target => Result.Data.Items.all,
                  Target_Last => New_Length);
               Set_Length (Result, New_Length);
            end;
         end return;
      end Translate;

      procedure Translate (
         Source : in out Unbounded_String;
         Mapping : Fixed_Maps.Character_Mapping)
      is
         pragma Suppress (Access_Check); -- finalizer
      begin
         --  Translate can not update destructively.
         Assign (Source, Translate (Source, Mapping));
      end Translate;

      function Translate (
         Source : Unbounded_String;
         Mapping : not null access function (From : Wide_Wide_Character)
            return Wide_Wide_Character)
         return Unbounded_String
      is
         pragma Suppress (Access_Check);
      begin
         return Result : Unbounded_String do
            Set_Length (Result, Source.Length * Fixed_Maps.Expanding);
            declare
               New_Length : Natural;
            begin
               Fixed_Maps.Translate (
                  Source.Data.Items (1 .. Source.Length),
                  Mapping,
                  Target => Result.Data.Items.all,
                  Target_Last => New_Length);
               Set_Length (Result, New_Length);
            end;
         end return;
      end Translate;

      procedure Translate (
         Source : in out Unbounded_String;
         Mapping : not null access function (From : Wide_Wide_Character)
            return Wide_Wide_Character)
      is
         pragma Suppress (Access_Check); -- finalizer
      begin
         --  Translate can not update destructively.
         Assign (Source, Translate (Source, Mapping));
      end Translate;

      function Translate_Element (
         Source : Unbounded_String;
         Mapping : not null access function (From : Character_Type)
            return Character_Type)
         return Unbounded_String
      is
         pragma Suppress (Access_Check);
      begin
         return Result : Unbounded_String do
            Set_Length (Result, Source.Length);
            Fixed_Maps.Translate_Element (
               Source.Data.Items (1 .. Source.Length),
               Mapping,
               Target => Result.Data.Items (1 .. Source.Length));
         end return;
      end Translate_Element;

      procedure Translate_Element (
         Source : in out Unbounded_String;
         Mapping : not null access function (From : Character_Type)
            return Character_Type)
      is
         pragma Suppress (Access_Check);
      begin
         Unique (Source);
         Fixed_Maps.Translate_Element (
            Source.Data.Items (1 .. Source.Length),
            Mapping);
      end Translate_Element;

      function Trim (
         Source : Unbounded_String;
         Left : Fixed_Maps.Character_Set;
         Right : Fixed_Maps.Character_Set)
         return Unbounded_String
      is
         pragma Suppress (Access_Check);
         First : Positive;
         Last : Natural;
      begin
         Fixed_Maps.Trim (
            Source.Data.Items (1 .. Source.Length),
            Left,
            Right,
            First,
            Last);
         return Unbounded_Slice (Source, First, Last);
      end Trim;

      procedure Trim (
         Source : in out Unbounded_String;
         Left : Fixed_Maps.Character_Set;
         Right : Fixed_Maps.Character_Set)
      is
         pragma Suppress (Access_Check);
         First : Positive;
         Last : Natural;
      begin
         Fixed_Maps.Trim (
            Source.Data.Items (1 .. Source.Length),
            Left,
            Right,
            First,
            Last);
         Unbounded_Slice (Source, Source, First, Last);
      end Trim;

   end Generic_Maps;

end Ada.Strings.Generic_Unbounded.Generic_Functions;
