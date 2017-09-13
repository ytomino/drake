package body Ada.Strings.Generic_Bounded.Generic_Functions is

   package body Generic_Bounded_Length is

      --  Copying
      procedure Tail (
         Source : Bounded.Bounded_String;
         Count : Natural;
         Pad : Character_Type := Fixed_Functions.Space;
         Drop : Truncation := Error;
         Target : out Bounded.Bounded_String);
      procedure Tail (
         Source : Bounded.Bounded_String;
         Count : Natural;
         Pad : Character_Type := Fixed_Functions.Space;
         Drop : Truncation := Error;
         Target : out Bounded.Bounded_String) is
      begin
         if Count > Bounded.Max then
            declare
               S : String_Type (1 .. Count);
               S_Last : Natural;
            begin
               Fixed_Functions.Tail (
                  Source.Element (1 .. Source.Length),
                  Count,
                  Pad,
                  Target => S,
                  Target_Last => S_Last);
               Bounded.Set_Bounded_String (Target, S (1 .. S_Last), Drop);
            end;
         else
            Fixed_Functions.Tail (
               Source.Element (1 .. Source.Length),
               Count,
               Pad,
               Target => Target.Element,
               Target_Last => Target.Length);
         end if;
      end Tail;

      --  implementation

      function Index (
         Source : Bounded.Bounded_String;
         Pattern : String_Type;
         From : Positive;
         Going : Direction := Forward)
         return Natural is
      begin
         return Fixed_Functions.Index (
            Source.Element (1 .. Source.Length),
            Pattern,
            From,
            Going);
      end Index;

      function Index (
         Source : Bounded.Bounded_String;
         Pattern : String_Type;
         Going : Direction := Forward)
         return Natural is
      begin
         return Fixed_Functions.Index (
            Source.Element (1 .. Source.Length),
            Pattern,
            Going);
      end Index;

      function Index_Non_Blank (
         Source : Bounded.Bounded_String;
         From : Positive;
         Going : Direction := Forward)
         return Natural is
      begin
         return Fixed_Functions.Index_Non_Blank (
            Source.Element (1 .. Source.Length),
            From,
            Going);
      end Index_Non_Blank;

      function Index_Non_Blank (
         Source : Bounded.Bounded_String;
         Going : Direction := Forward)
         return Natural is
      begin
         return Fixed_Functions.Index_Non_Blank (
            Source.Element (1 .. Source.Length),
            Going);
      end Index_Non_Blank;

      function Count (Source : Bounded.Bounded_String; Pattern : String_Type)
         return Natural is
      begin
         return Fixed_Functions.Count (
            Source.Element (1 .. Source.Length),
            Pattern);
      end Count;

      function Replace_Slice (
         Source : Bounded.Bounded_String;
         Low : Positive;
         High : Natural;
         By : String_Type;
         Drop : Truncation := Error)
         return Bounded.Bounded_String
      is
         pragma Check (Pre,
            Check =>
               (Low <= Source.Length + 1 and then High <= Source.Length)
               or else raise Index_Error);
      begin
         return Result : Bounded.Bounded_String do
            declare
               New_Length : constant Natural :=
                  Source.Length
                  + By'Length
                  - Integer'Max (High - Low + 1, 0);
            begin
               if New_Length > Bounded.Max then
                  declare
                     S : String_Type (1 .. New_Length);
                     S_Last : Natural;
                  begin
                     Fixed_Functions.Replace_Slice (
                        Source.Element (1 .. Source.Length),
                        Low,
                        High,
                        By,
                        Target => S,
                        Target_Last => S_Last);
                     Bounded.Set_Bounded_String (
                        Result,
                        S (1 .. S_Last),
                        Drop);
                  end;
               else
                  Fixed_Functions.Replace_Slice (
                     Source.Element (1 .. Source.Length),
                     Low,
                     High,
                     By,
                     Target => Result.Element,
                     Target_Last => Result.Length);
               end if;
            end;
         end return;
      end Replace_Slice;

      procedure Replace_Slice (
         Source : in out Bounded.Bounded_String;
         Low : Positive;
         High : Natural;
         By : String_Type;
         Drop : Truncation := Error)
      is
         pragma Check (Pre,
            Check =>
               (Low <= Source.Length + 1 and then High <= Source.Length)
               or else raise Index_Error); -- CXA4019
         New_Length : constant Natural :=
            Source.Length + By'Length - Integer'Max (High - Low + 1, 0);
      begin
         if New_Length > Bounded.Max then
            declare
               S : String_Type (1 .. New_Length);
               S_Last : Natural;
            begin
               Fixed_Functions.Replace_Slice (
                  Source.Element (1 .. Source.Length),
                  Low,
                  High,
                  By,
                  Target => S, -- copying
                  Target_Last => S_Last);
               Bounded.Set_Bounded_String (Source, S (1 .. S_Last), Drop);
            end;
         else
            Fixed_Functions.Replace_Slice (
               Source.Element,
               Source.Length,
               Low,
               High,
               By);
         end if;
      end Replace_Slice;

      function Insert (
         Source : Bounded.Bounded_String;
         Before : Positive;
         New_Item : String_Type;
         Drop : Truncation := Error)
         return Bounded.Bounded_String
      is
         pragma Check (Pre,
            Check => Before <= Source.Length + 1 or else raise Index_Error);
      begin
         return Result : Bounded.Bounded_String do
            declare
               New_Length : constant Natural :=
                  Source.Length + New_Item'Length;
            begin
               if New_Length > Bounded.Max then
                  declare
                     S : String_Type (1 .. New_Length);
                     S_Last : Natural;
                  begin
                     Fixed_Functions.Insert (
                        Source.Element (1 .. Source.Length),
                        Before,
                        New_Item,
                        Target => S,
                        Target_Last => S_Last);
                     Bounded.Set_Bounded_String (
                        Result,
                        S (1 .. S_Last),
                        Drop);
                  end;
               else
                  Fixed_Functions.Insert (
                     Source.Element (1 .. Source.Length),
                     Before,
                     New_Item,
                     Target => Result.Element,
                     Target_Last => Result.Length);
               end if;
            end;
         end return;
      end Insert;

      procedure Insert (
         Source : in out Bounded.Bounded_String;
         Before : Positive;
         New_Item : String_Type;
         Drop : Truncation := Error)
      is
         pragma Check (Pre,
            Check => Before <= Source.Length + 1 or else raise Index_Error);
         New_Length : constant Natural := Source.Length + New_Item'Length;
      begin
         if New_Length > Bounded.Max then
            declare
               S : String_Type (1 .. New_Length);
               S_Last : Natural;
            begin
               Fixed_Functions.Insert (
                  Source.Element (1 .. Source.Length),
                  Before,
                  New_Item,
                  Target => S, -- copying
                  Target_Last => S_Last);
               Bounded.Set_Bounded_String (Source, S (1 .. S_Last), Drop);
            end;
         else
            Fixed_Functions.Insert (
               Source.Element,
               Source.Length,
               Before,
               New_Item);
         end if;
      end Insert;

      function Overwrite (
         Source : Bounded.Bounded_String;
         Position : Positive;
         New_Item : String_Type;
         Drop : Truncation := Error)
         return Bounded.Bounded_String is
      begin
         return Replace_Slice (
            Source,
            Position, -- checking Index_Error
            Integer'Min (Position + New_Item'Length - 1, Source.Length),
            New_Item,
            Drop);
      end Overwrite;

      procedure Overwrite (
         Source : in out Bounded.Bounded_String;
         Position : Positive;
         New_Item : String_Type;
         Drop : Truncation := Error) is
      begin
         Replace_Slice (
            Source,
            Position, -- checking Index_Error
            Integer'Min (Position + New_Item'Length - 1, Source.Length),
            New_Item,
            Drop);
      end Overwrite;

      function Delete (
         Source : Bounded.Bounded_String;
         From : Positive;
         Through : Natural)
         return Bounded.Bounded_String
      is
         pragma Check (Pre,
            Check =>
               (From <= Source.Length + 1 and then Through <= Source.Length)
               or else raise Index_Error);
      begin
         return Result : Bounded.Bounded_String do
            Fixed_Functions.Delete (
               Source.Element (1 .. Source.Length),
               From,
               Through,
               Target => Result.Element,
               Target_Last => Result.Length);
         end return;
      end Delete;

      procedure Delete (
         Source : in out Bounded.Bounded_String;
         From : Positive;
         Through : Natural)
      is
         pragma Check (Pre,
            Check =>
               (From <= Source.Length + 1 and then Through <= Source.Length)
               or else raise Index_Error);
      begin
         Fixed_Functions.Delete (Source.Element, Source.Length, From, Through);
      end Delete;

      function Trim (
         Source : Bounded.Bounded_String;
         Side : Trim_End;
         Blank : Character_Type := Fixed_Functions.Space)
         return Bounded.Bounded_String
      is
         First : Positive;
         Last : Natural;
      begin
         Fixed_Functions.Trim (
            Source.Element (1 .. Source.Length),
            Side,
            Blank,
            First,
            Last);
         return Bounded.Bounded_Slice (Source, First, Last);
      end Trim;

      procedure Trim (
         Source : in out Bounded.Bounded_String;
         Side : Trim_End;
         Blank : Character_Type := Fixed_Functions.Space)
      is
         First : Positive;
         Last : Natural;
      begin
         Fixed_Functions.Trim (
            Source.Element (1 .. Source.Length),
            Side,
            Blank,
            First,
            Last);
         Bounded.Bounded_Slice (Source, Source, First, Last);
      end Trim;

      function Head (
         Source : Bounded.Bounded_String;
         Count : Natural;
         Pad : Character_Type := Fixed_Functions.Space;
         Drop : Truncation := Error)
         return Bounded.Bounded_String is
      begin
         return Result : Bounded.Bounded_String do
            if Count > Bounded.Max then
               declare
                  S : String_Type (1 .. Count);
                  S_Last : Natural;
               begin
                  Fixed_Functions.Head (
                     Source.Element (1 .. Source.Length),
                     Count,
                     Pad,
                     Target => S,
                     Target_Last => S_Last);
                  Bounded.Set_Bounded_String (Result, S (1 .. S_Last), Drop);
               end;
            else
               Fixed_Functions.Head (
                  Source.Element (1 .. Source.Length),
                  Count,
                  Pad,
                  Target => Result.Element,
                  Target_Last => Result.Length);
            end if;
         end return;
      end Head;

      procedure Head (
         Source : in out Bounded.Bounded_String;
         Count : Natural;
         Pad : Character_Type := Fixed_Functions.Space;
         Drop : Truncation := Error) is
      begin
         if Count > Bounded.Max then
            declare
               S : String_Type (1 .. Count);
               S_Last : Natural;
            begin
               Fixed_Functions.Head (
                  Source.Element (1 .. Source.Length),
                  Count,
                  Pad,
                  Target => S, -- copying
                  Target_Last => S_Last);
               Bounded.Set_Bounded_String (Source, S (1 .. S_Last), Drop);
            end;
         else
            Fixed_Functions.Head (Source.Element, Source.Length, Count, Pad);
         end if;
      end Head;

      function Tail (
         Source : Bounded.Bounded_String;
         Count : Natural;
         Pad : Character_Type := Fixed_Functions.Space;
         Drop : Truncation := Error)
         return Bounded.Bounded_String is
      begin
         return Result : Bounded.Bounded_String do
            Tail (Source, Count, Pad, Drop, Target => Result);
         end return;
      end Tail;

      procedure Tail (
         Source : in out Bounded.Bounded_String;
         Count : Natural;
         Pad : Character_Type := Fixed_Functions.Space;
         Drop : Truncation := Error) is
      begin
         if Count /= Source.Length then
            Tail (Source, Count, Pad, Drop, Target => Source); -- copying
         end if;
      end Tail;

   end Generic_Bounded_Length;

   package body Generic_Maps is

      package body Generic_Bounded_Length is

         function Index (
            Source : Bounded.Bounded_String;
            Pattern : String_Type;
            From : Positive;
            Going : Direction := Forward;
            Mapping : Fixed_Maps.Character_Mapping)
            return Natural is
         begin
            return Fixed_Maps.Index (
               Source.Element (1 .. Source.Length),
               Pattern,
               From,
               Going,
               Mapping);
         end Index;

         function Index (
            Source : Bounded.Bounded_String;
            Pattern : String_Type;
            Going : Direction := Forward;
            Mapping : Fixed_Maps.Character_Mapping)
            return Natural is
         begin
            return Fixed_Maps.Index (
               Source.Element (1 .. Source.Length),
               Pattern,
               Going,
               Mapping);
         end Index;

         function Index (
            Source : Bounded.Bounded_String;
            Pattern : String_Type;
            From : Positive;
            Going : Direction := Forward;
            Mapping : not null access function (From : Wide_Wide_Character)
               return Wide_Wide_Character)
            return Natural is
         begin
            return Fixed_Maps.Index (
               Source.Element (1 .. Source.Length),
               Pattern,
               From,
               Going,
               Mapping);
         end Index;

         function Index (
            Source : Bounded.Bounded_String;
            Pattern : String_Type;
            Going : Direction := Forward;
            Mapping : not null access function (From : Wide_Wide_Character)
               return Wide_Wide_Character)
            return Natural is
         begin
            return Fixed_Maps.Index (
               Source.Element (1 .. Source.Length),
               Pattern,
               Going,
               Mapping);
         end Index;

         function Index_Element (
            Source : Bounded.Bounded_String;
            Pattern : String_Type;
            From : Positive;
            Going : Direction := Forward;
            Mapping : not null access function (From : Character_Type)
               return Character_Type)
            return Natural is
         begin
            return Fixed_Maps.Index_Element (
               Source.Element (1 .. Source.Length),
               Pattern,
               From,
               Going,
               Mapping);
         end Index_Element;

         function Index_Element (
            Source : Bounded.Bounded_String;
            Pattern : String_Type;
            Going : Direction := Forward;
            Mapping : not null access function (From : Character_Type)
               return Character_Type)
            return Natural is
         begin
            return Fixed_Maps.Index_Element (
               Source.Element (1 .. Source.Length),
               Pattern,
               Going,
               Mapping);
         end Index_Element;

         function Index (
            Source : Bounded.Bounded_String;
            Set : Fixed_Maps.Character_Set;
            From : Positive;
            Test : Membership := Inside;
            Going : Direction := Forward)
            return Natural is
         begin
            return Fixed_Maps.Index (
               Source.Element (1 .. Source.Length),
               Set,
               From,
               Test,
               Going);
         end Index;

         function Index (
            Source : Bounded.Bounded_String;
            Set : Fixed_Maps.Character_Set;
            Test : Membership := Inside;
            Going : Direction := Forward)
            return Natural is
         begin
            return Fixed_Maps.Index (
               Source.Element (1 .. Source.Length),
               Set,
               Test,
               Going);
         end Index;

         function Count (
            Source : Bounded.Bounded_String;
            Pattern : String_Type;
            Mapping : Fixed_Maps.Character_Mapping)
            return Natural is
         begin
            return Fixed_Maps.Count (
               Source.Element (1 .. Source.Length),
               Pattern,
               Mapping);
         end Count;

         function Count (
            Source : Bounded.Bounded_String;
            Pattern : String_Type;
            Mapping : not null access function (From : Wide_Wide_Character)
               return Wide_Wide_Character)
            return Natural is
         begin
            return Fixed_Maps.Count (
               Source.Element (1 .. Source.Length),
               Pattern,
               Mapping);
         end Count;

         function Count_Element (
            Source : Bounded.Bounded_String;
            Pattern : String_Type;
            Mapping : not null access function (From : Character_Type)
               return Character_Type)
            return Natural is
         begin
            return Fixed_Maps.Count_Element (
               Source.Element (1 .. Source.Length),
               Pattern,
               Mapping);
         end Count_Element;

         function Count (
            Source : Bounded.Bounded_String;
            Set : Fixed_Maps.Character_Set)
            return Natural is
         begin
            return Fixed_Maps.Count (Source.Element (1 .. Source.Length), Set);
         end Count;

         procedure Find_Token (
            Source : Bounded.Bounded_String;
            Set : Fixed_Maps.Character_Set;
            From : Positive;
            Test : Membership;
            First : out Positive;
            Last : out Natural) is
         begin
            Fixed_Maps.Find_Token (
               Source.Element (1 .. Source.Length),
               Set,
               From,
               Test,
               First,
               Last);
         end Find_Token;

         procedure Find_Token (
            Source : Bounded.Bounded_String;
            Set : Fixed_Maps.Character_Set;
            Test : Membership;
            First : out Positive;
            Last : out Natural) is
         begin
            Fixed_Maps.Find_Token (
               Source.Element (1 .. Source.Length),
               Set,
               Test,
               First,
               Last);
         end Find_Token;

         function Translate (
            Source : Bounded.Bounded_String;
            Mapping : Fixed_Maps.Character_Mapping;
            Drop : Truncation := Error)
            return Bounded.Bounded_String is
         begin
            return Result : Bounded.Bounded_String do
               declare
                  Expanded_Length : constant Natural :=
                     Source.Length * Fixed_Maps.Expanding;
               begin
                  if Expanded_Length > Bounded.Max then
                     declare
                        S : String_Type (1 .. Expanded_Length);
                        S_Last : Natural;
                     begin
                        Fixed_Maps.Translate (
                           Source.Element (1 .. Source.Length),
                           Mapping,
                           Target => S,
                           Target_Last => S_Last);
                        Bounded.Set_Bounded_String (
                           Result,
                           S (1 .. S_Last),
                           Drop);
                     end;
                  else
                     Fixed_Maps.Translate (
                        Source.Element (1 .. Source.Length),
                        Mapping,
                        Target => Result.Element,
                        Target_Last => Result.Length);
                  end if;
               end;
            end return;
         end Translate;

         procedure Translate (
            Source : in out Bounded.Bounded_String;
            Mapping : Fixed_Maps.Character_Mapping;
            Drop : Truncation := Error)
         is
            --  Translate can not update destructively.
            S : String_Type (1 .. Source.Length * Fixed_Maps.Expanding);
            S_Last : Natural;
         begin
            Fixed_Maps.Translate (
               Source.Element (1 .. Source.Length),
               Mapping,
               Target => S,
               Target_Last => S_Last);
            Bounded.Set_Bounded_String (Source, S (1 .. S_Last), Drop);
         end Translate;

         function Translate (
            Source : Bounded.Bounded_String;
            Mapping : not null access function (From : Wide_Wide_Character)
               return Wide_Wide_Character;
            Drop : Truncation := Error)
            return Bounded.Bounded_String is
         begin
            return Result : Bounded.Bounded_String do
               declare
                  Expanded_Length : constant Natural :=
                     Source.Length * Fixed_Maps.Expanding;
               begin
                  if Expanded_Length > Bounded.Max then
                     declare
                        S : String_Type (1 .. Expanded_Length);
                        S_Last : Natural;
                     begin
                        Fixed_Maps.Translate (
                           Source.Element (1 .. Source.Length),
                           Mapping,
                           Target => S,
                           Target_Last => S_Last);
                        Bounded.Set_Bounded_String (
                           Result,
                           S (1 .. S_Last),
                           Drop);
                     end;
                  else
                     Fixed_Maps.Translate (
                        Source.Element (1 .. Source.Length),
                        Mapping,
                        Target => Result.Element,
                        Target_Last => Result.Length);
                  end if;
               end;
            end return;
         end Translate;

         procedure Translate (
            Source : in out Bounded.Bounded_String;
            Mapping : not null access function (From : Wide_Wide_Character)
               return Wide_Wide_Character;
            Drop : Truncation := Error)
         is
            --  Translate can not update destructively.
            S : String_Type (1 .. Source.Length * Fixed_Maps.Expanding);
            S_Last : Natural;
         begin
            Fixed_Maps.Translate (
               Source.Element (1 .. Source.Length),
               Mapping,
               Target => S,
               Target_Last => S_Last);
            Bounded.Set_Bounded_String (Source, S (1 .. S_Last), Drop);
         end Translate;

         function Translate_Element (
            Source : Bounded.Bounded_String;
            Mapping : not null access function (From : Character_Type)
               return Character_Type)
            return Bounded.Bounded_String is
         begin
            return Result : Bounded.Bounded_String := (
               Capacity => Bounded.Max,
               Length => Source.Length,
               Element => <>)
            do
               Fixed_Maps.Translate_Element (
                  Source.Element (1 .. Source.Length),
                  Mapping,
                  Target => Result.Element (1 .. Source.Length));
            end return;
         end Translate_Element;

         procedure Translate_Element (
            Source : in out Bounded.Bounded_String;
            Mapping : not null access function (From : Character_Type)
               return Character_Type) is
         begin
            Fixed_Maps.Translate_Element (
               Source.Element (1 .. Source.Length),
               Mapping);
         end Translate_Element;

         function Trim (
            Source : Bounded.Bounded_String;
            Left : Fixed_Maps.Character_Set;
            Right : Fixed_Maps.Character_Set)
            return Bounded.Bounded_String
         is
            First : Positive;
            Last : Natural;
         begin
            Fixed_Maps.Trim (
               Source.Element (1 .. Source.Length),
               Left,
               Right,
               First,
               Last);
            return Bounded.Bounded_Slice (Source, First, Last);
         end Trim;

         procedure Trim (
            Source : in out Bounded.Bounded_String;
            Left : Fixed_Maps.Character_Set;
            Right : Fixed_Maps.Character_Set)
         is
            First : Positive;
            Last : Natural;
         begin
            Fixed_Maps.Trim (
               Source.Element (1 .. Source.Length),
               Left,
               Right,
               First,
               Last);
            Bounded.Bounded_Slice (Source, Source, First, Last);
         end Trim;

      end Generic_Bounded_Length;

   end Generic_Maps;

end Ada.Strings.Generic_Bounded.Generic_Functions;
