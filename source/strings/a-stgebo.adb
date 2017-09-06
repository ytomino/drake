package body Ada.Strings.Generic_Bounded is

   type String_Access is access all String_Type;

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
      Drop : Truncation := Error)
   is
      Source_Length : constant Natural := Source'Length;
   begin
      if Source_Length > Target.Capacity then
         case Drop is
            when Right =>
               Target.Length := Target.Capacity;
               Target.Element :=
                  Source (Source'First .. Source'First + Target.Capacity - 1);
            when Left =>
               Target.Length := Target.Capacity;
               Target.Element :=
                  Source (Source'Last - Target.Capacity + 1 .. Source'Last);
            when Error =>
               raise Length_Error;
         end case;
      else
         Target.Length := Source_Length;
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
      New_Item_Length : constant Natural := New_Item'Length;
      Old_Length : constant Natural := Source.Length;
      Rest : constant Natural := Source.Capacity - Old_Length;
   begin
      if New_Item_Length > Rest then
         case Drop is
            when Right =>
               Source.Length := Source.Capacity;
               Source.Element (Old_Length + 1 .. Source.Capacity) :=
                  New_Item (New_Item'First .. New_Item'First + Rest - 1);
            when Left =>
               Source.Length := Source.Capacity;
               declare
                  Moving : constant Natural :=
                     Integer'Max (Source.Capacity - New_Item_Length, 0);
                  S : constant String_Type (1 .. Moving) :=
                     Source.Element (Old_Length - Moving + 1 .. Old_Length);
                     --  Save it before copying for Append (B, B).
               begin
                  Source.Element (Moving + 1 .. Source.Capacity) :=
                     New_Item (
                        New_Item'Last - (Source.Capacity - Moving) + 1 ..
                        New_Item'Last);
                  Source.Element (1 .. Moving) := S;
               end;
            when Error =>
               raise Length_Error;
         end case;
      else
         Source.Length := Old_Length + New_Item_Length;
         Source.Element (Old_Length + 1 .. Source.Length) := New_Item;
      end if;
   end Append;

   procedure Append_Element (
      Source : in out Bounded_String;
      New_Item : Character_Type;
      Drop : Truncation := Error) is
   begin
      Append (Source, String_Type'(1 => New_Item), Drop);
   end Append_Element;

   function Element (
      Source : Bounded_String;
      Index : Positive)
      return Character_Type
   is
      pragma Check (Pre, Index <= Source.Length or else raise Index_Error);
   begin
      return Source.Element (Index);
   end Element;

   procedure Replace_Element (
      Source : in out Bounded_String;
      Index : Positive;
      By : Character_Type)
   is
      pragma Check (Pre, Index <= Source.Length or else raise Index_Error);
   begin
      Source.Element (Index) := By;
   end Replace_Element;

   function Slice (
      Source : Bounded_String;
      Low : Positive;
      High : Natural)
      return String_Type
   is
      pragma Check (Pre,
         Check =>
            (Low <= Source.Length + 1 and then High <= Source.Length)
            or else raise Index_Error); -- CXA4034
   begin
      return Source.Element (Low .. High);
   end Slice;

   overriding function "=" (Left, Right : Bounded_String) return Boolean is
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
      Source : aliased Bounded_String)
      return Slicing.Constant_Reference_Type is
   begin
      return Slicing.Constant_Slice (
         String_Access'(Source.Element'Unrestricted_Access).all,
         1,
         Source.Length);
   end Constant_Reference;

   function Reference (
      Source : aliased in out Bounded_String)
      return Slicing.Reference_Type is
   begin
      return Slicing.Slice (
         String_Access'(Source.Element'Unrestricted_Access).all,
         1,
         Source.Length);
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
            Assign (Result, Left);
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
            Assign (Result, Left);
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

      function Append_Element (
         Left : Bounded_String;
         Right : Character_Type;
         Drop : Truncation := Error)
         return Bounded_String is
      begin
         return Result : Bounded_String do
            Assign (Result, Left);
            Append_Element (Result, Right, Drop);
         end return;
      end Append_Element;

      function Append_Element (
         Left : Character_Type;
         Right : Bounded_String;
         Drop : Truncation := Error)
         return Bounded_String is
      begin
         return Result : Bounded_String do
            Set_Bounded_String (Result, (1 => Left), Drop);
            Append (Result, Right, Drop);
         end return;
      end Append_Element;

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
         return Append_Element (Left, Right);
      end "&";

      function "&" (Left : Character_Type; Right : Bounded_String)
         return Bounded_String is
      begin
         return Append_Element (Left, Right);
      end "&";

      function Bounded_Slice (
         Source : Bounded_String;
         Low : Positive;
         High : Natural)
         return Bounded_String is
      begin
         return Result : Bounded_String do
            Bounded_Slice (Source, Result, Low, High); -- checking Index_Error
         end return;
      end Bounded_Slice;

      procedure Bounded_Slice (
         Source : Bounded_String;
         Target : out Bounded_String;
         Low : Positive;
         High : Natural)
      is
         pragma Check (Pre,
            Check =>
               (Low <= Source.Length + 1 and then High <= Source.Length)
               or else raise Index_Error);
      begin
         --  Target.Length <= Max because High <= Source.Length
         Target.Length := Integer'Max (High - Low + 1, 0);
         Target.Element (1 .. Target.Length) := Source.Element (Low .. High);
      end Bounded_Slice;

      procedure Assign (
         Target : in out Bounded_String;
         Source : Bounded_String) is
      begin
         Target.Element (1 .. Source.Length) :=
            Source.Element (1 .. Source.Length);
         Target.Length := Source.Length;
      end Assign;

      procedure Move (
         Target : in out Bounded_String;
         Source : in out Bounded_String) is
      begin
         Assign (Target, Source);
         Source.Length := 0;
      end Move;

      function "*" (Left : Natural; Right : Character_Type)
         return Bounded_String is
      begin
         return Replicate_Element (Left, Right, Error);
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

      function Replicate_Element (
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
      end Replicate_Element;

      function Replicate (
         Count : Natural;
         Item : String_Type;
         Drop : Truncation := Error)
         return Bounded_String
      is
         Item_Length : constant Natural := Item'Length;
         Total_Length : Natural := Count * Item_Length;
         Actual_Count : Natural := Count;
      begin
         if Total_Length > Max then
            if Drop = Error then
               raise Length_Error;
            end if;
            Total_Length := Max;
            Actual_Count := Total_Length / Item_Length;
         end if;
         return Result : Bounded_String :=
            (Capacity => Max, Length => Total_Length, Element => <>)
         do
            case Drop is
               when Right | Error =>
                  declare
                     Last : Natural := 0;
                  begin
                     for I in 1 .. Actual_Count loop
                        Result.Element (Last + 1 .. Last + Item_Length) :=
                           Item;
                        Last := Last + Item_Length;
                     end loop;
                     if Last < Total_Length then
                        Result.Element (Last + 1 .. Total_Length) :=
                           Item (
                              Item'First ..
                              Item'First + Total_Length - (Last + 1));
                     end if;
                  end;
               when Left =>
                  declare
                     First : Positive := Total_Length + 1;
                  begin
                     for I in 1 .. Actual_Count loop
                        Result.Element (First - Item_Length .. First - 1) :=
                           Item;
                        First := First - Item_Length;
                     end loop;
                     if First > 1 then
                        Result.Element (1 .. First - 1) :=
                           Item (Item'Last - (First - 1) + 1 .. Item'Last);
                     end if;
                  end;
            end case;
         end return;
      end Replicate;

      function Replicate (
         Count : Natural;
         Item : Bounded_String;
         Drop : Truncation := Error)
         return Bounded_String is
      begin
         return Replicate (Count, Item.Element (1 .. Item.Length), Drop);
      end Replicate;

      package body Streaming is

         procedure Read (
            Stream : not null access Streams.Root_Stream_Type'Class;
            Item : out Bounded_String)
         is
            First : Integer;
            Last : Integer;
         begin
            Integer'Read (Stream, First);
            Integer'Read (Stream, Last);
            declare
               Length : constant Integer := Last - First + 1;
            begin
               if Length > Item.Capacity then
                  raise Length_Error;
               end if;
               Item.Length := Length;
               Read (Stream, Item.Element (1 .. Length));
            end;
         end Read;

         procedure Write (
            Stream : not null access Streams.Root_Stream_Type'Class;
            Item : Bounded_String) is
         begin
            Integer'Write (Stream, 1);
            Integer'Write (Stream, Item.Length);
            Write (Stream, Item.Element (1 .. Item.Length));
         end Write;

         function Input (
            Stream : not null access Streams.Root_Stream_Type'Class)
            return Bounded_String is
         begin
            return Result : Bounded_String do
               Read (Stream, Result);
            end return;
         end Input;

      end Streaming;

   end Generic_Bounded_Length;

end Ada.Strings.Generic_Bounded;
