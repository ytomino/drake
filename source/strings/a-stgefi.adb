with System.Storage_Elements;
package body Ada.Strings.Generic_Fixed is
   use type System.Address;
   use type System.Storage_Elements.Storage_Offset;

   --  gcc's builtin-function
   function memchr (
      s : System.Address;
      c : System.Storage_Elements.Storage_Element;
      n : System.Storage_Elements.Storage_Count)
      return System.Address;
   pragma Import (Intrinsic, memchr, "__builtin_memchr");

   function Index (
      Source : String_Type;
      Pattern : String_Type)
      return Natural is
   begin
      return Index (Source, Pattern, From => Source'First);
   end Index;

   function Index (
      Source : String_Type;
      Pattern : String_Type;
      From : Positive)
      return Natural is
   begin
      if Pattern'Length = 0 then
         return Source'First;
      else
         declare
            Current : Natural := From;
            Last : constant Integer := Source'Last - Pattern'Length + 1;
         begin
            while Current <= Last loop
               Current := Index (Source, Pattern (Pattern'First), Current);
               exit when Current = 0 or else Current > Last;
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
   end Index;

   function Index (
      Source : String_Type;
      Pattern : String_Type;
      From : Positive;
      Going : Backward_Direction)
      return Natural
   is
      pragma Unreferenced (Going);
   begin
      if Pattern'Length = 0 then
         return Source'First;
      else
         declare
            Current : Natural := Integer'Min (
               From,
               Source'Last - Pattern'Length + 1);
         begin
            while Current >= Source'First loop
               Current := Index (
                  Source,
                  Pattern (Pattern'First),
                  Current,
                  Backward);
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
   end Index;

   function Index (
      Source : String_Type;
      Pattern : Character_Type;
      From : Positive)
      return Natural is
   begin
      if Character_Type'Size = Character'Size then
         declare
            Offset : constant System.Storage_Elements.Storage_Offset :=
               System.Storage_Elements.Storage_Offset (From) - 1;
            Result : constant System.Address := memchr (
               Source'Address + Offset,
               Character_Type'Pos (Pattern),
               Source'Length - Offset);
         begin
            if Result = System.Null_Address then
               return 0;
            else
               return Source'First + Integer (Result - Source'Address);
            end if;
         end;
      else
         for I in From .. Source'Last loop
            if Source (I) = Pattern then
               return I;
            end if;
         end loop;
         return 0;
      end if;
   end Index;

   function Index (
      Source : String_Type;
      Pattern : Character_Type;
      From : Positive;
      Going : Backward_Direction)
      return Natural
   is
      pragma Unreferenced (Going);
   begin
      --  __builtin_memrchr does not exist...
      for I in reverse Source'First .. From loop
         if Source (I) = Pattern then
            return I;
         end if;
      end loop;
      return 0;
   end Index;

   function Trim (Source : String_Type; Side : Trim_End) return String_Type is
      First : Positive := Source'First;
      Last : Natural := Source'Last;
   begin
      case Side is
         when Left | Both =>
            while First <= Last and then Source (First) = Space loop
               First := First + 1;
            end loop;
         when Right =>
            null;
      end case;
      case Side is
         when Right | Both =>
            while Last >= First and then Source (Last) = Space loop
               Last := Last - 1;
            end loop;
         when Left =>
            null;
      end case;
      return Source (First .. Last);
   end Trim;

   function "*" (Left : Natural; Right : Character_Type)
      return String_Type is
   begin
      return (1 .. Left => Right);
   end "*";

   function "*" (Left : Natural; Right : String_Type)
      return String_Type
   is
      First : Positive := Right'First;
   begin
      return Result : String_Type (1 .. Left * Right'Length) do
         for I in 1 .. Left loop
            Result (First .. First + Right'Length - 1) := Right;
            First := First + Right'Length;
         end loop;
      end return;
   end "*";

end Ada.Strings.Generic_Fixed;
