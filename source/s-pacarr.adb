with System.Storage_Elements;
package body System.Packed_Arrays is
   pragma Suppress (All_Checks);

   function Compare (
      Left : Address;
      Right : Address;
      Left_Len : Natural;
      Right_Len : Natural)
      return Integer is
   begin
      if Element_Type'Size = Standard'Storage_Unit
         and then Element_Type'Enum_Rep (Element_Type'First) = 0
      then
         declare
            pragma Suppress (Range_Check);
            function memcmp (
               s1 : Address;
               s2 : Address;
               n : Storage_Elements.Storage_Count)
               return Integer;
            pragma Import (Intrinsic, memcmp, "__builtin_memcmp");
            Result : constant Integer := memcmp (
               Left,
               Right,
               Storage_Elements.Storage_Count (
                  Integer'Min (Left_Len, Right_Len)));
         begin
            if Result /= 0 then
               return Result;
            end if;
         end;
      else
         declare
            pragma Compile_Time_Error (Element_Type'Alignment /= 1,
               "misaligned");
            Min_Length : constant Integer := Integer'Min (Left_Len, Right_Len);
            type Min_Array_Type is array (1 .. Min_Length) of Element_Type;
            pragma Pack (Min_Array_Type);
            pragma Suppress_Initialization (Min_Array_Type);
            L : Min_Array_Type;
            for L'Address use Left;
            R : Min_Array_Type;
            for R'Address use Right;
         begin
            for I in 1 .. Min_Length loop
               if L (I) < R (I) then
                  return -1;
               elsif L (I) > R (I) then
                  return 1;
               end if;
            end loop;
         end;
      end if;
      if Left_Len < Right_Len then
         return -1;
      elsif Left_Len > Right_Len then
         return 1;
      else
         return 0;
      end if;
   end Compare;

   package body Indexing is

      subtype Rem_8 is Natural range 0 .. 7;

      type Record_8_Units is record
         E0, E1, E2, E3, E4, E5, E6, E7 : Element_Type;
      end record;
      pragma Pack (Record_8_Units);
      pragma Suppress_Initialization (Record_8_Units);
      pragma Compile_Time_Error (
         Record_8_Units'Size /= Element_Type'Size * 8
         or else Record_8_Units'Size rem Standard'Storage_Unit /= 0,
         "Is Storage_Unit not a mutiple of 8 ?");

      --  implementation

      function Get (Arr : Address; N : Natural) return Element_Type is
         Units : Record_8_Units;
         for Units'Address use
            Arr
            + Address (N / 8 * (Record_8_Units'Size / Standard'Storage_Unit));
      begin
         case Rem_8 (N rem (Record_8_Units'Size / Standard'Storage_Unit)) is
            when 0 => return Units.E0;
            when 1 => return Units.E1;
            when 2 => return Units.E2;
            when 3 => return Units.E3;
            when 4 => return Units.E4;
            when 5 => return Units.E5;
            when 6 => return Units.E6;
            when 7 => return Units.E7;
         end case;
      end Get;

      procedure Set (Arr : Address; N : Natural; E : Element_Type) is
         Units : Record_8_Units;
         for Units'Address use
            Arr
            + Address (N / 8 * (Record_8_Units'Size / Standard'Storage_Unit));
      begin
         case Rem_8 (N rem (Record_8_Units'Size / Standard'Storage_Unit)) is
            when 0 => Units.E0 := E;
            when 1 => Units.E1 := E;
            when 2 => Units.E2 := E;
            when 3 => Units.E3 := E;
            when 4 => Units.E4 := E;
            when 5 => Units.E5 := E;
            when 6 => Units.E6 := E;
            when 7 => Units.E7 := E;
         end case;
      end Set;

   end Indexing;

end System.Packed_Arrays;
