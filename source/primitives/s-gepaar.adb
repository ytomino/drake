package body System.Generic_Packed_Arrays is

   subtype Rem_8 is Natural range 0 .. 7;

   type Record_8_Units is record
      E0, E1, E2, E3, E4, E5, E6, E7 : Bits_Type;
   end record;
   pragma Pack (Record_8_Units);
   pragma Suppress_Initialization (Record_8_Units);
   pragma Compile_Time_Error (
      Record_8_Units'Size /= Bits_Type'Size * 8
      or else Record_8_Units'Size rem Standard'Storage_Unit /= 0,
      "Is Storage_Unit not a mutiple of 8 ?");

   function Get (Arr : Address; N : Natural) return Bits_Type is
      Units : Record_8_Units;
      for Units'Address use Arr
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

   procedure Set (Arr : Address; N : Natural; E : Bits_Type) is
      Units : Record_8_Units;
      for Units'Address use Arr
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

end System.Generic_Packed_Arrays;
