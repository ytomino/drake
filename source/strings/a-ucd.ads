pragma License (Unrestricted);
--  extended package
package Ada.UCD is
   pragma Pure;

   type UCS_4 is mod 16#80000000#; --  same as System.UTF_Conversions.UCS_4

   type UCS_4_Array is array (Positive range <>) of UCS_4;
   for UCS_4_Array'Component_Size use 32;

   subtype UCS_2 is UCS_4 range 0 .. 16#ffff#;

   type UCS_2_Array is array (Positive range <>) of UCS_2;
   for UCS_2_Array'Component_Size use 16;

   --  set

   type Set_Element_2 is record
      Low : UCS_2;
      High : UCS_2;
   end record;
   pragma Suppress_Initialization (Set_Element_2);

   type Set_Type_2 is array (Positive range <>) of Set_Element_2;
   pragma Suppress_Initialization (Set_Type_2);

   type Set_Element_4 is record
      Low : UCS_4;
      High : UCS_4;
   end record;
   pragma Suppress_Initialization (Set_Element_4);

   type Set_Type_4 is array (Positive range <>) of Set_Element_4;
   pragma Suppress_Initialization (Set_Type_4);

   --  map

   type Map_Element_2x1 is record
      Code : UCS_2;
      Mapping : UCS_2;
   end record;
   pragma Suppress_Initialization (Map_Element_2x1);
   pragma Pack (Map_Element_2x1); --  16 + 16
   pragma Compile_Time_Error (Map_Element_2x1'Size /= 32, "packed?");
   type Map_Type_2x1 is array (Positive range <>) of Map_Element_2x1;
   pragma Pack (Map_Type_2x1);
   pragma Suppress_Initialization (Map_Type_2x1);

   type Map_Element_4x1 is record
      Code : UCS_4;
      Mapping : UCS_4;
   end record;
   pragma Pack (Map_Element_4x1); --  32 + 32
   pragma Suppress_Initialization (Map_Element_4x1);
   pragma Compile_Time_Error (Map_Element_4x1'Size /= 64, "packed?");
   type Map_Type_4x1 is array (Positive range <>) of Map_Element_4x1;
   pragma Pack (Map_Type_4x1);
   pragma Suppress_Initialization (Map_Type_4x1);

   type Map_Element_2x2 is record
      Code : UCS_2;
      Mapping : UCS_2_Array (1 .. 2);
   end record;
   pragma Pack (Map_Element_2x2); --  16 + 16 * 2
   pragma Suppress_Initialization (Map_Element_2x2);
   pragma Compile_Time_Error (Map_Element_2x2'Size /= 48, "packed?");
   type Map_Type_2x2 is array (Positive range <>) of Map_Element_2x2;
   pragma Pack (Map_Type_2x2);
   pragma Suppress_Initialization (Map_Type_2x2);

   type Map_Element_4x2 is record
      Code : UCS_4;
      Mapping : UCS_4_Array (1 .. 2);
   end record;
   pragma Pack (Map_Element_4x2); --  16 + 16 * 2
   pragma Suppress_Initialization (Map_Element_4x2);
   pragma Compile_Time_Error (Map_Element_4x2'Size /= 96, "packed?");
   type Map_Type_4x2 is array (Positive range <>) of Map_Element_4x2;
   pragma Pack (Map_Type_4x2);
   pragma Suppress_Initialization (Map_Type_4x2);

   type Map_Element_2x3 is record
      Code : UCS_2;
      Mapping : UCS_2_Array (1 .. 3);
   end record;
   pragma Pack (Map_Element_2x3); --  16 + 16 * 3
   pragma Suppress_Initialization (Map_Element_2x3);
   pragma Compile_Time_Error (Map_Element_2x3'Size /= 64, "packed?");
   type Map_Type_2x3 is array (Positive range <>) of Map_Element_2x3;
   pragma Pack (Map_Type_2x3);
   pragma Suppress_Initialization (Map_Type_2x3);

   --  class

   type Shortest_Unsigned is mod 2 ** Standard'Storage_Unit;

   type Class_Element_2 is record
      Low : UCS_2;
      High : UCS_2;
      Class : Shortest_Unsigned;
   end record;
   pragma Suppress_Initialization (Class_Element_2);
   pragma Pack (Class_Element_2); --  32 + 8
   pragma Compile_Time_Error (Class_Element_2'Size /= 40, "packed?");
   type Class_Type_2 is array (Positive range <>) of Class_Element_2;
   pragma Pack (Class_Type_2);
   pragma Suppress_Initialization (Class_Type_2);

   type Class_Element_4 is record
      Low : UCS_4;
      High : UCS_4;
      Class : Shortest_Unsigned;
   end record;
   pragma Suppress_Initialization (Class_Element_4);
   pragma Pack (Class_Element_4); --  64 + 8
   pragma Compile_Time_Error (Class_Element_4'Size /= 72, "packed?");
   type Class_Type_4 is array (Positive range <>) of Class_Element_4;
   pragma Pack (Class_Type_4);
   pragma Suppress_Initialization (Class_Type_4);

end Ada.UCD;
