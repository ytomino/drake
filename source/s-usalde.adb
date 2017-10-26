with System.Address_To_Named_Access_Conversions;
with System.Formatting.Address;
with System.Long_Long_Integer_Types;
with System.Termination;
package body System.Unbounded_Stack_Allocators.Debug is
   pragma Suppress (All_Checks);
   use type Storage_Elements.Storage_Offset;

   subtype Word_Unsigned is Long_Long_Integer_Types.Word_Unsigned;

   package BA_Conv is
      new Address_To_Named_Access_Conversions (Block, Block_Access);

   procedure Dump (Allocator : aliased in out Allocator_Type) is
      subtype Buffer_Type is String (1 .. 256);
      procedure Put (
         Buffer : in out Buffer_Type;
         Last : in out Natural;
         S : String);
      procedure Put (
         Buffer : in out Buffer_Type;
         Last : in out Natural;
         S : String)
      is
         First : constant Natural := Last + 1;
      begin
         Last := Last + S'Length;
         Buffer (First .. Last) := S;
      end Put;
      Header_Size : constant Storage_Elements.Storage_Count :=
         Block'Size / Standard'Storage_Unit;
   begin
      Termination.Error_Put_Line ("Secondary Stack:");
      declare
         Buffer : Buffer_Type;
         Last : Natural;
         --  formatting
         Error : Boolean;
         --  index
         I : Address := Allocator;
         Block_Number : Natural := 0;
      begin
         while I /= Null_Address loop
            Last := 0;
            --  put block number
            Put (Buffer, Last, "#");
            Formatting.Image (
               Word_Unsigned (Block_Number),
               Buffer (Last + 1 .. Buffer'Last),
               Last,
               Width => 2,
               Error => Error);
            --  start
            Put (Buffer, Last, " 0x");
            Formatting.Address.Image (
               I,
               Buffer (
                  Last + 1 ..
                  Last + Formatting.Address.Address_String'Length),
               Set => Formatting.Lower_Case);
            Last := Last + Formatting.Address.Address_String'Length;
            --  used
            Put (Buffer, Last, " 0x");
            Formatting.Address.Image (
               BA_Conv.To_Pointer (I).Used,
               Buffer (
                  Last + 1 ..
                  Last + Formatting.Address.Address_String'Length),
               Set => Formatting.Lower_Case);
            Last := Last + Formatting.Address.Address_String'Length;
            --  limit
            Put (Buffer, Last, " 0x");
            Formatting.Address.Image (
               BA_Conv.To_Pointer (I).Limit,
               Buffer (
                  Last + 1 ..
                  Last + Formatting.Address.Address_String'Length),
               Set => Formatting.Lower_Case);
            Last := Last + Formatting.Address.Address_String'Length;
            --  percentage
            Put (Buffer, Last, " (");
            declare
               Space : constant Storage_Elements.Storage_Count :=
                  BA_Conv.To_Pointer (I).Limit - (I + Header_Size);
               Used : constant Storage_Elements.Storage_Count :=
                  BA_Conv.To_Pointer (I).Used - (I + Header_Size);
               Percentage : constant Storage_Elements.Storage_Count :=
                  (Used * 100 + Space - 1) / Space;
            begin
               Formatting.Image (
                  Word_Unsigned (Percentage),
                  Buffer (Last + 1 .. Buffer'Last),
                  Last,
                  Width => 3,
                  Padding => ' ',
                  Error => Error);
            end;
            Put (Buffer, Last, "%)");
            Termination.Error_Put_Line (Buffer (1 .. Last));
            --  next
            I := BA_Conv.To_Pointer (I).Previous;
            Block_Number := Block_Number + 1;
         end loop;
      end;
   end Dump;

end System.Unbounded_Stack_Allocators.Debug;
