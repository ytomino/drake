with System.Address_To_Named_Access_Conversions;
with System.Formatting.Address;
with System.Termination;
package body System.Unbounded_Stack_Allocators.Debug is
   pragma Suppress (All_Checks);
   use type Storage_Elements.Storage_Offset;

   package Conv is
      new Address_To_Named_Access_Conversions (Block, Block_Access);

   procedure Error_Put (Item : Address) is
      S : Formatting.Address.Address_String;
   begin
      Termination.Error_Put ("0x");
      Formatting.Address.Image (
         Item,
         S,
         Set => Formatting.Lower_Case);
      Termination.Error_Put (S);
   end Error_Put;

   procedure Dump (Allocator : aliased in out Allocator_Type) is
      Header_Size : constant Storage_Elements.Storage_Count :=
         Block'Size / Standard'Storage_Unit;
   begin
      Termination.Error_Put ("Secondary Stack:");
      Termination.Error_New_Line;
      declare
         --  formatting
         S : String (1 .. 10);
         Last : Natural;
         Error : Boolean;
         --  index
         I : Address := Allocator;
         Block_Number : Natural := 0;
      begin
         while I /= Null_Address loop
            --  put block number
            Termination.Error_Put ("#");
            Formatting.Image (
               Formatting.Unsigned (Block_Number),
               S,
               Last,
               Width => 2,
               Error => Error);
            Termination.Error_Put (S (1 .. Last));
            Termination.Error_Put (" ");
            --  start
            Error_Put (I);
            --  used
            Termination.Error_Put (" ");
            Error_Put (Conv.To_Pointer (I).Used);
            --  limit
            Termination.Error_Put (" ");
            Error_Put (Conv.To_Pointer (I).Limit);
            --  percentage
            Termination.Error_Put (" (");
            declare
               Space : constant Storage_Elements.Storage_Count :=
                  Conv.To_Pointer (I).Limit - (I + Header_Size);
               Used : constant Storage_Elements.Storage_Count :=
                  Conv.To_Pointer (I).Used - (I + Header_Size);
               Percentage : constant Storage_Elements.Storage_Count :=
                  (Used * 100 + Space - 1) / Space;
            begin
               Formatting.Image (
                  Formatting.Unsigned (Percentage),
                  S,
                  Last,
                  Width => 3,
                  Padding => ' ',
                  Error => Error);
               Termination.Error_Put (S (1 .. Last));
            end;
            Termination.Error_Put ("%)");
            Termination.Error_New_Line;
            --  next
            I := Conv.To_Pointer (I).Previous;
            Block_Number := Block_Number + 1;
         end loop;
      end;
   end Dump;

end System.Unbounded_Stack_Allocators.Debug;
