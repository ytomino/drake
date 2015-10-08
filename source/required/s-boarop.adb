package body System.Boolean_Array_Operations is
   pragma Suppress (All_Checks);
   use type Storage_Elements.Storage_Element;
   use type Storage_Elements.Storage_Offset;

   type Word is mod 2 ** Standard'Word_Size;
   Word_Unit : constant := Standard'Word_Size / Standard'Storage_Unit;
   Word_Mask : constant := Word_Unit - 1;

   pragma Compile_Time_Error (Word_Unit > 8, "should fix Vector_Not");

   --  implementation

   procedure Vector_Not (
      R, X : Address;
      Length : Storage_Elements.Storage_Count)
   is
      Dest_Addr : Address := R;
      Right_Addr : Address := X;
      Dest_End : constant Address := Dest_Addr + Length;
   begin
      if ((Dest_Addr or Right_Addr) and Word_Mask) = 0 then
         declare
            Dest_End_Word : constant Address := Dest_End and not Word_Mask;
         begin
            while Dest_Addr < Dest_End_Word loop
               declare
                  Dest : Word;
                  for Dest'Address use Dest_Addr;
                  Right : Word;
                  for Right'Address use Right_Addr;
               begin
                  Dest :=
                     (2 ** 0
                        or 2 ** Standard'Storage_Unit
                        or 2 ** (Standard'Storage_Unit * 2)
                        or 2 ** (Standard'Storage_Unit * 3)
                        or 2 ** (Standard'Storage_Unit * 4)
                        or 2 ** (Standard'Storage_Unit * 5)
                        or 2 ** (Standard'Storage_Unit * 6)
                        or 2 ** (Standard'Storage_Unit * 7))
                     xor Right;
               end;
               Dest_Addr := Dest_Addr + Address'(Word_Unit);
               Right_Addr := Right_Addr + Address'(Word_Unit);
            end loop;
         end;
      end if;
      while Dest_Addr < Dest_End loop
         declare
            Dest : Storage_Elements.Storage_Element;
            for Dest'Address use Dest_Addr;
            Right : Storage_Elements.Storage_Element;
            for Right'Address use Right_Addr;
         begin
            Dest := 1 xor Right;
         end;
         Dest_Addr := Dest_Addr + Address'(1);
         Right_Addr := Right_Addr + Address'(1);
      end loop;
   end Vector_Not;

   procedure Vector_And (
      R, X, Y : Address;
      Length : Storage_Elements.Storage_Count)
   is
      Dest_Addr : Address := R;
      Left_Addr : Address := X;
      Right_Addr : Address := Y;
      Dest_End : constant Address := Dest_Addr + Length;
   begin
      if ((Dest_Addr or Left_Addr or Right_Addr) and Word_Mask) = 0 then
         declare
            Dest_End_Word : constant Address := Dest_End and not Word_Mask;
         begin
            while Dest_Addr < Dest_End_Word loop
               declare
                  Dest : Word;
                  for Dest'Address use Dest_Addr;
                  Left : Word;
                  for Left'Address use Left_Addr;
                  Right : Word;
                  for Right'Address use Right_Addr;
               begin
                  Dest := Left and Right;
               end;
               Dest_Addr := Dest_Addr + Address'(Word_Unit);
               Left_Addr := Left_Addr + Address'(Word_Unit);
               Right_Addr := Right_Addr + Address'(Word_Unit);
            end loop;
         end;
      end if;
      while Dest_Addr < Dest_End loop
         declare
            Dest : Storage_Elements.Storage_Element;
            for Dest'Address use Dest_Addr;
            Left : Storage_Elements.Storage_Element;
            for Left'Address use Left_Addr;
            Right : Storage_Elements.Storage_Element;
            for Right'Address use Right_Addr;
         begin
            Dest := Left and Right;
         end;
         Dest_Addr := Dest_Addr + Address'(1);
         Left_Addr := Left_Addr + Address'(1);
         Right_Addr := Right_Addr + Address'(1);
      end loop;
   end Vector_And;

   procedure Vector_Or (
      R, X, Y : Address;
      Length : Storage_Elements.Storage_Count)
   is
      Dest_Addr : Address := R;
      Left_Addr : Address := X;
      Right_Addr : Address := Y;
      Dest_End : constant Address := Dest_Addr + Length;
   begin
      if ((Dest_Addr or Left_Addr or Right_Addr) and Word_Mask) = 0 then
         declare
            Dest_End_Word : constant Address := Dest_End and not Word_Mask;
         begin
            while Dest_Addr < Dest_End_Word loop
               declare
                  Dest : Word;
                  for Dest'Address use Dest_Addr;
                  Left : Word;
                  for Left'Address use Left_Addr;
                  Right : Word;
                  for Right'Address use Right_Addr;
               begin
                  Dest := Left or Right;
               end;
               Dest_Addr := Dest_Addr + Address'(Word_Unit);
               Left_Addr := Left_Addr + Address'(Word_Unit);
               Right_Addr := Right_Addr + Address'(Word_Unit);
            end loop;
         end;
      end if;
      while Dest_Addr < Dest_End loop
         declare
            Dest : Storage_Elements.Storage_Element;
            for Dest'Address use Dest_Addr;
            Left : Storage_Elements.Storage_Element;
            for Left'Address use Left_Addr;
            Right : Storage_Elements.Storage_Element;
            for Right'Address use Right_Addr;
         begin
            Dest := Left or Right;
         end;
         Dest_Addr := Dest_Addr + Address'(1);
         Left_Addr := Left_Addr + Address'(1);
         Right_Addr := Right_Addr + Address'(1);
      end loop;
   end Vector_Or;

   procedure Vector_Xor (
      R, X, Y : Address;
      Length : Storage_Elements.Storage_Count)
   is
      Dest_Addr : Address := R;
      Left_Addr : Address := X;
      Right_Addr : Address := Y;
      Dest_End : constant Address := Dest_Addr + Length;
   begin
      if ((Dest_Addr or Left_Addr or Right_Addr) and Word_Mask) = 0 then
         declare
            Dest_End_Word : constant Address := Dest_End and not Word_Mask;
         begin
            while Dest_Addr < Dest_End_Word loop
               declare
                  Dest : Word;
                  for Dest'Address use Dest_Addr;
                  Left : Word;
                  for Left'Address use Left_Addr;
                  Right : Word;
                  for Right'Address use Right_Addr;
               begin
                  Dest := Left xor Right;
               end;
               Dest_Addr := Dest_Addr + Address'(Word_Unit);
               Left_Addr := Left_Addr + Address'(Word_Unit);
               Right_Addr := Right_Addr + Address'(Word_Unit);
            end loop;
         end;
      end if;
      while Dest_Addr < Dest_End loop
         declare
            Dest : Storage_Elements.Storage_Element;
            for Dest'Address use Dest_Addr;
            Left : Storage_Elements.Storage_Element;
            for Left'Address use Left_Addr;
            Right : Storage_Elements.Storage_Element;
            for Right'Address use Right_Addr;
         begin
            Dest := Left xor Right;
         end;
         Dest_Addr := Dest_Addr + Address'(1);
         Left_Addr := Left_Addr + Address'(1);
         Right_Addr := Right_Addr + Address'(1);
      end loop;
   end Vector_Xor;

end System.Boolean_Array_Operations;
