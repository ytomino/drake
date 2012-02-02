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
      Dest : Address := R;
      Source_R : Address := X;
      Dest_End : constant Address := Dest + Length;
   begin
      if ((Dest or Source_R) and Address'(Word_Mask)) = 0 then
         declare
            Dest_End_Word : constant Address := Dest_End and not Word_Mask;
         begin
            while Dest < Dest_End_Word loop
               declare
                  Result : Word;
                  for Result'Address use Dest;
                  Right : Word;
                  for Right'Address use Source_R;
               begin
                  Result := (
                     2 ** 0 or
                     2 ** Standard'Storage_Unit or
                     2 ** (Standard'Storage_Unit * 2) or
                     2 ** (Standard'Storage_Unit * 3) or
                     2 ** (Standard'Storage_Unit * 4) or
                     2 ** (Standard'Storage_Unit * 5) or
                     2 ** (Standard'Storage_Unit * 6) or
                     2 ** (Standard'Storage_Unit * 7)) xor
                     Right;
               end;
               Dest := Dest + Address'(Word_Unit);
               Source_R := Source_R + Address'(Word_Unit);
            end loop;
         end;
      end if;
      while Dest < Dest_End loop
         declare
            Result : Storage_Elements.Storage_Element;
            for Result'Address use Dest;
            Right : Storage_Elements.Storage_Element;
            for Right'Address use Source_R;
         begin
            Result := 1 xor Right;
         end;
         Dest := Dest + Address'(1);
         Source_R := Source_R + Address'(1);
      end loop;
   end Vector_Not;

   procedure Vector_And (
      R, X, Y : Address;
      Length : Storage_Elements.Storage_Count)
   is
      Dest : Address := R;
      Source_L : Address := X;
      Source_R : Address := Y;
      Dest_End : constant Address := Dest + Length;
   begin
      if ((Dest or Source_L or Source_R) and Address'(Word_Mask)) = 0 then
         declare
            Dest_End_Word : constant Address := Dest_End and not Word_Mask;
         begin
            while Dest < Dest_End_Word loop
               declare
                  Result : Word;
                  for Result'Address use Dest;
                  Left : Word;
                  for Left'Address use Source_L;
                  Right : Word;
                  for Right'Address use Source_R;
               begin
                  Result := Left and Right;
               end;
               Dest := Dest + Address'(Word_Unit);
               Source_L := Source_L + Address'(Word_Unit);
               Source_R := Source_R + Address'(Word_Unit);
            end loop;
         end;
      end if;
      while Dest < Dest_End loop
         declare
            Result : Storage_Elements.Storage_Element;
            for Result'Address use Dest;
            Left : Storage_Elements.Storage_Element;
            for Left'Address use Source_L;
            Right : Storage_Elements.Storage_Element;
            for Right'Address use Source_R;
         begin
            Result := Left and Right;
         end;
         Dest := Dest + Address'(1);
         Source_L := Source_L + Address'(1);
         Source_R := Source_R + Address'(1);
      end loop;
   end Vector_And;

   procedure Vector_Or (
      R, X, Y : Address;
      Length : Storage_Elements.Storage_Count)
   is
      Dest : Address := R;
      Source_L : Address := X;
      Source_R : Address := Y;
      Dest_End : constant Address := Dest + Length;
   begin
      if ((Dest or Source_L or Source_R) and Address'(Word_Mask)) = 0 then
         declare
            Dest_End_Word : constant Address := Dest_End and not Word_Mask;
         begin
            while Dest < Dest_End_Word loop
               declare
                  Result : Word;
                  for Result'Address use Dest;
                  Left : Word;
                  for Left'Address use Source_L;
                  Right : Word;
                  for Right'Address use Source_R;
               begin
                  Result := Left or Right;
               end;
               Dest := Dest + Address'(Word_Unit);
               Source_L := Source_L + Address'(Word_Unit);
               Source_R := Source_R + Address'(Word_Unit);
            end loop;
         end;
      end if;
      while Dest < Dest_End loop
         declare
            Result : Storage_Elements.Storage_Element;
            for Result'Address use Dest;
            Left : Storage_Elements.Storage_Element;
            for Left'Address use Source_L;
            Right : Storage_Elements.Storage_Element;
            for Right'Address use Source_R;
         begin
            Result := Left or Right;
         end;
         Dest := Dest + Address'(1);
         Source_L := Source_L + Address'(1);
         Source_R := Source_R + Address'(1);
      end loop;
   end Vector_Or;

   procedure Vector_Xor (
      R, X, Y : Address;
      Length : Storage_Elements.Storage_Count)
   is
      Dest : Address := R;
      Source_L : Address := X;
      Source_R : Address := Y;
      Dest_End : constant Address := Dest + Length;
   begin
      if ((Dest or Source_L or Source_R) and Address'(Word_Mask)) = 0 then
         declare
            Dest_End_Word : constant Address := Dest_End and not Word_Mask;
         begin
            while Dest < Dest_End_Word loop
               declare
                  Result : Word;
                  for Result'Address use Dest;
                  Left : Word;
                  for Left'Address use Source_L;
                  Right : Word;
                  for Right'Address use Source_R;
               begin
                  Result := Left xor Right;
               end;
               Dest := Dest + Address'(Word_Unit);
               Source_L := Source_L + Address'(Word_Unit);
               Source_R := Source_R + Address'(Word_Unit);
            end loop;
         end;
      end if;
      while Dest < Dest_End loop
         declare
            Result : Storage_Elements.Storage_Element;
            for Result'Address use Dest;
            Left : Storage_Elements.Storage_Element;
            for Left'Address use Source_L;
            Right : Storage_Elements.Storage_Element;
            for Right'Address use Source_R;
         begin
            Result := Left xor Right;
         end;
         Dest := Dest + Address'(1);
         Source_L := Source_L + Address'(1);
         Source_R := Source_R + Address'(1);
      end loop;
   end Vector_Xor;

end System.Boolean_Array_Operations;
