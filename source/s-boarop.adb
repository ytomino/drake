package body System.Boolean_Array_Operations is
   use type Storage_Elements.Storage_Element;
   use type Storage_Elements.Storage_Offset;

   type Word is mod 2 ** Standard'Word_Size;
   Word_Unit : constant := Standard'Word_Size / Standard'Storage_Unit;

   pragma Compile_Time_Error (Word_Unit > 8, "should fix Vector_Not");

   procedure Vector_Not (
      R, X : Address;
      Length : Storage_Elements.Storage_Count)
   is
      Dest : Address := R;
      Source_Right : Address := X;
      Source_Lp1 : constant Address := X + Length;
   begin
      if ((Dest or Source_Right) and
         Address'(Word_Unit - Address'(1))) = 0
      then
         declare
            Source_Lp1A : constant Address :=
               Source_Lp1 and not (Word_Unit - Address'(1));
         begin
            while Source_Right < Source_Lp1A loop
               declare
                  Result : Word;
                  for Result'Address use Dest;
                  Right : Word;
                  for Right'Address use Source_Right;
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
               Source_Right := Source_Right + Address'(Word_Unit);
            end loop;
         end;
      end if;
      while Source_Right < Source_Lp1 loop
         declare
            Result : Storage_Elements.Storage_Element;
            for Result'Address use Dest;
            Right : Storage_Elements.Storage_Element;
            for Right'Address use Source_Right;
         begin
            Result := 1 xor Right;
         end;
         Dest := Dest + Address'(1);
         Source_Right := Source_Right + Address'(1);
      end loop;
   end Vector_Not;

   procedure Vector_And (
      R, X, Y : Address;
      Length : Storage_Elements.Storage_Count)
   is
      Dest : Address := R;
      Source_Left : Address := X;
      Source_Right : Address := Y;
      Source_Lp1 : constant Address := Y + Length;
   begin
      if ((Dest or Source_Left or Source_Right) and
         Address'(Word_Unit - Address'(1))) = 0
      then
         declare
            Source_Lp1A : constant Address :=
               Source_Lp1 and not (Word_Unit - Address'(1));
         begin
            while Source_Right < Source_Lp1A loop
               declare
                  Result : Word;
                  for Result'Address use Dest;
                  Left : Word;
                  for Left'Address use Source_Left;
                  Right : Word;
                  for Right'Address use Source_Right;
               begin
                  Result := Left and Right;
               end;
               Dest := Dest + Address'(Word_Unit);
               Source_Left := Source_Left + Address'(Word_Unit);
               Source_Right := Source_Right + Address'(Word_Unit);
            end loop;
         end;
      end if;
      while Source_Right < Source_Lp1 loop
         declare
            Result : Storage_Elements.Storage_Element;
            for Result'Address use Dest;
            Left : Storage_Elements.Storage_Element;
            for Left'Address use Source_Left;
            Right : Storage_Elements.Storage_Element;
            for Right'Address use Source_Right;
         begin
            Result := Left and Right;
         end;
         Dest := Dest + Address'(1);
         Source_Left := Source_Left + Address'(1);
         Source_Right := Source_Right + Address'(1);
      end loop;
   end Vector_And;

   procedure Vector_Or (
      R, X, Y : Address;
      Length : Storage_Elements.Storage_Count)
   is
      Dest : Address := R;
      Source_Left : Address := X;
      Source_Right : Address := Y;
      Source_Lp1 : constant Address := Y + Length;
   begin
      if ((Dest or Source_Left or Source_Right) and
         Address'(Word_Unit - Address'(1))) = 0
      then
         declare
            Source_Lp1A : constant Address :=
               Source_Lp1 and not (Word_Unit - Address'(1));
         begin
            while Source_Right < Source_Lp1A loop
               declare
                  Result : Word;
                  for Result'Address use Dest;
                  Left : Word;
                  for Left'Address use Source_Left;
                  Right : Word;
                  for Right'Address use Source_Right;
               begin
                  Result := Left or Right;
               end;
               Dest := Dest + Address'(Word_Unit);
               Source_Left := Source_Left + Address'(Word_Unit);
               Source_Right := Source_Right + Address'(Word_Unit);
            end loop;
         end;
      end if;
      while Source_Right < Source_Lp1 loop
         declare
            Result : Storage_Elements.Storage_Element;
            for Result'Address use Dest;
            Left : Storage_Elements.Storage_Element;
            for Left'Address use Source_Left;
            Right : Storage_Elements.Storage_Element;
            for Right'Address use Source_Right;
         begin
            Result := Left or Right;
         end;
         Dest := Dest + Address'(1);
         Source_Left := Source_Left + Address'(1);
         Source_Right := Source_Right + Address'(1);
      end loop;
   end Vector_Or;

   procedure Vector_Xor (
      R, X, Y : Address;
      Length : Storage_Elements.Storage_Count)
   is
      Dest : Address := R;
      Source_Left : Address := X;
      Source_Right : Address := Y;
      Source_Lp1 : constant Address := Y + Length;
   begin
      if ((Dest or Source_Left or Source_Right) and
         Address'(Word_Unit - Address'(1))) = 0
      then
         declare
            Source_Lp1A : constant Address :=
               Source_Lp1 and not (Word_Unit - Address'(1));
         begin
            while Source_Right < Source_Lp1A loop
               declare
                  Result : Word;
                  for Result'Address use Dest;
                  Left : Word;
                  for Left'Address use Source_Left;
                  Right : Word;
                  for Right'Address use Source_Right;
               begin
                  Result := Left xor Right;
               end;
               Dest := Dest + Address'(Word_Unit);
               Source_Left := Source_Left + Address'(Word_Unit);
               Source_Right := Source_Right + Address'(Word_Unit);
            end loop;
         end;
      end if;
      while Source_Right < Source_Lp1 loop
         declare
            Result : Storage_Elements.Storage_Element;
            for Result'Address use Dest;
            Left : Storage_Elements.Storage_Element;
            for Left'Address use Source_Left;
            Right : Storage_Elements.Storage_Element;
            for Right'Address use Source_Right;
         begin
            Result := Left xor Right;
         end;
         Dest := Dest + Address'(1);
         Source_Left := Source_Left + Address'(1);
         Source_Right := Source_Right + Address'(1);
      end loop;
   end Vector_Xor;

end System.Boolean_Array_Operations;
