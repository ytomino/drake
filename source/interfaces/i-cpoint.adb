with System.Storage_Elements;
package body Interfaces.C.Pointers is
   pragma Suppress (All_Checks);
   use type System.Storage_Elements.Storage_Offset;

   --  no System.Address_To_Access_Conversions for modifying to Pure
   function To_Pointer (Value : System.Address) return access Element
      with Import, Convention => Intrinsic;
   function To_Address (Value : access constant Element) return System.Address
      with Import, Convention => Intrinsic;

   --  implementation

   function Value (
      Ref : access constant Element;
      Terminator : Element := Default_Terminator)
      return Element_Array is
   begin
      if Ref = null then
         raise Dereference_Error; -- CXB3014
      else
         declare
            Length : constant ptrdiff_t :=
               Virtual_Length (Ref, Terminator) + 1; -- including nul
         begin
            return Value (Ref, Length);
         end;
      end if;
   end Value;

   function Value (
      Ref : access constant Element; Length : ptrdiff_t)
      return Element_Array is
   begin
      if Ref = null then
         raise Dereference_Error; -- CXB3014
      end if;
      declare
         Result : Element_Array (Index);
         for Result'Address use To_Address (Ref);
         First : Index;
         Last : Index'Base;
      begin
         if Index'First = Index'Base'First and then Length = 0 then
            First := Index'Succ (Index'First);
            Last := Index'First;
         else
            First := Index'First;
            Last := Index'Base'Val (Index'Pos (Index'First) + Length - 1);
         end if;
         return Result (First .. Last);
      end;
   end Value;

   function Virtual_Length (
      Ref : access constant Element;
      Terminator : Element := Default_Terminator)
      return ptrdiff_t is
   begin
      if Ref = null then
         raise Dereference_Error; -- CXB3016
      else
         declare
            Result : Element_Array (Index);
            for Result'Address use To_Address (Ref);
            I : Index'Base := Index'First;
         begin
            loop
               if Result (I) = Terminator then
                  return Index'Base'Pos (I) - Index'Pos (Index'First);
               end if;
               I := Index'Base'Succ (I);
            end loop;
         end;
      end if;
   end Virtual_Length;

   function Virtual_Length (
      Ref : not null access constant Element;
      Limit : ptrdiff_t;
      Terminator : Element := Default_Terminator)
      return ptrdiff_t
   is
      Source : Element_Array (Index);
      for Source'Address use To_Address (Ref);
      Result : ptrdiff_t := 0;
   begin
      while Result < Limit loop
         exit when Source (Index'Val (Index'Pos (Index'First) + Result)) =
            Terminator;
         Result := Result + 1;
      end loop;
      return Result;
   end Virtual_Length;

   procedure Copy_Terminated_Array (
      Source : access constant Element;
      Target : access Element;
      Limit : ptrdiff_t := ptrdiff_t'Last;
      Terminator : Element := Default_Terminator) is
   begin
      if Source = null or else Target = null then
         raise Dereference_Error; -- CXB3016
      end if;
      declare
         Length : ptrdiff_t;
      begin
         if Limit < ptrdiff_t'Last then
            Length := Virtual_Length (Source, Limit, Terminator);
            if Length < Limit then
               Length := Length + 1; -- including nul
            end if;
         else -- unlimited
            Length := Virtual_Length (Source, Terminator) + 1; -- including nul
         end if;
         Copy_Array (Source, Target, Length);
      end;
   end Copy_Terminated_Array;

   procedure Copy_Array (
      Source : access constant Element;
      Target : access Element;
      Length : ptrdiff_t) is
   begin
      if Source = null or else Target = null then
         raise Dereference_Error; -- CXB3016
      end if;
      if Length > 0 then
         declare
            subtype R is
               Index range
                  Index'First ..
                  Index'Val (Index'Pos (Index'First) + Length - 1);
            Source_A : Element_Array (R);
            for Source_A'Address use To_Address (Source);
            Target_A : Element_Array (R);
            for Target_A'Address use To_Address (Target);
         begin
            Target_A := Source_A;
         end;
      end if;
   end Copy_Array;

   procedure Decrement (Ref : in out Pointer) is
   begin
      Ref := Ref - 1;
   end Decrement;

   procedure Decrement (Ref : in out not null Constant_Pointer) is
   begin
      Ref := Ref - 1;
   end Decrement;

   procedure Increment (Ref : in out not null Pointer) is
   begin
      Ref := Ref + 1;
   end Increment;

   procedure Increment (Ref : in out not null Constant_Pointer) is
   begin
      Ref := Ref + 1;
   end Increment;

   function "+" (
      Left : Pointer;
      Right : ptrdiff_t)
      return not null Pointer is
   begin
      if not Standard'Fast_Math and then Left = null then
         raise Pointer_Error; -- CXB3015
      end if;
      return To_Pointer (
         To_Address (Left)
         + System.Storage_Elements.Storage_Offset (Right)
            * (Element_Array'Component_Size / Standard'Storage_Unit));
   end "+";

   function "+" (
      Left : not null Constant_Pointer;
      Right : ptrdiff_t)
      return not null Constant_Pointer is
   begin
      return To_Pointer (
         To_Address (Left)
         + System.Storage_Elements.Storage_Offset (Right)
            * (Element_Array'Component_Size / Standard'Storage_Unit));
   end "+";

   function "+" (
      Left : ptrdiff_t;
      Right : not null Pointer)
      return not null Pointer is
   begin
      return Right + Left;
   end "+";

   function "+" (
      Left : ptrdiff_t;
      Right : not null Constant_Pointer)
      return not null Constant_Pointer is
   begin
      return Right + Left;
   end "+";

   function "-" (
      Left : Pointer;
      Right : ptrdiff_t)
      return not null Pointer is
   begin
      if not Standard'Fast_Math and then Left = null then
         raise Pointer_Error; -- CXB3015
      end if;
      return To_Pointer (
         To_Address (Left)
         - System.Storage_Elements.Storage_Offset (Right)
            * (Element_Array'Component_Size / Standard'Storage_Unit));
   end "-";

   function "-" (
      Left : not null Constant_Pointer;
      Right : ptrdiff_t)
      return not null Constant_Pointer is
   begin
      return To_Pointer (
         To_Address (Left)
         - System.Storage_Elements.Storage_Offset (Right)
            * (Element_Array'Component_Size / Standard'Storage_Unit));
   end "-";

   function "-" (
      Left : not null Pointer;
      Right : not null access constant Element)
      return ptrdiff_t is
   begin
      return Constant_Pointer (Left) - Right;
   end "-";

   function "-" (
      Left : not null Constant_Pointer;
      Right : not null access constant Element)
      return ptrdiff_t is
   begin
      return ptrdiff_t (
         (To_Address (Left) - To_Address (Right))
         / (Element_Array'Component_Size / Standard'Storage_Unit));
   end "-";

end Interfaces.C.Pointers;
