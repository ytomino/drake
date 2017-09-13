with System.Storage_Elements;
package body Interfaces.C.Pointers is
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
      return Element_Array
   is
      pragma Check (Dynamic_Predicate,
         Check => Ref /= null or else raise Dereference_Error); -- CXB3014
      Length : constant ptrdiff_t :=
         Virtual_Length (Ref, Terminator) + 1; -- including nul
   begin
      return Value (Ref, Length);
   end Value;

   function Value (
      Ref : access constant Element;
      Length : ptrdiff_t)
      return Element_Array
   is
      pragma Check (Dynamic_Predicate,
         Check => Ref /= null or else raise Dereference_Error); -- CXB3014
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
      declare
         pragma Suppress (Alignment_Check);
         Source : Element_Array (First .. Last);
         for Source'Address use To_Address (Ref);
      begin
         return Source;
      end;
   end Value;

   function "+" (
      Left : Pointer;
      Right : ptrdiff_t)
      return not null Pointer
   is
      pragma Check (Dynamic_Predicate,
         Check => Left /= null or else raise Pointer_Error); -- CXB3015
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

   function "-" (
      Left : Pointer;
      Right : ptrdiff_t)
      return not null Pointer
   is
      pragma Check (Dynamic_Predicate,
         Check => Left /= null or else raise Pointer_Error); -- CXB3015
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

   procedure Increment (Ref : in out not null Pointer) is
   begin
      Ref := Ref + 1;
   end Increment;

   procedure Decrement (Ref : in out Pointer) is
   begin
      Ref := Ref - 1;
   end Decrement;

   function "+" (Left : not null Constant_Pointer; Right : ptrdiff_t)
      return not null Constant_Pointer is
   begin
      return To_Pointer (
         To_Address (Left)
            + System.Storage_Elements.Storage_Offset (Right)
               * (Element_Array'Component_Size / Standard'Storage_Unit));
   end "+";

   function "+" (Left : ptrdiff_t; Right : not null Constant_Pointer)
      return not null Constant_Pointer is
   begin
      return Right + Left;
   end "+";

   function "-" (Left : not null Constant_Pointer; Right : ptrdiff_t)
      return not null Constant_Pointer is
   begin
      return To_Pointer (
         To_Address (Left)
            - System.Storage_Elements.Storage_Offset (Right)
               * (Element_Array'Component_Size / Standard'Storage_Unit));
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

   procedure Increment (Ref : in out not null Constant_Pointer) is
   begin
      Ref := Ref + 1;
   end Increment;

   procedure Decrement (Ref : in out not null Constant_Pointer) is
   begin
      Ref := Ref - 1;
   end Decrement;

   function Virtual_Length (
      Ref : access constant Element;
      Terminator : Element := Default_Terminator)
      return ptrdiff_t
   is
      pragma Check (Dynamic_Predicate,
         Check => Ref /= null or else raise Dereference_Error); -- CXB3016
      Result : ptrdiff_t := 0;
   begin
      while Constant_Pointer'(Ref + Result).all /= Terminator loop
         Result := Result + 1;
      end loop;
      return Result;
   end Virtual_Length;

   function Virtual_Length (
      Ref : not null access constant Element;
      Limit : ptrdiff_t;
      Terminator : Element := Default_Terminator)
      return ptrdiff_t
   is
      Result : ptrdiff_t := 0;
   begin
      while Result < Limit
         and then Constant_Pointer'(Ref + Result).all /= Terminator
      loop
         Result := Result + 1;
      end loop;
      return Result;
   end Virtual_Length;

   procedure Copy_Terminated_Array (
      Source : access constant Element;
      Target : access Element;
      Limit : ptrdiff_t := ptrdiff_t'Last;
      Terminator : Element := Default_Terminator)
   is
      pragma Check (Dynamic_Predicate,
         Check => Source /= null or else raise Dereference_Error); -- CXB3016
      pragma Check (Dynamic_Predicate,
         Check => Target /= null or else raise Dereference_Error); -- CXB3016
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
   end Copy_Terminated_Array;

   procedure Copy_Array (
      Source : access constant Element;
      Target : access Element;
      Length : ptrdiff_t)
   is
      pragma Check (Dynamic_Predicate,
         Check => Source /= null or else raise Dereference_Error); -- CXB3016
      pragma Check (Dynamic_Predicate,
         Check => Target /= null or else raise Dereference_Error); -- CXB3016
   begin
      if Length > 0 then
         declare
            pragma Suppress (Alignment_Check);
            subtype R is
               Index range
                  Index'First ..
                  Index'Val (Index'Pos (Index'First) + Length - 1);
            Source_Array : Element_Array (R);
            for Source_Array'Address use To_Address (Source);
            Target_Array : Element_Array (R);
            for Target_Array'Address use To_Address (Target);
         begin
            Target_Array := Source_Array;
         end;
      end if;
   end Copy_Array;

end Interfaces.C.Pointers;
