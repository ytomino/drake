with Ada.Exceptions.Finally;
with Ada.Unchecked_Deallocation;
package body Ada.Command_Line.Generic_Parsing is
   pragma Check_Policy (Trace, Off); -- inside of package because generic

   procedure Finally (Context : not null access Argument_Context_Access);
   procedure Finally (Context : not null access Argument_Context_Access) is
      procedure Free is
         new Unchecked_Deallocation (String, String_Access);
      procedure Free is
         new Unchecked_Deallocation (
            Argument_Parsing.Argument_Iterator,
            Argument_Iterator_Access);
      procedure Free is
         new Unchecked_Deallocation (
            Argument_Context,
            Argument_Context_Access);
   begin
      pragma Check (Trace, Debug.Put ("free"));
      Free (Context.all.Argument);
      Free (Context.all.Argument_Iterator);
      Free (Context.all);
   end Finally;

   function Make_Cursor (
      Index : Input_Cursor;
      State : Argument_Parsing.State_Type)
      return Cursor;
   function Make_Cursor (
      Index : Input_Cursor;
      State : Argument_Parsing.State_Type)
      return Cursor is
   begin
      if Has_Element (Index) then
         pragma Check (Trace, Debug.Put ("allocate"));
         declare
            package Holder is
               new Exceptions.Finally.Scoped_Holder (
                  Argument_Context_Access,
                  Finally);
            Context : aliased Argument_Context_Access;
            Subindex : Argument_Parsing.Cursor;
         begin
            Holder.Assign (Context'Access);
            Context := new Argument_Context'(
               Reference_Count => 1,
               Index => Index,
               Next_Index => Index,
               Argument => <>,
               Argument_Iterator => <>);
            Context.Argument := new String'(Argument (Index));
            Context.Argument_Iterator :=
               new Argument_Parsing.Argument_Iterator'(
                  Argument_Parsing.Iterate (Context.Argument.all, State));
            Subindex := Argument_Parsing.First (Context.Argument_Iterator.all);
            if not Argument_Parsing.Has_Element (Subindex) then
               return Make_Cursor (
                  Index =>
                     Input_Iterator_Interfaces.Next (Input_Iterator, Index),
                  State =>
                     Argument_Parsing.State (Context.Argument_Iterator.all));
            else
               return Result : constant Cursor := Create (Context, Subindex) do
                  Holder.Clear;
               end return;
            end if;
         end;
      else
         return Create (null, Argument_Parsing.No_Element);
      end if;
   end Make_Cursor;

   --  implementation

   function Has_Element (Position : Cursor) return Boolean is
      Context : constant Argument_Context_Access :=
         Get_Argument_Context (Position);
   begin
      if Context = null then
         return False;
      else
         return Has_Element (Context.Index);
      end if;
   end Has_Element;

   function Iterate (Posixly_Correct : Boolean := False)
      return Iterator_Interfaces.Forward_Iterator'Class is
   begin
      return Iterator'(
         State => (Posixly_Correct => Posixly_Correct, others => <>));
   end Iterate;

   function Argument (Position : Cursor) return String is
   begin
      return Get_Argument_Context (Position).Argument.all;
   end Argument;

   function Is_Option (
      Position : Cursor;
      Name : Character;
      Option : Option_Character := ' ')
      return Boolean is
   begin
      return Argument_Parsing.Is_Option (
         Get_Argument_Context (Position).Argument.all,
         Get_Subindex (Position).all,
         Name,
         Argument_Parsing.Option_Character'Enum_Val (
            Option_Character'Enum_Rep (Option)));
   end Is_Option;

   function Is_Option (
      Position : Cursor;
      Long_Name : String;
      Option : Option_Character := ' ')
      return Boolean is
   begin
      return Argument_Parsing.Is_Option (
         Get_Argument_Context (Position).Argument.all,
         Get_Subindex (Position).all,
         Long_Name,
         Argument_Parsing.Option_Character'Enum_Val (
            Option_Character'Enum_Rep (Option)));
   end Is_Option;

   function Is_Option (
      Position : Cursor;
      Name : Character;
      Long_Name : String;
      Option : Option_Character := ' ')
      return Boolean is
   begin
      return Argument_Parsing.Is_Option (
         Get_Argument_Context (Position).Argument.all,
         Get_Subindex (Position).all,
         Name,
         Long_Name,
         Argument_Parsing.Option_Character'Enum_Val (
            Option_Character'Enum_Rep (Option)));
   end Is_Option;

   function Is_Unknown_Option (Position : Cursor) return Boolean is
   begin
      return Argument_Parsing.Is_Unknown_Option (
         Get_Argument_Context (Position).Argument.all,
         Get_Subindex (Position).all);
   end Is_Unknown_Option;

   function Name (Position : Cursor) return String is
   begin
      return Argument_Parsing.Name (
         Get_Argument_Context (Position).Argument.all,
         Get_Subindex (Position).all);
   end Name;

   function Short_Name (Position : Cursor) return Character is
   begin
      return Argument_Parsing.Short_Name (
         Get_Argument_Context (Position).Argument.all,
         Get_Subindex (Position).all);
   end Short_Name;

   function Long_Name (Position : Cursor) return String is
   begin
      return Argument_Parsing.Long_Name (
         Get_Argument_Context (Position).Argument.all,
         Get_Subindex (Position).all);
   end Long_Name;

   function Value (Position : Cursor) return String is
      Context : constant not null Argument_Context_Access :=
         Get_Argument_Context (Position);
      Subindex : constant not null access Argument_Parsing.Cursor :=
         Get_Subindex (Position);
   begin
      case Argument_Parsing.Has_Value (Context.Argument.all, Subindex.all) is
         when Argument_Parsing.None | Argument_Parsing.Same =>
            return Argument_Parsing.Value (Context.Argument.all, Subindex.all);
         when Argument_Parsing.Next =>
            if Context.Next_Index /= Context.Index then
               return Argument (Context.Next_Index);
            else
               declare
                  Next_Index : constant Input_Cursor :=
                     Input_Iterator_Interfaces.Next (
                        Input_Iterator,
                        Context.Index);
               begin
                  if Has_Element (Next_Index) then
                     Context.Next_Index := Next_Index;
                     return Argument (Next_Index);
                  else
                     return "";
                  end if;
               end;
            end if;
      end case;
   end Value;

   overriding function First (Object : Iterator) return Cursor is
      Index : constant Input_Cursor :=
         Input_Iterator_Interfaces.First (Input_Iterator);
   begin
      return Make_Cursor (Index => Index, State => Object.State);
   end First;

   overriding function Next (Object : Iterator; Position : Cursor)
      return Cursor
   is
      pragma Unreferenced (Object);
      Context : constant not null Argument_Context_Access :=
         Get_Argument_Context (Position);
      Subindex : constant Argument_Parsing.Cursor :=
         Argument_Parsing.Next (
            Context.Argument_Iterator.all,
            Get_Subindex (Position).all);
   begin
      if Argument_Parsing.Has_Element (Subindex) then
         Context.Reference_Count := Context.Reference_Count + 1;
         return Create (Context, Subindex);
      else
         declare
            Index : constant Input_Cursor :=
               Input_Iterator_Interfaces.Next (
                  Input_Iterator,
                  Context.Next_Index);
         begin
            return Make_Cursor (
               Index => Index,
               State =>
                  Argument_Parsing.State (Context.Argument_Iterator.all));
         end;
      end if;
   end Next;

   package body Cursors is

      function Create (
         Argument_Context : Argument_Context_Access;
         Subindex : Argument_Parsing.Cursor)
         return Cursor is
      begin
         return (Finalization.Controlled with Argument_Context, Subindex);
      end Create;

      function Get_Argument_Context (Position : Cursor)
         return Argument_Context_Access is
      begin
         return Position.Argument_Context;
      end Get_Argument_Context;

      function Get_Subindex (Position : Cursor)
         return not null access Argument_Parsing.Cursor is
      begin
         return Position.Subindex'Unrestricted_Access;
      end Get_Subindex;

      overriding procedure Adjust (Object : in out Cursor) is
      begin
         if Object.Argument_Context /= null then
            Object.Argument_Context.Reference_Count :=
               Object.Argument_Context.Reference_Count + 1;
         end if;
      end Adjust;

      overriding procedure Finalize (Object : in out Cursor) is
      begin
         if Object.Argument_Context /= null then
            Object.Argument_Context.Reference_Count :=
               Object.Argument_Context.Reference_Count - 1;
            if Object.Argument_Context.Reference_Count = 0 then
               Finally (Object.Argument_Context'Access);
            end if;
         end if;
      end Finalize;

   end Cursors;

end Ada.Command_Line.Generic_Parsing;
