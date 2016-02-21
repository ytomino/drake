with Ada.Exceptions.Finally;
with Ada.Unchecked_Deallocation;
package body Ada.Command_Line.Generic_Parsing is
   pragma Check_Policy (Trace => Ignore);

   procedure Retain (Context : not null Argument_Context_Access);
   procedure Retain (Context : not null Argument_Context_Access) is
   begin
      Context.Reference_Count := Context.Reference_Count + 1;
   end Retain;

   procedure Release (Context : in out Argument_Context_Access);
   procedure Release (Context : in out Argument_Context_Access) is
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
      Context.Reference_Count := Context.Reference_Count - 1;
      if Context.Reference_Count <= 0 then
         pragma Check (Trace, Debug.Put ("deallocate"));
         Free (Context.Argument);
         Free (Context.Argument_Iterator);
         Free (Context);
      end if;
   end Release;

   function Make_Cursor (
      Index : Input_Cursor;
      State : Argument_Parsing.State_Type)
      return Cursor;
   function Make_Cursor (
      Index : Input_Cursor;
      State : Argument_Parsing.State_Type)
      return Cursor
   is
      Next_Index : Input_Cursor := Index;
      Next_State : Argument_Parsing.State_Type := State;
   begin
      while Has_Element (Next_Index) loop
         pragma Check (Trace, Debug.Put ("allocate"));
         declare
            package Holder is
               new Exceptions.Finally.Scoped_Holder (
                  Argument_Context_Access,
                  Release);
            Context : aliased Argument_Context_Access;
            Subindex : Argument_Parsing.Cursor;
         begin
            Holder.Assign (Context);
            Context := new Argument_Context'(
               Reference_Count => 1,
               Index => Next_Index,
               Next_Index => Next_Index,
               Argument => <>,
               Argument_Iterator => <>);
            Context.Argument := new String'(Argument (Next_Index));
            Context.Argument_Iterator :=
               new Argument_Parsing.Argument_Iterator'(
                  Argument_Parsing.Iterate (Context.Argument.all, Next_State));
            Subindex := Argument_Parsing.First (Context.Argument_Iterator.all);
            if Argument_Parsing.Has_Element (Subindex) then
               return Create (Context, Subindex);
            end if;
            Next_Index :=
               Input_Iterator_Interfaces.Next (Input_Iterator, Next_Index);
            Next_State :=
               Argument_Parsing.State (Context.Argument_Iterator.all);
         end;
      end loop;
      return Create (null, Argument_Parsing.No_Element);
   end Make_Cursor;

   --  implementation

   function Has_Element (Position : Cursor) return Boolean is
      NC_Position : Non_Controlled_Cursor
         renames Controlled.Reference (Position).all;
      Context : constant Argument_Context_Access :=
         NC_Position.Argument_Context;
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
      NC_Position : Non_Controlled_Cursor
         renames Controlled.Reference (Position).all;
   begin
      return NC_Position.Argument_Context.Argument.all;
   end Argument;

   function Is_Option (
      Position : Cursor;
      Name : Character;
      Option : Option_Character := ' ')
      return Boolean
   is
      NC_Position : Non_Controlled_Cursor
         renames Controlled.Reference (Position).all;
   begin
      return Argument_Parsing.Is_Option (
         NC_Position.Argument_Context.Argument.all,
         NC_Position.Subindex,
         Name,
         Argument_Parsing.Option_Character'Enum_Val (
            Option_Character'Enum_Rep (Option)));
   end Is_Option;

   function Is_Option (
      Position : Cursor;
      Long_Name : String;
      Option : Option_Character := ' ')
      return Boolean
   is
      NC_Position : Non_Controlled_Cursor
         renames Controlled.Reference (Position).all;
   begin
      return Argument_Parsing.Is_Option (
         NC_Position.Argument_Context.Argument.all,
         NC_Position.Subindex,
         Long_Name,
         Argument_Parsing.Option_Character'Enum_Val (
            Option_Character'Enum_Rep (Option)));
   end Is_Option;

   function Is_Option (
      Position : Cursor;
      Name : Character;
      Long_Name : String;
      Option : Option_Character := ' ')
      return Boolean
   is
      NC_Position : Non_Controlled_Cursor
         renames Controlled.Reference (Position).all;
   begin
      return Argument_Parsing.Is_Option (
         NC_Position.Argument_Context.Argument.all,
         NC_Position.Subindex,
         Name,
         Long_Name,
         Argument_Parsing.Option_Character'Enum_Val (
            Option_Character'Enum_Rep (Option)));
   end Is_Option;

   function Is_Unknown_Option (Position : Cursor) return Boolean is
      NC_Position : Non_Controlled_Cursor
         renames Controlled.Reference (Position).all;
   begin
      return Argument_Parsing.Is_Unknown_Option (
         NC_Position.Argument_Context.Argument.all,
         NC_Position.Subindex);
   end Is_Unknown_Option;

   function Name (Position : Cursor) return String is
      NC_Position : Non_Controlled_Cursor
         renames Controlled.Reference (Position).all;
   begin
      return Argument_Parsing.Name (
         NC_Position.Argument_Context.Argument.all,
         NC_Position.Subindex);
   end Name;

   function Short_Name (Position : Cursor) return Character is
      NC_Position : Non_Controlled_Cursor
         renames Controlled.Reference (Position).all;
   begin
      return Argument_Parsing.Short_Name (
         NC_Position.Argument_Context.Argument.all,
         NC_Position.Subindex);
   end Short_Name;

   function Long_Name (Position : Cursor) return String is
      NC_Position : Non_Controlled_Cursor
         renames Controlled.Reference (Position).all;
   begin
      return Argument_Parsing.Long_Name (
         NC_Position.Argument_Context.Argument.all,
         NC_Position.Subindex);
   end Long_Name;

   function Value (Position : Cursor) return String is
      NC_Position : Non_Controlled_Cursor
         renames Controlled.Reference (Position).all;
      Context : constant not null Argument_Context_Access :=
         NC_Position.Argument_Context;
   begin
      case Argument_Parsing.Has_Value (
         Context.Argument.all,
         NC_Position.Subindex)
      is
         when Argument_Parsing.None | Argument_Parsing.Same =>
            return Argument_Parsing.Value (
               Context.Argument.all,
               NC_Position.Subindex);
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
      NC_Position : Non_Controlled_Cursor
         renames Controlled.Reference (Position).all;
      Context : constant not null Argument_Context_Access :=
         NC_Position.Argument_Context;
      Subindex : constant Argument_Parsing.Cursor :=
         Argument_Parsing.Next (
            Context.Argument_Iterator.all,
            NC_Position.Subindex);
   begin
      if Argument_Parsing.Has_Element (Subindex) then
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

   package body Controlled is

      function Create (
         Argument_Context : Argument_Context_Access;
         Subindex : Argument_Parsing.Cursor)
         return Cursor is
      begin
         if Argument_Context /= null then
            Retain (Argument_Context);
         end if;
         return (Finalization.Controlled with (Argument_Context, Subindex));
      end Create;

      function Reference (Position : Generic_Parsing.Cursor)
         return not null access Non_Controlled_Cursor is
      begin
         return Cursor (Position).Data'Unrestricted_Access;
      end Reference;

      overriding procedure Adjust (Object : in out Cursor) is
      begin
         if Object.Data.Argument_Context /= null then
            Retain (Object.Data.Argument_Context);
         end if;
      end Adjust;

      overriding procedure Finalize (Object : in out Cursor) is
      begin
         if Object.Data.Argument_Context /= null then
            Release (Object.Data.Argument_Context);
         end if;
      end Finalize;

   end Controlled;

end Ada.Command_Line.Generic_Parsing;
