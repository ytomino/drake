pragma License (Unrestricted);
--  implementation unit required by compiler
with System.Tasking.Synchronous_Objects;
package System.Tasking.Protected_Objects is

   --  required by compiler
   subtype Protected_Entry_Index is Entry_Index range Null_Entry .. Max_Entry;

   subtype Positive_Protected_Entry_Index is
      Protected_Entry_Index range 1 .. Max_Entry; -- not req

   type Barrier_Function_Pointer is access function (
      O : Address;
      E : Protected_Entry_Index)
      return Boolean;
   type Entry_Action_Pointer is access procedure (
      O : Address;
      P : Address;
      E : Protected_Entry_Index);

   --  required by compiler
   type Entry_Body is record
      Barrier : Barrier_Function_Pointer;
      Action  : Entry_Action_Pointer;
   end record;
   pragma Suppress_Initialization (Entry_Body);

   --  required for protected object by compiler
   type Protection is limited record
      Lock : Synchronous_Objects.RW_Lock;
   end record;
   pragma Suppress_Initialization (Protection);

   procedure Initialize_Protection (
      Object : not null access Protection;
      Ceiling_Priority : Integer);
   procedure Finalize_Protection (Object : in out Protection);

   --  required for protected procedure by compiler
   procedure Lock (Object : not null access Protection);

   --  required for protected function by compiler
   procedure Lock_Read_Only (Object : not null access Protection);

   --  required for protected procedure/function by compiler
   procedure Unlock (Object : not null access Protection);

   --  protected type be expanded below:
   --
   --  protected type protected1__rwT is
   --     ... subprograms ...
   --  end protected1__rwT;
   --  type protected1__rwTV is limited record
   --     _object : aliased system__tasking__protected_objects__protection;
   --  end record;
   --  procedure protected1__rw__procN (_object : in out protected1__rwTV);
   --  procedure protected1__rw__procP (_object : in out protected1__rwTV);
   --  freeze protected1__rwTV [
   --     procedure protected1__rwTVIP (_init : in out protected1__rwTV) is
   --     begin
   --        $system__tasking__protected_objects__initialize_protection (
   --          _init._object'unchecked_access,
   --          system__tasking__unspecified_priority, objectL => 0);
   --        return;
   --     end protected1__rwTVIP;
   --  ]
   --
   --  procedure protected1___finalizer is
   --     A24b : constant boolean := $ada__exceptions__triggered_by_abort;
   --     E25b : ada__exceptions__exception_occurrence;
   --     R26b : boolean := false;
   --  begin
   --     system__soft_links__abort_defer.all;
   --     if ... initialization is completed ... then
   --        B27b : begin
   --           ... finalize components ...
   --        exception
   --           when others =>
   --              null;
   --        end B27b;
   --        $system__tasking__protected_objects__finalize_protection (
   --           protected1__rwTV!(rw)._object);
   --     end if;
   --     if R26b and then not A24b then
   --        $ada__exceptions__raise_from_controlled_operation (E25b);
   --     end if;
   --     system__soft_links__abort_undefer.all;
   --     return;
   --  end protected1___finalizer;
   --
   --  procedure protected1__rw__procP (_object : in out protected1__rwTV) is
   --     procedure protected1__rw__procP___finalizer is
   --     begin
   --        $system__tasking__protected_objects__unlock (_object._object'
   --          unchecked_access, objectL => 0);
   --        system__soft_links__abort_undefer.all;
   --        return;
   --     end protected1__rw__procP___finalizer;
   --  begin
   --     system__soft_links__abort_defer.all;
   --     $system__tasking__protected_objects__lock (_object._object'
   --       unchecked_access, objectL => 0);
   --     B12b : begin
   --        protected1__rw__procN (_object);
   --     at end
   --        protected1__rw__procP___finalizer;
   --     end B12b;
   --     return;
   --  end protected1__rw__procP;

   --  suffix N is body of protected subprogram
   --  suffix P is locking wrapper

end System.Tasking.Protected_Objects;
