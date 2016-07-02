with Ada.Tags;
with Ada.Unchecked_Deallocation;
with System.Standard_Allocators;
with System.Storage_Elements;
with System.Storage_Pools.Standard_Pools;
procedure Ada.Unchecked_Reallocation (
   X : in out Name;
   First_Index : Index_Type;
   Last_Index : Index_Type'Base)
is
   pragma Suppress (All_Checks);
   use type Tags.Tag;
   Old_First_Index : constant Index_Type := X'First;
   Old_Last_Index : constant Index_Type := X'Last;
   Min_Last_Index : constant Index_Type'Base :=
      Index_Type'Base'Min (Last_Index, Old_Last_Index);
   New_Length : constant Index_Type'Base :=
      Index_Type'Base'Max (Last_Index - First_Index + 1, 0);
begin
   --  reallocate
   if Name'Storage_Pool'Tag =
      System.Storage_Pools.Standard_Pools.Standard_Pool'Tag
   then
      --  standard storage pool
      --  delete from front
      if Old_First_Index < First_Index
         and then First_Index <= Min_Last_Index
      then
         declare
            Move_Length : constant Index_Type'Base :=
               Min_Last_Index - First_Index + 1;
         begin
            X (Old_First_Index .. Old_First_Index + Move_Length - 1) :=
               X (First_Index .. First_Index + Move_Length - 1);
         end;
      end if;
      --  real reallocation
      declare
         use type System.Storage_Elements.Storage_Offset;
         subtype Storage_Offset is System.Storage_Elements.Storage_Offset;
         Object_Address : constant System.Address := X.all'Address;
         Pool_Address : constant System.Address := X'Pool_Address;
         Constraints_Size : constant Storage_Offset :=
            Object_Address - Pool_Address;
         New_Object_Size : Storage_Offset;
         New_Pool_Size : Storage_Offset;
         New_Pool_Address : System.Address;
         New_Object_Address : System.Address;
      begin
         if Array_Type'Component_Size
            rem Standard'Storage_Unit = 0
         then -- optimized for packed
            New_Object_Size :=
               Storage_Offset (New_Length)
               * (Array_Type'Component_Size / Standard'Storage_Unit);
         else -- unpacked
            New_Object_Size :=
               (Storage_Offset (New_Length) * Array_Type'Component_Size
                  + (Standard'Storage_Unit - 1))
               / Standard'Storage_Unit;
         end if;
         New_Pool_Size := Constraints_Size + New_Object_Size;
         New_Pool_Address := System.Standard_Allocators.Reallocate (
            Pool_Address,
            New_Pool_Size);
         New_Object_Address := New_Pool_Address + Constraints_Size;
         --  rewrite 'First and 'Last
         declare
            type Constraints is record
               First : Index_Type;
               Last : Index_Type'Base;
            end record;
            pragma Suppress_Initialization (Constraints);
            C : Constraints;
            for C'Address use New_Pool_Address;
         begin
            C.First := First_Index;
            C.Last := Last_Index;
         end;
         --  set X
         declare
            type Repr is record
               Data : System.Address;
               Constraints : System.Address;
            end record;
            pragma Suppress_Initialization (Repr);
            R : Repr;
            for R'Address use X'Address;
         begin
            R.Data := New_Object_Address;
            R.Constraints := New_Pool_Address;
         end;
      end;
      --  insert to front
      if First_Index < Old_First_Index
         and then Old_First_Index <= Min_Last_Index
      then
         declare
            Move_Length : constant Index_Type'Base :=
               Min_Last_Index - Old_First_Index + 1;
         begin
            X (Old_First_Index .. Old_First_Index + Move_Length - 1) :=
               X (First_Index .. First_Index + Move_Length - 1);
         end;
      end if;
   else
      --  user defined storage pool
      --  allocate, copy, and deallocate
      declare
         procedure Free is new Unchecked_Deallocation (Array_Type, Name);
         Max_First_Index : constant Index_Type :=
            Index_Type'Max (First_Index, Old_First_Index);
         New_X : constant Name := new Array_Type (First_Index .. Last_Index);
      begin
         New_X (Max_First_Index .. Min_Last_Index) :=
            X (Max_First_Index .. Min_Last_Index);
         Free (X);
         X := New_X;
      end;
   end if;
end Ada.Unchecked_Reallocation;
