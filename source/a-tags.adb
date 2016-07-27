pragma Check_Policy (Trace => Ignore);
with Ada.Exception_Identification.From_Here;
with System.Address_To_Named_Access_Conversions;
with System.Formatting.Address;
with System.Shared_Locking;
with System.UTF_Conversions.From_8_To_16;
with System.UTF_Conversions.From_8_To_32;
package body Ada.Tags is
   pragma Suppress (All_Checks);
   use Exception_Identification.From_Here;
   use type System.Address;
   use type System.Storage_Elements.Storage_Offset;

   --  (a-tags.ads)
   type Object_Specific_Data_Ptr is access all Object_Specific_Data;

   Nested_Prefix : constant String := "Internal tag at 16#";

   package Tag_Conv is
      new System.Address_To_Named_Access_Conversions (Dispatch_Table, Tag);
   package Tag_Ptr_Conv is
      new System.Address_To_Named_Access_Conversions (Tag, Tag_Ptr);

   package DT_Ptr_Conv is
      new System.Address_To_Named_Access_Conversions (
         Dispatch_Table_Wrapper,
         Dispatch_Table_Ptr);
   package TSD_Ptr_Conv is
      new System.Address_To_Named_Access_Conversions (
         Type_Specific_Data,
         Type_Specific_Data_Ptr);
   package OSD_Ptr_Conv is
      new System.Address_To_Named_Access_Conversions (
         Object_Specific_Data,
         Object_Specific_Data_Ptr);

   function strlen (Item : not null Cstring_Ptr)
      return System.Storage_Elements.Storage_Count
      with Import,
         Convention => Intrinsic, External_Name => "__builtin_strlen";

   type E_Node;
   type E_Node_Access is access E_Node;
   type E_Node is record
      Left, Right : aliased E_Node_Access;
      Tag : Tags.Tag;
   end record;
   pragma Suppress_Initialization (E_Node);

   procedure E_Insert (
      Node : aliased in out E_Node_Access;
      T : Tag;
      External : String);
   procedure E_Insert (
      Node : aliased in out E_Node_Access;
      T : Tag;
      External : String)
   is
      type E_Node_Access_Access is access all E_Node_Access;
      for E_Node_Access_Access'Storage_Size use 0;
      Current : not null E_Node_Access_Access := Node'Access;
   begin
      loop
         if Current.all = null then
            Current.all := new E_Node'(Left => null, Right => null, Tag => T);
            exit;
         else
            declare
               Current_DT : constant Dispatch_Table_Ptr :=
                  DT (Current.all.Tag);
               Current_TSD : constant Type_Specific_Data_Ptr :=
                  TSD_Ptr_Conv.To_Pointer (Current_DT.TSD);
               Current_External_Len : constant Natural :=
                  Natural (strlen (Current_TSD.External_Tag));
               Current_External : String
                  renames Current_TSD.External_Tag (1 .. Current_External_Len);
            begin
               if Current_External > External then
                  Current := Current.all.Left'Access;
               elsif Current_External < External then
                  Current := Current.all.Right'Access;
               else
                  exit; -- already added
               end if;
            end;
         end if;
      end loop;
   end E_Insert;

   function E_Find (Node : E_Node_Access; External : String)
      return E_Node_Access;
   function E_Find (Node : E_Node_Access; External : String)
      return E_Node_Access
   is
      Current : E_Node_Access := Node;
   begin
      while Current /= null loop
         declare
            Current_DT : constant Dispatch_Table_Ptr := DT (Current.Tag);
            Current_TSD : constant Type_Specific_Data_Ptr :=
               TSD_Ptr_Conv.To_Pointer (Current_DT.TSD);
            Current_External_Len : constant Natural :=
               Natural (strlen (Current_TSD.External_Tag));
            Current_External : String
               renames Current_TSD.External_Tag (1 .. Current_External_Len);
         begin
            if Current_External > External then
               Current := Current.Left;
            elsif Current_External < External then
               Current := Current.Right;
            else
               return Current; -- found
            end if;
         end;
      end loop;
      return null; -- not found
   end E_Find;

   External_Map : aliased E_Node_Access := null;

   function DT_With_Checking (T : Tag) return Dispatch_Table_Ptr;
   function DT_With_Checking (T : Tag) return Dispatch_Table_Ptr is
   begin
      if T = No_Tag then
         Raise_Exception (Tag_Error'Identity);
      else
         return DT (T);
      end if;
   end DT_With_Checking;

   --  inheritance relation check
   function Is_Descendant (
      Descendant, Ancestor : Tag;
      Primary_Only : Boolean;
      Same_Level : Boolean)
      return Boolean;
   function Is_Descendant (
      Descendant, Ancestor : Tag;
      Primary_Only : Boolean;
      Same_Level : Boolean)
      return Boolean
   is
      D_DT : constant Dispatch_Table_Ptr := DT (Descendant);
      A_DT : constant Dispatch_Table_Ptr := DT (Ancestor);
      D_TSD : constant Type_Specific_Data_Ptr :=
         TSD_Ptr_Conv.To_Pointer (D_DT.TSD);
      A_TSD : constant Type_Specific_Data_Ptr :=
         TSD_Ptr_Conv.To_Pointer (A_DT.TSD);
   begin
      if Same_Level and then D_TSD.Access_Level /= A_TSD.Access_Level then
         return False;
      else
         case A_DT.Signature is
            when Primary_DT => -- tagged record
               declare
                  Offset : constant Integer := D_TSD.Idepth - A_TSD.Idepth;
               begin
                  return Offset >= 0
                     and then D_TSD.Tags_Table (Offset) = Ancestor;
               end;
            when Secondary_DT | Unknown => -- interface
               if Primary_Only then
                  return False;
               else
                  declare
                     D_Interfaces_Table : constant Interface_Data_Ptr :=
                        D_TSD.Interfaces_Table;
                  begin
                     if D_Interfaces_Table /= null then
                        for I in 1 .. D_Interfaces_Table.Nb_Ifaces loop
                           if D_Interfaces_Table.Ifaces_Table (I).Iface_Tag =
                              Ancestor
                           then
                              return True;
                           end if;
                        end loop;
                     end if;
                     return False;
                  end;
               end if;
         end case;
      end if;
   end Is_Descendant;

   --  implementation

   function Expanded_Name (T : Tag) return String is
      DT : constant Dispatch_Table_Ptr := DT_With_Checking (T);
      TSD : constant Type_Specific_Data_Ptr :=
         TSD_Ptr_Conv.To_Pointer (DT.TSD);
   begin
      return TSD.Expanded_Name (1 .. Natural (strlen (TSD.Expanded_Name)));
   end Expanded_Name;

   function Wide_Expanded_Name (T : Tag) return Wide_String is
   begin
      return System.UTF_Conversions.From_8_To_16.Convert (Expanded_Name (T));
   end Wide_Expanded_Name;

   function Wide_Wide_Expanded_Name (T : Tag) return Wide_Wide_String is
   begin
      return System.UTF_Conversions.From_8_To_32.Convert (Expanded_Name (T));
   end Wide_Wide_Expanded_Name;

   function External_Tag (T : Tag) return String is
      DT : constant Dispatch_Table_Ptr := DT_With_Checking (T);
      TSD : constant Type_Specific_Data_Ptr :=
         TSD_Ptr_Conv.To_Pointer (DT.TSD);
      Result : String
         renames TSD.External_Tag (1 .. Natural (strlen (TSD.External_Tag)));
      L : constant Natural := Result'First + (Nested_Prefix'Length - 1);
   begin
      if L < Result'Last
         and then Result (Result'First .. L) = Nested_Prefix
      then
         null; -- nested
      else
         System.Shared_Locking.Enter;
         E_Insert (External_Map, T, Result); -- library-level
         System.Shared_Locking.Leave;
      end if;
      return Result;
   end External_Tag;

   function Internal_Tag (External : String) return Tag is
      L : constant Natural := External'First + (Nested_Prefix'Length - 1);
   begin
      if L <= External'Last
         and then External (External'First .. L) = Nested_Prefix
      then
         declare
            Addr_First : constant Positive := L + 1;
            Addr_Last : constant Natural :=
               Addr_First
               + (System.Formatting.Address.Address_String'Length - 1);
         begin
            if Addr_Last >= External'Last
               or else External (Addr_Last + 1) /= '#'
            then
               Raise_Exception (Tag_Error'Identity);
            end if;
            declare
               Result : System.Address;
               Error : Boolean;
            begin
               System.Formatting.Address.Value (
                  External (Addr_First .. Addr_Last),
                  Result,
                  Error => Error);
               if Error then
                  Raise_Exception (Tag_Error'Identity);
               end if;
               return Tag_Conv.To_Pointer (Result);
            end;
         end;
      else
         declare
            Node : E_Node_Access;
         begin
            System.Shared_Locking.Enter;
            Node := E_Find (External_Map, External);
            System.Shared_Locking.Leave;
            if Node = null then
               Raise_Exception (Tag_Error'Identity);
            end if;
            return Node.Tag;
         end;
      end if;
   end Internal_Tag;

   function Descendant_Tag (External : String; Ancestor : Tag) return Tag is
   begin
      if Ancestor = No_Tag then
         Raise_Exception (Tag_Error'Identity);
      end if;
      declare
         Result : constant Tag := Internal_Tag (External);
      begin
         if not Is_Descendant (
            Result,
            Ancestor,
            Primary_Only => False,
            Same_Level => False)
         then
            Raise_Exception (Tag_Error'Identity);
         end if;
         return Result;
      end;
   end Descendant_Tag;

   function Is_Descendant_At_Same_Level (Descendant, Ancestor : Tag)
      return Boolean is
   begin
      if Descendant = No_Tag or else Ancestor = No_Tag then
         Raise_Exception (Tag_Error'Identity);
      end if;
      return Is_Descendant (
         Descendant,
         Ancestor,
         Primary_Only => False,
         Same_Level => True);
   end Is_Descendant_At_Same_Level;

   function Parent_Tag (T : Tag) return Tag is
      DT : constant Dispatch_Table_Ptr := DT_With_Checking (T);
      TSD : constant Type_Specific_Data_Ptr :=
         TSD_Ptr_Conv.To_Pointer (DT.TSD);
   begin
      if TSD.Idepth = 0 then
         return No_Tag;
      else
         return TSD.Tags_Table (1);
      end if;
   end Parent_Tag;

   function Interface_Ancestor_Tags (T : Tag) return Tag_Array is
      DT : constant Dispatch_Table_Ptr := DT_With_Checking (T);
      TSD : constant Type_Specific_Data_Ptr :=
         TSD_Ptr_Conv.To_Pointer (DT.TSD);
      Interfaces_Table : constant Interface_Data_Ptr :=
         TSD.Interfaces_Table;
      Length : Natural;
   begin
      if Interfaces_Table = null then
         Length := 0;
      else
         Length := Interfaces_Table.Nb_Ifaces;
      end if;
      return Result : Tag_Array (1 .. Length) do
         for I in Result'Range loop
            Result (I) := Interfaces_Table.Ifaces_Table (I).Iface_Tag;
         end loop;
      end return;
   end Interface_Ancestor_Tags;

   function Is_Abstract (T : Tag) return Boolean is
      DT : constant Dispatch_Table_Ptr := DT_With_Checking (T);
      TSD : constant Type_Specific_Data_Ptr :=
         TSD_Ptr_Conv.To_Pointer (DT.TSD);
   begin
      return TSD.Type_Is_Abstract;
   end Is_Abstract;

   function Base_Address (This : System.Address) return System.Address is
      function Offset_To_Top (This : System.Address)
         return System.Storage_Elements.Storage_Offset;
      function Offset_To_Top (This : System.Address)
         return System.Storage_Elements.Storage_Offset
      is
         T : constant Tag := Tag_Ptr_Conv.To_Pointer (This).all;
         T_DT : constant Dispatch_Table_Ptr := DT (T);
      begin
--       if T_DT.Offset_To_Top =
--          System.Storage_Elements.Storage_Offset'Last
--       then -- set by Set_Dynamic_Offset_To_Top
--          declare
--             package OTT_Ptr_Conv is
--                new System.Address_To_Named_Access_Conversions (
--                   System.Storage_Elements.Storage_Offset,
--                   Offset_To_Top_Ptr);
--             Tag_Size : constant :=
--                Standard'Address_Size / Standard'Storage_Unit;
--          begin
--             return OTT_Ptr_Conv.To_Pointer (This + Tag_Size).all;
--          end;
--       end if;
         return T_DT.Offset_To_Top;
      end Offset_To_Top;
   begin
      return This - Offset_To_Top (This);
   end Base_Address;

   function Displace (This : System.Address; T : Tag) return System.Address is
   begin
      if This = System.Null_Address then
         return System.Null_Address;
      else
         declare
            Base_Object : constant System.Address := Base_Address (This);
            Base_Tag : constant Tag :=
               Tag_Ptr_Conv.To_Pointer (Base_Object).all;
            Base_DT : constant Dispatch_Table_Ptr := DT (Base_Tag);
            Base_TSD : constant Type_Specific_Data_Ptr :=
               TSD_Ptr_Conv.To_Pointer (Base_DT.TSD);
            Base_Interfaces_Table : constant Interface_Data_Ptr :=
               Base_TSD.Interfaces_Table;
         begin
            if Base_Interfaces_Table /= null then
               for I in 1 .. Base_Interfaces_Table.Nb_Ifaces loop
                  declare
                     E : Interface_Data_Element
                        renames Base_Interfaces_Table.Ifaces_Table (I);
                  begin
                     if E.Iface_Tag = T then
                        if E.Static_Offset_To_Top then
                           return Base_Object + E.Offset_To_Top_Value;
                        else
                           return Base_Object
                              + E.Offset_To_Top_Func.all (Base_Object);
                        end if;
                     end if;
                  end;
               end loop;
            end if;
            if Is_Descendant (
               Base_Tag,
               T,
               Primary_Only => True,
               Same_Level => False)
            then
               return Base_Object;
            else
               if Get_Delegation /= null then
                  declare
                     Aggregated : constant System.Address :=
                        Get_Delegation (Base_Object, T);
                  begin
                     if Aggregated /= System.Null_Address then
                        pragma Check (Trace, Debug.Put ("delegating"));
                        return Aggregated;
                     end if;
                  end;
               end if;
               raise Constraint_Error; -- invalid interface conversion
            end if;
         end;
      end if;
   end Displace;

   function DT (T : Tag) return Dispatch_Table_Ptr is
      subtype Dispatch_Table_Wrapper_0 is Dispatch_Table_Wrapper (0);
   begin
      return DT_Ptr_Conv.To_Pointer (
         Tag_Conv.To_Address (T)
            - Dispatch_Table_Wrapper_0'Size / Standard'Storage_Unit);
   end DT;

   function Get_Entry_Index (T : Tag; Position : Positive) return Positive is
      T_DT : constant Dispatch_Table_Ptr := DT (T);
      T_TSD : constant Type_Specific_Data_Ptr :=
         TSD_Ptr_Conv.To_Pointer (T_DT.TSD);
   begin
      return T_TSD.SSD.SSD_Table (Position).Index;
   end Get_Entry_Index;

   function Get_Offset_Index (T : Tag; Position : Positive) return Positive is
      T_DT : constant Dispatch_Table_Ptr := DT (T);
   begin
      if T_DT.Signature = Primary_DT then
         return Position;
      else
         declare
            T_OSD : constant Object_Specific_Data_Ptr :=
               OSD_Ptr_Conv.To_Pointer (T_DT.TSD);
         begin
            return T_OSD.OSD_Table (Position);
         end;
      end if;
   end Get_Offset_Index;

   function Get_Prim_Op_Kind (T : Tag; Position : Positive)
      return Prim_Op_Kind
   is
      T_DT : constant Dispatch_Table_Ptr := DT (T);
      T_TSD : constant Type_Specific_Data_Ptr :=
         TSD_Ptr_Conv.To_Pointer (T_DT.TSD);
   begin
      return T_TSD.SSD.SSD_Table (Position).Kind;
   end Get_Prim_Op_Kind;

   function Get_Tagged_Kind (T : Tag) return Tagged_Kind is
      T_DT : constant Dispatch_Table_Ptr := DT (T);
   begin
      return T_DT.Tag_Kind;
   end Get_Tagged_Kind;

   function IW_Membership (This : System.Address; T : Tag) return Boolean is
      Base_Object : constant System.Address := Base_Address (This);
      Base_Tag : constant Tag := Tag_Ptr_Conv.To_Pointer (Base_Object).all;
   begin
      if Is_Descendant (
         Base_Tag,
         T,
         Primary_Only => False,
         Same_Level => False)
      then
         return True;
      else
         if Get_Delegation /= null then
            declare
               Aggregated : constant System.Address :=
                  Get_Delegation (Base_Object, T);
            begin
               if Aggregated /= System.Null_Address then
                  pragma Check (Trace, Debug.Put ("delegating"));
                  return True;
               end if;
            end;
         end if;
         return False;
      end if;
   end IW_Membership;

   function Needs_Finalization (T : Tag) return Boolean is
      T_DT : constant Dispatch_Table_Ptr := DT (T);
      T_TSD : constant Type_Specific_Data_Ptr :=
         TSD_Ptr_Conv.To_Pointer (T_DT.TSD);
   begin
      return T_TSD.Needs_Finalization;
   end Needs_Finalization;

   procedure Register_Interface_Offset (
      This : System.Address;
      Interface_T : Tag;
      Is_Static : Boolean;
      Offset_Value : System.Storage_Elements.Storage_Offset;
      Offset_Func : Offset_To_Top_Function_Ptr)
   is
      T : constant Tag := Tag_Ptr_Conv.To_Pointer (This).all;
      T_DT : constant Dispatch_Table_Ptr := DT (T);
      T_TSD : constant Type_Specific_Data_Ptr :=
         TSD_Ptr_Conv.To_Pointer (T_DT.TSD);
      T_Interfaces_Table : constant Interface_Data_Ptr :=
         T_TSD.Interfaces_Table;
   begin
      for I in 1 .. T_Interfaces_Table.Nb_Ifaces loop
         declare
            E : Interface_Data_Element
               renames T_Interfaces_Table.Ifaces_Table (I);
         begin
            if E.Iface_Tag = Interface_T then
               E.Static_Offset_To_Top := Is_Static or else Offset_Value = 0;
               if E.Static_Offset_To_Top then
                  E.Offset_To_Top_Value := Offset_Value;
               else
                  E.Offset_To_Top_Func := Offset_Func;
               end if;
               exit;
            end if;
         end;
      end loop;
      --  should it raise some error ???
   end Register_Interface_Offset;

   function Secondary_Tag (T, Iface : Tag) return Tag is
      T_DT : constant Dispatch_Table_Ptr := DT (T);
      T_TSD : constant Type_Specific_Data_Ptr :=
         TSD_Ptr_Conv.To_Pointer (T_DT.TSD);
      T_Interfaces_Table : constant Interface_Data_Ptr :=
         T_TSD.Interfaces_Table;
   begin
      if T_Interfaces_Table /= null then
         for I in 1 .. T_Interfaces_Table.Nb_Ifaces loop
            declare
               E : Interface_Data_Element
                  renames T_Interfaces_Table.Ifaces_Table (I);
            begin
               if E.Iface_Tag = Iface then
                  return E.Secondary_DT;
               end if;
            end;
         end loop;
      end if;
      raise Constraint_Error; -- invalid interface conversion
   end Secondary_Tag;

   procedure Set_Entry_Index (
      T : Tag;
      Position : Positive;
      Value : Positive)
   is
      T_DT : constant Dispatch_Table_Ptr := DT (T);
      T_TSD : constant Type_Specific_Data_Ptr :=
         TSD_Ptr_Conv.To_Pointer (T_DT.TSD);
   begin
      T_TSD.SSD.SSD_Table (Position).Index := Value;
   end Set_Entry_Index;

   procedure Set_Prim_Op_Kind (
      T : Tag;
      Position : Positive;
      Value : Prim_Op_Kind)
   is
      T_DT : constant Dispatch_Table_Ptr := DT (T);
      T_TSD : constant Type_Specific_Data_Ptr :=
         TSD_Ptr_Conv.To_Pointer (T_DT.TSD);
   begin
      T_TSD.SSD.SSD_Table (Position).Kind := Value;
   end Set_Prim_Op_Kind;

end Ada.Tags;
