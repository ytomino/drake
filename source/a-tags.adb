pragma Check_Policy (Trace, Off);
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

   package OTT_Ptr_Conv is
      new System.Address_To_Named_Access_Conversions (
         System.Storage_Elements.Storage_Offset,
         Offset_To_Top_Ptr);

   function strlen (Item : not null Cstring_Ptr)
      return System.Storage_Elements.Storage_Count;
   pragma Import (Intrinsic, strlen, "__builtin_strlen");

   type E_Node;
   type E_Node_Access is access E_Node;
   type E_Node is record
      Left, Right : aliased E_Node_Access;
      Tag : Tags.Tag;
   end record;
   pragma Suppress_Initialization (E_Node);

   procedure E_Insert (
      Node : in out E_Node_Access;
      T : Tag;
      External : String);
   procedure E_Insert (
      Node : in out E_Node_Access;
      T : Tag;
      External : String) is
   begin
      if Node = null then
         Node := new E_Node'(Left => null, Right => null, Tag => T);
      else
         declare
            TSD : constant Type_Specific_Data_Ptr :=
               TSD_Ptr_Conv.To_Pointer (DT (Node.all.Tag).TSD);
            Node_External : String
               renames
                  TSD.External_Tag (1 .. Natural (strlen (TSD.External_Tag)));
         begin
            if Node_External > External then
               E_Insert (Node.Left, T, External);
            elsif Node_External < External then
               E_Insert (Node.Right, T, External);
            else
               null; -- already added
            end if;
         end;
      end if;
   end E_Insert;

   function E_Find (Node : E_Node_Access; External : String)
      return E_Node_Access;
   function E_Find (Node : E_Node_Access; External : String)
      return E_Node_Access is
   begin
      if Node = null then
         return null;
      else
         declare
            TSD : constant Type_Specific_Data_Ptr :=
               TSD_Ptr_Conv.To_Pointer (DT (Node.Tag).TSD);
            Node_External : String
               renames
                  TSD.External_Tag (1 .. Natural (strlen (TSD.External_Tag)));
         begin
            if Node_External > External then
               return E_Find (Node.Left, External);
            elsif Node_External < External then
               return E_Find (Node.Right, External);
            else
               return Node;
            end if;
         end;
      end if;
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

   --  implementation

   function Base_Address (This : System.Address) return System.Address is
      function Offset_To_Top (This : System.Address)
         return System.Storage_Elements.Storage_Offset;
      function Offset_To_Top (This : System.Address)
         return System.Storage_Elements.Storage_Offset
      is
         T_DT : constant Dispatch_Table_Ptr :=
            DT (Tag_Ptr_Conv.To_Pointer (This).all);
      begin
         if T_DT.Offset_To_Top =
            System.Storage_Elements.Storage_Offset'Last
         then
            declare
               Tag_Size : constant :=
                  Standard'Address_Size / Standard'Storage_Unit;
               Offset_To_Top : constant System.Address := This + Tag_Size;
            begin
               return OTT_Ptr_Conv.To_Pointer (Offset_To_Top).all;
            end;
         else
            return T_DT.Offset_To_Top;
         end if;
      end Offset_To_Top;
   begin
      return This - Offset_To_Top (This);
   end Base_Address;

   function Descendant_Tag (External : String; Ancestor : Tag) return Tag is
      Result : constant Tag := Internal_Tag (External);
   begin
      if not Is_Descendant (
         Result,
         Ancestor,
         Primary_Only => False,
         Same_Level => False)
      then
         Raise_Exception (Tag_Error'Identity);
      else
         return Result;
      end if;
   end Descendant_Tag;

   function Displace (This : System.Address; T : Tag) return System.Address is
   begin
      if This = System.Null_Address then
         return System.Null_Address;
      else
         declare
            Base_Object : constant System.Address := Base_Address (This);
            Base_Tag : constant Tag :=
               Tag_Ptr_Conv.To_Pointer (Base_Object).all;
            Obj_DT : constant Dispatch_Table_Ptr := DT (Base_Tag);
            Iface_Table : constant Interface_Data_Ptr :=
               TSD_Ptr_Conv.To_Pointer (Obj_DT.TSD).Interfaces_Table;
         begin
            if Iface_Table /= null then
               for Id in 1 .. Iface_Table.Nb_Ifaces loop
                  declare
                     E : Interface_Data_Element
                        renames Iface_Table.Ifaces_Table (Id);
                  begin
                     if E.Iface_Tag = T then
                        return Result : System.Address do
                           if E.Static_Offset_To_Top then
                              Result := Base_Object + E.Offset_To_Top_Value;
                           else
                              Result := Base_Object
                                 + E.Offset_To_Top_Func.all (Base_Object);
                           end if;
                        end return;
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

   function Expanded_Name (T : Tag) return String is
      DT : constant Dispatch_Table_Ptr := DT_With_Checking (T);
      TSD : constant Type_Specific_Data_Ptr :=
         TSD_Ptr_Conv.To_Pointer (DT.TSD);
   begin
      return TSD.Expanded_Name (1 .. Natural (strlen (TSD.Expanded_Name)));
   end Expanded_Name;

   function External_Tag (T : Tag) return String is
      DT : constant Dispatch_Table_Ptr := DT_With_Checking (T);
      TSD : constant Type_Specific_Data_Ptr :=
         TSD_Ptr_Conv.To_Pointer (DT.TSD);
      Result : String
         renames TSD.External_Tag (1 .. Natural (strlen (TSD.External_Tag)));
   begin
      if Result'Length > Nested_Prefix'Length
         and then Result (
            Result'First ..
            Result'First - 1 + Nested_Prefix'Length) = Nested_Prefix
      then
         null; -- nested
      else
         System.Shared_Locking.Enter;
         E_Insert (External_Map, T, Result); -- library level
         System.Shared_Locking.Leave;
      end if;
      return Result;
   end External_Tag;

   function Get_Entry_Index (T : Tag; Position : Positive) return Positive is
      TSD : constant Type_Specific_Data_Ptr :=
         TSD_Ptr_Conv.To_Pointer (DT (T).TSD);
   begin
      return TSD.SSD.SSD_Table (Position).Index;
   end Get_Entry_Index;

   function Get_Prim_Op_Kind (T : Tag; Position : Positive)
      return Prim_Op_Kind
   is
      TSD : constant Type_Specific_Data_Ptr :=
         TSD_Ptr_Conv.To_Pointer (DT (T).TSD);
   begin
      return TSD.SSD.SSD_Table (Position).Kind;
   end Get_Prim_Op_Kind;

   function Interface_Ancestor_Tags (T : Tag) return Tag_Array is
      DT : constant Dispatch_Table_Ptr := DT_With_Checking (T);
      TSD : constant Type_Specific_Data_Ptr :=
         TSD_Ptr_Conv.To_Pointer (DT.TSD);
      Intf_Table : constant Interface_Data_Ptr := TSD.Interfaces_Table;
      Length : Natural;
   begin
      if Intf_Table = null then
         Length := 0;
      else
         Length := Intf_Table.Nb_Ifaces;
      end if;
      return Result : Tag_Array (1 .. Length) do
         for I in Result'Range loop
            Result (I) := Intf_Table.Ifaces_Table (I).Iface_Tag;
         end loop;
      end return;
   end Interface_Ancestor_Tags;

   function Internal_Tag (External : String) return Tag is
   begin
      if External'Length >= Nested_Prefix'Length
         and then External (
            External'First ..
            External'First + Nested_Prefix'Length - 1) = Nested_Prefix
      then
         declare
            Addr_First : constant Positive :=
               External'First + Nested_Prefix'Length;
            Addr_Last : constant Natural :=
               Addr_First
               + System.Formatting.Address.Address_String'Length
               - 1;
            Result : System.Address;
            Error : Boolean;
         begin
            if Addr_Last >= External'Last
               or else External (Addr_Last + 1) /= '#'
            then
               Raise_Exception (Tag_Error'Identity);
            end if;
            System.Formatting.Address.Value (
               External (Addr_First .. Addr_Last),
               Result,
               Error => Error);
            if Error then
               Raise_Exception (Tag_Error'Identity);
            end if;
            return Tag_Conv.To_Pointer (Result);
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

   function Is_Abstract (T : Tag) return Boolean is
      DT : constant Dispatch_Table_Ptr := DT_With_Checking (T);
      TSD : constant Type_Specific_Data_Ptr :=
         TSD_Ptr_Conv.To_Pointer (DT.TSD);
   begin
      return TSD.Type_Is_Abstract;
   end Is_Abstract;

   function Is_Descendant (
      Descendant, Ancestor : Tag;
      Primary_Only : Boolean;
      Same_Level : Boolean)
      return Boolean
   is
      D_DT : constant Dispatch_Table_Ptr := DT_With_Checking (Descendant);
      A_DT : constant Dispatch_Table_Ptr := DT_With_Checking (Ancestor);
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
                     Intf_Table : constant Interface_Data_Ptr :=
                        D_TSD.Interfaces_Table;
                  begin
                     if Intf_Table /= null then
                        for Id in 1 .. Intf_Table.Nb_Ifaces loop
                           if Intf_Table.Ifaces_Table (Id).Iface_Tag =
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

   function Is_Descendant_At_Same_Level (Descendant, Ancestor : Tag)
      return Boolean is
   begin
      return Is_Descendant (
         Descendant,
         Ancestor,
         Primary_Only => False,
         Same_Level => True);
   end Is_Descendant_At_Same_Level;

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
      DT : constant Dispatch_Table_Ptr := DT_With_Checking (T);
      TSD : constant Type_Specific_Data_Ptr :=
         TSD_Ptr_Conv.To_Pointer (DT.TSD);
   begin
      return TSD.Needs_Finalization;
   end Needs_Finalization;

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

   procedure Register_Interface_Offset (
      This : System.Address;
      Interface_T : Tag;
      Is_Static : Boolean;
      Offset_Value : System.Storage_Elements.Storage_Offset;
      Offset_Func : Offset_To_Top_Function_Ptr)
   is
      DT : constant Dispatch_Table_Ptr :=
         Tags.DT (Tag_Ptr_Conv.To_Pointer (This).all);
      Iface_Table : constant Interface_Data_Ptr :=
         TSD_Ptr_Conv.To_Pointer (DT.TSD).Interfaces_Table;
   begin
      for I in 1 .. Iface_Table.Nb_Ifaces loop
         declare
            Item : Interface_Data_Element
               renames Iface_Table.Ifaces_Table (I);
         begin
            if Item.Iface_Tag = Interface_T then
               Item.Static_Offset_To_Top := Is_Static or else Offset_Value = 0;
               if Item.Static_Offset_To_Top then
                  Item.Offset_To_Top_Value := Offset_Value;
               else
                  Item.Offset_To_Top_Func := Offset_Func;
               end if;
               exit;
            end if;
         end;
      end loop;
      --  should it raise some error ???
   end Register_Interface_Offset;

   procedure Set_Entry_Index (
      T : Tag;
      Position : Positive;
      Value : Positive)
   is
      TSD : constant Type_Specific_Data_Ptr :=
         TSD_Ptr_Conv.To_Pointer (DT (T).TSD);
   begin
      TSD.SSD.SSD_Table (Position).Index := Value;
   end Set_Entry_Index;

   procedure Set_Prim_Op_Kind (
      T : Tag;
      Position : Positive;
      Value : Prim_Op_Kind)
   is
      TSD : constant Type_Specific_Data_Ptr :=
         TSD_Ptr_Conv.To_Pointer (DT (T).TSD);
   begin
      TSD.SSD.SSD_Table (Position).Kind := Value;
   end Set_Prim_Op_Kind;

   function Wide_Expanded_Name (T : Tag) return Wide_String is
   begin
      return System.UTF_Conversions.From_8_To_16.Convert (Expanded_Name (T));
   end Wide_Expanded_Name;

   function Wide_Wide_Expanded_Name (T : Tag) return Wide_Wide_String is
   begin
      return System.UTF_Conversions.From_8_To_32.Convert (Expanded_Name (T));
   end Wide_Wide_Expanded_Name;

end Ada.Tags;
