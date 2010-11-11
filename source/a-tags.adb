pragma Check_Policy (Trace, Off);
with Ada.Unchecked_Conversion;
with System.To_String;
with System.UTF_Conversions;
package body Ada.Tags is
   pragma Suppress (All_Checks);
   use type System.Address;
   use type System.Storage_Elements.Storage_Offset;

   function Is_Descendant (
      Descendant, Ancestor : Tag;
      Primary_Only : Boolean;
      Same_Level : Boolean)
      return Boolean is
   begin
      if Descendant = No_Tag or else Ancestor = No_Tag then
         raise Tag_Error;
      else
         declare
            function Cast is new Unchecked_Conversion (
               System.Address,
               Type_Specific_Data_Ptr);
            A_DT : constant Dispatch_Table_Ptr := DT (Ancestor);
            D : constant Type_Specific_Data_Ptr := Cast (DT (Descendant).TSD);
            A : constant Type_Specific_Data_Ptr := Cast (A_DT.TSD);
         begin
            if Same_Level and then D.Access_Level /= A.Access_Level then
               return False;
            else
               case A_DT.Signature is
                  when Primary_DT => --  tagged record
                     declare
                        Offset : constant Integer := D.Idepth - A.Idepth;
                     begin
                        return Offset >= 0
                           and then D.Tags_Table (Offset) = Ancestor;
                     end;
                  when Secondary_DT | Unknown => --  interface
                     if Primary_Only then
                        return False;
                     else
                        declare
                           Intf_Table : constant Interface_Data_Ptr :=
                              D.Interfaces_Table;
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
         end;
      end if;
   end Is_Descendant;

   function Base_Address (This : System.Address) return System.Address is
      function Offset_To_Top (This : System.Address)
         return System.Storage_Elements.Storage_Offset;
      function Offset_To_Top (This : System.Address)
         return System.Storage_Elements.Storage_Offset
      is
         function Cast is new Unchecked_Conversion (
            System.Address,
            Tag_Ptr);
         T_DT : constant Dispatch_Table_Ptr := DT (Cast (This).all);
      begin
         if T_DT.Offset_To_Top =
            System.Storage_Elements.Storage_Offset'Last
         then
            declare
               type Storage_Offset_Ptr is
                  access all System.Storage_Elements.Storage_Offset;
               function Cast is new Unchecked_Conversion (
                  System.Address,
                  Storage_Offset_Ptr);
               Tag_Size : constant :=
                  Standard'Address_Size / Standard'Storage_Unit;
               Offset_To_Top : constant System.Address := This + Tag_Size;
            begin
               return Cast (Offset_To_Top).all;
            end;
         else
            return T_DT.Offset_To_Top;
         end if;
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
            function Cast is new Unchecked_Conversion (
               System.Address,
               Tag_Ptr);
            function Cast is new Unchecked_Conversion (
               System.Address,
               Type_Specific_Data_Ptr);
            Base_Object : constant System.Address := Base_Address (This);
            Base_Tag : constant Tag := Cast (Base_Object).all;
            Obj_DT : constant Dispatch_Table_Ptr := DT (Base_Tag);
            Iface_Table : constant Interface_Data_Ptr :=
               Cast (Obj_DT.TSD).Interfaces_Table;
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
                              Result := Base_Object +
                                 E.Offset_To_Top_Func.all (Base_Object);
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
               raise Constraint_Error with "invalid interface conversion";
            end if;
         end;
      end if;
   end Displace;

   function DT (T : Tag) return Dispatch_Table_Ptr is
      function Cast is new Unchecked_Conversion (
         System.Address,
         Dispatch_Table_Ptr);
      subtype Dispatch_Table_Wrapper_0 is Dispatch_Table_Wrapper (0);
   begin
      return Cast (System.Address'(T.all'Address -
         Dispatch_Table_Wrapper_0'Size / Standard'Storage_Unit));
   end DT;

   function Expanded_Name (T : Tag) return String is
   begin
      if T = No_Tag then
         raise Tag_Error;
      else
         declare
            function Cast is new Unchecked_Conversion (
               System.Address,
               Type_Specific_Data_Ptr);
            TSD : constant Type_Specific_Data_Ptr := Cast (DT (T).TSD);
         begin
            return System.To_String (TSD.Expanded_Name.all'Address);
         end;
      end if;
   end Expanded_Name;

   function External_Tag (T : Tag) return String is
   begin
      if T = No_Tag then
         raise Tag_Error;
      else
         declare
            function Cast is new Unchecked_Conversion (
               System.Address,
               Type_Specific_Data_Ptr);
            TSD : constant Type_Specific_Data_Ptr := Cast (DT (T).TSD);
         begin
            return System.To_String (TSD.External_Tag.all'Address);
         end;
      end if;
   end External_Tag;

   function Get_Prim_Op_Kind (T : Tag; Position : Positive)
      return Prim_Op_Kind
   is
      function Cast is new Unchecked_Conversion (
         System.Address,
         Type_Specific_Data_Ptr);
      TSD : constant Type_Specific_Data_Ptr := Cast (DT (T).TSD);
   begin
      return TSD.SSD.SSD_Table (Position).Kind;
   end Get_Prim_Op_Kind;

   function Interface_Ancestor_Tags (T : Tag) return Tag_Array is
   begin
      if T = No_Tag then
         raise Tag_Error;
      else
         declare
            function Cast is new Unchecked_Conversion (
               System.Address,
               Type_Specific_Data_Ptr);
            TSD : constant Type_Specific_Data_Ptr := Cast (DT (T).TSD);
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
         end;
      end if;
   end Interface_Ancestor_Tags;

   function Is_Abstract (T : Tag) return Boolean is
      T_DT : constant Dispatch_Table_Ptr := DT (T);
   begin
      case T_DT.Signature is
         when Primary_DT =>
            case T_DT.Tag_Kind is
               when TK_Abstract_Limited_Tagged | TK_Abstract_Tagged =>
                  return True;
               when TK_Limited_Tagged | TK_Protected | TK_Tagged | TK_Task =>
                  return False;
            end case;
         when Secondary_DT | Unknown =>
            return True; --  interface
      end case;
   end Is_Abstract;

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
      function Cast is new Unchecked_Conversion (System.Address, Tag_Ptr);
      Base_Object : constant System.Address := Base_Address (This);
      Base_Tag : constant Tag := Cast (Base_Object).all;
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

   function Parent_Tag (T : Tag) return Tag is
   begin
      if T = No_Tag then
         raise Tag_Error;
      else
         declare
            function Cast is new Unchecked_Conversion (
               System.Address,
               Type_Specific_Data_Ptr);
            TSD : constant Type_Specific_Data_Ptr := Cast (DT (T).TSD);
         begin
            if TSD.Idepth = 0 then
               return No_Tag;
            else
               return TSD.Tags_Table (1);
            end if;
         end;
      end if;
   end Parent_Tag;

   function Wide_Expanded_Name (T : Tag) return Wide_String is
      S : constant String := Expanded_Name (T);
      W : Wide_String (1 .. S'Length);
      L : Natural;
      Error : Boolean; --  ignored
   begin
      System.UTF_Conversions.UTF_8_To_UTF_16 (S, W, L, Error);
      return W (1 .. L);
   end Wide_Expanded_Name;

   function Wide_Wide_Expanded_Name (T : Tag) return Wide_Wide_String is
      S : constant String := Expanded_Name (T);
      W : Wide_Wide_String (1 .. S'Length);
      L : Natural;
      Error : Boolean; --  ignored
   begin
      System.UTF_Conversions.UTF_8_To_UTF_32 (S, W, L, Error);
      return W (1 .. L);
   end Wide_Wide_Expanded_Name;

end Ada.Tags;
