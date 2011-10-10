pragma Check_Policy (Trace, Off);
with Ada.Tags.Inside;
with System.Address_To_Named_Access_Conversions;
with System.Shared_Locking;
with System.Soft_Links;
with System.Standard_Library;
with System.Storage_Elements;
with System.Termination;
with System.Unwind.Raising;
with System.Unwind.Standard;
package body System.Finalization_Implementation is
   pragma Suppress (All_Checks);
   use type Finalization_Root.Finalizable_Ptr;
   use type Standard_Library.Exception_Data_Ptr;
   use type Storage_Elements.Storage_Offset;

   --  local / finalize global controlled objects (s-finimp.ads)
   procedure Finalize_Global_List;
   procedure Finalize_Global_List is
   begin
      pragma Check (Trace, Ada.Debug.Put ("enter"));
      Soft_Links.Abort_Defer.all;
      Finalize_List (Global_Final_List);
      Soft_Links.Abort_Undefer.all;
      pragma Check (Trace, Ada.Debug.Put ("leave"));
   end Finalize_Global_List;

   Finalize_Global_List_Registered : Boolean := False;

   procedure Register_Finalize_Global_List;
   procedure Register_Finalize_Global_List is
   begin
      if not Finalize_Global_List_Registered then
         pragma Check (Trace, Ada.Debug.Put ("enter"));
         Finalize_Global_List_Registered := True;
         Termination.Register_Exit (Finalize_Global_List'Access);
         pragma Check (Trace, Ada.Debug.Put ("leave"));
      end if;
   end Register_Finalize_Global_List;

   --  local (s-finimp.adb)
   procedure Raise_From_Finalize (
      L : Finalization_Root.Finalizable_Ptr;
      From_Abort : Boolean;
      E_Occ : Unwind.Exception_Occurrence);
   procedure Raise_From_Finalize (
      L : Finalization_Root.Finalizable_Ptr;
      From_Abort : Boolean;
      E_Occ : Unwind.Exception_Occurrence)
   is
      I : Finalization_Root.Finalizable_Ptr := L;
   begin
      while I /= null loop
         declare
            Next : constant Finalization_Root.Finalizable_Ptr := I.Next;
         begin
            begin
               Finalization_Root.Finalize (I.all); --  dynamic dispatching
            exception
               when others => null; --  already in exception handler
            end;
            I := Next;
         end;
      end loop;
      if not From_Abort then
         Unwind.Raising.Raise_From_Controlled_Operation (E_Occ);
      end if;
   end Raise_From_Finalize;

   --  local (s-finimp.adb)
   type RC_Ptr is access all Record_Controller;
   function Get_Deep_Controller (Obj : Address) return RC_Ptr;
   function Get_Deep_Controller (Obj : Address) return RC_Ptr is
      package RC_Conv is new Address_To_Named_Access_Conversions (
         Record_Controller,
         RC_Ptr);
      package Finalizable_Conv is new Address_To_Named_Access_Conversions (
         Finalization_Root.Finalizable,
         Finalization_Root.Finalizable_Ptr);
      The_Tag : Ada.Tags.Tag := Finalizable_Conv.To_Pointer (Obj)'Tag;
      Offset : Storage_Elements.Storage_Offset :=
         Ada.Tags.Inside.Get_RC_Offset (The_Tag);
   begin
      while Offset = -2 loop
         The_Tag := Ada.Tags.Parent_Tag (The_Tag);
         Offset := Ada.Tags.Inside.Get_RC_Offset (The_Tag);
      end loop;
      if Offset = 0 then
         return null;
      elsif Offset > 0 then
         return RC_Conv.To_Pointer (Obj + Offset);
      else
         declare
            type Faked_Record_Controller is record
               Tag, Prec, Next : Address;
            end record;
            pragma Suppress_Initialization (Faked_Record_Controller);
            type Parent_Type is new Storage_Elements.Storage_Array (
               1 ..
               (Ada.Tags.Inside.Parent_Size (Obj, The_Tag) +
               Standard'Storage_Unit - 1) / Standard'Storage_Unit);
            for Parent_Type'Alignment use Address'Alignment;
            type Faked_Type_Of_Obj is record
               Parent : Parent_Type;
               Controller : Faked_Record_Controller;
            end record;
            pragma Suppress_Initialization (Faked_Type_Of_Obj);
            type Obj_Ptr is access all Faked_Type_Of_Obj;
            package FTOO_Conv is new Address_To_Named_Access_Conversions (
               Faked_Type_Of_Obj,
               Obj_Ptr);
         begin
            return RC_Conv.To_Pointer (
               FTOO_Conv.To_Pointer (Obj).Controller'Address);
         end;
      end if;
   end Get_Deep_Controller;

   procedure Attach_To_Final_List (
      L : in out Finalization_Root.Finalizable_Ptr;
      Obj : in out Finalization_Root.Finalizable;
      Nb_Link : Short_Short_Integer) is
   begin
      Register_Finalize_Global_List; --  regist here
      case Nb_Link is
         when 0 => --  ???
            null;
         when 1 => --  normal object
            Obj.Next := L;
            L := Obj'Unchecked_Access;
         when 2 => --  dynamic allocated object
            pragma Assert (L /= null
               and then L.all'Address /= Collection_Finalization_Started);
            Shared_Locking.Enter;
            Obj.Next := L.Next;
            Obj.Prev := L.Next.Prev;
            L.Next.Prev := Obj'Unchecked_Access;
            L.Next := Obj'Unchecked_Access;
            Shared_Locking.Leave;
         when 3 => --  return
            declare
               P : Finalization_Root.Finalizable_Ptr := Obj'Unchecked_Access;
            begin
               while P.Next /= null loop
                  P := P.Next;
               end loop;
               P.Next := L;
               L := Obj'Unchecked_Access;
            end;
         when 4 => --  Finalize_Storage_Only ?
            Obj.Prev := null;
            Obj.Next := null;
         when others =>
            pragma Assert (False);
            null;
      end case;
   end Attach_To_Final_List;

   procedure Detach_From_Final_List (
      Obj : in out Finalization_Root.Finalizable) is
   begin
      if Obj.Next /= null and then Obj.Prev /= null then
         --  dynamic allocated object
         Shared_Locking.Enter;
         Obj.Next.Prev := Obj.Prev;
         Obj.Prev.Next := Obj.Next;
         Obj.Next := null;
         Obj.Prev := null;
         Shared_Locking.Leave;
      end if;
   end Detach_From_Final_List;

   procedure Deep_Tag_Attach (
      L : in out Finalization_Root.Finalizable_Ptr;
      A : Address;
      B : Short_Short_Integer)
   is
      Controller : constant RC_Ptr := Get_Deep_Controller (A);
   begin
      if Controller /= null then
         Attach_To_Final_List (L, Controller.all, B);
      end if;
      --  A points object that inherites Root_Controlled or not ?
      if Ada.Tags.Inside.CW_Membership (
         A,
         Finalization_Root.Root_Controlled'Tag)
      then
         declare
            package Finalizable_Conv is
               new Address_To_Named_Access_Conversions (
                  Finalization_Root.Finalizable,
                  Finalization_Root.Finalizable_Ptr);
         begin
            Attach_To_Final_List (L, Finalizable_Conv.To_Pointer (A).all, B);
         end;
      end if;
   end Deep_Tag_Attach;

   procedure Finalize_One (Obj : in out Finalization_Root.Finalizable) is
   begin
      Detach_From_Final_List (Obj);
      Finalization_Root.Finalize (Obj); --  dynamic dispatching
   exception
      when others =>
         declare
            TLS : constant not null Soft_Links.Task_Local_Storage_Access :=
               Soft_Links.Get_Task_Local_Storage.all;
            E : Unwind.Exception_Occurrence;
         begin
            Unwind.Save_Occurrence_No_Private (E, TLS.Current_Exception);
            Unwind.Raising.Raise_From_Controlled_Operation (E);
         end;
   end Finalize_One;

   procedure Finalize_List (L : Finalization_Root.Finalizable_Ptr) is
      TLS : constant not null Soft_Links.Task_Local_Storage_Access :=
         Soft_Links.Get_Task_Local_Storage.all;
      From_Abort : constant Boolean := TLS.Current_Exception.Id =
         Unwind.Standard.Abort_Signal'Access;
      I : Finalization_Root.Finalizable_Ptr := L;
   begin
      while I /= null loop
         declare
            Next : constant Finalization_Root.Finalizable_Ptr := I.Next;
         begin
            begin
               Finalization_Root.Finalize (I.all); --  dynamic dispatching
            exception
               when others =>
                  declare
                     E : Unwind.Exception_Occurrence;
                  begin
                     Unwind.Save_Occurrence_No_Private (
                        E,
                        TLS.Current_Exception);
                     Raise_From_Finalize (Next, From_Abort, E);
                  end;
            end;
            I := Next;
         end;
      end loop;
   end Finalize_List;

   procedure Move_Final_List (
      From : in out Finalization_Root.Finalizable_Ptr;
      To : Finalizable_Ptr_Ptr) is
   begin
      Soft_Links.Abort_Defer.all;
      Attach_To_Final_List (To.all, From.all, 3);
      From := null;
      Soft_Links.Abort_Undefer.all;
   end Move_Final_List;

   overriding procedure Finalize (Object : in out Limited_Record_Controller) is
   begin
      Finalize_List (Object.F);
   end Finalize;

   overriding procedure Initialize (Object : in out Record_Controller) is
   begin
      Object.My_Address := Object'Address;
   end Initialize;

   overriding procedure Adjust (Object : in out Record_Controller) is
      First_Comp : Finalization_Root.Finalizable_Ptr;
      My_Offset : constant Storage_Elements.Storage_Offset :=
         Object.My_Address - Object'Address;
      procedure Ptr_Adjust (Ptr : in out Finalization_Root.Finalizable_Ptr);
      procedure Ptr_Adjust (Ptr : in out Finalization_Root.Finalizable_Ptr) is
         package Finalizable_Conv is new Address_To_Named_Access_Conversions (
            Finalization_Root.Finalizable,
            Finalization_Root.Finalizable_Ptr);
      begin
         if Ptr /= null then
            Ptr := Finalizable_Conv.To_Pointer (
               Finalizable_Conv.To_Address (Ptr) - My_Offset);
         end if;
      end Ptr_Adjust;
      procedure Reverse_Adjust (P : Finalization_Root.Finalizable_Ptr);
      procedure Reverse_Adjust (P : Finalization_Root.Finalizable_Ptr) is
      begin
         if P /= null then
            Ptr_Adjust (P.Next);
            Reverse_Adjust (P.Next);
            Finalization_Root.Adjust (P.all); --  dynamic dispatching
            Object.F := P;
         end if;
      end Reverse_Adjust;
   begin
      First_Comp := Object.F;
      Object.F := null;
      Ptr_Adjust (First_Comp);
      Reverse_Adjust (First_Comp);
      Object.My_Address := Object'Address;
   exception
      when others => -- it could NOT handle except Ada exception...
         Finalize (Object);
         raise;
   end Adjust;

end System.Finalization_Implementation;
