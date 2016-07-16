with Ada.Exceptions;
with Ada.Unchecked_Deallocation;
with System.Address_To_Named_Access_Conversions;
with System.Shared_Locking;
package body System.Finalization_Masters is
   pragma Suppress (All_Checks);
   use type Storage_Barriers.Flag;

   procedure Free is new Ada.Unchecked_Deallocation (FM_List, FM_List_Access);

   procedure Initialize_List (
      List : not null FM_List_Access);

   procedure Initialize_List (
      List : not null FM_List_Access) is
   begin
      List.Objects.Next := List.Objects'Access;
      List.Objects.Prev := List.Objects'Access;
   end Initialize_List;

   procedure Finalize_List (
      List : not null FM_List_Access;
      Raised : in out Boolean;
      X : in out Ada.Exceptions.Exception_Occurrence);

   procedure Finalize_List (
      List : not null FM_List_Access;
      Raised : in out Boolean;
      X : in out Ada.Exceptions.Exception_Occurrence) is
   begin
      while List.Objects.Next /= List.Objects'Unchecked_Access loop
         declare
            package Conv is
               new Address_To_Named_Access_Conversions (FM_Node, FM_Node_Ptr);
            Curr_Ptr : constant FM_Node_Ptr := List.Objects.Next;
            Obj_Addr : constant Address :=
               Conv.To_Address (Curr_Ptr) + Header_Size;
         begin
            Detach_Unprotected (Curr_Ptr);
            begin
               List.Finalize_Address (Obj_Addr);
            exception
               when E : others =>
                  if not Raised then
                     Raised := True;
                     Ada.Exceptions.Save_Occurrence (X, E);
                  end if;
            end;
         end;
      end loop;
   end Finalize_List;

   procedure Get_List_Unprotected (
      Master : in out Finalization_Master'Class;
      Fin_Addr_Ptr : Finalize_Address_Ptr;
      List : out FM_List_Access);

   procedure Get_List_Unprotected (
      Master : in out Finalization_Master'Class;
      Fin_Addr_Ptr : Finalize_Address_Ptr;
      List : out FM_List_Access) is
   begin
      if Master.List.Finalize_Address = null then
         Master.List.Finalize_Address := Fin_Addr_Ptr;
         List := Master.List'Unchecked_Access;
      else
         declare
            I : FM_List_Access := Master.List'Unchecked_Access;
         begin
            while I /= null loop
               if I.Finalize_Address = Fin_Addr_Ptr then
                  List := I;
                  return;
               end if;
               I := I.Next;
            end loop;
         end;
         declare
            New_List : constant FM_List_Access := new FM_List;
         begin
            Initialize_List (New_List);
            New_List.Finalize_Address := Fin_Addr_Ptr;
            New_List.Next := Master.List.Next;
            Master.List.Next := New_List;
            List := New_List;
         end;
      end if;
   end Get_List_Unprotected;

   --  implementation

   procedure Attach_Unprotected (N, L : not null FM_Node_Ptr) is
   begin
      L.Next.Prev := N;
      N.Next := L.Next;
      L.Next := N;
      N.Prev := L;
   end Attach_Unprotected;

   procedure Attach (N, L : not null FM_Node_Ptr) is
   begin
      Shared_Locking.Enter;
      Attach_Unprotected (N, L);
      Shared_Locking.Leave;
   end Attach;

   procedure Detach_Unprotected (N : not null FM_Node_Ptr) is
   begin
      if N.Prev /= null and then N.Next /= null then
         N.Prev.Next := N.Next;
         N.Next.Prev := N.Prev;
         N.Prev := null;
         N.Next := null;
      end if;
   end Detach_Unprotected;

   procedure Detach (N : not null FM_Node_Ptr) is
   begin
      Shared_Locking.Enter;
      Detach_Unprotected (N);
      Shared_Locking.Leave;
   end Detach;

   function Objects_Unprotected (
      Master : aliased in out Finalization_Master'Class;
      Fin_Addr_Ptr : Finalize_Address_Ptr)
      return FM_Node_Ptr
   is
      List : FM_List_Access;
   begin
      Get_List_Unprotected (Master, Fin_Addr_Ptr, List);
      return List.Objects'Access;
   end Objects_Unprotected;

   function Finalization_Started (Master : Finalization_Master'Class)
      return Boolean is
   begin
      return Storage_Barriers.atomic_load (
         Master.Finalization_Started'Access) /= 0;
   end Finalization_Started;

   procedure Set_Finalize_Address_Unprotected (
      Master : in out Finalization_Master'Class;
      Fin_Addr_Ptr : Finalize_Address_Ptr)
   is
      Dummy : FM_List_Access;
   begin
      Get_List_Unprotected (Master, Fin_Addr_Ptr, Dummy);
   end Set_Finalize_Address_Unprotected;

   procedure Set_Finalize_Address (
      Master : in out Finalization_Master'Class;
      Fin_Addr_Ptr : Finalize_Address_Ptr) is
   begin
      Shared_Locking.Enter;
      Set_Finalize_Address_Unprotected (Master, Fin_Addr_Ptr);
      Shared_Locking.Leave;
   end Set_Finalize_Address;

   overriding procedure Initialize (Object : in out Finalization_Master) is
   begin
      Storage_Barriers.atomic_clear (Object.Finalization_Started'Access);
      Initialize_List (Object.List'Unchecked_Access);
      Object.List.Finalize_Address := null;
      Object.List.Next := null;
   end Initialize;

   overriding procedure Finalize (Object : in out Finalization_Master) is
   begin
      if not Storage_Barriers.atomic_test_and_set (
         Object.Finalization_Started'Access)
      then
         declare
            Raised : Boolean := False;
            X : Ada.Exceptions.Exception_Occurrence;
         begin
            Finalize_List (Object.List'Unchecked_Access, Raised, X);
            declare
               I : FM_List_Access := Object.List.Next;
            begin
               while I /= null loop
                  declare
                     Next : constant FM_List_Access := I.Next;
                  begin
                     Finalize_List (I, Raised, X);
                     Free (I);
                     I := Next;
                  end;
               end loop;
            end;
            if Raised then
               Ada.Exceptions.Reraise_Nonnull_Occurrence (X);
            end if;
         end;
      end if;
   end Finalize;

   function Base_Pool (Master : Finalization_Master'Class)
      return Any_Storage_Pool_Ptr is
   begin
      return Master.Base_Pool;
   end Base_Pool;

   procedure Set_Base_Pool (
      Master : in out Finalization_Master'Class;
      Pool_Ptr : Any_Storage_Pool_Ptr) is
   begin
      Master.Base_Pool := Pool_Ptr;
   end Set_Base_Pool;

end System.Finalization_Masters;
