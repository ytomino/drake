pragma Check_Policy (Trace => Ignore);
with Ada.Unchecked_Conversion;
with System.Address_To_Named_Access_Conversions;
with System.Shared_Locking;
package body Ada.Tags.Delegating is
   pragma Suppress (All_Checks);
   use type System.Address;

   package Tag_Conv is
      new System.Address_To_Named_Access_Conversions (Dispatch_Table, Tag);
   package Tag_Ptr_Conv is
      new System.Address_To_Named_Access_Conversions (Tag, Tag_Ptr);

   type Delegate is access function (Object : System.Address)
      return System.Address;

   type I_Node;
   type I_Node_Access is access I_Node;
   type I_Node is record
      Left, Right : aliased I_Node_Access;
      Interface_Tag : Tag;
      Get : not null Delegate;
   end record;
   pragma Suppress_Initialization (I_Node);

   procedure I_Insert (
      Node : aliased in out I_Node_Access;
      Interface_Tag : Tag;
      Get : not null access function (Object : System.Address)
         return System.Address);
   procedure I_Insert (
      Node : aliased in out I_Node_Access;
      Interface_Tag : Tag;
      Get : not null access function (Object : System.Address)
         return System.Address)
   is
      function "+" (X : Tag) return System.Address
         renames Tag_Conv.To_Address;
      type I_Node_Access_Access is access all I_Node_Access;
      for I_Node_Access_Access'Storage_Size use 0;
      Current : not null I_Node_Access_Access := Node'Access;
   begin
      loop
         if Current.all = null then
            pragma Check (Trace, Debug.Put ("add"));
            Current.all := new I_Node'(
               Left => null,
               Right => null,
               Interface_Tag => Interface_Tag,
               Get => Get);
            exit;
         elsif +Current.all.Interface_Tag > +Interface_Tag then
            Current := Current.all.Left'Access;
         elsif +Current.all.Interface_Tag < +Interface_Tag then
            Current := Current.all.Right'Access;
         else
            exit; -- already added
         end if;
      end loop;
   end I_Insert;

   function I_Find (Node : I_Node_Access; Interface_Tag : Tag)
      return I_Node_Access;
   function I_Find (Node : I_Node_Access; Interface_Tag : Tag)
      return I_Node_Access
   is
      function "+" (X : Tag) return System.Address
         renames Tag_Conv.To_Address;
      Current : I_Node_Access := Node;
   begin
      while Current /= null loop
         if +Current.Interface_Tag > +Interface_Tag then
            Current := Current.Left;
         elsif +Current.Interface_Tag < +Interface_Tag then
            Current := Current.Right;
         else
            return Current; -- found
         end if;
      end loop;
      return null; -- not found
   end I_Find;

   type D_Node;
   type D_Node_Access is access D_Node;
   type D_Node is record
      Left, Right : aliased D_Node_Access;
      Object_Tag : Tag;
      Map : aliased I_Node_Access;
   end record;
   pragma Suppress_Initialization (D_Node);

   procedure D_Insert (
      Node : aliased in out D_Node_Access;
      T : Tag;
      Result : out D_Node_Access);
   procedure D_Insert (
      Node : aliased in out D_Node_Access;
      T : Tag;
      Result : out D_Node_Access)
   is
      function "+" (X : Tag) return System.Address
         renames Tag_Conv.To_Address;
      type D_Node_Access_Access is access all D_Node_Access;
      for D_Node_Access_Access'Storage_Size use 0;
      Current : not null D_Node_Access_Access := Node'Access;
   begin
      loop
         if Current.all = null then
            Current.all := new D_Node'(
               Left => null,
               Right => null,
               Object_Tag => T,
               Map => null);
            Result := Current.all;
            exit;
         elsif +Current.all.Object_Tag > +T then
            Current := Current.all.Left'Access;
         elsif +Current.all.Object_Tag < +T then
            Current := Current.all.Right'Access;
         else
            Result := Current.all;
            exit; -- already added
         end if;
      end loop;
   end D_Insert;

   function D_Find (Node : D_Node_Access; T : Tag) return D_Node_Access;
   function D_Find (Node : D_Node_Access; T : Tag) return D_Node_Access is
      function "+" (X : Tag) return System.Address
         renames Tag_Conv.To_Address;
      Current : D_Node_Access := Node;
   begin
      while Current /= null loop
         if +Current.Object_Tag > +T then
            Current := Current.Left;
         elsif +Current.Object_Tag < +T then
            Current := Current.Right;
         else
            return Current; -- found
         end if;
      end loop;
      return null; -- not found
   end D_Find;

   Delegating_Map : aliased D_Node_Access := null;

   function Get_Delegation (Object : System.Address; Interface_Tag : Tag)
      return System.Address;
   function Get_Delegation (Object : System.Address; Interface_Tag : Tag)
      return System.Address
   is
      Result : System.Address := System.Null_Address;
      T : Tag := Tag_Ptr_Conv.To_Pointer (Object).all;
   begin
      System.Shared_Locking.Enter;
      Search : loop
         declare
            D : constant D_Node_Access := D_Find (Delegating_Map, T);
         begin
            if D /= null then
               declare
                  I : constant I_Node_Access := I_Find (D.Map, Interface_Tag);
               begin
                  exit Search when I = null;
                  Result := I.Get (Object);
                  exit Search;
               end;
            end if;
         end;
         T := Parent_Tag (T);
         exit Search when T = No_Tag;
      end loop Search;
      System.Shared_Locking.Leave;
      return Result;
   end Get_Delegation;

   procedure Register_Delegation (
      T : Tag;
      Interface_Tag : Tag;
      Get : not null access function (Object : System.Address)
         return System.Address);
   procedure Register_Delegation (
      T : Tag;
      Interface_Tag : Tag;
      Get : not null access function (Object : System.Address)
         return System.Address)
   is
      D : D_Node_Access;
   begin
      System.Shared_Locking.Enter;
      D_Insert (Delegating_Map, T, D);
      I_Insert (D.Map, Interface_Tag, Get);
      System.Shared_Locking.Leave;
   end Register_Delegation;

   --  implementation

   type Get_Access is
      access function (Object : System.Address) return System.Address;

   procedure Implements is
      function Cast is new Unchecked_Conversion (System.Address, Get_Access);
   begin
      Tags.Get_Delegation := Get_Delegation'Access;
      Register_Delegation (T'Tag, I'Tag, Cast (Get'Code_Address));
   end Implements;

end Ada.Tags.Delegating;
