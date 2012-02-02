pragma Check_Policy (Trace, Off);
with Ada.Unchecked_Conversion;
with System.Shared_Locking;
package body Ada.Tags.Delegating is
   pragma Suppress (All_Checks);
   use type System.Address;

   type Delegate is access function (Object : System.Address)
      return System.Address;

   type I_Node;
   type I_Node_Access is access I_Node;
   type I_Node is record
      Left, Right : I_Node_Access;
      Interface_Tag : Tag;
      Get : not null Delegate;
   end record;
   pragma Suppress_Initialization (I_Node);

   procedure I_Insert (
      Node : in out I_Node_Access;
      Interface_Tag : Tag;
      Get : not null access function (Object : System.Address)
         return System.Address);
   procedure I_Insert (
      Node : in out I_Node_Access;
      Interface_Tag : Tag;
      Get : not null access function (Object : System.Address)
         return System.Address)
   is
      function Cast is new Unchecked_Conversion (Tag, System.Address);
   begin
      if Node = null then
         pragma Check (Trace, Debug.Put ("add"));
         Node := new I_Node'(
            Left => null,
            Right => null,
            Interface_Tag => Interface_Tag,
            Get => Get);
      elsif Cast (Node.Interface_Tag) > Cast (Interface_Tag) then
         I_Insert (Node.Left, Interface_Tag, Get);
      elsif Cast (Node.Interface_Tag) < Cast (Interface_Tag) then
         I_Insert (Node.Right, Interface_Tag, Get);
      else
         null; -- already added
      end if;
   end I_Insert;

   function I_Find (Node : I_Node_Access; Interface_Tag : Tag)
      return I_Node_Access;
   function I_Find (Node : I_Node_Access; Interface_Tag : Tag)
      return I_Node_Access
   is
      function Cast is new Unchecked_Conversion (Tag, System.Address);
   begin
      if Node = null then
         return null;
      elsif Cast (Node.Interface_Tag) > Cast (Interface_Tag) then
         return I_Find (Node.Left, Interface_Tag);
      elsif Cast (Node.Interface_Tag) < Cast (Interface_Tag) then
         return I_Find (Node.Right, Interface_Tag);
      else
         return Node;
      end if;
   end I_Find;

   type D_Node;
   type D_Node_Access is access D_Node;
   type D_Node is record
      Left, Right : D_Node_Access;
      Object_Tag : Tag;
      Map : I_Node_Access;
   end record;
   pragma Suppress_Initialization (D_Node);

   procedure D_Insert (
      Node : in out D_Node_Access;
      T : Tag;
      Result : out D_Node_Access);
   procedure D_Insert (
      Node : in out D_Node_Access;
      T : Tag;
      Result : out D_Node_Access)
   is
      function Cast is new Unchecked_Conversion (Tag, System.Address);
   begin
      if Node = null then
         Node := new D_Node'(
            Left => null,
            Right => null,
            Object_Tag => T,
            Map => null);
         Result := Node;
      elsif Cast (Node.Object_Tag) > Cast (T) then
         D_Insert (Node.Left, T, Result);
      elsif Cast (Node.Object_Tag) < Cast (T) then
         D_Insert (Node.Right, T, Result);
      else
         Result := Node;
      end if;
   end D_Insert;

   function D_Find (Node : D_Node_Access; T : Tag) return D_Node_Access;
   function D_Find (Node : D_Node_Access; T : Tag) return D_Node_Access is
      function Cast is new Unchecked_Conversion (Tag, System.Address);
   begin
      if Node = null then
         return null;
      elsif Cast (Node.Object_Tag) > Cast (T) then
         return D_Find (Node.Left, T);
      elsif Cast (Node.Object_Tag) < Cast (T) then
         return D_Find (Node.Right, T);
      else
         return Node;
      end if;
   end D_Find;

   Delegating_Map : D_Node_Access := null;

   --  implementation

   function Get_Delegation (Object : System.Address; Interface_Tag : Tag)
      return System.Address
   is
      function Cast is new Unchecked_Conversion (System.Address, Tag_Ptr);
      Result : System.Address := System.Null_Address;
      T : Tag := Cast (Object).all;
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
         return System.Address)
   is
      D : D_Node_Access;
   begin
      System.Shared_Locking.Enter;
      D_Insert (Delegating_Map, T, D);
      I_Insert (D.Map, Interface_Tag, Get);
      System.Shared_Locking.Leave;
   end Register_Delegation;

   type Get_Access is
      access function (Object : System.Address) return System.Address;

   procedure Implements is
      function Cast is new Unchecked_Conversion (System.Address, Get_Access);
   begin
      Tags.Get_Delegation := Get_Delegation'Access;
      Register_Delegation (T'Tag, I'Tag, Cast (Get'Code_Address));
   end Implements;

end Ada.Tags.Delegating;
