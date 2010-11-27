pragma Check_Policy (Trace, Off);
with Ada.Unchecked_Conversion;
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
         null; --  already added
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

   function Get_Delegation (Object : System.Address; Interface_Tag : Tag)
      return System.Address
   is
      function Cast is new Unchecked_Conversion (System.Address, Tag_Ptr);
      T : constant Tag := Cast (Object).all;
      D : constant D_Node_Access := D_Find (Delegating_Map, T);
   begin
      if D = null then
         return System.Null_Address;
      else
         declare
            I : constant I_Node_Access := I_Find (D.Map, Interface_Tag);
         begin
            if I = null then
               return System.Null_Address;
            else
               return I.Get (Object);
            end if;
         end;
      end if;
   end Get_Delegation;

   procedure Register_Delegation (
      T : Tag;
      Interface_Tag : Tag;
      Get : not null access function (Object : System.Address)
         return System.Address)
   is
      D : D_Node_Access;
   begin
      D_Insert (Delegating_Map, T, D);
      I_Insert (D.Map, Interface_Tag, Get);
   end Register_Delegation;

   package body Implements is

      function F (Object : System.Address) return System.Address is
         type T_Ref is access all T'Class;
         function Cast is new Unchecked_Conversion (System.Address, T_Ref);
         type I_Ref is access all I'Class;
         function Cast is new Unchecked_Conversion (I_Ref, System.Address);
      begin
         return Cast (I_Ref (Get (Cast (Object))));
      end F;

   begin
      Tags.Get_Delegation := Get_Delegation'Access;
      Register_Delegation (T'Tag, I'Tag, F'Access);
   end Implements;

end Ada.Tags.Delegating;
