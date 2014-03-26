with Ada.Unchecked_Conversion;
package body System.Interrupt_Handlers is

   type Node;
   type Node_Access is access Node;
   pragma Atomic (Node_Access);
   type Node is record
      Next : Node_Access;
      pragma Atomic (Next);
      Code_Address : Address;
      pragma Atomic (Code_Address);
      Is_Static : Boolean;
      pragma Atomic (Is_Static);
   end record;
   pragma Suppress_Initialization (Node);

   List : aliased Node_Access := null;
   pragma Atomic (List);

   function Find (Code_Address : Address) return Node_Access;
   function Find (Code_Address : Address) return Node_Access is
      I : Node_Access := List;
   begin
      while I /= null loop
         if I.Code_Address = Code_Address then
            return I;
         end if;
         I := I.Next;
      end loop;
      return null;
   end Find;

   procedure Add (New_Node : not null Node_Access);
   procedure Add (New_Node : not null Node_Access) is
      Node_Access_Size : constant Integer := Node_Access'Size;
      pragma Warnings (Off, Node_Access_Size);
   begin
      loop
         New_Node.Next := List;
         if Node_Access_Size = 32 then
            declare
               function sync_bool_compare_and_swap_32 (
                  A1 : not null access Node_Access;
                  A2 : Node_Access;
                  A3 : Node_Access)
                  return Boolean;
               pragma Import (Intrinsic, sync_bool_compare_and_swap_32,
                  "__sync_bool_compare_and_swap_4");
               pragma Warnings (Off, sync_bool_compare_and_swap_32);
               --  [gcc-4.8] excessive prototype checking
            begin
               exit when sync_bool_compare_and_swap_32 (
                  List'Access,
                  New_Node.Next,
                  New_Node);
            end;
         else
            declare
               function sync_bool_compare_and_swap_64 (
                  A1 : not null access Node_Access;
                  A2 : Node_Access;
                  A3 : Node_Access)
                  return Boolean;
               pragma Import (Intrinsic, sync_bool_compare_and_swap_64,
                  "__sync_bool_compare_and_swap_8");
               pragma Warnings (Off, sync_bool_compare_and_swap_64);
               --  [gcc-4.8] excessive prototype checking
            begin
               exit when sync_bool_compare_and_swap_64 (
                  List'Access,
                  New_Node.Next,
                  New_Node);
            end;
         end if;
      end loop;
   end Add;

   --  implementation

   procedure Register_Interrupt_Handler (
      Code_Address : Address) is
   begin
      if Find (Code_Address) = null then
         declare
            New_Node : constant not null Node_Access := new Node;
         begin
            New_Node.Code_Address := Code_Address;
            Add (New_Node);
         end;
      end if;
   end Register_Interrupt_Handler;

   procedure Register_Interrupt_Handler (
      Handler : Ada.Interrupts.Parameterless_Handler)
   is
      type Repr is record
         Data : Address;
         Code_Address : Address;
      end record;
      pragma Suppress_Initialization (Repr);
      function Cast is
         new Ada.Unchecked_Conversion (
            Ada.Interrupts.Parameterless_Handler,
            Repr);
   begin
      Register_Interrupt_Handler (Cast (Handler).Code_Address);
   end Register_Interrupt_Handler;

   procedure Set_Static_Handler (
      Code_Address : Address)
   is
      Node : Node_Access := Find (Code_Address);
   begin
      if Node = null then
         Node := new Interrupt_Handlers.Node;
         Node.Code_Address := Code_Address;
         Add (Node);
      end if;
      Node.Is_Static := True;
   end Set_Static_Handler;

   procedure Set_Static_Handler (
      Handler : Ada.Interrupts.Parameterless_Handler)
   is
      type Repr is record
         Data : Address;
         Code_Address : Address;
      end record;
      pragma Suppress_Initialization (Repr);
      function Cast is
         new Ada.Unchecked_Conversion (
            Ada.Interrupts.Parameterless_Handler,
            Repr);
   begin
      Set_Static_Handler (Cast (Handler).Code_Address);
   end Set_Static_Handler;

   function Is_Static_Handler (
      Code_Address : Address)
      return Boolean
   is
      Node : constant Node_Access := Find (Code_Address);
   begin
      return Node = null or else Node.Is_Static;
   end Is_Static_Handler;

   function Is_Static_Handler (
      Handler : Ada.Interrupts.Parameterless_Handler)
      return Boolean
   is
      type Repr is record
         Data : Address;
         Code_Address : Address;
      end record;
      pragma Suppress_Initialization (Repr);
      function Cast is
         new Ada.Unchecked_Conversion (
            Ada.Interrupts.Parameterless_Handler,
            Repr);
   begin
      return Is_Static_Handler (Cast (Handler).Code_Address);
   end Is_Static_Handler;

end System.Interrupt_Handlers;
