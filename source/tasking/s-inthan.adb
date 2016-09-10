with Ada.Unchecked_Conversion;
with System.Storage_Barriers;
package body System.Interrupt_Handlers is

   type Node;
   type Node_Access is access Node;
   pragma Atomic (Node_Access);
   type Node is record
      Next : aliased Node_Access;
      Code_Address : Address;
      Is_Static : Boolean;
      pragma Atomic (Is_Static); -- because it's mutable
   end record;
   pragma Suppress_Initialization (Node);

   pragma Compile_Time_Error (
      Standard'Address_Size /= 32 and then Standard'Address_Size /= 64,
      "Standard'Address_Size is neither 32 nor 64");

   --  Use sequentially consistent model because the list and each node's
   --    components should be synchronized.
   Order : constant := Storage_Barriers.ATOMIC_SEQ_CST;

   function atomic_compare_exchange (
      ptr : not null access Node_Access;
      expected : not null access Node_Access;
      desired : Node_Access;
      weak : Boolean := False;
      success_memorder : Integer := Order;
      failure_memorder : Integer := Order)
      return Boolean
      with Import,
         Convention => Intrinsic,
         External_Name =>
            (case Standard'Address_Size is
               when 32 => "__atomic_compare_exchange_4",
               when others => "__atomic_compare_exchange_8");
   pragma Warnings (Off, atomic_compare_exchange);
      --  [gcc-4.8/4.9/5] excessive prototype checking

   List : aliased Node_Access := null;

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
      Expected : aliased Node_Access := List;
   begin
      loop
         New_Node.Next := Expected;
         exit when atomic_compare_exchange (
            List'Access,
            Expected'Access,
            New_Node);
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
            New_Node.Is_Static := False;
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
         Node.Is_Static := True;
         Add (Node);
      else
         Node.Is_Static := True;
      end if;
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
