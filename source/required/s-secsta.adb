with System.Runtime_Context;
package body System.Secondary_Stack is
   pragma Suppress (All_Checks);
   use type Storage_Elements.Storage_Offset;

   type Unsigned is mod 2 ** Integer'Size;

   function clz (X : Unsigned) return Unsigned
      with Import, Convention => Intrinsic, External_Name => "__builtin_clz";

   procedure unreachable
      with Import,
         Convention => Intrinsic, External_Name => "__builtin_unreachable";
   pragma No_Return (unreachable);

   --  implementation

   procedure SS_Allocate (
      Addr : out Address;
      Storage_Size : Storage_Elements.Storage_Count)
   is
      TLS : constant not null Runtime_Context.Task_Local_Storage_Access :=
         Runtime_Context.Get_Task_Local_Storage;
      Alignment : Storage_Elements.Storage_Count;
   begin
      --  alignment
      if Storage_Size <= Standard'Maximum_Alignment / 2 then
         declare
            H : constant Integer := Integer (
               clz (Unsigned (Storage_Size) * 2 - 1)
               xor (Unsigned'Size - 1)); -- cancel wordy xor
         begin
            if H not in 0 .. Standard'Address_Size - 1 then
               unreachable; -- assume H is in address-width
            end if;
            Alignment := Storage_Elements.Storage_Offset (
               Storage_Elements.Integer_Address'(
                  Storage_Elements.Shift_Left (1, H)));
         end;
      else
         Alignment := Standard'Maximum_Alignment;
      end if;
      Unbounded_Stack_Allocators.Allocate (
         TLS.Secondary_Stack,
         Addr,
         Storage_Size,
         Alignment);
   end SS_Allocate;

   function SS_Mark return Mark_Id is
      TLS : constant not null Runtime_Context.Task_Local_Storage_Access :=
         Runtime_Context.Get_Task_Local_Storage;
   begin
      return Unbounded_Stack_Allocators.Mark (TLS.Secondary_Stack);
   end SS_Mark;

   procedure SS_Release (M : Mark_Id) is
      TLS : constant not null Runtime_Context.Task_Local_Storage_Access :=
         Runtime_Context.Get_Task_Local_Storage;
   begin
      Unbounded_Stack_Allocators.Release (TLS.Secondary_Stack, M);
   end SS_Release;

end System.Secondary_Stack;
