with System.Address_To_Named_Access_Conversions;
with System.Memory;
with System.Soft_Links;
package body System.Secondary_Stack is
   pragma Suppress (All_Checks);
   use type Storage_Elements.Integer_Address;
   use type Storage_Elements.Storage_Offset;

   Expanding : constant := 1; -- connecting next page
   pragma Warnings (Off, Expanding);

   function Ceiling_Page_Size (Required : Storage_Elements.Storage_Count)
      return Storage_Elements.Storage_Count;
   function Ceiling_Page_Size (Required : Storage_Elements.Storage_Count)
      return Storage_Elements.Storage_Count
   is
      Alignment : constant Storage_Elements.Integer_Address :=
         Storage_Elements.Integer_Address (Memory.Page_Size);
   begin
      return Storage_Elements.Storage_Offset (
         Storage_Elements.Integer_Address'Mod (Required)
         + Storage_Elements.Integer_Address'Mod (-Required) mod Alignment);
   end Ceiling_Page_Size;

   package Conv is new Address_To_Named_Access_Conversions (
      Block,
      Block_Access);
   function Cast (X : Address) return Block_Access
      renames Conv.To_Pointer;

   type Unsigned is mod 2 ** Integer'Size;

   function clz (X : Storage_Elements.Integer_Address) return Unsigned;
   pragma Import (Intrinsic, clz, "__builtin_clzl");

   procedure unreachable;
   pragma Import (Intrinsic, unreachable, "__builtin_unreachable");

   --  implementation

   procedure SS_Allocate (
      Addr : out Address;
      Storage_Size : Storage_Elements.Storage_Count)
   is
      Header_Size : constant Storage_Elements.Storage_Count :=
         Block'Size / Standard'Storage_Unit;
      TLS : constant Soft_Links.Task_Local_Storage_Access :=
         Soft_Links.Get_Task_Local_Storage.all;
      Top : constant Address := TLS.Secondary_Stack;
      Alignment : Storage_Elements.Integer_Address;
      Mask : Storage_Elements.Integer_Address;
      Aligned_Top_Used : Address;
   begin
      --  alignment
      if Storage_Size <= Standard'Maximum_Alignment / 2 then
         declare
--          H : constant Integer := Standard'Address_Size - 1 - Integer (
--             clz (Storage_Elements.Integer_Address (Storage_Size) * 2 - 1));
            H : constant Integer := Integer (
               clz (Storage_Elements.Integer_Address (Storage_Size) * 2 - 1)
               xor (Standard'Address_Size - 1)); -- cancel wordy xor
         begin
            if H not in 0 .. Standard'Address_Size - 1 then
               unreachable; -- assume H is in address-width
            end if;
            Alignment := Storage_Elements.Shift_Left (1, H);
         end;
      else
         Alignment := Standard'Maximum_Alignment;
      end if;
      Mask := Alignment - 1;
      if Top /= Null_Address then
         --  when top block is empty and previous block has enough space
         if Cast (Top).Used = Top + Header_Size
            and then Cast (Top).Previous /= Null_Address
         then
            declare
               Previous : constant Address := Cast (Top).Previous;
               Aligned_Previous_Used : constant Address := Address (
                  (Storage_Elements.Integer_Address (Cast (Previous).Used)
                     + Mask)
                  and not Mask);
            begin
               if Aligned_Previous_Used + Storage_Size <=
                  Cast (Previous).Limit
               then
                  TLS.Secondary_Stack := Previous;
                  Memory.Unmap (Top, Cast (Top).Limit - Top);
                  Addr := Aligned_Previous_Used;
                  Cast (Previous).Used := Addr + Storage_Size;
                  return;
               end if;
            end;
         end if;
         --  when top block has enough space
         Aligned_Top_Used := Address (
            (Storage_Elements.Integer_Address (Cast (Top).Used) + Mask)
            and not Mask);
         if Aligned_Top_Used + Storage_Size <= Cast (Top).Limit then
            Addr := Aligned_Top_Used;
            Cast (Top).Used := Addr + Storage_Size;
            return;
         end if;
         --  try expanding top block
         if Expanding /= 0 then
            declare
               Additional_Block_Size : constant
                  Storage_Elements.Storage_Count := Ceiling_Page_Size (
                     Storage_Size - (Cast (Top).Limit - Aligned_Top_Used));
               Additional_Block : constant Address := Memory.Map (
                  Cast (Top).Limit,
                  Additional_Block_Size,
                  Raise_On_Error => False);
            begin
               if Additional_Block = Cast (Top).Limit then
                  Cast (Top).Limit := Cast (Top).Limit + Additional_Block_Size;
                  Addr := Aligned_Top_Used;
                  Cast (Top).Used := Addr + Storage_Size;
                  return;
               end if;
            end;
         end if;
         --  top block is not enough, then free it if unused
         if Cast (Top).Used = Top + Header_Size then
            TLS.Secondary_Stack := Cast (Top).Previous;
            Memory.Unmap (Top, Cast (Top).Limit - Top);
         end if;
      end if;
      --  new block
      declare
         Default_Block_Size : constant := 10 * 1024;
         Aligned_Header_Size : constant Storage_Elements.Storage_Count :=
            Storage_Elements.Storage_Count (
               (Storage_Elements.Integer_Address (Header_Size) + Mask)
               and not Mask);
         Block_Size : constant Storage_Elements.Storage_Count :=
            Ceiling_Page_Size (
               Storage_Elements.Storage_Offset'Max (
                  Default_Block_Size,
                  Storage_Size + Aligned_Header_Size));
         New_Block : constant Address := Memory.Map (Block_Size);
      begin
         Cast (New_Block).Previous := TLS.Secondary_Stack;
         TLS.Secondary_Stack := New_Block;
         Cast (New_Block).Limit := New_Block + Block_Size;
         Addr := New_Block + Aligned_Header_Size;
         Cast (New_Block).Used := Addr + Storage_Size;
      end;
   end SS_Allocate;

   function SS_Mark return Mark_Id is
      Header_Size : constant Storage_Elements.Storage_Count :=
         Block'Size / Standard'Storage_Unit;
      Top : constant Address :=
         Soft_Links.Get_Task_Local_Storage.all.Secondary_Stack;
   begin
      if Top = Null_Address then
         return (Sstk => Null_Address, Sptr => 0);
      elsif Cast (Top).Used = Top + Header_Size then
         declare
            Previous : constant Address := Cast (Top).Previous;
         begin
            if Previous = Null_Address then
               return (Sstk => Null_Address, Sptr => 0);
            else
               return (Sstk => Previous, Sptr => Cast (Previous).Used);
            end if;
         end;
      else
         return (Sstk => Top, Sptr => Cast (Top).Used);
      end if;
   end SS_Mark;

   procedure SS_Release (M : Mark_Id) is
      Header_Size : constant Storage_Elements.Storage_Count :=
         Block'Size / Standard'Storage_Unit;
      TLS : constant Soft_Links.Task_Local_Storage_Access :=
         Soft_Links.Get_Task_Local_Storage.all;
   begin
      if TLS.Secondary_Stack /= Null_Address then
         loop
            declare
               Top : constant Address := TLS.Secondary_Stack;
            begin
               if Top = M.Sstk then
                  Cast (Top).Used := M.Sptr;
                  exit;
               elsif Cast (Top).Previous = M.Sstk
                  and then (M.Sstk = Null_Address
                     or else M.Sptr = Cast (M.Sstk).Used)
               then
                  --  leave Limit unused block
                  Cast (Top).Used := Top + Header_Size;
                  exit;
               end if;
               TLS.Secondary_Stack := Cast (Top).Previous;
               Memory.Unmap (Top, Cast (Top).Limit - Top);
            end;
         end loop;
      end if;
   end SS_Release;

   procedure Clear is
      TLS : constant Soft_Links.Task_Local_Storage_Access :=
         Soft_Links.Get_Task_Local_Storage.all;
   begin
      while TLS.Secondary_Stack /= Null_Address loop
         declare
            Top : constant Address := TLS.Secondary_Stack;
         begin
            TLS.Secondary_Stack := Cast (Top).Previous;
            Memory.Unmap (Top, Cast (Top).Limit - Top);
         end;
      end loop;
   end Clear;

end System.Secondary_Stack;
