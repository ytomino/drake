with Ada.Unchecked_Conversion;
with System.Memory;
with System.Soft_Links;
package body System.Secondary_Stack is
   pragma Suppress (All_Checks);
   use type Storage_Elements.Storage_Offset;

   type Block is record
      Previous : Address;
      Last : Address; --  Last + 1
      Used : Address;
   end record;
   pragma Suppress_Initialization (Block);
   type Block_Access is access Block;

   function Ceiling_Block_Size (Required : Storage_Elements.Storage_Count)
      return Storage_Elements.Storage_Count;
   function Ceiling_Block_Size (Required : Storage_Elements.Storage_Count)
      return Storage_Elements.Storage_Count
   is
      Header_Size : constant Storage_Elements.Storage_Count :=
         Block'Size / Standard'Storage_Unit;
      Default_Block_Size : constant := 10 * 1024;
      Page_Size : constant Storage_Elements.Storage_Count := Memory.Page_Size;
   begin
      return (Storage_Elements.Storage_Offset'Max (
         Default_Block_Size,
         Required + Header_Size) + Page_Size - 1) / Page_Size * Page_Size;
   end Ceiling_Block_Size;

   function Cast is new Ada.Unchecked_Conversion (
      Address,
      Block_Access);

   procedure SS_Allocate (
      Addr : out Address;
      Storage_Size : Storage_Elements.Storage_Count)
   is
      Header_Size : constant Storage_Elements.Storage_Count :=
         Block'Size / Standard'Storage_Unit;
      TLS : constant Soft_Links.Task_Local_Storage_Access :=
         Soft_Links.Get_Task_Local_Storage.all;
      Top : constant Address := TLS.Secondary_Stack;
   begin
      if Top = Null_Address then
         declare
            Size : constant Storage_Elements.Storage_Count :=
               Ceiling_Block_Size (Storage_Size);
            New_Block : constant Address := Memory.Map (Size);
         begin
            Cast (New_Block).Previous := Null_Address;
            TLS.Secondary_Stack := New_Block;
            Cast (New_Block).Last := New_Block + Size;
            Addr := New_Block + Header_Size;
            Cast (New_Block).Used := Addr + Storage_Size;
         end;
      elsif Cast (Top).Used = Top + Header_Size
         and then Cast (Top).Previous /= Null_Address
         and then Cast (Cast (Top).Previous).Used + Storage_Size <
            Cast (Cast (Top).Previous).Last
      then
         --  use previous block
         declare
            Previous : constant Address := Cast (Top).Previous;
         begin
            TLS.Secondary_Stack := Previous;
            Memory.Unmap (Top, Cast (Top).Last - Top);
            Addr := Cast (Previous).Used;
            Cast (Previous).Used := Addr + Storage_Size;
         end;
      elsif Cast (Top).Used + Storage_Size < Cast (Top).Last then
         --  current block
         Addr := Cast (Top).Used;
         Cast (Top).Used := Addr + Storage_Size;
      else
         --  new block
         if Cast (Top).Used = Top + Header_Size then
            TLS.Secondary_Stack := Cast (Top).Previous;
            Memory.Unmap (Top, Cast (Top).Last - Top);
         end if;
         declare
            Size : constant Storage_Elements.Storage_Count :=
               Ceiling_Block_Size (Storage_Size);
            New_Block : constant Address := Memory.Map (Size);
         begin
            Cast (New_Block).Previous := TLS.Secondary_Stack;
            TLS.Secondary_Stack := New_Block;
            Cast (New_Block).Last := New_Block + Size;
            Addr := New_Block + Header_Size;
            Cast (New_Block).Used := Addr + Storage_Size;
         end;
      end if;
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
                  --  leave last unused block
                  Cast (Top).Used := Top + Header_Size;
                  exit;
               end if;
               TLS.Secondary_Stack := Cast (Top).Previous;
               Memory.Unmap (Top, Cast (Top).Last - Top);
            end;
         end loop;
      end if;
   end SS_Release;

end System.Secondary_Stack;
