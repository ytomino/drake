with System.Address_To_Named_Access_Conversions;
with System.Standard_Allocators;
package body System.Unbounded_Stack_Allocators is
   pragma Suppress (All_Checks);
   use type System.Storage_Elements.Integer_Address;
   use type System.Storage_Elements.Storage_Offset;

   Expanding : constant := 1; -- connecting next page
   pragma Warnings (Off, Expanding);

   function Ceiling_Page_Size (Required : Storage_Elements.Storage_Count)
      return Storage_Elements.Storage_Count;
   function Ceiling_Page_Size (Required : Storage_Elements.Storage_Count)
      return Storage_Elements.Storage_Count
   is
      Alignment : constant Storage_Elements.Integer_Address :=
         Storage_Elements.Integer_Address (Standard_Allocators.Page_Size);
   begin
      return Storage_Elements.Storage_Offset (
         Storage_Elements.Integer_Address'Mod (Required)
         + Storage_Elements.Integer_Address'Mod (-Required) mod Alignment);
   end Ceiling_Page_Size;

   package Conv is
      new Address_To_Named_Access_Conversions (Block, Block_Access);

   function Cast (X : Address) return Block_Access
      renames Conv.To_Pointer;

   --  implementation

   procedure Allocate (
      Allocator : not null access Allocator_Type;
      Storage_Address : out Address;
      Size_In_Storage_Elements : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count)
   is
      Header_Size : constant Storage_Elements.Storage_Count :=
         Block'Size / Standard'Storage_Unit;
      Top : constant Address := Allocator.all;
      Mask : constant Storage_Elements.Integer_Address :=
         Storage_Elements.Integer_Address (Alignment - 1);
      Aligned_Top_Used : Address;
   begin
      if Top /= Null_Address then
         --  when top block is empty and previous block has enough space
         if Cast (Top).Used = Top + Header_Size
            and then Cast (Top).Previous /= Null_Address
         then
            declare
               Previous : constant Address := Cast (Top).Previous;
               Aligned_Previous_Used : constant Address :=
                  Address (
                     (Storage_Elements.Integer_Address (Cast (Previous).Used)
                        + Mask)
                     and not Mask);
            begin
               if Aligned_Previous_Used + Size_In_Storage_Elements <=
                  Cast (Previous).Limit
               then
                  Allocator.all := Previous;
                  Standard_Allocators.Unmap (Top, Cast (Top).Limit - Top);
                  Storage_Address := Aligned_Previous_Used;
                  Cast (Previous).Used :=
                     Storage_Address + Size_In_Storage_Elements;
                  return;
               end if;
            end;
         end if;
         --  when top block has enough space
         Aligned_Top_Used := Address (
            (Storage_Elements.Integer_Address (Cast (Top).Used) + Mask)
            and not Mask);
         if Aligned_Top_Used + Size_In_Storage_Elements <=
            Cast (Top).Limit
         then
            Storage_Address := Aligned_Top_Used;
            Cast (Top).Used := Storage_Address + Size_In_Storage_Elements;
            return;
         end if;
         --  try expanding top block
         if Expanding /= 0 then
            declare
               Additional_Block_Size : constant
                  Storage_Elements.Storage_Count :=
                     Ceiling_Page_Size (
                        Size_In_Storage_Elements
                        - (Cast (Top).Limit - Aligned_Top_Used));
               Additional_Block : constant Address := Standard_Allocators.Map (
                  Cast (Top).Limit,
                  Additional_Block_Size,
                  Raise_On_Error => False);
            begin
               if Additional_Block = Cast (Top).Limit then
                  Cast (Top).Limit := Cast (Top).Limit + Additional_Block_Size;
                  Storage_Address := Aligned_Top_Used;
                  Cast (Top).Used :=
                     Storage_Address + Size_In_Storage_Elements;
                  return;
               end if;
            end;
         end if;
         --  top block is not enough, then free it if unused
         if Cast (Top).Used = Top + Header_Size then
            Allocator.all := Cast (Top).Previous;
            Standard_Allocators.Unmap (Top, Cast (Top).Limit - Top);
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
                  Size_In_Storage_Elements + Aligned_Header_Size));
         New_Block : constant Address := Standard_Allocators.Map (Block_Size);
      begin
         Cast (New_Block).Previous := Allocator.all;
         Allocator.all := New_Block;
         Cast (New_Block).Limit := New_Block + Block_Size;
         Storage_Address := New_Block + Aligned_Header_Size;
         Cast (New_Block).Used := Storage_Address + Size_In_Storage_Elements;
      end;
   end Allocate;

   function Mark (Allocator : not null access Allocator_Type)
      return Marker
   is
      Header_Size : constant Storage_Elements.Storage_Count :=
         Block'Size / Standard'Storage_Unit;
      Top : constant Address := Allocator.all;
   begin
      if Top = Null_Address then
         return (Top => Null_Address, Used => Null_Address);
      elsif Cast (Top).Used = Top + Header_Size then
         declare
            Previous : constant Address := Cast (Top).Previous;
         begin
            if Previous = Null_Address then
               return (Top => Null_Address, Used => Null_Address);
            else
               return (Top => Previous, Used => Cast (Previous).Used);
            end if;
         end;
      else
         return (Top => Top, Used => Cast (Top).Used);
      end if;
   end Mark;

   procedure Release (
      Allocator : not null access Allocator_Type;
      Mark : Marker)
   is
      Header_Size : constant Storage_Elements.Storage_Count :=
         Block'Size / Standard'Storage_Unit;
   begin
      if Allocator.all /= Null_Address then
         loop
            declare
               Top : constant Address := Allocator.all;
            begin
               if Top = Mark.Top then
                  Cast (Top).Used := Mark.Used;
                  exit;
               elsif Cast (Top).Previous = Mark.Top
                  and then (Mark.Top = Null_Address
                     or else Mark.Used = Cast (Mark.Top).Used)
               then
                  --  leave Limit unused block
                  Cast (Top).Used := Top + Header_Size;
                  exit;
               end if;
               Allocator.all := Cast (Top).Previous;
               Standard_Allocators.Unmap (Top, Cast (Top).Limit - Top);
            end;
         end loop;
      end if;
   end Release;

   procedure Clear (Allocator : not null access Allocator_Type) is
   begin
      while Allocator.all /= Null_Address loop
         declare
            Top : constant Address := Allocator.all;
         begin
            Allocator.all := Cast (Top).Previous;
            Standard_Allocators.Unmap (Top, Cast (Top).Limit - Top);
         end;
      end loop;
   end Clear;

end System.Unbounded_Stack_Allocators;
