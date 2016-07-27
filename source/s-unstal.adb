with System.Address_To_Named_Access_Conversions;
with System.System_Allocators;
package body System.Unbounded_Stack_Allocators is
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
         Storage_Elements.Integer_Address (System_Allocators.Page_Size);
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
      Allocator : aliased in out Allocator_Type;
      Storage_Address : out Address;
      Size_In_Storage_Elements : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count)
   is
      Header_Size : constant Storage_Elements.Storage_Count :=
         Block'Size / Standard'Storage_Unit;
      Mask : constant Storage_Elements.Integer_Address :=
         Storage_Elements.Integer_Address (Alignment - 1);
      Top : Address := Allocator;
      --  new block:
      New_Block : Address := Null_Address;
      New_Block_Size : Storage_Elements.Storage_Count;
      Aligned_Header_Size : Storage_Elements.Storage_Count;
      --  top block:
      Aligned_Top_Used : Address;
      New_Top_Used : Address;
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
               New_Previous_Used : constant Address :=
                  Aligned_Previous_Used + Size_In_Storage_Elements;
            begin
               if New_Previous_Used <= Cast (Previous).Limit then
                  Allocator := Previous;
                  System_Allocators.Unmap (Top, Cast (Top).Limit - Top);
                  Storage_Address := Aligned_Previous_Used;
                  Cast (Previous).Used := New_Previous_Used;
                  return;
               end if;
            end;
         end if;
         --  when top block has enough space
         Aligned_Top_Used :=
            Address (
               (Storage_Elements.Integer_Address (Cast (Top).Used) + Mask)
               and not Mask);
         New_Top_Used := Aligned_Top_Used + Size_In_Storage_Elements;
         if New_Top_Used <= Cast (Top).Limit then
            Storage_Address := Aligned_Top_Used;
            Cast (Top).Used := New_Top_Used;
            return;
         end if;
         --  try expanding top block
         if Expanding /= 0 then
            Aligned_Header_Size :=
               Storage_Elements.Storage_Offset (
                  (Storage_Elements.Integer_Address (Header_Size) + Mask)
                  and not Mask);
            declare
               Additional_Block_Size : constant
                  Storage_Elements.Storage_Count :=
                     Ceiling_Page_Size (
                        Size_In_Storage_Elements + Aligned_Header_Size);
               Additional_Block : constant Address :=
                  System_Allocators.Map (
                     Cast (Top).Limit,
                     Additional_Block_Size);
            begin
               if Additional_Block = Cast (Top).Limit then
                  Cast (Top).Limit := Cast (Top).Limit + Additional_Block_Size;
                  Storage_Address := Aligned_Top_Used;
                  Cast (Top).Used := New_Top_Used;
                  return;
               elsif Additional_Block + Additional_Block_Size = Top then
                  --  The new block is allocated brefore the top block,
                  --    concatenate them.
                  --  Especially, this often happen in Linux.
                  declare
                     Top_Is_Unused : constant Boolean :=
                        Cast (Top).Used = Top + Header_Size;
                  begin
                     Cast (Additional_Block).all := Cast (Top).all;
                     Allocator := Additional_Block;
                     Top := Additional_Block;
                     if Top_Is_Unused then
                        Storage_Address :=
                           Address (
                              (Storage_Elements.Integer_Address (
                                    Top + Header_Size)
                                 + Mask)
                              and not Mask);
                        Cast (Top).Used :=
                           Storage_Address + Size_In_Storage_Elements;
                        return;
                     end if;
                  end;
                  goto Allocating_New_Block;
               end if;
               New_Block := Additional_Block;
               New_Block_Size := Additional_Block_Size;
            end;
         end if;
         --  top block is not enough, then free it if unused
         if Cast (Top).Used = Top + Header_Size then
            declare
               New_Top : constant Address := Cast (Top).Previous;
            begin
               System_Allocators.Unmap (Top, Cast (Top).Limit - Top);
               Allocator := New_Top;
               Top := New_Top;
            end;
         end if;
      end if;
   <<Allocating_New_Block>>
      --  new block
      declare
         Default_Block_Size : constant := 10 * 1024;
      begin
         if New_Block = Null_Address then
            Aligned_Header_Size :=
               Storage_Elements.Storage_Offset (
                  (Storage_Elements.Integer_Address (Header_Size) + Mask)
                  and not Mask);
            New_Block_Size := Size_In_Storage_Elements + Aligned_Header_Size;
            if Top = Null_Address then
               New_Block_Size :=
                  Storage_Elements.Storage_Offset'Max (
                     Default_Block_Size,
                     New_Block_Size);
            end if;
            New_Block_Size := Ceiling_Page_Size (New_Block_Size);
            New_Block := System_Allocators.Map (Null_Address, New_Block_Size);
            if New_Block = Null_Address then
               raise Storage_Error;
            end if;
         end if;
         Cast (New_Block).Previous := Top;
         Allocator := New_Block;
         Cast (New_Block).Limit := New_Block + New_Block_Size;
         Storage_Address := New_Block + Aligned_Header_Size;
         Cast (New_Block).Used := Storage_Address + Size_In_Storage_Elements;
      end;
   end Allocate;

   function Mark (Allocator : aliased in out Allocator_Type)
      return Marker
   is
      Header_Size : constant Storage_Elements.Storage_Count :=
         Block'Size / Standard'Storage_Unit;
      Top : constant Address := Allocator;
   begin
      if Top = Null_Address then
         return Marker (Null_Address);
      elsif Cast (Top).Used = Top + Header_Size then
         declare
            Previous : constant Address := Cast (Top).Previous;
         begin
            if Previous = Null_Address then
               return Marker (Null_Address);
            else
               return Marker (Cast (Previous).Used);
            end if;
         end;
      else
         return Marker (Cast (Top).Used);
      end if;
   end Mark;

   procedure Release (
      Allocator : aliased in out Allocator_Type;
      Mark : Marker)
   is
      Header_Size : constant Storage_Elements.Storage_Count :=
         Block'Size / Standard'Storage_Unit;
   begin
      if Allocator /= Null_Address then
         loop
            declare
               Top : constant Address := Allocator;
            begin
               if Address (Mark) in Top .. Cast (Top).Limit then
                  Cast (Top).Used := Address (Mark);
                  exit;
               elsif Cast (Top).Previous = Null_Address
                  or else Address (Mark) = Cast (Cast (Top).Previous).Used
               then
                  --  leave one unused block
                  Cast (Top).Used := Top + Header_Size;
                  exit;
               end if;
               Allocator := Cast (Top).Previous;
               System_Allocators.Unmap (Top, Cast (Top).Limit - Top);
            end;
         end loop;
      end if;
   end Release;

   procedure Clear (Allocator : aliased in out Allocator_Type) is
   begin
      while Allocator /= Null_Address loop
         declare
            Top : constant Address := Allocator;
         begin
            Allocator := Cast (Top).Previous;
            System_Allocators.Unmap (Top, Cast (Top).Limit - Top);
         end;
      end loop;
   end Clear;

end System.Unbounded_Stack_Allocators;
