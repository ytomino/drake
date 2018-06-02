with System.Address_To_Named_Access_Conversions;
with System.Storage_Map;
with System.System_Allocators;
package body System.Unbounded_Stack_Allocators is
   pragma Suppress (All_Checks);
   use type Storage_Elements.Integer_Address;
   use type Storage_Elements.Storage_Offset;

   Down : Boolean
      renames Storage_Map.Growing_Down_Is_Preferred;

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

   package BA_Conv is
      new Address_To_Named_Access_Conversions (Block, Block_Access);

   function Cast (X : Address) return Block_Access
      renames BA_Conv.To_Pointer;

   function Align_Header_Size (Mask : Storage_Elements.Integer_Address)
      return Storage_Elements.Storage_Count;
   function Align_Header_Size (Mask : Storage_Elements.Integer_Address)
      return Storage_Elements.Storage_Count
   is
      Header_Size : constant Storage_Elements.Storage_Count :=
         Block'Size / Standard'Storage_Unit;
   begin
      return Storage_Elements.Storage_Count (
         (Storage_Elements.Integer_Address (Header_Size) + Mask) and not Mask);
   end Align_Header_Size;

   --  direction-depended operations

   procedure Commit (
      Used : in out Address;
      Storage_Address : out Address;
      Size_In_Storage_Elements : Storage_Elements.Storage_Count;
      Mask : Storage_Elements.Integer_Address);
   procedure Commit (
      Used : in out Address;
      Storage_Address : out Address;
      Size_In_Storage_Elements : Storage_Elements.Storage_Count;
      Mask : Storage_Elements.Integer_Address) is
   begin
      if Down then
         Storage_Address :=
            Address (
               (Storage_Elements.Integer_Address (
                        Used - Size_In_Storage_Elements)
                     - Mask)
                  and not Mask);
         Used := Storage_Address;
      else
         Storage_Address :=
            Address (
               (Storage_Elements.Integer_Address (Used) + Mask) and not Mask);
         Used := Storage_Address + Size_In_Storage_Elements;
      end if;
   end Commit;

   function Is_In (New_Used, B : Address) return Boolean;
   function Is_In (New_Used, B : Address) return Boolean is
      Header_Size : constant Storage_Elements.Storage_Count :=
         Block'Size / Standard'Storage_Unit;
   begin
      if Down then
         return New_Used >= B + Header_Size;
      else
         return New_Used <= Cast (B).Limit;
      end if;
   end Is_In;

   function Bottom (B : Address) return Address;
   function Bottom (B : Address) return Address is
      Header_Size : constant Storage_Elements.Storage_Count :=
         Block'Size / Standard'Storage_Unit;
   begin
      if Down then
         return Cast (B).Limit;
      else
         return B + Header_Size;
      end if;
   end Bottom;

   function Growing_Address (
      Current_Block : Address;
      Additional_Block_Size : Storage_Elements.Storage_Count)
      return Address;
   function Growing_Address (
      Current_Block : Address;
      Additional_Block_Size : Storage_Elements.Storage_Count)
      return Address is
   begin
      if Down then
         return Current_Block - Additional_Block_Size;
      else
         return Cast (Current_Block).Limit;
      end if;
   end Growing_Address;

   function Is_Growable (
      Current_Block : Address;
      Additional_Block : Address;
      Additional_Block_Size : Storage_Elements.Storage_Count;
      Reverse_Growing : Boolean)
      return Boolean;
   function Is_Growable (
      Current_Block : Address;
      Additional_Block : Address;
      Additional_Block_Size : Storage_Elements.Storage_Count;
      Reverse_Growing : Boolean)
      return Boolean is
   begin
      if Down /= Reverse_Growing then
         return (Additional_Block + Additional_Block_Size) = Current_Block;
      else
         return Cast (Current_Block).Limit = Additional_Block;
      end if;
   end Is_Growable;

   procedure Grow (
      Current_Block : in out Address;
      Additional_Block : Address;
      Additional_Block_Size : Storage_Elements.Storage_Count;
      Reverse_Growing : Boolean;
      Top_Is_Unused : Boolean);
   procedure Grow (
      Current_Block : in out Address;
      Additional_Block : Address;
      Additional_Block_Size : Storage_Elements.Storage_Count;
      Reverse_Growing : Boolean;
      Top_Is_Unused : Boolean) is
   begin
      if Down /= Reverse_Growing then
         Cast (Additional_Block).all := Cast (Current_Block).all;
         if Top_Is_Unused then
            Cast (Additional_Block).Used := Bottom (Additional_Block);
         end if;
         Current_Block := Additional_Block;
      else
         Cast (Current_Block).Limit :=
            Cast (Current_Block).Limit + Additional_Block_Size;
      end if;
   end Grow;

   --  implementation

   procedure Allocate (
      Allocator : aliased in out Allocator_Type;
      Storage_Address : out Address;
      Size_In_Storage_Elements : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count)
   is
      Mask : constant Storage_Elements.Integer_Address :=
         Storage_Elements.Integer_Address (Alignment - 1);
      Top : Address := Allocator;
      Top_Is_Unused : Boolean;
      --  new block:
      New_Block : Address := Null_Address;
      New_Block_Size : Storage_Elements.Storage_Count;
      Aligned_Header_Size : Storage_Elements.Storage_Count;
   begin
      if Top /= Null_Address then
         --  when top block is empty and previous block has enough space
         Top_Is_Unused := Cast (Top).Used = Bottom (Top);
         if Top_Is_Unused and then Cast (Top).Previous /= Null_Address then
            declare
               Previous : constant Address := Cast (Top).Previous;
               New_Previous_Used : Address := Cast (Previous).Used;
               New_Storage_Address : Address;
            begin
               Commit (
                  Used => New_Previous_Used,
                  Storage_Address => New_Storage_Address,
                  Size_In_Storage_Elements => Size_In_Storage_Elements,
                  Mask => Mask);
               if Is_In (New_Previous_Used, Previous) then
                  Allocator := Previous;
                  System_Allocators.Unmap (Top, Cast (Top).Limit - Top);
                  Storage_Address := New_Storage_Address;
                  Cast (Previous).Used := New_Previous_Used;
                  return;
               end if;
            end;
         end if;
         --  when top block has enough space
         declare
            New_Top_Used : Address := Cast (Top).Used;
            New_Storage_Address : Address;
         begin
            Commit (
               Used => New_Top_Used,
               Storage_Address => New_Storage_Address,
               Size_In_Storage_Elements => Size_In_Storage_Elements,
               Mask => Mask);
            if Is_In (New_Top_Used, Top) then
               Storage_Address := New_Storage_Address;
               Cast (Top).Used := New_Top_Used;
               return;
            end if;
         end;
         --  try expanding top block
         if Expanding /= 0 then
            Aligned_Header_Size := Align_Header_Size (Mask);
            declare
               Additional_Block_Size : constant
                     Storage_Elements.Storage_Count :=
                  Ceiling_Page_Size (
                     Size_In_Storage_Elements + Aligned_Header_Size);
               Additional_Block : constant Address :=
                  System_Allocators.Map (
                     Growing_Address (Top, Additional_Block_Size),
                     Additional_Block_Size);
            begin
               if Is_Growable (
                  Current_Block => Top,
                  Additional_Block => Additional_Block,
                  Additional_Block_Size => Additional_Block_Size,
                  Reverse_Growing => False)
               then
                  Grow (
                     Current_Block => Top,
                     Additional_Block => Additional_Block,
                     Additional_Block_Size => Additional_Block_Size,
                     Reverse_Growing => False,
                     Top_Is_Unused => Top_Is_Unused);
                  Allocator := Top;
                  Commit (
                     Used => Cast (Top).Used,
                     Storage_Address => Storage_Address,
                     Size_In_Storage_Elements => Size_In_Storage_Elements,
                     Mask => Mask);
                  return;
               elsif Is_Growable (
                  Current_Block => Top,
                  Additional_Block => Additional_Block,
                  Additional_Block_Size => Additional_Block_Size,
                  Reverse_Growing => True) -- reverse
                  and then Top_Is_Unused
               then
                  --  The new block is allocated brefore the top block,
                  --    concatenate them.
                  Grow (
                     Current_Block => Top,
                     Additional_Block => Additional_Block,
                     Additional_Block_Size => Additional_Block_Size,
                     Reverse_Growing => True,
                     Top_Is_Unused => True); -- already checked in above
                  Allocator := Top;
                  Commit (
                     Used => Cast (Top).Used,
                     Storage_Address => Storage_Address,
                     Size_In_Storage_Elements => Size_In_Storage_Elements,
                     Mask => Mask);
                  return;
               end if;
               New_Block := Additional_Block;
               New_Block_Size := Additional_Block_Size;
            end;
         end if;
         --  top block is not enough, then free it if unused
         if Top_Is_Unused then
            declare
               New_Top : constant Address := Cast (Top).Previous;
            begin
               System_Allocators.Unmap (Top, Cast (Top).Limit - Top);
               Allocator := New_Top;
               Top := New_Top;
            end;
         end if;
      end if;
      --  new block
      declare
         Default_Block_Size : constant := 10 * 1024;
      begin
         if New_Block = Null_Address then
            Aligned_Header_Size := Align_Header_Size (Mask);
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
         Cast (New_Block).Used := Bottom (New_Block);
         Commit (
            Used => Cast (New_Block).Used,
            Storage_Address => Storage_Address,
            Size_In_Storage_Elements => Size_In_Storage_Elements,
            Mask => Mask);
      end;
   end Allocate;

   function Mark (Allocator : aliased in out Allocator_Type)
      return Marker
   is
      Top : constant Address := Allocator;
   begin
      if Top = Null_Address then
         return Marker (Null_Address);
      elsif Cast (Top).Used = Bottom (Top) then
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
      Mark : Marker) is
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
                  Cast (Top).Used := Bottom (Top);
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

   function Size (B : Address) return Storage_Elements.Storage_Count is
      Header_Size : constant Storage_Elements.Storage_Count :=
         Block'Size / Standard'Storage_Unit;
   begin
      return Cast (B).Limit - (B + Header_Size);
   end Size;

   function Used_Size (B : Address) return Storage_Elements.Storage_Count is
   begin
      if Down then
         return Bottom (B) - Cast (B).Used;
      else
         return Cast (B).Used - Bottom (B);
      end if;
   end Used_Size;

end System.Unbounded_Stack_Allocators;
