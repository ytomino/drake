pragma Check_Policy (Validate, Off);
pragma Check_Policy (Trace, Disable);
--  with Ada;
with System.Shared_Locking;
--  with System.Storage_Elements.Formatting;
package body System.Pool_Size is
   pragma Suppress (All_Checks);

   type Unaligned_Storage_Offset is new Storage_Elements.Storage_Offset;
   for Unaligned_Storage_Offset'Alignment use 1;

   type Freed_Chunk is record
      Size : Storage_Elements.Storage_Count;
      Next : Storage_Elements.Storage_Offset; -- -1 means last
   end record;
   pragma Suppress_Initialization (Freed_Chunk);

   --  implementation of mixed

   procedure Allocate (
      Allocator : aliased in out Bounded_Allocator;
      Storage_Address : out Address;
      Size_In_Storage_Elements : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count)
   is
      Actual_Alignment : constant Storage_Elements.Storage_Count :=
         Storage_Elements.Storage_Offset'Max (
            Allocator.Alignment,
            Freed_Chunk'Size / Standard'Storage_Unit);
      Error : Boolean := False;
   begin
      if Allocator.Alignment rem Alignment /= 0 then
         raise Constraint_Error;
      end if;
      Shared_Locking.Enter;
      declare
         Actual_Size : constant Storage_Elements.Storage_Count :=
            Size_In_Storage_Elements
            + (-Size_In_Storage_Elements) mod Actual_Alignment;
      begin
         Storage_Address := Null_Address;
         if Allocator.First_Free >= 0 then
            --  search freed list by best matching
            declare
               Matched_Size : Storage_Elements.Storage_Count :=
                  Storage_Elements.Storage_Count'Last;
               Matched_Index : Storage_Elements.Storage_Offset := -1;
               Previous_Matched_Index : Storage_Elements.Storage_Offset := -1;
            begin
               declare
                  Index : Storage_Elements.Storage_Count :=
                     Allocator.First_Free;
                  Previous_Index : Storage_Elements.Storage_Offset := -1;
               begin
                  loop
                     declare
                        Chunk : Freed_Chunk;
                        for Chunk'Address use
                           Allocator.Storage'Address + Index;
                     begin
                        if Chunk.Size >= Actual_Size
                           and then Chunk.Size < Matched_Size
                        then
                           Previous_Matched_Index := Previous_Index;
                           Matched_Size := Chunk.Size;
                           Matched_Index := Index;
                        end if;
                        Previous_Index := Index;
                        exit when Chunk.Next < 0;
                        pragma Check (Validate, Chunk.Next > Index);
                        Index := Chunk.Next;
                     end;
                  end loop;
               end;
               if Matched_Index >= 0 then
                  --  found
                  Storage_Address := Allocator.Storage'Address + Matched_Index;
                  declare
                     Chunk : Freed_Chunk;
                     for Chunk'Address use Storage_Address;
                     Rest_Size : constant Storage_Elements.Storage_Count :=
                        Chunk.Size - Actual_Size;
                  begin
                     if Rest_Size >= Actual_Alignment then
                        --  split
                        declare
                           New_Index : constant
                              Storage_Elements.Storage_Count :=
                                 Matched_Index + Actual_Size;
                           New_Chunk : Freed_Chunk;
                           for New_Chunk'Address use
                              Allocator.Storage'Address + New_Index;
                        begin
                           New_Chunk.Size := Rest_Size;
                           New_Chunk.Next := Chunk.Next;
                           Chunk.Size := Actual_Size;
                           Chunk.Next := New_Index;
                        end;
                        pragma Check (Trace, Ada.Debug.Put ("split"));
                     end if;
                     if Previous_Matched_Index >= 0 then
                        declare
                           Previous_Chunk : Freed_Chunk;
                           for Previous_Chunk'Address use
                              Allocator.Storage'Address
                              + Previous_Matched_Index;
                        begin
                           Previous_Chunk.Next := Chunk.Next;
                        end;
                     else
                        Allocator.First_Free := Chunk.Next;
                     end if;
                  end;
                  pragma Check (Trace, Ada.Debug.Put ("from freed list"));
               end if;
            end;
         end if;
         if Storage_Address = Null_Address then
            --  allocate from empty area
            declare
               Next_Empty : Storage_Elements.Storage_Count;
            begin
               Next_Empty := Allocator.First_Empty + Actual_Size;
               if Next_Empty <= Allocator.Size then
                  Storage_Address :=
                     Allocator.Storage'Address + Allocator.First_Empty;
                  Allocator.First_Empty := Next_Empty;
                  pragma Check (Trace, Ada.Debug.Put ("from empty area"));
               else
                  Error := True;
               end if;
            end;
         end if;
      end;
      Shared_Locking.Leave;
      if Error then
         raise Storage_Error;
      end if;
      pragma Check (Trace, Ada.Debug.Put (
         System.Storage_Elements.Formatting.Image (Storage_Address)));
   end Allocate;

   procedure Deallocate (
      Allocator : aliased in out Bounded_Allocator;
      Storage_Address : Address;
      Size_In_Storage_Elements : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count)
   is
      Actual_Alignment : constant Storage_Elements.Storage_Count :=
         Storage_Elements.Storage_Offset'Max (
            Allocator.Alignment,
            Freed_Chunk'Size / Standard'Storage_Unit);
   begin
      pragma Check (Trace, Ada.Debug.Put (
         System.Storage_Elements.Formatting.Image (Storage_Address)));
      if Allocator.Alignment rem Alignment /= 0 then
         raise Constraint_Error;
      end if;
      Shared_Locking.Enter;
      declare
         Actual_Index : Storage_Elements.Storage_Count :=
            Storage_Address - Allocator.Storage'Address;
         Actual_Size : Storage_Elements.Storage_Count :=
            Size_In_Storage_Elements
            + (-Size_In_Storage_Elements) mod Actual_Alignment;
         Previous_Index : Storage_Elements.Storage_Offset := -1;
      begin
         if Allocator.First_Free >= 0 then
            declare
               Previous_Previous_Index : Storage_Elements.Storage_Offset := -1;
            begin
               --  search freed list for previous chunk
               declare
                  Index : Storage_Elements.Storage_Count :=
                     Allocator.First_Free;
               begin
                  while Index < Actual_Index loop
                     Previous_Previous_Index := Previous_Index;
                     Previous_Index := Index;
                     declare
                        Chunk : Freed_Chunk;
                        for Chunk'Address use
                           Allocator.Storage'Address + Index;
                     begin
                        exit when Chunk.Next < 0;
                        pragma Check (Validate, Chunk.Next > Index);
                        Index := Chunk.Next;
                     end;
                  end loop;
               end;
               --  try to merge
               if Previous_Index >= 0 then
                  declare
                     Previous_Chunk : Freed_Chunk;
                     for Previous_Chunk'Address use
                        Allocator.Storage'Address + Previous_Index;
                  begin
                     if Actual_Index + Actual_Size = Previous_Chunk.Next then
                        --  merge next chunk
                        declare
                           Next_Chunk : Freed_Chunk;
                           for Next_Chunk'Address use
                              Allocator.Storage'Address + Previous_Chunk.Next;
                        begin
                           Actual_Size := Actual_Size + Next_Chunk.Size;
                           Previous_Chunk.Next := Next_Chunk.Next;
                        end;
                        pragma Check (Trace, Ada.Debug.Put ("merge next"));
                     end if;
                     if Previous_Index + Previous_Chunk.Size =
                        Actual_Index
                     then
                        --  merge previous chunk
                        if Previous_Previous_Index >= 0 then
                           declare
                              Previous_Previous_Chunk : Freed_Chunk;
                              for Previous_Previous_Chunk'Address use
                                 Allocator.Storage'Address
                                 + Previous_Previous_Index;
                           begin
                              Previous_Previous_Chunk.Next :=
                                 Previous_Chunk.Next;
                           end;
                        else
                           Allocator.First_Free := Previous_Chunk.Next;
                        end if;
                        Actual_Index := Previous_Index;
                        Actual_Size := Actual_Size + Previous_Chunk.Size;
                        Previous_Index := Previous_Previous_Index;
                        pragma Check (Trace, Ada.Debug.Put ("merge previous"));
                     end if;
                  end;
               else
                  if Actual_Index + Actual_Size = Allocator.First_Free then
                     --  merge next chunk
                     declare
                        Next_Chunk : Freed_Chunk;
                        for Next_Chunk'Address use
                           Allocator.Storage'Address + Allocator.First_Free;
                     begin
                        Actual_Size := Actual_Size + Next_Chunk.Size;
                        Allocator.First_Free := Next_Chunk.Next;
                     end;
                     pragma Check (Trace, Ada.Debug.Put ("merge next"));
                  end if;
               end if;
            end;
         end if;
         if Actual_Index + Actual_Size < Allocator.First_Empty then
            --  add new freed chunk
            declare
               New_Chunk : Freed_Chunk;
               for New_Chunk'Address use
                  Allocator.Storage'Address + Actual_Index;
            begin
               New_Chunk.Size := Actual_Size;
               if Previous_Index >= 0 then
                  pragma Check (Validate, Previous_Index < Actual_Index);
                  declare
                     Previous_Chunk : Freed_Chunk;
                     for Previous_Chunk'Address use
                        Allocator.Storage'Address + Previous_Index;
                  begin
                     New_Chunk.Next := Previous_Chunk.Next;
                     Previous_Chunk.Next := Actual_Index;
                  end;
               else
                  New_Chunk.Next := Allocator.First_Free;
                  Allocator.First_Free := Actual_Index;
               end if;
            end;
            pragma Check (Trace, Ada.Debug.Put ("to freed list"));
         else
            --  compaction
            if Previous_Index >= 0 then
               declare
                  Previous_Chunk : Freed_Chunk;
                  for Previous_Chunk'Address use
                     Allocator.Storage'Address + Previous_Index;
               begin
                  Previous_Chunk.Next := -1;
               end;
            else
               Allocator.First_Free := -1;
            end if;
            Allocator.First_Empty := Actual_Index;
            pragma Check (Trace, Ada.Debug.Put ("to empty area"));
         end if;
      end;
      Shared_Locking.Leave;
   end Deallocate;

   function Storage_Size (Allocator : Bounded_Allocator)
      return Storage_Elements.Storage_Count is
   begin
      return Allocator.Size;
   end Storage_Size;

   --  implementation of fixed

   procedure Allocate (
      Allocator : aliased in out Bounded_Fixed_Allocator;
      Storage_Address : out Address;
      Size_In_Storage_Elements : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count)
   is
      Aligned_Component_Size : constant Storage_Elements.Storage_Count :=
         Storage_Elements.Storage_Offset'Max (
            --  minimum chunk size
            (Storage_Elements.Storage_Count'Size + Standard'Storage_Unit - 1) /
               Standard'Storage_Unit,
            --  component size
            Allocator.Component_Size
               + (-Allocator.Component_Size) mod Allocator.Alignment);
      Error : Boolean := False;
   begin
      if Allocator.Alignment rem Alignment /= 0
         or else Size_In_Storage_Elements /= Allocator.Component_Size
      then
         raise Constraint_Error;
      end if;
      Shared_Locking.Enter;
      if Allocator.First_Free >= 0 then
         Storage_Address := Allocator.Storage'Address + Allocator.First_Free;
         declare
            Next : Unaligned_Storage_Offset;
            for Next'Address use Storage_Address;
         begin
            Allocator.First_Free := Storage_Elements.Storage_Offset (Next);
         end;
      elsif Allocator.First_Empty <=
         Allocator.Size - Aligned_Component_Size + 1
      then
         Storage_Address := Allocator.Storage'Address + Allocator.First_Empty;
         Allocator.First_Empty :=
            Allocator.First_Empty + Aligned_Component_Size;
      else
         Error := True;
      end if;
      Shared_Locking.Leave;
      if Error then
         raise Storage_Error;
      end if;
   end Allocate;

   procedure Deallocate (
      Allocator : aliased in out Bounded_Fixed_Allocator;
      Storage_Address : Address;
      Size_In_Storage_Elements : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count) is
   begin
      if Size_In_Storage_Elements /= Allocator.Component_Size
         or else Alignment /= Allocator.Alignment
      then
         raise Constraint_Error;
      end if;
      Shared_Locking.Enter;
      declare
         Next : Unaligned_Storage_Offset;
         for Next'Address use Storage_Address;
      begin
         Next := Unaligned_Storage_Offset (Allocator.First_Free);
      end;
      Allocator.First_Free := Storage_Address - Allocator.Storage'Address;
      Shared_Locking.Leave;
   end Deallocate;

   function Storage_Size (Allocator : Bounded_Fixed_Allocator)
      return Storage_Elements.Storage_Count is
   begin
      return Allocator.Size;
   end Storage_Size;

   --  implementation of required by compiler

   overriding procedure Allocate (
      Pool : in out Stack_Bounded_Pool;
      Storage_Address : out Address;
      Size_In_Storage_Elements : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count) is
   begin
      case Pool.Elmt_Size is
         when 0 =>
            Allocate (
               Pool.Mixed,
               Storage_Address,
               Size_In_Storage_Elements,
               Alignment);
         when others =>
            Allocate (
               Pool.Fixed,
               Storage_Address,
               Size_In_Storage_Elements,
               Alignment);
      end case;
   end Allocate;

   overriding procedure Deallocate (
      Pool : in out Stack_Bounded_Pool;
      Storage_Address : Address;
      Size_In_Storage_Elements : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count) is
   begin
      case Pool.Elmt_Size is
         when 0 =>
            Deallocate (
               Pool.Mixed,
               Storage_Address,
               Size_In_Storage_Elements,
               Alignment);
         when others =>
            Deallocate (
               Pool.Fixed,
               Storage_Address,
               Size_In_Storage_Elements,
               Alignment);
      end case;
   end Deallocate;

   overriding function Storage_Size (Pool : Stack_Bounded_Pool)
      return Storage_Elements.Storage_Count is
   begin
      case Pool.Elmt_Size is
         when 0 =>
            return Storage_Size (Pool.Mixed);
         when others =>
            return Storage_Size (Pool.Fixed);
      end case;
   end Storage_Size;

end System.Pool_Size;
