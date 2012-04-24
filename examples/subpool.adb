pragma Ada_2012;
-- for MR_Pool
with System.Storage_Pools.Subpools;
with System.Storage_Elements;
with Ada.Unchecked_Deallocate_Subpool;
-- for dummy item type
with Ada.Finalization;
-- with Ada.Unchecked_Deallocation;
with System.Address_Image;
procedure subpool is
	--  RM 13-11-6
	package MR_Pool is
		use System.Storage_Pools;
			-- For uses of Subpools.
		use System.Storage_Elements;
			-- For uses of Storage_Count and Storage_Array.]
		
		-- Mark and Release work in a stack fashion, and allocations are not allowed
		-- from a subpool other than the one at the top of the stack. This is also
		-- the default pool.
		
		subtype Subpool_Handle is Subpools.Subpool_Handle;
		
		type Mark_Release_Pool_Type (Pool_Size : Storage_Count) is new
			Subpools.Root_Storage_Pool_With_Subpools with private;
		
		function Mark (Pool : in out Mark_Release_Pool_Type)
			return not null Subpool_Handle;
		
		procedure Release (Subpool : in out Subpool_Handle) renames
			Ada.Unchecked_Deallocate_Subpool;
		
	private
		
		type MR_Subpool is new Subpools.Root_Subpool with record
			Start : Storage_Count;
		end record;
		subtype Subpool_Indexes is Positive range 1 .. 10;
		type Subpool_Array is array (Subpool_Indexes) of aliased MR_Subpool;
		
		type Mark_Release_Pool_Type (Pool_Size : Storage_Count) is new
			Subpools.Root_Storage_Pool_With_Subpools with record
			Storage         : Storage_Array (1 .. Pool_Size);
			Next_Allocation : Storage_Count := 1;
			Markers         : Subpool_Array;
			Current_Pool    : Subpool_Indexes := 1;
		end record;
		
		overriding
		function Create_Subpool (Pool : aliased in out Mark_Release_Pool_Type)
			return not null Subpool_Handle;
		
		function Mark (Pool : in out Mark_Release_Pool_Type)
			return not null Subpool_Handle renames Create_Subpool;
		
		overriding
		procedure Allocate_From_Subpool (
			Pool : in out Mark_Release_Pool_Type;
			Storage_Address : out System.Address;
			Size_In_Storage_Elements : in Storage_Count;
			Alignment : in Storage_Count;
			Subpool : not null Subpool_Handle);
		
		overriding
		procedure Deallocate_Subpool (
			Pool : in out Mark_Release_Pool_Type;
			Subpool : in out Subpool_Handle);
		
		overriding
		function Default_Subpool_for_Pool (
			Pool : in Mark_Release_Pool_Type) return not null Subpool_Handle;
		
		overriding
		procedure Initialize (Pool : in out Mark_Release_Pool_Type);
		
		-- We don't need Finalize.
		
	end MR_Pool;
	
	package body MR_Pool is
		use type System.Storage_Pools.Subpools.Subpool_Handle; -- bug of RM???
		
		procedure Initialize (Pool : in out Mark_Release_Pool_Type) is
			-- Initialize the first default subpool.
		begin
			Subpools.Initialize (Subpools.Root_Storage_Pool_With_Subpools (Pool)); -- drake
			Pool.Markers(1).Start := 1;
			Subpools.Set_Pool_of_Subpool
				(Pool.Markers(1)'Unchecked_Access,
				 Pool); -- Pool'Unchecked_Access); / bug of RM???
		end Initialize;
		
		function Create_Subpool (Pool : in out Mark_Release_Pool_Type)
			return not null Subpool_Handle is
			-- Mark the current allocation location.
		begin
			if Pool.Current_Pool = Subpool_Indexes'Last then
				raise Storage_Error; -- No more subpools.
			end if;
			Pool.Current_Pool := Pool.Current_Pool + 1; -- Move to the next subpool
			
			return Result : constant not null Subpool_Handle :=
				Pool.Markers(Pool.Current_Pool)'Unchecked_Access
			do
				-- Result.Start := / bug of RM???
				MR_Subpool (Result.all).Start := Pool.Next_Allocation;
				Subpools.Set_Pool_of_Subpool (
					Result,
					Pool); -- Pool'Unchecked_Access); -- bug of RM???
			end return;
		end Create_Subpool;
		
		procedure Deallocate_Subpool (
			Pool : in out Mark_Release_Pool_Type;
			Subpool : in out Subpool_Handle) is
		begin
			if Subpool /= Pool.Markers(Pool.Current_Pool)'Unchecked_Access then
				raise Program_Error; -- Only the last marked subpool can be released.
			end if;
			if Pool.Current_Pool /= 1 then
				Pool.Next_Allocation := Pool.Markers(Pool.Current_Pool)
					.Start; -- ; / bug of RM???
				Pool.Current_Pool := Pool.Current_Pool - 1; -- Move to the previous subpool
			else -- Reinitialize the default subpool:
				Pool.Next_Allocation := 1;
				Subpools.Set_Pool_of_Subpool
					(Pool.Markers(1)'Unchecked_Access,
					 Pool); -- Pool'Unchecked_Access); / bug of RM???
			end if;
		end Deallocate_Subpool;
		
		function Default_Subpool_for_Pool (
			Pool : in Mark_Release_Pool_Type) return not null Subpool_Handle is
		begin
			return Pool.Markers(Pool.Current_Pool)
				'Unrestricted_Access; -- 'Unchecked_Access; / different gcc from Standard
		end Default_Subpool_for_Pool;
		
		procedure Allocate_From_Subpool (
			Pool : in out Mark_Release_Pool_Type;
			Storage_Address : out System.Address;
			Size_In_Storage_Elements : in Storage_Count;
			Alignment : in Storage_Count;
			Subpool : not null Subpool_Handle) is
		begin
			if Subpool /= Pool.Markers(Pool.Current_Pool)'Unchecked_Access then
				raise Program_Error; -- Only the last marked subpool can be used for allocations.
			end if;
			
			-- Correct the alignment if necessary:
			Pool.Next_Allocation := Pool.Next_Allocation +
				((-Pool.Next_Allocation) mod Alignment);
			if Pool.Next_Allocation + Size_In_Storage_Elements >
				Pool.Pool_Size then
				raise Storage_Error; -- Out of space.
			end if;
			Storage_Address := Pool.Storage (Pool.Next_Allocation)'Address;
			Pool.Next_Allocation :=
				Pool.Next_Allocation + Size_In_Storage_Elements;
		end Allocate_From_Subpool;
		
	end MR_Pool;
	
	Pool : MR_Pool.Mark_Release_Pool_Type (1024);
	
	package Dummy is
		type Dummy is new Ada.Finalization.Controlled with null record;
		overriding procedure Initialize (Object : in out Dummy);
		overriding procedure Adjust (Object : in out Dummy);
		overriding procedure Finalize (Object : in out Dummy);
	end Dummy;
	
	package body Dummy is
		overriding procedure Initialize (Object : in out Dummy) is
		begin
			Ada.Debug.Put ("at " & System.Address_Image (Object'Address));
		end Initialize;
		overriding procedure Adjust (Object : in out Dummy) is
		begin
			Ada.Debug.Put ("at " & System.Address_Image (Object'Address));
		end Adjust;
		overriding procedure Finalize (Object : in out Dummy) is
		begin
			Ada.Debug.Put ("at " & System.Address_Image (Object'Address));
		end Finalize;
	end Dummy;
	
	type A is access Dummy.Dummy;
	for A'Storage_Pool use Pool;
--	procedure Free is new Ada.Unchecked_Deallocation (Dummy.Dummy, A);
	
	X : array (1 .. 3) of A;
	Mark : MR_Pool.Subpool_Handle;
begin
	Mark := MR_Pool.Mark (Pool);
	for I in X'Range loop
		X (I) := new Dummy.Dummy;
	end loop;
	MR_Pool.Release (Mark);
	pragma Debug (Ada.Debug.Put ("OK"));
end subpool;
