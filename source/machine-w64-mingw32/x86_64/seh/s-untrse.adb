pragma Check_Policy (Trace, Off);
with Ada;
with System.Address_To_Constant_Access_Conversions;
with System.Storage_Elements;
with C.basetsd;
with C.winnt;
separate (System.Unwind.Traceback)
package body Separated is
   pragma Suppress (All_Checks);
   use type Storage_Elements.Storage_Offset;
   use type C.basetsd.DWORD64;
   use type C.winnt.PRUNTIME_FUNCTION;

   package PCDWORD64_Conv is
      new Address_To_Constant_Access_Conversions (
         C.basetsd.DWORD64,
         C.basetsd.DWORD64_const_ptr);

   procedure memset (
      b : not null access C.winnt.UNWIND_HISTORY_TABLE;
      c : Integer;
      n : Storage_Elements.Storage_Count);
   procedure memset (
      b : not null access C.winnt.KNONVOLATILE_CONTEXT_POINTERS;
      c : Integer;
      n : Storage_Elements.Storage_Count);
   pragma Import (Intrinsic, memset, "__builtin_memset");

   --  implementation

   procedure Get_Traceback (
      Traceback : out Tracebacks_Array;
      Length : out Natural;
      Exclude_Min : Address;
      Exclude_Max : Address;
      Skip_Frames : Natural)
   is
      context : aliased C.winnt.CONTEXT;
      history : aliased C.winnt.UNWIND_HISTORY_TABLE;
      Skipped_Frames : Natural;
   begin
      pragma Check (Trace, Ada.Debug.Put ("enter"));
      --  Get the context.
      C.winnt.RtlCaptureContext (context'Access);
      --  Setup unwind history table (a cached to speed-up unwinding).
      memset (
         history'Access,
         0,
         C.winnt.UNWIND_HISTORY_TABLE'Size / Standard'Storage_Unit);
      Length := 0;
      Skipped_Frames := 0;
      loop
         declare
            RuntimeFunction : C.winnt.PRUNTIME_FUNCTION;
            NvContext : aliased C.winnt.KNONVOLATILE_CONTEXT_POINTERS;
            ImageBase : aliased C.basetsd.ULONG64;
            HandlerData : aliased C.winnt.PVOID;
            EstablisherFrame : aliased C.basetsd.ULONG64;
            Dummy : C.winnt.PEXCEPTION_ROUTINE;
            pragma Unreferenced (Dummy);
         begin
            --  Get function metadata.
            RuntimeFunction := C.winnt.RtlLookupFunctionEntry (
               context.Rip,
               ImageBase'Access,
               history'Access);
            if RuntimeFunction = null then
               --  In case of failure, assume this is a leaf function.
               context.Rip :=
                  PCDWORD64_Conv.To_Pointer (Address (context.Rsp)).all;
               context.Rsp := context.Rsp + 8;
            else
               --  Unwind.
               memset (
                  NvContext'Access,
                  0,
                  C.winnt.KNONVOLATILE_CONTEXT_POINTERS'Size
                     / Standard'Storage_Unit);
                  Dummy := C.winnt.RtlVirtualUnwind (
                     0,
                     ImageBase,
                     context.Rip,
                     RuntimeFunction,
                     context'Access,
                     HandlerData'Access,
                     EstablisherFrame'Access,
                     NvContext'Access);
            end if;
            --  0 means bottom of the stack.
            exit when Address (context.Rip) = Null_Address;
            if Skipped_Frames < Skip_Frames then
               --  Skip frames.
               Skipped_Frames := Skipped_Frames + 1;
               pragma Check (Trace, Ada.Debug.Put ("skip"));
            elsif Address (context.Rip) >= Exclude_Min
               and then Address (context.Rip) <= Exclude_Max
            then
               --  Excluded frames.
               pragma Check (Trace, Ada.Debug.Put ("exclude"));
               null;
            else
               Length := Length + 1;
               Traceback (Length) :=
                  Address (context.Rip) - Storage_Elements.Storage_Offset'(2);
               pragma Check (Trace, Ada.Debug.Put ("fill"));
               exit when Length >= Tracebacks_Array'Length;
            end if;
         end;
      end loop;
      pragma Check (Trace, Ada.Debug.Put ("leave"));
   end Get_Traceback;

end Separated;
