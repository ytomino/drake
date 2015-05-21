pragma Check_Policy (Trace => Ignore);
with System.Address_To_Constant_Access_Conversions;
with System.Storage_Elements;
with C.basetsd;
with C.winnt;
separate (System.Unwind.Backtrace)
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

   procedure Backtrace (
      Item : aliased out Tracebacks_Array;
      Last : out Natural;
      Exclude_Min : Address;
      Exclude_Max : Address;
      Skip_Frames : Natural)
   is
      context : aliased C.winnt.CONTEXT;
      history : aliased C.winnt.UNWIND_HISTORY_TABLE := (
         Count => <>,
         Search => <>,
         LowAddress => <>,
         HighAddress => <>,
         F_Entry => (others => <>));
      Skip : Natural;
   begin
      pragma Check (Trace, Ada.Debug.Put ("start"));
      --  Get the context.
      C.winnt.RtlCaptureContext (context'Access);
      --  Setup unwind history table (a cached to speed-up unwinding).
      memset (
         history'Access,
         0,
         C.winnt.UNWIND_HISTORY_TABLE'Size / Standard'Storage_Unit);
      Last := Tracebacks_Array'First - 1;
      Skip := Skip_Frames;
      loop
         declare
            RuntimeFunction : C.winnt.PRUNTIME_FUNCTION;
            NvContext : aliased C.winnt.KNONVOLATILE_CONTEXT_POINTERS := (
               FloatingContext => (others => <>),
               IntegerContext => (others => <>));
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
                  PCDWORD64_Conv.To_Pointer (
                     System'To_Address (context.Rsp)).all;
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
            exit when System'To_Address (context.Rip) = Null_Address;
            if Skip > 0 then
               Skip := Skip - 1;
               pragma Check (Trace, Ada.Debug.Put ("skip"));
            elsif System'To_Address (context.Rip) >= Exclude_Min
               and then System'To_Address (context.Rip) <= Exclude_Max
            then
               Skip := 0; -- end of skip
               pragma Check (Trace, Ada.Debug.Put ("exclude"));
            else
               Last := Last + 1;
               Item (Last) :=
                  System'To_Address (context.Rip)
                  - Storage_Elements.Storage_Offset'(2);
               pragma Check (Trace, Ada.Debug.Put ("fill"));
               exit when Last >= Tracebacks_Array'Last;
            end if;
         end;
      end loop;
      pragma Check (Trace, Ada.Debug.Put ("end"));
   end Backtrace;

end Separated;
