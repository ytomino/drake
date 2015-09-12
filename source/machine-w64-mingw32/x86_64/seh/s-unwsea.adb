pragma Check_Policy (Trace => Ignore);
with Ada.Unchecked_Conversion;
with System.Address_To_Named_Access_Conversions;
with System.Address_To_Constant_Access_Conversions;
with System.Storage_Elements;
with System.Unwind.Mapping;
with System.Unwind.Representation;
with C.basetsd;
with C.unwind_pe;
package body System.Unwind.Searching is
   pragma Suppress (All_Checks);
   use type Representation.Machine_Occurrence_Access;
   use type Storage_Elements.Storage_Offset;
   use type C.ptrdiff_t;
   use type C.signed_int;
   use type C.size_t;
   use type C.unsigned_char;
   use type C.unsigned_char_const_ptr;
   use type C.unsigned_int; -- _Unwind_Ptr is unsigned int or unsigned long
   use type C.unsigned_long;
   use type C.unsigned_long_long;
   use type C.unwind.sleb128_t;
   use type C.winnt.PRUNTIME_FUNCTION;

   Foreign_Exception : aliased Exception_Data
      with Import,
         Convention => Ada,
         External_Name => "system__exceptions__foreign_exception";

   function builtin_eh_return_data_regno (A1 : C.signed_int)
      return C.signed_int
      with Import,
         Convention => Intrinsic,
         External_Name => "__builtin_eh_return_data_regno";

   package unsigned_char_const_ptr_Conv is
      new Address_To_Constant_Access_Conversions (
         C.unsigned_char,
         C.unsigned_char_const_ptr);

   function "+" (Left : C.unsigned_char_const_ptr; Right : C.ptrdiff_t)
      return C.unsigned_char_const_ptr
      with Convention => Intrinsic;
   pragma Inline_Always ("+");

   function "+" (Left : C.unsigned_char_const_ptr; Right : C.ptrdiff_t)
      return C.unsigned_char_const_ptr is
   begin
      return unsigned_char_const_ptr_Conv.To_Pointer (
         unsigned_char_const_ptr_Conv.To_Address (Left)
         + Storage_Elements.Storage_Offset (Right));
   end "+";

   function "<" (Left, Right : C.unsigned_char_const_ptr) return Boolean
      with Import, Convention => Intrinsic;

   package Unwind_Exception_ptr_Conv is
      new Address_To_Named_Access_Conversions (
         C.unwind.struct_Unwind_Exception,
         C.unwind.struct_Unwind_Exception_ptr);
   package PUINT16_Conv is
      new Address_To_Named_Access_Conversions (
         C.basetsd.UINT16,
         C.basetsd.PUINT16);
   package PDWORD64_Conv is
      new Address_To_Named_Access_Conversions (
         C.basetsd.DWORD64,
         C.basetsd.PDWORD64);

   --  (unwind-seh.c)
   STATUS_USER_DEFINED : constant := 2 ** 29;
   GCC_MAGIC : constant := 16#474343#; -- "GCC"
   STATUS_GCC_THROW : constant :=
      STATUS_USER_DEFINED + 0 * 2 ** 24 + GCC_MAGIC;

--  UWOP_PUSH_NONVOL : constant := 0;
   UWOP_ALLOC_LARGE : constant := 1;
--  UWOP_ALLOC_SMALL : constant := 2;
--  UWOP_SET_FPREG : constant := 3;
   UWOP_SAVE_NONVOL : constant := 4;
--  UWOP_SAVE_NONVOL_FAR : constant := 5;
   UWOP_SAVE_XMM128 : constant := 8;
--  UWOP_SAVE_XMM128_FAR : constant := 9;
   UWOP_PUSH_MACHFRAME : constant := 10;

   --  equivalent to __gnat_adjust_context (raise-gcc.c)
   procedure Adjust_Context (
      Unwind_Data : Address;
      Context_RSP : C.basetsd.ULONG64);
   procedure Adjust_Context (
      Unwind_Data : Address;
      Context_RSP : C.basetsd.ULONG64)
   is
      unw : C.unsigned_char_const_ptr :=
         unsigned_char_const_ptr_Conv.To_Pointer (Unwind_Data);
      rsp : C.basetsd.ULONG64 := Context_RSP;
      len : C.unsigned_char;
   begin
      --  Version = 1, no flags, no prolog.
      if unw.all /= 1
         or else C.unsigned_char_const_ptr'(unw + 1).all /= 0
         --  No frame pointer.
         or else C.unsigned_char_const_ptr'(unw + 3).all /= 0
      then
         null;
      else
         len := C.unsigned_char_const_ptr'(unw + 2).all;
         unw := unw + 4;
         while len > 0 loop
            --  Offset in prolog = 0.
            exit when unw.all /= 0;
            case C.unsigned_char_const_ptr'(unw + 1).all and 16#0f# is
               when UWOP_ALLOC_LARGE =>
                  --  Expect < 512KB.
                  exit when
                     (C.unsigned_char_const_ptr'(unw + 1).all and 16#f0#) /= 0;
                  rsp :=
                     rsp
                     + 8
                        * C.basetsd.ULONG64 (
                           PUINT16_Conv.To_Pointer (
                              unsigned_char_const_ptr_Conv.To_Address (
                                 unw + 2)).all);
                  len := len - 1;
                  unw := unw + 2;
               when UWOP_SAVE_NONVOL | UWOP_SAVE_XMM128 =>
                  len := len - 1;
                  unw := unw + 2;
               when UWOP_PUSH_MACHFRAME =>
                  declare
                     rip : C.basetsd.ULONG64;
                  begin
                     rip := rsp;
                     if (C.unsigned_char_const_ptr'(unw + 1).all and 16#f0#) =
                        16#10#
                     then
                        rip :=
                           rip
                           + C.basetsd.ULONG64'Size / Standard'Storage_Unit;
                     end if;
                     --  Adjust rip.
                     PDWORD64_Conv.To_Pointer (System'To_Address (rip)).all :=
                        PDWORD64_Conv.To_Pointer (System'To_Address (rip)).all
                        + 1;
                  end;
                  exit;
               when others =>
                  --  Unexpected.
                  exit;
            end case;
            unw := unw + 2;
            len := len - 1;
         end loop;
      end if;
   end Adjust_Context;

   --  implementation

   function Personality (
      ABI_Version : C.signed_int;
      Phases : C.unwind.Unwind_Action;
      Exception_Class : C.unwind.Unwind_Exception_Class;
      Exception_Object : access C.unwind.struct_Unwind_Exception;
      Context : access C.unwind.struct_Unwind_Context)
      return C.unwind.Unwind_Reason_Code
   is
      function To_GNAT is
         new Ada.Unchecked_Conversion (
            C.unwind.struct_Unwind_Exception_ptr,
            Representation.Machine_Occurrence_Access);
      function Cast is
         new Ada.Unchecked_Conversion (
            C.unwind.struct_Unwind_Exception_ptr,
            C.unwind.Unwind_Word);
      GCC_Exception : constant Representation.Machine_Occurrence_Access :=
         To_GNAT (Exception_Object);
      landing_pad : C.unwind.Unwind_Ptr;
      ttype_filter : C.unwind.Unwind_Sword; -- 0 => finally, others => handler
   begin
      pragma Check (Trace, Ada.Debug.Put ("enter"));
      if ABI_Version /= 1 then
         pragma Check (Trace, Ada.Debug.Put ("leave, ABI_Version /= 1"));
         return C.unwind.URC_FATAL_PHASE1_ERROR;
      end if;
      if Exception_Class = Representation.GNAT_Exception_Class
         and then C.unsigned_int (Phases) =
            (C.unwind.UA_CLEANUP_PHASE or C.unwind.UA_HANDLER_FRAME)
      then
         landing_pad := GCC_Exception.landing_pad;
         ttype_filter := GCC_Exception.ttype_filter;
         pragma Check (Trace, Ada.Debug.Put ("shortcut!"));
      else
         declare
            --  about region
            lsda : C.void_ptr;
            base : C.unwind.Unwind_Ptr;
            call_site_encoding : C.unsigned_char;
            call_site_table : C.unsigned_char_const_ptr;
            lp_base : aliased C.unwind.Unwind_Ptr;
            action_table : C.unsigned_char_const_ptr;
            ttype_encoding : C.unsigned_char;
            ttype_table : C.unsigned_char_const_ptr;
            ttype_base : C.unwind.Unwind_Ptr;
            --  about action
            table_entry : C.unsigned_char_const_ptr;
         begin
            if Context = null then
               pragma Check (Trace, Ada.Debug.Put ("leave, Context = null"));
               return C.unwind.URC_CONTINUE_UNWIND;
            end if;
            lsda := C.unwind.Unwind_GetLanguageSpecificData (Context);
            if Address (lsda) = Null_Address then
               pragma Check (Trace, Ada.Debug.Put ("leave, lsda = null"));
               return C.unwind.URC_CONTINUE_UNWIND;
            end if;
            base := C.unwind.Unwind_GetRegionStart (Context);
            declare
               p : C.unsigned_char_const_ptr :=
                  unsigned_char_const_ptr_Conv.To_Pointer (Address (lsda));
               tmp : aliased C.unwind.uleb128_t;
               lpbase_encoding : C.unsigned_char;
            begin
               lpbase_encoding := p.all;
               p := p + 1;
               if lpbase_encoding /= C.unwind_pe.DW_EH_PE_omit then
                  p := C.unwind_pe.read_encoded_value (
                     Context,
                     lpbase_encoding,
                     p,
                     lp_base'Access);
               else
                  lp_base := base;
               end if;
               ttype_encoding := p.all;
               p := p + 1;
               if ttype_encoding /= C.unwind_pe.DW_EH_PE_omit then
                  p := C.unwind_pe.read_uleb128 (p, tmp'Access);
                  ttype_table := p + C.ptrdiff_t (tmp);
               else
                  pragma Check (Trace, Ada.Debug.Put (
                     "ttype_encoding = DW_EH_PE_omit"));
                  ttype_table := null; -- be access violation ?
               end if;
               ttype_base := C.unwind_pe.base_of_encoded_value (
                  ttype_encoding,
                  Context);
               call_site_encoding := p.all;
               p := p + 1;
               call_site_table := C.unwind_pe.read_uleb128 (p, tmp'Access);
               action_table := call_site_table + C.ptrdiff_t (tmp);
            end;
            declare
               p : C.unsigned_char_const_ptr := call_site_table;
               ip_before_insn : aliased C.signed_int := 0;
               ip : C.unwind.Unwind_Ptr :=
                  C.unwind.Unwind_GetIPInfo (Context, ip_before_insn'Access);
            begin
               if ip_before_insn = 0 then
                  pragma Check (Trace, Ada.Debug.Put ("ip_before_insn = 0"));
                  ip := ip - 1;
               end if;
               loop
                  if not (p < action_table) then
                     pragma Check (Trace, Ada.Debug.Put (
                        "leave, not (p < action_table)"));
                     return C.unwind.URC_CONTINUE_UNWIND;
                  end if;
                  declare
                     cs_start, cs_len, cs_lp : aliased C.unwind.Unwind_Ptr;
                     cs_action : aliased C.unwind.uleb128_t;
                  begin
                     p := C.unwind_pe.read_encoded_value (
                        null,
                        call_site_encoding,
                        p,
                        cs_start'Access);
                     p := C.unwind_pe.read_encoded_value (
                        null,
                        call_site_encoding,
                        p,
                        cs_len'Access);
                     p := C.unwind_pe.read_encoded_value (
                        null,
                        call_site_encoding,
                        p,
                        cs_lp'Access);
                     p := C.unwind_pe.read_uleb128 (
                        p,
                        cs_action'Access);
                     if ip < base + cs_start then
                        pragma Check (Trace, Ada.Debug.Put (
                           "leave, ip < base + cs_start"));
                        return C.unwind.URC_CONTINUE_UNWIND;
                     elsif ip < base + cs_start + cs_len then
                        if cs_lp /= 0 then
                           landing_pad := lp_base + cs_lp;
                        else
                           pragma Check (Trace, Ada.Debug.Put (
                              "leave, cs_lp = 0"));
                           return C.unwind.URC_CONTINUE_UNWIND;
                        end if;
                        if cs_action /= 0 then
                           table_entry := action_table
                              + C.ptrdiff_t (cs_action - 1);
                        else
                           table_entry := null;
                        end if;
                        exit;
                     end if;
                  end;
               end loop;
            end;
            --  landing_pad is found in here
            if table_entry = null then
               ttype_filter := 0;
            else
               declare
                  p : C.unsigned_char_const_ptr := table_entry;
                  ar_filter, ar_disp : aliased C.unwind.sleb128_t;
               begin
                  loop
                     p := C.unwind_pe.read_sleb128 (p, ar_filter'Access);
                     declare
                        Dummy : C.unsigned_char_const_ptr;
                     begin
                        Dummy := C.unwind_pe.read_sleb128 (p, ar_disp'Access);
                     end;
                     if ar_filter = 0 then
                        ttype_filter := 0;
                        if ar_disp = 0 then
                           pragma Check (Trace, Ada.Debug.Put ("finally"));
                           exit;
                        end if;
                     elsif ar_filter > 0
                        and then (C.unsigned_int (Phases)
                           and C.unwind.UA_FORCE_UNWIND) = 0
                     then
                        declare
                           function Cast is
                              new Ada.Unchecked_Conversion (
                                 Exception_Data_Access,
                                 C.unwind.Unwind_Ptr);
                           function Cast is
                              new Ada.Unchecked_Conversion (
                                 C.char_const_ptr,
                                 C.unwind.Unwind_Ptr);
                           filter : constant C.ptrdiff_t :=
                              C.ptrdiff_t (ar_filter)
                              * C.ptrdiff_t (
                                 C.unwind_pe.size_of_encoded_value (
                                    ttype_encoding));
                           choice : aliased C.unwind.Unwind_Ptr;
                           is_handled : Boolean;
                           Dummy : C.unsigned_char_const_ptr;
                        begin
                           Dummy := C.unwind_pe.read_encoded_value_with_base (
                              ttype_encoding,
                              ttype_base,
                              ttype_table + (-filter),
                              choice'Access);
                           if Exception_Class =
                              Representation.GNAT_Exception_Class
                           then
                              if choice =
                                 Cast (Unhandled_Others_Value'Access)
                              then
                                 pragma Check (Trace, Ada.Debug.Put (
                                    "unhandled exception"));
                                 is_handled := True;
                              else
                                 is_handled :=
                                    choice = Cast (GCC_Exception.Occurrence.Id)
                                    or else
                                       (choice = Cast (Others_Value'Access)
                                       and then
                                          not GCC_Exception.Occurrence.Id.
                                             Not_Handled_By_Others)
                                    or else
                                       choice = Cast (All_Others_Value'Access);
                              end if;
                           else
                              pragma Check (Trace, Ada.Debug.Put (
                                 "foreign exception"));
                              is_handled :=
                                 choice = Cast (Others_Value'Access)
                                 or else
                                    choice = Cast (All_Others_Value'Access)
                                 or else
                                    choice = Cast (Foreign_Exception'Access);
                           end if;
                           if is_handled then
                              ttype_filter := C.unwind.Unwind_Sword (
                                 ar_filter);
                              pragma Check (Trace, Ada.Debug.Put (
                                 "handler is found"));
                              exit;
                           end if;
                        end;
                     else
                        pragma Check (Trace, Ada.Debug.Put ("ar_filter < 0"));
                        null;
                     end if;
                     if ar_disp = 0 then
                        pragma Check (Trace, Ada.Debug.Put (
                           "leave, ar_disp = 0"));
                        return C.unwind.URC_CONTINUE_UNWIND;
                     end if;
                     p := p + C.ptrdiff_t (ar_disp);
                  end loop;
               end;
            end if;
            --  ttype_filter is found (or 0) in here
            if (C.unsigned_int (Phases) and C.unwind.UA_SEARCH_PHASE) /= 0 then
               if ttype_filter = 0 then -- cleanup
                  pragma Check (Trace, Ada.Debug.Put (
                     "leave, UA_SEARCH_PHASE, cleanup"));
                  return C.unwind.URC_CONTINUE_UNWIND;
               else
                  --  Setup_Current_Excep (GCC_Exception);
                  null; -- exception tracing (a-exextr.adb) is not implementd.
                  --  shortcut for phase2
                  if Exception_Class = Representation.GNAT_Exception_Class then
                     pragma Check (Trace, Ada.Debug.Put ("save for shortcut"));
                     GCC_Exception.landing_pad := landing_pad;
                     GCC_Exception.ttype_filter := ttype_filter;
                  end if;
                  pragma Check (Trace, Ada.Debug.Put (
                     "leave, UA_SEARCH_PHASE, handler found"));
                  return C.unwind.URC_HANDLER_FOUND;
               end if;
            elsif Phases = C.unwind.UA_CLEANUP_PHASE then
               if ttype_filter = 0
                  and then Exception_Class =
                     Representation.GNAT_Exception_Class
                  and then GCC_Exception.Stack_Guard /= Null_Address
               then
                  declare
                     Stack_Pointer : constant C.unwind.Unwind_Word :=
                        C.unwind.Unwind_GetCFA (Context);
                  begin
                     if Stack_Pointer <
                        C.unwind.Unwind_Word (GCC_Exception.Stack_Guard)
                     then
                        pragma Check (Trace, Ada.Debug.Put (
                           "leave, skip cleanup"));
                        return C.unwind.URC_CONTINUE_UNWIND;
                     end if;
                  end;
               end if;
               pragma Check (Trace, Ada.Debug.Put (
                  "UA_CLEANUP_PHASE without UA_HANDLER_FRAME"));
               null; -- ???
            elsif Phases = C.unwind.UA_END_OF_STACK then
               pragma Check (Trace, Ada.Debug.Put ("leave, end of stack"));
               return C.unwind.URC_HANDLER_FOUND;
            else
               pragma Check (Trace, Ada.Debug.Put ("miscellany phase"));
               null; -- ???
            end if;
         end;
      end if;
      pragma Check (Trace, Ada.Debug.Put ("unwind!"));
      --  setup_to_install (raise-gcc.c)
      C.unwind.Unwind_SetGR (
         Context,
         builtin_eh_return_data_regno (0),
         Cast (C.unwind.struct_Unwind_Exception_ptr (Exception_Object)));
      C.unwind.Unwind_SetGR (
         Context,
         builtin_eh_return_data_regno (1),
         C.unwind.Unwind_Word'Mod (ttype_filter));
      C.unwind.Unwind_SetIP (Context, landing_pad);
      --  Setup_Current_Excep (GCC_Exception); -- moved to Begin_Handler
      pragma Check (Trace, Ada.Debug.Put ("leave"));
      return C.unwind.URC_INSTALL_CONTEXT;
   end Personality;

   function Personality_SEH (
      ms_exc : C.winnt.PEXCEPTION_RECORD;
      this_frame : C.void_ptr;
      ms_orig_context : C.winnt.PCONTEXT;
      ms_disp : C.winnt.PDISPATCHER_CONTEXT)
      return C.excpt.EXCEPTION_DISPOSITION
   is
      Result : C.excpt.EXCEPTION_DISPOSITION;
   begin
      pragma Check (Trace, Ada.Debug.Put ("enter"));
      if (ms_exc.ExceptionCode and STATUS_USER_DEFINED) = 0 then
         pragma Check (Trace, Ada.Debug.Put ("Windows exception"));
         declare
            the_exception : Representation.Machine_Occurrence_Access;
            excpip : constant C.basetsd.ULONG64 :=
               C.basetsd.ULONG64 (Address (ms_exc.ExceptionAddress));
         begin
            if excpip /= 0
               and then excpip >=
                  ms_disp.ImageBase
                  + C.basetsd.ULONG64 (ms_disp.FunctionEntry.BeginAddress)
               and then excpip <
                  ms_disp.ImageBase
                  + C.basetsd.ULONG64 (ms_disp.FunctionEntry.EndAddress)
            then
               --  This is a fault in this function.
               --  We need to adjust the return address
               --    before raising the GCC exception.
               declare
                  context : aliased C.winnt.CONTEXT;
                  mf_func : C.winnt.PRUNTIME_FUNCTION := null;
                  mf_imagebase : C.basetsd.ULONG64;
                  mf_rsp : C.basetsd.ULONG64 := 0;
               begin
                  --  Get the context.
                  C.winnt.RtlCaptureContext (context'Access);
                  loop
                     declare
                        RuntimeFunction : C.winnt.PRUNTIME_FUNCTION;
                        ImageBase : aliased C.basetsd.ULONG64;
                        HandlerData : aliased C.winnt.PVOID;
                        EstablisherFrame : aliased C.basetsd.ULONG64;
                     begin
                        --  Get function metadata.
                        RuntimeFunction :=
                           C.winnt.RtlLookupFunctionEntry (
                              context.Rip,
                              ImageBase'Access,
                              ms_disp.HistoryTable);
                        exit when RuntimeFunction = ms_disp.FunctionEntry;
                        mf_func := RuntimeFunction;
                        mf_imagebase := ImageBase;
                        mf_rsp := context.Rsp;
                        if RuntimeFunction = null then
                           --  In case of failure,
                           --    assume this is a leaf function.
                           context.Rip := PDWORD64_Conv.To_Pointer (
                              System'To_Address (context.Rsp)).all;
                           context.Rsp := context.Rsp + 8;
                        else
                           --  Unwind.
                           declare
                              Dummy : C.winnt.PEXCEPTION_ROUTINE;
                           begin
                              Dummy := C.winnt.RtlVirtualUnwind (
                                 0,
                                 ImageBase,
                                 context.Rip,
                                 RuntimeFunction,
                                 context'Access,
                                 HandlerData'Access,
                                 EstablisherFrame'Access,
                                 null);
                           end;
                        end if;
                        --  0 means bottom of the stack.
                        if context.Rip = 0 then
                           mf_func := null;
                           exit;
                        end if;
                     end;
                  end loop;
                  if mf_func /= null then
                     Adjust_Context (
                        System'To_Address (
                           mf_imagebase
                           + C.basetsd.ULONG64 (mf_func.UnwindData)),
                        mf_rsp);
                  end if;
               end;
            end if;
            the_exception := Mapping.New_Machine_Occurrence_From_SEH (ms_exc);
            if the_exception /= null then
               declare
                  exc : C.unwind.struct_Unwind_Exception_ptr;
               begin
                  --  Directly convert the system exception to a GCC one.
                  --  This is really breaking the API, but is necessary
                  --    for stack size reasons: the normal way is to call
                  --    Raise_From_Signal_Handler, which build the exception
                  --    and calls _Unwind_RaiseException, which unwinds
                  --    the stack and will call this personality routine.
                  --  But the Windows unwinder needs about 2KB of stack.
                  exc := the_exception.Header'Access;
                  exc.F_private := (others => 0);
                  ms_exc.ExceptionCode := STATUS_GCC_THROW;
                  ms_exc.NumberParameters := 1;
                  ms_exc.ExceptionInformation (0) :=
                     C.basetsd.ULONG_PTR (
                        Unwind_Exception_ptr_Conv.To_Address (exc));
               end;
            end if;
         end;
      end if;
      pragma Check (Trace, Ada.Debug.Put ("GCC_specific_handler"));
      Result := C.unwind.GCC_specific_handler (
         ms_exc,
         this_frame,
         ms_orig_context,
         ms_disp,
         Personality'Access);
      pragma Check (Trace, Ada.Debug.Put ("leave"));
      return Result;
   end Personality_SEH;

end System.Unwind.Searching;
