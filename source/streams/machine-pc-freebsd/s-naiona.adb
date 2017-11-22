with System.Address_To_Named_Access_Conversions;
with System.Zero_Terminated_Strings;
with C.stdlib;
package body System.Native_IO.Names is
   use type C.char;
   use type C.char_ptr;
   use type C.size_t;

   function strlen (s : not null access constant C.char) return C.size_t
      with Import,
         Convention => Intrinsic, External_Name => "__builtin_strlen";

   package Name_Pointer_Conv is
      new Address_To_Named_Access_Conversions (Name_Character, Name_Pointer);

   --  implementation

   procedure Open_Ordinary (
      Method : Open_Method;
      Handle : aliased out Handle_Type;
      Mode : File_Mode;
      Name : String;
      Out_Name : aliased out Name_Pointer;
      Form : Packed_Form)
   is
      Name_Length : constant Natural := Name'Length;
      Out_Name_Length : C.size_t;
   begin
      if Name (Name'First) = '/' then
         --  absolute path
         Out_Name := Name_Pointer_Conv.To_Pointer (
            Address (
               C.stdlib.malloc (
                  C.size_t (Name_Length) * Zero_Terminated_Strings.Expanding
                     + 1))); -- NUL
         if Out_Name = null then
            raise Storage_Error;
         end if;
         Out_Name_Length := 0;
      else
         --  current directory
         Out_Name := C.unistd.getcwd (null, 0);
         Out_Name_Length := strlen (Out_Name);
         --  reuse the memory from malloc (similar to reallocf)
         declare
            New_Out_Name : constant Name_Pointer :=
               Name_Pointer_Conv.To_Pointer (
                  Address (
                     C.stdlib.realloc (
                        C.void_ptr (Name_Pointer_Conv.To_Address (Out_Name)),
                        Out_Name_Length
                           + C.size_t (Name_Length)
                              * Zero_Terminated_Strings.Expanding
                           + 2))); -- '/' & NUL
         begin
            if New_Out_Name = null then
               raise Storage_Error;
            end if;
            Out_Name := New_Out_Name;
         end;
         --  append slash
         declare
            Out_Name_All : Name_String (0 .. Out_Name_Length); -- '/'
            for Out_Name_All'Address use
               Name_Pointer_Conv.To_Address (Out_Name);
         begin
            if Out_Name_All (Out_Name_Length - 1) /= '/' then
               Out_Name_All (Out_Name_Length) := '/';
               Out_Name_Length := Out_Name_Length + 1;
            end if;
         end;
      end if;
      --  append Item
      declare
         Out_Name_All : Name_String (0 .. Out_Name_Length); -- NUL
         for Out_Name_All'Address use Name_Pointer_Conv.To_Address (Out_Name);
      begin
         Zero_Terminated_Strings.To_C (
            Name,
            Out_Name_All (Out_Name_Length)'Access);
      end;
      --  open
      Open_Ordinary (Method, Handle, Mode, Out_Name, Form);
   end Open_Ordinary;

end System.Native_IO.Names;
