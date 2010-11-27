with Ada.Environment_Variables.Inside;
with System.Zero_Terminated_Strings;
with C.stdlib;
with C.string;
package body Ada.Environment_Variables is
   pragma Suppress (All_Checks);
   use type C.char_ptr;
   use type C.signed_int;

   function getenv (Name : String) return C.char_ptr;
   function getenv (Name : String) return C.char_ptr is
      Name_Length : constant C.size_t := Name'Length;
      C_Name : C.char_array (0 .. Name_Length);
      Dummy : C.void_ptr;
      pragma Unreferenced (Dummy);
   begin
      Dummy := C.string.memcpy (
         C.void_ptr (C_Name'Address),
         C.void_const_ptr (Name'Address),
         Name_Length);
      C_Name (Name_Length) := C.char'Val (0);
      return C.stdlib.getenv (C_Name (0)'Access);
   end getenv;

   function Value (Name : String) return String is
      Result : constant C.char_ptr := getenv (Name);
   begin
      if Result = null then
         raise Constraint_Error;
      else
         return System.Zero_Terminated_Strings.Value (Result.all'Address);
      end if;
   end Value;

   function Exists (Name : String) return Boolean is
   begin
      return getenv (Name) /= null;
   end Exists;

   procedure Set (Name : String; Value : String) is
      Name_Length : constant C.size_t := Name'Length;
      C_Name : C.char_array (0 .. Name_Length);
      Value_Length : constant C.size_t := Value'Length;
      C_Value : C.char_array (0 .. Value_Length);
      Dummy : C.void_ptr;
      pragma Unreferenced (Dummy);
   begin
      Dummy := C.string.memcpy (
         C.void_ptr (C_Name'Address),
         C.void_const_ptr (Name'Address),
         Name_Length);
      C_Name (Name_Length) := C.char'Val (0);
      Dummy := C.string.memcpy (
         C.void_ptr (C_Value'Address),
         C.void_const_ptr (Value'Address),
         Value_Length);
      C_Value (Value_Length) := C.char'Val (0);
      if C.stdlib.setenv (C_Name (0)'Access, C_Value (0)'Access, 1) /= 0 then
         raise Constraint_Error;
      end if;
   end Set;

   procedure Clear (Name : String) is
      Name_Length : constant C.size_t := Name'Length;
      C_Name : C.char_array (0 .. Name_Length);
      Dummy : C.void_ptr;
      pragma Unreferenced (Dummy);
   begin
      Dummy := C.string.memcpy (
         C.void_ptr (C_Name'Address),
         C.void_const_ptr (Name'Address),
         Name_Length);
      C_Name (Name_Length) := C.char'Val (0);
      if C.stdlib.unsetenv (C_Name (0)'Access) /= 0 then
         raise Constraint_Error;
      end if;
   end Clear;

   procedure Clear is
      type String_Array is array (Natural) of C.char_ptr;
      environment : String_Array;
      pragma Import (C, environment);
      for environment'Address use Inside.Environment_Block.all'Address;
      Count : Natural := 0;
   begin
      while environment (Count) /= null loop
         Count := Count + 1;
      end loop;
      for I in reverse 0 .. Count - 1 loop
         declare
            p : constant C.char_ptr := environment (I);
            subtype Fixed_String is String (Positive);
            S : Fixed_String;
            for S'Address use p.all'Address;
            I : Positive := 1;
         begin
            loop
               case S (I) is
                  when '=' | Character'Val (0) =>
                     Clear (S (1 .. I - 1));
                     exit;
                  when others =>
                     null;
               end case;
               I := I + 1;
            end loop;
         end;
      end loop;
   end Clear;

   procedure Iterate (
      Process : not null access procedure (Name, Value : String))
   is
      type String_Array is array (Natural) of C.char_ptr;
      environment : String_Array;
      pragma Import (C, environment);
      for environment'Address use Inside.Environment_Block.all'Address;
      I : Natural := 0;
   begin
      while environment (I) /= null loop
         declare
            p : constant C.char_ptr := environment (I);
            Length : constant Natural := Integer (C.string.strlen (p));
            S : String (1 .. Length);
            for S'Address use p.all'Address;
         begin
            for I in S'Range loop
               if S (I) = '=' then
                  Process (S (1 .. I - 1), S (I + 1 .. Length));
                  goto Next;
               end if;
            end loop;
            Process (S, "");
         end;
         <<Next>>
         I := I + 1;
      end loop;
   end Iterate;

end Ada.Environment_Variables;
