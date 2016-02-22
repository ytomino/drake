with System.Address_To_Constant_Access_Conversions;
with System.Address_To_Named_Access_Conversions;
with System.Debug;
with System.Storage_Elements;
with System.Zero_Terminated_WStrings;
with C.string;
with C.winbase;
with C.windef;
with C.winerror;
with C.winnt;
package body System.Native_Environment_Variables is
   use type Storage_Elements.Storage_Offset;
   use type C.size_t;
   use type C.windef.DWORD;
   use type C.windef.WINBOOL;
   use type C.winnt.LPWCH;
   use type C.winnt.WCHAR;

   package LPCWCH_Conv is
      new Address_To_Constant_Access_Conversions (
         C.winnt.WCHAR,
         C.winnt.LPCWCH);

   procedure Get_1 (
      Name : not null C.winnt.LPCWCH;
      Length : out C.windef.DWORD;
      Found : out Boolean);
   procedure Get_1 (
      Name : not null C.winnt.LPCWCH;
      Length : out C.windef.DWORD;
      Found : out Boolean) is
   begin
      Length := C.winbase.GetEnvironmentVariable (Name, null, 0);
      Found := Length > 0
         or else C.winbase.GetLastError /= C.winerror.ERROR_ENVVAR_NOT_FOUND;
   end Get_1;

   function Get_2 (
      Name : not null C.winnt.LPCWCH;
      Length : C.windef.DWORD)
      return String;
   function Get_2 (
      Name : not null C.winnt.LPCWCH;
      Length : C.windef.DWORD)
      return String
   is
      Result : C.winnt.WCHAR_array (0 .. C.size_t (Length));
      Result_Length : C.windef.DWORD;
   begin
      Result_Length := C.winbase.GetEnvironmentVariable (
         Name,
         Result (0)'Access,
         Result'Length);
      return Zero_Terminated_WStrings.Value (
         Result (0)'Access,
         C.size_t (Result_Length));
   end Get_2;

   procedure Do_Separate (
      Item : not null C.winnt.LPCWCH;
      Name_Length : out C.size_t;
      Value : out C.winnt.LPCWCH);
   procedure Do_Separate (
      Item : not null C.winnt.LPCWCH;
      Name_Length : out C.size_t;
      Value : out C.winnt.LPCWCH)
   is
      --  skip first '=', it means special variable
      Next : constant C.winnt.LPCWCH := LPCWCH_Conv.To_Pointer (
         LPCWCH_Conv.To_Address (C.winnt.LPCWCH (Item))
         + Storage_Elements.Storage_Offset'(
            C.winnt.WCHAR'Size / Standard'Storage_Unit));
      P : C.wchar_t_ptr;
   begin
      P := C.string.wcschr (Next, C.wchar_t'Val (Character'Pos ('=')));
      if P /= null then
         Name_Length := C.size_t (
            (LPCWCH_Conv.To_Address (C.winnt.LPCWCH (P))
               - LPCWCH_Conv.To_Address (C.winnt.LPCWCH (Item)))
            / Storage_Elements.Storage_Offset'(
               C.winnt.WCHAR'Size / Standard'Storage_Unit));
         Value := LPCWCH_Conv.To_Pointer (
            LPCWCH_Conv.To_Address (C.winnt.LPCWCH (P))
            + Storage_Elements.Storage_Offset'(
               C.winnt.WCHAR'Size / Standard'Storage_Unit));
      else
         Name_Length := C.string.wcslen (Item);
         Value := LPCWCH_Conv.To_Pointer (
            LPCWCH_Conv.To_Address (C.winnt.LPCWCH (Item))
            + Storage_Elements.Storage_Offset (Name_Length)
               * (C.winnt.WCHAR'Size / Standard'Storage_Unit));
      end if;
   end Do_Separate;

   --  implementation

   function Value (Name : String) return String is
      W_Name : aliased C.winnt.WCHAR_array (
         0 ..
         Name'Length * Zero_Terminated_WStrings.Expanding);
      Length : C.windef.DWORD;
      Found : Boolean;
   begin
      Zero_Terminated_WStrings.To_C (Name, W_Name (0)'Access);
      Get_1 (W_Name (0)'Unchecked_Access, Length, Found => Found);
      if not Found then
         raise Constraint_Error;
      else
         return Get_2 (W_Name (0)'Unchecked_Access, Length);
      end if;
   end Value;

   function Value (Name : String; Default : String) return String is
      W_Name : C.winnt.WCHAR_array (
         0 ..
         Name'Length * Zero_Terminated_WStrings.Expanding);
      Length : C.windef.DWORD;
      Found : Boolean;
   begin
      Zero_Terminated_WStrings.To_C (Name, W_Name (0)'Access);
      Get_1 (W_Name (0)'Unchecked_Access, Length, Found => Found);
      if not Found then
         return Default;
      else
         return Get_2 (W_Name (0)'Unchecked_Access, Length);
      end if;
   end Value;

   function Exists (Name : String) return Boolean is
      W_Name : C.winnt.WCHAR_array (
         0 ..
         Name'Length * Zero_Terminated_WStrings.Expanding);
      Length : C.windef.DWORD;
      Found : Boolean;
   begin
      Zero_Terminated_WStrings.To_C (Name, W_Name (0)'Access);
      Get_1 (W_Name (0)'Unchecked_Access, Length, Found => Found);
      return Found;
   end Exists;

   procedure Set (Name : String; Value : String) is
      W_Name : C.winnt.WCHAR_array (
         0 ..
         Name'Length * Zero_Terminated_WStrings.Expanding);
      W_Value : C.winnt.WCHAR_array (
         0 ..
         Value'Length * Zero_Terminated_WStrings.Expanding);
   begin
      Zero_Terminated_WStrings.To_C (Name, W_Name (0)'Access);
      Zero_Terminated_WStrings.To_C (Value, W_Value (0)'Access);
      if C.winbase.SetEnvironmentVariable (
         W_Name (0)'Access,
         W_Value (0)'Access) = 0
      then
         raise Constraint_Error;
      end if;
   end Set;

   procedure Clear (Name : String) is
      W_Name : C.winnt.WCHAR_array (
         0 ..
         Name'Length * Zero_Terminated_WStrings.Expanding);
   begin
      Zero_Terminated_WStrings.To_C (Name, W_Name (0)'Access);
      if C.winbase.SetEnvironmentVariable (
         W_Name (0)'Access,
         null) = 0
      then
         raise Constraint_Error;
      end if;
   end Clear;

   procedure Clear is
      Block : constant Address := Get_Block;
      I : Cursor := First (Block);
      Error : Boolean := False;
   begin
      while Has_Element (I) loop
         declare
            Item : constant C.winnt.LPCWCH :=
               LPCWCH_Conv.To_Pointer (Address (I));
         begin
            if Item.all /= Character'Pos ('=') then -- skip special variable
               declare
                  Name_Length : C.size_t;
                  Value : C.winnt.LPCWCH;
               begin
                  Do_Separate (Item, Name_Length, Value);
                  declare
                     pragma Suppress (Alignment_Check);
                     Item_A : C.winnt.WCHAR_array (C.size_t);
                     for Item_A'Address use LPCWCH_Conv.To_Address (Item);
                     Name : aliased C.winnt.WCHAR_array (0 .. Name_Length);
                  begin
                     Name (0 .. Name_Length - 1) :=
                        Item_A (0 .. Name_Length - 1);
                     Name (Name_Length) := C.winnt.WCHAR'Val (0);
                     if C.winbase.SetEnvironmentVariable (
                        Name (0)'Access,
                        null) = 0
                     then
                        Error := True;
                        exit;
                     end if;
                  end;
               end;
            end if;
         end;
         I := Next (Block, I);
      end loop;
      Release_Block (Block);
      if Error then
         raise Constraint_Error;
      end if;
   end Clear;

   function Has_Element (Position : Cursor) return Boolean is
   begin
      return LPCWCH_Conv.To_Pointer (Address (Position)).all /=
         C.winnt.WCHAR'Val (0);
   end Has_Element;

   function Name (Position : Cursor) return String is
      Item : constant C.winnt.LPCWCH :=
         LPCWCH_Conv.To_Pointer (Address (Position));
      Name_Length : C.size_t;
      Value : C.winnt.LPCWCH;
   begin
      Do_Separate (Item, Name_Length, Value);
      return Zero_Terminated_WStrings.Value (Item, Name_Length);
   end Name;

   function Value (Position : Cursor) return String is
      Item : constant C.winnt.LPCWCH :=
         LPCWCH_Conv.To_Pointer (Address (Position));
      Name_Length : C.size_t;
      Value : C.winnt.LPCWCH;
   begin
      Do_Separate (Item, Name_Length, Value);
      return Zero_Terminated_WStrings.Value (Value);
   end Value;

   function Get_Block return Address is
      --  a trailing W for calling GetEnvironmentStringsW is necessary.
      --  since kernel32.dll exports not GetEnvironmentStringsA
      --  but GetEnvironmentStrings.
      Result : constant C.winnt.LPWCH := C.winbase.GetEnvironmentStringsW;
   begin
      if Result = null then
         raise Constraint_Error;
      else
         return LPCWCH_Conv.To_Address (C.winnt.LPCWCH (Result));
      end if;
   end Get_Block;

   procedure Release_Block (Block : Address) is
      package LPWCH_Conv is
         new Address_To_Named_Access_Conversions (
            C.winnt.WCHAR,
            C.winnt.LPWCH);
      R : C.windef.WINBOOL;
   begin
      R := C.winbase.FreeEnvironmentStrings (LPWCH_Conv.To_Pointer (Block));
      pragma Check (Debug,
         Check => R /= 0
            or else Debug.Runtime_Error ("FreeEnvironmentStrings failed"));
   end Release_Block;

   function First (Block : Address) return Cursor is
   begin
      return Cursor (Block);
   end First;

   function Next (Block : Address; Position : Cursor) return Cursor is
      pragma Unreferenced (Block);
      Item_Length : constant C.size_t :=
         C.string.wcslen (LPCWCH_Conv.To_Pointer (Address (Position)));
   begin
      return Cursor (
         Address (Position)
         + (Storage_Elements.Storage_Offset (Item_Length) + 1)
            * (C.winnt.WCHAR'Size / Standard'Storage_Unit));
   end Next;

end System.Native_Environment_Variables;
