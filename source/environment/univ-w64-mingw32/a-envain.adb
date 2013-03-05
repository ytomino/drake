with System.Address_To_Constant_Access_Conversions;
with System.Address_To_Named_Access_Conversions;
with System.Storage_Elements;
with System.Zero_Terminated_WStrings;
with C.string;
with C.windef;
with C.winbase;
with C.winerror;
with C.winnt;
package body Ada.Environment_Variables.Inside is
   pragma Suppress (All_Checks);
   use type System.Storage_Elements.Storage_Offset;
   use type C.size_t;
   use type C.windef.DWORD;
   use type C.windef.WINBOOL;
   use type C.winnt.LPWCH;
   use type C.winnt.WCHAR;

   package LPCWCH_Conv is
      new System.Address_To_Constant_Access_Conversions (
         C.winnt.WCHAR,
         C.winnt.LPCWCH);

   procedure Get_1 (
      Name : not null access constant C.winnt.WCHAR;
      Length : out C.windef.DWORD;
      Found : out Boolean);
   procedure Get_1 (
      Name : not null access constant C.winnt.WCHAR;
      Length : out C.windef.DWORD;
      Found : out Boolean) is
   begin
      Length := C.winbase.GetEnvironmentVariable (Name, null, 0);
      Found := Length > 0
         or else C.winbase.GetLastError /= C.winerror.ERROR_ENVVAR_NOT_FOUND;
   end Get_1;

   function Get_2 (
      Name : not null access constant C.winnt.WCHAR;
      Length : C.windef.DWORD)
      return String;

   function Get_2 (
      Name : not null access constant C.winnt.WCHAR;
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
      return System.Zero_Terminated_WStrings.Value (
         Result (0)'Access,
         C.signed_int (Result_Length));
   end Get_2;

   procedure Do_Separate (
      Item : not null access constant C.winnt.WCHAR;
      Name_Length : out C.size_t;
      Value : out C.winnt.LPCWCH);
   procedure Do_Separate (
      Item : not null access constant C.winnt.WCHAR;
      Name_Length : out C.size_t;
      Value : out C.winnt.LPCWCH)
   is
      --  skip first '=', it means special variable
      Next : constant C.winnt.LPCWCH := LPCWCH_Conv.To_Pointer (
         LPCWCH_Conv.To_Address (C.winnt.LPCWCH (Item))
         + (C.winnt.WCHAR'Size / Standard'Storage_Unit));
      P : C.wchar_t_ptr;
   begin
      P := C.string.wcschr (Next, C.wchar_t'Val (Character'Pos ('=')));
      if P /= null then
         Name_Length := C.size_t (
            (LPCWCH_Conv.To_Address (C.winnt.LPCWCH (P))
               - LPCWCH_Conv.To_Address (C.winnt.LPCWCH (Item)))
            / (C.winnt.WCHAR'Size / Standard'Storage_Unit));
         Value := LPCWCH_Conv.To_Pointer (
            LPCWCH_Conv.To_Address (C.winnt.LPCWCH (P))
            + (C.winnt.WCHAR'Size / Standard'Storage_Unit));
      else
         Name_Length := C.string.wcslen (Item);
         Value := LPCWCH_Conv.To_Pointer (
            LPCWCH_Conv.To_Address (C.winnt.LPCWCH (Item))
            + System.Storage_Elements.Storage_Offset (Name_Length)
               * (C.winnt.WCHAR'Size / Standard'Storage_Unit));
      end if;
   end Do_Separate;

   --  implementation

   function Value (Name : String) return String is
      W_Name : aliased C.winnt.WCHAR_array (0 .. Name'Length);
      Length : C.windef.DWORD;
      Found : Boolean;
   begin
      System.Zero_Terminated_WStrings.Convert (Name, W_Name (0)'Access);
      Get_1 (W_Name (0)'Access, Length, Found => Found);
      if not Found then
         raise Constraint_Error;
      else
         return Get_2 (W_Name (0)'Access, Length);
      end if;
   end Value;

   function Value (Name : String; Default : String) return String is
      W_Name : C.winnt.WCHAR_array (0 .. Name'Length);
      Length : C.windef.DWORD;
      Found : Boolean;
   begin
      System.Zero_Terminated_WStrings.Convert (Name, W_Name (0)'Access);
      Get_1 (W_Name (0)'Access, Length, Found => Found);
      if not Found then
         return Default;
      else
         return Get_2 (W_Name (0)'Access, Length);
      end if;
   end Value;

   function Exists (Name : String) return Boolean is
      W_Name : C.winnt.WCHAR_array (0 .. Name'Length);
      Length : C.windef.DWORD;
      Found : Boolean;
   begin
      System.Zero_Terminated_WStrings.Convert (Name, W_Name (0)'Access);
      Get_1 (W_Name (0)'Access, Length, Found => Found);
      return Found;
   end Exists;

   procedure Set (Name : String; Value : String) is
      W_Name : C.winnt.WCHAR_array (0 .. Name'Length);
      W_Value : C.winnt.WCHAR_array (0 .. Value'Length);
   begin
      System.Zero_Terminated_WStrings.Convert (Name, W_Name (0)'Access);
      System.Zero_Terminated_WStrings.Convert (Value, W_Value (0)'Access);
      if C.winbase.SetEnvironmentVariable (
         W_Name (0)'Access,
         W_Value (0)'Access) = 0
      then
         raise Constraint_Error;
      end if;
   end Set;

   procedure Clear (Name : String) is
      W_Name : C.winnt.WCHAR_array (0 .. Name'Length);
   begin
      System.Zero_Terminated_WStrings.Convert (Name, W_Name (0)'Access);
      if C.winbase.SetEnvironmentVariable (
         W_Name (0)'Access,
         null) = 0
      then
         raise Constraint_Error;
      end if;
   end Clear;

   procedure Clear is
      Block : constant System.Address := Get_Block;
      I : Cursor := First (Block);
      Error : Boolean := False;
   begin
      while Has_Element (I) loop
         declare
            Item : constant C.winnt.LPCWCH :=
               LPCWCH_Conv.To_Pointer (System.Address (I));
         begin
            if Item.all /= Character'Pos ('=') then -- skip special variable
               declare
                  Name_Length : C.size_t;
                  Value : C.winnt.LPCWCH;
               begin
                  Do_Separate (Item, Name_Length, Value);
                  declare
                     Item_S : C.winnt.WCHAR_array (C.size_t);
                     for Item_S'Address use LPCWCH_Conv.To_Address (Item);
                     Name : aliased C.winnt.WCHAR_array (0 .. Name_Length);
                  begin
                     Name (0 .. Name_Length - 1) :=
                        Item_S (0 .. Name_Length - 1);
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
      return LPCWCH_Conv.To_Pointer (System.Address (Position)).all /=
         C.winnt.WCHAR'Val (0);
   end Has_Element;

   function Name (Position : Cursor) return String is
      Item : constant C.winnt.LPCWCH :=
         LPCWCH_Conv.To_Pointer (System.Address (Position));
      Name_Length : C.size_t;
      Value : C.winnt.LPCWCH;
   begin
      Do_Separate (Item, Name_Length, Value);
      return System.Zero_Terminated_WStrings.Value (
         Item,
         C.signed_int (Name_Length));
   end Name;

   function Value (Position : Cursor) return String is
      Item : constant C.winnt.LPCWCH :=
         LPCWCH_Conv.To_Pointer (System.Address (Position));
      Name_Length : C.size_t;
      Value : C.winnt.LPCWCH;
   begin
      Do_Separate (Item, Name_Length, Value);
      return System.Zero_Terminated_WStrings.Value (Value);
   end Value;

   function Get_Block return System.Address is
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

   procedure Release_Block (Block : System.Address) is
      package LPWCH_Conv is
         new System.Address_To_Named_Access_Conversions (
            C.winnt.WCHAR,
            C.winnt.LPWCH);
      R : C.windef.WINBOOL;
   begin
      R := C.winbase.FreeEnvironmentStrings (LPWCH_Conv.To_Pointer (Block));
      pragma Assert (R /= 0);
   end Release_Block;

   function First (Block : System.Address) return Cursor is
   begin
      return Cursor (Block);
   end First;

   function Next (Block : System.Address; Position : Cursor) return Cursor is
      pragma Unreferenced (Block);
      Item_Length : constant C.size_t :=
         C.string.wcslen (LPCWCH_Conv.To_Pointer (System.Address (Position)));
   begin
      return Cursor (
         System.Address (Position)
         + (System.Storage_Elements.Storage_Offset (Item_Length) + 1)
            * (C.winnt.WCHAR'Size / Standard'Storage_Unit));
   end Next;

end Ada.Environment_Variables.Inside;
