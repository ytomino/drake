with System.Zero_Terminated_WStrings;
with C.string;
with C.winnls;
with C.winnt;
package body Ada.Directories.Inside.File_Names is

   --  see http://blogs.msdn.com/b/michkap/archive/2005/10/17/481600.aspx

   function NTFS_Compare (Left, Right : String) return Integer;
   function NTFS_Compare (Left, Right : String) return Integer is
   begin
      --  LCMapString fails at zero length string
      if Left'Length = 0 then
         if Right'Length = 0 then
            return 0;
         else
            return -1; -- Left'Length < Right'Length
         end if;
      elsif Right'Length = 0 then
         return 1; -- Left'Length > Right'Length
      else
         declare
            W_Left : aliased C.winnt.WCHAR_array (0 .. Left'Length);
            W_Right : aliased C.winnt.WCHAR_array (0 .. Right'Length);
            W_Left_Length : C.size_t;
            W_Right_Length : C.size_t;
         begin
            System.Zero_Terminated_WStrings.Convert (
               Left,
               W_Left (0)'Access,
               W_Left_Length);
            System.Zero_Terminated_WStrings.Convert (
               Right,
               W_Right (0)'Access,
               W_Right_Length);
            W_Left_Length := C.size_t (C.winnls.LCMapString (
               C.winnt.LOCALE_INVARIANT,
               C.winnls.LCMAP_UPPERCASE,
               W_Left (0)'Access,
               C.signed_int (W_Left_Length),
               W_Left (0)'Access, -- overwritable when only LCMAP_UPPERCASE
               C.signed_int (W_Left_Length)));
            W_Left (W_Left_Length) := C.winnt.WCHAR'Val (0);
            W_Right_Length := C.size_t (C.winnls.LCMapString (
               C.winnt.LOCALE_INVARIANT,
               C.winnls.LCMAP_UPPERCASE,
               W_Right (0)'Access,
               C.signed_int (W_Right_Length),
               W_Right (0)'Access,
               C.signed_int (W_Right_Length)));
            W_Right (W_Right_Length) := C.winnt.WCHAR'Val (0);
            return Integer (
               C.string.wcscmp (W_Left (0)'Access, W_Right (0)'Access));
            --  it should be replaced to wmemcmp for practical use length info
         end;
      end if;
   end NTFS_Compare;

   --  implementation

   function Equal_File_Names (
      FS : Volumes.File_System;
      Left, Right : String)
      return Boolean is
   begin
      if not Volumes.Case_Sensitive (FS) then
         return NTFS_Compare (Left, Right) = 0;
      else
         return Left = Right;
      end if;
   end Equal_File_Names;

   function Less_File_Names (
      FS : Volumes.File_System;
      Left, Right : String)
      return Boolean is
   begin
      if not Volumes.Case_Sensitive (FS) then
         return NTFS_Compare (Left, Right) < 0;
      else
         return Left < Right;
      end if;
   end Less_File_Names;

end Ada.Directories.Inside.File_Names;
