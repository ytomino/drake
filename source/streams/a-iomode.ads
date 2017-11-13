pragma License (Unrestricted);
--  extended unit
package Ada.IO_Modes is
   --  Root types of File_Mode and for the parameters Form.
   pragma Pure;

   type File_Mode is (In_File, Out_File, Append_File);
   type Inout_File_Mode is (In_File, Inout_File, Out_File); -- Direct_IO

   --  the types for the parameters Form of Stream_IO

   type File_Shared_Spec is (
      Allow, -- "shared=allow", "shared=yes", or "shared=no"
      Read_Only, -- "shared=read"
      Deny, -- "shared=deny"
      By_Mode); --  default
   type File_Shared is new File_Shared_Spec range Allow .. Deny;

--  subtype File_Wait is Boolean;
      --  False as "wait=false", or default
      --  True as "wait=true"

--  subtype File_Overwrite is Boolean;
      --  False as "overwrite=false"
      --  True as "overwrite=true", or default

   --  the types for the parameters Form of Text_IO

   type File_External_Base is (
      Terminal,
      UTF_8, -- "external=utf-8", or "wcem=8"
      Locale, -- "external=dbcs", Windows only
      By_Target); -- default, UTF_8 in POSIX, or Locale in Windows
   type File_External_Spec is new File_External_Base range UTF_8 .. By_Target;
   type File_External is new File_External_Base range Terminal .. Locale;

   type File_New_Line_Spec is (
      LF, -- "nl=lf"
      CR, -- "nl=cr"
      CR_LF, -- "nl=m"
      By_Target); -- default, LF in POSIX, or CR_LF in Windows
   type File_New_Line is new File_New_Line_Spec range LF .. CR_LF;

end Ada.IO_Modes;
