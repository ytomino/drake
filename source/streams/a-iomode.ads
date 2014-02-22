pragma License (Unrestricted);
--  extended unit
package Ada.IO_Modes is
   --  This package provides the root type of File_Mode,
   --    and the types for the parameters Form of Stream_IO
   pragma Pure;

   type File_Mode is (In_File, Out_File, Append_File);

   --  the types for the parameters Form of Stream_IO

   type File_Shared_Spec is (
      Allow, -- "shared=allow" or "shared=yes"
      Read_Only, -- "shared=read"
      Deny, -- "shared=deny"
      By_Mode); -- "shared=no" or default
   type File_Shared is new File_Shared_Spec range Allow .. Deny;

--  subtype File_Wait is Boolean;
   --  False as "wait=false" or default
   --  True as "wait=true"

--  subtype File_Overwrite is Boolean;
   --  False as "overwrite=false"
   --  True as "overwrite=true" or default

end Ada.IO_Modes;
