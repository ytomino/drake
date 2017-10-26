package body Ada.Directories.Volumes is

   --  implementation

   function Is_Assigned (FS : File_System) return Boolean is
      N_FS : System.Native_Directories.Volumes.File_System
         renames Controlled.Reference (FS).all;
   begin
      return System.Native_Directories.Volumes.Is_Assigned (N_FS);
   end Is_Assigned;

   function Size (
      FS : File_System)
      return File_Size
   is
      pragma Check (Dynamic_Predicate,
         Check => Is_Assigned (FS) or else raise Status_Error);
      N_FS : System.Native_Directories.Volumes.File_System
         renames Controlled.Reference (FS).all;
   begin
      return System.Native_Directories.Volumes.Size (N_FS);
   end Size;

   function Free_Space (
      FS : File_System)
      return File_Size
   is
      pragma Check (Dynamic_Predicate,
         Check => Is_Assigned (FS) or else raise Status_Error);
      N_FS : System.Native_Directories.Volumes.File_System
         renames Controlled.Reference (FS).all;
   begin
      return System.Native_Directories.Volumes.Free_Space (N_FS);
   end Free_Space;

   function Owner (
      FS : File_System)
      return String
   is
      pragma Check (Dynamic_Predicate,
         Check => Is_Assigned (FS) or else raise Status_Error);
      N_FS : System.Native_Directories.Volumes.File_System
         renames Controlled.Reference (FS).all;
   begin
      return System.Native_Directories.Volumes.Owner (N_FS);
   end Owner;

   function Format_Name (
      FS : File_System)
      return String
   is
      pragma Check (Dynamic_Predicate,
         Check => Is_Assigned (FS) or else raise Status_Error);
      N_FS : System.Native_Directories.Volumes.File_System
         renames Controlled.Reference (FS).all;
   begin
      return System.Native_Directories.Volumes.Format_Name (N_FS);
   end Format_Name;

   function Directory (
      FS : File_System)
      return String
   is
      pragma Check (Dynamic_Predicate,
         Check => Is_Assigned (FS) or else raise Status_Error);
      N_FS : System.Native_Directories.Volumes.File_System
         renames Controlled.Reference (FS).all;
   begin
      return System.Native_Directories.Volumes.Directory (N_FS);
   end Directory;

   function Device (
      FS : File_System)
      return String
   is
      pragma Check (Dynamic_Predicate,
         Check => Is_Assigned (FS) or else raise Status_Error);
      N_FS : System.Native_Directories.Volumes.File_System
         renames Controlled.Reference (FS).all;
   begin
      return System.Native_Directories.Volumes.Device (N_FS);
   end Device;

   function Case_Preserving (
      FS : File_System)
      return Boolean
   is
      pragma Check (Dynamic_Predicate,
         Check => Is_Assigned (FS) or else raise Status_Error);
      N_FS : System.Native_Directories.Volumes.File_System
         renames Controlled.Reference (FS).all;
   begin
      return System.Native_Directories.Volumes.Case_Preserving (N_FS);
   end Case_Preserving;

   function Case_Sensitive (
      FS : File_System)
      return Boolean
   is
      pragma Check (Dynamic_Predicate,
         Check => Is_Assigned (FS) or else raise Status_Error);
      N_FS : System.Native_Directories.Volumes.File_System
         renames Controlled.Reference (FS).all;
   begin
      return System.Native_Directories.Volumes.Case_Sensitive (N_FS);
   end Case_Sensitive;

   function Is_HFS (
      FS : File_System)
      return Boolean
   is
      pragma Check (Dynamic_Predicate,
         Check => Is_Assigned (FS) or else raise Status_Error);
      N_FS : System.Native_Directories.Volumes.File_System
         renames Controlled.Reference (FS).all;
   begin
      return System.Native_Directories.Volumes.Is_HFS (N_FS);
   end Is_HFS;

   function Identity (
      FS : File_System)
      return File_System_Id
   is
      pragma Check (Dynamic_Predicate,
         Check => Is_Assigned (FS) or else raise Status_Error);
      N_FS : System.Native_Directories.Volumes.File_System
         renames Controlled.Reference (FS).all;
   begin
      return File_System_Id (
         System.Native_Directories.Volumes.Identity (N_FS));
   end Identity;

   package body Controlled is

      function Reference (Object : Volumes.File_System)
         return not null
            access System.Native_Directories.Volumes.File_System is
      begin
         return File_System (Object).Data'Unrestricted_Access;
      end Reference;

      function Where (Name : String) return Volumes.File_System is
      begin
         return Result : Volumes.File_System do
            System.Native_Directories.Volumes.Get (
               Name,
               File_System (Result).Data'Unrestricted_Access.all);
         end return;
      end Where;

      overriding procedure Finalize (Object : in out File_System) is
      begin
         System.Native_Directories.Volumes.Finalize (Object.Data);
      end Finalize;

   end Controlled;

end Ada.Directories.Volumes;
