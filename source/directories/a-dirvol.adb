package body Ada.Directories.Volumes is

   --  implementation

   function Where (Name : String) return File_System is
   begin
      return Result : File_System do
         pragma Unmodified (Result); -- modified via Reference
         System.Native_Directories.Volumes.Get (Name, Reference (Result).all);
      end return;
   end Where;

   function Size (FS : File_System) return File_Size is
   begin
      return System.Native_Directories.Volumes.Size (Reference (FS).all);
   end Size;

   function Free_Space (FS : File_System) return File_Size is
   begin
      return System.Native_Directories.Volumes.Free_Space (Reference (FS).all);
   end Free_Space;

   function Owner (FS : File_System) return String is
   begin
      return System.Native_Directories.Volumes.Owner (Reference (FS).all);
   end Owner;

   function Format_Name (FS : File_System) return String is
   begin
      return System.Native_Directories.Volumes.Format_Name (
         Reference (FS).all);
   end Format_Name;

   function Directory (FS : File_System) return String is
   begin
      return System.Native_Directories.Volumes.Directory (Reference (FS).all);
   end Directory;

   function Device (FS : File_System) return String is
   begin
      return System.Native_Directories.Volumes.Device (Reference (FS).all);
   end Device;

   function Case_Preserving (FS : File_System) return Boolean is
   begin
      return System.Native_Directories.Volumes.Case_Preserving (
         Reference (FS).all);
   end Case_Preserving;

   function Case_Sensitive (FS : File_System) return Boolean is
   begin
      return System.Native_Directories.Volumes.Case_Sensitive (
         Reference (FS).all);
   end Case_Sensitive;

   function Is_HFS (FS : File_System) return Boolean is
   begin
      return System.Native_Directories.Volumes.Is_HFS (Reference (FS).all);
   end Is_HFS;

end Ada.Directories.Volumes;
