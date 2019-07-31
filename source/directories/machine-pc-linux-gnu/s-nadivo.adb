with Ada.Exception_Identification.From_Here;
with Ada.Exceptions.Finally;
with System.Address_To_Named_Access_Conversions;
with System.Growth;
with System.Standard_Allocators;
with System.Storage_Elements;
with System.Zero_Terminated_Strings;
with C.errno;
with C.fcntl;
with C.unistd;
package body System.Native_Directories.Volumes is
   use Ada.Exception_Identification.From_Here;
   use type Storage_Elements.Storage_Offset;
   use type File_Size;
   use type C.char;
   use type C.char_array;
   use type C.char_ptr;
   use type C.ptrdiff_t;
   use type C.signed_int;
   use type C.signed_long; -- f_type in 64bit
   use type C.signed_long_long; -- f_type in x32
   use type C.size_t;
   use type C.sys.types.fsid_t;

   procedure memcpy (dst, src : not null C.char_ptr; n : C.size_t)
      with Import,
         Convention => Intrinsic, External_Name => "__builtin_memcpy";

   package char_ptr_Conv is
      new Address_To_Named_Access_Conversions (C.char, C.char_ptr);

   function "+" (Left : C.char_ptr; Right : C.ptrdiff_t) return C.char_ptr
      with Convention => Intrinsic;
   function "-" (Left : C.char_ptr; Right : C.char_ptr) return C.ptrdiff_t
      with Convention => Intrinsic;
   function "<" (Left, Right : C.char_ptr) return Boolean
      with Import, Convention => Intrinsic;

   pragma Inline_Always ("+");
   pragma Inline_Always ("-");

   function "+" (Left : C.char_ptr; Right : C.ptrdiff_t) return C.char_ptr is
   begin
      return char_ptr_Conv.To_Pointer (
         char_ptr_Conv.To_Address (Left)
            + Storage_Elements.Storage_Offset (Right));
   end "+";

   function "-" (Left : C.char_ptr; Right : C.char_ptr) return C.ptrdiff_t is
   begin
      return C.ptrdiff_t (
         char_ptr_Conv.To_Address (Left) - char_ptr_Conv.To_Address (Right));
   end "-";

   proc_self_mountinfo : aliased constant C.char_array (0 .. 20) :=
      "/proc/self/mountinfo" & C.char'Val (0);

   --  36 35 98:0 /mnt1 /mnt2 rw,noatime master:1 - ext3 /dev/root rw,...
   --  (1)(2)(3)   (4)   (5)      (6)      (7)   (8) (9)   (10)         (11)
   --   (1) mount ID
   --   (2) parent ID
   --   (3) major:minor, st_dev
   --   (4) root, '/'
   --   (5) mount point (for Directory)
   --   (6) mount options
   --   (7) optional fields
   --   (8) separator, '-'
   --   (9) filesystem type (for Format_Name)
   --  (10) mount source (for Device)
   --  (11) super options

   procedure Close (fd : in out C.signed_int);
   procedure Close (fd : in out C.signed_int) is
   begin
      if C.unistd.close (fd) < 0 then
         null; -- raise Use_Error
      end if;
   end Close;

   procedure Skip (P : in out C.char_ptr; To : C.char);
   procedure Skip (P : in out C.char_ptr; To : C.char) is
   begin
      while P.all /= C.char'Val (10) loop
         if P.all = To then
            P := P + 1;
            exit;
         end if;
         P := P + 1;
      end loop;
   end Skip;

   procedure Skip_Escaped (P : in out C.char_ptr; Length : out C.size_t);
   procedure Skip_Escaped (P : in out C.char_ptr; Length : out C.size_t) is
      Z : constant := C.char'Pos ('0');
      D : C.char_ptr := P;
   begin
      Length := 0;
      while P.all /= C.char'Val (10) loop
         if P.all = ' ' then
            P := P + 1;
            exit;
         elsif P.all = '\'
            and then C.char_ptr'(P + 1).all in '0' .. '3'
            and then C.char_ptr'(P + 2).all in '0' .. '7'
            and then C.char_ptr'(P + 3).all in '0' .. '7'
         then
            D.all :=
               C.char'Val (
                  64 * (C.char'Pos (C.char_ptr'(P + 1).all) - Z)
                  + 8 * (C.char'Pos (C.char_ptr'(P + 2).all) - Z)
                  + (C.char'Pos (C.char_ptr'(P + 3).all) - Z));
            P := P + 4;
         else
            if D /= P then
               D.all := P.all;
            end if;
            P := P + 1;
         end if;
         D := D + 1;
         Length := Length + 1;
      end loop;
   end Skip_Escaped;

   procedure Skip_Line (P : in out C.char_ptr);
   procedure Skip_Line (P : in out C.char_ptr) is
   begin
      while P.all /= C.char'Val (10) loop
         P := P + 1;
      end loop;
      P := P + 1; -- skip '\n'
   end Skip_Line;

   procedure Read_Info (FS : in out File_System);
   procedure Read_Info (FS : in out File_System) is
      package Buffer_Holder is
         new Growth.Scoped_Holder (
            C.sys.types.ssize_t,
            Component_Size => C.char_array'Component_Size);
      Size : C.size_t := 0;
   begin
      Buffer_Holder.Reserve_Capacity (4096);
      --  read /proc/self/mountinfo
      declare
         package File_Holder is
            new Ada.Exceptions.Finally.Scoped_Holder (C.signed_int, Close);
         File : aliased C.signed_int;
      begin
         File :=
            C.fcntl.open (proc_self_mountinfo (0)'Access, C.fcntl.O_RDONLY);
         if File < 0 then
            Raise_Exception (Named_IO_Exception_Id (C.errno.errno));
         end if;
         File_Holder.Assign (File);
         Reading : loop
            loop
               declare
                  Read_Size : C.sys.types.ssize_t;
               begin
                  Read_Size :=
                     C.unistd.read (
                        File,
                        C.void_ptr (
                           char_ptr_Conv.To_Address (
                              char_ptr_Conv.To_Pointer (
                                    Buffer_Holder.Storage_Address)
                                 + C.ptrdiff_t (Size))),
                        C.size_t (Buffer_Holder.Capacity) - Size);
                  if Read_Size < 0 then
                     Raise_Exception (IO_Exception_Id (C.errno.errno));
                  end if;
                  exit Reading when Read_Size = 0;
                  Size := Size + C.size_t (Read_Size);
               end;
               exit when Size >= C.size_t (Buffer_Holder.Capacity);
            end loop;
            --  growth
            declare
               function Grow is new Growth.Fast_Grow (C.sys.types.ssize_t);
            begin
               Buffer_Holder.Reserve_Capacity (Grow (Buffer_Holder.Capacity));
            end;
         end loop Reading;
      end;
      --  parsing
      declare
         End_Of_Buffer : constant C.char_ptr :=
            char_ptr_Conv.To_Pointer (Buffer_Holder.Storage_Address)
               + C.ptrdiff_t (Size);
         Line : C.char_ptr :=
            char_ptr_Conv.To_Pointer (Buffer_Holder.Storage_Address);
      begin
         while Line < End_Of_Buffer loop
            declare
               Directory_Offset : C.ptrdiff_t;
               Directory_Length : C.size_t;
               Format_Name_Offset : C.ptrdiff_t;
               Format_Name_Length : C.size_t;
               Device_Offset : C.ptrdiff_t;
               Device_Length : C.size_t;
               statfs : aliased C.sys.statfs.struct_statfs;
               P : C.char_ptr := Line;
            begin
               Skip (P, To => ' '); -- mount ID
               Skip (P, To => ' '); -- parent ID
               Skip (P, To => ' '); -- major:minor
               Skip (P, To => ' '); -- root
               Directory_Offset := P - Line; -- mount point
               Skip_Escaped (P, Directory_Length);
               Skip (P, To => ' '); -- mount options
               loop -- optional fields
                  Skip (P, To => ' ');
                  exit when P.all = '-' or else P.all = C.char'Val (10);
               end loop;
               Skip (P, To => ' '); -- separator
               Format_Name_Offset := P - Line; -- filesystem type
               Skip_Escaped (P, Format_Name_Length);
               Device_Offset := P - Line; -- mount source
               Skip_Escaped (P, Device_Length);
               Skip_Line (P); -- super options
               --  matching
               declare
                  End_Of_Directory : constant C.char_ptr :=
                     Line
                        + Directory_Offset
                        + C.ptrdiff_t (Directory_Length);
                  Orig : constant C.char := End_Of_Directory.all;
               begin
                  End_Of_Directory.all := C.char'Val (0);
                  if C.sys.statfs.statfs (
                     Line + Directory_Offset,
                     statfs'Access) < 0
                  then
                     Raise_Exception (Named_IO_Exception_Id (C.errno.errno));
                  end if;
                  End_Of_Directory.all := Orig;
               end;
               if statfs.f_fsid /= (val => (others => 0))
                  and then statfs.f_fsid = FS.Statistics.f_fsid
               then
                  FS.Format_Name_Offset := Format_Name_Offset;
                  FS.Format_Name_Length := Format_Name_Length;
                  FS.Directory_Offset := Directory_Offset;
                  FS.Directory_Length := Directory_Length;
                  FS.Device_Offset := Device_Offset;
                  FS.Device_Length := Device_Length;
                  FS.Info :=
                     char_ptr_Conv.To_Pointer (
                        Standard_Allocators.Allocate (
                           Storage_Elements.Storage_Count (P - Line)));
                  memcpy (FS.Info, Line, C.size_t (P - Line));
                  return; -- found
               end if;
               --  continue
               Line := P;
            end;
         end loop;
      end;
      Raise_Exception (Use_Error'Identity); -- not found
   end Read_Info;

   --  implementation

   function Is_Assigned (FS : File_System) return Boolean is
   begin
      return FS.Statistics.f_type /= 0;
   end Is_Assigned;

   procedure Get (Name : String; FS : aliased out File_System) is
      C_Name : C.char_array (
         0 ..
         Name'Length * Zero_Terminated_Strings.Expanding);
   begin
      Zero_Terminated_Strings.To_C (Name, C_Name (0)'Access);
      FS.Info := null;
      if C.sys.statfs.statfs (C_Name (0)'Access, FS.Statistics'Access) < 0 then
         Raise_Exception (Named_IO_Exception_Id (C.errno.errno));
      end if;
   end Get;

   procedure Finalize (FS : in out File_System) is
   begin
      Standard_Allocators.Free (char_ptr_Conv.To_Address (FS.Info));
   end Finalize;

   function Size (FS : File_System) return File_Size is
   begin
      return File_Size (FS.Statistics.f_blocks)
         * File_Size (FS.Statistics.f_bsize);
   end Size;

   function Free_Space (FS : File_System) return File_Size is
   begin
      return File_Size (FS.Statistics.f_bfree)
         * File_Size (FS.Statistics.f_bsize);
   end Free_Space;

   function Format_Name (FS : aliased in out File_System) return String is
   begin
      if FS.Info = null then
         Read_Info (FS);
      end if;
      return Zero_Terminated_Strings.Value (
         FS.Info + FS.Format_Name_Offset,
         FS.Format_Name_Length);
   end Format_Name;

   function Directory (FS : aliased in out File_System) return String is
   begin
      if FS.Info = null then
         Read_Info (FS);
      end if;
      return Zero_Terminated_Strings.Value (
         FS.Info + FS.Directory_Offset,
         FS.Directory_Length);
   end Directory;

   function Device (FS : aliased in out File_System) return String is
   begin
      if FS.Info = null then
         Read_Info (FS);
      end if;
      return Zero_Terminated_Strings.Value (
         FS.Info + FS.Device_Offset,
         FS.Device_Length);
   end Device;

   function Identity (FS : File_System) return File_System_Id is
   begin
      return FS.Statistics.f_fsid;
   end Identity;

end System.Native_Directories.Volumes;
