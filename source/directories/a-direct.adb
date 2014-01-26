with Ada.Directories.Inside;
with Ada.Exceptions;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with System.Form_Parameters;
with System.Native_Time;
with System.Storage_Elements;
package body Ada.Directories is
   use type Directory_Searching.Handle_Type;
   use type System.Storage_Elements.Storage_Offset;

   procedure Free is new Unchecked_Deallocation (String, String_Access);

   function Pack_For_Copy_File (Form : String) return Boolean;
   function Pack_For_Copy_File (Form : String) return Boolean is
      Keyword_First : Positive;
      Keyword_Last : Natural;
      Item_First : Positive;
      Item_Last : Natural;
      Last : Natural;
      Overwrite : Boolean := True; -- default
   begin
      Last := Form'First - 1;
      while Last < Form'Last loop
         System.Form_Parameters.Get (
            Form (Last + 1 .. Form'Last),
            Keyword_First,
            Keyword_Last,
            Item_First,
            Item_Last,
            Last);
         Set : declare
            Keyword : String
               renames Form (Keyword_First .. Keyword_Last);
            Item : String
               renames Form (Item_First .. Item_Last);
         begin
            if Keyword = "overwrite" then
               if Item'Length > 0 and then Item (Item'First) = 'f' then
                  Overwrite := False; -- false
               elsif Item'Length > 0 and then Item (Item'First) = 't' then
                  Overwrite := True; -- true
               end if;
            elsif Keyword = "mode" then
               --  compatibility with GNAT runtime
               if Item'Length > 0 and then Item (Item'First) = 'c' then
                  Overwrite := False; -- copy
               elsif Item'Length > 0 and then Item (Item'First) = 'o' then
                  Overwrite := True; -- overwrite
               end if;
            end if;
         end Set;
      end loop;
      return Overwrite;
   end Pack_For_Copy_File;

   procedure Next (Search : in out Search_Type);
   procedure Next (Search : in out Search_Type) is
   begin
      --  increment
      Search.Count := Search.Count + 1;
      --  search next
      Directory_Searching.Get_Next_Entry (
         Search.Search,
         Search.Next_Directory_Entry.Data'Access,
         Search.Has_Next);
   end Next;

   --  directory and file operations

   function Current_Directory return String
      renames Inside.Current_Directory;

   procedure Set_Directory (Directory : String)
      renames Inside.Set_Directory;

   procedure Create_Directory (New_Directory : String; Form : String := "")
      renames Inside.Create_Directory;

   procedure Delete_Directory (Directory : String)
      renames Inside.Delete_Directory;

   procedure Create_Path (New_Directory : String; Form : String := "") is
      pragma Unreferenced (Form);
   begin
      if not Exists (New_Directory) then
         declare
            D_First : Positive;
            D_Last : Natural;
         begin
            Containing_Directory (New_Directory, D_First, D_Last);
            if D_First <= D_Last then
               Create_Path (New_Directory (D_First .. D_Last)); -- recursive
            end if;
         end;
         Create_Directory (New_Directory);
      end if;
   end Create_Path;

   procedure Delete_Tree (Directory : String) is
      Search : Search_Type;
   begin
      Start_Search (Search, Directory, "*", (others => True));
      while More_Entries (Search) loop
         declare
            Directory_Entry : Directory_Entry_Type;
         begin
            Get_Next_Entry (Search, Directory_Entry);
            declare
               Name : constant String := Full_Name (Directory_Entry);
            begin
               case Kind (Directory_Entry) is
                  when Ordinary_File | Special_File =>
                     Delete_File (Full_Name (Name));
                  when Directories.Directory =>
                     Delete_Tree (Name); -- recursive
               end case;
            end;
         end;
      end loop;
      End_Search (Search);
      Delete_Directory (Directory);
   end Delete_Tree;

   procedure Delete_File (Name : String)
      renames Inside.Delete_File;

   procedure Rename (
      Old_Name : String;
      New_Name : String;
      Overwrite : Boolean := True)
      renames  Inside.Rename;

   procedure Copy_File (
      Source_Name : String;
      Target_Name : String;
      Form : String) is
   begin
      Inside.Copy_File (Source_Name, Target_Name, Pack_For_Copy_File (Form));
   end Copy_File;

   procedure Copy_File (
      Source_Name : String;
      Target_Name : String;
      Overwrite : Boolean := True)
      renames Inside.Copy_File;

   procedure Symbolic_Link (
      Source_Name : String;
      Target_Name : String;
      Overwrite : Boolean := True)
      renames Inside.Symbolic_Link;

   --  file and directory name operations

   function Full_Name (Name : String) return String
      renames Inside.Full_Name;

   --  file and directory queries

   function Exists (Name : String) return Boolean
      renames Inside.Exists;

   function Kind (Name : String) return File_Kind is
      Information : aliased Inside.Directory_Entry_Information_Type;
   begin
      Inside.Get_Information (Name, Information'Access);
      return Inside.Kind (Information);
   end Kind;

   function Size (Name : String) return File_Size is
      Information : aliased Inside.Directory_Entry_Information_Type;
   begin
      Inside.Get_Information (Name, Information'Access);
      if Inside.Kind (Information) /= Ordinary_File then
         raise Constraint_Error; -- implementation-defined
      else
         return Inside.Size (Information);
      end if;
   end Size;

   function Modification_Time (Name : String) return Calendar.Time is
      function Cast is new Unchecked_Conversion (Duration, Calendar.Time);
      Information : aliased Inside.Directory_Entry_Information_Type;
   begin
      Inside.Get_Information (Name, Information'Access);
      return Cast (System.Native_Time.To_Time (
         Inside.Modification_Time (Information)));
   end Modification_Time;

   procedure Set_Modification_Time (Name : String; Time : Calendar.Time) is
      function Cast is new Unchecked_Conversion (Calendar.Time, Duration);
   begin
      Inside.Set_Modification_Time (
         Name,
         System.Native_Time.To_Native_Time (Cast (Time)));
   end Set_Modification_Time;

   --  directory searching

   procedure Start_Search (
      Search : in out Search_Type;
      Directory : String;
      Pattern : String := "*";
      Filter : Filter_Type := (others => True))
   is
      function Cast is new
         Unchecked_Conversion (Filter_Type, Directory_Searching.Filter_Type);
   begin
      Finalize (Search); -- cleanup
      Directory_Searching.Start_Search (
         Search.Search,
         Directory,
         Pattern,
         Cast (Filter),
         Search.Next_Directory_Entry.Data'Access,
         Search.Has_Next);
      Search.Path := new String'(Full_Name (Directory));
      Search.Count := 1;
   end Start_Search;

   function Start_Search (
      Directory : String;
      Pattern : String := "*";
      Filter : Filter_Type := (others => True))
      return Search_Type is
   begin
      return Result : Search_Type do
         Start_Search (Result, Directory, Pattern, Filter);
      end return;
   end Start_Search;

   procedure Finalize (Search : in out Search_Type) is
   begin
      if Search.Search.Handle /= Directory_Searching.Null_Handle then
         Directory_Searching.End_Search (Search.Search);
         Free (Search.Path);
      end if;
   end Finalize;

   function More_Entries (Search : Search_Type) return Boolean is
   begin
      if Search.Search.Handle = Directory_Searching.Null_Handle then
         Exceptions.Raise_Exception_From_Here (Status_Error'Identity);
      end if;
      return Search.Has_Next;
   end More_Entries;

   procedure Get_Next_Entry (
      Search : in out Search_Type;
      Directory_Entry : out Directory_Entry_Type) is
   begin
      if Search.Search.Handle = Directory_Searching.Null_Handle
         or else not Search.Has_Next
      then
         Exceptions.Raise_Exception_From_Here (Status_Error'Identity);
      else
         --  copy entry and get info
         Directory_Entry.Search := Search'Unrestricted_Access; -- overwrite
         Directory_Entry.Data := Search.Next_Directory_Entry.Data;
         Directory_Entry.Additional.Filled := False;
         --  increment and search next
         Next (Search);
      end if;
   end Get_Next_Entry;

   procedure Search (
      Directory : String;
      Pattern : String := "*";
      Filter : Filter_Type := (others => True);
      Process : not null access procedure (
         Directory_Entry : Directory_Entry_Type))
   is
      Srch : Search_Type;
      Directory_Entry : Directory_Entry_Type;
   begin
      Start_Search (Srch, Directory, Pattern, Filter);
      while More_Entries (Srch) loop
         Get_Next_Entry (Srch, Directory_Entry);
         Process (Directory_Entry);
      end loop;
      End_Search (Srch);
   end Search;

   --  iterator

   function Has_Element (Position : Cursor) return Boolean is
   begin
      return Position > 0;
   end Has_Element;

   function Element (Container : Search_Type'Class; Position : Cursor)
      return Directory_Entry_Type is
   begin
      return Constant_Reference (Container, Position).Element.all;
   end Element;

   function Constant_Reference (
      Container : aliased Search_Type;
      Position : Cursor)
      return Constant_Reference_Type is
   begin
      if Integer (Position) /= Container.Count then
         Exceptions.Raise_Exception_From_Here (Status_Error'Identity);
      end if;
      return (Element => Container.Next_Directory_Entry'Access);
   end Constant_Reference;

   function Iterate (Container : Search_Type)
      return Search_Iterator_Interfaces.Forward_Iterator'Class is
   begin
      return Search_Iterator'(Search => Container'Unrestricted_Access);
   end Iterate;

   overriding function First (Object : Search_Iterator) return Cursor is
   begin
      if Object.Search.Search.Handle = Directory_Searching.Null_Handle
         or else Object.Search.Count /= 1
      then
         Exceptions.Raise_Exception_From_Here (Status_Error'Identity);
      end if;
      if not Object.Search.Has_Next then
         return 0; -- No_Element
      else
         Object.Search.Next_Directory_Entry.Search := Object.Search;
         Object.Search.Next_Directory_Entry.Additional.Filled := False;
         return 1;
      end if;
   end First;

   overriding function Next (Object : Search_Iterator; Position : Cursor)
      return Cursor is
   begin
      if Integer (Position) /= Object.Search.Count then
         Exceptions.Raise_Exception_From_Here (Status_Error'Identity);
      end if;
      --  increment and search next
      Next (Object.Search.all);
      if not Object.Search.Has_Next then
         Object.Search.Next_Directory_Entry.Search := null;
         return 0; -- No_Element
      else
         Object.Search.Next_Directory_Entry.Additional.Filled := False;
         return Cursor (Object.Search.Count);
      end if;
   end Next;

   --  operations on directory entries

   function Simple_Name (Directory_Entry : Directory_Entry_Type)
      return String is
   begin
      if Directory_Entry.Search = null then
         Exceptions.Raise_Exception_From_Here (Status_Error'Identity);
      end if;
      return Directory_Searching.Simple_Name (Directory_Entry.Data);
   end Simple_Name;

   function Full_Name (Directory_Entry : Directory_Entry_Type) return String is
      Name : constant String := Simple_Name (Directory_Entry);
   begin
      return Compose (
         Directory_Entry.Search.Path.all,
         Name);
   end Full_Name;

   function Kind (Directory_Entry : Directory_Entry_Type) return File_Kind is
   begin
      if Directory_Entry.Search = null then
         Exceptions.Raise_Exception_From_Here (Status_Error'Identity);
      end if;
      return File_Kind'Enum_Val (Directory_Searching.File_Kind'Enum_Rep (
         Directory_Searching.Kind (Directory_Entry.Data)));
   end Kind;

   function Size (Directory_Entry : Directory_Entry_Type) return File_Size is
   begin
      if Kind (Directory_Entry) /= Ordinary_File then
         raise Constraint_Error; -- implementation-defined
      else
         return Directory_Searching.Size (
            Directory_Entry.Search.Path.all,
            Directory_Entry.Data,
            Directory_Entry.Additional'Unrestricted_Access);
      end if;
   end Size;

   function Modification_Time (Directory_Entry : Directory_Entry_Type)
      return Calendar.Time
   is
      function Cast is new Unchecked_Conversion (Duration, Calendar.Time);
   begin
      if Directory_Entry.Search = null then
         Exceptions.Raise_Exception_From_Here (Status_Error'Identity);
      end if;
      return Cast (System.Native_Time.To_Time (
         Directory_Searching.Modification_Time (
            Directory_Entry.Search.Path.all,
            Directory_Entry.Data,
            Directory_Entry.Additional'Unrestricted_Access)));
   end Modification_Time;

end Ada.Directories;
