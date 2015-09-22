with Ada.Exception_Identification.From_Here;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with System.Form_Parameters;
with System.Native_Calendar;
with System.Storage_Elements;
package body Ada.Directories is
   use Exception_Identification.From_Here;
   use type System.Native_Directories.File_Kind;
   use type System.Native_Directories.Searching.Handle_Type;
   use type System.Storage_Elements.Storage_Offset;

   subtype Directory_Entry_Information_Type is
      System.Native_Directories.Directory_Entry_Information_Type;

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

   procedure Finalize (Object : in out Non_Controlled_Directory_Entry_Type);
   procedure Finalize (Object : in out Non_Controlled_Directory_Entry_Type) is
   begin
      if Object.Status = Detached then
         Free (Object.Path);
         System.Native_Directories.Searching.Free (Object.Directory_Entry);
      end if;
   end Finalize;

   procedure End_Search (
      Search : in out Search_Type;
      Raise_On_Error : Boolean);
   procedure End_Search (
      Search : in out Search_Type;
      Raise_On_Error : Boolean) is
   begin
      Free (Search.Path);
      System.Native_Directories.Searching.End_Search (
         Search.Search,
         Raise_On_Error => Raise_On_Error);
   end End_Search;

   procedure Next (Search : in out Search_Type);
   procedure Next (Search : in out Search_Type) is
      Has_Next : Boolean;
   begin
      --  set the queried flag
      Search.Next_Is_Queried := True;
      --  increment
      Search.Count := Search.Count + 1;
      --  search next
      declare
         NC_Directory_Entry : constant
            not null access Non_Controlled_Directory_Entry_Type :=
            Reference (Search.Next_Directory_Entry);
      begin
         System.Native_Directories.Searching.Get_Next_Entry (
            Search.Search,
            NC_Directory_Entry.Directory_Entry,
            Has_Next);
         if Has_Next then
            NC_Directory_Entry.Additional.Filled := False;
         else
            NC_Directory_Entry.Status := Empty;
         end if;
      end;
   end Next;

   --  directory and file operations

   procedure Create_Directory (New_Directory : String; Form : String := "") is
      pragma Unreferenced (Form);
   begin
      System.Native_Directories.Create_Directory (New_Directory);
   end Create_Directory;

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
                     Delete_File (Name);
                  when Directories.Directory =>
                     Delete_Tree (Name); -- recursive
               end case;
            end;
         end;
      end loop;
      End_Search (Search);
      Delete_Directory (Directory);
   end Delete_Tree;

   procedure Copy_File (
      Source_Name : String;
      Target_Name : String;
      Form : String) is
   begin
      System.Native_Directories.Copy_File (
         Source_Name,
         Target_Name,
         Overwrite => Pack_For_Copy_File (Form));
   end Copy_File;

   --  file and directory queries

   function Kind (Name : String) return File_Kind is
      Information : aliased Directory_Entry_Information_Type;
   begin
      System.Native_Directories.Get_Information (Name, Information);
      return File_Kind'Enum_Val (
         System.Native_Directories.File_Kind'Enum_Rep (
            System.Native_Directories.Kind (Information)));
   end Kind;

   function Size (Name : String) return File_Size is
      Information : aliased Directory_Entry_Information_Type;
   begin
      System.Native_Directories.Get_Information (Name, Information);
      if System.Native_Directories.Kind (Information) /=
         System.Native_Directories.Ordinary_File
      then
         raise Constraint_Error; -- implementation-defined
      else
         return System.Native_Directories.Size (Information);
      end if;
   end Size;

   function Modification_Time (Name : String) return Calendar.Time is
      function Cast is new Unchecked_Conversion (Duration, Calendar.Time);
      Information : aliased Directory_Entry_Information_Type;
   begin
      System.Native_Directories.Get_Information (Name, Information);
      return Cast (
         System.Native_Calendar.To_Time (
            System.Native_Directories.Modification_Time (Information)));
   end Modification_Time;

   procedure Set_Modification_Time (Name : String; Time : Calendar.Time) is
      function Cast is new Unchecked_Conversion (Calendar.Time, Duration);
   begin
      System.Native_Directories.Set_Modification_Time (
         Name,
         System.Native_Calendar.To_Native_Time (Cast (Time)));
   end Set_Modification_Time;

   --  directory searching

   function Is_Assigned (Directory_Entry : Directory_Entry_Type)
      return Boolean
   is
      NC_Directory_Entry : constant
         not null access Non_Controlled_Directory_Entry_Type :=
         Reference (Directory_Entry);
   begin
      return NC_Directory_Entry.Status /= Empty;
   end Is_Assigned;

   function Is_Open (Search : Search_Type) return Boolean is
   begin
      return Search.Search.Handle /=
         System.Native_Directories.Searching.Null_Handle;
   end Is_Open;

   procedure Start_Search (
      Search : in out Search_Type;
      Directory : String;
      Pattern : String := "*";
      Filter : Filter_Type := (others => True))
   is
      pragma Check (Pre,
         Check => not Is_Open (Search) or else raise Status_Error);
      function Cast is new
         Unchecked_Conversion (
            Filter_Type,
            System.Native_Directories.Searching.Filter_Type);
      Has_Next : Boolean;
   begin
      Search.Next_Is_Queried := True;
      Search.Path := new String'(Full_Name (Directory));
      Search.Count := 1;
      declare
         NC_Directory_Entry : constant
            not null access Non_Controlled_Directory_Entry_Type :=
            Reference (Search.Next_Directory_Entry);
      begin
         System.Native_Directories.Searching.Start_Search (
            Search.Search,
            Directory,
            Pattern,
            Cast (Filter),
            NC_Directory_Entry.Directory_Entry,
            Has_Next);
         if Has_Next then
            NC_Directory_Entry.Path := Search.Path;
            NC_Directory_Entry.Additional.Filled := False;
            NC_Directory_Entry.Status := Attached;
         else
            NC_Directory_Entry.Status := Empty;
         end if;
      end;
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

   procedure End_Search (Search : in out Search_Type) is
      pragma Check (Pre,
         Check => Is_Open (Search) or else raise Status_Error);
   begin
      End_Search (Search, Raise_On_Error => True);
   end End_Search;

   function More_Entries (
      Search : Search_Type)
      return Boolean
   is
      pragma Check (Dynamic_Predicate,
         Check => Is_Open (Search) or else raise Status_Error);
   begin
      if not Search.Next_Is_Queried then
         Next (Search'Unrestricted_Access.all);
      end if;
      return Reference (Search.Next_Directory_Entry).Status /= Empty;
   end More_Entries;

   procedure Get_Next_Entry (
      Search : in out Search_Type;
      Directory_Entry : out Directory_Entry_Type)
   is
      pragma Unmodified (Directory_Entry); -- modified via Reference
   begin
      if not More_Entries (Search) then -- checking the predicate
         Raise_Exception (Use_Error'Identity); -- RM A.16 (110/3)
      else
         --  current buffer
         declare
            Source_NC_Directory_Entry : constant
               not null access Non_Controlled_Directory_Entry_Type :=
               Reference (Search.Next_Directory_Entry);
            Target_NC_Directory_Entry : constant
               not null access Non_Controlled_Directory_Entry_Type :=
               Reference (Directory_Entry);
         begin
            Finalize (Target_NC_Directory_Entry.all);
            Target_NC_Directory_Entry.Path := Search.Path;
            Target_NC_Directory_Entry.Directory_Entry :=
               Source_NC_Directory_Entry.Directory_Entry;
            Target_NC_Directory_Entry.Additional.Filled := False;
            Target_NC_Directory_Entry.Status := Attached;
         end;
         --  reset the queried flag
         Search.Next_Is_Queried := False;
      end if;
   end Get_Next_Entry;

   function Get_Next_Entry (
      Search : aliased in out Search_Type)
      return Directory_Entry_Type is
   begin
      return Result : Directory_Entry_Type do
         Get_Next_Entry (Search, Result);
      end return;
   end Get_Next_Entry;

   procedure Get_Entry (
      Name : String;
      Directory_Entry : out Directory_Entry_Type)
   is
      pragma Unmodified (Directory_Entry); -- modified via Reference
      NC_Directory_Entry : constant
         not null access Non_Controlled_Directory_Entry_Type :=
         Reference (Directory_Entry);
      Directory_First : Positive;
      Directory_Last : Natural;
      Simple_Name_First : Positive;
      Simple_Name_Last : Natural;
   begin
      --  decompose the name
      Containing_Directory (Name, Directory_First, Directory_Last);
      Simple_Name (Name, Simple_Name_First, Simple_Name_Last);
      --  make a detached entry
      Finalize (NC_Directory_Entry.all);
      NC_Directory_Entry.Path :=
         new String'(Full_Name (Name (Directory_First .. Directory_Last)));
      NC_Directory_Entry.Directory_Entry := null;
      NC_Directory_Entry.Additional.Filled := False;
      NC_Directory_Entry.Status := Detached;
      System.Native_Directories.Searching.Get_Entry (
         NC_Directory_Entry.Path.all,
         Name (Simple_Name_First .. Simple_Name_Last),
         NC_Directory_Entry.Directory_Entry,
         NC_Directory_Entry.Additional);
   end Get_Entry;

   function Get_Entry (
      Name : String)
      return Directory_Entry_Type is
   begin
      return Result : Directory_Entry_Type do
         Get_Entry (Name, Result);
      end return;
   end Get_Entry;

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

   overriding procedure Finalize (Search : in out Search_Type) is
   begin
      if Search.Search.Handle /=
         System.Native_Directories.Searching.Null_Handle
      then
         End_Search (Search, Raise_On_Error => False);
      end if;
   end Finalize;

   --  iterator

   function Has_Element (Position : Cursor) return Boolean is
   begin
      return Position > 0;
   end Has_Element;

   function Element (
      Container : Search_Type'Class;
      Position : Cursor)
      return Directory_Entry_Type is
   begin
      return Result : Directory_Entry_Type do
         pragma Unmodified (Result); -- modified via Reference
         declare
            Source_Reference : constant Constant_Reference_Type :=
               Constant_Reference (Container, Position);
            --  checking the predicate and Position in Constant_Reference
            Target_NC_Directory_Entry : constant
               not null access Non_Controlled_Directory_Entry_Type :=
               Reference (Result);
            Source_NC_Directory_Entry : constant
               not null access Non_Controlled_Directory_Entry_Type :=
               Reference (Source_Reference.Element.all);
         begin
            Target_NC_Directory_Entry.Path :=
               new String'(Source_NC_Directory_Entry.Path.all);
            Target_NC_Directory_Entry.Additional.Filled := False;
            Target_NC_Directory_Entry.Status := Detached;
            Target_NC_Directory_Entry.Directory_Entry :=
               System.Native_Directories.Searching.New_Directory_Entry (
                  Source_NC_Directory_Entry.Directory_Entry);
         end;
      end return;
   end Element;

   function Constant_Reference (
      Container : aliased Search_Type;
      Position : Cursor)
      return Constant_Reference_Type
   is
      pragma Check (Dynamic_Predicate,
         Check => Is_Open (Container) or else raise Status_Error);
      pragma Check (Pre,
         Check => Integer (Position) = Container.Count
            or else raise Status_Error);
   begin
      return (Element => Container.Next_Directory_Entry'Access);
   end Constant_Reference;

   function Iterate (
      Container : Search_Type'Class)
      return Search_Iterator_Interfaces.Forward_Iterator'Class
   is
      pragma Check (Dynamic_Predicate,
         Check => Is_Open (Container) or else raise Status_Error);
   begin
      return Search_Iterator'(Search => Container'Unrestricted_Access);
   end Iterate;

   overriding function First (Object : Search_Iterator) return Cursor is
      pragma Check (Pre,
         Check =>
            (Is_Open (Object.Search.all) and then Object.Search.Count = 1)
            or else raise Status_Error);
   begin
      if Reference (Object.Search.Next_Directory_Entry).Status = Empty then
         return 0; -- No_Element
      else
         return 1;
      end if;
   end First;

   overriding function Next (Object : Search_Iterator; Position : Cursor)
      return Cursor
   is
      pragma Check (Pre,
         Check => Integer (Position) = Object.Search.Count
            or else raise Status_Error);
   begin
      --  increment and search next
      Next (Object.Search.all);
      if Reference (Object.Search.Next_Directory_Entry).Status = Empty then
         return 0; -- No_Element
      else
         return Cursor (Object.Search.Count);
      end if;
   end Next;

   --  operations on directory entries

   function Simple_Name (
      Directory_Entry : Directory_Entry_Type)
      return String
   is
      pragma Check (Dynamic_Predicate,
         Check => Is_Assigned (Directory_Entry) or else raise Status_Error);
      NC_Directory_Entry : constant
         not null access Non_Controlled_Directory_Entry_Type :=
         Reference (Directory_Entry);
   begin
      return System.Native_Directories.Searching.Simple_Name (
         NC_Directory_Entry.Directory_Entry);
   end Simple_Name;

   function Full_Name (
      Directory_Entry : Directory_Entry_Type)
      return String
   is
      Name : constant String :=
         Simple_Name (Directory_Entry); -- checking the predicate
      NC_Directory_Entry : constant
         not null access Non_Controlled_Directory_Entry_Type :=
         Reference (Directory_Entry);
   begin
      return Compose (
         NC_Directory_Entry.Path.all,
         Name);
   end Full_Name;

   function Kind (
      Directory_Entry : Directory_Entry_Type)
      return File_Kind
   is
      pragma Check (Dynamic_Predicate,
         Check => Is_Assigned (Directory_Entry) or else raise Status_Error);
      NC_Directory_Entry : constant
         not null access Non_Controlled_Directory_Entry_Type :=
         Reference (Directory_Entry);
   begin
      return File_Kind'Enum_Val (
         System.Native_Directories.File_Kind'Enum_Rep (
            System.Native_Directories.Searching.Kind (
               NC_Directory_Entry.Directory_Entry)));
   end Kind;

   function Size (
      Directory_Entry : Directory_Entry_Type)
      return File_Size is
   begin
      if Kind (Directory_Entry) /= Ordinary_File then -- checking the predicate
         raise Constraint_Error; -- implementation-defined
      else
         declare
            NC_Directory_Entry : constant
               not null access Non_Controlled_Directory_Entry_Type :=
               Reference (Directory_Entry);
         begin
            return System.Native_Directories.Searching.Size (
               NC_Directory_Entry.Path.all,
               NC_Directory_Entry.Directory_Entry,
               NC_Directory_Entry.Additional);
         end;
      end if;
   end Size;

   function Modification_Time (
      Directory_Entry : Directory_Entry_Type)
      return Calendar.Time
   is
      pragma Check (Dynamic_Predicate,
         Check => Is_Assigned (Directory_Entry) or else raise Status_Error);
      function Cast is new Unchecked_Conversion (Duration, Calendar.Time);
      NC_Directory_Entry : constant
         not null access Non_Controlled_Directory_Entry_Type :=
         Reference (Directory_Entry);
   begin
      return Cast (
         System.Native_Calendar.To_Time (
            System.Native_Directories.Searching.Modification_Time (
               NC_Directory_Entry.Path.all,
               NC_Directory_Entry.Directory_Entry,
               NC_Directory_Entry.Additional)));
   end Modification_Time;

   package body Controlled is

      function Reference (Object : Directory_Entry_Type)
         return not null access Non_Controlled_Directory_Entry_Type is
      begin
         return Object.Data'Unrestricted_Access;
      end Reference;

      overriding procedure Finalize (Object : in out Directory_Entry_Type) is
      begin
         Finalize (Object.Data);
      end Finalize;

   end Controlled;

end Ada.Directories;
