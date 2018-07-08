with Ada.Calendar.Naked;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with System.Form_Parameters;
with System.Storage_Elements;
package body Ada.Directories is
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
         declare
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
         end;
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

   procedure Assign (
      Target : out Non_Controlled_Directory_Entry_Type;
      Source : Non_Controlled_Directory_Entry_Type);
   procedure Assign (
      Target : out Non_Controlled_Directory_Entry_Type;
      Source : Non_Controlled_Directory_Entry_Type) is
   begin
      Target.Additional.Filled := False;
      Target.Status := Detached;
      Target.Path := new String'(Source.Path.all);
      Target.Directory_Entry :=
         System.Native_Directories.Searching.New_Directory_Entry (
            Source.Directory_Entry);
   end Assign;

   procedure End_Search (
      Search : aliased in out Non_Controlled_Search_Type;
      Raise_On_Error : Boolean);
   procedure End_Search (
      Search : aliased in out Non_Controlled_Search_Type;
      Raise_On_Error : Boolean) is
   begin
      Free (Search.Path);
      System.Native_Directories.Searching.End_Search (
         Search.Search,
         Raise_On_Error => Raise_On_Error);
   end End_Search;

   procedure Get_Next_Entry (
      Search : aliased in out Non_Controlled_Search_Type;
      Directory_Entry : in out Non_Controlled_Directory_Entry_Type);
   procedure Get_Next_Entry (
      Search : aliased in out Non_Controlled_Search_Type;
      Directory_Entry : in out Non_Controlled_Directory_Entry_Type)
   is
      Has_Next : Boolean;
   begin
      System.Native_Directories.Searching.Get_Next_Entry (
         Search.Search,
         Directory_Entry.Directory_Entry,
         Has_Next);
      if Has_Next then
         Directory_Entry.Additional.Filled := False;
      else
         Directory_Entry.Status := Empty;
      end if;
   end Get_Next_Entry;

   function Current (Object : Directory_Iterator) return Cursor;
   function Current (Object : Directory_Iterator) return Cursor is
      Listing : constant not null Directory_Listing_Access := Object.Listing;
   begin
      if not Is_Assigned (
         Controlled_Searches.Next_Directory_Entry (Listing.Search).all)
      then
         --  call End_Search at this time, for propagating the exceptions.
         declare
            NC_Search : Non_Controlled_Search_Type
               renames Controlled_Searches.Reference (Listing.Search).all;
         begin
            End_Search (NC_Search, Raise_On_Error => True);
         end;
         return 0; -- No_Element
      else
         return Cursor (Listing.Count);
      end if;
   end Current;

   --  directory and file operations

   procedure Create_Directory (
      New_Directory : String;
      Form : String)
   is
      pragma Unreferenced (Form);
   begin
      System.Native_Directories.Create_Directory (New_Directory);
   end Create_Directory;

   procedure Create_Directory (
      New_Directory : String) is
   begin
      System.Native_Directories.Create_Directory (New_Directory);
   end Create_Directory;

   procedure Create_Path (
      New_Directory : String;
      Form : String)
   is
      pragma Unreferenced (Form);
   begin
      Create_Path (New_Directory);
   end Create_Path;

   procedure Create_Path (
      New_Directory : String)
   is
      I : Positive := New_Directory'Last + 1;
   begin
      while I > New_Directory'First loop
         declare
            P : Positive := I - 1;
         begin
            Hierarchical_File_Names.Exclude_Trailing_Path_Delimiter (
               New_Directory,
               Last => P);
            exit when Exists (New_Directory (New_Directory'First .. P));
            declare
               S_First : Positive;
               S_Last : Natural;
            begin
               Hierarchical_File_Names.Simple_Name (
                  New_Directory (New_Directory'First .. P),
                  First => S_First,
                  Last => S_Last);
               I := S_First;
            end;
         end;
      end loop;
      while I <= New_Directory'Last loop
         declare
            R_First : Positive;
            R_Last : Natural;
         begin
            Hierarchical_File_Names.Relative_Name (
               New_Directory (I .. New_Directory'Last),
               First => R_First,
               Last => R_Last);
            declare
               P : Natural := R_First - 1;
            begin
               Hierarchical_File_Names.Exclude_Trailing_Path_Delimiter (
                  New_Directory,
                  Last => P);
               Create_Directory (New_Directory (New_Directory'First .. P));
            end;
            I := R_First;
         end;
      end loop;
   end Create_Path;

   procedure Delete_Tree (Directory : String) is
      Search : aliased Search_Type;
   begin
      Start_Search (Search, Directory, "*", (others => True));
      while More_Entries (Search) loop
         declare
            Directory_Entry : Directory_Entry_Type
               renames Look_Next_Entry (Search).Element.all;
            Name : constant String := Full_Name (Directory_Entry);
         begin
            case Kind (Directory_Entry) is
               when Ordinary_File | Special_File =>
                  Delete_File (Name);
               when Directories.Directory =>
                  Delete_Tree (Name); -- recursive
            end case;
         end;
         Skip_Next_Entry (Search);
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

   function Compose (
      Containing_Directory : String := "";
      Name : String;
      Extension : String := "";
      Path_Delimiter : Hierarchical_File_Names.Path_Delimiter_Type :=
         Hierarchical_File_Names.Default_Path_Delimiter)
      return String
   is
      pragma Check (Pre,
         Check =>
            Containing_Directory'Length = 0
            or else Hierarchical_File_Names.Is_Simple_Name (Name)
            or else raise Name_Error); -- RM A.16(82/3)
   begin
      return Hierarchical_File_Names.Compose (
         Containing_Directory,
         Name,
         Extension,
         Path_Delimiter => Path_Delimiter);
   end Compose;

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
      Information : aliased Directory_Entry_Information_Type;
   begin
      System.Native_Directories.Get_Information (Name, Information);
      return Calendar.Naked.To_Time (
         System.Native_Directories.Modification_Time (Information));
   end Modification_Time;

   procedure Set_Modification_Time (Name : String; Time : Calendar.Time) is
   begin
      System.Native_Directories.Set_Modification_Time (
         Name,
         Calendar.Naked.To_Native_Time (Time));
   end Set_Modification_Time;

   --  directory searching

   function Is_Assigned (Directory_Entry : Directory_Entry_Type)
      return Boolean
   is
      NC_Directory_Entry : Non_Controlled_Directory_Entry_Type
         renames Controlled_Entries.Reference (Directory_Entry).all;
   begin
      return NC_Directory_Entry.Status /= Empty;
   end Is_Assigned;

   package body Controlled_Entries is

      function Reference (Object : Directories.Directory_Entry_Type)
         return not null access Non_Controlled_Directory_Entry_Type is
      begin
         return Directory_Entry_Type (Object).Data'Unrestricted_Access;
      end Reference;

      overriding procedure Finalize (Object : in out Directory_Entry_Type) is
      begin
         Finalize (Object.Data);
      end Finalize;

   end Controlled_Entries;

   function Is_Open (Search : Search_Type) return Boolean is
      NC_Search : Non_Controlled_Search_Type
         renames Controlled_Searches.Reference (Search).all;
   begin
      return NC_Search.Search.Handle /=
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
      function Cast is
         new Unchecked_Conversion (
            Filter_Type,
            System.Native_Directories.Searching.Filter_Type);
      NC_Search : Non_Controlled_Search_Type
         renames Controlled_Searches.Reference (Search).all;
      Has_Next : Boolean;
   begin
      NC_Search.Path := new String'(Full_Name (Directory));
      declare
         NC_Next_Directory_Entry : Non_Controlled_Directory_Entry_Type
            renames Controlled_Entries.Reference (
               Controlled_Searches.Next_Directory_Entry (Search).all).all;
      begin
         System.Native_Directories.Searching.Start_Search (
            NC_Search.Search,
            Directory,
            Pattern,
            Cast (Filter),
            NC_Next_Directory_Entry.Directory_Entry,
            Has_Next);
         if Has_Next then
            NC_Next_Directory_Entry.Path := NC_Search.Path;
            NC_Next_Directory_Entry.Additional.Filled := False;
            NC_Next_Directory_Entry.Status := Attached;
         else
            NC_Next_Directory_Entry.Status := Empty;
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
      pragma Check (Pre, Is_Open (Search) or else raise Status_Error);
      NC_Search : Non_Controlled_Search_Type
         renames Controlled_Searches.Reference (Search).all;
   begin
      End_Search (NC_Search, Raise_On_Error => True);
   end End_Search;

   function More_Entries (
      Search : Search_Type)
      return Boolean
   is
      pragma Check (Dynamic_Predicate,
         Check => Is_Open (Search) or else raise Status_Error);
   begin
      return Is_Assigned (
         Controlled_Searches.Next_Directory_Entry (Search).all);
   end More_Entries;

   procedure Get_Next_Entry (
      Search : in out Search_Type;
      Directory_Entry : out Directory_Entry_Type)
   is
      pragma Unmodified (Directory_Entry); -- modified via Reference
      pragma Check (Pre,
         Check =>
            More_Entries (Search) -- checking the predicate
            or else raise Use_Error); -- RM A.16(110/3)
      NC_Search : Non_Controlled_Search_Type
         renames Controlled_Searches.Reference (Search).all;
      NC_Next_Directory_Entry : Non_Controlled_Directory_Entry_Type
         renames Controlled_Entries.Reference (
            Controlled_Searches.Next_Directory_Entry (Search).all).all;
      NC_Directory_Entry : Non_Controlled_Directory_Entry_Type
         renames Controlled_Entries.Reference (Directory_Entry).all;
   begin
      Finalize (NC_Directory_Entry);
      --  copy to the detached entry
      Assign (Target => NC_Directory_Entry, Source => NC_Next_Directory_Entry);
      --  search next
      Get_Next_Entry (NC_Search, NC_Next_Directory_Entry);
   end Get_Next_Entry;

   function Get_Next_Entry (
      Search : aliased in out Search_Type)
      return Directory_Entry_Type is
   begin
      return Result : Directory_Entry_Type do
         Get_Next_Entry (Search, Result);
      end return;
   end Get_Next_Entry;

   function Look_Next_Entry (
      Search : aliased Search_Type)
      return Constant_Reference_Type
   is
      pragma Check (Dynamic_Predicate,
         Check => Is_Open (Search) or else raise Status_Error);
   begin
      return (Element => Controlled_Searches.Next_Directory_Entry (Search));
   end Look_Next_Entry;

   procedure Skip_Next_Entry (
      Search : in out Search_Type)
   is
      pragma Check (Dynamic_Predicate,
         Check => Is_Open (Search) or else raise Status_Error);
      NC_Search : Non_Controlled_Search_Type
         renames Controlled_Searches.Reference (Search).all;
      NC_Next_Directory_Entry : Non_Controlled_Directory_Entry_Type
         renames Controlled_Entries.Reference (
            Controlled_Searches.Next_Directory_Entry (Search).all).all;
   begin
      Get_Next_Entry (NC_Search, NC_Next_Directory_Entry);
   end Skip_Next_Entry;

   procedure Search (
      Directory : String;
      Pattern : String := "*";
      Filter : Filter_Type := (others => True);
      Process : not null access procedure (
         Directory_Entry : Directory_Entry_Type))
   is
      Search : aliased Search_Type;
   begin
      Start_Search (Search, Directory, Pattern, Filter);
      while More_Entries (Search) loop
         Process (Look_Next_Entry (Search).Element.all);
         Skip_Next_Entry (Search);
      end loop;
      End_Search (Search);
   end Search;

   package body Controlled_Searches is

      function Reference (Object : Directories.Search_Type)
         return not null access Non_Controlled_Search_Type is
      begin
         return Search_Type (Object).Data'Unrestricted_Access;
      end Reference;

      function Next_Directory_Entry (Object : Directories.Search_Type)
         return not null access Directory_Entry_Type is
      begin
         return Search_Type (Object).Next_Directory_Entry'Unrestricted_Access;
      end Next_Directory_Entry;

      overriding procedure Finalize (Search : in out Search_Type) is
      begin
         if Search.Data.Search.Handle /=
            System.Native_Directories.Searching.Null_Handle
         then
            End_Search (Search.Data, Raise_On_Error => False);
         end if;
      end Finalize;

   end Controlled_Searches;

   --  directory iteration

   function Is_Open (Listing : Directory_Listing) return Boolean is
   begin
      return Is_Open (Listing.Search);
   end Is_Open;

   function Entries (
      Directory : String;
      Pattern : String := "*";
      Filter : Filter_Type := (others => True))
      return Directory_Listing is
   begin
      return Result : Directory_Listing do
         Result.Count := 1;
         Start_Search (Result.Search, Directory, Pattern, Filter);
      end return;
   end Entries;

   function Has_Entry (Position : Cursor) return Boolean is
   begin
      return Position > 0;
   end Has_Entry;

   function Iterate (
      Listing : Directory_Listing'Class)
      return Directory_Iterators.Forward_Iterator'Class
   is
      pragma Check (Dynamic_Predicate,
         Check =>
            Is_Open (Directory_Listing (Listing)) or else raise Status_Error);
   begin
      return Directory_Iterator'(Listing => Listing'Unrestricted_Access);
   end Iterate;

   function Current_Entry (
      Entries : Directory_Listing'Class;
      Position : Cursor)
      return Directory_Entry_Type is
   begin
      return Result : Directory_Entry_Type do
         pragma Unmodified (Result); -- modified via Reference
         declare
            Source_Reference : constant Constant_Reference_Type :=
               Constant_Reference (Directory_Listing (Entries), Position);
            --  checking the predicate and Position in Constant_Reference
            NC_Next_Directory_Entry : Non_Controlled_Directory_Entry_Type
               renames Controlled_Entries.Reference (
                     Source_Reference.Element.all)
                  .all;
            NC_Result : Non_Controlled_Directory_Entry_Type
               renames Controlled_Entries.Reference (Result).all;
         begin
            Assign (Target => NC_Result, Source => NC_Next_Directory_Entry);
         end;
      end return;
   end Current_Entry;

   function Constant_Reference (
      Container : aliased Directory_Listing;
      Position : Cursor)
      return Constant_Reference_Type
   is
      pragma Check (Dynamic_Predicate,
         Check => Is_Open (Container) or else raise Status_Error);
      pragma Check (Pre,
         Check =>
            Integer (Position) = Container.Count or else raise Status_Error);
   begin
      return Look_Next_Entry (Container.Search);
   end Constant_Reference;

   overriding function First (Object : Directory_Iterator) return Cursor is
      pragma Check (Pre, Object.Listing.Count = 1 or else raise Status_Error);
   begin
      return Current (Object);
   end First;

   overriding function Next (Object : Directory_Iterator; Position : Cursor)
      return Cursor
   is
      pragma Check (Pre,
         Check =>
            Integer (Position) = Object.Listing.Count
            or else raise Status_Error);
      Listing : constant not null Directory_Listing_Access := Object.Listing;
   begin
      --  increment
      Listing.Count := Listing.Count + 1;
      --  search next
      Skip_Next_Entry (Listing.Search);
      return Current (Object);
   end Next;

   --  operations on directory entries

   procedure Get_Entry (
      Name : String;
      Directory_Entry : out Directory_Entry_Type)
   is
      pragma Unmodified (Directory_Entry); -- modified via Reference
      NC_Directory_Entry : Non_Controlled_Directory_Entry_Type
         renames Controlled_Entries.Reference (Directory_Entry).all;
      Directory_First : Positive;
      Directory_Last : Natural;
      Simple_Name_First : Positive;
      Simple_Name_Last : Natural;
   begin
      --  decompose the name
      Hierarchical_File_Names.Containing_Directory (Name,
         First => Directory_First, Last => Directory_Last);
      Hierarchical_File_Names.Simple_Name (Name,
         First => Simple_Name_First, Last => Simple_Name_Last);
      --  make a detached entry
      Finalize (NC_Directory_Entry);
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

   function Simple_Name (
      Directory_Entry : Directory_Entry_Type)
      return String
   is
      pragma Check (Dynamic_Predicate,
         Check => Is_Assigned (Directory_Entry) or else raise Status_Error);
      NC_Directory_Entry : Non_Controlled_Directory_Entry_Type
         renames Controlled_Entries.Reference (Directory_Entry).all;
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
      NC_Directory_Entry : Non_Controlled_Directory_Entry_Type
         renames Controlled_Entries.Reference (Directory_Entry).all;
   begin
      return Hierarchical_File_Names.Compose (
         NC_Directory_Entry.Path.all,
         Name);
   end Full_Name;

   function Kind (
      Directory_Entry : Directory_Entry_Type)
      return File_Kind
   is
      pragma Check (Dynamic_Predicate,
         Check => Is_Assigned (Directory_Entry) or else raise Status_Error);
      NC_Directory_Entry : Non_Controlled_Directory_Entry_Type
         renames Controlled_Entries.Reference (Directory_Entry).all;
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
            NC_Directory_Entry : Non_Controlled_Directory_Entry_Type
               renames Controlled_Entries.Reference (Directory_Entry).all;
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
      NC_Directory_Entry : Non_Controlled_Directory_Entry_Type
         renames Controlled_Entries.Reference (Directory_Entry).all;
   begin
      return Calendar.Naked.To_Time (
         System.Native_Directories.Searching.Modification_Time (
            NC_Directory_Entry.Path.all,
            NC_Directory_Entry.Directory_Entry,
            NC_Directory_Entry.Additional));
   end Modification_Time;

end Ada.Directories;
