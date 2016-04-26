with Ada.Exception_Identification.From_Here;
package body Ada.Hierarchical_File_Names is
   use Exception_Identification.From_Here;

   function Parent_Directory_Name (
      Level : Positive;
      Path_Delimiter : Character)
      return String;
   function Parent_Directory_Name (
      Level : Positive;
      Path_Delimiter : Character)
      return String is
   begin
      return Result : String (1 .. 3 * Level - 1) do
         Result (1) := '.';
         Result (2) := '.';
         for I in 2 .. Level loop
            Result (I * 3 - 3) := Path_Delimiter;
            Result (I * 3 - 2) := '.';
            Result (I * 3 - 1) := '.';
         end loop;
      end return;
   end Parent_Directory_Name;

   function Current_Directory_Name return String;
   function Current_Directory_Name return String is
   begin
      return ".";
   end Current_Directory_Name;

   procedure Containing_Root_Directory (Name : String; Last : out Natural);
   procedure Containing_Root_Directory (Name : String; Last : out Natural) is
   begin
      if Name'First > Name'Last then
         Last := Name'First - 1;
      elsif Is_Path_Delimiter (Name (Name'First)) then
         if Name'First < Name'Last
            and then Is_Path_Delimiter (Name (Name'First + 1))
         then -- UNC \\HOST\SHARE\
            Last := Name'First + 1;
            loop -- skip host-name
               if Last = Name'Last then
                  return;
               end if;
               Last := Last + 1;
               exit when Is_Path_Delimiter (Name (Last));
            end loop;
            loop -- skip share-name
               if Last = Name'Last then
                  return;
               end if;
               Last := Last + 1;
               exit when Is_Path_Delimiter (Name (Last));
            end loop;
         else
            Last := Name'First; -- no drive letter
         end if;
      elsif Name'First < Name'Last
         and then (
            Name (Name'First) in 'A' .. 'Z'
            or else Name (Name'First) in 'a' .. 'z')
         and then Name (Name'First + 1) = ':'
      then
         if Name'First + 2 <= Name'Last
            and then Is_Path_Delimiter (Name (Name'First + 2))
         then
            Last := Name'First + 2; -- "C:\"
         else
            Last := Name'First + 1; -- "C:"
         end if;
      else
         Last := Name'First - 1; -- relative
      end if;
   end Containing_Root_Directory;

   procedure Raw_Simple_Name (Name : String; First : out Positive);
   procedure Raw_Simple_Name (Name : String; First : out Positive) is
   begin
      First := Name'First;
      for I in reverse Name'Range loop
         if Is_Path_Delimiter (Name (I)) then
            First := I + 1;
            exit; -- found
         end if;
      end loop;
   end Raw_Simple_Name;

   procedure Exclude_Trailing_Directories (
      Directory : String;
      Last : in out Natural;
      Level : in out Natural);
   procedure Exclude_Trailing_Directories (
      Directory : String;
      Last : in out Natural;
      Level : in out Natural)
   is
      Root_Last : Natural;
   begin
      Exclude_Trailing_Path_Delimiter (Directory, Last);
      Containing_Root_Directory (
         Directory (Directory'First .. Last),
         Last => Root_Last); -- First - 1 if not Is_Full_Name (...)
      while Last > Root_Last loop
         declare
            S_First : Positive;
         begin
            Raw_Simple_Name (
               Directory (Root_Last + 1 .. Last),
               First => S_First);
            if Is_Current_Directory_Name (Directory (S_First .. Last)) then
               null; -- skip "./"
            elsif Is_Parent_Directory_Name (Directory (S_First .. Last)) then
               Level := Level + 1;
            elsif Level = 0 then
               exit;
            else
               Level := Level - 1;
            end if;
            --  Containing_Directory (Directory (First .. Last), ...)
            Last := S_First - 1;
            Exclude_Trailing_Path_Delimiter (Directory, Last);
         end;
      end loop;
   end Exclude_Trailing_Directories;

   --  path delimiter

   function Is_Path_Delimiter (Item : Character) return Boolean is
   begin
      return Item = '\' or else Item = '/';
   end Is_Path_Delimiter;

   procedure Include_Trailing_Path_Delimiter (
      S : in out String;
      Last : in out Natural;
      Path_Delimiter : Character := Default_Path_Delimiter) is
   begin
      if not Is_Path_Delimiter (S (Last)) then
         Last := Last + 1;
         S (Last) := Path_Delimiter;
      end if;
   end Include_Trailing_Path_Delimiter;

   procedure Exclude_Trailing_Path_Delimiter (
      S : String;
      Last : in out Natural) is
   begin
      while Last > S'First -- no removing root path delimiter
         and then Is_Path_Delimiter (S (Last))
      loop
         Last := Last - 1;
      end loop;
   end Exclude_Trailing_Path_Delimiter;

   --  operations in Ada.Directories

   function Simple_Name (Name : String) return String is
      First : Positive;
      Last : Natural;
   begin
      Simple_Name (Name, First => First, Last => Last);
      return Name (First .. Last);
   end Simple_Name;

   function Containing_Directory (
      Name : String;
      Raise_On_Error : Boolean := True)
      return String
   is
      First : Positive;
      Last : Natural;
   begin
      Containing_Directory (Name, First => First, Last => Last);
      if First > Last and then Raise_On_Error then
         Raise_Exception (Use_Error'Identity); -- RM A.16.1 (38/3)
      end if;
      return Name (First .. Last);
   end Containing_Directory;

   function Extension (Name : String) return String is
      First : Positive;
      Last : Natural;
   begin
      Extension (Name, First => First, Last => Last);
      return Name (First .. Last);
   end Extension;

   function Base_Name (Name : String) return String is
      First : Positive;
      Last : Natural;
   begin
      Base_Name (Name, First => First, Last => Last);
      return Name (First .. Last);
   end Base_Name;

   procedure Simple_Name (
      Name : String;
      First : out Positive;
      Last : out Natural)
   is
      Root_Last : Natural;
   begin
      Containing_Root_Directory (Name, Last => Root_Last);
      Raw_Simple_Name (Name (Root_Last + 1 .. Name'Last), First => First);
      Last := Name'Last;
   end Simple_Name;

   procedure Containing_Directory (
      Name : String;
      First : out Positive;
      Last : out Natural)
   is
      Root_Last : Natural;
   begin
      Containing_Root_Directory (Name, Last => Root_Last);
      First := Name'First;
      Last := Root_Last;
      for I in reverse Last + 1 .. Name'Last loop
         if Is_Path_Delimiter (Name (I)) then
            if I > First then
               Last := I - 1;
            else
               Last := I; -- no removing root path delimiter
            end if;
            exit; -- found
         end if;
      end loop;
   end Containing_Directory;

   procedure Extension (
      Name : String;
      First : out Positive;
      Last : out Natural)
   is
      Root_Last : Natural;
   begin
      Containing_Root_Directory (Name, Last => Root_Last);
      First := Name'Last + 1;
      Last := Name'Last;
      for I in reverse
         Root_Last + 2 .. -- >= Name'First + 1
         Last
      loop
         if Is_Path_Delimiter (Name (I)) then
            exit; -- not found
         elsif Name (I) = '.' then
            --  Extension (".DOTFILE") = ""
            if not Is_Path_Delimiter (Name (I - 1)) then
               First := I + 1;
            end if;
            exit; -- found
         end if;
      end loop;
   end Extension;

   procedure Base_Name (
      Name : String;
      First : out Positive;
      Last : out Natural) is
   begin
      Simple_Name (Name, First => First, Last => Last);
      if First > Last or else Name (Last) /= '.' then -- AA-A-16 79.a/2
         for I in reverse First .. Last - 1 loop
            if Name (I) = '.' then
               --  Base_Name (".DOTFILE") = ".DOTFILE"
               if I > First then
                  Last := I - 1;
               end if;
               exit;
            end if;
         end loop;
      end if;
   end Base_Name;

   function Unfolded_Compose (
      Containing_Directory : String := "";
      Name : String;
      Extension : String := "";
      Path_Delimiter : Path_Delimiter_Type := Default_Path_Delimiter)
      return String
   is
      --  this is Directories.Compose
      --  if you want to fold '.' or '..', use Hierarchical_File_Names.Compose
      Containing_Directory_Length : constant Natural :=
         Containing_Directory'Length;
      Name_Length : constant Natural := Name'Length;
      Extension_Length : constant Natural := Extension'Length;
      Result : String (
         1 ..
         Containing_Directory_Length + Name_Length + Extension_Length + 2);
      Last : Natural;
   begin
      --  append directory
      Last := Containing_Directory_Length;
      if Last > 0 then
         Result (1 .. Last) := Containing_Directory;
         Include_Trailing_Path_Delimiter (
            Result,
            Last => Last,
            Path_Delimiter => Path_Delimiter);
      end if;
      --  append name
      Result (Last + 1 .. Last + Name_Length) := Name;
      Last := Last + Name_Length;
      --  append extension
      if Extension_Length /= 0 then
         Last := Last + 1;
         Result (Last) := '.';
         Result (Last + 1 .. Last + Extension_Length) := Extension;
         Last := Last + Extension_Length;
      end if;
      return Result (1 .. Last);
   end Unfolded_Compose;

   --  operations in Ada.Directories.Hierarchical_File_Names

   function Is_Simple_Name (Name : String) return Boolean is
   begin
      for I in Name'Range loop
         if Is_Path_Delimiter (Name (I)) then
            return False;
         end if;
      end loop;
      return True;
   end Is_Simple_Name;

   function Is_Root_Directory_Name (Name : String) return Boolean is
      Last : Natural;
   begin
      Containing_Root_Directory (Name, Last => Last);
      return Name'First <= Last and then Last = Name'Last;
   end Is_Root_Directory_Name;

   function Is_Parent_Directory_Name (Name : String) return Boolean is
   begin
      return Name = "..";
   end Is_Parent_Directory_Name;

   function Is_Current_Directory_Name (Name : String) return Boolean is
   begin
      return Name = ".";
   end Is_Current_Directory_Name;

   function Is_Full_Name (Name : String) return Boolean is
      Last : Natural;
   begin
      Containing_Root_Directory (Name, Last => Last);
      return Name'First <= Last;
   end Is_Full_Name;

   function Is_Relative_Name (Name : String) return Boolean is
   begin
      return not Is_Full_Name (Name);
   end Is_Relative_Name;

   function Initial_Directory (Name : String) return String is
      First : Positive;
      Last : Natural;
   begin
      Initial_Directory (Name, First => First, Last => Last);
      return Name (First .. Last);
   end Initial_Directory;

   function Relative_Name (Name : String) return String is
      First : Positive;
      Last : Natural;
   begin
      Relative_Name (Name, First => First, Last => Last);
      return Name (First .. Last);
   end Relative_Name;

   procedure Initial_Directory (
      Name : String;
      First : out Positive;
      Last : out Natural) is
   begin
      Containing_Root_Directory (Name, Last => Last);
      First := Name'First;
      if First > Last then -- relative
         Last := Name'Last;
         for I in Name'Range loop
            if Is_Path_Delimiter (Name (I)) then
               Last := I - 1;
               exit; -- found
            end if;
         end loop;
      end if;
   end Initial_Directory;

   procedure Relative_Name (
      Name : String;
      First : out Positive;
      Last : out Natural)
   is
      Root_Last : Natural;
   begin
      Containing_Root_Directory (Name, Last => Root_Last);
      Last := Name'Last;
      if Name'First <= Root_Last then -- full
         First := Root_Last + 1; -- skip root
      else -- relative
         First := Name'Last + 1;
         for I in Name'Range loop
            if Is_Path_Delimiter (Name (I)) then
               First := I + 1;
               exit; -- found
            end if;
         end loop;
      end if;
   end Relative_Name;

   function Compose (
      Directory : String := "";
      Relative_Name : String;
      Extension : String := "";
      Path_Delimiter : Path_Delimiter_Type := Default_Path_Delimiter)
      return String
   is
      Parent_Count : Natural := 0;
      C_D_Last : Natural; -- Containing_Directory (Directory)
      R_R_First : Positive; -- Relative_Name (Relative_Name)
      R_R_Last : Natural;
   begin
      R_R_First := Relative_Name'First;
      R_R_Last := Relative_Name'Last;
      while R_R_First <= R_R_Last loop
         declare
            I_R_First : Positive; -- Initial_Directory (Relative_Name)
            I_R_Last : Natural;
         begin
            Initial_Directory (
               Relative_Name (R_R_First .. R_R_Last),
               First => I_R_First,
               Last => I_R_Last);
            if Is_Current_Directory_Name (
               Relative_Name (I_R_First .. I_R_Last))
            then
               Hierarchical_File_Names.Relative_Name (
                  Relative_Name (R_R_First .. R_R_Last),
                  First => R_R_First,
                  Last => R_R_Last);
            elsif Is_Parent_Directory_Name (
               Relative_Name (I_R_First .. I_R_Last))
            then
               Parent_Count := Parent_Count + 1;
               Hierarchical_File_Names.Relative_Name (
                  Relative_Name (R_R_First .. R_R_Last),
                  First => R_R_First,
                  Last => R_R_Last);
            else
               exit;
            end if;
         end;
      end loop;
      C_D_Last := Directory'Last;
      Exclude_Trailing_Directories (
         Directory,
         Last => C_D_Last,
         Level => Parent_Count);
      if Parent_Count > 0 then
         return Unfolded_Compose (
            Unfolded_Compose (
               Directory (Directory'First .. C_D_Last),
               Parent_Directory_Name (
                  Parent_Count,
                  Path_Delimiter => Path_Delimiter),
               Path_Delimiter => Path_Delimiter),
            Relative_Name (R_R_First .. R_R_Last),
            Extension,
            Path_Delimiter => Path_Delimiter);
      elsif Directory'First > C_D_Last
         and then R_R_First > R_R_Last
         and then (Directory'Length > 0 or else Relative_Name'Length > 0)
         and then Extension'Length = 0
      then
         return Current_Directory_Name;
      else
         return Unfolded_Compose (
            Directory (Directory'First .. C_D_Last),
            Relative_Name (R_R_First .. R_R_Last),
            Extension,
            Path_Delimiter => Path_Delimiter);
      end if;
   end Compose;

   function Relative_Name (
      Name : String;
      From : String;
      Path_Delimiter : Path_Delimiter_Type := Default_Path_Delimiter)
      return String
   is
      Name_Root_Last : Natural;
      From_Root_Last : Natural;
   begin
      Containing_Root_Directory (Name, Last => Name_Root_Last);
      Containing_Root_Directory (From, Last => From_Root_Last);
      if (Name'First <= Name_Root_Last) /= (From'First <= From_Root_Last) then
         --  Relative_Name ("A", "/B") or reverse
         Raise_Exception (Use_Error'Identity);
      elsif Name'First <= Name_Root_Last
         and then Name (Name'First .. Name_Root_Last) /=
            From (From'First .. From_Root_Last)
      then
         --  full names and different drive letters
         return Name;
      else
         declare
            R_N_First : Positive := Name'First;
            R_N_Last : Natural := Name'Last;
            R_F_First : Positive := From'First;
            R_F_Last : Natural := From'Last;
            Parent_Count : Natural := 0;
         begin
            --  remove same part
            while R_N_First <= R_N_Last and then R_F_First <= R_F_Last loop
               declare
                  I_N_First : Positive; -- Initial_Directory (Name)
                  I_N_Last : Natural;
                  I_F_First : Positive; -- Initial_Directory (From)
                  I_F_Last : Natural;
               begin
                  Initial_Directory (
                     Name (R_N_First .. R_N_Last),
                     First => I_N_First,
                     Last => I_N_Last);
                  Initial_Directory (
                     From (R_N_First .. R_F_Last),
                     First => I_F_First,
                     Last => I_F_Last);
                  if Name (I_N_First .. I_N_Last) =
                     From (I_F_First .. I_F_Last)
                  then
                     Relative_Name (
                        Name (R_N_First .. R_N_Last),
                        First => R_N_First,
                        Last => R_N_Last);
                     Relative_Name (
                        Name (R_F_First .. R_F_Last),
                        First => R_F_First,
                        Last => R_F_Last);
                  else
                     exit;
                  end if;
               end;
            end loop;
            --  strip "./" in remainder of Name
            while R_N_First <= R_N_Last loop
               declare
                  I_N_First : Positive; -- Initial_Directory (Name)
                  I_N_Last : Natural;
               begin
                  Initial_Directory (
                     Name (R_N_First .. R_N_Last),
                     First => I_N_First,
                     Last => I_N_Last);
                  exit when not Is_Current_Directory_Name (
                     Name (I_N_First .. I_N_Last));
                  Relative_Name (
                     Name (R_N_First .. R_N_Last),
                     First => R_N_First,
                     Last => R_N_Last);
               end;
            end loop;
            --  remainder of From
            while R_F_First <= R_F_Last loop
               declare
                  I_F_First : Positive; -- Initial_Directory (From)
                  I_F_Last : Natural;
               begin
                  Initial_Directory (
                     From (R_F_First .. R_F_Last),
                     First => I_F_First,
                     Last => I_F_Last);
                  if Is_Current_Directory_Name (
                     From (I_F_First .. I_F_Last))
                  then
                     null; -- skip "./" of From
                  elsif Is_Parent_Directory_Name (
                     From (I_F_First .. I_F_Last))
                  then
                     if Parent_Count > 0 then
                        Parent_Count := Parent_Count - 1;
                     else
                        --  Relative_Name ("A", "..")
                        Raise_Exception (Use_Error'Identity);
                     end if;
                  else
                     Parent_Count := Parent_Count + 1;
                  end if;
                  Relative_Name (
                     From (R_F_First .. R_F_Last),
                     First => R_F_First,
                     Last => R_F_Last);
               end;
            end loop;
            if Parent_Count > 0 then
               if R_N_First > R_N_Last then
                  return Parent_Directory_Name (
                     Parent_Count,
                     Path_Delimiter => Path_Delimiter);
               else
                  return Unfolded_Compose (
                     Parent_Directory_Name (
                        Parent_Count,
                        Path_Delimiter => Path_Delimiter),
                     Name (R_N_First .. R_N_Last),
                     Path_Delimiter => Path_Delimiter);
               end if;
            elsif R_N_First > R_N_Last then
               return Current_Directory_Name;
            else
               return Name (R_N_First .. R_N_Last);
            end if;
         end;
      end if;
   end Relative_Name;

   function Parent_Directory (
      Directory : String;
      Path_Delimiter : Path_Delimiter_Type := Default_Path_Delimiter)
      return String
   is
      First : Positive;
      Last : Natural;
      Parent_Count : Natural;
   begin
      Parent_Directory (
         Directory,
         First => First,
         Last => Last,
         Parent_Count => Parent_Count);
      if Parent_Count = 0 then
         if First <= Last then
            return Directory (First .. Last);
         else
            return Current_Directory_Name;
         end if;
      else
         if First <= Last then -- Is_Full_Name (Directory)
            --  raise Use_Error ?
            return Unfolded_Compose (
               Directory (First .. Last),
               Parent_Directory_Name (
                  Parent_Count,
                  Path_Delimiter => Path_Delimiter),
               Path_Delimiter => Path_Delimiter);
         else
            return Parent_Directory_Name (
               Parent_Count,
               Path_Delimiter => Path_Delimiter);
         end if;
      end if;
   end Parent_Directory;

   procedure Parent_Directory (
      Directory : String;
      First : out Positive;
      Last : out Natural;
      Parent_Count : out Natural) is
   begin
      First := Directory'First;
      Last := Directory'Last;
      Parent_Count := 1;
      Exclude_Trailing_Directories (
         Directory,
         Last => Last,
         Level => Parent_Count);
   end Parent_Directory;

end Ada.Hierarchical_File_Names;
