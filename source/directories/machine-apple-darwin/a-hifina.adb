with Ada.Exception_Identification.From_Here;
package body Ada.Hierarchical_File_Names is
   use Exception_Identification.From_Here;

   function Parent_Directory_Name (
      Level : Positive)
      return String;
   function Parent_Directory_Name (
      Level : Positive)
      return String is
   begin
      return Result : String (1 .. 3 * Level - 1) do
         Result (1) := '.';
         Result (2) := '.';
         for I in 2 .. Level loop
            Result (I * 3 - 3) := '/';
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
      Last := Name'First - 1;
      while Last < Name'Last and then Is_Path_Delimiter (Name (Last + 1)) loop
         Last := Last + 1;
      end loop;
   end Containing_Root_Directory;

   procedure Exclude_Trailing_Directories (
      Directory : String;
      Last : in out Natural;
      Level : in out Natural);
   procedure Exclude_Trailing_Directories (
      Directory : String;
      Last : in out Natural;
      Level : in out Natural)
   is
      R_Last : Natural;
   begin
      Exclude_Trailing_Path_Delimiter (Directory, Last);
      Containing_Root_Directory (
         Directory (Directory'First .. Last),
         Last => R_Last); -- First - 1 if not Is_Full_Name (...)
      while Last > R_Last loop
         declare
            S_First : Positive;
            S_Last : Natural;
         begin
            Simple_Name (
               Directory (Directory'First .. Last),
               First => S_First,
               Last => S_Last);
            if Is_Current_Directory_Name (Directory (S_First .. S_Last)) then
               null; -- skip "./"
            elsif Is_Parent_Directory_Name (Directory (S_First .. S_Last)) then
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
      return Item = '/';
   end Is_Path_Delimiter;

   procedure Include_Trailing_Path_Delimiter (
      S : in out String;
      Last : in out Natural;
      Path_Delimiter : Path_Delimiter_Type := Default_Path_Delimiter)
   is
      pragma Unreferenced (Path_Delimiter);
   begin
      if not Is_Path_Delimiter (S (Last)) then
         Last := Last + 1;
         S (Last) := '/';
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
      if First > Last then
         Raise_Exception (Name_Error'Identity); -- CXAG002
      end if;
      return Name (First .. Last);
   end Simple_Name;

   function Unchecked_Simple_Name (Name : String) return String is
      First : Positive;
      Last : Natural;
   begin
      Simple_Name (Name, First => First, Last => Last);
      return Name (First .. Last);
   end Unchecked_Simple_Name;

   function Containing_Directory (Name : String) return String is
      First : Positive;
      Last : Natural;
      Error : Boolean;
   begin
      Containing_Directory (Name, First => First, Last => Last);
      Error := First > Last;
      if not Error then
         --  ignore trailing delimiters on error-checking
         Error := True;
         for I in reverse Last + 1 .. Name'Last loop
            if not Is_Path_Delimiter (Name (I)) then
               Error := False;
               exit;
            end if;
         end loop;
      end if;
      if Error then
         Raise_Exception (Use_Error'Identity); -- RM A.16.1(38/3)
      end if;
      return Name (First .. Last);
   end Containing_Directory;

   function Unchecked_Containing_Directory (Name : String) return String is
      First : Positive;
      Last : Natural;
   begin
      Containing_Directory (Name, First => First, Last => Last);
      return Name (First .. Last);
   end Unchecked_Containing_Directory;

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
      Last : out Natural) is
   begin
      First := Name'First;
      Last := Name'Last;
      for I in reverse Name'Range loop
         if Is_Path_Delimiter (Name (I)) then
            First := I + 1;
            exit; -- found
         end if;
      end loop;
   end Simple_Name;

   procedure Containing_Directory (
      Name : String;
      First : out Positive;
      Last : out Natural) is
   begin
      First := Name'First;
      Last := Name'First - 1;
      for I in reverse Name'Range loop
         if Is_Path_Delimiter (Name (I)) then
            if I > First then
               Last := I - 1;
               --  "//" as "/"
               Exclude_Trailing_Path_Delimiter (Name, Last => Last);
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
      Last : out Natural) is
   begin
      First := Name'Last + 1;
      Last := Name'Last;
      for I in reverse Name'Range loop
         if Is_Path_Delimiter (Name (I)) then
            exit; -- not found
         elsif Name (I) = '.' then
            --  Extension (".DOTFILE") = ""
            if I > Name'First
               and then not Is_Path_Delimiter (Name (I - 1))
            then
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
   begin
      return Name'First <= Name'Last
         and then Is_Path_Delimiter (Name (Name'First));
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
      if First > Last or else First = Name'First then
         Raise_Exception (Name_Error'Identity); -- CXAG002
      end if;
      return Name (First .. Last);
   end Relative_Name;

   function Unchecked_Relative_Name (Name : String) return String is
      First : Positive;
      Last : Natural;
   begin
      Relative_Name (Name, First => First, Last => Last);
      return Name (First .. Last);
   end Unchecked_Relative_Name;

   procedure Initial_Directory (
      Name : String;
      First : out Positive;
      Last : out Natural) is
   begin
      First := Name'First;
      if Is_Full_Name (Name) then -- full
         Last := Name'First; -- Name (First .. Last) = "/"
      else -- relative
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
      Last : out Natural) is
   begin
      First := Name'Last + 1;
      Last := Name'Last;
      for I in Name'Range loop
         if Is_Path_Delimiter (Name (I)) then
            First := I + 1;
            --  "//" as "/"
            while First <= Last and then Is_Path_Delimiter (Name (First)) loop
               First := First + 1;
            end loop;
            exit; -- found
         end if;
      end loop;
   end Relative_Name;

   function Compose (
      Directory : String := "";
      Relative_Name : String;
      Extension : String := "";
      Path_Delimiter : Path_Delimiter_Type := Default_Path_Delimiter)
      return String
   is
      pragma Check (Pre,
         Check => Directory'Length = 0
            or else Is_Relative_Name (Relative_Name)
            or else raise Name_Error); -- CXAG002
      pragma Unreferenced (Path_Delimiter);
      Directory_Length : constant Natural := Directory'Length;
      Relative_Name_Length : constant Natural := Relative_Name'Length;
      Extension_Length : constant Natural := Extension'Length;
      Result : String (
         1 .. Directory_Length + Relative_Name_Length + Extension_Length + 2);
      Last : Natural;
   begin
      --  append directory
      Last := Directory_Length;
      if Last > 0 then
         Result (1 .. Last) := Directory;
         Include_Trailing_Path_Delimiter (Result, Last => Last);
      end if;
      --  append name
      Result (Last + 1 .. Last + Relative_Name_Length) := Relative_Name;
      Last := Last + Relative_Name_Length;
      --  append extension
      if Extension_Length /= 0 then
         Last := Last + 1;
         Result (Last) := '.';
         Result (Last + 1 .. Last + Extension_Length) := Extension;
         Last := Last + Extension_Length;
      end if;
      return Result (1 .. Last);
   end Compose;

   function Normalized_Compose (
      Directory : String := "";
      Relative_Name : String;
      Extension : String := "";
      Path_Delimiter : Path_Delimiter_Type := Default_Path_Delimiter)
      return String
   is
      pragma Unreferenced (Path_Delimiter);
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
         return Compose (
            Compose (
               Directory (Directory'First .. C_D_Last),
               Parent_Directory_Name (
                  Parent_Count)),
            Relative_Name (R_R_First .. R_R_Last),
            Extension);
      elsif Directory'First > C_D_Last
         and then R_R_First > R_R_Last
         and then (Directory'Length > 0 or else Relative_Name'Length > 0)
         and then Extension'Length = 0
      then
         return Current_Directory_Name;
      else
         return Compose (
            Directory (Directory'First .. C_D_Last),
            Relative_Name (R_R_First .. R_R_Last),
            Extension);
      end if;
   end Normalized_Compose;

   function Relative_Name (
      Name : String;
      From : String;
      Path_Delimiter : Path_Delimiter_Type := Default_Path_Delimiter)
      return String
   is
      pragma Unreferenced (Path_Delimiter);
   begin
      if Is_Full_Name (Name) /= Is_Full_Name (From) then
         --  Relative_Name ("A", "/B") or reverse
         Raise_Exception (Use_Error'Identity);
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
                     Parent_Count);
               else
                  return Compose (
                     Parent_Directory_Name (
                        Parent_Count),
                     Name (R_N_First .. R_N_Last));
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
      pragma Unreferenced (Path_Delimiter);
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
            return Compose (
               Directory (First .. Last),
               Parent_Directory_Name (
                  Parent_Count));
         else
            return Parent_Directory_Name (
               Parent_Count);
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
