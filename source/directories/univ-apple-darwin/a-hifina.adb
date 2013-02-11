with Ada.Exceptions;
pragma Warnings (Off, Ada.Exceptions); -- break "pure" rule
package body Ada.Hierarchical_File_Names is

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

   --  path delimiter

   function Is_Path_Delimiter (Item : Character) return Boolean is
   begin
      return Item = '/';
   end Is_Path_Delimiter;

   procedure Include_Trailing_Path_Delimiter (
      S : in out String;
      Last : in out Natural) is
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

   procedure Exclude_Leading_Path_Delimiter (
      S : String;
      First : in out Positive) is
   begin
      while First <= S'Last
         and then Is_Path_Delimiter (S (First))
      loop
         First := First + 1;
      end loop;
   end Exclude_Leading_Path_Delimiter;

   --  operations in Ada.Directories

   function Simple_Name (Name : String) return String is
      First : Positive;
      Last : Natural;
   begin
      Simple_Name (Name, First, Last);
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
      Containing_Directory (Name, First, Last);
      if First > Last and then Raise_On_Error then
         --  A.16.1 (38/3)
         Exceptions.Raise_Exception_From_Here (Use_Error'Identity);
      end if;
      return Name (First .. Last);
   end Containing_Directory;

   function Extension (Name : String) return String is
      First : Positive;
      Last : Natural;
   begin
      Extension (Name, First, Last);
      return Name (First .. Last);
   end Extension;

   function Base_Name (Name : String) return String is
      First : Positive;
      Last : Natural;
   begin
      Base_Name (Name, First, Last);
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
               Exclude_Trailing_Path_Delimiter (Name, Last);
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
      Simple_Name (Name, First, Last);
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

   function Compose_No_Folding (
      Containing_Directory : String := "";
      Name : String;
      Extension : String := "") return String
   is
      --  this is Directories.Compose
      --  if you want to fold '.' or '..', use Hierarchical_File_Names.Compose
      Result : String (
         1 ..
         Containing_Directory'Length + Name'Length + Extension'Length + 2);
      Last : Natural;
   begin
      --  append directory
      Last := Containing_Directory'Length;
      if Last > 0 then
         Result (1 .. Last) := Containing_Directory;
         Include_Trailing_Path_Delimiter (
            Result,
            Last);
      end if;
      --  append name
      Result (Last + 1 .. Last + Name'Length) := Name;
      Last := Last + Name'Length;
      --  append extension
      if Extension'Length /= 0 then
         Last := Last + 1;
         Result (Last) := '.';
         Result (Last + 1 .. Last + Extension'Length) := Extension;
         Last := Last + Extension'Length;
      end if;
      return Result (1 .. Last);
   end Compose_No_Folding;

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
   begin
      return Name = "/";
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
         and then Name (Name'First) = '/';
   end Is_Full_Name;

   function Is_Relative_Name (Name : String) return Boolean is
   begin
      return not Is_Full_Name (Name);
   end Is_Relative_Name;

   function Initial_Directory (Name : String) return String is
      First : Positive;
      Last : Natural;
   begin
      Initial_Directory (Name, First, Last);
      return Name (First .. Last);
   end Initial_Directory;

   function Relative_Name (Name : String) return String is
      First : Positive;
      Last : Natural;
   begin
      Relative_Name (Name, First, Last);
      return Name (First .. Last);
   end Relative_Name;

   procedure Initial_Directory (
      Name : String;
      First : out Positive;
      Last : out Natural) is
   begin
      First := Name'First;
      if Is_Full_Name (Name) then -- full
         Last := Name'First; -- Name (First .. Last) = "/"
      else -- relative
         Last := First - 1;
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
      Last := Name'Last;
      if Is_Full_Name (Name) then -- full
         First := Name'First + 1; -- skip root "/"
      else -- relative
         First := Name'First;
         for I in Name'Range loop
            if Is_Path_Delimiter (Name (I)) then
               First := I + 1;
               --  "//" as "/"
               Exclude_Leading_Path_Delimiter (Name (First .. Last), First);
               exit; -- found
            end if;
         end loop;
      end if;
   end Relative_Name;

   function Compose (
      Directory : String := "";
      Relative_Name : String;
      Extension : String := "")
      return String
   is
      Parent_Count : Natural := 0;
      C_D_First : Positive; -- Containing_Directory (Directory)
      C_D_Last : Natural;
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
               I_R_First,
               I_R_Last);
            if Is_Current_Directory_Name (
               Relative_Name (I_R_First .. I_R_Last))
            then
               Hierarchical_File_Names.Relative_Name (
                  Relative_Name (R_R_First .. R_R_Last),
                  R_R_First,
                  R_R_Last);
            elsif Is_Parent_Directory_Name (
               Relative_Name (I_R_First .. I_R_Last))
            then
               Parent_Count := Parent_Count + 1;
               Hierarchical_File_Names.Relative_Name (
                  Relative_Name (R_R_First .. R_R_Last),
                  R_R_First,
                  R_R_Last);
            else
               exit;
            end if;
         end;
      end loop;
      C_D_First := Directory'First;
      C_D_Last := Directory'Last;
      while C_D_First <= C_D_Last loop
         declare
            S_D_First : Positive; -- Simple_Name (Directory)
            S_D_Last : Natural;
         begin
            Simple_Name (
               Directory (C_D_First .. C_D_Last),
               S_D_First,
               S_D_Last);
            if Is_Current_Directory_Name (
               Directory (S_D_First .. S_D_Last))
            then
               Containing_Directory (
                  Directory (C_D_First .. C_D_Last),
                  C_D_First,
                  C_D_Last);
            elsif Is_Parent_Directory_Name (
               Directory (S_D_First .. S_D_Last))
            then
               Parent_Count := Parent_Count + 1;
               Containing_Directory (
                  Directory (C_D_First .. C_D_Last),
                  C_D_First,
                  C_D_Last);
            elsif Parent_Count > 0
               and then not Is_Root_Directory_Name (
                  Directory (C_D_First .. C_D_Last))
            then
               Parent_Count := Parent_Count - 1;
               Containing_Directory (
                  Directory (C_D_First .. C_D_Last),
                  C_D_First,
                  C_D_Last);
            else
               exit;
            end if;
         end;
      end loop;
      if Parent_Count > 0 then
         return Compose_No_Folding (
            Compose_No_Folding (
               Directory (C_D_First .. C_D_Last),
               Parent_Directory_Name (
                  Parent_Count)),
            Relative_Name (R_R_First .. R_R_Last),
            Extension);
      else
         return Compose_No_Folding (
            Directory (C_D_First .. C_D_Last),
            Relative_Name (R_R_First .. R_R_Last),
            Extension);
      end if;
   end Compose;

   function Relative_Name (
      Name : String;
      From : String)
      return String is
   begin
      if Is_Full_Name (Name) /= Is_Full_Name (From) then
         --  Relative_Name ("A", "/B") or reverse
         Exceptions.Raise_Exception_From_Here (Use_Error'Identity);
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
                     I_N_First,
                     I_N_Last);
                  if I_N_First > I_N_Last then
                     I_N_Last := R_N_Last;
                  end if;
                  Initial_Directory (
                     From (R_N_First .. R_F_Last),
                     I_F_First,
                     I_F_Last);
                  if I_F_First > I_F_Last then
                     I_F_Last := R_F_Last;
                  end if;
                  if Name (I_N_First .. I_N_Last) =
                     From (I_F_First .. I_F_Last)
                  then
                     declare
                        Old_R_N_First : constant Positive := R_F_First;
                     begin
                        Relative_Name (
                           Name (R_N_First .. R_N_Last),
                           R_N_First,
                           R_N_Last);
                        if R_N_First = Old_R_N_First then
                           R_N_First := R_N_Last + 1;
                        end if;
                     end;
                     declare
                        Old_R_F_First : constant Positive := R_F_First;
                     begin
                        Relative_Name (
                           Name (R_F_First .. R_F_Last),
                           R_F_First,
                           R_F_Last);
                        if R_F_First = Old_R_F_First then
                           R_F_First := R_F_Last + 1;
                        end if;
                     end;
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
                  Old_R_N_First : constant Positive := R_N_First;
               begin
                  Initial_Directory (
                     Name (R_N_First .. R_N_Last),
                     I_N_First,
                     I_N_Last);
                  if I_N_First > I_N_Last then
                     I_N_Last := R_N_Last;
                  end if;
                  exit when not Is_Current_Directory_Name (
                     Name (I_N_First .. I_N_Last));
                  Relative_Name (
                     Name (R_N_First .. R_N_Last),
                     R_N_First,
                     R_N_Last);
                  if R_N_First = Old_R_N_First then
                     R_N_First := R_N_Last + 1;
                  end if;
               end;
            end loop;
            --  remainder of From
            while R_F_First <= R_F_Last loop
               declare
                  I_F_First : Positive; -- Initial_Directory (From)
                  I_F_Last : Natural;
                  Old_R_F_First : constant Positive := R_F_First;
               begin
                  Initial_Directory (
                     From (R_F_First .. R_F_Last),
                     I_F_First,
                     I_F_Last);
                  if I_F_First > I_F_Last then
                     I_F_Last := R_F_Last;
                  end if;
                  if Is_Current_Directory_Name (
                     From (I_F_First .. I_F_Last))
                  then
                     null;
                  elsif Is_Parent_Directory_Name (
                     From (I_F_First .. I_F_Last))
                  then
                     if Parent_Count > 0 then
                        Parent_Count := Parent_Count - 1;
                     else
                        --  Relative_Name ("A", "..")
                        Exceptions.Raise_Exception_From_Here (
                           Use_Error'Identity);
                     end if;
                  else
                     Parent_Count := Parent_Count + 1;
                  end if;
                  Relative_Name (
                     From (R_F_First .. R_F_Last),
                     R_F_First,
                     R_F_Last);
                  exit when R_F_First = Old_R_F_First; -- last one
               end;
            end loop;
            if Parent_Count > 0 then
               if R_N_First > R_N_Last then
                  return Parent_Directory_Name (
                     Parent_Count);
               else
                  return Compose_No_Folding (
                     Parent_Directory_Name (
                        Parent_Count),
                     Name (R_N_First .. R_N_Last));
               end if;
            elsif R_N_First > R_N_Last then
               return ".";
            else
               return Name (R_N_First .. R_N_Last);
            end if;
         end;
      end if;
   end Relative_Name;

end Ada.Hierarchical_File_Names;
