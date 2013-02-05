with Ada.Exceptions;
package body Ada.Directories.Hierarchical_File_Names is

   function Is_Simple_Name (Name : String) return Boolean is
   begin
      for I in Name'Range loop
         if Name (I) = '/' then
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
      return Name'First <= Name'Last and then Name (Name'First) = '/';
   end Is_Full_Name;

   function Is_Relative_Name (Name : String) return Boolean is
   begin
      return not Is_Full_Name (Name);
   end Is_Relative_Name;

   procedure Initial_Directory (
      Name : String;
      First : out Positive;
      Last : out Natural) is
   begin
      First := Name'First;
      if Is_Full_Name (Name) then
         Last := Name'First; -- Name (First .. Last) = "/"
      else
         Last := First - 1;
         for I in Name'Range loop
            if Name (I) = '/' then
               Last := I - 1;
               exit; -- found
            end if;
         end loop;
      end if;
   end Initial_Directory;

   function Initial_Directory (Name : String) return String is
      First : Positive;
      Last : Natural;
   begin
      Initial_Directory (Name, First, Last);
      return Name (First .. Last);
   end Initial_Directory;

   procedure Relative_Name (
      Name : String;
      First : out Positive;
      Last : out Natural) is
   begin
      Last := Name'Last;
      if Is_Full_Name (Name) then
         First := Name'First + 1; -- skip root "/"
      else
         First := Name'First;
         for I in Name'Range loop
            if Name (I) = '/' then
               First := I + 1;
               --  Relative_Name ("A//B") = "B"
               while First <= Last and then Name (First) = '/' loop
                  First := First + 1;
               end loop;
               exit; -- found
            end if;
         end loop;
      end if;
   end Relative_Name;

   function Relative_Name (Name : String) return String is
      First : Positive;
      Last : Natural;
   begin
      Relative_Name (Name, First, Last);
      return Name (First .. Last);
   end Relative_Name;

   function Parent_Directory_Name (Level : Positive) return String is
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
         return Directories.Compose (
            Directories.Compose (
               Directory (C_D_First .. C_D_Last),
               Parent_Directory_Name (Parent_Count)),
            Relative_Name (R_R_First .. R_R_Last),
            Extension);
      else
         return Directories.Compose (
            Directory (C_D_First .. C_D_Last),
            Relative_Name (R_R_First .. R_R_Last),
            Extension);
      end if;
   end Compose;

   function Relative_Name (Name : String; From : String)
      return String is
   begin
      if Is_Full_Name (Name) /= Is_Full_Name (From) then
         --  Relative_Name ("A", "/B") or reverse
         Exceptions.Raise_Exception_From_Here (Name_Error'Identity);
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
                           Name_Error'Identity);
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
                  return Parent_Directory_Name (Parent_Count);
               else
                  return Directories.Compose (
                     Parent_Directory_Name (Parent_Count),
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

end Ada.Directories.Hierarchical_File_Names;
