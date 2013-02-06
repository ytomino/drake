with Ada.Directories;
with Ada.Directories.Hierarchical_File_Names;
with Ada.Directories.Volumes;
with Ada.Directories.Equal_File_Names;
with Ada.Directories.Less_File_Names;
procedure filename is
	package AD renames Ada.Directories;
	package ADH renames Ada.Directories.Hierarchical_File_Names;
begin
	pragma Assert (AD.Containing_Directory ("") = "");
	pragma Assert (AD.Containing_Directory ("A") = "");
	pragma Assert (AD.Containing_Directory ("A/B") = "A");
	pragma Assert (AD.Containing_Directory ("A/B/") = "A/B");
	pragma Assert (AD.Containing_Directory ("A//B") = "A");
	pragma Assert (AD.Containing_Directory ("/") = "/");
	pragma Assert (AD.Containing_Directory ("/A") = "/");
	pragma Assert (AD.Containing_Directory ("/A/B") = "/A");
	pragma Assert (AD.Containing_Directory ("/A/B/") = "/A/B");
	pragma Assert (AD.Containing_Directory ("/A//B") = "/A");
	pragma Assert (AD.Simple_Name ("") = "");
	pragma Assert (AD.Simple_Name ("A") = "A");
	pragma Assert (AD.Simple_Name ("A/B") = "B");
	pragma Assert (AD.Simple_Name ("A/B/") = "");
	pragma Assert (AD.Simple_Name ("A//B") = "B");
	pragma Assert (AD.Simple_Name ("/") = "");
	pragma Assert (AD.Simple_Name ("/A") = "A");
	pragma Assert (AD.Simple_Name ("/A/B") = "B");
	pragma Assert (AD.Simple_Name ("/A/B/") = "");
	pragma Assert (AD.Simple_Name ("/A//B") = "B");
	pragma Assert (AD.Base_Name ("README") = "README");
	pragma Assert (AD.Base_Name ("README.") = "README.");
	pragma Assert (AD.Base_Name ("README.TXT") = "README");
	pragma Assert (AD.Base_Name (".TXT") = ".TXT");
	pragma Assert (AD.Base_Name (".") = ".");
	pragma Assert (AD.Base_Name ("..") = "..");
	pragma Assert (AD.Extension ("README") = "");
	pragma Assert (AD.Extension ("README.") = "");
	pragma Assert (AD.Extension ("README.TXT") = "TXT");
	pragma Assert (AD.Extension (".TXT") = "");
	pragma Assert (AD.Extension (".") = "");
	pragma Assert (AD.Extension ("..") = "");
	pragma Assert (ADH.Initial_Directory ("") = "");
	pragma Assert (ADH.Initial_Directory ("A") = "");
	pragma Assert (ADH.Initial_Directory ("A/B") = "A");
	pragma Assert (ADH.Initial_Directory ("A/B/") = "A");
	pragma Assert (ADH.Initial_Directory ("A//B") = "A");
	pragma Assert (ADH.Initial_Directory ("/") = "/");
	pragma Assert (ADH.Initial_Directory ("/A") = "/");
	pragma Assert (ADH.Initial_Directory ("/A/B") = "/");
	pragma Assert (ADH.Initial_Directory ("/A/B/") = "/");
	pragma Assert (ADH.Initial_Directory ("/A//B") = "/");
	pragma Assert (ADH.Relative_Name ("") = "");
	pragma Assert (ADH.Relative_Name ("A") = "A");
	pragma Assert (ADH.Relative_Name ("A/B") = "B");
	pragma Assert (ADH.Relative_Name ("A/B/") = "B/");
	pragma Assert (ADH.Relative_Name ("A//B") = "B");
	pragma Assert (ADH.Relative_Name ("/") = "");
	pragma Assert (ADH.Relative_Name ("/A") = "A");
	pragma Assert (ADH.Relative_Name ("/A/B") = "A/B");
	pragma Assert (ADH.Relative_Name ("/A/B/") = "A/B/");
	pragma Assert (ADH.Relative_Name ("/A//B") = "A//B");
	pragma Assert (AD.Compose ("", "", "") = "");
	pragma Assert (AD.Compose ("", "../A") = "../A");
	pragma Assert (AD.Compose ("A", "B", "C") = "A/B.C");
	pragma Assert (AD.Compose ("A", "../B") = "A/../B");
	pragma Assert (AD.Compose ("A/B", "../C") = "A/B/../C");
	pragma Assert (AD.Compose ("/", "../A") = "/../A");
	pragma Assert (ADH.Compose ("", "", "") = "");
	pragma Assert (ADH.Compose ("", "../A") = "../A");
	pragma Assert (ADH.Compose ("A", "B", "C") = "A/B.C");
	pragma Assert (ADH.Compose ("A", "../B") = "B");
	pragma Assert (ADH.Compose ("A/B", "../C") = "A/C");
	pragma Assert (ADH.Compose ("/", "../A") = "/../A");
	pragma Assert (ADH.Relative_Name ("A", "B") = "../A");
	pragma Assert (ADH.Relative_Name ("A", "A") = ".");
	pragma Assert (ADH.Relative_Name ("A/B", "A") = "B");
	pragma Assert (ADH.Relative_Name ("A/B", "A/C") = "../B");
	pragma Assert (ADH.Relative_Name ("/A", "/B") = "../A");
	pragma Assert (ADH.Relative_Name ("/A", "/A") = ".");
	pragma Assert (ADH.Relative_Name ("/A/B", "/A") = "B");
	pragma Assert (ADH.Relative_Name ("/A/B", "/A/C") = "../B");
	pragma Assert (ADH.Relative_Name ("../A", "B") = "../../A");
	pragma Assert (ADH.Relative_Name ("../A", "../B") = "../A");
	pragma Assert (ADH.Relative_Name ("A", "B/C") = "../../A");
	pragma Assert (ADH.Relative_Name ("A", "") = "A");
	pragma Assert (ADH.Relative_Name ("A", ".") = "A");
	pragma Assert (ADH.Relative_Name ("", "") = ".");
	pragma Assert (ADH.Relative_Name ("", ".") = ".");
	pragma Assert (ADH.Relative_Name ("", "A") = "..");
	pragma Assert (ADH.Relative_Name (".", "A") = "..");
	begin
		declare
			X : constant String := ADH.Relative_Name ("A", "..");
		begin
			raise Program_Error; -- NG
		end;
	exception
		when AD.Name_Error => null; -- OK
	end;
	pragma Assert (ADH.Relative_Name ("A/B", "C/../D") = "../A/B");
	Ada.Debug.Put (ADH.Relative_Name ("A/B", "C/../A")); -- "../A/B", it should be normalized to "B" ?
	declare
		FS : Ada.Directories.Volumes.File_System :=
			Ada.Directories.Volumes.Where ("/");
		FS_Name : constant String := Ada.Directories.Volumes.Format_Name (FS);
	begin
		Ada.Debug.Put (FS_Name);
		if FS_Name = "hfs" then
			Ada.Debug.Put ("test for comparing HFS+ filenames");
			declare
				subtype C is Character;
				Full_Width_Upper_A : constant String := (
					C'Val (16#ef#), C'Val (16#bc#), C'Val (16#a1#));
				Full_Width_Lower_A : constant String := (
					C'Val (16#ef#), C'Val (16#bd#), C'Val (16#81#));
				Full_Width_Upper_B : constant String := (
					C'Val (16#ef#), C'Val (16#bc#), C'Val (16#a2#));
				Full_Width_Lower_B : constant String := (
					C'Val (16#ef#), C'Val (16#bd#), C'Val (16#82#));
				KA_Dakuten : constant String := (
					C'Val (16#e3#), C'Val (16#81#), C'Val (16#8b#), C'Val (16#e3#), C'Val (16#82#), C'Val (16#99#));
				GA : constant String := (
					C'Val (16#e3#), C'Val (16#81#), C'Val (16#8c#));
			begin
				pragma Assert (AD.Equal_File_Names (FS, "", ""));
				pragma Assert (not AD.Equal_File_Names (FS, "", "#"));
				pragma Assert (not AD.Equal_File_Names (FS, "#", ""));
				pragma Assert (AD.Equal_File_Names (FS, "#", "#"));
				pragma Assert (AD.Equal_File_Names (FS, "A", "A"));
				pragma Assert (AD.Equal_File_Names (FS, "a", "A"));
				pragma Assert (AD.Equal_File_Names (FS, Full_Width_Lower_A, Full_Width_Upper_A));
				pragma Assert (not AD.Less_File_Names (FS, "", ""));
				pragma Assert (AD.Less_File_Names (FS, "", "#"));
				pragma Assert (not AD.Less_File_Names (FS, "#", ""));
				pragma Assert (not AD.Less_File_Names (FS, "#", "#"));
				pragma Assert (AD.Less_File_Names (FS, Full_Width_Upper_A, Full_Width_Lower_B));
				pragma Assert (AD.Less_File_Names (FS, Full_Width_Lower_A, Full_Width_Upper_B));
				pragma Assert (AD.Equal_File_Names (FS, (1 => C'Val (16#80#)), "%80"));
				pragma Assert (AD.Equal_File_Names (FS, KA_Dakuten, GA));
				null;
			end;
		end if;
	end;
	pragma Debug (Ada.Debug.Put ("OK"));
end filename;
