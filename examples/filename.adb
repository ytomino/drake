with Ada.Directories;
with Ada.Directories.Hierarchical_File_Names;
with Ada.Directories.Volumes;
with Ada.Directories.Equal_File_Names;
with Ada.Directories.Less_File_Names;
with Ada.Hierarchical_File_Names;
procedure filename is
	Windows : constant Boolean := Ada.Hierarchical_File_Names.Is_Path_Delimiter ('\');
	package AD renames Ada.Directories;
	package ADH renames Ada.Directories.Hierarchical_File_Names;
begin
	pragma Assert (AD.Containing_Directory ("", Raise_On_Error => False) = "");
	pragma Assert (AD.Containing_Directory ("A", Raise_On_Error => False) = "");
	pragma Assert (AD.Containing_Directory ("A/B") = "A");
	pragma Assert (AD.Containing_Directory ("A/B/") = "A/B");
	pragma Assert (Windows or else AD.Containing_Directory ("A//B") = "A");
	pragma Assert (AD.Containing_Directory ("/") = "/");
	pragma Assert (AD.Containing_Directory ("/A") = "/");
	pragma Assert (AD.Containing_Directory ("/A/B") = "/A");
	pragma Assert (AD.Containing_Directory ("/A/B/") = "/A/B");
	pragma Assert (Windows or else AD.Containing_Directory ("/A//B") = "/A");
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
	pragma Assert (AD.Extension ("DIR/.TXT") = "");
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
	pragma Assert (Windows or else ADH.Relative_Name ("A//B") = "B");
	pragma Assert (ADH.Relative_Name ("/") = "");
	pragma Assert (ADH.Relative_Name ("/A") = "A");
	pragma Assert (ADH.Relative_Name ("/A/B") = "A/B");
	pragma Assert (ADH.Relative_Name ("/A/B/") = "A/B/");
	pragma Assert (ADH.Relative_Name ("/A//B") = "A//B");
	pragma Assert (AD.Compose ("", "", "") = "");
	pragma Assert (AD.Compose ("", "../A") = "../A");
	pragma Assert (AD.Compose ("/", "../A") = "/../A");
	if Windows then
		pragma Assert (AD.Compose ("A", "B", "C") = "A\B.C");
		pragma Assert (AD.Compose ("A", "../B") = "A\../B");
		pragma Assert (AD.Compose ("A/B", "../C") = "A/B\../C");
		null;
	else
		pragma Assert (AD.Compose ("A", "B", "C") = "A/B.C");
		pragma Assert (AD.Compose ("A", "../B") = "A/../B");
		pragma Assert (AD.Compose ("A/B", "../C") = "A/B/../C");
		null;
	end if;
	pragma Assert (ADH.Compose ("", "", "") = "");
	pragma Assert (ADH.Compose ("A", "../B") = "B");
	if Windows then
		pragma Assert (ADH.Compose ("", "../A") = "..\A");
		pragma Assert (ADH.Compose ("A", "B", "C") = "A\B.C");
		pragma Assert (ADH.Compose ("A/B", "../C") = "A\C");
		pragma Assert (ADH.Compose ("/", "../A") = "/..\A");
		null;
	else
		pragma Assert (ADH.Compose ("", "../A") = "../A");
		pragma Assert (ADH.Compose ("A", "B", "C") = "A/B.C");
		pragma Assert (ADH.Compose ("A/B", "../C") = "A/C");
		pragma Assert (ADH.Compose ("/", "../A") = "/../A");
		null;
	end if;
	pragma Assert (ADH.Relative_Name ("A", "A") = ".");
	pragma Assert (ADH.Relative_Name ("A/B", "A") = "B");
	pragma Assert (ADH.Relative_Name ("/A", "/A") = ".");
	pragma Assert (ADH.Relative_Name ("/A/B", "/A") = "B");
	pragma Assert (ADH.Relative_Name ("A", "") = "A");
	pragma Assert (ADH.Relative_Name ("A", ".") = "A");
	pragma Assert (ADH.Relative_Name ("", "") = ".");
	pragma Assert (ADH.Relative_Name ("", ".") = ".");
	pragma Assert (ADH.Relative_Name ("", "A") = "..");
	pragma Assert (ADH.Relative_Name (".", "A") = "..");
	if Windows then
		pragma Assert (ADH.Relative_Name ("A", "B") = "..\A");
		pragma Assert (ADH.Relative_Name ("A/B", "A/C") = "..\B");
		pragma Assert (ADH.Relative_Name ("/A", "/B") = "..\A");
		pragma Assert (ADH.Relative_Name ("/A/B", "/A/C") = "..\B");
		pragma Assert (ADH.Relative_Name ("../A", "B") = "..\../A");
		pragma Assert (ADH.Relative_Name ("../A", "../B") = "..\A");
		pragma Assert (ADH.Relative_Name ("A", "B/C") = "..\..\A");
		null;
	else
		pragma Assert (ADH.Relative_Name ("A", "B") = "../A");
		pragma Assert (ADH.Relative_Name ("A/B", "A/C") = "../B");
		pragma Assert (ADH.Relative_Name ("/A", "/B") = "../A");
		pragma Assert (ADH.Relative_Name ("/A/B", "/A/C") = "../B");
		pragma Assert (ADH.Relative_Name ("../A", "B") = "../../A");
		pragma Assert (ADH.Relative_Name ("../A", "../B") = "../A");
		pragma Assert (ADH.Relative_Name ("A", "B/C") = "../../A");
		null;
	end if;
	begin
		declare
			X : constant String := ADH.Relative_Name ("A", "..");
		begin
			raise Program_Error; -- NG
		end;
	exception
		when AD.Use_Error => null; -- OK
	end;
	if not Windows then
		pragma Assert (ADH.Relative_Name ("A/B", "C/../D") = "../A/B");
		Ada.Debug.Put (ADH.Relative_Name ("A/B", "C/../A")); -- "../A/B", it should be normalized to "B" ?
	end if;
	if Windows then
		-- drive letters
		Ada.Debug.Put ("test for drive letter");
		pragma Assert (ADH.Is_Full_Name ("C:\"));
		pragma Assert (ADH.Is_Full_Name ("C:\autoexec.bat"));
		pragma Assert (ADH.Is_Full_Name ("\\host\share\"));
		pragma Assert (ADH.Is_Full_Name ("\\host\share\filename"));
		pragma Assert (ADH.Is_Root_Directory_Name ("C:\"));
		pragma Assert (not ADH.Is_Root_Directory_Name ("C:\autoexec.bat"));
		pragma Assert (ADH.Is_Root_Directory_Name ("\\host\share\"));
		pragma Assert (not ADH.Is_Root_Directory_Name ("\\host\share\filename"));
		pragma Assert (ADH.Initial_Directory ("C:\") = "C:\");
		pragma Assert (ADH.Initial_Directory ("C:\autoexec.bat") = "C:\");
		pragma Assert (ADH.Initial_Directory ("\\host\share\") = "\\host\share\");
		pragma Assert (ADH.Initial_Directory ("\\host\share\filename") = "\\host\share\");
		pragma Assert (ADH.Relative_Name ("C:\") = "");
		pragma Assert (ADH.Relative_Name ("C:\autoexec.bat") = "autoexec.bat");
		pragma Assert (ADH.Relative_Name ("\\host\share\") = "");
		pragma Assert (ADH.Relative_Name ("\\host\share\filename") = "filename");
		pragma Assert (ADH.Relative_Name ("C:\A", "D:\B") = "C:\A");
	end if;
	declare
		FS : Ada.Directories.Volumes.File_System :=
			Ada.Directories.Volumes.Where ("/");
		FS_Name : constant String := Ada.Directories.Volumes.Format_Name (FS);
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
		A_DIAERESIS_DOTBELOW : constant String := (
			'A', C'Val (16#cc#), C'Val (16#88#), C'Val (16#cc#), C'Val (16#a3#));
		A_DOTBELOW_DIAERESIS : constant String := (
			'A', C'Val (16#cc#), C'Val (16#a3#), C'Val (16#cc#), C'Val (16#88#));
	begin
		Ada.Debug.Put (FS_Name);
		if FS_Name = "hfs" then
			Ada.Debug.Put ("test for comparing HFS+ filenames");
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
			pragma Assert (AD.Equal_File_Names (FS, A_DIAERESIS_DOTBELOW, A_DOTBELOW_DIAERESIS));
		elsif FS_Name = "NTFS" then
			Ada.Debug.Put ("test for comparing NTFS filenames");
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
			pragma Assert (not AD.Equal_File_Names (FS, (1 => C'Val (16#80#)), "%80"));
			pragma Assert (not AD.Equal_File_Names (FS, KA_Dakuten, GA));
			pragma Assert (not AD.Equal_File_Names (FS, A_DIAERESIS_DOTBELOW, A_DOTBELOW_DIAERESIS));
		end if;
	end;
	pragma Debug (Ada.Debug.Put ("OK"));
end filename;
