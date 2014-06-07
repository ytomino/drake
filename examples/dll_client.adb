with Ada.IO_Exceptions;
with Interfaces.C.Strings;
with System.Program.Dynamic_Linking;
with System.Storage_Elements.Formatting;
procedure dll_client is
begin
	Ada.Debug.Put (System.Program.Full_Name);
	Ada.Debug.Put (System.Storage_Elements.Formatting.Image (System.Program.Load_Address));
	use_zlib : declare
		zlib : System.Program.Dynamic_Linking.Library;
	begin
		begin
			System.Program.Dynamic_Linking.Open (zlib, "libz.so");
			pragma Debug (Ada.Debug.Put ("in BSD or Linux"));
		exception
			when Ada.IO_Exceptions.Name_Error =>
				begin
					System.Program.Dynamic_Linking.Open (zlib, "libz.dylib");
					pragma Debug (Ada.Debug.Put ("in Darwin"));
				exception
					when Ada.IO_Exceptions.Name_Error =>
						System.Program.Dynamic_Linking.Open (zlib, "libz.dll");
						pragma Debug (Ada.Debug.Put ("in Windows"));
				end;
		end;
		declare
			function zlibVersion return Interfaces.C.Strings.const_chars_ptr;
			pragma Import (C, zlibVersion);
			for zlibVersion'Address use
				System.Program.Dynamic_Linking.Import (zlib, "zlibVersion");
		begin
			Ada.Debug.Put (Interfaces.C.Strings.Value (zlibVersion));
		end;
	end use_zlib;
	pragma Debug (Ada.Debug.Put ("OK"));
end dll_client;
