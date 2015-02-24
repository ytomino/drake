with Ada.Exceptions;
procedure exception6 is
	S : Ada.Exceptions.Exception_Occurrence;
begin
	-- Save_Exception
	Ada.Exceptions.Save_Exception (S, Constraint_Error'Identity, Message => "save!");
	Ada.Debug.Put ("(1)");
	begin
		Ada.Exceptions.Reraise_Occurrence (S);
		raise Program_Error;
	exception
		when X : Constraint_Error =>
			Ada.Debug.Put (Ada.Exceptions.Exception_Information (X));
			pragma Assert (Ada.Exceptions.Exception_Message (X) = "save!");
			null;
	end;
	-- Save_Exception_From_Here
	Ada.Exceptions.Save_Exception_From_Here (S, Tasking_Error'Identity); -- line 18
	Ada.Debug.Put ("(2)");
	begin
		Ada.Exceptions.Reraise_Occurrence (S);
		raise Program_Error;
	exception
		when X : Tasking_Error =>
			Ada.Debug.Put (Ada.Exceptions.Exception_Information (X));
			pragma Assert (Ada.Exceptions.Exception_Message (X) = "exception6.adb:18: explicit raise");
			null;
	end;
	pragma Debug (Ada.Debug.Put ("OK"));
end exception6;
