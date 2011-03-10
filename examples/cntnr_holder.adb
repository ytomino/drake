with Ada.Containers.Indefinite_Holders;
procedure cntnr_holder is
	package String_Holders is new Ada.Containers.Indefinite_Holders (String);
	procedure Test_1 is
		X : aliased String_Holders.Holder := String_Holders.To_Holder ("123");
	begin
		pragma Assert (X.Constant_Reference.Element.all = "123");
		String_Holders.Replace_Element (X, "ABCDEFG"); -- changing constraints
		pragma Assert (X.Reference.Element.all = "ABCDEFG");
		null;
	end Test_1;
	pragma Debug (Test_1);
	pragma Debug (Ada.Debug.Put ("OK"));
begin
	 null;
end cntnr_holder;
