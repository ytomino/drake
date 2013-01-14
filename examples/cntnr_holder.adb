with Ada.Containers.Indefinite_Holders;
with Ada.Streams.Buffer_Storage_IO;
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
begin
	Stream_Test : declare
		package BSIO renames Ada.Streams.Buffer_Storage_IO;
		X : String_Holders.Holder;
		Buffer : Ada.Streams.Buffer_Storage_IO.Buffer;
	begin
		-- Holder -> Raw
		String_Holders.Replace_Element (X, "");
		String_Holders.Holder'Write (BSIO.Stream (Buffer), X);
		Ada.Streams.Set_Index (Ada.Streams.Seekable_Stream_Type'Class (BSIO.Stream (Buffer).all), 1);
		declare
			S : constant String := String'Input (BSIO.Stream (Buffer));
		begin
			pragma Assert (S = "");
			null;
		end;
		-- Raw -> Holder
		Ada.Streams.Set_Index (Ada.Streams.Seekable_Stream_Type'Class (BSIO.Stream (Buffer).all), 1);
		String'Output (BSIO.Stream (Buffer), "XYZ");
		Ada.Streams.Set_Index (Ada.Streams.Seekable_Stream_Type'Class (BSIO.Stream (Buffer).all), 1);
		String_Holders.Holder'Read (BSIO.Stream (Buffer), X);
		pragma Assert (X.Element = "XYZ");
	end Stream_Test;
	pragma Debug (Ada.Debug.Put ("OK"));
end cntnr_holder;
