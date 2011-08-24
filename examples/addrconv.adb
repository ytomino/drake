with Ada;
with System.Address_To_Access_Conversions;
with System.Address_To_Named_Access_Conversions;
with System.Address_To_Constant_Access_Conversions;
procedure addrconv is
	package AC1 is new System.Address_To_Access_Conversions (Integer);
	type TA is access all Integer;
	pragma No_Strict_Aliasing (TA);
	package AC2 is new System.Address_To_Named_Access_Conversions (Integer, TA);
	type TC is access constant Integer;
	pragma No_Strict_Aliasing (TC);
	package AC3 is new System.Address_To_Constant_Access_Conversions (Integer, TC);
	V : aliased Integer;
	V1 : AC1.Object_Pointer := AC1.To_Pointer (V'Address);
	V2 : TA := AC2.To_Pointer (V'Address);
	V3 : TC := AC3.To_Pointer (V'Address);
	pragma Suppress (Access_Check);
begin
	V := 10;
	V1.all := V1.all + 10;
	V2.all := V2.all + 10;
	pragma Assert (V3.all = 30);
	pragma Debug (Ada.Debug.Put ("OK"));
end addrconv;
