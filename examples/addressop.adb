with Ada;
with System.Storage_Elements;
procedure addressop is
	use type System.Address;
	use type System.Storage_Elements.Storage_Offset;
	X : System.Address := System'To_Address (100);
begin
	pragma Assert (X + System.Storage_Elements.Storage_Offset'(10) = System'To_Address (110));
	pragma Assert (X + System.Storage_Elements.Storage_Offset'(-10) = System'To_Address (90));
	pragma Assert (System.Storage_Elements.Storage_Offset'(10) + X = System'To_Address (110));
	pragma Assert (System.Storage_Elements.Storage_Offset'(-10) + X = System'To_Address (90));
	pragma Assert (X - System.Storage_Elements.Storage_Offset'(10) = System'To_Address (90));
	pragma Assert (X - System.Storage_Elements.Storage_Offset'(-10) = System'To_Address (110));
	pragma Assert (X - System'To_Address (90) = System.Storage_Elements.Storage_Offset'(10));
	pragma Debug (Ada.Debug.Put ("OK"));
	null;
end addressop;
