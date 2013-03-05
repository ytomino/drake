with Ada;
procedure protected1 is
	protected RW is
		procedure Sync_Proc;
		function Sync_Func return Integer;
	end RW;
	protected body RW is
		procedure Sync_Proc is
		begin
			Ada.Debug.Put ("Ahaha");
		end Sync_Proc;
		function Sync_Func return Integer is
		begin
			Ada.Debug.Put ("Ufufu");
			return 0;
		end Sync_Func;
	end RW;
	Dummy : Integer;
begin
	RW.Sync_Proc;
	Dummy := RW.Sync_Func;
	pragma Debug (Ada.Debug.Put ("OK"));
end protected1;
