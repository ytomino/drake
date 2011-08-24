with Ada.Tags;
with Ada.Finalization;
procedure tagged5 is
	use type Ada.Tags.Tag;
	type T is tagged null record;
begin
	Ada.Debug.Put (T'External_Tag);
	pragma Assert (Ada.Tags.Internal_Tag (T'External_Tag) = T'Tag);
	Ada.Debug.Put (Ada.Finalization.Controlled'External_Tag);
	pragma Assert (Ada.Tags.Internal_Tag (Ada.Finalization.Controlled'External_Tag) = Ada.Finalization.Controlled'Tag);
	Ada.Debug.Put (Ada.Finalization.Limited_Controlled'External_Tag);
	pragma Assert (Ada.Tags.Internal_Tag (Ada.Finalization.Limited_Controlled'External_Tag) = Ada.Finalization.Limited_Controlled'Tag);
	pragma Debug (Ada.Debug.Put ("OK"));
end tagged5;
