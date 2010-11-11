with Ada.Tags;
procedure tagged1 is
	use type Ada.Tags.Tag;
	use type Ada.Tags.Tag_Array;
	type T is tagged null record;
	type D is new T with null record;
	type I is interface;
	type DI is new T and I with null record;
	type L is tagged limited null record;
	type LD is new L with null record;
	type LI is limited interface;
	type LDI is new L and LI with null record;
begin
	Ada.Debug.Put (Ada.Tags.Expanded_Name (LDI'Tag));
	Ada.Debug.Put (Ada.Tags.External_Tag (LDI'Tag));
	pragma Assert (Ada.Tags.Parent_Tag (T'Tag) = Ada.Tags.No_Tag);
	pragma Assert (Ada.Tags.Parent_Tag (D'Tag) = T'Tag);
	pragma Assert (Ada.Tags.Interface_Ancestor_Tags (D'Tag)'Length = 0);
	pragma Assert (Ada.Tags.Interface_Ancestor_Tags (DI'Tag) = (1 => I'Tag));
	pragma Assert (Ada.Tags.Is_Descendant_At_Same_Level (D'Tag, T'Tag));
	pragma Assert (Ada.Tags.Is_Descendant_At_Same_Level (D'Tag, D'Tag));
	pragma Assert (Ada.Tags.Is_Descendant_At_Same_Level (DI'Tag, I'Tag));
	declare
		Obj : aliased DI;
		Ref : access T'Class := Obj'Access;
		IRef : access I'Class := Obj'Access;
	begin
		pragma Assert (Ref.all in DI'Class); -- resolved inline (no runtime)
		pragma Assert (Ref.all in I'Class); -- use IW_Membership
		pragma Assert (IRef'Tag = DI'Tag);
		declare
			type IA is access all I'Class;
			IRef2 : access I'Class := IA (Ref);
		begin
			null;
		end;
	end;
	Ada.Debug.Put ("ok");
end tagged1;
