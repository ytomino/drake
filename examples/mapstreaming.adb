with Ada.Containers.Hashed_Maps;
with Ada.Containers.Ordered_Maps;
with Ada.Streams.Unbounded_Storage_IO;
with Ada.Strings.Maps.Constants;
procedure mapstreaming is
	use type Ada.Containers.Count_Type;
	use type Ada.Streams.Stream_Element_Offset;
	use type Ada.Strings.Maps.Character_Mapping;
	Source : constant Ada.Strings.Maps.Character_Mapping :=
		Ada.Strings.Maps.Constants.Case_Folding_Map;
	Source_Domain : constant Wide_Wide_String :=
		Ada.Strings.Maps.Overloaded_To_Domain (Source);
	Source_Length : constant Ada.Containers.Count_Type := Source_Domain'Length;
	Buffer : Ada.Streams.Unbounded_Storage_IO.Buffer_Type;
begin
	Ada.Strings.Maps.Character_Mapping'Write (
		Ada.Streams.Unbounded_Storage_IO.Stream (Buffer),
		Source);
	-- Character_Mapping
	Ada.Streams.Unbounded_Storage_IO.Reset (Buffer);
	declare
		X : Ada.Strings.Maps.Character_Mapping;
	begin
		Ada.Strings.Maps.Character_Mapping'Read (
			Ada.Streams.Unbounded_Storage_IO.Stream (Buffer),
			X);
		pragma Assert (
			Ada.Streams.Index (
				Ada.Streams.Seekable_Stream_Type'Class (
					Ada.Streams.Unbounded_Storage_IO.Stream (Buffer).all)) =
			Ada.Streams.Unbounded_Storage_IO.Size (Buffer) + 1);
		pragma Assert (X = Source);
	end;
	-- Ordered_Maps
	Ada.Streams.Unbounded_Storage_IO.Reset (Buffer);
	declare
		package Maps is
			new Ada.Containers.Ordered_Maps (
				Wide_Wide_Character,
				Wide_Wide_Character);
		X : Maps.Map;
	begin
		Maps.Map'Read (
			Ada.Streams.Unbounded_Storage_IO.Stream (Buffer),
			X);
		pragma Assert (
			Ada.Streams.Index (
				Ada.Streams.Seekable_Stream_Type'Class (
					Ada.Streams.Unbounded_Storage_IO.Stream (Buffer).all)) =
			Ada.Streams.Unbounded_Storage_IO.Size (Buffer) + 1);
		pragma Assert (X.Length = Source_Length);
		for I in X.Iterate loop
			pragma Assert (
				Ada.Strings.Maps.Overloaded_Value (Source, Maps.Key (I)) =
				Maps.Element (I));
			null;
		end loop;
	end;
	-- Hashed_Maps
	Ada.Streams.Unbounded_Storage_IO.Reset (Buffer);
	declare
		function Hash (Item : Wide_Wide_Character) return Ada.Containers.Hash_Type is
		begin
			return Wide_Wide_Character'Pos (Item);
		end Hash;
		package Maps is
			new Ada.Containers.Hashed_Maps (
				Wide_Wide_Character,
				Wide_Wide_Character,
				Hash => Hash,
				Equivalent_Keys => "=");
		X : Maps.Map;
	begin
		Maps.Map'Read (
			Ada.Streams.Unbounded_Storage_IO.Stream (Buffer),
			X);
		pragma Assert (
			Ada.Streams.Index (
				Ada.Streams.Seekable_Stream_Type'Class (
					Ada.Streams.Unbounded_Storage_IO.Stream (Buffer).all)) =
			Ada.Streams.Unbounded_Storage_IO.Size (Buffer) + 1);
		pragma Assert (X.Length = Source_Length);
		for I in X.Iterate loop
			pragma Assert (
				Ada.Strings.Maps.Overloaded_Value (Source, Maps.Key (I)) =
				Maps.Element (I));
			null;
		end loop;
	end;
	pragma Debug (Ada.Debug.Put ("OK"));
end mapstreaming;
