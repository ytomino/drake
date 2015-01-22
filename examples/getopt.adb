pragma Ada_2012;
with Ada.Command_Line.Generic_Parsing;
with Ada.Command_Line.Parsing;
with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Strings.Unbounded;
procedure getopt is
	use type Ada.Strings.Unbounded.Unbounded_String;
	package Lists is new Ada.Containers.Indefinite_Doubly_Linked_Lists (String);
	type Option_Type is record
		a, b : Natural := 0;
		o : Ada.Strings.Unbounded.Unbounded_String := Ada.Strings.Unbounded.Null_Unbounded_String;
		long : Natural := 0;
		prefix : Ada.Strings.Unbounded.Unbounded_String := Ada.Strings.Unbounded.Null_Unbounded_String;
		to : Ada.Strings.Unbounded.Unbounded_String := Ada.Strings.Unbounded.Null_Unbounded_String;
		verbose : Natural := 0;
		Unknown : Lists.List := Lists.Empty_List;
		Arguments : Lists.List := Lists.Empty_List;
	end record;
	-- parse arguments
	procedure Test_Parsing (Arguments : aliased Lists.List; Option : out Option_Type) is
		package Parsing is
			new Ada.Command_Line.Generic_Parsing (
				Input_Cursor => Lists.Cursor,
				Has_Element => Lists.Has_Element,
				Input_Iterator_Interfaces => Lists.List_Iterator_Interfaces,
				Input_Iterator => Arguments.Iterate,
				Argument => Lists.Element);
	begin
		for I in Parsing.Iterate loop
			if Parsing.Is_Option (I, 'a') then
				pragma Assert (Parsing.Name (I) = "-a");
				pragma Assert (Parsing.Short_Name (I) = 'a');
				pragma Assert (Parsing.Value (I) = "");
				Option.a := Option.a + 1;
			elsif Parsing.Is_Option (I, 'b') then
				pragma Assert (Parsing.Name (I) = "-b");
				pragma Assert (Parsing.Short_Name (I) = 'b');
				pragma Assert (Parsing.Value (I) = "");
				Option.b := Option.b + 1;
			elsif Parsing.Is_Option (I, 'o', ':') then
				pragma Assert (Parsing.Name (I) = "-o");
				pragma Assert (Parsing.Short_Name (I) = 'o');
				Option.o := +Parsing.Value (I);
			elsif Parsing.Is_Option (I, "long") then
				pragma Assert (Parsing.Name (I) = "--long");
				pragma Assert (Parsing.Long_Name (I) = "long");
				pragma Assert (Parsing.Value (I) = "");
				Option.long := Option.long + 1;
			elsif Parsing.Is_Option (I, "prefix", '?') then
				pragma Assert (Parsing.Name (I) = "--prefix");
				Option.prefix := +Parsing.Value (I);
			elsif Parsing.Is_Option (I, "to", ':') then
				pragma Assert (Parsing.Name (I) = "--to");
				Option.to := +Parsing.Value (I);
			elsif Parsing.Is_Option (I, 'v', "verbose") then
				pragma Assert (Parsing.Name (I) = "-v"
					or else Parsing.Name (I) = "--verbose");
				pragma Assert (Parsing.Short_Name (I) = 'v'
					or else Parsing.Long_Name (I) = "verbose");
				pragma Assert (Parsing.Value (I) = "");
				Option.verbose := Option.verbose + 1;
			elsif Parsing.Is_Unknown_Option (I) then
				pragma Assert (Parsing.Long_Name (I) /= "long"
					or else Parsing.Value (I) = "value"); -- *A*
				Lists.Append (Option.Unknown, Parsing.Name (I));
			else
				Lists.Append (Option.Arguments, Parsing.Argument (I));
			end if;
		end loop;
	end Test_Parsing;
begin
	-- empty
	declare
		Arguments : aliased Lists.List := Lists.Empty_List;
	begin
		declare
			Option : Option_Type;
		begin
			Test_Parsing (Arguments, Option);
		end;
	end;
	-- arguments
	declare
		Arguments : aliased Lists.List := Lists.Empty_List;
	begin
		Lists.Append (Arguments, "a.txt");
		Lists.Append (Arguments, "b.txt");
		declare
			Option : Option_Type;
		begin
			Test_Parsing (Arguments, Option);
			pragma Assert (Option.Arguments.Length = 2);
			pragma Assert (Option.Arguments.Constant_Reference (Option.Arguments.First) = "a.txt");
			pragma Assert (Option.Arguments.Constant_Reference (Option.Arguments.Last) = "b.txt");
		end;
	end;
	-- double hyphen at first
	declare
		Arguments : aliased Lists.List := Lists.Empty_List;
	begin
		Lists.Append (Arguments, "--");
		Lists.Append (Arguments, "-a.txt");
		Lists.Append (Arguments, "--b.txt");
		declare
			Option : Option_Type;
		begin
			Test_Parsing (Arguments, Option);
			pragma Assert (Option.Arguments.Length = 2);
			pragma Assert (Option.Arguments.Constant_Reference (Option.Arguments.First) = "-a.txt");
			pragma Assert (Option.Arguments.Constant_Reference (Option.Arguments.Last) = "--b.txt");
		end;
	end;
	-- parameterless options
	declare
		Arguments : aliased Lists.List := Lists.Empty_List;
	begin
		Lists.Append (Arguments, "-ab");
		Lists.Append (Arguments, "--long");
		Lists.Append (Arguments, "-v");
		Lists.Append (Arguments, "--verbose");
		Lists.Append (Arguments, "a.txt");
		Lists.Append (Arguments, "--");
		Lists.Append (Arguments, "-b.txt");
		declare
			Option : Option_Type;
		begin
			Test_Parsing (Arguments, Option);
			pragma Assert (Option.a = 1);
			pragma Assert (Option.b = 1);
			pragma Assert (Option.long = 1);
			pragma Assert (Option.verbose = 2);
			pragma Assert (Option.Arguments.Length = 2);
			pragma Assert (Option.Arguments.Constant_Reference (Option.Arguments.First) = "a.txt");
			pragma Assert (Option.Arguments.Constant_Reference (Option.Arguments.Last) = "-b.txt");
		end;
	end;
	-- values without space
	declare
		Arguments : aliased Lists.List := Lists.Empty_List;
	begin
		Lists.Append (Arguments, "-oa.out");
		Lists.Append (Arguments, "--prefix=/usr/local");
		Lists.Append (Arguments, "--to=utf-8");
		declare
			Option : Option_Type;
		begin
			Test_Parsing (Arguments, Option);
			pragma Assert (Option.o = "a.out");
			pragma Assert (Option.prefix = "/usr/local");
			pragma Assert (Option.to = "utf-8");
			pragma Assert (Option.Arguments.Length = 0);
		end;
	end;
	-- values with space
	declare
		Arguments : aliased Lists.List := Lists.Empty_List;
	begin
		Lists.Append (Arguments, "-o");
		Lists.Append (Arguments, "a.out");
		Lists.Append (Arguments, "--prefix");
		Lists.Append (Arguments, "/usr/local");
		Lists.Append (Arguments, "--to");
		Lists.Append (Arguments, "utf-8");
		declare
			Option : Option_Type;
		begin
			Test_Parsing (Arguments, Option);
			pragma Assert (Option.o = "a.out");
			pragma Assert (Option.to = "utf-8");
			pragma Assert (Option.prefix = "");
			pragma Assert (Option.Arguments.Length = 1);
			pragma Assert (Option.Arguments.Constant_Reference (Option.Arguments.First) = "/usr/local");
		end;
	end;
	-- missing values by end-of-arguments (short)
	declare
		Arguments : aliased Lists.List := Lists.Empty_List;
	begin
		Lists.Append (Arguments, "-t");
		declare
			Option : Option_Type;
		begin
			Test_Parsing (Arguments, Option);
			pragma Assert (Option.to = "");
			pragma Assert (Option.Arguments.Length = 0);
		end;
	end;
	-- missing values by end-of-arguments (long)
	declare
		Arguments : aliased Lists.List := Lists.Empty_List;
	begin
		Lists.Append (Arguments, "--to");
		declare
			Option : Option_Type;
		begin
			Test_Parsing (Arguments, Option);
			pragma Assert (Option.to = "");
			pragma Assert (Option.Arguments.Length = 0);
		end;
	end;
	-- unknown options
	declare
		Arguments : aliased Lists.List := Lists.Empty_List;
	begin
		Lists.Append (Arguments, "-u");
		Lists.Append (Arguments, "--long=value"); -- *A*
		Lists.Append (Arguments, "--unknown");
		Lists.Append (Arguments, "--unknown=xyz");
		declare
			Option : Option_Type;
		begin
			Test_Parsing (Arguments, Option);
			pragma Assert (Option.to = "");
			pragma Assert (Option.Unknown.Length = 4);
			pragma Assert (Option.Unknown.Constant_Reference (Option.Unknown.First) = "-u");
			pragma Assert (Option.Unknown.Constant_Reference (Lists.Next (Option.Unknown.First)) = "--long");
			pragma Assert (Option.Unknown.Constant_Reference (Lists.Next (Lists.Next (Option.Unknown.First))) = "--unknown");
			pragma Assert (Option.Unknown.Constant_Reference (Option.Unknown.Last) = "--unknown");
			pragma Assert (Option.Arguments.Length = 0);
		end;
	end;
	pragma Debug (Ada.Debug.Put ("OK"));
end getopt;
