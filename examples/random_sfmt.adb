--  translated unit from SFMT (test.c)
--
--   Copyright (C) 2007 Mutsuo Saito, Makoto Matsumoto and Hiroshima
--   University. All rights reserved.
--
--   The new BSD License is applied to this software, see LICENSE.txt
--
--
--   Ada version by yt
--
with Ada.Command_Line;
with Ada.Execution_Time;
with Ada.Formatting;
with Ada.Integer_Text_IO;
with Ada.Real_Time;
with Ada.Text_IO;
-- all instances of Ada.Numerics.SFMT.Random
with Ada.Numerics.SFMT.Params_19937;
with Ada.Numerics.SFMT.Params_216091;
procedure random_sfmt is
	use Ada.Numerics.SFMT.Params_19937;
	-- use Ada.Numerics.SFMT.Params_216091;
	use type Ada.Execution_Time.CPU_Time;
	use type Ada.Real_Time.Time_Span;
	use type Unsigned_32;
	use type Unsigned_64;
	
	package Unsigned_32_IO is new Ada.Text_IO.Modular_IO (Unsigned_32);
	package Unsigned_64_IO is new Ada.Text_IO.Modular_IO (Unsigned_64);
	
	function Hex_Image is new Ada.Formatting.Modular_Image (
		Unsigned_32,
		Form => Ada.Formatting.Simple,
		Signs => Ada.Formatting.Triming_Unsign_Marks,
		Base => 16,
		Set => Ada.Formatting.Lower_Case,
		Width => 8);
	function Hex_Image is new Ada.Formatting.Modular_Image (
		Unsigned_64,
		Form => Ada.Formatting.Simple,
		Signs => Ada.Formatting.Triming_Unsign_Marks,
		Base => 16,
		Set => Ada.Formatting.Lower_Case,
		Width => 8);
	
	Gen : aliased Generator;
	
	BLOCK_SIZE : constant := 100000;
	BLOCK_SIZE64 : constant := 50000;
	COUNT : constant := 1000;
	
	procedure check32;
	procedure speed32;
	procedure check64;
	procedure speed64;
	
	array1 : Unsigned_64_Array (0 .. BLOCK_SIZE / 4 * 2 - 1);
	array2 : Unsigned_64_Array (0 .. 10000 / 4 * 2 - 1);
	
	procedure check32 is
		array32 : Unsigned_32_Array (0 .. BLOCK_SIZE - 1);
		for array32'Address use array1'Address;
		pragma Compile_Time_Error (array32'Size /= array1'Size, "bad array32");
		array32_2 : Unsigned_32_Array (0 .. 10000 - 1);
		for array32_2'Address use array2'Address;
		pragma Compile_Time_Error (array32_2'Size /= array2'Size, "bad array32_2");
		ini : constant Unsigned_32_Array :=
			(16#1234#, 16#5678#, 16#9abc#, 16#def0#);
		r32 : Unsigned_32;
	begin
		if Min_Array_Length_32 > 10000 then
			raise Program_Error with "array size too small!";
		end if;
		Ada.Text_IO.Put_Line (Id);
		Ada.Text_IO.Put_Line ("32 bit generated randoms");
		Ada.Text_IO.Put_Line ("init_gen_rand__________");
		-- 32 bit generation
		Reset (Gen, 1234);
		Fill_Random_32 (Gen, array32 (0 .. 10000 - 1));
		Fill_Random_32 (Gen, array32_2 (0 .. 10000 - 1));
		Reset (Gen, 1234);
		for i in 0 .. 10000 - 1 loop
			if i < 1000 then
				Unsigned_32_IO.Put (array32 (i), Width => 10);
				Ada.Text_IO.Put (' ');
				if i rem 5 = 4 then
					Ada.Text_IO.New_Line;
				end if;
			end if;
			r32 := Random_32 (Gen);
			if r32 /= array32 (i) then
				raise Program_Error with "mismatch at" & Integer'Image (i)
					& " array32:" & Hex_Image (array32 (i))
					& " gen:" & Hex_Image (r32);
			end if;
		end loop;
		for i in 0 .. 700 - 1 loop
			r32 := Random_32 (Gen);
			if r32 /= array32_2 (i) then
				raise Program_Error with "mismatch at" & Integer'Image (I)
					& " array32_2:" & Hex_Image (array32_2 (i))
					& " gen:" & Hex_Image (r32);
			end if;
		end loop;
		Ada.Text_IO.New_Line;
		Reset (Gen, Initialize (ini));
		Ada.Text_IO.Put_Line ("init_by_array__________");
		Fill_Random_32 (Gen, array32 (0 .. 10000 - 1));
		Fill_Random_32 (Gen, array32_2 (0 .. 10000 - 1));
		Reset (Gen, Initialize (ini));
		for i in 0 .. 10000 - 1 loop
			if i < 1000 then
				Unsigned_32_IO.Put (array32 (i), Width => 10);
				Ada.Text_IO.Put (' ');
				if i rem 5 = 4 then
					Ada.Text_IO.New_Line;
				end if;
			end if;
			r32 := Random_32 (Gen);
			if r32 /= array32 (i) then
				raise Program_Error with "mismatch at" & Integer'Image (I)
					& " array32:" & Hex_Image (array32 (i))
					& " gen:" & Hex_Image (r32);
			end if;
		end loop;
		for i in 0 .. 700 - 1 loop
			r32 := Random_32 (Gen);
			if r32 /= array32_2 (i) then
				raise Program_Error with "mismatch at" & Integer'Image (I)
					& " array32_2:" & Hex_Image (array32_2 (i))
					& " gen:" & Hex_Image (r32);
			end if;
		end loop;
	end check32;
	
	procedure speed32 is
		clo : Ada.Execution_Time.CPU_Time;
		min : Ada.Real_Time.Time_Span := Ada.Real_Time.Time_Span_Last;
		array32 : Unsigned_32_Array (0 .. BLOCK_SIZE - 1);
		for array32'Address use array1'Address;
		pragma Compile_Time_Error (array32'Size /= array1'Size, "bad array32");
		clo_Span : Ada.Real_Time.Time_Span;
		Dummy : Unsigned_32;
		pragma Unreferenced (Dummy);
	begin
		if Min_Array_Length_32 > BLOCK_SIZE then
			raise Program_Error with "array size too small!";
		end if;
		-- 32 bit generation
		Reset (Gen, 1234);
		for i in 0 .. 10 - 1 loop
			clo := Ada.Execution_Time.Clock;
			for j in 0 .. COUNT - 1 loop
				Fill_Random_32 (Gen, array32 (0 .. BLOCK_SIZE - 1));
			end loop;
			clo_Span := Ada.Execution_Time.Clock - clo;
			if clo_Span < min then
				min := clo_Span;
			end if;
		end loop;
		Ada.Text_IO.Put ("32 bit BLOCK:");
		Ada.Integer_Text_IO.Put (Integer (Ada.Real_Time.To_Duration (min) * 1000), Width => 1);
		Ada.Text_IO.Put ("ms for ");
		Ada.Integer_Text_IO.Put (BLOCK_SIZE * COUNT, Width => 0);
		Ada.Text_IO.Put (" randoms generation");
		Ada.Text_IO.New_Line;
		min := Ada.Real_Time.Time_Span_Last;
		Reset (Gen, 1234);
		for i in 0 .. 10 - 1 loop
			clo := Ada.Execution_Time.Clock;
			for j in 0 .. BLOCK_SIZE * COUNT - 1 loop
				Dummy := Random_32 (Gen);
			end loop;
			clo_Span:= Ada.Execution_Time.Clock - clo;
			if clo_Span < min then
				min := clo_Span;
			end if;
		end loop;
		Ada.Text_IO.Put ("32 bit SEQUE:");
		Ada.Integer_Text_IO.Put (Integer (Ada.Real_Time.To_Duration (min) * 1000), Width => 1);
		Ada.Text_IO.Put ("ms for ");
		Ada.Integer_Text_IO.Put (BLOCK_SIZE * COUNT, Width => 0);
		Ada.Text_IO.Put (" randoms generation");
		Ada.Text_IO.New_Line;
	end speed32;
	
	procedure check64 is
		array64 : Unsigned_64_Array renames array1;
		array64_2 : Unsigned_64_Array renames array2;
		r : Unsigned_64;
		ini : constant Unsigned_32_Array := (5, 4, 3, 2, 1);
	begin
		if Min_Array_Length_64 > 5000 then
			raise Program_Error with "array size too small!";
		end if;
		Ada.Text_IO.Put_Line (Id);
		Ada.Text_IO.Put_Line ("64 bit generated randoms");
		Ada.Text_IO.Put_Line ("init_gen_rand__________");
		-- 64 bit generation
		Reset (Gen, 4321);
		Fill_Random_64 (Gen, array64 (0 .. 5000 - 1));
		Fill_Random_64 (Gen, array64_2 (0 .. 5000 - 1));
		Reset (Gen, 4321);
		for i in 0 .. 5000 - 1 loop
			if i < 1000 then
				Unsigned_64_IO.Put (array64 (i), Width => 20);
				Ada.Text_IO.Put (' ');
				if i rem 3 = 2 then
					Ada.Text_IO.New_Line;
				end if;
			end if;
			r := Random_64 (Gen);
			if r /= array64 (i) then
				raise Program_Error with "mismatch at" & Integer'Image (I)
					& " array64:" & Hex_Image (array64 (i))
					& " gen:" & Hex_Image (r);
			end if;
		end loop;
		Ada.Text_IO.New_Line;
		for i in 0 .. 700 - 1 loop
			r := Random_64 (Gen);
			if r /= array64_2 (i) then
				raise Program_Error with "mismatch at" & Integer'Image (I)
					& " array64_2:" & Hex_Image (array64_2 (i))
					& " gen:" & Hex_Image (r);
			end if;
		end loop;
		Ada.Text_IO.Put_Line ("init_by_array__________");
		-- 64 bit generation
		Reset (Gen, Initialize (ini));
		Fill_Random_64 (Gen, array64 (0 .. 5000 - 1));
		Fill_Random_64 (Gen, array64_2 (0 .. 5000 - 1));
		Reset (Gen, Initialize (ini));
		for i in 0 .. 5000 - 1 loop
			if i < 1000 then
				Unsigned_64_IO.Put (array64 (i), Width => 20);
				Ada.Text_IO.Put (' ');
				if i rem 3 = 2 then
					Ada.Text_IO.New_Line;
				end if;
			end if;
			r := Random_64 (Gen);
			if r /= array64 (i) then
				raise Program_Error with "mismatch at" & Integer'Image (I)
					& " array64:" & Hex_Image (array64 (i))
					& " gen:" & Hex_Image (r);
			end if;
		end loop;
		Ada.Text_IO.New_Line;
		for i in 0 .. 700 - 1 loop
			r := Random_64 (Gen);
			if r /= array64_2 (i) then
				raise Program_Error with "mismatch at" & Integer'Image (I)
					& " array64_2:" & Hex_Image (array64_2 (i))
					& " gen:" & Hex_Image (r);
			end if;
		end loop;
	end check64;
	
	procedure speed64 is
		clo : Ada.Execution_Time.CPU_Time;
		min : Ada.Real_Time.Time_Span := Ada.Real_Time.Time_Span_Last;
		array64 : Unsigned_64_Array renames array1;
		clo_Span : Ada.Real_Time.Time_Span;
		Dummy : Unsigned_64;
		pragma Unreferenced (Dummy);
	begin
		if Min_Array_Length_64 > BLOCK_SIZE64 then
			raise Program_Error with "array size too small!";
		end if;
		-- 64 bit generation
		Reset (Gen, 1234);
		for i in 0 .. 10 - 1 loop
			clo := Ada.Execution_Time.Clock;
			for j in 0 .. COUNT - 1 loop
				Fill_Random_64 (Gen, array64 (0 .. BLOCK_SIZE64 - 1));
			end loop;
			clo_Span := Ada.Execution_Time.Clock - clo;
			if clo_Span < min then
				min := clo_Span;
			end if;
		end loop;
		Ada.Text_IO.Put ("64 bit BLOCK:");
		Ada.Integer_Text_IO.Put (Integer (Ada.Real_Time.To_Duration (min) * 1000), Width => 1);
		Ada.Text_IO.Put ("ms for ");
		Ada.Integer_Text_IO.Put (BLOCK_SIZE64 * COUNT, Width => 0);
		Ada.Text_IO.Put (" randoms generation");
		Ada.Text_IO.New_Line;
		min := Ada.Real_Time.Time_Span_Last;
		Reset (Gen, 1234);
		for i in 0 .. 10 - 1 loop
			clo := Ada.Execution_Time.Clock;
			for j in 0 .. BLOCK_SIZE64 * COUNT - 1 loop
				Dummy := Random_64 (Gen);
			end loop;
			clo_Span := Ada.Execution_Time.Clock - clo;
			if clo_Span < min then
				min := clo_Span;
			end if;
		end loop;
		Ada.Text_IO.Put ("64 bit SEQUE:");
		Ada.Integer_Text_IO.Put (Integer (Ada.Real_Time.To_Duration (min) * 1000), Width => 1);
		Ada.Text_IO.Put ("ms for ");
		Ada.Integer_Text_IO.Put (BLOCK_SIZE64 * COUNT, Width => 0);
		Ada.Text_IO.Put (" randoms generation");
		Ada.Text_IO.New_Line;
	end speed64;
	
	speed : Boolean := False;
	bit32 : Boolean := False;
	bit64 : Boolean := False;
begin
	for i in 1 .. Ada.Command_Line.Argument_Count loop
		declare
			argv_i : constant String := Ada.Command_Line.Argument (i);
		begin
			if argv_i = "-s" then
				speed := True;
			end if;
			if argv_i = "-b32" then
				bit32 := True;
			end if;
			if argv_i = "-b64" then
				bit64 := True;
			end if;
		end;
	end loop;
	if not (speed or else bit32 or else bit64) then
		Ada.Text_IO.Put_Line ("usage:");
		Ada.Text_IO.Put_Line (
			Ada.Command_Line.Command_Name
			& " [-s | -b32 | -b64]");
		return;
	end if;
	if speed then
		speed32;
		speed64;
	end if;
	if bit32 then
		check32;
	end if;
	if bit64 then
		check64;
	end if;
end random_sfmt;
