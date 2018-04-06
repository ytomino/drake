pragma License (Unrestricted); -- BSD 3-Clause
--  translated unit from dSFMT (test.c)
--
--   Copyright (c) 2007, 2008, 2009 Mutsuo Saito, Makoto Matsumoto
--   and Hiroshima University.
--   Copyright (c) 2011, 2002 Mutsuo Saito, Makoto Matsumoto, Hiroshima
--   University and The University of Tokyo.
--   All rights reserved.
--
--
--   Ada version by yt
--
with Ada.Command_Line;
with Ada.Execution_Time;
with Ada.Formatting;
with Ada.Integer_Text_IO;
with Ada.Long_Float_Text_IO;
with Ada.Numerics.dSFMT_19937;
-- with Ada.Numerics.dSFMT_216091;
with Ada.Real_Time;
with Ada.Text_IO;
with Interfaces;
procedure random_dsfmt is
	use Ada.Numerics.dSFMT_19937;
	-- use Ada.Numerics.dSFMT_216091;
	use type Ada.Execution_Time.CPU_Time;
	use type Ada.Real_Time.Time_Span;
	use type Interfaces.Unsigned_64;
	
	exit_1 : exception;
	
	dsfmt_global_data : aliased Generator;
	DSFMT_N : constant Natural := (DSFMT_MEXP - 128) / 104 + 1;
	
	NUM_RANDS : constant := 50000;
	TIC_COUNT : constant := 2000;
	
	dummy : Long_Float_Array (
		0 .. (NUM_RANDS / 2 + 1) * 2 - 1); -- w128_t to Long_Float
	
	type genrand_t is access function return Long_Float;
	type st_genrand_t is
		access function (dsfmt : aliased in out Generator) return Long_Float;
	type fill_array_t is access procedure (the_array : out Long_Float_Array);
	type st_fill_array_t is
		access procedure (
			dsfmt : aliased in out Generator;
			the_array : out Long_Float_Array);
	
	procedure test_co;
	procedure test_oc;
	procedure test_oo;
	procedure test_12;
	procedure test_seq_co;
	procedure test_seq_oc;
	procedure test_seq_oo;
	procedure test_seq_12;
	
	pragma No_Inline (test_co);
	pragma No_Inline (test_oc);
	pragma No_Inline (test_oo);
	pragma No_Inline (test_12);
	pragma No_Inline (test_seq_co);
	pragma No_Inline (test_seq_oc);
	pragma No_Inline (test_seq_oo);
	pragma No_Inline (test_seq_12);
	
	procedure check (
		range_str : in String; -- start_mess
		genrand : in genrand_t;
		fill_array : in fill_array_t;
		st_genrand : in st_genrand_t;
		st_fill_array : in st_fill_array_t;
		seed : in Unsigned_32;
		print_size : in Integer); -- n
	procedure check_ar (
		range_str : in String; -- start_mess
		genrand : in genrand_t;
		fill_array : in fill_array_t;
		st_genrand : in st_genrand_t;
		st_fill_array : in st_fill_array_t;
		print_size : in Integer); -- n
	
	-- not inline wrapper functions for check()
	function s_genrand_close_open return Long_Float is
	begin
		return Random_0_To_Less_Than_1 (dsfmt_global_data);
	end s_genrand_close_open;
	function s_genrand_open_close return Long_Float is
	begin
		return Random_Greater_Than_0_To_1 (dsfmt_global_data);
	end s_genrand_open_close;
	function s_genrand_open_open return Long_Float is
	begin
		return Random_Greater_Than_0_To_Less_Than_1 (dsfmt_global_data);
	end s_genrand_open_open;
	function s_genrand_close1_open2 return Long_Float is
	begin
		return Random_1_To_Less_Than_2 (dsfmt_global_data);
	end s_genrand_close1_open2;
	function sst_genrand_close_open (dsfmt : aliased in out Generator)
		return Long_Float is
	begin
		return Random_0_To_Less_Than_1 (dsfmt);
	end sst_genrand_close_open;
	function sst_genrand_open_close (dsfmt : aliased in out Generator)
		return Long_Float is
	begin
		return Random_Greater_Than_0_To_1 (dsfmt);
	end sst_genrand_open_close;
	function sst_genrand_open_open (dsfmt : aliased in out Generator)
		return Long_Float is
	begin
		return Random_Greater_Than_0_To_Less_Than_1 (dsfmt);
	end sst_genrand_open_open;
	function sst_genrand_close1_open2 (dsfmt : aliased in out Generator)
		return Long_Float is
	begin
		return Random_1_To_Less_Than_2 (dsfmt);
	end sst_genrand_close1_open2;
	procedure s_fill_array_close_open (the_array : out Long_Float_Array) is
	begin
		Fill_Random_0_To_Less_Than_1 (dsfmt_global_data, the_array);
	end s_fill_array_close_open;
	procedure s_fill_array_open_close (the_array : out Long_Float_Array) is
	begin
		Fill_Random_Greater_Than_0_To_1 (dsfmt_global_data, the_array);
	end s_fill_array_open_close;
	procedure s_fill_array_open_open (the_array : out Long_Float_Array) is
	begin
		Fill_Random_Greater_Than_0_To_Less_Than_1 (dsfmt_global_data, the_array);
	end s_fill_array_open_open;
	procedure s_fill_array_close1_open2 (the_array : out Long_Float_Array) is
	begin
		Fill_Random_1_To_Less_Than_2 (dsfmt_global_data, the_array);
	end s_fill_array_close1_open2;
	procedure sst_fill_array_close_open (
		dsfmt : aliased in out Generator;
		the_array : out Long_Float_Array) is
	begin
		Fill_Random_0_To_Less_Than_1 (dsfmt, the_array);
	end sst_fill_array_close_open;
	procedure sst_fill_array_open_close (
		dsfmt : aliased in out Generator;
		the_array : out Long_Float_Array) is
	begin
		Fill_Random_Greater_Than_0_To_1 (dsfmt, the_array);
	end sst_fill_array_open_close;
	procedure sst_fill_array_open_open (
		dsfmt : aliased in out Generator;
		the_array : out Long_Float_Array) is
	begin
		Fill_Random_Greater_Than_0_To_Less_Than_1 (dsfmt, the_array);
	end sst_fill_array_open_open;
	procedure sst_fill_array_close1_open2 (
		dsfmt : aliased in out Generator;
		the_array : out Long_Float_Array) is
	begin
		Fill_Random_1_To_Less_Than_2 (dsfmt, the_array);
	end sst_fill_array_close1_open2;
	
	type union_W64_T_Tag is (u, d);
	pragma Discard_Names (union_W64_T_Tag);
	type union_W64_T (Unchecked_Tag : union_W64_T_Tag := d) is record
		case Unchecked_Tag is
			when u =>
				u : Interfaces.Unsigned_64;
			when d =>
				d : Long_Float;
		end case;
	end record;
	pragma Unchecked_Union (union_W64_T);
	
	-- printf("%1.15f(%08"PRIx64")", d, u);
	procedure Put (Item : in union_W64_T) is
		function Image_08PRIx64 is
			new Ada.Formatting.Modular_Image (
				Interfaces.Unsigned_64,
				Form => Ada.Formatting.Simple,
				Signs => Ada.Formatting.Triming_Unsign_Marks,
				Base => 16,
				Set => Ada.Formatting.Lower_Case,
				Digits_Width => 8);
	begin
		Ada.Long_Float_Text_IO.Put (Item.d, Aft => 15);
		Ada.Text_IO.Put ("(");
		Ada.Text_IO.Put (Image_08PRIx64 (Item.u));
		Ada.Text_IO.Put (")");
	end Put;
	
	procedure check (
		range_str : in String;
		genrand : in genrand_t;
		fill_array : in fill_array_t;
		st_genrand : in st_genrand_t;
		st_fill_array : in st_fill_array_t;
		seed : in Unsigned_32;
		print_size : in Integer)
	is
		lsize : constant Natural := DSFMT_N * 2 + 2;
		the_array : Long_Float_Array renames dummy;
		little : Long_Float_Array (0 .. lsize - 1); -- w128_t to Long_Float
		r, r_st : union_W64_T;
		dsfmt : aliased Generator;
	begin
		Ada.Text_IO.Put ("generated randoms ");
		Ada.Text_IO.Put (range_str);
		Ada.Text_IO.New_Line;
		Reset (dsfmt_global_data, Integer (seed));
		fill_array (little (0 .. lsize - 1));
		fill_array (the_array (0 .. 5000 - 1));
		Reset (dsfmt_global_data, Integer (seed));
		Reset (dsfmt, Integer (seed));
		for i in 0 .. lsize - 1 loop
			r.d := genrand.all;
			r_st.d := st_genrand (dsfmt);
			if r.u /= r_st.u
				or else r.u /= union_W64_T'(d, d => little (i)).u
			then
				Ada.Text_IO.New_Line;
				Ada.Text_IO.Put (range_str);
				Ada.Text_IO.Put (" mismatch i = ");
				Ada.Integer_Text_IO.Put (i, Width => 1);
				Ada.Text_IO.Put (": r = ");
				Put (r);
				Ada.Text_IO.Put (", st = ");
				Put (r_st);
				Ada.Text_IO.Put (", array = ");
				Put (union_W64_T'(d, d => little (i)));
				Ada.Text_IO.New_Line;
				raise exit_1;
			end if;
			if i < print_size then
				Ada.Long_Float_Text_IO.Put (little (i), Aft => 15);
				Ada.Text_IO.Put (" ");
				if i rem 4 = 3 then
					Ada.Text_IO.New_Line;
				end if;
			end if;
		end loop;
		for i in 0 .. 5000 - 1 loop
			r.d := genrand.all;
			if r.u /= union_W64_T'(d, d => the_array (i)).u then
				Ada.Text_IO.New_Line;
				Ada.Text_IO.Put (range_str);
				Ada.Text_IO.Put (" mismatch i = ");
				Ada.Integer_Text_IO.Put (i + lsize, Width => 1);
				Ada.Text_IO.Put (": r = ");
				Put (r);
				Ada.Text_IO.Put (", array = ");
				Put (union_W64_T'(d, d => the_array (i)));
				Ada.Text_IO.New_Line;
				raise exit_1;
			end if;
			if i + lsize < print_size then
				Ada.Long_Float_Text_IO.Put (the_array (i), Aft => 15);
				Ada.Text_IO.Put (" ");
				if (i + lsize) rem 4 = 3 then
					Ada.Text_IO.New_Line;
				end if;
			end if;
		end loop;
		
		Reset (dsfmt, Integer (seed));
		st_fill_array (dsfmt, little (0 .. lsize - 1));
		st_fill_array (dsfmt, the_array (0 .. 5000 - 1));
		Reset (dsfmt, Integer (seed));
		for i in 0 .. lsize - 1 loop
			r_st.d := st_genrand (dsfmt);
			if r_st.u /= union_W64_T'(d, d => little (i)).u then
				Ada.Text_IO.New_Line;
				Ada.Text_IO.Put (range_str);
				Ada.Text_IO.Put (" mismatch i = ");
				Ada.Integer_Text_IO.Put (i, Width => 1);
				Ada.Text_IO.Put (": st = ");
				Put (r_st);
				Ada.Text_IO.Put (", array = ");
				Put (union_W64_T'(d, d => little (i)));
				Ada.Text_IO.New_Line;
				raise exit_1;
			end if;
		end loop;
		for i in 0 .. 5000 - 1 loop
			r_st.d := st_genrand (dsfmt);
			if r_st.u /= union_W64_T'(d, d => the_array (i)).u then
				Ada.Text_IO.New_Line;
				Ada.Text_IO.Put (range_str);
				Ada.Text_IO.Put (" mismatch i = ");
				Ada.Integer_Text_IO.Put (i + lsize, Width => 1);
				Ada.Text_IO.Put (": st = ");
				Put (r_st);
				Ada.Text_IO.Put (", array = ");
				Put (union_W64_T'(d, d => the_array (i)));
				Ada.Text_IO.New_Line;
				raise exit_1;
			end if;
		end loop;
	end check;
	
	procedure check_ar (
		range_str : in String;
		genrand : in genrand_t;
		fill_array : in fill_array_t;
		st_genrand : in st_genrand_t;
		st_fill_array : in st_fill_array_t;
		print_size : in Integer)
	is
		lsize : constant Natural := DSFMT_N * 2 + 2;
		the_array : Long_Float_Array renames dummy;
		little : Long_Float_Array (0 .. lsize - 1); -- w128_t to Long_Float
		r, r_st : union_W64_T;
		dsfmt : aliased Generator;
		ar : Unsigned_32_Array (0 .. 3) := (1, 2, 3, 4);
	begin
		Ada.Text_IO.Put ("generated randoms ");
		Ada.Text_IO.Put (range_str);
		Ada.Text_IO.New_Line;
		Reset (dsfmt_global_data, Initialize (ar));
		fill_array (little (0 .. lsize - 1));
		fill_array (the_array (0 .. 5000 - 1));
		Reset (dsfmt_global_data, Initialize (ar));
		Reset (dsfmt, Initialize (ar));
		for i in 0 .. lsize - 1 loop
			r.d := genrand.all;
			r_st.d := st_genrand.all (dsfmt);
			if r.u /= r_st.u
				or else r.u /= union_W64_T'(d, d => little (i)).u
			then
				Ada.Text_IO.New_Line;
				Ada.Text_IO.Put (range_str);
				Ada.Text_IO.Put (" mismatch i = ");
				Ada.Integer_Text_IO.Put (i, Width => 1);
				Ada.Text_IO.Put (": r = ");
				Put (r);
				Ada.Text_IO.Put (", st = ");
				Put (r_st);
				Ada.Text_IO.Put (", array = ");
				Put (union_W64_T'(d, d => little (i)));
				Ada.Text_IO.New_Line;
				raise exit_1;
			end if;
			if i < print_size then
				Ada.Long_Float_Text_IO.Put (little (i), Aft => 15);
				Ada.Text_IO.Put (" ");
				if i rem 4 = 3 then
					Ada.Text_IO.New_Line;
				end if;
			end if;
		end loop;
		for i in 0 .. 5000 - 1 loop
			r.d := genrand.all;
			if r.u /= union_W64_T'(d, d => the_array (i)).u then
				Ada.Text_IO.New_Line;
				Ada.Text_IO.Put (range_str);
				Ada.Text_IO.Put (" mismatch i = ");
				Ada.Integer_Text_IO.Put (i + lsize, Width => 1);
				Ada.Text_IO.Put (": r = ");
				Put (r);
				Ada.Text_IO.Put (", array = ");
				Put (union_W64_T'(d, d => the_array (i)));
				Ada.Text_IO.New_Line;
				raise exit_1;
			end if;
			if i + lsize < print_size then
				Ada.Long_Float_Text_IO.Put (the_array (i), Aft => 15);
				Ada.Text_IO.Put (" ");
				if (i + lsize) rem 4 = 3 then
					Ada.Text_IO.New_Line;
				end if;
			end if;
		end loop;
		
		Reset (dsfmt, Initialize (ar));
		st_fill_array (dsfmt, little (0 .. lsize - 1));
		st_fill_array (dsfmt, the_array (0 .. 5000 - 1));
		Reset (dsfmt, Initialize (ar));
		for i in 0 .. lsize - 1 loop
			r_st.d := st_genrand (dsfmt);
			if r_st.u /= union_W64_T'(d, d => little (i)).u then
				Ada.Text_IO.New_Line;
				Ada.Text_IO.Put (range_str);
				Ada.Text_IO.Put (" mismatch i = ");
				Ada.Integer_Text_IO.Put (i, Width => 1);
				Ada.Text_IO.Put (": st = ");
				Put (r_st);
				Ada.Text_IO.Put (", array = ");
				Put (union_W64_T'(d, d => little (i)));
				Ada.Text_IO.New_Line;
				raise exit_1;
			end if;
		end loop;
		for i in 0 .. 5000 - 1 loop
			r_st.d := st_genrand (dsfmt);
			if r_st.u /= union_W64_T'(d, d => the_array (i)).u then
				Ada.Text_IO.New_Line;
				Ada.Text_IO.Put (range_str);
				Ada.Text_IO.Put (" mismatch i = ");
				Ada.Integer_Text_IO.Put (i + lsize, Width => 1);
				Ada.Text_IO.Put (": st = ");
				Put (r_st);
				Ada.Text_IO.Put (", array = ");
				Put (union_W64_T'(d, d => the_array (i)));
				Ada.Text_IO.New_Line;
				raise exit_1;
			end if;
		end loop;
	end check_ar;
	
	procedure test_co is
		clo : Ada.Execution_Time.CPU_Time;
		sum : Ada.Real_Time.Time_Span;
		the_array : Long_Float_Array renames dummy;
		dsfmt : aliased Generator;
	begin
-- #if 0
--		dsfmt_gv_init_gen_rand(1234);
--		sum = 0;
--		for (i = 0; i < 10; i++) {
--			clo = clock();
--			for (j = 0; j < TIC_COUNT; j++) {
--				dsfmt_gv_fill_array_close_open(array, NUM_RANDS);
--			}
--			clo = clock() - clo;
--			sum += clo;
--		}
--		printf("GL BLOCK [0, 1) AVE:%4"PRIu64"ms.\n",
--			(sum * 100) / CLOCKS_PER_SEC);
-- #endif
		Reset (dsfmt_global_data, 1234);
		sum := Ada.Real_Time.Time_Span_Zero;
		for i in 0 .. 10 - 1 loop
			clo := Ada.Execution_Time.Clock;
			for j in 0 .. TIC_COUNT - 1 loop
				Fill_Random_0_To_Less_Than_1 (
					dsfmt,
					the_array (0 .. NUM_RANDS - 1));
			end loop;
			sum := sum + (Ada.Execution_Time.Clock - clo);
		end loop;
		Ada.Text_IO.Put ("ST BLOCK [0, 1) AVE:");
		Ada.Integer_Text_IO.Put (Integer (Ada.Real_Time.To_Duration (sum * 100)),
			Width => 4);
		Ada.Text_IO.Put ("ms.");
		Ada.Text_IO.New_Line;
	end test_co;
	
	procedure test_oc is
		clo : Ada.Execution_Time.CPU_Time;
		sum : Ada.Real_Time.Time_Span;
		the_array : Long_Float_Array renames dummy;
		dsfmt : aliased Generator;
	begin
-- #if 0
--		dsfmt_gv_init_gen_rand(1234);
--		sum = 0;
--		for (i = 0; i < 10; i++) {
--			clo = clock();
--			for (j = 0; j < TIC_COUNT; j++) {
--				dsfmt_gv_fill_array_open_close(array, NUM_RANDS);
--			}
--			clo = clock() - clo;
--			sum += clo;
--		}
--		printf("GL BLOCK (0, 1] AVE:%4"PRIu64"ms.\n",
--			(sum * 100) / CLOCKS_PER_SEC);
-- #endif
		Reset (dsfmt_global_data, 1234);
		sum := Ada.Real_Time.Time_Span_Zero;
		for i in 0 .. 10 - 1 loop
			clo := Ada.Execution_Time.Clock;
			for j in 0 .. TIC_COUNT - 1 loop
				Fill_Random_Greater_Than_0_To_1 (
					dsfmt,
					the_array (0 .. NUM_RANDS - 1));
			end loop;
			sum := sum + (Ada.Execution_Time.Clock - clo);
		end loop;
		Ada.Text_IO.Put ("ST BLOCK (0, 1] AVE:");
		Ada.Integer_Text_IO.Put (Integer (Ada.Real_Time.To_Duration (sum * 100)),
			Width => 4);
		Ada.Text_IO.Put ("ms.");
		Ada.Text_IO.New_Line;
	end test_oc;
	
	procedure test_oo is
		clo : Ada.Execution_Time.CPU_Time;
		sum : Ada.Real_Time.Time_Span;
		the_array : Long_Float_Array renames dummy;
		dsfmt : aliased Generator;
	begin
-- #if 0
--		dsfmt_gv_init_gen_rand(1234);
--		sum = 0;
--		for (i = 0; i < 10; i++) {
--			clo = clock();
--			for (j = 0; j < TIC_COUNT; j++) {
--				dsfmt_gv_fill_array_open_open(array, NUM_RANDS);
--			}
--			clo = clock() - clo;
--			sum += clo;
--		}
--		printf("GL BLOCK (0, 1) AVE:%4"PRIu64"ms.\n",
--			(sum * 100) / CLOCKS_PER_SEC);
-- #endif
		Reset (dsfmt_global_data, 1234);
		sum := Ada.Real_Time.Time_Span_Zero;
		for i in 0 .. 10 - 1 loop
			clo := Ada.Execution_Time.Clock;
			for j in 0 .. TIC_COUNT - 1 loop
				Fill_Random_Greater_Than_0_To_Less_Than_1 (
					dsfmt,
					the_array (0 .. NUM_RANDS - 1));
			end loop;
			sum := sum + (Ada.Execution_Time.Clock - clo);
		end loop;
		Ada.Text_IO.Put ("ST BLOCK (0, 1) AVE:");
		Ada.Integer_Text_IO.Put (Integer (Ada.Real_Time.To_Duration (sum * 100)),
			Width => 4);
		Ada.Text_IO.Put ("ms.");
		Ada.Text_IO.New_Line;
	end test_oo;
	
	procedure test_12 is
		clo : Ada.Execution_Time.CPU_Time;
		sum : Ada.Real_Time.Time_Span;
		the_array : Long_Float_Array renames dummy;
		dsfmt : aliased Generator;
	begin
-- #if 0
--		dsfmt_gv_init_gen_rand(1234);
--		sum = 0;
--		for (i = 0; i < 10; i++) {
--			clo = clock();
--			for (j = 0; j < TIC_COUNT; j++) {
--				dsfmt_gv_fill_array_close1_open2(array, NUM_RANDS);
--			}
--			clo = clock() - clo;
--			sum += clo;
--		}
--		printf("GL BLOCK [1, 2) AVE:%4"PRIu64"ms.\n",
--			(sum * 100) / CLOCKS_PER_SEC);
-- #endif
		Reset (dsfmt_global_data, 1234);
		sum := Ada.Real_Time.Time_Span_Zero;
		for i in 0 .. 10 - 1 loop
			clo := Ada.Execution_Time.Clock;
			for j in 0 .. TIC_COUNT - 1 loop
				Fill_Random_1_To_Less_Than_2 (
					dsfmt,
					the_array (0 .. NUM_RANDS - 1));
			end loop;
			sum := sum + (Ada.Execution_Time.Clock - clo);
		end loop;
		Ada.Text_IO.Put ("ST BLOCK [1, 2) AVE:");
		Ada.Integer_Text_IO.Put (Integer (Ada.Real_Time.To_Duration (sum * 100)),
			Width => 4);
		Ada.Text_IO.Put ("ms.");
		Ada.Text_IO.New_Line;
	end test_12;
	
	procedure test_seq_co is
		clo : Ada.Execution_Time.CPU_Time;
		sum : Ada.Real_Time.Time_Span;
		the_array : Long_Float_Array renames dummy;
		r : Long_Float;
		total : Long_Float := 0.0;
		dsfmt : aliased Generator;
	begin
-- #if 0
--		dsfmt_gv_init_gen_rand(1234);
--		sum = 0;
--		r = 0;
--		for (i = 0; i < 10; i++) {
--			clo = clock();
--			for (j = 0; j < TIC_COUNT; j++) {
--				for (k = 0; k < NUM_RANDS; k++) {
--					r += dsfmt_gv_genrand_close_open();
--				}
--			}
--			clo = clock() - clo;
--			sum += clo;
--		}
--		total = r;
--		printf("GL SEQ [0, 1) 1 AVE:%4"PRIu64"ms.\n",
--			(sum * 100)  / CLOCKS_PER_SEC);
		
--		sum = 0;
--		for (i = 0; i < 10; i++) {
--			clo = clock();
--			for (j = 0; j < TIC_COUNT; j++) {
--				for (k = 0; k < NUM_RANDS; k++) {
--					array[k] = dsfmt_gv_genrand_close_open();
--				}
--			}
--			clo = clock() - clo;
--			sum += clo;
--		}
--		for (k = 0; k < NUM_RANDS; k++) {
--			total += array[k];
--		}
--		printf("GL SEQ [0, 1) 2 AVE:%4"PRIu64"ms.\n",
--			(sum * 100)  / CLOCKS_PER_SEC);
-- #endif
		Reset (dsfmt_global_data, 1234);
		sum := Ada.Real_Time.Time_Span_Zero;
		r := 0.0;
		for i in 0 .. 10 - 1 loop
			clo := Ada.Execution_Time.Clock;
			for j in 0 .. TIC_COUNT - 1 loop
				for k in 0 .. NUM_RANDS - 1 loop
					r := r + Random_0_To_Less_Than_1 (dsfmt);
				end loop;
			end loop;
			sum := sum + (Ada.Execution_Time.Clock - clo);
		end loop;
		total := r;
		Ada.Text_IO.Put ("ST SEQ [0, 1) 1 AVE:");
		Ada.Integer_Text_IO.Put (Integer (Ada.Real_Time.To_Duration (sum * 100)),
			Width => 4);
		Ada.Text_IO.Put ("ms.");
		Ada.Text_IO.New_Line;
		
		sum := Ada.Real_Time.Time_Span_Zero;
		for i in 0 .. 10 - 1 loop
			clo := Ada.Execution_Time.Clock;
			for j in 0 ..TIC_COUNT - 1 loop
				for k in 0 .. NUM_RANDS - 1 loop
					the_array (k) := Random_0_To_Less_Than_1 (dsfmt);
				end loop;
			end loop;
			sum := sum + (Ada.Execution_Time.Clock - clo);
		end loop;
		for k in 0 .. NUM_RANDS - 1 loop
			total := total + the_array (k);
		end loop;
		Ada.Text_IO.Put ("ST SEQ [0, 1) 2 AVE:");
		Ada.Integer_Text_IO.Put (Integer (Ada.Real_Time.To_Duration (sum * 100)),
			Width => 4);
		Ada.Text_IO.Put ("ms.");
		Ada.Text_IO.New_Line;
		
		Ada.Text_IO.Put ("total = ");
		Ada.Long_Float_Text_IO.Put (total);
		Ada.Text_IO.New_Line;
	end test_seq_co;
	
	procedure test_seq_oc is
		clo : Ada.Execution_Time.CPU_Time;
		sum : Ada.Real_Time.Time_Span;
		the_array : Long_Float_Array renames dummy;
		r : Long_Float;
		total : Long_Float := 0.0;
		dsfmt : aliased Generator;
	begin
-- #if 0
--		dsfmt_gv_init_gen_rand(1234);
--		sum = 0;
--		r = 0;
--		for (i = 0; i < 10; i++) {
--			clo = clock();
--			for (j = 0; j < TIC_COUNT; j++) {
--				for (k = 0; k < NUM_RANDS; k++) {
--					r += dsfmt_gv_genrand_open_close();
--				}
--			}
--			clo = clock() - clo;
--			sum += clo;
--		}
--		total = r;
--		printf("GL SEQ (0, 1] 1 AVE:%4"PRIu64"ms.\n",
--			(sum * 100)  / CLOCKS_PER_SEC);
--		sum = 0;
--		for (i = 0; i < 10; i++) {
--			clo = clock();
--			for (j = 0; j < TIC_COUNT; j++) {
--				for (k = 0; k < NUM_RANDS; k++) {
--					array[k] = dsfmt_gv_genrand_open_close();
--				}
--			}
--			clo = clock() - clo;
--			sum += clo;
--		}
--		for (k = 0; k < NUM_RANDS; k++) {
--			total += array[k];
--		}
--		printf("GL SEQ (0, 1] 2 AVE:%4"PRIu64"ms.\n",
--			(sum * 100)  / CLOCKS_PER_SEC);
-- #endif
		Reset (dsfmt_global_data, 1234);
		sum := Ada.Real_Time.Time_Span_Zero;
		r := 0.0;
		for i in 0 .. 10 - 1 loop
			clo := Ada.Execution_Time.Clock;
			for j in 0 .. TIC_COUNT - 1 loop
				for k in 0 .. NUM_RANDS - 1 loop
					r := r + Random_Greater_Than_0_To_1 (dsfmt);
				end loop;
			end loop;
			sum := sum + (Ada.Execution_Time.Clock - clo);
		end loop;
		total := r;
		Ada.Text_IO.Put ("ST SEQ (0, 1] 1 AVE:");
		Ada.Integer_Text_IO.Put (Integer (Ada.Real_Time.To_Duration (sum * 100)),
			Width => 4);
		Ada.Text_IO.Put ("ms.");
		Ada.Text_IO.New_Line;
		
		sum := Ada.Real_Time.Time_Span_Zero;
		for i in 0 .. 10 - 1 loop
			clo := Ada.Execution_Time.Clock;
			for j in 0 ..TIC_COUNT - 1 loop
				for k in 0 .. NUM_RANDS - 1 loop
					the_array (k) := Random_Greater_Than_0_To_1 (dsfmt);
				end loop;
			end loop;
			sum := sum + (Ada.Execution_Time.Clock - clo);
		end loop;
		for k in 0 .. NUM_RANDS - 1 loop
			total := total + the_array (k);
		end loop;
		Ada.Text_IO.Put ("ST SEQ (0, 1] 2 AVE:");
		Ada.Integer_Text_IO.Put (Integer (Ada.Real_Time.To_Duration (sum * 100)),
			Width => 4);
		Ada.Text_IO.Put ("ms.");
		Ada.Text_IO.New_Line;
		
		Ada.Text_IO.Put ("total = ");
		Ada.Long_Float_Text_IO.Put (total);
		Ada.Text_IO.New_Line;
	end test_seq_oc;
	
	procedure test_seq_oo is
		clo : Ada.Execution_Time.CPU_Time;
		sum : Ada.Real_Time.Time_Span;
		the_array : Long_Float_Array renames dummy;
		r : Long_Float;
		total : Long_Float := 0.0;
		dsfmt : aliased Generator;
	begin
-- #if 0
--		dsfmt_gv_init_gen_rand(1234);
--		sum = 0;
--		r = 0;
--		for (i = 0; i < 10; i++) {
--			clo = clock();
--			for (j = 0; j < TIC_COUNT; j++) {
--				for (k = 0; k < NUM_RANDS; k++) {
--					r += dsfmt_gv_genrand_open_open();
--				}
--			}
--			clo = clock() - clo;
--			sum += clo;
--		}
--		total = r;
--		printf("GL SEQ (0, 1) 1 AVE:%4"PRIu64"ms.\n",
--			(sum * 100)  / CLOCKS_PER_SEC);
--		sum = 0;
--		for (i = 0; i < 10; i++) {
--			clo = clock();
--			for (j = 0; j < TIC_COUNT; j++) {
--				for (k = 0; k < NUM_RANDS; k++) {
--					array[k] = dsfmt_gv_genrand_open_open();
--				}
--			}
--			clo = clock() - clo;
--			sum += clo;
--		}
--		for (k = 0; k < NUM_RANDS; k++) {
--			total += array[k];
--		}
--		printf("GL SEQ (0, 1) 2 AVE:%4"PRIu64"ms.\n",
--			(sum * 100)  / CLOCKS_PER_SEC);
-- #endif
		Reset (dsfmt_global_data, 1234);
		sum := Ada.Real_Time.Time_Span_Zero;
		r := 0.0;
		for i in 0 .. 10 - 1 loop
			clo := Ada.Execution_Time.Clock;
			for j in 0 .. TIC_COUNT - 1 loop
				for k in 0 .. NUM_RANDS - 1 loop
					r := r + Random_Greater_Than_0_To_Less_Than_1 (dsfmt);
				end loop;
			end loop;
			sum := sum + (Ada.Execution_Time.Clock - clo);
		end loop;
		total := r;
		Ada.Text_IO.Put ("ST SEQ (0, 1) 1 AVE:");
		Ada.Integer_Text_IO.Put (Integer (Ada.Real_Time.To_Duration (sum * 100)),
			Width => 4);
		Ada.Text_IO.Put ("ms.");
		Ada.Text_IO.New_Line;
		
		sum := Ada.Real_Time.Time_Span_Zero;
		for i in 0 .. 10 - 1 loop
			clo := Ada.Execution_Time.Clock;
			for j in 0 ..TIC_COUNT - 1 loop
				for k in 0 .. NUM_RANDS - 1 loop
					the_array (k) := Random_Greater_Than_0_To_Less_Than_1 (dsfmt);
				end loop;
			end loop;
			sum := sum + (Ada.Execution_Time.Clock - clo);
		end loop;
		for k in 0 .. NUM_RANDS - 1 loop
			total := total + the_array (k);
		end loop;
		Ada.Text_IO.Put ("ST SEQ (0, 1) 2 AVE:");
		Ada.Integer_Text_IO.Put (Integer (Ada.Real_Time.To_Duration (sum * 100)),
			Width => 4);
		Ada.Text_IO.Put ("ms.");
		Ada.Text_IO.New_Line;
		
		Ada.Text_IO.Put ("total = ");
		Ada.Long_Float_Text_IO.Put (total);
		Ada.Text_IO.New_Line;
	end test_seq_oo;
	
	procedure test_seq_12 is
		clo : Ada.Execution_Time.CPU_Time;
		sum : Ada.Real_Time.Time_Span;
		the_array : Long_Float_Array renames dummy;
		r : Long_Float;
		total : Long_Float := 0.0;
		dsfmt : aliased Generator;
	begin
-- #if 0
--		dsfmt_gv_init_gen_rand(1234);
--		sum = 0;
--		r = 0;
--		for (i = 0; i < 10; i++) {
--			clo = clock();
--			for (j = 0; j < TIC_COUNT; j++) {
--				for (k = 0; k < NUM_RANDS; k++) {
--					r += dsfmt_gv_genrand_close1_open2();
--				}
--			}
--			clo = clock() - clo;
--			sum += clo;
--		}
--		total = r;
--		printf("GL SEQ [1, 2) 1 AVE:%4"PRIu64"ms.\n",
--			(sum * 100)  / CLOCKS_PER_SEC);
--		sum = 0;
--		for (i = 0; i < 10; i++) {
--			clo = clock();
--			for (j = 0; j < TIC_COUNT; j++) {
--				for (k = 0; k < NUM_RANDS; k++) {
--					array[k] = dsfmt_gv_genrand_close1_open2();
--				}
--			}
--			clo = clock() - clo;
--			sum += clo;
--		}
--		for (k = 0; k < NUM_RANDS; k++) {
--			total += array[k];
--		}
--		printf("GL SEQ [1, 2) 2 AVE:%4"PRIu64"ms.\n",
--			(sum * 100)  / CLOCKS_PER_SEC);
-- #endif
		Reset (dsfmt_global_data, 1234);
		sum := Ada.Real_Time.Time_Span_Zero;
		r := 0.0;
		for i in 0 .. 10 - 1 loop
			clo := Ada.Execution_Time.Clock;
			for j in 0 .. TIC_COUNT - 1 loop
				for k in 0 .. NUM_RANDS - 1 loop
					r := r + Random_1_To_Less_Than_2 (dsfmt);
				end loop;
			end loop;
			sum := sum + (Ada.Execution_Time.Clock - clo);
		end loop;
		total := r;
		Ada.Text_IO.Put ("ST SEQ [1, 2) 1 AVE:");
		Ada.Integer_Text_IO.Put (Integer (Ada.Real_Time.To_Duration (sum * 100)),
			Width => 4);
		Ada.Text_IO.Put ("ms.");
		Ada.Text_IO.New_Line;
		
		sum := Ada.Real_Time.Time_Span_Zero;
		for i in 0 .. 10 - 1 loop
			clo := Ada.Execution_Time.Clock;
			for j in 0 ..TIC_COUNT - 1 loop
				for k in 0 .. NUM_RANDS - 1 loop
					the_array (k) := Random_1_To_Less_Than_2 (dsfmt);
				end loop;
			end loop;
			sum := sum + (Ada.Execution_Time.Clock - clo);
		end loop;
		for k in 0 .. NUM_RANDS - 1 loop
			total := total + the_array (k);
		end loop;
		Ada.Text_IO.Put ("ST SEQ [1, 2) 2 AVE:");
		Ada.Integer_Text_IO.Put (Integer (Ada.Real_Time.To_Duration (sum * 100)),
			Width => 4);
		Ada.Text_IO.Put ("ms.");
		Ada.Text_IO.New_Line;
		
		Ada.Text_IO.Put ("total = ");
		Ada.Long_Float_Text_IO.Put (total);
		Ada.Text_IO.New_Line;
	end test_seq_12;
	
begin
	Ada.Long_Float_Text_IO.Default_Fore := 0;
	Ada.Long_Float_Text_IO.Default_Aft := 6; -- default of "%f"
	Ada.Long_Float_Text_IO.Default_Exp := 0;
	if Ada.Command_Line.Argument_Count >= 1
		and then Ada.Command_Line.Argument (1) = "-s"
	then
		Ada.Text_IO.Put ("consumed time for generating ");
		Ada.Integer_Text_IO.Put (NUM_RANDS * TIC_COUNT, Width => 1);
		Ada.Text_IO.Put (" randoms.");
		Ada.Text_IO.New_Line;
		test_co;
		test_oc;
		test_oo;
		test_12;
		test_seq_co;
		test_seq_oc;
		test_seq_oo;
		test_seq_12;
	else
		Ada.Text_IO.Put_Line (Id);
		Ada.Text_IO.Put ("init_gen_rand(0) ");
		check (
			"[1, 2)",
			s_genrand_close1_open2'Access,
			s_fill_array_close1_open2'Access,
			sst_genrand_close1_open2'Access,
			sst_fill_array_close1_open2'Access,
			0,
			1000);
		for i in 0 .. 19 loop
			Ada.Text_IO.Put ("init_gen_rand(");
			Ada.Integer_Text_IO.Put (i, Width => 1);
			Ada.Text_IO.Put (") ");
			case i rem 4 is
				when 0 =>
					check (
						"[0, 1)",
						s_genrand_close_open'Access,
						s_fill_array_close_open'Access,
						sst_genrand_close_open'Access,
						sst_fill_array_close_open'Access,
						Unsigned_32'Mod (i),
						12);
				when 1 =>
					check (
						"(0, 1]",
						s_genrand_open_close'Access,
						s_fill_array_open_close'Access,
						sst_genrand_open_close'Access,
						sst_fill_array_open_close'Access,
						Unsigned_32'Mod (i),
						12);
				when 2 =>
					check (
						"(0, 1)",
						s_genrand_open_open'Access,
						s_fill_array_open_open'Access,
						sst_genrand_open_open'Access,
						sst_fill_array_open_open'Access,
						Unsigned_32'Mod (i),
						12);
				when others =>
					check (
						"[1, 2)",
						s_genrand_close1_open2'Access,
						s_fill_array_close1_open2'Access,
						sst_genrand_close1_open2'Access,
						sst_fill_array_close1_open2'Access,
						Unsigned_32'Mod (i),
						12);
			end case;
		end loop;
		Ada.Text_IO.Put ("init_by_array {1, 2, 3, 4} ");
		check_ar (
			"[1, 2)",
			s_genrand_close1_open2'Access,
			s_fill_array_close1_open2'Access,
			sst_genrand_close1_open2'Access,
			sst_fill_array_close1_open2'Access,
			1000);
	end if;
exception
	when exit_1 =>
		Ada.Command_Line.Set_Exit_Status (1);
end random_dsfmt;
