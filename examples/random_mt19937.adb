--  translated unit from MT19937 (mtTest.c)
--
--   A C-program for MT19937, with initialization improved 2002/1/26.
--   Coded by Takuji Nishimura and Makoto Matsumoto.
--
--   Before using, initialize the state by using init_genrand(seed)  
--   or init_by_array(init_key, key_length).
--
--   Copyright (C) 1997 - 2002, Makoto Matsumoto and Takuji Nishimura,
--   All rights reserved.
--   Copyright (C) 2005, Mutsuo Saito,
--   All rights reserved.
--
--   Redistribution and use in source and binary forms, with or without
--   modification, are permitted provided that the following conditions
--   are met:
--
--     1. Redistributions of source code must retain the above copyright
--        notice, this list of conditions and the following disclaimer.
--
--     2. Redistributions in binary form must reproduce the above copyright
--        notice, this list of conditions and the following disclaimer in the
--        documentation and/or other materials provided with the distribution.
--
--     3. The names of its contributors may not be used to endorse or promote 
--        products derived from this software without specific prior written 
--        permission.
--
--   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
--   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
--   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
--   A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT OWNER OR
--   CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
--   EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
--   PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
--   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
--   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
--   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
--   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
--
--
--   Any feedback is very welcome.
--   http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/emt.html
--   email: m-mat @ math.sci.hiroshima-u.ac.jp (remove space)
--
--
--   Ada version by yt
--
with Ada.Long_Long_Float_Text_IO;
with Ada.Numerics.MT19937;
with Ada.Text_IO;
procedure random_mt19937 is
	use Ada.Numerics.MT19937;
	package Cardinal_IO is new Ada.Text_IO.Modular_IO (Cardinal);
	init : Cardinal_Vector (0 .. 3) := (16#123#, 16#234#, 16#345#, 16#456#);
	Gen : aliased Generator := Initialize (init);
begin
	Ada.Text_IO.Put_Line ("1000 outputs of genrand_int32()");
	for i in 0 .. 1000 - 1 loop
		Cardinal_IO.Put (
			Random_32 (Gen'Access),
			Width => 10);
		Ada.Text_IO.Put (' ');
		if i rem 5 = 4 then
			Ada.Text_IO.New_Line;
		end if;
	end loop;
	Ada.Text_IO.New_Line;
	Ada.Text_IO.Put_Line ("1000 outputs of genrand_real2()");
	for i in 0 .. 1000 - 1 loop
		Ada.Long_Long_Float_Text_IO.Put (
			Random_0_To_Less_1 (Gen'Access),
			Fore => 0,
			Aft => 8,
			Exp => 0);
		Ada.Text_IO.Put (' ');
		if i rem 5 = 4 then
			Ada.Text_IO.New_Line;
		end if;
	end loop;
end random_mt19937;
