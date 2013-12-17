pragma License (Unrestricted);
--  translated unit from MT19937
--
--   A C-program for MT19937, with initialization improved 2002/1/26.
--   Coded by Takuji Nishimura and Makoto Matsumoto.
--
--   Before using, initialize the state by using init_genrand(seed)
--   or init_by_array(init_key, key_length).
--
--   Copyright (C) 1997 - 2002, Makoto Matsumoto and Takuji Nishimura,
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
--   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
--   OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
--   EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
--   PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
--   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
--   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
--   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
--   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
--
--
--   Any feedback is very welcome.
--   http://www.math.keio.ac.jp/matumoto/emt.html
--   email: matumoto@math.keio.ac.jp
--
--
--   Ada version by yt
--
with Ada.IO_Exceptions;
with Interfaces;
package Ada.Numerics.MT19937 is
   pragma Preelaborate;

   subtype Cardinal is Interfaces.Unsigned_32;
   type Cardinal_Vector is array (Natural range <>) of Cardinal;

   type Generator is limited private;

   function Random_32 (Gen : not null access Generator) return Cardinal;

   function Initialize return Generator;
   function Initialize (Initiator : Cardinal) return Generator;
   function Initialize (Initiator : Cardinal_Vector) return Generator;

   procedure Reset (Gen : in out Generator);
   procedure Reset (Gen : in out Generator; Initiator : Integer);

   type State is private;

   function Initialize return State;
   function Initialize (Initiator : Cardinal) return State;
   function Initialize (Initiator : Cardinal_Vector) return State;

   procedure Save (Gen : Generator; To_State : out State);
   procedure Reset (Gen : in out Generator; From_State : State);
   function Reset (From_State : State) return Generator;

   Max_Image_Width : constant := (624 + 1) * (32 / 4 + 1) - 1;

   function Image (Of_State : State) return String;
   function Value (Coded_State : String) return State;

   Default_Initiator : constant := 5489;

   subtype Uniformly_Distributed is Long_Long_Float range 0.0 .. 1.0;

   function Random_0_To_1 (Gen : not null access Generator)
      return Uniformly_Distributed;
   function Random_0_To_Less_Than_1 (Gen : not null access Generator)
      return Uniformly_Distributed;
   function Random_Greater_Than_0_To_Less_Than_1 (
      Gen : not null access Generator)
      return Uniformly_Distributed;
   function Random_53_0_To_Less_Than_1 (Gen : not null access Generator)
      return Uniformly_Distributed;

   generic
      type Result_Subtype is (<>);
   package Discrete_Random is
      function Random (Gen : not null access Generator) return Result_Subtype;
   end Discrete_Random;

   Use_Error : exception
      renames IO_Exceptions.Use_Error;
   --  Use_Error may be raised from Initialize (Generic_Initiator)

private

   N : constant := 624;
   M : constant := 397;
   subtype N_Range is Natural range 0 .. N - 1;

   type State is record
      Vector : Cardinal_Vector (N_Range);
      Condition : Cardinal;
   end record;

   type Generator is limited record
      State : MT19937.State := Initialize (Default_Initiator);
   end record;

--  Max_Image_Width : constant Positive := (N + 1) * Image_Column;

end Ada.Numerics.MT19937;
