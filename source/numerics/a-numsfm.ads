pragma License (Unrestricted);
--  translated unit from SFMT
--
--  Copyright (c) 2006,2007 Mutsuo Saito, Makoto Matsumoto and Hiroshima
--  University. All rights reserved.
--
--  Redistribution and use in source and binary forms, with or without
--  modification, are permitted provided that the following conditions are
--  met:
--
--      * Redistributions of source code must retain the above copyright
--        notice, this list of conditions and the following disclaimer.
--      * Redistributions in binary form must reproduce the above
--        copyright notice, this list of conditions and the following
--        disclaimer in the documentation and/or other materials provided
--        with the distribution.
--      * Neither the name of the Hiroshima University nor the names of
--        its contributors may be used to endorse or promote products
--        derived from this software without specific prior written
--        permission.
--
--  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
--  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
--  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
--  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
--  OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
--  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
--  LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
--  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
--  THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
--  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
--  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
--
--  Ada version: 2012 yt
with Ada.IO_Exceptions;
with Interfaces;
generic
   --  Mersenne Exponent. The period of the sequence
   --  is a multiple of 2^MEXP-1.
   MEXP : Natural := 19937;
   --  the pick up position of the array.
   POS1 : Natural := 122;
   --  the parameter of shift left as four 32-bit registers.
   SL1 : Natural := 18;
   --  the parameter of shift left as one 128-bit register.
   --  The 128-bit integer is shifted by (SL2 * 8) bits.
   SL2 : Natural := 1;
   --  the parameter of shift right as four 32-bit registers.
   SR1 : Natural := 11;
   --  the parameter of shift right as one 128-bit register.
   --  The 128-bit integer is shifted by (SL2 * 8) bits.
   SR2 : Natural := 1;
   --  A bitmask, used in the recursion.  These parameters are introduced
   --  to break symmetry of SIMD.
   MSK1 : Interfaces.Unsigned_32 := 16#dfffffef#;
   MSK2 : Interfaces.Unsigned_32 := 16#ddfecb7f#;
   MSK3 : Interfaces.Unsigned_32 := 16#bffaffff#;
   MSK4 : Interfaces.Unsigned_32 := 16#bffffff6#;
   --  These definitions are part of a 128-bit period certification vector.
   PARITY1 : Interfaces.Unsigned_32 := 16#00000001#;
   PARITY2 : Interfaces.Unsigned_32 := 16#00000000#;
   PARITY3 : Interfaces.Unsigned_32 := 16#00000000#;
   PARITY4 : Interfaces.Unsigned_32 := 16#c98e126a#;
package Ada.Numerics.SFMT is
   --  SIMD-oriented Fast Mersenne Twister.
   pragma Preelaborate;

   subtype Unsigned_32 is Interfaces.Unsigned_32;

   type Unsigned_32_Array is
      array (Natural range <>) of aliased Interfaces.Unsigned_32;
   for Unsigned_32_Array'Alignment use 16;

   subtype Unsigned_64 is Interfaces.Unsigned_64;

   type Unsigned_64_Array is
      array (Natural range <>) of aliased Interfaces.Unsigned_64;
   for Unsigned_64_Array'Alignment use 16;

   --  Identification string

   function Id return String;

   --  Basic facilities

   type Generator is limited private;

   function Random_32 (Gen : aliased in out Generator) return Unsigned_32;
   function Random_64 (Gen : aliased in out Generator) return Unsigned_64;

   pragma Inline (Random_32);
   pragma Inline (Random_64);

   procedure Fill_Random_32 (
      Gen : aliased in out Generator;
      Item : out Unsigned_32_Array);
   procedure Fill_Random_64 (
      Gen : aliased in out Generator;
      Item : out Unsigned_64_Array);

   function Initialize return Generator;
   function Initialize (Initiator : Unsigned_32) return Generator;
   function Initialize (Initiator : Unsigned_32_Array) return Generator;

   procedure Reset (Gen : in out Generator);
   procedure Reset (Gen : in out Generator; Initiator : Integer);

   --  Advanced facilities

   type State is private;
   pragma Preelaborable_Initialization (State); -- uninitialized

   function Initialize return State;
   function Initialize (Initiator : Unsigned_32) return State;
   function Initialize (Initiator : Unsigned_32_Array) return State;

   procedure Save (Gen : Generator; To_State : out State);
   procedure Reset (Gen : in out Generator; From_State : State);
   function Reset (From_State : State) return Generator;

   Max_Image_Width : constant Natural;

   function Image (Of_State : State) return String;
   function Value (Coded_State : String) return State;

   Default_Initiator : constant := 1234; -- test.c

   --  This constant means the minimum size of array used for
   --  Fill_Random_32 procedure.
   Min_Array_Length_32 : constant Natural;
   pragma Warnings (Off, Min_Array_Length_32);
   --  This constant means the minimum size of array used for
   --  Fill_Random_64 procedure.
   Min_Array_Length_64 : constant Natural;
   pragma Warnings (Off, Min_Array_Length_64);

   subtype Uniformly_Distributed is Long_Long_Float range 0.0 .. 1.0;

   function To_0_To_1 (v : Unsigned_32)
      return Uniformly_Distributed;
   pragma Inline (To_0_To_1);

   function Random_0_To_1 (Gen : aliased in out Generator)
      return Uniformly_Distributed;
   pragma Inline (Random_0_To_1);

   function To_0_To_Less_Than_1 (v : Unsigned_32)
      return Uniformly_Distributed;
   pragma Inline (To_0_To_Less_Than_1);

   function Random_0_To_Less_Than_1 (Gen : aliased in out Generator)
      return Uniformly_Distributed;
   pragma Inline (Random_0_To_Less_Than_1);

   function To_Greater_Than_0_To_Less_Than_1 (v : Unsigned_32)
      return Uniformly_Distributed;
   pragma Inline (To_Greater_Than_0_To_Less_Than_1);

   function Random_Greater_Than_0_To_Less_Than_1 (
      Gen : aliased in out Generator)
      return Uniformly_Distributed;
   pragma Inline (Random_Greater_Than_0_To_Less_Than_1);

   function To_53_0_To_Less_Than_1 (v : Unsigned_64)
      return Uniformly_Distributed;
   pragma Inline (To_53_0_To_Less_Than_1);

   function Random_53_0_To_Less_Than_1 (Gen : aliased in out Generator)
      return Uniformly_Distributed;
   pragma Inline (Random_53_0_To_Less_Than_1);

   --  Exceptions

   Use_Error : exception
      renames IO_Exceptions.Use_Error;
   --  Note: Use_Error may be raised from Initialize

private

   --  SFMT generator has an internal state array of 128-bit integers,
   --  and N is its size.
   N : constant Natural := MEXP / 128 + 1;
   --  N32 is the size of internal state array when regarded as an array
   --  of 32-bit integers.
   Min_Array_Length_32 : constant Natural := N * 4;
   N32 : Natural renames Min_Array_Length_32;
   --  N64 is the size of internal state array when regarded as an array
   --  of 64-bit integers.
   Min_Array_Length_64 : constant Natural := N * 2;
   N64 : Natural renames Min_Array_Length_64;

   Max_Image_Width : constant Natural := (N32 + 1) * (32 / 4 + 1) - 1;

   subtype Unsigned_32_Array_N32 is Unsigned_32_Array (0 .. N32 - 1);
   subtype Unsigned_64_Array_N64 is Unsigned_64_Array (0 .. N64 - 1);

   --  128-bit data type
   type w128_t is array (0 .. 3) of Unsigned_32;
   pragma Suppress_Initialization (w128_t);
   type w128_t_Array is array (Natural range <>) of aliased w128_t;
   for w128_t_Array'Alignment use 16;
   pragma Suppress_Initialization (w128_t_Array);

   subtype w128_t_Array_N is w128_t_Array (0 .. N - 1);
   subtype w128_t_Array_Fixed is w128_t_Array (Natural);

   --  SFMT internal state
   type State is record
      --  the 128-bit internal state array
      state : aliased w128_t_Array_N;
      --  index counter to the 32-bit internal state array
      idx : Integer;
   end record;
   pragma Suppress_Initialization (State);

   type Generator is limited record
      sfmt : State := Initialize (Default_Initiator);
   end record;

end Ada.Numerics.SFMT;
