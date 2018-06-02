pragma License (Unrestricted); -- BSD 3-Clause
--  translated unit from dSFMT
--
--  Copyright (c) 2007, 2008, 2009 Mutsuo Saito, Makoto Matsumoto
--  and Hiroshima University.
--  Copyright (c) 2011, 2002 Mutsuo Saito, Makoto Matsumoto, Hiroshima
--  University and The University of Tokyo.
--  All rights reserved.
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
--  Ada version: 2018 yt
with Ada.IO_Exceptions;
with Interfaces;
generic
   --  Mersenne Exponent. The period of the sequence is a multiple of 2^MEXP-1.
   MEXP : Natural;
   --  the pick up position of the array.
   POS1 : Natural;
   --  the parameter of shift left as four 32-bit registers.
   SL1 : Natural;
   --  A bitmask, used in the recursion. These parameters are introduced to
   --    break symmetry of SIMD.
   MSK1 : Interfaces.Unsigned_64;
   MSK2 : Interfaces.Unsigned_64;
   FIX1 : Interfaces.Unsigned_64;
   FIX2 : Interfaces.Unsigned_64;
   --  These definitions are part of a 128-bit period certification vector.
   PCV1 : Interfaces.Unsigned_64;
   PCV2 : Interfaces.Unsigned_64;
package Ada.Numerics.dSFMT is
   --  Double precision SIMD oriented Fast Mersenne Twister(dSFMT)
   --  pseudorandom number generator based on IEEE 754 format.
   pragma Preelaborate;

   type Long_Float_Array is array (Natural range <>) of Long_Float;
   for Long_Float_Array'Alignment use 16;

   subtype Unsigned_32 is Interfaces.Unsigned_32;

   type Unsigned_32_Array is
      array (Natural range <>) of aliased Interfaces.Unsigned_32;
   for Unsigned_32_Array'Alignment use 16;

   --  Identification string

   function Id return String;

   DSFMT_MEXP : Natural
      renames MEXP;

   --  Basic facilities

   type Generator is limited private;

   function Random_1_To_Less_Than_2 (Gen : aliased in out Generator)
      return Long_Float;
   function Random_0_To_Less_Than_1 (Gen : aliased in out Generator)
      return Long_Float;
   function Random_Greater_Than_0_To_1 (Gen : aliased in out Generator)
      return Long_Float;
   function Random_Greater_Than_0_To_Less_Than_1 (
      Gen : aliased in out Generator)
      return Long_Float;

   pragma Inline (Random_1_To_Less_Than_2);
   pragma Inline (Random_0_To_Less_Than_1);
   pragma Inline (Random_Greater_Than_0_To_1);
   pragma Inline (Random_Greater_Than_0_To_Less_Than_1);

   procedure Fill_Random_1_To_Less_Than_2 (
      Gen : aliased in out Generator;
      Item : out Long_Float_Array);
   procedure Fill_Random_0_To_Less_Than_1 (
      Gen : aliased in out Generator;
      Item : out Long_Float_Array);
   procedure Fill_Random_Greater_Than_0_To_1 (
      Gen : aliased in out Generator;
      Item : out Long_Float_Array);
   procedure Fill_Random_Greater_Than_0_To_Less_Than_1 (
      Gen : aliased in out Generator;
      Item : out Long_Float_Array);

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
   --    Fill_Random_... procedures.
   Min_Array_Length : constant Natural;

   --  Exceptions

   Use_Error : exception
      renames IO_Exceptions.Use_Error;

   --  Note: Initialize propagates Use_Error if it failed.

private

   pragma Compile_Time_Error (Long_Float'Size /= 64, "Long_Float'Size /= 64");

   subtype Unsigned_64 is Interfaces.Unsigned_64;

   type Unsigned_64_Array is
      array (Natural range <>) of aliased Interfaces.Unsigned_64;
   for Unsigned_64_Array'Alignment use 16;

   --  the parameters of DSFMT
   LOW_MASK : constant := 16#000FFFFFFFFFFFFF#;
   HIGH_CONST : constant := 16#3FF0000000000000#;
   SR : constant := 12;

   --  DSFMT generator has an internal state array of 128-bit integers, and N
   --    is its size.
   N : constant Natural := (MEXP - 128) / 104 + 1;
   --  N32 is the size of internal state array when regarded as an array of
   --    32-bit integers.
   N32 : constant Natural := N * 4;
   --  N64 is the size of internal state array when regarded as an array of
   --    64-bit integers.
   Min_Array_Length : constant Natural := N * 2;
   N64 : Natural renames Min_Array_Length;

   Max_Image_Width : constant Natural := (N64 + 2) * (64 / 4 + 1) + 32 / 4;

   --  128-bit data type
   type w128_t is array (0 .. 1) of Unsigned_64;
   for w128_t'Alignment use 16;
   pragma Suppress_Initialization (w128_t);
   type w128_t_Array is array (Natural range <>) of aliased w128_t
      with Convention => Ada_Pass_By_Reference;
   pragma Suppress_Initialization (w128_t_Array);

   subtype w128_t_Array_1 is w128_t_Array (0 .. 0);
   subtype w128_t_Array_N is w128_t_Array (0 .. N - 1);

   type State is record
      --  the 128-bit internal state array
      status : aliased w128_t_Array_N;
      lung : aliased w128_t; -- status (N)
      idx : Integer;
   end record;
   pragma Suppress_Initialization (State);

   type Generator is limited record
      dsfmt : aliased State := Initialize (Default_Initiator);
   end record;

end Ada.Numerics.dSFMT;
