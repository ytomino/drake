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
--
--   Ada version by yt
--
with Ada.IO_Exceptions;
with Interfaces;
package Ada.Numerics.SFMT is
   pragma Preelaborate;
   use type Interfaces.Unsigned_32;
   use type Interfaces.Unsigned_64;

   subtype Unsigned_32 is Interfaces.Unsigned_32;

   function Shift_Left (Left : Unsigned_32; Right : Natural)
      return Unsigned_32
      renames Interfaces.Shift_Left;
   function Shift_Right (Left : Unsigned_32; Right : Natural)
      return Unsigned_32
      renames Interfaces.Shift_Right;

   type Unsigned_32_Array is array (Natural range <>) of
      aliased Interfaces.Unsigned_32;
   for Unsigned_32_Array'Alignment use 16;

   subtype Unsigned_64 is Interfaces.Unsigned_64;

   function Shift_Left (Left : Unsigned_64; Right : Natural)
      return Unsigned_64
      renames Interfaces.Shift_Left;
   function Shift_Right (Left : Unsigned_64; Right : Natural)
      return Unsigned_64
      renames Interfaces.Shift_Right;

   type Unsigned_64_Array is array (Natural range <>) of
      aliased Interfaces.Unsigned_64;
   for Unsigned_64_Array'Alignment use 16;

   Use_Error : exception
      renames Ada.IO_Exceptions.Use_Error;
   --  Note: Use_Error may be raised from Initialize (Generic_Initiator)

private

   function func1 (x : Unsigned_32) return Unsigned_32;
   pragma Inline (func1);
   function func2 (x : Unsigned_32) return Unsigned_32;
   pragma Inline (func2);

   --  for Image/Value
   procedure Hex_Put (To : out String; Item : Unsigned_32);
   procedure Hex_Get (From : String; Item : out Unsigned_32);

end Ada.Numerics.SFMT;
