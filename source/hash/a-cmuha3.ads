pragma License (Unrestricted); -- public domain
--  translated unit from MurmurHash3
--
--  MurmurHash3 was written by Austin Appleby, and is placed in the public
--  domain. The author hereby disclaims copyright to this source code.
--
--  Ada version: 2014 yt
package Ada.Containers.Murmur_Hash_3 is
   --  32bit version MurmurHash3 that targets low latency for hash table use.
   pragma Pure;

   type State is private;

   function Initialize (Initiator : Hash_Type) return State;
   pragma Inline (Initialize);

   --  Body

   procedure Update (S : in out State; Item : Hash_Type);

   --  Tail

   type Hash_8 is mod 2 ** 8;
   for Hash_8'Size use 8;
   type Hash_16 is mod 2 ** 16;
   for Hash_16'Size use 16;
   type Hash_24 is mod 2 ** 24;
   for Hash_24'Size use 24;

   procedure Update (S : in out State; Item : Hash_8);
   procedure Update (S : in out State; Item : Hash_16);
   procedure Update (S : in out State; Item : Hash_24);

   --  Finalization

   procedure Finalize (S : State; Digest : out Hash_Type);

private

   pragma Compile_Time_Error (Hash_Type'Size /= 32, "size mismatch");

   type State is record
      h1 : Hash_Type;
      len : Count_Type;
   end record;
   pragma Suppress_Initialization (State);

end Ada.Containers.Murmur_Hash_3;
