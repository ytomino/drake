pragma License (Unrestricted);
--  translated unit from MurmurHash3
--
--  MurmurHash3 was written by Austin Appleby, and is placed in the public
--  domain. The author hereby disclaims copyright to this source code.
--
--   Ada version by yt
--
package Ada.Containers.Murmur_Hash_3 is
   pragma Pure;

   type State is private;

   function Initialize (Initiator : Hash_Type) return State;
   pragma Inline (Initialize);

   procedure Update (S : in out State; Item : Hash_Type);
   procedure Update (S : in out State; Item : Count_Type);

   procedure Finalize (S : in out State; Digest : out Hash_Type);

private

   pragma Compile_Time_Error (Hash_Type'Size /= 32, "size mismatch");

   type State is new Hash_Type;

end Ada.Containers.Murmur_Hash_3;
