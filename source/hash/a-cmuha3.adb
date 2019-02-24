package body Ada.Containers.Murmur_Hash_3 is
   --  use MurmurHash3_x86_32

   procedure Step (h1 : in out Hash_Type; Item : Hash_Type);
   procedure Step (h1 : in out Hash_Type; Item : Hash_Type) is
      c1 : constant := 16#cc9e2d51#;
      c2 : constant := 16#1b873593#;
      k1 : Hash_Type := Item;
   begin
      k1 := k1 * c1;
      k1 := Rotate_Left (k1, 15);
      k1 := k1 * c2;
      h1 := h1 xor k1;
   end Step;

   --  implementation

   function Initialize (Initiator : Hash_Type) return State is
   begin
      return (h1 => Initiator, len => 0);
   end Initialize;

   procedure Update (S : in out State; Item : Hash_Type) is
   begin
      Step (S.h1, Item);
      S.h1 := Rotate_Left (S.h1, 13);
      S.h1 := S.h1 * 5 + 16#e6546b64#;
      S.len := S.len + 4;
   end Update;

   procedure Update (S : in out State; Item : Hash_8) is
   begin
      Step (S.h1, Hash_Type'Mod (Item));
      S.len := S.len + 1;
   end Update;

   procedure Update (S : in out State; Item : Hash_16) is
   begin
      Step (S.h1, Hash_Type'Mod (Item));
      S.len := S.len + 2;
   end Update;

   procedure Update (S : in out State; Item : Hash_24) is
   begin
      Step (S.h1, Hash_Type'Mod (Item));
      S.len := S.len + 3;
   end Update;

   procedure Finalize (S : State; Digest : out Hash_Type) is
   begin
      Digest := S.h1 xor Hash_Type'Mod (S.len);
      Digest := Digest xor Shift_Right (Digest, 16);
      Digest := Digest * 16#85ebca6b#;
      Digest := Digest xor Shift_Right (Digest, 13);
      Digest := Digest * 16#c2b2ae35#;
      Digest := Digest xor Shift_Right (Digest, 16);
   end Finalize;

end Ada.Containers.Murmur_Hash_3;
