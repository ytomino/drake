package body Ada.Containers.Murmur_Hash_3 is

   --  use MurmurHash3_x86_32

   function Initialize (Initiator : Hash_Type) return State is
   begin
      return State (Initiator);
   end Initialize;

   procedure Update (S : in out State; Item : Hash_Type) is
      c1 : constant := 16#cc9e2d51#;
      c2 : constant := 16#1b873593#;
      k1 : State := State (Item);
   begin
      k1 := k1 * c1;
      k1 := Rotate_Left (k1, 15);
      k1 := k1 * c2;
      S := S xor k1;
      S := Rotate_Left (S, 13);
      S := S * 5 + 16#e6546b64#;
   end Update;

   procedure Update (S : in out State; Item : Count_Type) is
   begin
      S := S xor State (Item);
   end Update;

   procedure Finalize (S : in out State; Digest : out Hash_Type) is
   begin
      S := S xor Shift_Right (S, 16);
      S := S * 16#85ebca6b#;
      S := S xor Shift_Right (S, 13);
      S := S * 16#c2b2ae35#;
      S := S xor Shift_Right (S, 16);
      Digest := Hash_Type (S);
   end Finalize;

end Ada.Containers.Murmur_Hash_3;
