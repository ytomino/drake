package body Ada.Containers.Composites is

   function LE (Left, Right : Element_Type) return Boolean is
   begin
      return not (Right < Left);
   end LE;

   function GT (Left, Right : Element_Type) return Boolean is
   begin
      return Right < Left;
   end GT;

   function GE (Left, Right : Element_Type) return Boolean is
   begin
      return not (Left < Right);
   end GE;

   function Compare (Left, Right : Element_Type) return Integer is
   begin
      if Left < Right then
         return -1;
      elsif Right < Left then
         return 1;
      else
         return 0;
      end if;
   end Compare;

   function Composite (A : A_Type) return R_Type is
   begin
      return F2 (F1 (A));
   end Composite;

   function XXR_Bind_1st (A2 : A2_Type) return R_Type is
   begin
      return F (A1, A2);
   end XXR_Bind_1st;

   function XXR_Bind_2nd (A1 : A1_Type) return R_Type is
   begin
      return F (A1, A2);
   end XXR_Bind_2nd;

   function XR_Not (A1 : A1_Type) return Boolean is
   begin
      return not F (A1);
   end XR_Not;

   procedure XZ_Inout_1st (A1 : in out A1_Type; A2 : out A2_Type) is
      pragma Unmodified (A1);
   begin
      P (A1, A2);
   end XZ_Inout_1st;

end Ada.Containers.Composites;
