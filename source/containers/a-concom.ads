pragma License (Unrestricted);
--  extended unit
package Ada.Containers.Composites is
   pragma Pure;

   --  comparators

   generic
      type Element_Type (<>) is limited private;
      with function "<" (Left, Right : Element_Type) return Boolean is <>;
   function LE (Left, Right : Element_Type) return Boolean;

   generic
      type Element_Type (<>) is limited private;
      with function "<" (Left, Right : Element_Type) return Boolean is <>;
   function GT (Left, Right : Element_Type) return Boolean;

   generic
      type Element_Type (<>) is limited private;
      with function "<" (Left, Right : Element_Type) return Boolean is <>;
   function GE (Left, Right : Element_Type) return Boolean;

   generic
      type Element_Type (<>) is limited private;
      with function "<" (Left, Right : Element_Type) return Boolean is <>;
   function Compare (Left, Right : Element_Type) return Integer;

   --  composite function

   generic
      type A_Type (<>) is limited private;
      type Q_Type (<>) is limited private;
      type R_Type (<>) is limited private;
      with function F1 (A : A_Type) return Q_Type;
      with function F2 (Q : Q_Type) return R_Type;
   function Composite (A : A_Type) return R_Type;

   --  X means in
   --  Y means in out
   --  Z means out
   --  R means return

   generic
      type A1_Type (<>) is limited private;
      type A2_Type (<>) is limited private;
      type R_Type (<>) is limited private;
      with function F (A1 : A1_Type; A2 : A2_Type) return R_Type;
      A1 : A1_Type;
   function XXR_Bind_1st (A2 : A2_Type) return R_Type;

   generic
      type A1_Type (<>) is limited private;
      type A2_Type (<>) is limited private;
      type R_Type (<>) is limited private;
      with function F (A1 : A1_Type; A2 : A2_Type) return R_Type;
      A2 : A2_Type;
   function XXR_Bind_2nd (A1 : A1_Type) return R_Type;

   generic
      type A1_Type (<>) is limited private;
      with function F (A1 : A1_Type) return Boolean;
   function XR_Not (A1 : A1_Type) return Boolean;

   generic
      type A1_Type (<>) is limited private;
      type A2_Type (<>) is limited private;
      with procedure P (A1 : A1_Type; A2 : out A2_Type);
   procedure XZ_Inout_1st (A1 : in out A1_Type; A2 : out A2_Type);

end Ada.Containers.Composites;
