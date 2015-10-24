pragma Check_Policy (Validate => Ignore);
with System.Once;
package body Ada.Strings.Canonical_Composites is

   type Long_Boolean is new Boolean;
   for Long_Boolean'Size use Long_Integer'Size;

   function expect (exp, c : Long_Boolean) return Long_Boolean
      with Import,
         Convention => Intrinsic, External_Name => "__builtin_expect";

   procedure unreachable
      with Import,
         Convention => Intrinsic, External_Name => "__builtin_unreachable";
   pragma No_Return (unreachable);

   --  decomposition

   procedure D_Fill (Map : out D_Map_Array);
   procedure D_Fill (Map : out D_Map_Array) is
      procedure Fill (
         Map : in out D_Map_Array;
         I : in out Positive;
         Table : UCD.Map_16x1_Type);
      procedure Fill (
         Map : in out D_Map_Array;
         I : in out Positive;
         Table : UCD.Map_16x1_Type) is
      begin
         for J in Table'Range loop
            Map (I).From := Wide_Wide_Character'Val (Table (J).Code);
            Map (I).To (1) := Wide_Wide_Character'Val (Table (J).Mapping);
            for K in 2 .. Expanding loop
               Map (I).To (K) := Wide_Wide_Character'Val (0);
            end loop;
            I := I + 1;
         end loop;
      end Fill;
      procedure Fill (
         Map : in out D_Map_Array;
         I : in out Positive;
         Table : UCD.Map_16x2_Type);
      procedure Fill (
         Map : in out D_Map_Array;
         I : in out Positive;
         Table : UCD.Map_16x2_Type) is
      begin
         for J in Table'Range loop
            Map (I).From := Wide_Wide_Character'Val (Table (J).Code);
            for K in 1 .. 2 loop
               Map (I).To (K) :=
                  Wide_Wide_Character'Val (Table (J).Mapping (K));
            end loop;
            for K in 3 .. Expanding loop
               Map (I).To (K) := Wide_Wide_Character'Val (0);
            end loop;
            I := I + 1;
         end loop;
      end Fill;
      procedure Fill (
         Map : in out D_Map_Array;
         I : in out Positive;
         Table : UCD.Map_32x2_Type);
      procedure Fill (
         Map : in out D_Map_Array;
         I : in out Positive;
         Table : UCD.Map_32x2_Type) is
      begin
         for J in Table'Range loop
            Map (I).From := Wide_Wide_Character'Val (Table (J).Code);
            for K in 1 .. 2 loop
               Map (I).To (K) :=
                  Wide_Wide_Character'Val (Table (J).Mapping (K));
            end loop;
            for K in 3 .. Expanding loop
               Map (I).To (K) := Wide_Wide_Character'Val (0);
            end loop;
            I := I + 1;
         end loop;
      end Fill;
   begin
      --  make table
      declare
         I : Positive := Map'First;
      begin
         --  16#00C0# ..
         Fill (Map, I, UCD.Normalization.NFD_D_Table_XXXX);
         --  16#0340#
         Fill (Map, I, UCD.Normalization.NFD_S_Table_XXXX);
         --  16#0344# ..
         Fill (Map, I, UCD.Normalization.NFD_E_Table_XXXX);
         --  16#1109A# ..
         Fill (Map, I, UCD.Normalization.NFD_D_Table_XXXXXXXX);
         --  16#1D15E#
         Fill (Map, I, UCD.Normalization.NFD_E_Table_XXXXXXXX);
         pragma Check (Validate, I = Map'Last + 1);
      end;
      --  sort
      for I in Map'First + 1 .. Map'Last loop
         for J in reverse Map'First .. I - 1 loop
            exit when Map (J).From <= Map (J + 1).From;
            declare
               T : constant D_Map_Element := Map (J);
            begin
               Map (J) := Map (J + 1);
               Map (J + 1) := T;
            end;
         end loop;
      end loop;
   end D_Fill;

   D_Flag : aliased System.Once.Flag := 0;

   procedure D_Init;
   procedure D_Init is
   begin
      D_Map := new D_Map_Array;
      D_Fill (D_Map.all);
      --  expanding re-decomposable
      loop
         declare
            Expanded : Boolean := False;
         begin
            for I in D_Map'Range loop
               declare
                  To : Decomposed_Wide_Wide_String
                     renames D_Map (I).To;
                  To_Last : Natural := Decomposed_Length (To);
                  J : Natural := To_Last;
               begin
                  while J >= To'First loop
                     declare
                        D : constant Natural := D_Find (To (J));
                     begin
                        if D > 0 then
                           Expanded := True;
                           declare
                              R_Length : constant Natural :=
                                 Decomposed_Length (D_Map (D).To);
                           begin
                              To (J + R_Length .. To_Last + R_Length - 1) :=
                                 To (J + 1 .. To_Last);
                              To (J .. J + R_Length - 1) :=
                                 D_Map (D).To (1 .. R_Length);
                              To_Last := To_Last + R_Length - 1;
                              pragma Check (Validate, To_Last <= Expanding);
                              J := J + R_Length - 1;
                           end;
                        else
                           J := J - 1;
                        end if;
                     end;
                  end loop;
               end;
            end loop;
            exit when not Expanded;
         end;
      end loop;
   end D_Init;

   Unexpanded_D_Flag : aliased System.Once.Flag := 0;

   procedure Unexpanded_D_Init;
   procedure Unexpanded_D_Init is
   begin
      Unexpanded_D_Map := new D_Map_Array;
      D_Fill (Unexpanded_D_Map.all);
   end Unexpanded_D_Init;

   --  implementation of decomposition

   function Decomposed_Length (Item : Decomposed_Wide_Wide_String)
      return Natural is
   begin
      for I in reverse Item'Range loop
         if Item (I) /= Wide_Wide_Character'Val (0) then
            return I;
         end if;
      end loop;
      pragma Check (Validate, Standard.False);
      unreachable;
   end Decomposed_Length;

   function D_Find (Item : Wide_Wide_Character) return Natural is
      L : Positive := D_Map'First;
      H : Natural := D_Map'Last;
   begin
      loop
         declare
            type Unsigned is mod 2 ** Integer'Size;
            M : constant Positive := Integer (Unsigned (L + H) / 2);
            M_Item : D_Map_Element
               renames D_Map (M);
         begin
            if Item < M_Item.From then
               H := M - 1;
            elsif expect (Long_Boolean (Item > M_Item.From), True) then
               L := M + 1;
            else
               return M;
            end if;
         end;
         exit when L > H;
      end loop;
      return 0;
   end D_Find;

   procedure Initialize_D is
   begin
      System.Once.Initialize (D_Flag'Access, D_Init'Access);
   end Initialize_D;

   procedure Initialize_Unexpanded_D is
   begin
      System.Once.Initialize (
         Unexpanded_D_Flag'Access,
         Unexpanded_D_Init'Access);
   end Initialize_Unexpanded_D;

   --  composition

   C_Flag : aliased System.Once.Flag := 0;

   procedure C_Init;
   procedure C_Init is
      procedure Fill (
         Map : in out C_Map_Array;
         I : in out Positive;
         Table : UCD.Map_16x2_Type);
      procedure Fill (
         Map : in out C_Map_Array;
         I : in out Positive;
         Table : UCD.Map_16x2_Type) is
      begin
         for J in Table'Range loop
            for K in 1 .. 2 loop
               Map (I).From (K) :=
                  Wide_Wide_Character'Val (Table (J).Mapping (K));
            end loop;
            Map (I).To := Wide_Wide_Character'Val (Table (J).Code);
            I := I + 1;
         end loop;
      end Fill;
      procedure Fill (
         Map : in out C_Map_Array;
         I : in out Positive;
         Table : UCD.Map_32x2_Type);
      procedure Fill (
         Map : in out C_Map_Array;
         I : in out Positive;
         Table : UCD.Map_32x2_Type) is
      begin
         for J in Table'Range loop
            for K in 1 .. 2 loop
               Map (I).From (K) :=
                  Wide_Wide_Character'Val (Table (J).Mapping (K));
            end loop;
            Map (I).To := Wide_Wide_Character'Val (Table (J).Code);
            I := I + 1;
         end loop;
      end Fill;
   begin
      --  initialize D table, too
      Initialize_D;
      --  make table
      C_Map := new C_Map_Array;
      declare
         I : Positive := C_Map'First;
      begin
         --  (16#0041#, 16#0300#) ..
         Fill (C_Map.all, I, UCD.Normalization.NFD_D_Table_XXXX);
         --  (16#11099#, 16#110BA#) ..
         Fill (C_Map.all, I, UCD.Normalization.NFD_D_Table_XXXXXXXX);
         pragma Check (Validate, I = C_Map'Last + 1);
      end;
      --  sort
      for I in C_Map'First + 1 .. C_Map'Last loop
         for J in reverse C_Map'First .. I - 1 loop
            exit when C_Map (J).From <= C_Map (J + 1).From;
            declare
               T : constant C_Map_Element := C_Map (J);
            begin
               C_Map (J) := C_Map (J + 1);
               C_Map (J + 1) := T;
            end;
         end loop;
      end loop;
   end C_Init;

   --  implementation of composition

   function C_Find (Item : Composing_Wide_Wide_String) return Natural is
      L : Positive := C_Map'First;
      H : Natural := C_Map'Last;
   begin
      loop
         declare
            type Unsigned is mod 2 ** Integer'Size;
            M : constant Positive := Integer (Unsigned (L + H) / 2);
            M_Item : C_Map_Element
               renames C_Map (M);
         begin
            if Item < M_Item.From then
               H := M - 1;
            elsif expect (Long_Boolean (Item > M_Item.From), True) then
               L := M + 1;
            else
               return M;
            end if;
         end;
         exit when L > H;
      end loop;
      return 0;
   end C_Find;

   procedure Initialize_C is
   begin
      System.Once.Initialize (C_Flag'Access, C_Init'Access);
   end Initialize_C;

end Ada.Strings.Canonical_Composites;
