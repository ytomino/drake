pragma License (Unrestricted);
package System.Machine_Code is
   pragma Pure;

   type Asm_Output_Operand is private;
   No_Output_Operands : constant Asm_Output_Operand;
   type Asm_Output_Operand_List is
      array (Integer range <>) of Asm_Output_Operand;

   type Asm_Input_Operand is private;
   No_Input_Operands : constant Asm_Input_Operand;
   type Asm_Input_Operand_List is
      array (Integer range <>) of Asm_Input_Operand;

   type Asm_Insn (<>) is private;

   procedure Asm (
      Template : String;
      Outputs : Asm_Output_Operand_List;
      Inputs : Asm_Input_Operand_List;
      Clobber : String := "";
      Volatile : Boolean := False)
      with Import, Convention => Intrinsic;
   procedure Asm (
      Template : String;
      Outputs : Asm_Output_Operand := No_Output_Operands;
      Inputs : Asm_Input_Operand_List;
      Clobber : String := "";
      Volatile : Boolean := False)
      with Import, Convention => Intrinsic;
   procedure Asm (
      Template : String;
      Outputs : Asm_Output_Operand_List;
      Inputs : Asm_Input_Operand := No_Input_Operands;
      Clobber : String := "";
      Volatile : Boolean := False)
      with Import, Convention => Intrinsic;
   procedure Asm (
      Template : String;
      Outputs : Asm_Output_Operand := No_Output_Operands;
      Inputs : Asm_Input_Operand := No_Input_Operands;
      Clobber : String := "";
      Volatile : Boolean := False)
      with Import, Convention => Intrinsic;
   function Asm (
      Template : String;
      Outputs : Asm_Output_Operand_List;
      Inputs : Asm_Input_Operand_List;
      Clobber : String := "";
      Volatile : Boolean := False)
      return Asm_Insn
      with Import, Convention => Intrinsic;
   function Asm (
      Template : String;
      Outputs : Asm_Output_Operand := No_Output_Operands;
      Inputs : Asm_Input_Operand_List;
      Clobber : String := "";
      Volatile : Boolean := False)
      return Asm_Insn
      with Import, Convention => Intrinsic;
   function Asm (
      Template : String;
      Outputs : Asm_Output_Operand_List;
      Inputs : Asm_Input_Operand := No_Input_Operands;
      Clobber : String := "";
      Volatile : Boolean := False)
      return Asm_Insn
      with Import, Convention => Intrinsic;
   function Asm (
      Template : String;
      Outputs : Asm_Output_Operand := No_Output_Operands;
      Inputs : Asm_Input_Operand := No_Input_Operands;
      Clobber : String := "";
      Volatile : Boolean := False)
      return Asm_Insn
      with Import, Convention => Intrinsic;

private

   type Asm_Output_Operand is new Integer;
   No_Output_Operands : constant Asm_Output_Operand := 0;

   type Asm_Input_Operand is new Integer;
   No_Input_Operands : constant Asm_Input_Operand := 0;

   type Asm_Insn is new Integer;

end System.Machine_Code;
