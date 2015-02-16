pragma License (Unrestricted);
--  runtime unit
package System.Unwind.Standard is
   pragma Preelaborate;

   --  Standard.Constraint_Error / Numeric_Error (s-stalib.ads)
   Constraint_Error_Name : aliased constant String (1 .. 17) :=
      "CONSTRAINT_ERROR" & Character'Val (0);
   Constraint_Error : aliased constant Exception_Data := (
      Not_Handled_By_Others => False,
      Lang => 'A',
      Name_Length => Constraint_Error_Name'Length,
      Full_Name => Constraint_Error_Name'Address,
      HTable_Ptr => null,
      Import_Code => 0,
      Raise_Hook => null);
   pragma Export (Ada, Constraint_Error, "constraint_error");

   --  Standard.Program_Error (s-stalib.ads)
   Program_Error_Name : aliased constant String (1 .. 14) :=
      "PROGRAM_ERROR" & Character'Val (0);
   Program_Error : aliased constant Exception_Data := (
      Not_Handled_By_Others => False,
      Lang => 'A',
      Name_Length => Program_Error_Name'Length,
      Full_Name => Program_Error_Name'Address,
      HTable_Ptr => null,
      Import_Code => 0,
      Raise_Hook => null);
   pragma Export (Ada, Program_Error, "program_error");

   --  Standard.Storage_Error (s-stalib.ads)
   Storage_Error_Name : aliased constant String (1 .. 14) :=
      "STORAGE_ERROR" & Character'Val (0);
   Storage_Error : aliased constant Exception_Data := (
      Not_Handled_By_Others => False,
      Lang => 'A',
      Name_Length => Storage_Error_Name'Length,
      Full_Name => Storage_Error_Name'Address,
      HTable_Ptr => null,
      Import_Code => 0,
      Raise_Hook => null);
   pragma Export (Ada, Storage_Error, "storage_error");

   --  Standard.Tasking_Error (s-stalib.ads)
   Tasking_Error_Name : aliased constant String (1 .. 14) :=
      "TASKING_ERROR" & Character'Val (0);
   Tasking_Error : aliased constant Exception_Data := (
      Not_Handled_By_Others => False,
      Lang => 'A',
      Name_Length => Tasking_Error_Name'Length,
      Full_Name => Tasking_Error_Name'Address,
      HTable_Ptr => null,
      Import_Code => 0,
      Raise_Hook => null);
   pragma Export (Ada, Tasking_Error, "tasking_error");

   --  Standard'Abort_Signal (s-stalib.ads)
   Abort_Signal_Name : aliased constant String (1 .. 14) :=
      "_ABORT_SIGNAL" & Character'Val (0);
   Abort_Signal : aliased constant Exception_Data := (
      Not_Handled_By_Others => True,
      Lang => 'A',
      Name_Length => Abort_Signal_Name'Length,
      Full_Name => Abort_Signal_Name'Address,
      HTable_Ptr => null,
      Import_Code => 0,
      Raise_Hook => null);
   pragma Export (Ada, Abort_Signal, "_abort_signal");

end System.Unwind.Standard;
