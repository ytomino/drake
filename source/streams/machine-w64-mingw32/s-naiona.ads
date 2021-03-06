pragma License (Unrestricted);
--  implementation unit specialized for Windows
package System.Native_IO.Names is
   pragma Preelaborate;

   procedure Open_Ordinary (
      Method : Open_Method;
      Handle : aliased out Handle_Type;
      Mode : File_Mode;
      Name : String;
      Out_Name : aliased out Name_Pointer; -- null
      Form : Packed_Form);

   procedure Get_Full_Name (
      Handle : Handle_Type;
      Has_Full_Name : in out Boolean;
      Name : in out Name_Pointer;
      Is_Standard : Boolean;
      Raise_On_Error : Boolean);

end System.Native_IO.Names;
