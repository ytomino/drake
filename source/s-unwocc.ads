pragma License (Unrestricted);
--  runtime unit
package System.Unwind.Occurrences is
   pragma Preelaborate;

   --  implementation for catching object (a-except-2005.adb)
   procedure Save_Occurrence (
      Target : out Exception_Occurrence;
      Source : Exception_Occurrence);
   pragma Export (Ada, Save_Occurrence,
      "ada__exceptions__save_occurrence");

   --  equivalent to Append_Info_Exception_Information (a-exexda.adb)
   procedure Exception_Information (
      X : Exception_Occurrence;
      Params : Address;
      Put : not null access procedure (S : String; Params : Address);
      New_Line : not null access procedure (Params : Address));

end System.Unwind.Occurrences;
