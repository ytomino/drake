pragma License (Unrestricted);
--  implementation unit
package System.IO_Options is
   pragma Preelaborate;

   --  parsing form parameter
   --  the format is "keyword1=value1,keyword2=value2,..."

   procedure Form_Parameter (
      Form : String;
      Keyword : String;
      First : out Positive;
      Last : out Natural);

end System.IO_Options;
