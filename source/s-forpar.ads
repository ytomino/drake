pragma License (Unrestricted);
--  implementation unit
package System.Form_Parameters is
   pragma Preelaborate;

   --  parsing form parameter
   --  the format is "keyword1=value1,keyword2=value2,..."

   procedure Get (
      Form : String;
      Keyword_First : out Positive;
      Keyword_Last : out Natural;
      Item_First : out Positive;
      Item_Last : out Natural;
      Last : out Natural);

end System.Form_Parameters;
