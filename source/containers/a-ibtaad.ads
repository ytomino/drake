pragma License (Unrestricted);
--  implementation package
package Ada.Containers.Inside.Binary_Trees.Arne_Andersson.Debug is
   pragma Preelaborate;

   function Dump (
      Container : Node_Access;
      Marker : Node_Access;
      Message : String := "")
      return Boolean;

   function Validate (
      Container : Node_Access;
      Length : Count_Type;
      Level_Check : Boolean := True)
      return Boolean;

end Ada.Containers.Inside.Binary_Trees.Arne_Andersson.Debug;
