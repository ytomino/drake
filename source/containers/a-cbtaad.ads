pragma License (Unrestricted);
--  implementation unit
package Ada.Containers.Binary_Trees.Arne_Andersson.Debug is
   pragma Preelaborate;

   function Root (Node : not null Node_Access) return not null Node_Access;

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

end Ada.Containers.Binary_Trees.Arne_Andersson.Debug;
