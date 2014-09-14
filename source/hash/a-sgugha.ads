pragma License (Unrestricted);
--  generic implementation of Ada.Strings.Unbounded.Hash
with Ada.Containers;
generic
   with function Fixed_Hash (Key : String_Type)
      return Ada.Containers.Hash_Type;
function Ada.Strings.Generic_Unbounded.Generic_Hash (Key : Unbounded_String)
   return Ada.Containers.Hash_Type;
pragma Preelaborate (Ada.Strings.Generic_Unbounded.Generic_Hash);
