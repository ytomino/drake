pragma License (Unrestricted);
--  optional runtime unit specialized for Windows
package System.Wide_Startup is
   pragma Preelaborate;
   pragma Linker_Options ("-municode");

   wargc : Integer
      with Export, Convention => C, External_Name => "__drake_wargc";

   wargv : Address
      with Export, Convention => C, External_Name => "__drake_wargv";

   wenvp : Address
      with Export, Convention => C, External_Name => "__drake_wenvp";

   function wmain (argc : Integer; argv : Address; envp : Address)
      return Integer
      with Export, Convention => C, External_Name => "wmain";

end System.Wide_Startup;
