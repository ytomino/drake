pragma License (Unrestricted);
--  optional runtime unit
package System.Wide_Startup is
   pragma Preelaborate;
   pragma Linker_Options ("-municode");

   wargc : Integer;
   pragma Export (C, wargc, "__drake_wargc");

   wargv : Address;
   pragma Export (C, wargv, "__drake_wargv");

   wenvp : Address;
   pragma Export (C, wenvp, "__drake_wenvp");

   function wmain (argc : Integer; argv : Address; envp : Address)
      return Integer;
   pragma Export (C, wmain, "wmain");

end System.Wide_Startup;
