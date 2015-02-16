pragma License (Unrestricted);
--  implementation unit required by compiler
package System.Wid_LLI is
   pragma Pure;

   --  required for Long_Long_Integer'Width by compiler (s-widlli.ads)
   function Width_Long_Long_Integer (Lo, Hi : Long_Long_Integer)
      return Natural;

end System.Wid_LLI;
