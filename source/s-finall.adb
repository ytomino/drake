with Ada.Unchecked_Conversion;
with System.Startup;
with System.Unwind.Occurrences;
with System.Unwind.Raising;
package body System.Finally is
   pragma Suppress (All_Checks);
   use type Ada.Exceptions.Exception_Occurrence_Access;
   use type Startup.Finalize_Library_Objects_Handler;
   use type Unwind.Exception_Data_Access;

   procedure Finalize_Library_Objects;
   pragma Linker_Destructor (Finalize_Library_Objects); -- after atexit

   procedure Finalize_Library_Objects is
   begin
      if Startup.Finalize_Library_Objects /= null then
         Startup.Finalize_Library_Objects.all;
      end if;
   end Finalize_Library_Objects;

   type Uninitialized_Exception_Occurrence is record
      X : Unwind.Exception_Occurrence;
   end record;
   pragma Suppress_Initialization (Uninitialized_Exception_Occurrence);

   Library_Exception : Uninitialized_Exception_Occurrence;
   Library_Exception_Set : Boolean := False;

   --  implementation

   procedure Save_Library_Occurrence (
      E : Ada.Exceptions.Exception_Occurrence_Access)
   is
      function Cast is
         new Ada.Unchecked_Conversion (
            Ada.Exceptions.Exception_Occurrence_Access,
            Unwind.Exception_Occurrence_Access);
   begin
      if not Library_Exception_Set then
         Library_Exception_Set := True;
         if E /= null then
            Unwind.Occurrences.Save_Occurrence (
               Library_Exception.X,
               Cast (E).all);
         end if;
      end if;
   end Save_Library_Occurrence;

   procedure Reraise_Library_Exception_If_Any is
   begin
      if Library_Exception_Set then
         declare
            X : Unwind.Exception_Occurrence
               renames Library_Exception.X;
         begin
            if X.Id = null then
               Unwind.Raising.Finalize_Raised_Exception;
            else
               Unwind.Raising.Reraise_From_Controlled_Operation (X);
            end if;
         end;
      end if;
   end Reraise_Library_Exception_If_Any;

end System.Finally;
