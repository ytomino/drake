with Ada.Exception_Identification.From_Here;
package body Ada.Sequential_IO is
   use Exception_Identification.From_Here;
   use type Streams.Stream_Element_Offset;

   procedure Create (
      File : in out File_Type;
      Mode : File_Mode := Out_File;
      Name : String := "";
      Form : String := "") is
   begin
      Streams.Stream_IO.Create (
         Streams.Stream_IO.File_Type (File),
         Streams.Stream_IO.File_Mode (Mode),
         Name,
         Form);
   end Create;

   procedure Open (
      File : in out File_Type;
      Mode : File_Mode;
      Name : String;
      Form : String := "") is
   begin
      Streams.Stream_IO.Open (
         Streams.Stream_IO.File_Type (File),
         Streams.Stream_IO.File_Mode (Mode),
         Name,
         Form);
   end Open;

   procedure Close (File : in out File_Type) is
   begin
      Streams.Stream_IO.Close (Streams.Stream_IO.File_Type (File));
   end Close;

   procedure Delete (File : in out File_Type) is
   begin
      Streams.Stream_IO.Delete (Streams.Stream_IO.File_Type (File));
   end Delete;

   procedure Reset (File : in out File_Type; Mode : File_Mode) is
   begin
      Streams.Stream_IO.Reset (
         Streams.Stream_IO.File_Type (File),
         Streams.Stream_IO.File_Mode (Mode));
   end Reset;

   procedure Reset (File : in out File_Type) is
   begin
      Streams.Stream_IO.Reset (Streams.Stream_IO.File_Type (File));
   end Reset;

   function Mode (
      File : File_Type)
      return File_Mode is
   begin
      return File_Mode (
         Streams.Stream_IO.Mode (
            Streams.Stream_IO.File_Type (File))); -- checking the predicate
   end Mode;

   function Name (
      File : File_Type)
      return String is
   begin
      return Streams.Stream_IO.Name (
         Streams.Stream_IO.File_Type (File)); -- checking the predicate
   end Name;

   function Form (
      File : File_Type)
      return String is
   begin
      return Streams.Stream_IO.Form (
         Streams.Stream_IO.File_Type (File)); -- checking the predicate
   end Form;

   function Is_Open (File : File_Type) return Boolean is
   begin
      return Streams.Stream_IO.Is_Open (Streams.Stream_IO.File_Type (File));
   end Is_Open;

   procedure Flush (
      File : File_Type) is
   begin
      Streams.Stream_IO.Flush (
         Streams.Stream_IO.File_Type (File)); -- checking the predicate
   end Flush;

   procedure Read (
      File : File_Type;
      Item : out Element_Type)
   is
      Unit : constant := Streams.Stream_Element'Size;
      Size : constant Streams.Stream_Element_Count :=
         (Item'Size + Unit - 1) / Unit;
   begin
      if not Element_Type'Definite or else Element_Type'Has_Discriminants then
         --  indefinite (or unconstrained) types
         declare
            Read_Size : Streams.Stream_Element_Count;
         begin
            Streams.Stream_Element_Count'Read (
               Stream (File), -- checking the predicate
               Read_Size);
            declare
               Image : Streams.Stream_Element_Array (1 .. Read_Size);
               for Image'Address use Item'Address;
               Last : Streams.Stream_Element_Offset;
            begin
               Streams.Stream_IO.Read (
                  Streams.Stream_IO.File_Type (File),
                  Image,
                  Last);
               if Last < Image'Last then
                  Raise_Exception (Data_Error'Identity);
               end if;
            end;
         end;
      else
         declare
            Image : Streams.Stream_Element_Array (1 .. Size);
            for Image'Address use Item'Address;
            Last : Streams.Stream_Element_Offset;
         begin
            Streams.Stream_IO.Read (
               Streams.Stream_IO.File_Type (File), -- checking the predicate
               Image,
               Last);
            if Last < Image'Last then
               Raise_Exception (End_Error'Identity);
            end if;
         end;
      end if;
   end Read;

   procedure Write (
      File : File_Type;
      Item : Element_Type)
   is
      Unit : constant := Streams.Stream_Element'Size;
      Size : constant Streams.Stream_Element_Count :=
         (Item'Size + Unit - 1) / Unit;
   begin
      if not Element_Type'Definite or else Element_Type'Has_Discriminants then
         --  indefinite (or unconstrained) types
         Streams.Stream_Element_Count'Write (
            Stream (File), -- checking the predicate, or below
            Size);
      end if;
      declare
         Image : Streams.Stream_Element_Array (1 .. Size);
         for Image'Address use Item'Address;
      begin
         Streams.Stream_IO.Write (
            Streams.Stream_IO.File_Type (File), -- checking the predicate
            Image);
      end;
   end Write;

   function End_Of_File (
      File : File_Type)
      return Boolean
   is
      pragma Check (Dynamic_Predicate,
         Check => Is_Open (File) or else raise Status_Error);
      pragma Check (Dynamic_Predicate,
         Check => Mode (File) = In_File or else raise Mode_Error);
   begin
      return Streams.Stream_IO.End_Of_File (
         Streams.Stream_IO.File_Type (File));
   end End_Of_File;

end Ada.Sequential_IO;
