with Ada.Exception_Identification.From_Here;
package body Ada.Sequential_IO is
   use Exception_Identification.From_Here;
   use type Streams.Stream_Element_Offset;

   procedure Close (File : in out File_Type) is
   begin
      Streams.Stream_IO.Close (Streams.Stream_IO.File_Type (File));
   end Close;

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

   procedure Delete (File : in out File_Type) is
   begin
      Streams.Stream_IO.Delete (Streams.Stream_IO.File_Type (File));
   end Delete;

   function End_Of_File (File : File_Type) return Boolean is
   begin
      if Mode (File) /= In_File then
         Raise_Exception (Mode_Error'Identity);
      end if;
      return Streams.Stream_IO.End_Of_File (
         Streams.Stream_IO.File_Type (File));
   end End_Of_File;

   function Form (File : File_Type) return String is
   begin
      return Streams.Stream_IO.Form (
         Streams.Stream_IO.File_Type (File));
   end Form;

   function Is_Open (File : File_Type) return Boolean is
   begin
      return Streams.Stream_IO.Is_Open (Streams.Stream_IO.File_Type (File));
   end Is_Open;

   function Mode (File : File_Type) return File_Mode is
   begin
      return File_Mode (Streams.Stream_IO.Mode (
         Streams.Stream_IO.File_Type (File)));
   end Mode;

   function Name (File : File_Type) return String is
   begin
      return Streams.Stream_IO.Name (Streams.Stream_IO.File_Type (File));
   end Name;

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

   procedure Read (File : File_Type; Item : out Element_Type) is
      Unit : constant := Streams.Stream_Element'Size;
      Size : constant Streams.Stream_Element_Count :=
         (Item'Size + Unit - 1) / Unit;
   begin
      if not Element_Type'Definite
         or else Element_Type'Has_Discriminants
      then
         --  indefinite (or unconstrained) types
         declare
            Read_Size : Streams.Stream_Element_Count;
         begin
            Streams.Stream_Element_Count'Read (Stream (File), Read_Size);
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
               Streams.Stream_IO.File_Type (File),
               Image,
               Last);
            if Last < Image'Last then
               Raise_Exception (End_Error'Identity);
            end if;
         end;
      end if;
   end Read;

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

   procedure Write (File : File_Type; Item : Element_Type) is
      Unit : constant := Streams.Stream_Element'Size;
      Size : constant Streams.Stream_Element_Count :=
         (Item'Size + Unit - 1) / Unit;
   begin
      if not Element_Type'Definite
         or else Element_Type'Has_Discriminants
      then
         --  indefinite (or unconstrained) types
         Streams.Stream_Element_Count'Write (Stream (File), Size);
      end if;
      declare
         Image : Streams.Stream_Element_Array (1 .. Size);
         for Image'Address use Item'Address;
      begin
         Streams.Stream_IO.Write (
            Streams.Stream_IO.File_Type (File),
            Image);
      end;
   end Write;

end Ada.Sequential_IO;
