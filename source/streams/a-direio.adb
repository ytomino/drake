with Ada.Exception_Identification.From_Here;
package body Ada.Direct_IO is
   use Exception_Identification.From_Here;
   use type Streams.Stream_Element_Offset;

   To_Mode : constant array (File_Mode) of Streams.Stream_IO.File_Mode := (
      In_File => Streams.Stream_IO.In_File,
      Inout_File => Streams.Stream_IO.Append_File,
      Out_File => Streams.Stream_IO.Out_File);

   From_Mode : constant array (Streams.Stream_IO.File_Mode) of File_Mode := (
      Streams.Stream_IO.In_File => In_File,
      Streams.Stream_IO.Out_File => Out_File,
      Streams.Stream_IO.Append_File => Inout_File);

   --  implementation

   procedure Create (
      File : in out File_Type;
      Mode : File_Mode := Inout_File;
      Name : String := "";
      Form : String := "") is
   begin
      Streams.Stream_IO.Create (
         Streams.Stream_IO.File_Type (File),
         To_Mode (Mode),
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
         To_Mode (Mode),
         Name,
         Form);
      --  move to first, because Inout_File is mapped to Append_File
      Streams.Stream_IO.Set_Index (
         Streams.Stream_IO.File_Type (File),
         1);
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
         To_Mode (Mode));
      Streams.Stream_IO.Set_Index (Streams.Stream_IO.File_Type (File), 1);
   end Reset;

   procedure Reset (File : in out File_Type) is
   begin
      Streams.Stream_IO.Reset (Streams.Stream_IO.File_Type (File));
      Streams.Stream_IO.Set_Index (Streams.Stream_IO.File_Type (File), 1);
   end Reset;

   function Mode (
      File : File_Type)
      return File_Mode is
   begin
      return From_Mode (Streams.Stream_IO.Mode (
         Streams.Stream_IO.File_Type (File)));
   end Mode;

   function Name (
      File : File_Type)
      return String is
   begin
      return Streams.Stream_IO.Name (
         Streams.Stream_IO.File_Type (File));
   end Name;

   function Form (
      File : File_Type)
      return String is
   begin
      return Streams.Stream_IO.Form (Streams.Stream_IO.File_Type (File));
   end Form;

   function Is_Open (File : File_Type) return Boolean is
   begin
      return Streams.Stream_IO.Is_Open (Streams.Stream_IO.File_Type (File));
   end Is_Open;

   procedure Read (
      File : File_Type;
      Item : out Element_Type;
      From : Positive_Count) is
   begin
      Set_Index (File, From);
      Read (File, Item);
   end Read;

   procedure Read (
      File : File_Type;
      Item : out Element_Type)
   is
      pragma Warnings (Off,
         "constrained for private type is an obsolescent feature (RM J.4)");
      Constrained : constant Boolean := Element_Type'Constrained;
      --  'Definite / 'Has_Discriminants attribute are disallowed here
      pragma Warnings (On,
         "constrained for private type is an obsolescent feature (RM J.4)");
   begin
      if not Constrained then
         --  indefinite (or unconstrained) types
         declare
            Buffer_Item : Element_Type;
            Buffer : Streams.Stream_Element_Array (
               1 ..
               Element_Type'Max_Size_In_Storage_Elements);
            for Buffer'Address use Buffer_Item'Address;
            Last : Streams.Stream_Element_Offset;
         begin
            Streams.Stream_IO.Read (
               Streams.Stream_IO.File_Type (File),
               Buffer,
               Last);
            if Last < Buffer'Last then
               Raise_Exception (End_Error'Identity);
            end if;
            Item := Buffer_Item;
         end;
      else
         declare
            Buffer : Streams.Stream_Element_Array (
               1 ..
               Element_Type'Max_Size_In_Storage_Elements);
            for Buffer'Address use Item'Address;
            Last : Streams.Stream_Element_Offset;
         begin
            Streams.Stream_IO.Read (
               Streams.Stream_IO.File_Type (File),
               Buffer,
               Last);
            if Last < Buffer'Last then
               Raise_Exception (End_Error'Identity);
            end if;
         end;
      end if;
   end Read;

   procedure Write (
      File : File_Type;
      Item : Element_Type;
      To : Positive_Count) is
   begin
      Set_Index (File, To);
      Write (File, Item);
   end Write;

   procedure Write (
      File : File_Type;
      Item : Element_Type)
   is
      Buffer : Streams.Stream_Element_Array (
         1 ..
         Element_Type'Max_Size_In_Storage_Elements);
      for Buffer'Address use Item'Address;
   begin
      Streams.Stream_IO.Write (
         Streams.Stream_IO.File_Type (File),
         Buffer);
   end Write;

   procedure Set_Index (
      File : File_Type;
      To : Positive_Count)
   is
      Raw_Index : constant Streams.Stream_IO.Positive_Count :=
         Streams.Stream_IO.Positive_Count (
            (To - 1) * Element_Type'Max_Size_In_Storage_Elements + 1);
   begin
      Streams.Stream_IO.Set_Index (
         Streams.Stream_IO.File_Type (File),
         Raw_Index);
   end Set_Index;

   function Index (
      File : File_Type)
      return Positive_Count
   is
      Raw_Index : constant Positive_Count := Positive_Count (
         Streams.Stream_IO.Index (Streams.Stream_IO.File_Type (File)));
      Index_From_0 : constant Count := Raw_Index - 1;
   begin
      if Index_From_0 rem Element_Type'Max_Size_In_Storage_Elements /= 0 then
         Raise_Exception (Use_Error'Identity);
      end if;
      return Index_From_0 / Element_Type'Max_Size_In_Storage_Elements + 1;
   end Index;

   function Size (
      File : File_Type)
      return Count
   is
      Raw_Size : constant Count := Count (
         Streams.Stream_IO.Size (Streams.Stream_IO.File_Type (File)));
   begin
      if Raw_Size rem Element_Type'Max_Size_In_Storage_Elements /= 0 then
         Raise_Exception (Use_Error'Identity);
      end if;
      return Raw_Size / Element_Type'Max_Size_In_Storage_Elements;
   end Size;

   function End_Of_File (
      File : File_Type)
      return Boolean is
   begin
      if Mode (File) = Out_File then
         Raise_Exception (Mode_Error'Identity);
      end if;
      return Streams.Stream_IO.End_Of_File (
         Streams.Stream_IO.File_Type (File));
   end End_Of_File;

end Ada.Direct_IO;
