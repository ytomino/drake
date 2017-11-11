with Ada.Exception_Identification.From_Here;
with Ada.Streams.Naked_Stream_IO;
with Ada.Streams.Stream_IO.Naked;
package body Ada.Direct_IO is
   use Exception_Identification.From_Here;
   use type Streams.Stream_Element_Offset;

   procedure Create (
      File : in out File_Type;
      Mode : File_Mode := Inout_File;
      Name : String := "";
      Form : String := "")
   is
      NC_File : Streams.Naked_Stream_IO.Non_Controlled_File_Type
         renames Streams.Stream_IO.Naked.Non_Controlled (
            Streams.Stream_IO.File_Type (File)).all;
   begin
      Streams.Naked_Stream_IO.Create (
         NC_File,
         IO_Modes.Inout_File_Mode (Mode),
         Name => Name,
         Form => Streams.Naked_Stream_IO.Pack (Form));
   end Create;

   procedure Open (
      File : in out File_Type;
      Mode : File_Mode;
      Name : String;
      Form : String := "")
   is
      NC_File : Streams.Naked_Stream_IO.Non_Controlled_File_Type
         renames Streams.Stream_IO.Naked.Non_Controlled (
            Streams.Stream_IO.File_Type (File)).all;
   begin
      Streams.Naked_Stream_IO.Open (
         NC_File,
         IO_Modes.Inout_File_Mode (Mode),
         Name => Name,
         Form => Streams.Naked_Stream_IO.Pack (Form));
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
      NC_File : Streams.Naked_Stream_IO.Non_Controlled_File_Type
         renames Streams.Stream_IO.Naked.Non_Controlled (
            Streams.Stream_IO.File_Type (File)).all;
   begin
      Streams.Naked_Stream_IO.Reset (NC_File, IO_Modes.Inout_File_Mode (Mode));
   end Reset;

   procedure Reset (File : in out File_Type) is
   begin
      Reset (File, File_Mode'(Mode (File)));
   end Reset;

   function Mode (
      File : File_Type)
      return File_Mode
   is
      pragma Check (Dynamic_Predicate,
         Check => Is_Open (File) or else raise Status_Error);
      NC_File : Streams.Naked_Stream_IO.Non_Controlled_File_Type
         renames Streams.Stream_IO.Naked.Non_Controlled (
            Streams.Stream_IO.File_Type (File)).all;
   begin
      return File_Mode (
         IO_Modes.Inout_File_Mode'(Streams.Naked_Stream_IO.Mode (NC_File)));
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
      Item : out Element_Type;
      From : Positive_Count)
   is
      pragma Check (Dynamic_Predicate,
         Check => Is_Open (File) or else raise Status_Error);
      pragma Check (Dynamic_Predicate,
         Check => Mode (File) /= Out_File or else raise Mode_Error);
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
            Buffer_Item_As_SEA : Streams.Stream_Element_Array (
               1 ..
               Element_Type'Max_Size_In_Storage_Elements);
            for Buffer_Item_As_SEA'Address use Buffer_Item'Address;
            Last : Streams.Stream_Element_Offset;
         begin
            Streams.Stream_IO.Read (
               Streams.Stream_IO.File_Type (File), -- checking the predicate
               Buffer_Item_As_SEA,
               Last);
            if Last < Buffer_Item_As_SEA'Last then
               Raise_Exception (End_Error'Identity);
            end if;
            Item := Buffer_Item;
         end;
      else
         declare
            Item_As_SEA : Streams.Stream_Element_Array (
               1 ..
               Element_Type'Max_Size_In_Storage_Elements);
            for Item_As_SEA'Address use Item'Address;
            Last : Streams.Stream_Element_Offset;
         begin
            Streams.Stream_IO.Read (
               Streams.Stream_IO.File_Type (File), -- checking the predicate
               Item_As_SEA,
               Last);
            if Last < Item_As_SEA'Last then
               Raise_Exception (End_Error'Identity);
            end if;
         end;
      end if;
   end Read;

   procedure Write (
      File : File_Type;
      Item : Element_Type;
      To : Positive_Count)
   is
      pragma Check (Dynamic_Predicate,
         Check => Is_Open (File) or else raise Status_Error);
      pragma Check (Dynamic_Predicate,
         Check => Mode (File) /= In_File or else raise Mode_Error);
   begin
      Set_Index (File, To);
      Write (File, Item);
   end Write;

   procedure Write (
      File : File_Type;
      Item : Element_Type)
   is
      Item_As_SEA : Streams.Stream_Element_Array (
         1 ..
         Element_Type'Max_Size_In_Storage_Elements);
      for Item_As_SEA'Address use Item'Address;
   begin
      Streams.Stream_IO.Write (
         Streams.Stream_IO.File_Type (File), -- checking the predicate
         Item_As_SEA);
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
         Streams.Stream_IO.File_Type (File), -- checking the predicate
         Raw_Index);
   end Set_Index;

   function Index (
      File : File_Type)
      return Positive_Count
   is
      Raw_Index : constant Positive_Count :=
         Positive_Count (
            Streams.Stream_IO.Index (
               Streams.Stream_IO.File_Type (File))); -- checking the predicate
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
      Raw_Size : constant Count :=
         Count (
            Streams.Stream_IO.Size (
               Streams.Stream_IO.File_Type (File))); -- checking the predicate
   begin
      if Raw_Size rem Element_Type'Max_Size_In_Storage_Elements /= 0 then
         Raise_Exception (Use_Error'Identity);
      end if;
      return Raw_Size / Element_Type'Max_Size_In_Storage_Elements;
   end Size;

   function End_Of_File (
      File : File_Type)
      return Boolean
   is
      pragma Check (Dynamic_Predicate,
         Check => Is_Open (File) or else raise Status_Error);
      pragma Check (Dynamic_Predicate,
         Check => Mode (File) /= Out_File or else raise Mode_Error);
   begin
      return Streams.Stream_IO.End_Of_File (
         Streams.Stream_IO.File_Type (File));
   end End_Of_File;

end Ada.Direct_IO;
