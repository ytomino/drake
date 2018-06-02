with Ada.Streams.Stream_IO.Naked;
with System.Form_Parameters;
package body Ada.Storage_Mapped_IO is
   use type Streams.Stream_Element_Offset;
   use type Streams.Stream_IO.File_Mode;
   use type System.Address;
   use type System.Native_IO.File_Mode;

   function Pack_For_Map (Form : String) return Boolean;
   function Pack_For_Map (Form : String) return Boolean is
      Keyword_First : Positive;
      Keyword_Last : Natural;
      Item_First : Positive;
      Item_Last : Natural;
      Last : Natural;
      Private_Copy : Boolean := False; -- default
   begin
      Last := Form'First - 1;
      while Last < Form'Last loop
         System.Form_Parameters.Get (
            Form (Last + 1 .. Form'Last),
            Keyword_First,
            Keyword_Last,
            Item_First,
            Item_Last,
            Last);
         declare
            Keyword : String
               renames Form (Keyword_First .. Keyword_Last);
            Item : String
               renames Form (Item_First .. Item_Last);
         begin
            if Keyword = "private" then
               if Item'Length > 0 and then Item (Item'First) = 'f' then
                  Private_Copy := False; -- false
               elsif Item'Length > 0 and then Item (Item'First) = 't' then
                  Private_Copy := True; -- true
               end if;
            end if;
         end;
      end loop;
      return Private_Copy;
   end Pack_For_Map;

   procedure Map (
      Object : in out Non_Controlled_Mapping;
      File : Streams.Naked_Stream_IO.Non_Controlled_File_Type;
      Private_Copy : Boolean;
      Offset : Streams.Stream_IO.Positive_Count;
      Size : Streams.Stream_IO.Count);
   procedure Map (
      Object : in out Non_Controlled_Mapping;
      File : Streams.Naked_Stream_IO.Non_Controlled_File_Type;
      Private_Copy : Boolean;
      Offset : Streams.Stream_IO.Positive_Count;
      Size : Streams.Stream_IO.Count)
   is
      Mapped_Size : Streams.Stream_IO.Count;
   begin
      if Size = 0 then
         Mapped_Size := Streams.Naked_Stream_IO.Size (File) - (Offset - 1);
      else
         Mapped_Size := Size;
      end if;
      System.Native_IO.Map (
         Object.Mapping,
         Streams.Naked_Stream_IO.Handle (File),
         Streams.Naked_Stream_IO.Mode (File)
            and System.Native_IO.Read_Write_Mask,
         Private_Copy => Private_Copy,
         Offset => Offset,
         Size => Mapped_Size);
   end Map;

   procedure Map (
      Object : aliased in out Non_Controlled_Mapping;
      Mode : File_Mode;
      Name : String;
      Form : System.Native_IO.Packed_Form;
      Private_Copy : Boolean;
      Offset : Streams.Stream_IO.Positive_Count;
      Size : Streams.Stream_IO.Count);
   procedure Map (
      Object : aliased in out Non_Controlled_Mapping;
      Mode : File_Mode;
      Name : String;
      Form : System.Native_IO.Packed_Form;
      Private_Copy : Boolean;
      Offset : Streams.Stream_IO.Positive_Count;
      Size : Streams.Stream_IO.Count) is
   begin
      --  open file
      --  this file will be closed in Finalize even if any exception is raised
      Streams.Naked_Stream_IO.Open (
         Object.File,
         IO_Modes.Inout_File_Mode (Mode),
         Name => Name,
         Form => Form);
      --  map
      Map (
         Object,
         Object.File,
         Private_Copy => Private_Copy,
         Offset => Offset,
         Size => Size);
   end Map;

   procedure Unmap (
      Object : in out Non_Controlled_Mapping;
      Raise_On_Error : Boolean);
   procedure Unmap (
      Object : in out Non_Controlled_Mapping;
      Raise_On_Error : Boolean) is
   begin
      --  unmap
      System.Native_IO.Unmap (
         Object.Mapping,
         Raise_On_Error => Raise_On_Error);
      --  close file
      if Streams.Naked_Stream_IO.Is_Open (Object.File) then
         Streams.Naked_Stream_IO.Close (
            Object.File,
            Raise_On_Error => Raise_On_Error);
      end if;
   end Unmap;

   --  implementation

   function Is_Mapped (Object : Storage_Type) return Boolean is
      NC_Object : Non_Controlled_Mapping
         renames Controlled.Reference (Object).all;
   begin
      return NC_Object.Mapping.Storage_Address /= System.Null_Address;
   end Is_Mapped;

   procedure Map (
      Object : in out Storage_Type;
      File : Streams.Stream_IO.File_Type;
      Form : String; -- removed default
      Offset : Streams.Stream_IO.Positive_Count := 1;
      Size : Streams.Stream_IO.Count := 0)
   is
      pragma Check (Pre,
         Check => not Is_Mapped (Object) or else raise Status_Error);
      NC_Object : Non_Controlled_Mapping
         renames Controlled.Reference (Object).all;
   begin
      Map (
         NC_Object,
         Streams.Stream_IO.Naked.Non_Controlled (File).all,
         Private_Copy => Pack_For_Map (Form),
         Offset => Offset,
         Size => Size);
   end Map;

   procedure Map (
      Object : in out Storage_Type;
      File : Streams.Stream_IO.File_Type;
      Private_Copy : Boolean := False;
      Offset : Streams.Stream_IO.Positive_Count := 1;
      Size : Streams.Stream_IO.Count := 0)
   is
      pragma Check (Pre,
         Check => not Is_Mapped (Object) or else raise Status_Error);
      NC_Object : Non_Controlled_Mapping
         renames Controlled.Reference (Object).all;
   begin
      Map (
         NC_Object,
         Streams.Stream_IO.Naked.Non_Controlled (File).all,
         Private_Copy => Private_Copy,
         Offset => Offset,
         Size => Size);
   end Map;

   function Map (
      File : Streams.Stream_IO.File_Type;
      Private_Copy : Boolean := False;
      Offset : Streams.Stream_IO.Positive_Count := 1;
      Size : Streams.Stream_IO.Count := 0)
      return Storage_Type is
   begin
      return Result : Storage_Type do
         Map (
            Result,
            File,
            Private_Copy => Private_Copy,
            Offset => Offset,
            Size => Size);
      end return;
   end Map;

   procedure Map (
      Object : in out Storage_Type;
      Mode : File_Mode := In_File;
      Name : String;
      Form : String;
      Offset : Streams.Stream_IO.Positive_Count := 1;
      Size : Streams.Stream_IO.Count := 0)
   is
      pragma Check (Pre,
         Check => not Is_Mapped (Object) or else raise Status_Error);
      NC_Object : Non_Controlled_Mapping
         renames Controlled.Reference (Object).all;
   begin
      Map (
         NC_Object,
         Mode,
         Name => Name,
         Form => Streams.Naked_Stream_IO.Pack (Form),
         Private_Copy => Pack_For_Map (Form),
         Offset => Offset,
         Size => Size);
   end Map;

   procedure Map (
      Object : in out Storage_Type;
      Mode : File_Mode := In_File;
      Name : String;
      Shared : IO_Modes.File_Shared_Spec := IO_Modes.By_Mode;
      Wait : Boolean := False;
      Overwrite : Boolean := True;
      Private_Copy : Boolean := False;
      Offset : Streams.Stream_IO.Positive_Count := 1;
      Size : Streams.Stream_IO.Count := 0)
   is
      pragma Check (Pre,
         Check => not Is_Mapped (Object) or else raise Status_Error);
      NC_Object : Non_Controlled_Mapping
         renames Controlled.Reference (Object).all;
   begin
      Map (
         NC_Object,
         Mode,
         Name => Name,
         Form => (Shared, Wait, Overwrite),
         Private_Copy => Private_Copy,
         Offset => Offset,
         Size => Size);
   end Map;

   function Map (
      Mode : File_Mode := In_File;
      Name : String;
      Shared : IO_Modes.File_Shared_Spec := IO_Modes.By_Mode;
      Wait : Boolean := False;
      Overwrite : Boolean := True;
      Private_Copy : Boolean := False;
      Offset : Streams.Stream_IO.Positive_Count := 1;
      Size : Streams.Stream_IO.Count := 0)
      return Storage_Type is
   begin
      return Result : Storage_Type do
         Map (
            Result,
            Mode,
            Name => Name,
            Shared => Shared,
            Wait => Wait,
            Overwrite => Overwrite,
            Private_Copy => Private_Copy,
            Offset => Offset,
            Size => Size);
      end return;
   end Map;

   procedure Unmap (Object : in out Storage_Type) is
      pragma Check (Pre,
         Check => Is_Mapped (Object) or else raise Status_Error);
      NC_Object : Non_Controlled_Mapping
         renames Controlled.Reference (Object).all;
   begin
      Unmap (NC_Object, Raise_On_Error => True);
   end Unmap;

   function Storage_Address (
      Object : Storage_Type)
      return System.Address
   is
      pragma Check (Dynamic_Predicate,
         Check => Is_Mapped (Object) or else raise Status_Error);
      NC_Object : Non_Controlled_Mapping
         renames Controlled.Reference (Object).all;
   begin
      return NC_Object.Mapping.Storage_Address;
   end Storage_Address;

   function Storage_Size (
      Object : Storage_Type)
      return System.Storage_Elements.Storage_Count
   is
      pragma Check (Dynamic_Predicate,
         Check => Is_Mapped (Object) or else raise Status_Error);
      NC_Object : Non_Controlled_Mapping
         renames Controlled.Reference (Object).all;
   begin
      return NC_Object.Mapping.Storage_Size;
   end Storage_Size;

   package body Controlled is

      function Reference (Object : Storage_Mapped_IO.Storage_Type)
         return not null access Non_Controlled_Mapping is
      begin
         return Storage_Type (Object).Data'Unrestricted_Access;
      end Reference;

      overriding procedure Finalize (Object : in out Storage_Type) is
      begin
         if Object.Data.Mapping.Storage_Address /= System.Null_Address then
            Unmap (Object.Data, Raise_On_Error => False);
         end if;
      end Finalize;

   end Controlled;

end Ada.Storage_Mapped_IO;
