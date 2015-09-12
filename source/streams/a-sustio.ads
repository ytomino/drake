pragma License (Unrestricted);
--  extended unit
with Ada.IO_Exceptions;
with System.Storage_Elements;
private with Ada.Finalization;
private with System.Reference_Counting;
package Ada.Streams.Unbounded_Storage_IO is
   --  This package provides temporary stream on memory.
   pragma Preelaborate;

   type Buffer_Type is private;

   procedure Reset (Object : in out Buffer_Type);

   function Size (Object : Buffer_Type) return Stream_Element_Count;
   procedure Set_Size (
      Object : in out Buffer_Type;
      Size : Stream_Element_Count);

   pragma Inline (Size);

   function Capacity (Object : Buffer_Type) return Stream_Element_Count;
   procedure Reserve_Capacity (
      Object : in out Buffer_Type;
      Capacity : Stream_Element_Count);

   pragma Inline (Capacity);

   --  direct storage accessing
   function Storage_Address (Object : aliased in out Buffer_Type)
      return System.Address;
   function Storage_Size (Object : Buffer_Type)
      return System.Storage_Elements.Storage_Count;

   pragma Inline (Storage_Size);

   --  streaming
   function Stream (Object : Buffer_Type)
      return not null access Root_Stream_Type'Class;
   pragma Inline (Stream);

   End_Error : exception
      renames IO_Exceptions.End_Error;

private

   type Stream_Element_Array_Access is access Stream_Element_Array;

   type Data is record -- "limited" prevents No_Elaboration_Code
      Reference_Count : aliased System.Reference_Counting.Counter;
      Max_Length : aliased System.Reference_Counting.Length_Type;
      Capacity : Stream_Element_Count;
      Storage : System.Address;
      --  the storage would be allocated in here
   end record;
   pragma Suppress_Initialization (Data);

   type Data_Access is access all Data;

   Empty_Data : aliased constant Data := (
      Reference_Count => System.Reference_Counting.Static,
      Max_Length => 0,
      Capacity => 0,
      Storage => System.Null_Address);

   type Stream_Type is limited new Seekable_Stream_Type with record
      Data : aliased not null Data_Access;
      Last : Stream_Element_Offset;
      Index : Stream_Element_Offset := 1;
   end record;

   overriding procedure Read (
      Stream : in out Stream_Type;
      Item : out Stream_Element_Array;
      Last : out Stream_Element_Offset);
   overriding procedure Write (
      Stream : in out Stream_Type;
      Item : Stream_Element_Array);
   overriding procedure Set_Index (
      Stream : in out Stream_Type;
      To : Stream_Element_Positive_Count);
   overriding function Index (Stream : Stream_Type)
      return Stream_Element_Positive_Count;
   overriding function Size (Stream : Stream_Type)
      return Stream_Element_Count;

   type Stream_Access is access Stream_Type;

   package Controlled is

      type Buffer_Type is private;

      function Stream (Object : Buffer_Type) return not null Stream_Access;
      pragma Inline (Stream);

   private

      type Buffer_Type is new Finalization.Controlled with record
         Stream : Stream_Access;
      end record;

      overriding procedure Initialize (Object : in out Buffer_Type);
      overriding procedure Finalize (Object : in out Buffer_Type);
      overriding procedure Adjust (Object : in out Buffer_Type);

      package Streaming is

         procedure Missing_Read (
            Stream : not null access Root_Stream_Type'Class;
            Item : out Buffer_Type)
            with Import,
               Convention => Ada, External_Name => "__drake_program_error";
         function Missing_Input (
            Stream : not null access Root_Stream_Type'Class)
            return Buffer_Type
            with Import,
               Convention => Ada, External_Name => "__drake_program_error";
         --  "out" parameter destructs size info, and result is also

         procedure Write (
            Stream : not null access Root_Stream_Type'Class;
            Item : Buffer_Type);

      end Streaming;

      for Buffer_Type'Read use Streaming.Missing_Read;
      for Buffer_Type'Input use Streaming.Missing_Input;
      for Buffer_Type'Write use Streaming.Write;
      for Buffer_Type'Output use Streaming.Write;

   end Controlled;

   type Buffer_Type is new Controlled.Buffer_Type;

end Ada.Streams.Unbounded_Storage_IO;
