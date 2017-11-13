pragma License (Unrestricted);
--  extended unit
with Ada.IO_Exceptions;
with System.Storage_Elements;
private with Ada.Finalization;
private with System.Reference_Counting;
package Ada.Streams.Unbounded_Storage_IO is
   --  Temporary stream on memory.
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

   procedure Write_To_Stream (
      Stream : not null access Root_Stream_Type'Class;
      Item : Buffer_Type);

   --  Exceptions

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

   type Non_Controlled_Buffer_Type;

   type Stream_Type is limited new Seekable_Stream_Type with record
      Buffer : not null access Non_Controlled_Buffer_Type;
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

   type Non_Controlled_Buffer_Type is record
      Data : aliased not null Data_Access;
      Last : Stream_Element_Offset;
      Index : Stream_Element_Offset;
      Stream : Stream_Access;
   end record;
   pragma Suppress_Initialization (Non_Controlled_Buffer_Type);

   package Controlled is

      type Buffer_Type is private;

      function Reference (Object : Unbounded_Storage_IO.Buffer_Type)
         return not null access Non_Controlled_Buffer_Type;
      pragma Inline (Reference);

   private

      type Buffer_Type is new Finalization.Controlled with record
         Data : aliased Non_Controlled_Buffer_Type := (
            Data => Data_Access'(Empty_Data'Unrestricted_Access),
            Last => 0,
            Index => 1,
            Stream => null);
      end record;

      overriding procedure Adjust (Object : in out Buffer_Type);
      overriding procedure Finalize (Object : in out Buffer_Type);

      package Streaming is

         procedure Read (
            Stream : not null access Root_Stream_Type'Class;
            Item : out Buffer_Type);
         procedure Write (
            Stream : not null access Root_Stream_Type'Class;
            Item : Buffer_Type);

      end Streaming;

      for Buffer_Type'Read use Streaming.Read;
      for Buffer_Type'Write use Streaming.Write;

   end Controlled;

   type Buffer_Type is new Controlled.Buffer_Type;

end Ada.Streams.Unbounded_Storage_IO;
