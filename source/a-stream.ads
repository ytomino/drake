pragma License (Unrestricted);
package Ada.Streams is
   pragma Pure;

   type Root_Stream_Type is abstract tagged limited private;
   pragma Preelaborable_Initialization (Root_Stream_Type);

   type Stream_Element is
      mod 2 ** Standard'Storage_Unit; --  implementation-defined
   type Stream_Element_Offset is range
      -(2 ** (Standard'Address_Size - 1)) ..
      +(2 ** (Standard'Address_Size - 1)) - 1; --  implementation-defined
   subtype Stream_Element_Count is
      Stream_Element_Offset range 0 .. Stream_Element_Offset'Last;
   type Stream_Element_Array is
      array (Stream_Element_Offset range <>) of aliased Stream_Element;

   procedure Read (
      Stream : in out Root_Stream_Type;
      Item : out Stream_Element_Array;
      Last : out Stream_Element_Offset) is abstract;

   procedure Write (
      Stream : in out Root_Stream_Type;
      Item : Stream_Element_Array) is abstract;

   --  extended
   type Seekable_Stream_Type is
      abstract limited new Root_Stream_Type with private;
   pragma Preelaborable_Initialization (Seekable_Stream_Type);

   subtype Stream_Element_Positive_Count is Stream_Element_Count
      range 1 .. Stream_Element_Count'Last;

   procedure Set_Index (
      Stream : in out Seekable_Stream_Type;
      To : Stream_Element_Positive_Count) is abstract;

   function Index (Stream : Seekable_Stream_Type)
      return Stream_Element_Positive_Count is abstract;
   function Size (Stream : Seekable_Stream_Type)
      return Stream_Element_Count is abstract;

private

   type Root_Stream_Type is abstract tagged limited null record;
   type Seekable_Stream_Type is
      abstract limited new Root_Stream_Type with null record;

end Ada.Streams;
