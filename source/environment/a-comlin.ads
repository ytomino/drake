pragma License (Unrestricted);
with Ada.Iterator_Interfaces;
private with Ada.Streams;
package Ada.Command_Line is
   pragma Preelaborate;

   function Argument_Count return Natural;
   pragma Inline (Argument_Count); -- renamed

   function Argument (Number : Positive) return String;

   --  extended
   --  The iterator.
   function Has_Element (Position : Natural) return Boolean;
   pragma Inline (Has_Element);
   package Iterator_Interfaces is
      new Ada.Iterator_Interfaces (Natural, Has_Element);
   function Iterate return Iterator_Interfaces.Reversible_Iterator'Class;
   function Iterate (First : Positive; Last : Natural)
      return Iterator_Interfaces.Reversible_Iterator'Class;

   function Command_Name return String;

   type Exit_Status is new Integer; -- implementation-defined integer type

   Success : constant Exit_Status;
   Failure : constant Exit_Status;

   procedure Set_Exit_Status (Code : Exit_Status);

private

   type Iterator is new Iterator_Interfaces.Reversible_Iterator with record
      First : Positive;
      Last : Natural;
   end record;

   overriding function First (Object : Iterator) return Natural;
   overriding function Next (Object : Iterator; Position : Natural)
      return Natural;
   overriding function Last (Object : Iterator) return Natural;
   overriding function Previous (Object : Iterator; Position : Natural)
      return Natural;

   package Streaming is

      procedure Missing_Read (
         Stream : access Streams.Root_Stream_Type'Class;
         Item : out Iterator);
      function Missing_Input (
         Stream : not null access Streams.Root_Stream_Type'Class)
         return Iterator;
      procedure Missing_Write (
         Stream : access Streams.Root_Stream_Type'Class;
         Item : Iterator);

      pragma Import (Ada, Missing_Read, "__drake_program_error");
      pragma Import (Ada, Missing_Input, "__drake_program_error");
      pragma Import (Ada, Missing_Write, "__drake_program_error");

   end Streaming;

   for Iterator'Read use Streaming.Missing_Read;
   for Iterator'Input use Streaming.Missing_Input;
   for Iterator'Write use Streaming.Missing_Write;
   for Iterator'Output use Streaming.Missing_Write;

   Success : constant Exit_Status := 0;
   Failure : constant Exit_Status := 1;

end Ada.Command_Line;
