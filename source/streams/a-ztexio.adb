with Ada.Text_IO.Inside.Wide;
with Ada.Unchecked_Deallocation;
package body Ada.Wide_Wide_Text_IO is

   type Wide_Wide_String_Access is access Wide_Wide_String;
   procedure Free is new Unchecked_Deallocation (
      Wide_Wide_String,
      Wide_Wide_String_Access);

   procedure Close (File : in out File_Type) is
   begin
      Text_IO.Close (Text_IO.File_Type (File));
   end Close;

   function Col (File : File_Type) return Positive_Count is
   begin
      return Positive_Count (Text_IO.Col (Text_IO.File_Type (File)));
   end Col;

   function Col return Positive_Count is
   begin
      return Col (Current_Output.all);
   end Col;

   procedure Create (
      File : in out File_Type;
      Mode : File_Mode := Out_File;
      Name : String := "";
      Form : String := "") is
   begin
      Text_IO.Create (
          File => Text_IO.File_Type (File),
          Mode => Text_IO.File_Mode (Mode),
          Name => Name,
          Form => Form);
   end Create;

   function Current_Error return File_Access is
   begin
      return File_Type (Text_IO.Current_Error.all)'Unrestricted_Access;
   end Current_Error;

   function Current_Input return File_Access is
   begin
      return File_Type (Text_IO.Current_Input.all)'Unrestricted_Access;
   end Current_Input;

   function Current_Output return File_Access is
   begin
      return File_Type (Text_IO.Current_Output.all)'Unrestricted_Access;
   end Current_Output;

   procedure Delete (File : in out File_Type) is
   begin
      Text_IO.Delete (Text_IO.File_Type (File));
   end Delete;

   function End_Of_File (File : File_Type) return Boolean is
   begin
      return Text_IO.End_Of_File (Text_IO.File_Type (File));
   end End_Of_File;

   function End_Of_File return Boolean is
   begin
      return End_Of_File (Current_Input.all);
   end End_Of_File;

   function End_Of_Line (File : File_Type) return Boolean is
   begin
      return Text_IO.End_Of_Line (Text_IO.File_Type (File));
   end End_Of_Line;

   function End_Of_Line return Boolean is
   begin
      return End_Of_Line (Current_Input.all);
   end End_Of_Line;

   function End_Of_Page (File : File_Type) return Boolean is
   begin
      return Text_IO.End_Of_Page (Text_IO.File_Type (File));
   end End_Of_Page;

   function End_Of_Page return Boolean is
   begin
      return End_Of_Page (Current_Input.all);
   end End_Of_Page;

   procedure Flush (File : File_Type) is
   begin
      Text_IO.Flush (Text_IO.File_Type (File));
   end Flush;

   procedure Flush is
   begin
      Flush (Current_Output.all);
   end Flush;

   function Form (File : File_Type) return String is
   begin
      return Text_IO.Form (Text_IO.File_Type (File));
   end Form;

   procedure Get (File : File_Type; Item : out Wide_Wide_Character) is
   begin
      Text_IO.Inside.Wide.Get (Text_IO.File_Type (File), Item);
   end Get;

   procedure Get (Item : out Wide_Wide_Character) is
   begin
      Get (Current_Input.all, Item);
   end Get;

   procedure Get (File : File_Type; Item : out Wide_Wide_String) is
   begin
      for I in Item'Range loop
         Get (File, Item (I));
      end loop;
   end Get;

   procedure Get (Item : out Wide_Wide_String) is
   begin
      Get (Current_Input.all, Item);
   end Get;

   procedure Get_Immediate (
      File : File_Type;
      Item : out Wide_Wide_Character) is
   begin
      Text_IO.Inside.Wide.Get_Immediate (Text_IO.File_Type (File), Item);
   end Get_Immediate;

   procedure Get_Immediate (Item : out Wide_Wide_Character) is
   begin
      Get_Immediate (Current_Input.all, Item);
   end Get_Immediate;

   procedure Get_Immediate (
      File : File_Type;
      Item : out Wide_Wide_Character;
      Available : out Boolean) is
   begin
      Text_IO.Inside.Wide.Get_Immediate (
         Text_IO.File_Type (File),
         Item, Available);
   end Get_Immediate;

   procedure Get_Immediate (
      Item : out Wide_Wide_Character;
      Available : out Boolean) is
   begin
      Get_Immediate (Current_Input.all, Item, Available);
   end Get_Immediate;

   procedure Get_Line (
      File : File_Type;
      Item : out Wide_Wide_String;
      Last : out Natural) is
   begin
      Last := Item'First - 1;
      while Last < Item'Last loop
         if End_Of_Line (File) then
            Text_IO.Skip_Line (Text_IO.File_Type (File));
            exit;
         else
            Last := Last + 1;
            Get (File, Item (Last));
         end if;
      end loop;
   end Get_Line;

   procedure Get_Line (
      Item : out Wide_Wide_String;
      Last : out Natural) is
   begin
      Get_Line (Current_Input.all, Item, Last);
   end Get_Line;

   function Get_Line (File : File_Type) return Wide_Wide_String is
      Buffer : Wide_Wide_String_Access := new Wide_Wide_String (1 .. 256);
      First : Positive := 1;
   begin
      loop
         declare
            Last : Natural;
         begin
            Get_Line (File, Buffer (First .. Buffer'Last), Last);
            if Last < Buffer'Last then
               declare
                  Result : constant Wide_Wide_String := Buffer (1 .. Last);
               begin
                  Free (Buffer);
                  return Result;
               end;
            else
               First := Buffer'Last + 1;
               declare
                  New_Buffer : constant Wide_Wide_String_Access :=
                     new Wide_Wide_String (1 .. Buffer'Last * 2);
               begin
                  New_Buffer (Buffer'Range) := Buffer.all;
                  Free (Buffer);
                  Buffer := New_Buffer;
               end;
            end if;
         end;
      end loop;
   exception
      when others =>
         Free (Buffer);
         raise;
   end Get_Line;

   function Get_Line return Wide_Wide_String is
   begin
      return Get_Line (Current_Input.all);
   end Get_Line;

   function Is_Open (File : File_Type) return Boolean is
   begin
      return Text_IO.Is_Open (Text_IO.File_Type (File));
   end Is_Open;

   function Line (File : File_Type) return Positive_Count is
   begin
      return Count (Text_IO.Line (Text_IO.File_Type (File)));
   end Line;

   function Line return Positive_Count is
   begin
      return Line (Current_Output.all);
   end Line;

   function Line_Length (File : File_Type) return Count is
   begin
      return Count (Text_IO.Line_Length (Text_IO.File_Type (File)));
   end Line_Length;

   function Line_Length return Count is
   begin
      return Line_Length (Current_Output.all);
   end Line_Length;

   procedure Look_Ahead (
      File : File_Type;
      Item : out Wide_Wide_Character;
      End_Of_Line : out Boolean) is
   begin
      Text_IO.Inside.Wide.Look_Ahead (
         Text_IO.File_Type (File),
         Item, End_Of_Line);
   end Look_Ahead;

   procedure Look_Ahead (
      Item : out Wide_Wide_Character;
      End_Of_Line : out Boolean) is
   begin
      Look_Ahead (Current_Input.all, Item, End_Of_Line);
   end Look_Ahead;

   function Mode (File : File_Type) return File_Mode is
   begin
      return File_Mode (Text_IO.Mode (Text_IO.File_Type (File)));
   end Mode;

   function Name (File : File_Type) return String is
   begin
      return Text_IO.Name (Text_IO.File_Type (File));
   end Name;

   procedure New_Line (File : File_Type; Spacing : Positive_Count := 1) is
   begin
      Text_IO.New_Line (
         Text_IO.File_Type (File),
         Text_IO.Positive_Count (Spacing));
   end New_Line;

   procedure New_Line (Spacing : Positive_Count := 1) is
   begin
      New_Line (Current_Output.all, Spacing);
   end New_Line;

   procedure New_Page (File : File_Type) is
   begin
      Text_IO.New_Page (Text_IO.File_Type (File));
   end New_Page;

   procedure New_Page is
   begin
      New_Page (Current_Output.all);
   end New_Page;

   procedure Open (
      File : in out File_Type;
      Mode : File_Mode;
      Name : String;
      Form : String := "") is
   begin
      Text_IO.Open (
          File => Text_IO.File_Type (File),
          Mode => Text_IO.File_Mode (Mode),
          Name => Name,
          Form => Form);
   end Open;

   function Page (File : File_Type) return Positive_Count is
   begin
      return Positive_Count (Text_IO.Page (Text_IO.File_Type (File)));
   end Page;

   function Page return Positive_Count is
   begin
      return Page (Current_Output.all);
   end Page;

   function Page_Length (File : File_Type) return Count is
   begin
      return Count (Text_IO.Page_Length (Text_IO.File_Type (File)));
   end Page_Length;

   function Page_Length return Count is
   begin
      return Page_Length (Current_Output.all);
   end Page_Length;

   procedure Put (File : File_Type; Item : Wide_Wide_Character) is
   begin
      Text_IO.Inside.Wide.Put (Text_IO.File_Type (File), Item);
   end Put;

   procedure Put (Item : Wide_Wide_Character) is
   begin
      Put (Current_Output.all, Item);
   end Put;

   procedure Put (File : File_Type; Item : Wide_Wide_String) is
   begin
      for I in Item'Range loop
         Put (File, Item (I));
      end loop;
   end Put;

   procedure Put (Item : Wide_Wide_String) is
   begin
      Put (Current_Output.all, Item);
   end Put;

   procedure Put_Line (File : File_Type; Item : Wide_Wide_String) is
   begin
      Put (File, Item);
      Text_IO.New_Line (Text_IO.File_Type (File)); --  New_Line is ambiguous
   end Put_Line;

   procedure Put_Line (Item : Wide_Wide_String) is
   begin
      Put_Line (Current_Output.all, Item);
   end Put_Line;

   procedure Reset (File : in out File_Type; Mode : File_Mode) is
   begin
      Text_IO.Reset (Text_IO.File_Type (File), Text_IO.File_Mode (Mode));
   end Reset;

   procedure Reset (File : in out File_Type) is
   begin
      Text_IO.Reset (Text_IO.File_Type (File));
   end Reset;

   procedure Set_Col (File : File_Type; To : Positive_Count) is
   begin
      Text_IO.Set_Col (Text_IO.File_Type (File), Text_IO.Positive_Count (To));
   end Set_Col;

   procedure Set_Col (To : Positive_Count) is
   begin
      Set_Col (Current_Output.all, To);
   end Set_Col;

   procedure Set_Error (File : File_Type) is
   begin
      Text_IO.Set_Error (Text_IO.File_Type (File));
   end Set_Error;

   procedure Set_Input (File : File_Type) is
   begin
      Text_IO.Set_Input (Text_IO.File_Type (File));
   end Set_Input;

   procedure Set_Line (File : File_Type; To : Positive_Count) is
   begin
      Text_IO.Set_Line (Text_IO.File_Type (File), Text_IO.Positive_Count (To));
   end Set_Line;

   procedure Set_Line (To : Positive_Count) is
   begin
      Set_Line (Current_Output.all, To);
   end Set_Line;

   procedure Set_Line_Length (File : File_Type; To : Count) is
   begin
      Text_IO.Set_Line_Length (Text_IO.File_Type (File), Text_IO.Count (To));
   end Set_Line_Length;

   procedure Set_Line_Length (To : Count) is
   begin
      Set_Line_Length (Current_Output.all, To);
   end Set_Line_Length;

   procedure Set_Output (File : File_Type) is
   begin
      Text_IO.Set_Output (Text_IO.File_Type (File));
   end Set_Output;

   procedure Set_Page_Length (File : File_Type; To : Count) is
   begin
      Text_IO.Set_Page_Length (Text_IO.File_Type (File), Text_IO.Count (To));
   end Set_Page_Length;

   procedure Set_Page_Length (To : Count) is
   begin
      Set_Page_Length (Current_Output.all, To);
   end Set_Page_Length;

   procedure Skip_Line (File : File_Type; Spacing : Positive_Count := 1) is
   begin
      Text_IO.Skip_Line (
         Text_IO.File_Type (File),
         Text_IO.Positive_Count (Spacing));
   end Skip_Line;

   procedure Skip_Line (Spacing : Positive_Count := 1) is
   begin
      Skip_Line (Current_Input.all, Spacing);
   end Skip_Line;

   procedure Skip_Page (File : File_Type) is
   begin
      Text_IO.Skip_Page (Text_IO.File_Type (File));
   end Skip_Page;

   procedure Skip_Page is
   begin
      Skip_Page (Current_Input.all);
   end Skip_Page;

   function Standard_Error return File_Access is
   begin
      return File_Type (Text_IO.Standard_Error.all)'Unrestricted_Access;
   end Standard_Error;

   function Standard_Input return File_Access is
   begin
      return File_Type (Text_IO.Standard_Input.all)'Unrestricted_Access;
   end Standard_Input;

   function Standard_Output return File_Access is
   begin
      return File_Type (Text_IO.Standard_Output.all)'Unrestricted_Access;
   end Standard_Output;

end Ada.Wide_Wide_Text_IO;
