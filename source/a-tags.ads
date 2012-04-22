pragma License (Unrestricted);
private with System.Storage_Elements;
package Ada.Tags is
   pragma Preelaborate;

   type Tag is private;
   pragma Preelaborable_Initialization (Tag);

   No_Tag : constant Tag;

   function Expanded_Name (T : Tag) return String;
   function Wide_Expanded_Name (T : Tag) return Wide_String;
   function Wide_Wide_Expanded_Name (T : Tag) return Wide_Wide_String;
   function External_Tag (T : Tag) return String;
   function Internal_Tag (External : String) return Tag;

   function Descendant_Tag (External : String; Ancestor : Tag) return Tag;
   function Is_Descendant_At_Same_Level (Descendant, Ancestor : Tag)
      return Boolean;

   function Parent_Tag (T : Tag) return Tag;

   type Tag_Array is array (Positive range <>) of Tag;

   function Interface_Ancestor_Tags (T : Tag) return Tag_Array;

   function Is_Abstract (T : Tag) return Boolean; -- Ada 2012

   Tag_Error : exception;

private

   type Prim_Ptr is access procedure;
   type Address_Array is array (Positive range <>) of Prim_Ptr;
   pragma Suppress_Initialization (Address_Array);
   subtype Dispatch_Table is Address_Array (1 .. 1); -- gdb knows it ?

   --  full declaration

   type Tag is access all Dispatch_Table;
   for Tag'Storage_Size use 0;

   No_Tag : constant Tag := null;

   --  zero-terminated string required by compiler (a-tags.ads)

   subtype Fixed_String is String (Positive);
   type Cstring_Ptr is access all Fixed_String;
   for Cstring_Ptr'Storage_Size use 0;

   --  required for tagged types by compiler (a-tags.ads)

   type Offset_To_Top_Function_Ptr is access function (This : System.Address)
      return System.Storage_Elements.Storage_Offset;

   type Interface_Data_Element is record
      Iface_Tag : Tag;
      Static_Offset_To_Top : Boolean;
      Offset_To_Top_Value : System.Storage_Elements.Storage_Offset;
      Offset_To_Top_Func : Offset_To_Top_Function_Ptr;
      Secondary_DT : Tag;
   end record;
   pragma Suppress_Initialization (Interface_Data_Element);

   type Interfaces_Array is array (Natural range <>) of Interface_Data_Element;
   pragma Suppress_Initialization (Interfaces_Array);

   type Interface_Data (Nb_Ifaces : Positive) is record
      Ifaces_Table : Interfaces_Array (1 .. Nb_Ifaces);
   end record;
   pragma Suppress_Initialization (Interface_Data);

   type Interface_Data_Ptr is access all Interface_Data;
   for Interface_Data_Ptr'Storage_Size use 0;

   type Prim_Op_Kind is (
      POK_Function,
      POK_Procedure,
      POK_Protected_Entry,
      POK_Protected_Function,
      POK_Protected_Procedure,
      POK_Task_Entry,
      POK_Task_Function,
      POK_Task_Procedure);
   pragma Discard_Names (Prim_Op_Kind);

   type Select_Specific_Data_Element is record
      Index : Positive;
      Kind : Prim_Op_Kind;
   end record;
   pragma Suppress_Initialization (Select_Specific_Data_Element);

   type Select_Specific_Data_Array is
      array (Positive range <>) of Select_Specific_Data_Element;
   pragma Suppress_Initialization (Select_Specific_Data_Array);

   type Select_Specific_Data (Nb_Prim : Positive) is record
      SSD_Table : Select_Specific_Data_Array (1 .. Nb_Prim);
   end record;
   pragma Suppress_Initialization (Select_Specific_Data);

   type Select_Specific_Data_Ptr is access all Select_Specific_Data;
   for Select_Specific_Data_Ptr'Storage_Size use 0;

   type Interface_Tag is access all Dispatch_Table;
   for Interface_Tag'Storage_Size use 0;

   type Tag_Ptr is access all Tag;
   for Tag_Ptr'Storage_Size use 0;

   type Offset_To_Top_Ptr is access all System.Storage_Elements.Storage_Offset;
   for Offset_To_Top_Ptr'Storage_Size use 0;

   type Tag_Table is array (Natural range <>) of Tag;
   pragma Suppress_Initialization (Tag_Table);

   type Size_Ptr is access function (A : System.Address)
      return Long_Long_Integer;

   type Type_Specific_Data (Idepth : Natural) is record
      Access_Level : Natural;
      Expanded_Name : Cstring_Ptr;
      External_Tag : Cstring_Ptr;
      HT_Link : Tag_Ptr;
      Transportable : Boolean;
      Type_Is_Abstract : Boolean;
      RC_Offset : System.Storage_Elements.Storage_Offset;
      Size_Func : Size_Ptr;
      Interfaces_Table : Interface_Data_Ptr;
      SSD : Select_Specific_Data_Ptr;
      Tags_Table : Tag_Table (0 .. Idepth);
   end record;
   pragma Suppress_Initialization (Type_Specific_Data);

   type Type_Specific_Data_Ptr is access all Type_Specific_Data;
   for Type_Specific_Data_Ptr'Storage_Size use 0;

   type Signature_Kind is (
      Unknown,
      Primary_DT,
      Secondary_DT);
   pragma Discard_Names (Signature_Kind);

   type Tagged_Kind is (
      TK_Abstract_Limited_Tagged,
      TK_Abstract_Tagged,
      TK_Limited_Tagged,
      TK_Protected,
      TK_Tagged,
      TK_Task);
   pragma Discard_Names (Tagged_Kind);

   type Dispatch_Table_Wrapper (Num_Prims : Natural) is record
      Signature : Signature_Kind;
      Tag_Kind : Tagged_Kind;
      Predef_Prims : System.Address;
      Offset_To_Top : System.Storage_Elements.Storage_Offset;
      TSD : System.Address;
      Prims_Ptr : aliased Address_Array (1 .. Num_Prims);
   end record;
   pragma Suppress_Initialization (Dispatch_Table_Wrapper);

   type Dispatch_Table_Ptr is access all Dispatch_Table_Wrapper;
   for Dispatch_Table_Ptr'Storage_Size use 0;

   type No_Dispatch_Table_Wrapper is record
      NDT_TSD : System.Address;
      NDT_Prims_Ptr : Natural;
   end record;
   pragma Suppress_Initialization (No_Dispatch_Table_Wrapper);

   DT_Predef_Prims_Size : constant :=
      Standard'Address_Size / Standard'Storage_Unit;

   DT_Offset_To_Top_Size : constant :=
      Standard'Address_Size / Standard'Storage_Unit;

   DT_Typeinfo_Ptr_Size : constant :=
      Standard'Address_Size / Standard'Storage_Unit;

   DT_Offset_To_Top_Offset : constant :=
      DT_Typeinfo_Ptr_Size + DT_Offset_To_Top_Size;

   DT_Predef_Prims_Offset : constant :=
      DT_Typeinfo_Ptr_Size + DT_Offset_To_Top_Size + DT_Predef_Prims_Size;

   type Object_Specific_Data_Array is array (Positive range <>) of Positive;
   pragma Suppress_Initialization (Object_Specific_Data_Array);

   type Object_Specific_Data (OSD_Num_Prims : Positive) is record
      OSD_Table : Object_Specific_Data_Array (1 .. OSD_Num_Prims);
   end record;
   pragma Suppress_Initialization (Object_Specific_Data);

   type Object_Specific_Data_Ptr is access all Object_Specific_Data;
   for Object_Specific_Data_Ptr'Storage_Size use 0;

   Max_Predef_Prims : constant := 16;

   subtype Predef_Prims_Table is Address_Array (1 .. Max_Predef_Prims);
   type Predef_Prims_Table_Ptr is access Predef_Prims_Table;

   type Addr_Ptr is access System.Address;
   for Addr_Ptr'Storage_Size use 0;

   --  required for controlled types by compiler (a-tags.ads)
   function DT (T : Tag) return Dispatch_Table_Ptr;

   --  required for interface class-wide types by compiler (a-tags.ads)
   function Base_Address (This : System.Address) return System.Address;

   --  required for downcast to interface types by compiler (a-tags.ads)
   function Displace (This : System.Address; T : Tag) return System.Address;

   --  required for Generic_Dispatching_Constructor with interface ???
--   function Secondary_Tag (T, Iface : Tag) return Tag;

--   function Get_Entry_Index (T : Tag; Position : Positive) return Positive;

--   function Get_Offset_Index (T : Tag; Position : Positive) return Positive;

   --  required for virtual subprograms by compiler (a-tags.ads)
   function Get_Prim_Op_Kind (T : Tag; Position : Positive)
      return Prim_Op_Kind;

   --  required for select statement with synchronized interface ???
--   function Get_Tagged_Kind (T : Tag) return Tagged_Kind;

   --  required for Obj in Intf'Class by compiler (a-tags.ads)
   function IW_Membership (This : System.Address; T : Tag) return Boolean;

   --  required (optional) for dynamic secondary dispatch table ???
--   function Offset_To_Top (This : System.Address)
--      return System.Storage_Elements.Storage_Offset;

   --  required for tagged types having two (or more) ancestors
   --    by compiler (s-exctab.ads)
   procedure Register_Interface_Offset (
      This : System.Address;
      Interface_T : Tag;
      Is_Static : Boolean;
      Offset_Value : System.Storage_Elements.Storage_Offset;
      Offset_Func : Offset_To_Top_Function_Ptr);

   --  required for library-level tagged types by compiler (s-exctab.ads)
   procedure Register_Tag (T : Tag) is null; -- unimplemented

--   procedure Set_Dynamic_Offset_To_Top (
--      This : System.Address;
--      Interface_T : Tag;
--      Offset_Value : System.Storage_Elements.Storage_Offset;
--      Offset_Func : Offset_To_Top_Function_Ptr);

--  procedure Set_Entry_Index (T : Tag; Position : Positive; Value : Positive);

--   procedure Set_Prim_Op_Kind (
--      T : Tag;
--      Position : Positive;
--      Value : Prim_Op_Kind);

   --  inheritance relation check
   function Is_Descendant (
      Descendant, Ancestor : Tag;
      Primary_Only : Boolean;
      Same_Level : Boolean)
      return Boolean;

   --  interface delegation
   Get_Delegation : access function (
      Object : System.Address;
      Interface_Tag : Tag)
      return System.Address := null;
   pragma Atomic (Get_Delegation);

end Ada.Tags;
