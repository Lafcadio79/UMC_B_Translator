with Flags; use Flags;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with Interfaces;
with Flags; use Flags;
with Dyn_Store;
with NickNames;
with Ada.Containers; use Ada.Containers;
with Ada.Containers; use Ada.Containers;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Hash;
with Ada.Containers.Hashed_Sets;
with Ada.Text_IO; use Ada.Text_IO;
with Unchecked_Conversion;
package body UMC_box is

  UML_Error: exception;
  Abstract_Predicates: String_Table_Ref := Empty_String_Table_Ref;
  Show_Params: Boolean := True;
  Show_Target:Boolean := True;
------------------------------------- DATA-BASES -------------------------------------
  type BitVector is array(1..32) of Boolean;
  Pragma Pack(Bitvector);
  for BitVector'Size use 32;
  Function Tobits is new Unchecked_Conversion (Integer, BitVector);
  Function ToInt is new Unchecked_Conversion (BitVector,Integer);
  subtype String4 is String(1..4);
  function tostring4 is new Unchecked_Conversion (Integer, String4);
  --------------------------------
  --   Hashit((value1,value2,value3)) becomes:
  --   Ada.Strings.Hash(" value1 value2 value3");
  --------------------------------
  function Hashit (This_Table: Int_Table; prefix: String := "") return Natural is
      use Interfaces;
      thestring: String((This_Table'First-1)*4+1..This_Table'Length * 4);
      thehash: Ada.Containers.Hash_Type;
      U32: Interfaces.Unsigned_32;
      MASK: Interfaces.Unsigned_32 := 2**26-1;
    begin
      for I in This_Table'Range loop
        thestring(I*4-3 .. I*4) := tostring4(This_Table(I));
      end loop;
      thehash :=  Ada.Strings.Hash(thestring);
      U32 := Interfaces.Unsigned_32(thehash);
      U32 := U32 and MASK;
      return Natural(U32);
--      return Natural(thehash mod Ada.Containers.Hash_Type(Key_Module));
  end Hashit;

------------------------------------------------------------------------

  --
  function Null_Int_Table return Int_Table is
  begin
     return Empty_Int_Table;
  end Null_Int_Table;


  function Mk_Key(This_Table: Int_Table;
        Cache_Max: Natural := Key_Module) return Positive is
    Result: Integer := 0 ;
  begin
    Result := HashIt(This_Table);
    return Positive (Result +1);
  end Mk_Key;
  --
   package Vectors_DB is new NickNames
   (  Int_Table,
      Null_Int_Table,
      Mk_Key,
      "=");
      
  function MK_Nullvector return Int64 is
  begin
     return Vectors_DB.NickNum(Empty_Int_Table);
  end MK_Nullvector;
  
  NullVector: Integer := MK_Nullvector;
  
  package body Global_Env is separate;
  package body UML_Types is separate;
  package body UML_Parser is separate;


begin
  null;
end UMC_box;
