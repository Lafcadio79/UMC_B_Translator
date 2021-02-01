-- Ada local libraries
with Ada.Text_IO;
with Ada.Exceptions;
with Ada.Characters.Handling;

-- separate (configurations.Kernel)
with Flags; use Flags;

package UML_Parser is
   -- enable the use of the imported libraries
   use Ada.Text_IO;
   use Ada.Exceptions;
   use Ada.Characters.Handling;

   -- parses the uml file indentified by the given name
   -- and creates the corresponding uml object in "This_UML_Object"
   procedure Parse (Uml_File_Name: String);

   ---------------------------------------
   -- objects and variables declaration --
   ---------------------------------------
   Max_Components                : Positive := 500;
   Max_Empty_lines               : Natural  := 500;
   Max_Line_Length               : Positive := 10000;
   Max_Comment_lines             : Natural  := 500;
   Max_Synchronizations          : Positive := 10000;

end UML_Parser;
