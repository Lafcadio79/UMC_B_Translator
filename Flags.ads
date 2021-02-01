with Ada.Unchecked_Deallocation;
package Flags is
   --
   subtype Int64 is Integer;
   type Int32 is mod 2**32;
   type Int_Table is array (Positive range <>) of Integer;
   --
   ------------------------- SHARED TYPES -----------------------
   type String_Ref is access String;
   Null_String: constant String_Ref := new String'("");
   procedure Free is new Ada.Unchecked_Deallocation (String, String_Ref);
   --
   type String_Table is array (Positive range <>) of String_Ref;
   Empty_String_Table: String_Table(1..0);
   type String_Table_Ref is access String_Table;
   Empty_String_Table_Ref: constant String_Table_Ref := new String_Table(1..0);
   procedure Free is
     new Ada.Unchecked_Deallocation (String_Table, String_Table_Ref);
   --
   type String_Tables_Vector is array (Positive range <>) of String_Table_Ref;
   Empty_String_Tables: String_Tables_Vector(1..0);
   type String_Tables_Vector_Ref is access String_Tables_Vector;
   Empty_String_Tables_Vector_Ref: constant String_Tables_Vector_Ref
     := new String_Tables_Vector(1..0);
   procedure Free is
     new Ada.Unchecked_Deallocation (String_Tables_Vector, String_Tables_Vector_Ref);
   --
   type Num_Table is array (Positive range <>) of Natural;
   Empty_Num_Table: Num_Table(1..0);
   type Num_Table_Ref is access Num_Table;
   procedure Free is new Ada.Unchecked_Deallocation(Num_Table,Num_Table_Ref);

   --------------------------   MORE   BASIC TYPES -------------------------
   type Bool_Table is array (Positive range <>) of Boolean;
   Empty_Bool_Table: Bool_Table(1..0);
   type Bool_Table_Ref is access Bool_Table;
   --
   --  type Int_Table is array (Positive range <>) of Integer; -- MOVED INTO FLAGS
   Empty_Int_Table: Int_Table(1..0);
   type Int_Table_Ref is access Int_Table;
   procedure Free is new Ada.Unchecked_Deallocation(Int_Table,Int_Table_Ref);
   --
   --  type Int64_Table is array (Positive range <>) of Int64;
   subtype Int64_Table is Int_Table;
   Empty_Int64_Table: Int64_Table(1..0);
   type Int64_Table_Ref is access Int64_Table;
   Empty_Int64_Table_Ref: constant Int64_Table_Ref := new Int64_Table(1..0);
   type Int64_List_Table is array (Positive range <>) of Int64_Table_Ref;
   procedure Free is new
     Ada.Unchecked_Deallocation(Int64_Table,Int64_Table_Ref);
   --
   type List_Table is array (Positive range <>) of Num_Table_Ref;
   Empty_List_Table: List_Table(1..0);
   type List_Table_Ref is access List_Table;
   --
   type Array_Table is array (Positive range <>) of Int_Table_Ref;
   Empty_Array_Table: Array_Table(1..0);
   type Array_Table_Ref is access Array_Table;
   ----------------------------------------------------------------------------
   --
   terse: Boolean := False;
   --
   Parsing_Error: exception;     -- raised by actl_parser
   UCTL_Error: exception;
   Non_Monotone: exception;    -- uctl_parser
   --
   Verbose_Eval: Boolean := True; -- WHY was it kept to false for WEB INTERACTIONS?
   --
   ACTL_Compatibility: Boolean := False;
   --
   All_Steps: Int64 :=0;
   Done_Computations: Int64 :=0;
   Done_States: Int64 :=0;
   -- 
   All_Computations_Count: Int64 :=0;
   All_States: Int64 :=0;
   --
   Default_LTS_Depth: Natural := 4;
   Max_Explanation_Depth: Natural := 500;   --  actually unused !!!
   Max_LTS_Depth: Natural := 16;              --- set by -<number>
   Top_LTS_Depth: Natural := 1_000_000;
   Depth_Increment: Natural :=0;              --- set by +<number>
   Static_MAX_Depth : Boolean := False;       --- set by -x 
   --
   NoGroundData: Boolean := False;            --- set by -y
   NoExplanations: Boolean := False;          --- set by -z
   AutoExplanations: Boolean := False;        --- set by +z
   ShowSequenceDiagram : Boolean :=False;     --- set bu +sd
   NonInteractive: Boolean := False;
   Ground_Action_Labels_Needed: Boolean := False;
   ---------
   QLimit: Natural := 20;   --set by -lqNNN  -- used by UMC
   NLimit: Natural := 100_000;   --set by -lnNNN  -- used by UMC
   VLimit: Natural := 10_000_000;   --set by -lvNNN  -- used by UMC
   ---------
   livefts: Boolean := False;
  
   ---------------
  
   ----------------  CONFIFURATION.ads defintions----------- 
   Runtime_Errors_Enabled: Boolean := True;
   Runtime_Errors_Count: Natural :=0;
   ----------------  CONFIFURATION.KERNEL.ads defintions----------- 
   Runtime_Error_Val: Integer := 0;
   Runtime_Error_Msg: String_Ref;
   ----------------------------  UML SETTINGS ------------------------------
   Priorities_Enabled: Boolean := True;
   Priorities_Defined:Boolean := False;
   Max_Evolutions_Depth: Positive := 10;
   ------------------------------------------------------------

   ---------
   --SystemVar.Observed: initially "False".
   --Set to "True" if encountered a rule  "assign(v,xx)" 
   ---------
   --Flags.Ground_Action_Labels_Needed: initially "False"
   --Set to "True" if we do not have a formula file (i.e. NonInteractive)
   -- in an interactive  session we need ground assignements during exploration 
   -- or state details visualization, or formula explanation.
   -- Option +z, forcing explanations, also should set  Ground_Action_Labels_Needed "True"
   -------------
   --Flags NonInteractive:  set when model and formula are provided on command line.
   -------------
   --Flags NoExplanations: "false" by default
   -- set "True" by option -z, set "false by +z"
   -------------
   --If var NOT Observed, Ground_Action_Labels NOT needed, and session NOT Interactive, 
   --and explantions NOT requested,
   --inside Object_Model we CAN AVOID the generation of "assign signals for var".
   -------------
   --Flags. NoExplanations:   defaults to "False"
   --Set to "True" by option "-z"
   --Set to "False" by option "+z"
   --------------
   --In case of "NoExplanantions" we can avoid saving SubComputations and SubEvolutions in
   --the Computations_DB, and we can use a unique Cref for Ftrue/FFalse for all states.
   --(I.e. Ftrue is evaluated only once).
   --(In the special case of "AG true" the "true" subformula is never evaluated. )
   --------------
   --
   HTML_Mode:  Boolean := False;              --- set by -w
   Debug: Boolean := False;
   --

   -------  used by tab/ccs models
   Display_Expanded_Terms: Boolean := True;
   Expanded_Tau: Boolean := False;
   --
   -- constants for display specifications
   --
   State_NickName: constant Positive := 1;
   State_Structure: constant Positive :=2;
   Detailed_Label: constant Positive :=3;
   Visible_Label: constant Positive :=4;
   Target_NickName: constant Positive :=5;
   Target_State_Structure: constant Positive :=6;
   Applied_Rule: constant Positive := 7;
   Abstract_Edge_Labels: constant Positive :=8;
   Ground_Edge_Labels: constant Positive :=9;
   --  mc2aut options
   Observe_Transition_Labels: Boolean := False; -- set by UML Parser
   Labels_in_Brakets: Boolean := True;  
   NoLabels_as_I: Boolean := False;
   NoLabels_as_tau: Boolean := False;
   --
   Product_Families: Boolean := False;  -- set to true by FMC or when "may" label found
   Lazy_Parsing: Boolean := False;
   Almost_Lazy_Parsing: Boolean := True;
   -------------- HARD LIMITS -----------------
   taskstacksize: constant Integer := 1024*1024*1024;
   --
   Vectors_Max: Natural := 4096;  
   Max_Elements: Natural := Vectors_Max * Vectors_Max;
   Cache_Module: Natural := 2**26;
   Key_Module: Natural := Cache_Module;
   --  Cache_Max: Natural := 8388607;  -- big prime like num
   --
   ThreadSafe: Boolean := False; --    -c or -m
   Cores: Natural :=0;           --     -c
   ModelCores: Natural :=0;      --     -m
   --
   -- configurations-global_env.adb:     Vectors_Max := Dyn_Store.Vectors_Max;
   -- mc2dot.adb:                        Slot_Size :=  Dyn_Store.Vectors_Max;
   --
   -- mc_server.adb:    PRAGMA STORAGE_SIZE( 8192 * 1024);
   --
   -- aut2dot.adb:  InputLine: String(1..10000); 
   --
   -- configurations-uml_parser.adb:  Max_States_Number: Positive := 100000;
   --
   -- configurations-global_env.adb:   Result:String(1..1000);     --- max outputline
   -- configurations.adb:              Result:String(1..1000);
   -- configurations-uml_explore.adb:  Result:String(1..30000);    --- max HTML line
   -- configurations-uml_model.adb:    All_Signals: Int64_Table(1..1000); -- max events in RTC step
   -- configurations-uml_types.adb:    Tmp: String(1..10000); -- max dot format line
   --
   -- configurations.adb:  Max_Line_Length: Positive := 1000;
   -- configurations.adb:  Max_Synchronizations: Positive := 10000;
   -- mc.adb:   Result:String(1..1000);
   --
   -- mc2dot.adb:  States_Limit: Integer := 2_000_000;         -- option "-s"
   -- mc2dot.adb:  Depth_Limit: Integer := 500_000;            -- option "-d"
   -- mc2dot.adb:  MaxBreadth: Natural := 100_000;  
   --
   -- uctl-computations_db.adb:     Reserve_Capacity(all_computations, 50000);
   -- uctl.adb:  Max_Explanation_Depth: Positive := 5000;
   -- uctl.adb:  Max_Line_Length: Positive := 10000;
   -- uctl.adb:  Max_Synchronizations: Positive := 10000;
   -- uctl.adb:   Result:String(1..10000);

end Flags;
