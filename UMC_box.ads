with Ada.Unchecked_Deallocation;
with Ada.Text_IO; use Ada.Text_IO;
-- with Ada.Containers; 
with Ada.Strings; use Ada.Strings;
with Flags; use Flags;
package UMC_box is

   Runtime_Errors_Enabled: Boolean := True;
   Runtime_Errors_Count: Natural :=0;
   ------------------------------------------------------------------
   --------  Basic database types and utilitities ---------
   ------------------------------------------------------------------
  package Global_Env is
    --
   function Normalised_Label (Label : String) return String;
   function Tail (Source: String) return String;
   function Trim (Source: String) return String;
   function Factorial (N: Positive) return Positive;
   function Add_Line_Breaks(Source:String) return String;
   function Join (Source: String_Table; sep:String := "") return String;
   function InsertSorted (This: Int64; Here: Int64_Table) return Int64_Table;

    -- used by VMC/FMC
    Cache_Modules: array (1..6) of Positive :=
      (131071, 524287, 1048577, 2097151, 4194305, 8388607);
    Cache_Max: Natural := Cache_Modules(6);

  end Global_Env;
  use Global_Env;



package UML_Types is
--
-- pragma Elaborate_Body;
--

type Bool_Ref is access Boolean;
type BoolMat is array  (Positive range <>, Positive range <>) of Bool_Ref;
type BoolMat_Ref is access BoolMat;

StructBase: constant Integer := - 1_000_000_000;
ObjectBase: constant Integer := - 2_000_000_000;
--
IntFalse: constant Integer  := Integer'First;  -- was 0 (0 when used as index)
IntTrue: constant Integer  := Integer'Last;    -- was 1 (1 when used as index)
IntNullObject: constant Integer := ObjectBase;    -- was 0
IntOUTObject: constant Integer := ObjectBase-1;    -- was 0
IntEmptyStruct: constant Integer := StructBase - 1;   --was 0; (must be item #1 in Vectors_DB)
IntUndefinedValue: constant Integer := StructBase +1;  -- Notice not a vector but an int
--
--UndefinedStructure: Constant Integer := StructBase;
--
Is_Active: constant String_Ref := new String'("Is_Active");
--
-- entita' dichiarate tramite: Vars  x,y,z,w
--

type Value_Kind is (SpecialToken, umlString, Undefined, Composite,
                    Object, Bool, Number,BoundedNat,
                    Objvector, BoolVector, Numvector, Vector,
                    Objmatrix,Boolmatrix, Nummatrix, Matrix,
                    Objcube,Boolcube,Numcube, Cube);

--
subtype  TypeInfo is Natural;  -- for obj types, if positive, is the Class index
--
type Basic_Value is record
  Code:Integer :=0;
  Kind: Value_Kind := Undefined;
end record;
--
type SystemVar;
type SystemVar_Ref is access SystemVar;
type Vars_Table is array (Positive range <>) of SystemVar_Ref;
type Vars_Table_Ref is access Vars_Table;
No_Vars: Vars_Table := (1..0 => null);

--
-- entita' dichiarate tramite:  Events  a(x), b(y),e(z)
--
-- event names e events vars devono essere diverse da system vars
type EventVar;
type EventVar_Ref is access EventVar;
type EventVars_Table is array (Positive range <>) of EventVar_Ref;
type EventVars_Table_Ref is access EventVars_Table;
--
-- The type of Trigger or Transition variables costituting the Transition Env
type EventVar is record  --
  Name: String_Ref;    -- the name of the var
  Num_Key: Positive;   -- the index in the Transition Env
  Kind: Value_Kind := Undefined;
  TypeInfo: Natural :=0;  -- Kind=Object -> TypeInfo=Class index in All_Classes
end record;
No_EventVars: constant EventVars_Table := (1..0 => null);


type IntExpr;
type IntExpr_Ref is access IntExpr;
type IntExpr_Table;
type IntExpr_Table_Ref is access IntExpr_Table;

--  SIMPLE INTEGER EXPRESSIONS
-- expressione che compare in guards, assignments, e output actions
--  e.g.  x,y,z, 40
--  OCCHIO   che   objvar.selection  e' permessa e indica una function call  !!!
--
type SimpleIntExpr;
type SimpleIntExpr_Ref is access SimpleIntExpr;
type SimpleIntExpr_Table is array (Positive range <>) of SimpleIntExpr_Ref;
type SimpleIntExpr_Table_Ref is access SimpleIntExpr_Table;
--
type SimpleIntExpr is record
  Literal_Value: Integer :=0;
  Image: String_Ref;
  Local_Variable: Natural :=0 ;   -- ChartVars(E.Local_variable) = V
  Event_Variable: EventVar_Ref;
  Remote_Variable: Natural :=0;     ---  localvar.remotevar,  eventvar.remotevar
  Special_Token: String_Ref;        -- Head, Tail, Length, Null, Self, This, ...
  Kind: Value_Kind := Number;
  Is_Vector: SimpleIntExpr_Table_Ref;
  Is_Indexing: IntExpr_Table_Ref;     -- this is the index expression -- should become IntExpr_Table_Ref
  Head_Tail_Data : SimpleIntExpr_Ref;   --- added 30-06-2016
end record;

OUTObject: constant SimpleIntExpr_Ref :=
           new SimpleIntExpr'(IntOUTObject, new String'("OUT"), 0, null, 0, null, Object,null,null,null);

-- INTEGER EXPRESSIONS
-- e.g.  x+y, y+3, 4-3
type IntOp is (No_Op, Plus , Minus, Times, Div, Modulus, Enclosed, Join);
type IntExpr is record
 Left: IntExpr_Ref;
 Op: IntOp := No_Op;
 Right: IntExpr_Ref;
 Simple: SimpleIntExpr_Ref;
end record;

Zero: constant IntExpr_Ref :=
  new IntExpr'(null,No_OP,null,
                new SimpleIntExpr'(0, new String'("0"), 0, null, 0, null, Number,null,null,null));
Uno: constant IntExpr_Ref :=
  new IntExpr'(null,No_OP,null,
                new SimpleIntExpr'(1, new String'("1"), 0, null,0,  null, Number,null,null,null));

BoolTrue: constant IntExpr_Ref  :=
  new IntExpr'(null,No_OP,null,
                new SimpleIntExpr'(IntTrue, new String'("true"), 0, null, 0, null, Bool,null,null,null));
BoolFalse:constant IntExpr_Ref  :=
  new IntExpr'(null,No_OP,null,
                new SimpleIntExpr'(IntFalse, new String'("false"), 0, null, 0, null, Bool,null,null,null));

-- further initialized by UML_Types.adb
NullStruct: IntExpr_Ref :=
  new IntExpr'(null,No_OP,null,
                new SimpleIntExpr'(IntEmptyStruct, new String'("[]"), 0, null, 0, null, Composite,null,null,null));
pragma Volatile(NullStruct);

-- further initialized by UML_Types.adb
NullObject: IntExpr_Ref :=
  new IntExpr'(null,No_OP,null,
                new SimpleIntExpr'(IntNullObject, new String'("null"), 0, null, 0, null, Object,null,null,null));
pragma Volatile(NullObject);

type IntExpr_Table is array (Positive range <>) of IntExpr_Ref;
Empty_IntExpr_Table: constant IntExpr_Table := (1..0 => null);
Empty_IntExpr_Table_Ref: constant IntExpr_Table_Ref := new IntExpr_Table'(Empty_IntExpr_Table);

-- INTEGER TO BOOLEAN EXPRESSIONS
--  e.g.   x<4, y+1>x-1, z+y>0
type IntBoolOp is ( LT, GT, EQ, NE, LE, GE, ISIN);

type IntBoolExpr is record
  Left: IntExpr_Ref;
  Op: IntBoolOp;
  Right: IntExpr_Ref;
end record;

-- BOOLEAN TO BOOLEAN EXPRESSIONS
--  e.g.  (z+y>0  & x<4 )
type BoolBoolOp is (NoOp, AndOp, OrOp, NotOp);
-- TRANSITION GUARDS
type BoolBoolExpr;
type BoolBoolExpr_Ref is access BoolBoolExpr;
subtype Guard_Ref is BoolBoolExpr_Ref;

type BoolBoolExpr (Kind: BoolBoolOp := NoOp) is record
  case Kind is
    when NoOp =>
       Compare: IntBoolExpr;
    when others =>
       Left: Guard_Ref;
       Right: Guard_Ref;
  end case;
end record;

type  umlExpr is record
   umlInt: IntExpr_Ref;      -- integer expression, obj, literals,
   umlBool: BoolBoolExpr_Ref;  -- composite boolean expressions
end record;

type umlExpr_Ref is access umlExpr;
type umlExpr_Table is array (Positive range <>) of umlExpr_Ref;
type umlExpr_Table_Ref is access umlExpr_Table;
Empty_umlExpr_Table: umlExpr_Table := (1..0 => null);

-- il Kind della systemvar e' definito da Systemvar-initial.kind
type SystemVar is record
  Name: String_Ref;
  Global_Num_Key: Positive;   --    All_Vars(V.Num_Mey) = V    --- UNUSED !!!!!!!
  Local_Num_Key: Positive;
  Chart: Natural :=0;          --  index in All_Charts
  Observed: Boolean := False;
  Initial: SimpleIntExpr_Ref;    --- constant integer literal or chartname
  Kind: Value_Kind := Undefined;
  TypeInfo: Natural :=0;  -- Kind=Object -> TypeInfo=Class index in All_Classes
  StaticSize: Integer := -1;  --  v: int[20] -- statically sized
  NatBound: Integer := 255;  --   v: 0..n 
end record;

-- STATES:  SIMPLE, COMPOSITE, CONCURRENT, REGION,
type State;
type State_Ref is access State;
type States_Table is  array (Positive range <>) of State_Ref;
type States_Table_Ref is access States_Table;

-- SYSTEM EVENTS: as declared by: SystemEvents  r2,a2,e1,e2,f1,r1,e(x), a(x)
type Event;
type Event_Ref is access Event;
type Events_Table is array (Positive range <>) of Event_Ref;
type Events_Table_Ref is access Events_Table;
No_Events: constant Events_Table := (1..0 => null);

-- STATE TRANSITIONS
type Transition;
type Transition_Ref is access Transition;
type Transitions_Table is array (Positive range <>) of Transition_Ref;
type Transitions_Table_Ref is access Transitions_Table;

-- IL CATALOGO DI TUTTE LE STATE TRANSITIONS CLASSIFICATE PER TRIGGERING EVENT
type Triggered_Transitions is record
   This_Event: Event_Ref;                   -- Null event in case of completion
   These_Transitions: Transitions_Table_Ref;
end record;
type TransitionsCatalogue is
     array (Positive range <>) of Triggered_Transitions;
type TransitionsCatalogue_Ref is access TransitionsCatalogue;

type Event_Kind is (Signal, Operation, Undefined);  --  NEEDED  UNDEFINED
-- SYSTEM EVENTS come da appropriata dichiarazione,  riferiti
--  all'interno delle transitions come parte del trigger e della action
type Event is record
   Name: String_Ref;
   Num_Key: Natural := 1;     -- indice dell'evento nella tabella All_Events
   Params: EventVars_Table_Ref;  -- a 0 or positively sized table.
   Observed: Boolean := True;   -- TOBEMOVED
   Return_Type: Value_Kind := Undefined;
   Kind: Event_Kind := Undefined;
-- TypeInfo:Natural :=0  -- TO BE ADDED
end record;
--
-- Each (active =declared) Chart has  ChartEvents(1) = Null_Event
--
Null_Event: constant Event_Ref :=
     new Event'(Name => new String'("-"),
--              Chart => 0,
                Num_Key => 1,
                Params => new EventVars_Table'(No_EventVars),
                Observed => False,
                Return_Type => Undefined,
                Kind => Signal);
--
--  OUTChart.ChartEvents(1) = Null_Event
--  ERRChart.ChartEvents(2) = Lost_Event
--  OUTChart.ChartEvents(3) = Assign_Event
--
Lost_Event: constant Event_Ref :=
     new Event'(Name=> new String'("lostevent"),
                Num_Key => 2,
                Params => new EventVars_Table'(1..1 =>
                        new EventVar'(new String'("id"),1,Undefined,0)),
                Observed => True,
                Return_Type => Undefined,
                Kind => Signal);
Assign_Event: constant Event_Ref :=
     new Event'(Name=> new String'("assign"),
                Num_Key => 3,
                Params => new EventVars_Table'(
                        new EventVar'(new String'("var"),1,Undefined,0),
                        new EventVar'(new String'("ind"),2,Undefined,0),
                        new EventVar'(new String'("val"),3,Undefined,0)),
                Observed => True,
                Return_Type => Undefined,
                Kind => Signal);
--
Runtime_Error_Event: constant Event_Ref :=
     new Event'(Name=> new String'("Runtime_Error"),
                Num_Key => 4,
                Params => new EventVars_Table'(
                --        new EventVar'(new String'("val1"),1,Undefined,0),
                        1 => new EventVar'(new String'("val2"),2,Undefined,0)),
--                Params => new EventVars_Table(1..0),
                Observed => True,
                Return_Type => Undefined,
                Kind => Signal);
--
OpReturn_Event: constant Event_Ref :=
     new Event'(Name=> new String'("return"),
                Num_Key => 5,
                Params => new EventVars_Table'(
                   1..1 => new EventVar'(new String'("val"), 1, Undefined,0)),
                Observed => True,
                Return_Type => Undefined,
                Kind => Signal);
--
OpReturnInt_Event: constant Event_Ref :=
     new Event'(Name=> new String'("return"),
                Num_Key => 6,
                Params => new EventVars_Table'(
                   1..1 => new EventVar'(new String'("val"), 1, Number,0)),
                Observed => True,
                Return_Type => Undefined,
                Kind => Signal);
--
OpReturnObj_Event: constant Event_Ref :=
     new Event'(Name=> new String'("return"),
                Num_Key => 7,
                Params => new EventVars_Table'(
                   1..1 => new EventVar'(new String'("val"), 1, Object,0)),
                Observed => True,
                Return_Type => Undefined,
                Kind => Signal);
--
OpReturnBool_Event: constant Event_Ref :=
     new Event'(Name=> new String'("return"),
                Num_Key => 8,
                Params => new EventVars_Table'(
                   1..1 => new EventVar'(new String'("val"), 1, Bool,0)),
                Observed => True,
                Return_Type => Undefined,
                Kind => Signal);
--
OpReturnIntV_Event: constant Event_Ref :=
     new Event'(Name=> new String'("return"),
                Num_Key => 9,
                Params => new EventVars_Table'(
                   1..1 => new EventVar'(new String'("val"), 1, Numvector,0)),
                Observed => True,
                Return_Type => Undefined,
                Kind => Signal);
--
OpReturnObjV_Event: constant Event_Ref :=
     new Event'(Name=> new String'("return"),
                Num_Key => 10,
                Params => new EventVars_Table'(
                   1..1 => new EventVar'(new String'("val"), 1, Objvector,0)),
                Observed => True,
                Return_Type => Undefined,
                Kind => Signal);
--
OpReturnBoolV_Event: constant Event_Ref :=
     new Event'(Name=> new String'("return"),
                Num_Key => 11,
                Params => new EventVars_Table'(
                   1..1 => new EventVar'(new String'("val"), 1, Boolvector,0)),
                Observed => True,
                Return_Type => Undefined,
                Kind => Signal);
--
OpReturnIntM_Event: constant Event_Ref :=
     new Event'(Name=> new String'("return"),
                Num_Key => 12,
                Params => new EventVars_Table'(
                   1..1 => new EventVar'(new String'("val"), 1, Nummatrix,0)),
                Observed => True,
                Return_Type => Undefined,
                Kind => Signal);
--
OpReturnObjM_Event: constant Event_Ref :=
     new Event'(Name=> new String'("return"),
                Num_Key => 13,
                Params => new EventVars_Table'(
                   1..1 => new EventVar'(new String'("val"), 1, Objmatrix,0)),
                Observed => True,
                Return_Type => Undefined,
                Kind => Signal);
--
OpReturnBoolM_Event: constant Event_Ref :=
     new Event'(Name=> new String'("return"),
                Num_Key => 14,
                Params => new EventVars_Table'(
                   1..1 => new EventVar'(new String'("val"), 1, Boolmatrix,0)),
                Observed => True,
                Return_Type => Undefined,
                Kind => Signal);
--
OpReturnIntC_Event: constant Event_Ref :=
     new Event'(Name=> new String'("return"),
                Num_Key => 15,
                Params => new EventVars_Table'(
                   1..1 => new EventVar'(new String'("val"), 1, Numcube,0)),
                Observed => True,
                Return_Type => Undefined,
                Kind => Signal);
--
OpReturnObjC_Event: constant Event_Ref :=
     new Event'(Name=> new String'("return"),
                Num_Key => 16,
                Params => new EventVars_Table'(
                   1..1 => new EventVar'(new String'("val"), 1, Objcube,0)),
                Observed => True,
                Return_Type => Undefined,
                Kind => Signal);
--
OpReturnBoolC_Event: constant Event_Ref :=
     new Event'(Name=> new String'("return"),
                Num_Key => 17,
                Params => new EventVars_Table'(
                   1..1 => new EventVar'(new String'("val"), 1, Boolcube,0)),
                Observed => True,
                Return_Type => Undefined,
                Kind => Signal);
--
Runtime_Error_Event2: constant Event_Ref :=
     new Event'(Name=> new String'("Runtime_Error"),
                Num_Key => 18,
                Params => new EventVars_Table'(
                            new EventVar'(new String'("val1"),1,Undefined,0),
                            new EventVar'(new String'("val2"),2,Undefined,0)),
                Observed => True,
                Return_Type => Undefined,
                Kind => Signal);

-- EVENTUALLY AN EVENT_INSTANCE MIGHT REFER TO A TARGET OBJECT.
type Event_Instance is record
 The_Event: Event_Ref;
 The_Target: SimpleIntExpr_Ref;
 The_Args: umlExpr_Table_Ref;  -- a 0 or positively sized table
end record;
--
type Events_Instance_Table is array (Positive range <>) of Event_Instance;
type Events_Instance_Table_Ref is access Events_Instance_Table;
Empty_Events_Instance_Table: Events_Instance_Table(1..0);
  ------ := (1..0 => (null,0,null));

-- TRANSITION GUARDS
--subtype Guard_Expr is BoolBoolExpr;
--type Guard_Ref is access Guard_Expr;

-- TRANSITION ACTION(S)
-- come appare in   ... -(../event(x+1))-> ...
type Action;
type Action_Ref is access Action;
type Actions_Table is array (Positive range <>) of Action_Ref;
type Actions_Table_Ref is access Actions_Table;
No_Actions: constant Actions_Table := (1..0 => null);
--
--   Action_Kind = Call  is EVER USED?????? or opcalls are just mapped in Signals?
type Action_Kind is (Signal, Call, Function_Call, Assignment, OpReturn,
                      Whileloop, Exitloop, Forloop, VarDecl, Conditional);
type Action is record
   Kind: Action_Kind := Signal;
   Signalled_Event: Event_Instance;    -- return, signal, opcall, funcall,
    -- Assignment_Left is the local reference to the SystemVar. (index in localvars)
    --                references to Eventvars have a negative value (index in tenv)
   Assignment_Left: Integer;     -- positive values and indexes of Class Vars
                                 -- negative values are indexes of Transition Vars
   Assignment_Left_Image: string_ref;
--   Assignment_Left_Index: IntExpr_Ref;
   Assignment_Left_Indexes: umlExpr_Table_Ref := new umlExpr_Table'(Empty_umlExpr_Table);
   Assignment_Right: umlExpr_Ref;
   LoopCond: BoolBoolExpr_Ref;    -- while loop
   LoopBody: Actions_Table_Ref;   -- while loop,  for loop
   TVar: EventVar_Ref;    -- Kind = Vardecl
   TValue: umlExpr_Ref;   -- Kind = Vardecl
   IfCond: BoolBoolExpr_Ref;   -- Kind = Conditional
   ThenBody:  Actions_Table_Ref;  -- Kind = Conditional
   ElseBody: Actions_Table_Ref;  -- Kind = Conditional
   For_Var: EventVar_Ref;  -- for loop
   For_VarMin: EventVar_Ref;  -- for loop
   For_VarMax: EventVar_Ref;  -- for loop
   For_Min: IntExpr_Ref; -- for loop
   For_Max: IntExpr_Ref; -- for loop
   Env_Depth: Natural :=0;
end record;

--  FULL STATES DEFINITION
-- stateref are relocatable !!!  I.e.  ChartStates can be simple copied !!!(
type State_Kind is (Parallel, Composite, Simple);
type State is record
  Kind: State_Kind := Simple;
  SubStates: States_Table_Ref := new States_Table(1..0);
  LocalTransitions: Transitions_Table_Ref
      := new Transitions_Table(1..0);  -- only for Composite States
  OutgoingTransitions: TransitionsCatalogue_Ref :=
          new TransitionsCatalogue(1..0);
  FullName: String_Ref;
  Parent: State_Ref;
  FirstRuntimePosition: Natural :=0;  -- set by uml_model  LOCAL to the chart
  LastRuntimePosition: Natural :=0;   -- set by uml_model  LOCAL to the chart
  Depth: Positive := 1;   --  Parallel states are considered for the depth
  Priority: Positive := 1;--  Parallel States are NOT considered
  Ancestors: States_Table_Ref;    -- set by Set_Ancestors  (relocatable)
  Num_Key: Positive;    -- local ref
--  Chart: Natural :=0;
  Finals: States_Table_Ref;  -- set by Set_Finals for composite states
  Deferred: Events_Table_Ref := new Events_Table(1..0);   -- list of deferred events
  EntryActions: Actions_Table_Ref;
  ExitActions: Actions_Table_Ref;
end record;


type Transition_Kind is (Simple, Internal, Synch, Join, Fork);
type Transition is record
  Label: String_Ref;
  Comms: String_Table_ref := Empty_String_Table_Ref;
  Trigger: Event_Ref;
  Guard:  Guard_Ref;
  Actions: Actions_Table_Ref;
  Source: States_Table_Ref;
  Target: States_Table_Ref;
  Kind: Transition_Kind := Simple;
  Owner: State_Ref;
  Mask:  States_Table_Ref;    -- non dovrebbe essere globale, se valida per ogni
  Num_Key: Positive;
end record;

--
-- il tipo  "Chart" e' utilizzato (in modo ambiguo) per descrivere sia le classi che gli
-- oggetti (istanze delle classi) del sistema.
-- Variabili di tipo "Chart" sono memorizzate sia in "All_Classes" (tabella delle classi)
--   sia in All_Charts (tabella degli oggetti sia passivi che attivi).
-- La lista degli oggetti "attivi" (dei loro indici in All_Charts) e' memorizzata in Active_Objects
-- Una dichiarazione di "Chart" crea sia una entri come class, che una come object (quindi due istanze
--  del tipo Chart)
--
type Chart is record
  Name: String_Ref;            -- relocatable
  Comms: String_Table_ref := Empty_String_Table_Ref;
  Chart_Position: Natural := 0;  -- index in Active_Charts
  Top_State_Num: Integer := -1; -- set to 1 for active charts, (i.e. charts with vars or states)
      --  Top_State_Num=0 indica una classe passiva (senza transizioni),
      --  eventualmente una classe con solo attributi pubblicamente accessibili,
      --  che e' eventualmente possibile estendere in un tempo successivo con un body.
      --  (che puo aggiungere nuovi attributi e nuovo behavior)
      -- Top_State_Num=1 indica una classe di cui e' stato definito il body
      --   e quindi la cui definizione e' congelata.
      --Top_State_Num=-1  indica un placeholder di class (ancora da definire)
  ChartParent: Natural :=0;    -- the parent (or class) definition for the child
  ChartVars: Vars_Table_Ref := new Vars_Table'(No_Vars);    -- SPECIFIC
  ChartEvents: Events_Table_Ref := new Events_Table(1..0);  -- relocatable
  ObservedEvents: Bool_Table_Ref := new Bool_Table(1..0);  -- SPECIFIC
  ChartStates: States_Table_Ref:= new States_Table(1..0);   -- relocatable
  ChartTransitions: Transitions_Table_Ref := new Transitions_Table(1..0);
  ChartStates_Base: Natural := 0;   -- set by Clone ..
  ChartInterferences: BoolMat_Ref;    -- shared among all objects of the class
  NiceParallelism: Boolean := False;
  IsRandom: Boolean := False;
end record;

type Chart_Table is array (Positive range <>) of Chart;
type Chart_Table_Ref is access Chart_Table;
--
-- Initialized by the parser (Parse)
--

 type Binding is record
    Attribute: String_Ref;   --  ChartVars(The_Var).Name
    Value_Image: String_Ref;     --  Inital_Expr.Image
 end record;

 type Bindings_Table is array (Positive range <>) of Binding;
 type Bindings_Table_Ref is access Bindings_Table;

 type UML_Object is record
   The_Object: Natural :=0;  -- index in All_Charts
   Bindings: Bindings_Table_Ref := new Bindings_Table (1..0);
 end record;

 type Objects_Table is array (Positive range <>) of UML_Object;
 type Objects_Table_Ref is access Objects_Table;

 -- updated by UML_parser
 UML_Configuration: Objects_Table_Ref := new Objects_Table(1..0);
 pragma Volatile(UML_Configuration);

--  The top-level state (the first declared)
-- This_UML_Object: State_Ref;
--
--  All Charts are all the chart explicilty declared by Chart daclarations
--  and (called active charts) and all charts mentioned of prefix of signals
--  (however still not being var names).  E.g.  OUT,  ERR of just the names
--  of other entities not currently packaged inside the system.
--
OutChart: constant Chart  := 
                    (Name => new String'("OUT"),
                     Comms => Empty_String_Table_Ref,
                     Top_State_Num => 0,
                     Chart_Position => 0,
                     Chartparent => 1,
                     ChartVars => new Vars_Table(1..0),
                     ChartEvents =>  new Events_Table(1..0),
                     ObservedEvents =>  new Bool_Table(1..0),
                     ChartStates => new States_Table(1..0),
                     ChartTransitions => new Transitions_Table(1..0) ,
                     ChartStates_Base => 0,
                     ChartInterferences => null,
                     NiceParallelism => False,
                     IsRandom => False
                    );
ErrChart: constant Chart   :=
                    (Name => new String'("ERR"),
                     Comms => Empty_String_Table_Ref,
                     Top_State_Num => 0,
                     Chart_Position => 0,
                     ChartParent => 2,
                     ChartVars => new Vars_Table(1..0),
                     ChartEvents =>  new Events_Table(1..0),
                     ObservedEvents =>  new Bool_Table(1..0),
                     ChartStates => new States_Table(1..0),
                     ChartTransitions => new Transitions_Table(1..0),
                     ChartStates_Base => 0,
                     ChartInterferences => null,
                     NiceParallelism => False,
                     IsRandom => False
                    );
TokensChart: constant Chart   := 
                    (Name => new String'("Token"),
                     Comms => Empty_String_Table_Ref,
                     Top_State_Num => 0,
                     Chart_Position => 0,
                     ChartParent => 3,
                     ChartVars => new Vars_Table(1..0),
                     ChartEvents =>  new Events_Table(1..0),
                     ObservedEvents =>  new Bool_Table(1..0),
                     ChartStates => new States_Table(1..0),
                     ChartTransitions => new Transitions_Table(1..0),
                     ChartStates_Base => 0,
                     ChartInterferences => null,
                     NiceParallelism => False,
                     IsRandom => False
                    );
OBJ_UnspecifiedError: constant Chart   :=
                    (Name => new String'("UnspecifiedError"),
                     Comms => Empty_String_Table_Ref,
                     Top_State_Num => 0,
                     Chart_Position => 0,
                     ChartParent => 3,
                     ChartVars => new Vars_Table(1..0),
                     ChartEvents =>  new Events_Table(1..0),
                     ObservedEvents =>  new Bool_Table(1..0),
                     ChartStates => new States_Table(1..0),
                     ChartTransitions => new Transitions_Table(1..0),
                     ChartStates_Base => 0,
                     ChartInterferences => null,
                     NiceParallelism => False,
                     IsRandom => False
                    );

OBJ_InvalidGuard: constant Chart   := 
                    (Name => new String'("InvalidGuard"),
                     Comms => Empty_String_Table_Ref,
                     Top_State_Num => 0,
                     Chart_Position => 0,
                     ChartParent => 3,
                     ChartVars => new Vars_Table(1..0),
                     ChartEvents =>  new Events_Table(1..0),
                     ObservedEvents =>  new Bool_Table(1..0),
                     ChartStates => new States_Table(1..0),
                     ChartTransitions => new Transitions_Table(1..0),
                     ChartStates_Base => 0,
                     ChartInterferences => null,
                     NiceParallelism => False,
                     IsRandom => False
                    );

OBJ_InvalidIndex: constant Chart   := 
                    (Name => new String'("InvalidIndex"),
                     Comms => Empty_String_Table_Ref,
                     Top_State_Num => 0,
                     Chart_Position => 0,
                     ChartParent => 3,
                     ChartVars => new Vars_Table(1..0),
                     ChartEvents =>  new Events_Table(1..0),
                     ObservedEvents =>  new Bool_Table(1..0),
                     ChartStates => new States_Table(1..0),
                     ChartTransitions => new Transitions_Table(1..0),
                     ChartStates_Base => 0,
                     ChartInterferences => null,
                     NiceParallelism => False,
                     IsRandom => False
                    );

OBJ_CodingBug: constant Chart   :=  
                     (Name => new String'("UMC_Internal_Error"),
                     Comms => Empty_String_Table_Ref,
                     Top_State_Num => 0,
                     Chart_Position => 0,
                     ChartParent => 3,
                     ChartVars => new Vars_Table(1..0),
                     ChartEvents =>  new Events_Table(1..0),
                     ObservedEvents =>  new Bool_Table(1..0),
                     ChartStates => new States_Table(1..0),
                     ChartTransitions => new Transitions_Table(1..0),
                     ChartStates_Base => 0,
                     ChartInterferences => null,
                     NiceParallelism => False,
                     IsRandom => False
                    );

OBJ_UndefinedValue: constant Chart   := 
                    (Name => new String'("UndefinedValue"),
                     Comms => Empty_String_Table_Ref,
                     Top_State_Num => 0,
                     Chart_Position => 0,
                     ChartParent => 3,
                     ChartVars => new Vars_Table(1..0),
                     ChartEvents =>  new Events_Table(1..0),
                     ObservedEvents =>  new Bool_Table(1..0),
                     ChartStates => new States_Table(1..0),
                     ChartTransitions => new Transitions_Table(1..0),
                     ChartStates_Base => 0,
                     ChartInterferences => null,
                     NiceParallelism => False,
                     IsRandom => False
                    );

OBJ_InvalidTarget: constant Chart   := 
                    (Name => new String'("InvalidTarget"),
                     Comms => Empty_String_Table_Ref,
                     Top_State_Num => 0,
                     Chart_Position => 0,
                     ChartParent => 3,
                     ChartVars => new Vars_Table(1..0),
                     ChartEvents =>  new Events_Table(1..0),
                     ObservedEvents =>  new Bool_Table(1..0),
                     ChartStates => new States_Table(1..0),
                     ChartTransitions => new Transitions_Table(1..0),
                     ChartStates_Base => 0,
                     ChartInterferences => null,
                     NiceParallelism => False,
                     IsRandom => False
                    );

OBJ_InvalidIndexing: constant Chart   := 
                    (Name => new String'("InvalidIndexing"),
                     Comms => Empty_String_Table_Ref,
                     Top_State_Num => 0,
                     Chart_Position => 0,
                     ChartParent => 3,
                     ChartVars => new Vars_Table(1..0),
                     ChartEvents =>  new Events_Table(1..0),
                     ObservedEvents =>  new Bool_Table(1..0),
                     ChartStates => new States_Table(1..0),
                     ChartTransitions => new Transitions_Table(1..0),
                     ChartStates_Base => 0,
                     ChartInterferences => null,
                     NiceParallelism => False,
                     IsRandom => False
                    );


-- updated by UML_Parser
All_Classes: Chart_Table_Ref  :=  new Chart_Table'(OutChart, ErrChart, TokensChart);
pragma Volatile (All_Classes);

All_Charts: Chart_Table_Ref  :=
      new Chart_Table'
        (OutChart,ErrChart,TokensChart,
         OBJ_UnspecifiedError,OBJ_InvalidGuard,OBJ_InvalidIndex,
         OBJ_CodingBug,OBJ_UndefinedValue,OBJ_InvalidTarget,OBJ_InvalidIndexing);
pragma Volatile (All_Charts);
  --
  OUTObj: Integer := UML_Types.ObjectBase - 1;
  ERRObj: Integer := UML_Types.ObjectBase - 2;
  UnspecifiedError: Integer := UML_Types.ObjectBase - 4;
  InvalidGuard: Integer := UML_Types.ObjectBase - 5;
  InvalidIndex: Integer := UML_Types.ObjectBase - 6;
  CodingBug: Integer := UML_Types.ObjectBase - 7;  -- ("UMC_Internal_Error"),
  UndefinedValue: Integer := UML_Types.ObjectBase - 8;
  InvalidTarget: Integer := UML_Types.ObjectBase - 9;
  InvalidIndexing: Integer := UML_Types.ObjectBase - 10;

Predefined_Charts_Count : constant Natural := All_Charts.all'Length;
--
-- Active Charts are only those explicitly declared by "Chart" declarations
-- (for which an explicit Top State has been declared)
--  (it is table of Chart indexes, extended with each new Current_Chart)
--
Active_Charts: Num_Table_Ref := new Num_Table(1..0);                          --!!!!
pragma Volatile(Active_Charts);

function Is_Active_Chart(The_Chart: Natural) return Boolean;

-- The initial systemevent Queue;
-- This_Initial_Queue: Events_Instance_Table_Ref :=  -- a 0 or >0 sized table
--      new Events_Instance_Table'(Empty_Events_Instance_Table);

--  type Evolution_Data is record
--     Source: Int64;    -- Nick in SRC_DB
--     Selector: Natural;  -- index in All_Charts
--     Actions: Int64;   -- Nick in Signals_DB
--     Target: Int64;  -- Nick in SRC_DB
--     AbstractLabels: String_Tables_Vector_Ref := Empty_String_Tables_Vector_Ref;
--  end record;
--  No_Evolution: Evolution_Data := (0,0,0,0,null);
--  type Evolutions_Table is array (Positive range <>) of Evolution_Data;
--  Empty_Evolutions_Table : Evolutions_Table := (1..0 => No_Evolution);
--  type Evolutions_Table_Ref is access Evolutions_Table;
-- procedure Free is new Ada.Unchecked_Deallocation(Evolutions_Table,Evolutions_Table_Ref);

-------------------  GLOBAL DATA -----------------------
-- The table of all the Chart Variables definition
--  further initialized by Parse_System_Vars
--
--  They exactly correspond to the Runtime vars vector
--  Their Num_Key is their runtime position in the runtime confuguration
--
-- All_Vars_Count: Natural :=0;
--All_Vars: Vars_Table_Ref := new Vars_Table'(No_Vars);


-- The table of all system Events (parametric) definitions
--  further initialized bu Parse_Events
--
--  All_Events e' una lista GLOBALE a tutte le classi di
--   "nomi" di eventi (senza altri dati).
--  Quando viene elaborato un signal di una classe, il corrispondente evento
--  viene aggiunto alla lista (se non c'e' gia'), inizializzato con il suo nome
--  ed il numero dei suoi parametri.
--  Quando viene elaborata una dichiarazione di classe, il corrispondento
--  evento viene aggiunto alla lista. Se c'e' gia' viene verificata la
--  corrispondenza con il suo precedente profilo, che se incompleto (e.e.
--  nomi dei parametri mancanti) viene completato.
--  In questo caso la dichiarazione completa viene anche aggiunta alla
--  lista locale degli eventi della classe.
--
--  Il campo Num_Key e' l'indice dell'evento nella tabella All_Events.
--
All_Events: Events_Table_Ref :=
   new Events_Table'(Null_Event, Lost_Event, Assign_Event, 
                     Runtime_Error_Event,
                     OpReturn_Event,
                     OpReturnInt_Event,OpReturnObj_Event,OpReturnBool_Event,
                     OpReturnIntV_Event,OpReturnObjV_Event,OpReturnBoolV_Event,
                     OpReturnIntM_Event,OpReturnObjM_Event,OpReturnBoolM_Event,
                     OpReturnIntC_Event,OpReturnObjC_Event,OpReturnBoolC_Event,
                     Runtime_Error_Event2);
        --   Null_Event = "-";
        --   Lost_event= "ERR.lostevent(id)"
        --   Assign_event= "OUT.assign(var,index, val)
pragma Volatile(All_Events);

-------------------- observation utilities ----------------------
 --
 function Eval_Literal (Id: String) return Integer;
 --
-- Observed_Objects: Num_Table_Ref := new Num_Table(1..0); -- indexes in All_Charts
 --
-- procedure Reset_Observations;
-- procedure Set_Observations (Mode: Observation_Kind; Args: String);
 --
-- function Is_Observable(Current_Chart: Natural;
--                     This_Action: Action_Ref) return Boolean;
-- function Is_Observable(Target, Event_key: Natural) return Boolean;

--procedure HTML_Settings;
--procedure TXT_Settings;

--procedure Set_Parameter(Input_Line: String);

---------------------------------------------------------------------------
--           OBSERVATIONS
--
type AbstractionKind is (StateKind, ActionKind, BothKinds);
--
type BinOp is (NOOP, EQ, GT, LT, LE, GE, NE, ISIN);
--
type Rule_Left_Elem is record
  LTerm: String_Ref;                                  -- src
  LPartner: String_Ref;                               -- target
  LOp: String_Ref;                                    -- event /obj (inState clause)
  LMode: String_Ref;
  LArgs: String_Table_Ref;                            -- [arg1,arg2]/ (Top.S1.s2)
  ----
  IdsL: String_Table_Ref := Empty_String_Table_Ref;   -- [obj1,var]/$v/literal/
  LeftOp: BinOp :=NOOP;                               --  EQ/NE/LT/GT/LE/GE
  IdsR: String_Table_Ref :=  Empty_String_Table_Ref;  --  [obj2,var]/$v/literal
end record;

type Rule_Left is array (Positive range <>) of Rule_Left_Elem;
type Rule_Left_Ref is access Rule_Left;

type Observation_Rule is record
  Kind: AbstractionKind := BothKinds;                 -- State/Action
  Left: Rule_Left_Ref;
  RLabels: String_Table_Ref := Empty_String_Table_Ref;--  [mainlabel,aarg1,aarg2]
end record;
 --
 -- State:   inState(obj.state) and obj.var=$v  ->  mainlabel(aarg1,$v)
 -- State:   obj.var1 > obj.var2  ->  label(op,op,1)
 -- Action:  src:target.event<arg1,arg2> -> mainlabel(aarg1,aarg2)
 -- Action:  src:target.event!<$1,$2,*,a>   ->  mainlabel(event,$2,$1)
 -- Action:  $1:$2.$3!<>   ->  mainlabel($3,$2,$1)

 type Abstractdata is record
  Labels: String_Tables_Vector_Ref := Empty_String_Tables_Vector_Ref;
 end record;

Emptydata: Abstractdata;
Tau: Constant Abstractdata := Emptydata;

type Observations_Table is array (Positive Range <>) of Observation_Rule;
type Observations_Table_Ref is access Observations_Table;
Default_Rules: constant Observations_Table(1..1)  :=
 (1..1 =>   -- $1($*) -> $1($*)
                (ActionKind,
                 new Rule_Left'( 1 =>
                     (null,null,new String'("$1"),null,
                      new String_Table'(1..1 => new String'("$*")),
                      null, noop,null)),
                 new String_Table'(1 => new String'("$1"),
                                  2 => new String'("$*"))));
                                  
Error_Rules: constant Observations_Table(1..1)  :=
 (1..1 =>   -- $1($*) -> $1($*)
                (ActionKind,
                 new Rule_Left'( 1 =>
                     (null,new String'("ERR"),new String'("$1"),null,
                      new String_Table'(1..1 => new String'("$*")),
                      null, noop,null)),
                 new String_Table'(1 => new String'("$1"),
                                  2 => new String'("$*"))));


-- Observe_Transition_Labels: Boolean := False; -- set by Parser  MOVED TO FLAGS
New_Observations: Observations_Table_Ref := new Observations_Table(1..0);
All_Observations: Observations_Table_Ref;
pragma Volatile(All_Observations);

--------------------------------------------
  function Has_Random_Queues(Index:Natural) return Boolean;

 --------------   Print / Abstraction utilities ----------------------------
 function Literal_Value (Literal:String) return Integer;
 function is_Operation (The_Signal: Int_Table) return Boolean;
 function Arg_Value (The_Signal: Int_Table; ArgNum: Positive) return Integer;

 function Value_Image(This_Var:SystemVar_Ref;
                      This_Value:Integer) return String;
 function Value_Image (Code: Integer; Kind: Value_kind) return String;
 function Signal_Image (The_Signal: Int_Table;
                          Target_Included:Boolean := True) return String;
 function Event_Image (The_Signal: Int_Table) return String;
 function Target_Image (The_Signal: Int_Table) return String;
 function Arg_Image (The_Signal: Int_Table;
                       ArgNum: Positive; Chart:Natural) return String;
 function Args_Count (The_Signal: Int_Table) return Natural;
 function Normalized_Literal(Source: String) return String;
 function Trigger_Image (The_Signal: Int_Table) return String;
 function Actions_Image (These_Actions: Actions_Table) return String;
 function BoolBoolExpr_Image (This_BoolBoolExpr: BoolBoolExpr) return String;
 function IntExpr_Image (This_IntExpr: IntExpr) return String;
 function IntExpr_Kind (This_IntExpr: IntExpr) return Value_Kind;
 function umlExpr_Kind (This_umlExpr: umlExpr) return Value_Kind;
 function SignalAct_Image (This_Instance: Event_Instance) return String;
 function Transition_Image (This: Transition_Ref;
                              Label_Included: Boolean := True) return String;
 function Transition_DotImage (This: Transition_Ref;
                              Label_Included: Boolean := True) return String;
 function Transition_Label (This: Transition_Ref) return String;
 function Transitions_Image (This_Sequence:Transitions_Table) return String;

  function Is_Final (This: State_Ref) return Boolean ;
  function Is_Default_Initial (This: State_Ref) return Boolean;
end UML_Types;
use UML_Types;


--######################################################################################
package UML_Parser is
  --
  -- Parses the uml file indentified by the given name and creates
  -- the corresponding uml object in "This_UML_Object"
  --
  procedure Parse (Uml_File_Name: String);
  --
  Max_Line_Length: Positive := 10000;
  Max_Empty_lines: Natural := 500;
  Max_Comment_lines: Natural := 500;
  Max_Components: Positive := 500;
  Max_Synchronizations: Positive := 10000;
  --
end UML_Parser;
--######################################################################################
use UML_Parser;


end UMC_box;
