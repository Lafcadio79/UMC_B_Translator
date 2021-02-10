-- predefined UMC libraries (not to be modified!)
with Flags;
with UML_Types;
with UML_Parser;

-- Ada local libraries
with Ada.Text_IO;
with Ada.Strings;
with Ada.Command_Line;
with Ada.Strings.Fixed;
with Ada.Characters.Handling;
with Ada.Exceptions;

procedure Main is
   -- enable the use of the imported libraries (both predefined and local)
   use Flags;
   use UML_Types;
   use UML_Parser;
   use Ada.Exceptions;
   
   use Ada.Text_IO;
   use Ada.Strings;
   use Ada.Command_Line;
   use Ada.Strings.Fixed;
   use Ada.Characters.Handling;

   --------------------------------------
   -- object and variable declarations --
   --------------------------------------
   F_Out       : File_Type;
   This_Chart  : Chart;
   UMCfilename : String_ref;
   Machine_name: String_ref;
   B_Filename  : String_ref;
   
   ----------------------------
   -- Procedures & Functions --
   ----------------------------
   ----------------------- 
   -- returns the simplename fragment of a state fullname
   -- notice:  Fullnames always start with "Top."
   ----------------------- 
   function Simple (full: String) return String is
      n: Natural;
   begin
      n:= Index(full, ".", Backward);
      return full(n+1..full'Last);
   end Simple;

   
   ----------------------- 
   -- assumption: the source state of the first transition is the initial state.
   ----------------------- 
   function Get_Initial_State(SD: Natural) return string is
   begin 
      This_Chart := All_Charts(SD);
      return Simple(This_Chart.ChartTransitions(1).Source(1).FullName.all);
   end Get_Initial_State;
   
   function Is_Last_Active_Chart (SD: Natural) return Boolean is
   begin
      for I in SD+1 .. All_Charts'Last loop
         if Is_Active_Chart(I) then
            return False;
         end if;
      end loop;
      return True;
   end Is_Last_Active_Chart;
   
   ------------------------------------------------------------------------
   --           MANAGEMENT OF OBJECT LOCAL VARIABLES
   -- Class variables of type "obj" are considered UML port constants, and not
   --   modelled as ProB variables. They must appear at the beginning of the Vars list.
   -- Class variables of type "0..n" are defined in the same way in ProB
   -- Class variables of type "int" are defined in ProB with type "0..MAXINT"
   -- Class variables of type int[] are defined in ProB with type "seq(0..MAXINT)" 
   -- For each object with name "obj" is defined the variable "obj_STATE" with type
   --   "obj_STATES".  
   -- The type "obj_STATES" is an enumeration of the Chart simple states names (prefixed by obj).
   -- For each object "obj" are defined the variables "obj_buff" and "obj_databuff".
   -- A variable "obj_buff" has type "obj_SIGNALS" where "obj_SIGNALS" is the enumeration
   --   of all the signals accepted by the object.
   -- A variable "obj_databuff" has type "seq(0..MAXINT)"
   -- CONSTANTS ARE NOT RECOGNISED AS SUCH AND NOT DECLARED AS DEFINITIONS
   -- In ProB all vars decls are terminated by "," except tha last decl.
   ------------------------------------------------------------------------


   -- SETS clause body
   procedure print_set_signals (SD: Natural) is
   begin	  
      This_Chart := All_Charts(SD);
      Put_Line("SIGNALS = {");
      for K in This_Chart.ChartEvents'Range loop
         if This_Chart.ChartEvents(K).Kind'Image = "SIGNAL" then
            Put(This_Chart.ChartEvents(K).Name.all);
            if K = This_Chart.ChartEvents'Last  and then
              SD = All_Charts'Last then
               -- PROB does not require the ',' after the last set element name
               New_line;
            else
               Put_line(", ");
            end if;
         end if;
      end loop;
      Put_Line("};");
	  
      -- TODO the state set
	  
   end print_set_signals;
   
   
   -- The procedure takes from the UMC model all the object and variable names
   -- (within the body of the OBJECT and VARS clauses) and returns a list of
   -- variable label prefixed with respect to the given object.
   --
   -- e.g. Being O = {O1, O2} the set of the objects and V={a,b,c} the set of variables,
   -- the the procedure returns the cartesian product OxV={O1_a, O1_b, O1_c, O2_a, O2_b, O2_c}

   procedure print_varnames (SD: Natural) is
   begin
      This_Chart := All_Charts(SD);
      New_Line;
      Put_Line ("-- " & All_Charts(SD).Name.all);
      for K in This_Chart.ChartVars'Range loop
         if This_Chart.ChartVars(K).Kind = BoundedNat or else
             This_Chart.ChartVars(K).Kind = Number or else
             (This_Chart.ChartVars(K).Initial /= null and then
               (This_Chart.ChartVars(K).Initial.Image(1) in '0'..'9' or else
                This_Chart.ChartVars(K).Initial.Is_Vector /= null)) then     
            Put (This_Chart.Name.all & "_" & This_Chart.ChartVars(K).Name.all); 
            Put_line(",");
         else
            null;    -- ignore port variables
         end if;
      end loop;
      Put_Line(This_Chart.Name.all & "_buff,"); 
      Put_Line(This_Chart.Name.all & "_databuff,"); 
      Put (This_Chart.Name.all & "_STATE"); 
      if Is_Last_Active_Chart(SD) then
         New_line; -- no ',' after the last variable name
      else
        Put_line(",");
      end if;
   end print_varnames;


   procedure print_vartypes (SD: Natural) is
   begin
      This_Chart := All_Charts(SD);
      New_Line;
      Put_Line ("-- " & All_Charts(SD).Name.all);
      for K in This_Chart.ChartVars'Range loop
         if This_Chart.ChartVars(K).Kind = BoundedNat then 
            Put (This_Chart.Name.all & "_" & This_Chart.ChartVars(K).Name.all);
            Put (": 0.." & This_Chart.ChartVars(K).NatBound'Img);
            Put_line(" &");
         elsif This_Chart.ChartVars(K).Kind = Number or else
           (This_Chart.ChartVars(K).Initial /= null and then
            This_Chart.ChartVars(K).Initial.Image(1) in '0'..'9') then 
            Put (This_Chart.Name.all & "_" & This_Chart.ChartVars(K).Name.all);
            Put_line(" &");
         elsif This_Chart.ChartVars(K).Initial /= null and then
           This_Chart.ChartVars(K).Initial.Is_Vector /= null then        
            Put (This_Chart.Name.all & "_" & This_Chart.ChartVars(K).Name.all);  
            Put_line(" &");
         else 
            null;  -- ignore port variables
         end if;
      end loop;
      Put_Line(This_Chart.Name.all & "_buff: seq(" & This_Chart.Name.all & "_SIGNALS) &" ); 
      Put_Line(This_Chart.Name.all & "_databuff: seq(0..MAXINT) &"); 
      Put (This_Chart.Name.all & "_STATE: " & This_Chart.Name.all & "_STATES"); 
      if Is_Last_Active_Chart(SD) then
         New_line; -- no '&' after the last variable invariant
         else
            Put_line(" &");
         end if;
   end print_vartypes;

   procedure print_varinitvalues(SD: Natural) is
   begin
      This_Chart := All_Charts(SD);
      New_Line;
      Put_Line ("-- " & All_Charts(SD).Name.all);
      for K in This_Chart.ChartVars'Range loop
         if This_Chart.ChartVars(K).Initial /= null and then
            (This_Chart.ChartVars(K).Initial.Image(1) in '0'..'9' or else
             This_Chart.ChartVars(K).Initial.Image(1) ='[' ) then 
         Put (This_Chart.Name.all & "_" & This_Chart.ChartVars(K).Name.all);
            Put (":= " & This_Chart.ChartVars(K).Initial.Image.all);
            Put_line(";");
         else
            null; -- ignore port variables
         end if;
      end loop;
      Put_Line(This_Chart.Name.all & "_buff := [];"); 
      Put_Line(This_Chart.Name.all & "_databuff :=  [];"); 
      Put (This_Chart.Name.all & "_STATE := " & get_Initial_State(SD)); 
      if Is_Last_Active_Chart(SD) then
            New_line;
         else
            Put_line(";");
         end if;
   end print_varinitvalues;



   function UmlExpr_img (expr: umlExpr_Ref) return String is
   begin
      if expr.umlInt /= null then
         return IntExpr_Image(expr.umlInt.all);
      else 
         return BoolBoolExpr_Image(expr.umlBool.all);
      end if;
   end;

   function GetTarget(SD: Natural; K: Natural; A:Natural) return String is
      -- The target of a signal action can be either directly an object name
      -- or the name of a local variable initialised with the value of an object name.
      Actions: Actions_Table_Ref; 
      targetimg: String_ref;
   begin
      This_Chart := All_Charts(SD);
      Actions := This_Chart.ChartTransitions(K).Actions;
      targetimg := Actions(A).Signalled_Event.The_Target.Image;
      for V in This_Chart.ChartVars'Range loop
         if targetimg.all = This_Chart.ChartVars(V).name.all then
            -- this is a var name
            targetimg := This_Chart.ChartVars(V).Initial.Image;
            exit;
         end if;
      end loop;
      return targetimg.all;
   end GetTarget;

   procedure  print_actions(SD: Natural; K: Natural) is
      Actions: Actions_Table_Ref;
   begin
      This_Chart := All_Charts(SD);
      Actions := This_Chart.ChartTransitions(K).Actions;
      if Actions /= null then
         for A in Actions'Range loop
            if Actions(A).Kind = Assignment then
               Put_Line("  " & Actions(A).Assignment_Left_Image.all & 
                          " := " &  UmlExpr_img(Actions(A).Assignment_Right) ) ;
            elsif Actions(A).Kind = Signal then
               -- obj_buff := obj_buff <- obj_signal
               Put_Line("  " &  GetTarget(SD,K,A) & "_buff := " & 
                          GetTarget(SD,K,A) & "_buff <- " & 
                          GetTarget(SD,K,A) & "_" &  
                          Actions(A).Signalled_Event.The_Event.Name.all & ";");
            else 
               Put_line (Standard_Error, "print_actions: Unsupported Action");
            end if;
         end loop;
      end if;
   end print_actions;
   

  --  LAVORARE PER ESTENDERE QUESTA PROCEDURA

   procedure print_transitions(SD: Natural) is
      HasTrigger: Boolean := False;
   begin
      This_Chart := All_Charts(SD);
      for K in This_Chart.ChartTransitions'Range loop
         New_Line;
         --  Import preceding comments
         if This_Chart.ChartTransitions(K).Comms.all'Length >0 then
            for I in This_Chart.ChartTransitions(K).Comms.all'Range loop
			   -- print comments
               Put_Line(This_Chart.ChartTransitions(K).Comms(I).all);
            end loop;
         end if;
         --  Operation NAME
         if This_Chart.ChartTransitions(K).label /= null then
		    -- print transition labels
            Put_Line (This_Chart.Name.all & "_" & This_Chart.ChartTransitions(K).label.all & " =");
         else
            Put_Line (This_Chart.Name.all & "_nolabel" & Trim(k'Img, Left) & " =");
         end if;  -- label /= null
         --
         Put_Line ("PRE");
         -- Trigger CONDITION
		 -- if the transitions table is not empty and the trigger name is not equal to "-" then
         if This_Chart.ChartTransitions(K).trigger /= null and then
           This_Chart.ChartTransitions(K).trigger.name.all /= "-" then
            hasTrigger := True;
            Put_line("  " & This_Chart.Name.all & "_buff /= [] & ");
			Put_line("  first(" & This_Chart.Name.all & "_buff) = " &
                       This_Chart.Name.all & "_" &
                       This_Chart.ChartTransitions(K).trigger.name.all & " & ");  
         end if;	 
         --  Guard CONDITION
         --      to be done !!!
	 	 
         -- Source STATE CONDITION
         Put_Line("  " & This_Chart.name.all & "_STATE = " & 
                    Simple(This_Chart.ChartTransitions(K).Source(1).FullName.all));
         --
         Put_Line ("THEN");
         print_actions(SD,K);
         if HasTrigger then
            Put_line ("  " & This_Chart.Name.all & "_buff := " &
                        "tail(" & This_Chart.Name.all & "_buff);" );
            -- if trigger has data we should also adjust databuff
         end if;
         --
         Put_line ("  " & This_Chart.name.all & "_STATUS := " & 
                     Simple(This_Chart.ChartTransitions(K).Target(1).FullName.all));
         --
         Put_Line ("END");
      end loop;  -- K in ChartTransitions
   end print_transitions;

   
---------------
-- Main body --
---------------
begin
   if Ada.Command_Line.Argument_Count = 0  then
      Put_Line (" Usage:   Main   UMCFilename");
      return;
   end if;

   -- storing the input and parsing
   UMCfilename  := new String'(Ada.Command_Line.Argument(1));
   Machine_name := new String'(Ada.Command_Line.Argument(2));
   parse(UMCfilename.all);

   -- get the output filename
   B_Filename := new String'(UMCfilename.all(1 .. UMCfilename'Length - 4) & ".mch");

   -- creation of the output file
   Create(F_Out, Out_File, B_Filename.all);

   -- redirect the stdout to the F_Out file
   Put_Line("File mch created");
   Put_Line("Translating ...");
   Set_Output(F_Out);
   
   
   New_Line;
  
   -- MACHINE clause + name (set-valued and scalar-valued parameters to be added if required ...)
   Put("MACHINE " & Machine_name.all);

   New_Line(3);
   
   -- CONSTRAINTS clause
   Put_line("CONSTRAINTS ");
   for I in All_Charts'Range loop
      if Is_Active_Chart(I) then
         This_Chart := All_Charts(I);
         -- Procedure/Function TODO
      end if;
   end loop;
   
   New_Line(2);
      
   -- SETS clause
 
   Put_line("SETS ");
   for I in All_Charts'Range loop
      if Is_Active_Chart(I) then
         This_Chart := All_Charts(I);
         for K in This_Chart.ChartEvents'Range loop
            if This_Chart.ChartEvents(K).Name.all'Length > 0 then
               print_set_signals(I);
            end if;
            -- to avoid duplicate entries
            exit;
         end loop;
         -- to avoid duplicate entries
         exit;
      end if;
   end loop;

 
   New_Line(2);
   
 
   -- DEFINITIONS clause
   Put_line("DEFINITIONS ");
   for I in All_Charts'Range loop
      if Is_Active_Chart(I) then
         This_Chart := All_Charts(I); -- useless assignment
         -- Procedure/Function TODO
      end if;
   end loop;
 
   New_Line(2);
 
   -- CONSTANTS clause
   Put_line("CONSTANTS ");
   for I in All_Charts'Range loop
      if Is_Active_Chart(I) then
         This_Chart := All_Charts(I);   -- useless assignment
         -- Procedure/Function TODO
      end if;
   end loop;
 
   New_Line(2);
 
    -- PROPERTIES clause
   Put_line("PROPERTIES ");
   for I in All_Charts'Range loop
      if Is_Active_Chart(I) then
         This_Chart := All_Charts(I);   -- useless assignment
         -- Procedure/Function TODO
      end if;
   end loop;


   New_Line(2);
 
   -- VARIABLES clause
   Put_line("VARIABLES ");
   for I in All_Charts'Range loop
      if Is_Active_Chart(I) then
         print_varnames(I);
      end if;
   end loop;
  
   New_Line;

   -- INVARIANT clause
   Put_line("INVARIANT ");
   for I in All_Charts'Range loop
      if Is_Active_Chart(I) then
         print_vartypes(I);
      end if;
   end loop;
  
   New_Line;

   -- INITIALISATION clause
   Put_line("INITIALISATION ");
   for I in All_Charts'Range loop
      if Is_Active_Chart(I) then
         print_varinitvalues(I);
      end if;
   end loop;
  
   New_Line;

   -- OPERATIONS clause
   Put_Line("OPERATIONS ");
   for I in All_Charts'Range loop
      if Is_Active_Chart(I) then
         print_transitions(I);
      end if;
   end loop;
   
   New_Line(2);
   
   -- AMN (End of the Machine)
   Put_line("END");

   -- close the output file
   Set_Output(Standard_Output);
   Close(F_Out);
   
   -- stdout messages
   Put_Line(Standard_Output, "File " & B_Filename.all & " closed");   
   Put_Line(Standard_Output, "Done!");   

end Main;

--type SystemVar is record
--  Name: String_Ref;
--  Global_Num_Key: Positive;   --    All_Vars(V.Num_Mey) = V    --- UNUSED !!!!!!!
--  Local_Num_Key: Positive;
--  Chart: Natural :=0;          --  index in All_Charts
--  Observed: Boolean := False;
--  Initial: SimpleIntExpr_Ref;    --- constant integer literal or chartname
--  Kind: Value_Kind := Undefined;
--  TypeInfo: Natural :=0;  -- Kind=Object -> TypeInfo=Class index in All_Classes
--  StaticSize: Integer := -1;  --  v: int[20] -- statically sized
--end record;

--  .......  PROB .......
--VARIABLES 
--  O1counter, 
--  O1cond,
--  O1buff,   <<<<--------  alla ,
--  O12counter, 
--  O2cond,
--  O2buff
--
--INVARIANT 
--  counter: 0..100 &
--  cond: BOOL &
--  buff: seq(OBJ)
--  
--INITIALISATION 
--  cond := FALSE;
-- buff :=[]
 