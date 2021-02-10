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

procedure UMC2B is
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
   Machine_name: String_ref := new String'("SYS");
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

  procedure Recursively_Print_Simple_Names (SD: Natural; StatesTable: States_Table) is
  begin
    for K in StatesTable'Range loop
      if StatesTable(K).Kind= Simple then
          Put("   " & All_Charts(SD).Name.all & "_" & Simple(StatesTable(K).FullName.all));
      else 
        Recursively_Print_Simple_Names(SD, StatesTable(K).SubStates.all);
      end if;
      if K < StatesTable'Last then
         Put_Line (", ");
      end if;
    end loop;
  end Recursively_Print_Simple_Names;


   -- for each object  define e set with the nams of signals accepted by the object.
   -- Since in B all sets cannot contain common names, we use a set for oach object
   -- and we prefix each signal with the object name. E.G:
   --  ICSL_SIGNALS = {ICSL_ICSL_tick, ICSL_SAI_CONNECT_indication, .. },
   procedure print_set_states (SD: Natural) is
   begin	  
      This_Chart := All_Charts(SD);
      New_line;
      Put_Line(All_Charts(SD).Name.all & "_STATES = {");
      -- The Top_level state "Top" is a single composite region which contains the chart.
      Recursively_Print_Simple_Names(SD, This_Chart.ChartStates(1).SubStates.all);
      Put_Line("};");
         -- add ";" also to the last set, since set-states if followed by set_signals
      -- TODO the state set
   end print_set_states;
   
   
   -- for each object  define e set with the nams of signals accepted by the object.
   -- Since in B all sets cannot contain common names, we use a set for oach object
   -- and we prefix each signal with the object name. E.G:
   --  ICSL_SIGNALS = {ICSL_ICSL_tick, ICSL_SAI_CONNECT_indication, .. },
   procedure print_set_signals (SD: Natural) is
   begin	  
      This_Chart := All_Charts(SD);
      New_line;
      if This_Chart.ChartEvents.all'Length > 0 then
         Put_Line(All_Charts(SD).Name.all & "_SIGNALS = {");
         for K in This_Chart.ChartEvents'Range loop
            if This_Chart.ChartEvents(K).Kind = Signal then
               --- SIGNAL namas are already supposed to be UNIQUE system wide
               --  no need of prefixing them with obj name
               Put("   " & This_Chart.ChartEvents(K).Name.all);
               if K < This_Chart.ChartEvents.all'Last then
                  Put_Line(", ");  --insert separator between set elements
                end if;
           end if;
         end loop;
         if Is_Last_Active_Chart(SD) then 
            Put_Line("}");
            -- skip ending "," for last set defintion.
         else
            Put_Line("};");
         end if;
      end if;
      -- TODO the state set
   end print_set_signals;
   
   
   procedure print_Token_Names is
      first_one: Boolean := True;
   begin
      for I in 11..All_Charts'Last loop
         if All_Charts(All_Charts(I).ChartParent).Name.all = "Token"  then
            if first_one then 
               New_Line;
               first_one := False;
            else
               Put_Line(",");
            end if;
            Put (All_Charts(I).Name.all); 
         end if;
      end loop;
      New_Line;
   end print_Token_Names;
   
   
   procedure print_Token_Values is
      first_one: Boolean := True;
   begin
      for I in 11..All_Charts'Last loop
         if All_Charts(All_Charts(I).ChartParent).Name.all = "Token"  then
            if  first_one then 
               New_Line;
               first_one := False;
            else
               Put_Line(" &");
            end if;
            Put (All_Charts(I).Name.all & " =" & Integer'Image(I-10)); 
            -- we choose to assign as default numeric value of a token its offset
            --  in All_Charts_table from the 10nth element.
         end if;
      end loop;
   end print_Token_Values;
   
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
      Put_Line ("// " & All_Charts(SD).Name.all);
      for K in This_Chart.ChartVars'Range loop
         if This_Chart.ChartVars(K).Kind = BoundedNat or else
            This_Chart.ChartVars(K).Kind = Bool or else
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
      Put_Line ("// " & All_Charts(SD).Name.all);
      for K in This_Chart.ChartVars'Range loop
         if This_Chart.ChartVars(K).Kind = BoundedNat then 
            Put (This_Chart.Name.all & "_" & This_Chart.ChartVars(K).Name.all);
            Put (": 0.." & This_Chart.ChartVars(K).NatBound'Img);
            Put_line(" &");
         elsif This_Chart.ChartVars(K).Kind = Number or else
           (This_Chart.ChartVars(K).Initial /= null and then
            This_Chart.ChartVars(K).Initial.Image(1) in '0'..'9') then 
            Put (This_Chart.Name.all & "_" & This_Chart.ChartVars(K).Name.all);
            Put (": 0..MAXINT");
            Put_line(" &");
         elsif This_Chart.ChartVars(K).Kind = Bool then
           Put (This_Chart.Name.all & "_" & This_Chart.ChartVars(K).Name.all);
            Put (": BOOL" );
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
      Put_Line(This_Chart.Name.all & "_databuff: seq(seq(0..MAXINT)) &"); 
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
      Put_Line ("// " & All_Charts(SD).Name.all);
      for K in This_Chart.ChartVars'Range loop
         if This_Chart.ChartVars(K).Initial /= null and then
            (This_Chart.ChartVars(K).Initial.Image(1) in '0'..'9' or else
             This_Chart.ChartVars(K).Initial.Image(1) ='[' or else
             To_Upper(This_Chart.ChartVars(K).Initial.Image.all) = "TRUE" or else 
             To_Upper(This_Chart.ChartVars(K).Initial.Image.all) = "FALSE") then 
         Put (This_Chart.Name.all & "_" & This_Chart.ChartVars(K).Name.all);
            Put (" := " & To_Upper(This_Chart.ChartVars(K).Initial.Image.all));
            Put_line(";");
         else
            null; -- ignore port variables
         end if;
      end loop;
      Put_Line(This_Chart.Name.all & "_buff := [];"); 
      Put_Line(This_Chart.Name.all & "_databuff :=  [];"); 
      Put (This_Chart.Name.all & "_STATE := " & 
           This_Chart.Name.all  & "_" & get_Initial_State(SD)); 
      if Is_Last_Active_Chart(SD) then
            New_line;
         else
            Put_line(";");
         end if;
   end print_varinitvalues;


  function SimpleIntExpr_Image (obj: String; This_Simple: SimpleIntExpr) return String is
  begin
     if This_Simple.Special_Token = null then
           if This_Simple.Local_Variable >0 then
              return obj & "_" & This_Simple.Image.all;
           elsif This_Simple.Image.all = "data" then
              return  "first(" & obj & "_databuff)";
           else 
              return  This_Simple.Image.all;
           end if;
     elsif This_Simple.Special_Token.all  = "Head" then
  Put_Line(Standard_Error, This_Simple.Image.all);
  Put_Line(Standard_Error, This_Simple.Head_Tail_Data.Image.all);
           return "first(" & SimpleIntExpr_Image(obj, This_Simple.Head_Tail_Data.all) & ")";
     elsif This_Simple.Special_Token.all  = "Tail" then
           return "tail(" & SimpleIntExpr_Image(obj,This_Simple.Head_Tail_Data.all) & ")";
     end if;
    --  IS THE FOLLOWIMG EVER NEEDED ? BOH
    if This_Simple.Local_Variable /= 0 then
        return obj & "_" & This_Simple.Image.all;
    elsif This_Simple.Event_Variable /= null then
        return This_Simple.Image.all;
    elsif This_Simple.Special_Token /= null then
        return This_Simple.Special_Token.all;
    else
      declare
        Tmp: String := Integer'Image(This_Simple.Literal_Value);
      begin
        if Tmp(1) =' ' then
          return Tmp (2..Tmp'Length);
        else
          return Tmp;
        end if;
      end;
    end if;
  end SimpleIntExpr_Image;

  function IntExpr_Image (obj: String; This_IntExpr: IntExpr) return String is
  begin
    case This_IntExpr.Op is
      when No_Op =>
        if This_IntExpr.left = null then
          return SimpleIntExpr_Image(obj, This_IntExpr.Simple.all);
        else
          if This_IntExpr.Left.Op = No_Op then
            return (IntExpr_Image(obj, This_IntExpr.Left.all));
          else
            return ("(" & IntExpr_Image(obj, This_IntExpr.Left.all) & ")" );
          end if;
        end if;
      when Plus | Join  =>
        return IntExpr_Image(obj, This_IntExpr.Left.all) &
         "+" & IntExpr_Image(obj, This_IntExpr.Right.all);
      when Times =>
        return IntExpr_Image(obj, This_IntExpr.Left.all) &
         "*" & IntExpr_Image(obj, This_IntExpr.Right.all);
      when Minus =>
        return IntExpr_Image(obj, This_IntExpr.Left.all) &
         "-" & IntExpr_Image(obj, This_IntExpr.Right.all);
      when Div =>
        return IntExpr_Image(obj, This_IntExpr.Left.all) &
         "/" & IntExpr_Image(obj, This_IntExpr.Right.all);
      when Modulus =>
         return    IntExpr_Image(obj,This_IntExpr.Left.all) &
         " mod " & IntExpr_Image(obj, This_IntExpr.Right.all);
      when Enclosed =>  
         return  "(" & IntExpr_Image(obj, This_IntExpr.Left.all) & ")" ;
    end case;
  end IntExpr_Image ;



  function IntBoolExpr_Image (obj: String; This_IntBoolExpr: IntBoolExpr) return String is
  begin
    case This_IntBoolExpr.Op is
      when LT =>
         return  IntExpr_Image(obj, This_IntBoolExpr.Left.all) &
          " < " & IntExpr_Image(obj, This_IntBoolExpr.Right.all);
      when GT =>
         return  IntExpr_Image(obj, This_IntBoolExpr.Left.all) &
          " > " & IntExpr_Image(obj, This_IntBoolExpr.Right.all);
      when EQ =>
         return  IntExpr_Image(obj, This_IntBoolExpr.Left.all) &
          " = " & IntExpr_Image(obj, This_IntBoolExpr.Right.all);
      when NE =>
         return  IntExpr_Image(obj,This_IntBoolExpr.Left.all) &
         " /= " & IntExpr_Image(obj, This_IntBoolExpr.Right.all);
      when LE =>
         return   IntExpr_Image(obj, This_IntBoolExpr.Left.all) &
          " <= " & IntExpr_Image(obj, This_IntBoolExpr.Right.all);
      when GE =>
         return  IntExpr_Image(obj, This_IntBoolExpr.Left.all) &
          " >= " & IntExpr_Image(obj, This_IntBoolExpr.Right.all);
      when ISIN =>
         return IntExpr_Image(obj,This_IntBoolExpr.Left.all) &
          " in " & IntExpr_Image(obj,This_IntBoolExpr.Right.all);
    end case;
  end IntBoolExpr_Image;


  function BoolBoolExpr_Image (obj: String; This_BoolBoolExpr: BoolBoolExpr) return String is
  begin
     case This_BoolBoolExpr.Kind is
       when NoOp =>
         return IntBoolExpr_Image(obj, This_BoolBoolExpr.Compare);
       when NotOp =>
         return "(not "& BoolBoolExpr_Image(obj, This_BoolBoolExpr.Left.all) & ")" ;
       when AndOp =>
         return "(" & BoolBoolExpr_Image(obj, This_BoolBoolExpr.Left.all) &
             ") & (" & BoolBoolExpr_Image(obj, This_BoolBoolExpr.Right.all) & ")" ;
       when OrOp =>
         return "(" & BoolBoolExpr_Image(obj, This_BoolBoolExpr.Left.all) &
             ") or (" & BoolBoolExpr_Image(obj, This_BoolBoolExpr.Right.all) & ")" ;
     end case;
  end BoolBoolExpr_Image;


   function UmlExpr_img (obj: String; expr: umlExpr_Ref) return String is
   begin
      if expr.umlInt /= null then
         return IntExpr_Image(obj, expr.umlInt.all);
      else 
         --  only  TRUE / FALSE  MUST BE PUT in UPPERCASE??
         return BoolBoolExpr_Image(obj,expr.umlBool.all);
      end if;
   end;

   function GetTarget(A: Action_Ref) return String is
      -- The target of a signal action can be either directly an object name
      -- or the name of a local variable initialised with the value of an object name.
      targetimg: String_ref;
   begin
      targetimg := A.Signalled_Event.The_Target.Image;
      for V in This_Chart.ChartVars'Range loop
         if targetimg.all = This_Chart.ChartVars(V).name.all then
            -- this is a var name
            targetimg := This_Chart.ChartVars(V).Initial.Image;
            exit;
         end if;
      end loop;
      return targetimg.all;
   end GetTarget;

   function Op_image (Op: IntBoolOp) return String is
   begin
     case Op is
       when LT  => return "<";
       when LE  => return "<=";
       when GT  => return ">";
       when GE  => return ">=";
       when NE  => return "/=";
       when EQ  => return "==";
       when ISIN => return "in";
     end case;
   end Op_image;
   
   -- for each UML actions prints the corresponding ProB image, 
   -- terminating and setting a newline after each each but the last one
   --   see the case:   VAR vv IN actions END
   -- If actions table is empty, prints "skip"
   -- The supported actions are
   --  + Simple assignments
   --  + Conditionals
   --  + Sending of signals. (OUT signal are translated as "skip");
   --  + declarations of transition vars
   procedure  print_actions(obj: String; 
                            Actions: Actions_Table_Ref;
                            Indent:Natural := 2) is
     I: Natural := Indent;
     HasTVAR: Boolean := False;
     Space: String := "      ";
   begin
      if Actions /= null and then Actions.all'Length > 0 then
         for A in Actions'Range loop
            --   ASSIGNMENTS
            if Actions(A).Kind = Assignment then
                -- "OBJPrefix_"  MISSING
               Put(Space(1..I) & obj & "_" & Actions(A).Assignment_Left_Image.all & 
                          " := " &  UmlExpr_img(obj, Actions(A).Assignment_Right));
            -- SENDING of SIGNALS
            elsif Actions(A).Kind = Signal then
               -- obj_buff := obj_buff <- obj_signal
               --  DO NOT TRANSLATE OUT.EVENT seignals !!
               if GetTarget(Actions(A)) /= "OUT" then
                 Put(Space(1..I) &  
                     GetTarget(Actions(A)) & "_buff := " & 
                     GetTarget(Actions(A)) & "_buff <- " & 
                     Actions(A).Signalled_Event.The_Event.Name.all );
                 -----
                 --- MUST PUT DATA WHEN NEEDED !!!!!!
                 if Actions(A).Signalled_Event.The_Event.Params.all'Length >0  then
                 Put_line(";");
                 Put(Space(1..I) &
                     GetTarget(Actions(A)) & "_databuff := " & 
                     GetTarget(Actions(A)) & "_databuff <- " & 
                     UmlExpr_img(obj, Actions(A).Signalled_Event.The_Args(1)) );
                          -- we suppose to have just ONE parameter !
                 end if;
                 -----
               else
                  Put(Space(1..I) & "skip");
               end if;
            -- CONDITIONALS
            elsif Actions(A).Kind = Conditional  then
                 Put(Space(1..I) & "IF " &
                 -- "OBJPrefix_"  MISSING
                 IntBoolExpr_Image(obj, Actions(A).IFcond.Compare));
                 Put_Line(Space(1..Indent) & "THEN");
                 print_actions(obj, Actions(A).ThenBody, 4);
                 New_Line;
               if Actions(A).ElseBody /= null then
                 Put_Line( Space(1..I) & "ELSE");
                  print_actions(obj, Actions(A).ElseBody, 4);
                  New_Line;
                end if;
              Put(Space(1..I) & "END");
            -- TRANSITION VAR DECL
            elsif Actions(A).Kind = vardecl  then
               HasTVAR := True;
               Put_Line(Space(1..I) &
                        "VAR " & Actions(A).TVar.Name.all & " IN");
               I := I +2;
               Put (Space(1..I) &
                    Actions(A).TVar.Name.all & 
                    " :=  " & UmlExpr_img(obj, Actions(A).TValue));
            else 
               Put_line (Standard_Error, "print_actions: Unsupported Action");
            end if;
            if A < Actions'Last then
               Put_line(";");
            else
              if HasTVAR then
                 New_Line;
                 I := I-2;
                 Put (Space(1..I) & "END");
                 -- TVAR scope ends at the end the actions sequence, no ";" needed
              end if;
            end if;
         end loop;
      else -- no UML transition actions
       Put (Space(1..I) & "skip");
      end if;
   end print_actions;
   

   procedure print_transitions(SD: Natural) is
      HasTrigger: Boolean := False;
      HasArg: Boolean := False;
   begin
      This_Chart := All_Charts(SD);
      for K in This_Chart.ChartTransitions'Range loop
         HasTrigger := False;
         HasArg := False;
         New_Line;
         -------  Import preceding comments -----
         if This_Chart.ChartTransitions(K).Comms.all'Length >0 then
            for I in This_Chart.ChartTransitions(K).Comms.all'Range loop
			   -- print comments
               Put_Line("// " & This_Chart.ChartTransitions(K).Comms(I).all);
            end loop;
         end if;
         --------- Operation NAME  -------
         if This_Chart.ChartTransitions(K).label /= null then
		    -- print transition labels
            Put_Line (This_Chart.ChartTransitions(K).label.all & " =");
         else
            Put_Line (This_Chart.Name.all & "_nolabel" & Trim(k'Img, Left) & " =");
         end if;  -- label /= null
         ---------- PRE -------------
         Put_Line ("PRE");
         
         -- Trigger CONDITION
		 -- if the transitions table is not empty and the trigger name is not equal to "-" then
         if This_Chart.ChartTransitions(K).trigger /= null and then
           This_Chart.ChartTransitions(K).trigger.name.all /= "-" then
            HasTrigger := True;
            HasArg := This_Chart.ChartTransitions(K).Trigger.Params.all'Length >0;
            Put_Line("  " & This_Chart.Name.all & "_buff /= [] & ");
			Put_Line("  first(" & This_Chart.Name.all & "_buff) = " &
                       This_Chart.ChartTransitions(K).trigger.name.all & " & ");  
         end if;	 
         
         --  Guard CONDITION
         if This_Chart.ChartTransitions(K).Guard /= null then
            Put_Line ("  " & 
               BoolBoolExpr_Image(This_Chart.Name.all,
                                  This_Chart.ChartTransitions(K).Guard.all) & " &");
	 	 end if;
	 	 
         -- Source STATE CONDITION
         Put_Line("  " & This_Chart.name.all & "_STATE = " & This_Chart.Name.all & 
                "_" & Simple(This_Chart.ChartTransitions(K).Source(1).FullName.all));
         ------------ THEN ---------
         Put_Line ("THEN");
         print_actions(This_Chart.Name.all, This_Chart.ChartTransitions(K).Actions);
         Put_line(";");
         Put_line("  //");
         if HasTrigger and HasArg then
            Put_line ("  " & This_Chart.Name.all & "_buff := " &
                        "tail(" & This_Chart.Name.all & "_buff);" );
            Put_Line ("  " & This_Chart.Name.all & "_databuff := " &
                        "tail(" & This_Chart.Name.all & "_databuff);" );
         elsif HasTrigger then
            Put_line ("  " & This_Chart.Name.all & "_buff := " &
                        "tail(" & This_Chart.Name.all & "_buff);" );
         end if;
         Put_line ("  " & This_Chart.name.all & "_STATE := " & This_Chart.Name.all & 
                "_" & Simple(This_Chart.ChartTransitions(K).Target(1).FullName.all));
         --
         Put ("END");
         if K < This_Chart.ChartTransitions'Last then
            Put_line(";");
         end if;
      end loop;  -- K in ChartTransitions
   end print_transitions;

   
---------------
-- Main body --
---------------
begin
   if Ada.Command_Line.Argument_Count = 0  then
      Put_Line (" Usage:   Main   UMCFilename [B_machine_name]");
      return;
   end if;

   -- storing the input and parsing
   UMCfilename  := new String'(Ada.Command_Line.Argument(1));
   if Ada.Command_Line.Argument_Count > 1 then
      Machine_name := new String'(Ada.Command_Line.Argument(2));
   else 
     null; -- keep the default "SYS" machine name
   end if;
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
   Put_Line("MACHINE " & Machine_name.all);

   New_Line;
   
   -- CONSTRAINTS clause
   --Put_line("CONSTRAINTS ");
   --
   --New_Line;
      
   -- SETS clause
   -- Th SETS that need to be defined are, for each active object:
   --  + the set of Signals accepted by the object (each set definition terminated by a ","
   --  + the set of simple state names used as source and target of transitions
   --     (all byt he last set definition must be terminated by ",")
   -- + The set BOOL = {TRUE, FALSE} is predefined
   Put_line("SETS ");
   
   for I in All_Charts'Range loop
      if Is_Active_Chart(I) then
           print_set_states(I);
      end if;
   end loop;
   
   for I in All_Charts'Range loop
      if Is_Active_Chart(I) then
           print_set_signals(I);
      end if;
   end loop;

   New_Line;
   
   -- DEFINITIONS clause
   Put_line("DEFINITIONS ");
   New_line;
   Put_line("SET_PREF_MAXINT == 30;");
   Put_line("SET_PREF_MININT == 0;");

   New_Line;
   
   -- CONSTANTS clause
   -- we can put here all the uppercase names of Token
   Put_line("CONSTANTS ");
   print_Token_Names;
   
   New_Line;
   
   
    -- PROPERTIES clause
   -- we must put here all the numeric values of Tokens
   Put_line("PROPERTIES ");
   print_Token_Values;

   New_Line(2);
 
   -- VARIABLES clause
   Put_line("VARIABLES ");
   for I in All_Charts'Range loop
      if Is_Active_Chart(I) then
         print_varnames(I);
      end if;
   end loop;
   Put_line(" ,");
   Put_Line("  data");
  
   New_Line;

   -- INVARIANT clause
   Put_line("INVARIANT ");
   for I in All_Charts'Range loop
      if Is_Active_Chart(I) then
         print_vartypes(I);
      end if;
   end loop;
   Put_line(" &");
   Put_Line("  data: seq(0..MAXINT)");
  
   New_Line;

   -- INITIALISATION clause
   Put_line("INITIALISATION ");
   for I in All_Charts'Range loop
      if Is_Active_Chart(I) then
         print_varinitvalues(I);
      end if;
   end loop;
   Put_line(";");
   Put_Line("  data:= []");
  
   New_Line;

   -- OPERATIONS clause
   Put_Line("OPERATIONS ");
   for I in All_Charts'Range loop
      if Is_Active_Chart(I) then
         print_transitions(I);
         if Is_Last_Active_Chart(I) then
            New_line; -- no '&' after the last variable invariant
         else
            Put_line(";");
         end if;
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

end UMC2B;

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
 