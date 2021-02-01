-- predefined libraries (not to be modified!)
with Flags;
with UML_Types;
with UML_Parser;

-- Ada local libraries
with Ada.Text_IO;
with Ada.Strings;
with Ada.Command_Line;
with Ada.Strings.Fixed;
with Ada.Characters.Handling;

procedure Main is
   -- enable the use of the imported libraries (both predefined and local)
   use Flags;
   use UML_Types;
   use UML_Parser;
   
   use Ada.Text_IO;
   use Ada.Strings;
   use Ada.Command_Line;
   use Ada.Strings.Fixed;
   use Ada.Characters.Handling;

   ------------------------------------------------------------
   -- object, variable, procedure, and function declarations --
   ------------------------------------------------------------
   F_Out       : File_Type;
   This_Chart  : Chart;
   UMCfilename : String_ref;
   
   -- procedure set_variables (SD: Natural) is separate;
   -- The procedure takes from the UMC model all the object and variable names
   -- (within the body of the OBJECT and VARS clauses) and returns a list of
   -- variable label prefixed with respect to the given object.
   --
   -- e.g. Being O = {O1, O2} the set of the objects and V={a,b,c} the set of variables,
   -- the the procedure returns the cartesian product OxV={O1_a, O1_b, O1_c, O2_a, O2_b, O2_c}
   procedure print_varnames (SD: Natural) is
   begin
      This_Chart := All_Charts(SD);
      Put_Line ("-- " & All_Charts(SD).Name.all);
      for K in This_Chart.ChartVars'Range loop
         Put (This_Chart.Name.all & "_" & This_Chart.ChartVars(K).Name.all);
         if K = This_Chart.ChartVars'Last  and then
           sd = All_Charts'Last then
            -- PROB does not require the ',' after the last variable name
            New_line;
         else
            Put_line(",");
         end if;
      end loop;
   end print_varnames;

   procedure print_vartypes (SD: Natural) is
   begin
      This_Chart := All_Charts(SD);
      Put_Line ("-- " & All_Charts(SD).Name.all);
      for K in This_Chart.ChartVars'Range loop
         Put (This_Chart.Name.all & "_" & This_Chart.ChartVars(K).Name.all);
         if This_Chart.ChartVars(K).Kind = BoundedNat then 
            Put (": 0.." & This_Chart.ChartVars(K).NatBound'Img);
         elsif This_Chart.ChartVars(K).Kind = Number or else
           (This_Chart.ChartVars(K).Initial /= null and then
            This_Chart.ChartVars(K).Initial.Image(1) in '0'..'9') then 
            Put (": 0..MAXINT");
         elsif This_Chart.ChartVars(K).Initial /= null and then
           This_Chart.ChartVars(K).Initial.Is_Vector /= null then        
            Put (": seq(0..MAXINT)");
         else 
            New_Line;
            Put_line (Standard_Error, "Unsupported vartype");
         end if;
         if K = This_Chart.ChartVars'Last  and then
           sd = All_Charts'Last then
            -- PROB does not require the '&' after the last variable type
            New_line;
         else
            Put_line(" &");
         end if;
      end loop;
   end print_vartypes;

   procedure print_varinitvalues(SD: Natural) is
   begin
      This_Chart := All_Charts(SD);
      Put_Line ("-- " & All_Charts(SD).Name.all);
      for K in This_Chart.ChartVars'Range loop
         Put (This_Chart.Name.all & "_" & This_Chart.ChartVars(K).Name.all);
         if This_Chart.ChartVars(K).Initial /= null then 
            Put (":= " & This_Chart.ChartVars(K).Initial.Image.all);
         else
            New_Line;
            Put_line (Standard_Error, "Missing Initial Value");
         end if;
         if K = This_Chart.ChartVars'Last  and then
           sd = All_Charts'Last then
            New_line;
         else
            Put_line(";");
         end if;
      end loop;
   end print_varinitvalues;


   -- returns the simplename fragment of a state fullname
   function Simple (full: String) return String is
      n: Natural;
   begin
      n:= Index(full, ".", Backward);
      return full(n+1..full'Last);
   end Simple;


   function UmlExpr_img (expr: umlExpr_Ref) return String is
   begin
      if expr.umlInt /= null then
         return IntExpr_Image(expr.umlInt.all);
      else 
         return BoolBoolExpr_Image(expr.umlBool.all);
      end if;
   end;

   function GetTarget(SD: Natural; K: Natural; A:Natural) return String is
      -- The target of a signal  action can be either directly an object name
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

   procedure print_transitions(SD: Natural) is
      HasTrigger: Boolean := False;
   begin
      This_Chart := All_Charts(SD);
      for K in This_Chart.ChartTransitions'Range loop
         New_Line;
         --  Import preceding comments
         if This_Chart.ChartTransitions(K).Comms.all'Length >0 then
            for I in This_Chart.ChartTransitions(K).Comms.all'Range loop
               Put_Line(This_Chart.ChartTransitions(K).Comms(I).all);
            end loop;
         end if;
         --  Operation NAME
         if This_Chart.ChartTransitions(K).label /= null then
            Put_Line (This_Chart.Name.all & "_" & This_Chart.ChartTransitions(K).label.all & " =");
         else
            Put_Line (This_Chart.Name.all & "_nolabel" & Trim(k'Img, Left) & " =");
         end if;  -- label /= null
         --
         Put_Line ("PRE");
         -- Trigger CONDITION
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
         Put_Line("  " & This_Chart.name.all & "_STATUS = " & 
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
   UMCfilename := new String'(Ada.Command_Line.Argument(1)) ;
   parse(UMCfilename.all);
   -----
   

   -- creation of the output file
   Create(F_Out, Out_File, "prova.mch");
   -- redirect of the stdout to such file
   Set_Output(F_Out);
   
   Put_Line(Standard_Output, "File mch created");
   Put_Line(Standard_Output, "Translating ...");
   
   New_Line;
   Put_line("VARIABLES ");
   for I in All_Charts'Range loop
      if Is_Active_Chart(I) then
         This_Chart := All_Charts(I);
         print_varnames(I);
      end if;
   end loop;
  
   New_Line;
   Put_line("INVARIANT ");
   for I in All_Charts'Range loop
      if Is_Active_Chart(I) then
         This_Chart := All_Charts(I);
         print_vartypes(I);
      end if;
   end loop;
  
   New_Line;
   Put_line("INITIALISATION ");
   for I in All_Charts'Range loop
      if Is_Active_Chart(I) then
         This_Chart := All_Charts(I);
         print_varinitvalues(I);
      end if;
   end loop;
  
   New_Line;
   Put_line("OPERATIONS ");
   for I in All_Charts'Range loop
      if Is_Active_Chart(I) then
         This_Chart := All_Charts(I);
         print_transitions(I);
      end if;
   end loop;
   
   -- close of the output file
   Close(F_Out);
   Put_Line(Standard_Output, "File mch closed");   
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
