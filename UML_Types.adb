-- local libraries
with Ada.Text_IO;
with Ada.Strings;
with Ada.Strings.Fixed;

-- separate (Configurations.Kernel)

package body UML_Types is
   -- enable the use of the imported libraries
   use Ada.Text_IO;
   use Ada.Strings;
   use Ada.Strings.Fixed;

   ----------------------
   --  MEMORY FRIENDLY --
   ----------------------

   function Append (To: String_Ref; What: String) return String_Ref is
      Tmp: String_Ref := To;
      Result: String_Ref;
   begin
      if To = null then
         Result := new String'(What);
      else
         Result := new String'(To.all & What);
         Free(Tmp);
      end if;
      return Result;
   end Append;

   
   function Args_Image (These_Params: EventVars_Table;
                        With_Types: Boolean := True) return String is
   begin
      if These_Params'Length = 1 then
         return These_Params(These_Params'First).Name.all;
      else
         return These_Params(These_Params'First).Name.all &
           "," & Args_Image(These_Params(These_Params'First+1 ..
                              These_Params'Last));
      end if;
   end Args_Image;

   
   function IntBoolExpr_Image (This_IntBoolExpr: IntBoolExpr) return String is
   begin
      case This_IntBoolExpr.Op is
      when LT =>
         return (IntExpr_Image(This_IntBoolExpr.Left.all) &
                   "<" & IntExpr_Image(This_IntBoolExpr.Right.all));
      when GT =>
         return IntExpr_Image(This_IntBoolExpr.Left.all) &
           ">" & IntExpr_Image(This_IntBoolExpr.Right.all);
      when EQ =>
         return IntExpr_Image(This_IntBoolExpr.Left.all) &
           "=" & IntExpr_Image(This_IntBoolExpr.Right.all);
      when NE =>
         return IntExpr_Image(This_IntBoolExpr.Left.all) &
           "/=" & IntExpr_Image(This_IntBoolExpr.Right.all);
      when LE =>
         return IntExpr_Image(This_IntBoolExpr.Left.all) &
           "<=" & IntExpr_Image(This_IntBoolExpr.Right.all);
      when GE =>
         return IntExpr_Image(This_IntBoolExpr.Left.all) &
           ">=" & IntExpr_Image(This_IntBoolExpr.Right.all);
      when ISIN =>
         return IntExpr_Image(This_IntBoolExpr.Left.all) &
           " in " & IntExpr_Image(This_IntBoolExpr.Right.all);
      end case;
   end IntBoolExpr_Image;

   
   function BoolBoolExpr_Image (This_BoolBoolExpr: BoolBoolExpr) return String is
   begin
      case This_BoolBoolExpr.Kind is
         when NoOp =>
            return IntBoolExpr_Image(This_BoolBoolExpr.Compare);
         when NotOp =>
            return "( ! "& BoolBoolExpr_Image(This_BoolBoolExpr.Left.all) & ")" ;
         when AndOp =>
            return "(" & BoolBoolExpr_Image(This_BoolBoolExpr.Left.all) &
              ") & (" & BoolBoolExpr_Image(This_BoolBoolExpr.Right.all) & ")" ;
         when OrOp =>
            return "(" & BoolBoolExpr_Image(This_BoolBoolExpr.Left.all) &
              ") | (" & BoolBoolExpr_Image(This_BoolBoolExpr.Right.all) & ")" ;
      end case;
   end BoolBoolExpr_Image;


   function SimpleIntExpr_Image (This_Simple: SimpleIntExpr) return String is
   begin
      if This_Simple.Image /= null then
         return This_Simple.Image.all;
      end if;
      --  IS THE FOLLOWIMG EVER NEEDED ? BOH
      if This_Simple.Local_Variable /= 0 then
         return This_Simple.Image.all;
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

   
   function IntExpr_Image (This_IntExpr: IntExpr) return String is
   begin
      case This_IntExpr.Op is
      when No_Op =>
         if This_IntExpr.left = null then
            return SimpleIntExpr_Image(This_IntExpr.Simple.all);
         else
            if This_IntExpr.Left.Op = No_Op then
               return (IntExpr_Image(This_IntExpr.Left.all));
            else
               return ("(" & IntExpr_Image(This_IntExpr.Left.all) & ")" );
            end if;
         end if;
      when Plus | Join  =>
         return IntExpr_Image(This_IntExpr.Left.all) &
           "+" & IntExpr_Image(This_IntExpr.Right.all);
      when Times =>
         return IntExpr_Image(This_IntExpr.Left.all) &
           "*" & IntExpr_Image(This_IntExpr.Right.all);
      when Minus =>
         return IntExpr_Image(This_IntExpr.Left.all) &
           "-" & IntExpr_Image(This_IntExpr.Right.all);
      when Div =>
         return IntExpr_Image(This_IntExpr.Left.all) &
           "/" & IntExpr_Image(This_IntExpr.Right.all);
      when Modulus =>
         return IntExpr_Image(This_IntExpr.Left.all) &
           " mod " & IntExpr_Image(This_IntExpr.Right.all);
      when Enclosed =>   --  NEVER NEEDED !!!!
         return  "(" & IntExpr_Image(This_IntExpr.Left.all) & ")" ;
      end case;
   end IntExpr_Image ;

   
   function IntExpr_Kind (This_IntExpr: IntExpr) return Value_Kind is
      Result: Value_Kind;
   begin
      if This_IntExpr.Left = null then
         Result := This_IntExpr.Simple.Kind;
      else
         return (IntExpr_Kind(This_IntExpr.Left.all));
      end if;
      return Result;
   end IntExpr_Kind ;

   
   function umlExpr_Kind (This_umlExpr: umlExpr) return Value_Kind is
   begin
      if This_umlExpr.umlInt /= null then
         return IntExpr_Kind(This_umlExpr.umlInt.all);
      else
         return Bool;
      end if;
   end umlExpr_Kind ;

   
   function umlExpr_Image (This_umlExpr: umlExpr) return String is
   begin
      if This_umlExpr.umlInt /= null then
         return IntExpr_Image(This_umlExpr.umlInt.all);
      else
         return BoolBoolExpr_Image(This_umlExpr.umlBool.all);
      end if;
   end umlExpr_Image;
   
   
   -- called by Image of Action:
   -- in the case of calls, the table is the slice starting at the
   -- second position
   function Exprs_Image (These_IntExprs: IntExpr_Table) return String is
   begin
      if These_IntExprs'Length = 0  then
         return "";
      elsif These_IntExprs'Length = 1 then
         return IntExpr_Image(These_IntExprs(These_IntExprs'First).all);
      else
         return IntExpr_Image(These_IntExprs(These_IntExprs'First).all) & "," &
           Exprs_Image(These_IntExprs(These_IntExprs'First+1 ..
                         These_IntExprs'Last));
      end if;
   end Exprs_Image;

   
   -------------------------
   -- called by Image of Action:
   --  in the case of calls, the table is the slice starting at the
   --   second position
   -------------------------
   function Exprs_Image (These_IntExprs: umlExpr_Table) return String is
   begin
      if These_IntExprs'Length = 0  then
         return "";
      elsif These_IntExprs'Length = 1 then
         return umlExpr_Image(These_IntExprs(These_IntExprs'First).all);
      else
         return umlExpr_Image(These_IntExprs(These_IntExprs'First).all) & "," &
           Exprs_Image(These_IntExprs(These_IntExprs'First+1 ..
                         These_IntExprs'Last));

      end if;
   end Exprs_Image;

   
   function Target_Image (This_Instance: Event_Instance) return String is
   begin
      if This_Instance.The_Target.Literal_Value /= -1 then
         return All_Charts(This_Instance.The_Target.Literal_Value).Name.all;
      end if;
      if This_Instance.The_Target.Local_Variable /= 0 then
         return This_Instance.The_Target.Image.all;
      else
         return This_Instance.The_Target.Image.all;
      end if;
   end Target_Image;

   -- called by  Image (of Action)  called by Image  (of Transition)
   function SignalAct_Image (This_Instance: Event_Instance) return String is
      Target: String :=  Target_Image(This_Instance);
   begin
      if Active_Charts.all'Length >1 or else
        This_Instance.The_Target.Literal_Value >1 then
         if This_Instance.The_Args.all'Length = 0 then
            return Target & "." & This_Instance.The_Event.Name.all;
         else
            return Target & "." & This_Instance.The_Event.Name.all &
              "(" & Exprs_Image(This_Instance.The_Args.all) & ")";
         end if;
      else
         if This_Instance.The_Args.all'Length = 0 then
            return This_Instance.The_Event.Name.all;
         else
            return This_Instance.The_Event.Name.all &
              "(" & Exprs_Image(This_Instance.The_Args.all) & ")";
         end if;
      end if;
   end SignalAct_Image;

   
   -- Notice that Call actions have an additional first parameter
   -- (the caller) which should not be displayed
   function Action_Image (This_Action: Action) return String is
      This_Event: Event_Instance;
      This_Target: SimpleIntExpr_Ref;
      Prefix: String_Ref;
      Prev: String_Ref;
   begin
      if This_Action.Kind = Exitloop then
         return "exit";
         --
      elsif This_Action.Kind = Conditional then
         --
         --   if  <cond>  { list } else {list}
         --
         if This_Action.ElseBody.all'length > 0 then
            Prefix :=
              new String'("if " & BoolBoolExpr_Image(This_Action.IfCond.all)  &
                            " {" &
                            Actions_Image(This_Action.ThenBody.all(1..This_Action.ThenBody.all'length)) &
                            "} else {" &
                            Actions_Image(This_Action.ElseBody.all(1..This_Action.ElseBody.all'length)) &
                            "}" );
         else
            Prefix :=
              new String'("if " & BoolBoolExpr_Image(This_Action.IfCond.all)  &
                            " {" &
                            Actions_Image(This_Action.ThenBody.all(1..This_Action.ThenBody.all'length)) &
                            "}" );
         end if;
         declare
            Result: String := Prefix.all;
         begin
            Free(Prefix);
            return Result;
         end;
         --
      elsif This_Action.Kind = WhileLoop then
         --
         --   while cond {  }
         --
         Prefix :=
           new String'("while " &
                         BoolBoolExpr_Image(This_Action.LoopCond.all)  &
                         " {" &
                         Actions_Image(This_Action.LoopBody.all(1..This_Action.LoopBody.all'length)) &
                         "}" );    --  NOMORE  skip the last Endloop intyroduced by the parser
         --
         declare
            Result: String := Prefix.all;
         begin
            Free(Prefix);
            return Result;
         end;
         --

      elsif This_Action.Kind = ForLoop then
         --
         --   for v in min .. max {  }
         --
         Prefix :=
           new String'("for " & This_Action.For_Var.Name.all  &  " in " &
                         IntExpr_Image(This_Action.For_Min.all) &
                         " .. " &
                         IntExpr_Image(This_Action.For_Max.all) &
                         " { " &
                         Actions_Image(This_Action.LoopBody.all(1..This_Action.LoopBody.all'length)) &
                         "}" );    --  NOMORE  skip the last Endloop intyroduced by the parser
         --
         declare
            Result: String := Prefix.all;
         begin
            Free(Prefix);
            return Result;
         end;
         --
      elsif This_Action.Kind = VarDecl then
         --
         --  tvar: tttype;   var: ttype[];
         --
         Prefix := new String'(This_Action.TVar.Name.all);
         if  This_Action.TVar.Kind  = Number then
            Prev := Prefix;
            Prefix := new String'(Prefix.all & ":int");
            Free (Prev);
         elsif  This_Action.TVar.Kind   = Numvector then
            Prev := Prefix;
            Prefix := new String'(Prefix.all & ":int[]");
            Free (Prev);
         elsif  This_Action.TVar.Kind   = Bool then
            Prev := Prefix;
            Prefix := new String'(Prefix.all & ":bool");
            Free (Prev);
         elsif  This_Action.TVar.Kind   = Boolvector then
            Prev := Prefix;
            Prefix := new String'(Prefix.all & ":bool[]");
            Free (Prev);
         elsif  This_Action.TVar.Kind   = Object then
            Prev := Prefix;
            if This_Action.TVar.TypeInfo   = 0 then
               Prefix := new String'(Prefix.all & ":obj");
            else
               Prefix := new String'(Prefix.all &
                                       ":" & All_Classes(This_Action.TVar.TypeInfo).name.all);
            end if;
            Free (Prev);
         elsif  This_Action.TVar.Kind= objvector then
            Prev := Prefix;
            if This_Action.TVar.TypeInfo = 0 then
               Prefix := new String'(Prefix.all & ":obj[]");
            else
               Prefix := new String'(Prefix.all &
                                       ":" & All_Classes(This_Action.TVar.TypeInfo).name.all &"[]");
            end if;
            Free (Prev);
         end if;  -- Kind = ...
         --
         if  This_Action.TValue /= null then
            Prev := Prefix;
            Prefix := new String'(Prefix.all &
                                    " := " & umlExpr_Image(This_Action.TValue.all));
            Free (Prev);
         end if;
         return Prefix.all;
         --
      elsif This_Action.Kind = Assignment then
         --
         -- Assignment
         --
         return This_Action.Assignment_Left_Image.all &
           ":=" & umlExpr_Image(This_Action.Assignment_Right.all);
         --
      end if;
      --
      -- self.event(args)    -- self can be omitted
      -- OUT.event(Args)
      -- CHART.event(args)   -- CHART can be omitted if only active
      -- var.event(args)
      -- eventvar.event(args)
      --
      if  This_Action.Kind = Signal  or else
        This_Action.Kind =  Call  then
         --
         -- self.event(args)    -- self can be omitted
         -- OUT.event(Args)
         -- CHART.event(args)   -- CHART can be omitted if only active
         -- var.event(args)
         -- eventvar.event(args)
         --
         This_Event := This_Action.Signalled_Event;
         This_Target := This_Event.The_Target;
         if This_Target.Event_Variable /= null then
            Prefix := new String'(This_Target.Event_Variable.Name.all);
         elsif  This_Target.Local_Variable /= 0 then
            Prefix := new String'(This_Target.Image.all);
         elsif This_Target.Special_Token = null and then
           This_Target.Kind = Object and then
           Active_Charts.all'Length > 1 then
            Prefix := new String'(This_Target.Image.all);
         elsif This_Target.Image.all ="self" then
            Prefix := new String'(This_Target.Image.all);  --- >> NEW NEW 
         elsif This_Target.Literal_Value in All_Charts.all'Range  then
            Prefix := new String'(All_Charts(This_Target.Literal_Value).Name.all);
         end if;
         --
         if This_Action.Kind = Signal and then
           Prefix /= null and then This_Event.The_Args.all'Length = 0 then
            Prev := Prefix;
            Prefix := new String'(Prefix.all & "." & This_Event.The_Event.Name.all);
            Free(Prev);
            declare
               result: String := Prefix.all;
            begin
               Free(Prefix);
               return Result;
            end;
            --
         elsif This_Action.Kind = Call and then
           Prefix /= null and then This_Event.The_Args.all'Length = 1 then
            Prev := Prefix;
            Prefix := new String'(Prefix.all & "." & This_Event.The_Event.Name.all);
            Free(Prev);
            declare
               result: String := Prefix.all;
            begin
               Free(Prefix);
               return Result;
            end;
            --
         elsif This_Action.Kind = Signal and then
           Prefix = null and then This_Event.The_Args.all'Length = 0 then
            return This_Event.The_Event.Name.all;
            --
         elsif This_Action.Kind = Call and then
           Prefix = null and then This_Event.The_Args.all'Length = 1 then
            return This_Event.The_Event.Name.all;
            --
         elsif This_Action.Kind = Signal and then
           Prefix /= null and then This_Event.The_Args.all'Length > 0 then
            Prev := Prefix;
            Prefix :=
              new String'(Prefix.all & "." & This_Event.The_Event.Name.all &
                            "(" & Exprs_Image(This_Event.The_Args.all) & ")");
            Free(Prev);
            declare
               result: String := Prefix.all;
            begin
               Free(Prefix);
               return Result;
            end;
            --
         elsif This_Action.Kind = Call and then
           Prefix /= null and then This_Event.The_Args.all'Length > 1 then
            Prev := Prefix;
            Prefix :=
              new String'(Prefix.all & "." & This_Event.The_Event.Name.all &
                            "(" &
                            Exprs_Image(
                            This_Event.The_Args.all(2..This_Event.The_Args.all'Length)) &
                            ")");
            Free(Prev);
            declare
               result: String := Prefix.all;
            begin
               Free(Prefix);
               return Result;
            end;
            --
         else
            return  This_Event.The_Event.Name.all &
              "(" &
              Exprs_Image(This_Event.The_Args.all) &
              ")";
         end if;
      end if;
      --
      if This_Action.Kind = Function_Call then
         --
         --  Function_Call     "  var :=   target.event (args)
         --
         Prefix := new String'(This_Action.Assignment_Left_Image.all &
                                 ":=" & This_Action.Signalled_Event.The_Target.Image.all &
                                 "." & This_Action.Signalled_Event.The_Event.Name.all);
         if This_Action.Signalled_Event.The_Args.all'Length >0 then
            Prev := Prefix;
            Prefix := new String'(Prefix.all &
                                    "(" & Exprs_Image( This_Action.Signalled_Event.The_Args.all) & ")");
            Free(Prev);
         end if;
         declare
            result: String := Prefix.all;
         begin
            Free(Prefix);
            return Result;
         end;
      end if;
      --
      if This_Action.Kind = OpReturn then
         if This_Action.Signalled_Event.The_Args(1) = null then
            return "return";
         else
            return "return(" &
              umlExpr_Image(This_Action.Signalled_Event.The_Args(1).all) &
              ")" ;
         end if;
      end if;
      --
      if Runtime_Errors_Enabled then
         Put_Line(Current_Error,
                  "UMC Runtime_Error! Undefined kind of action in Action_Image");
      end if;
      Runtime_Errors_Count := Runtime_Errors_Count +1;
      Runtime_Error_Msg := new String'("Undefined kind of action in Action_Image");
      return "Runtime_Error";
   end Action_Image;

   
   function Actions_Image (These_Actions: Actions_Table) return String is
   begin
      if These_Actions'Length = 0  then
         return "-";
      elsif These_Actions'Length = 1 then
         return Action_Image(These_Actions(These_Actions'First).all);
      else
         return Action_Image(These_Actions(These_Actions'First).all) & "; " &
           Actions_Image(These_Actions(These_Actions'First+1..These_Actions'Last));
      end if;
   end Actions_Image;

   
   procedure Print_Transition (This: Transition_Ref) is
   begin
      Put (Transition_Image(This));
   end;

   
   function Transition_Label (This: Transition_Ref) return String is
   begin
      if This.Label= null or else This.Label.all'Length=0 then
         declare
            Tmp: String := Integer'Image(This.Num_Key);
         begin
            Tmp(1) := '#';
            return Tmp;
         end;
      else
         return This.Label.all;
      end if;
   end Transition_Label;


   function Dot_Format(Source:String) return String is
      Tmp: String(1..10000);
      Ind: Natural :=0;
   begin
      for I in Source'range loop
         Ind := Ind+1;
         Tmp(Ind) := Source(I);
         if Source(I) = '{' or else
           (Source(I) = '}' and then I < Source'Last and then Source(I+1) /= ';')
           or else Source(I) = ';' then
            Tmp(Ind+1..Ind+2) := "\n";
            Ind := Ind+2;
         end if;
      end loop;
      return Tmp(1..Ind);
   end Dot_Format;

   
   -- called while drawing arcs inside dot diagrams
   function Transition_DotImage (This: Transition_Ref;
                                 Label_Included: Boolean := True) return String is
      Result: String_Ref := new String'("");
      Tmp: Transition_Ref := This;
      i: Natural;
   begin
      Result := Append (Result,  "");
      if Label_Included and then Tmp.Label /= null and then Tmp.Label'Length >0 then
         i := Index(Tmp.Label.all, "_");
         if i = 0  then
            Result := Append (Result, "@" & Tmp.Label.all & "@ ");
         else
            Result := Append (Result, "@" & Tmp.Label.all(1..i-1) & "@ ");
         end if;
      end if;
      -- display the trigger/guard/actions
      if Tmp.Trigger.Params.all'Length = 0  or else
        (Tmp.Trigger.Kind=Operation and then Tmp.Trigger.Params.all'Length=1) then
         Result := Append (Result,Tmp.Trigger.Name.all);
      elsif Tmp.Trigger.Kind=Signal then
         Result := Append (Result,Tmp.Trigger.Name.all & "(" &
                             Args_Image(Tmp.Trigger.Params.all) & ")" );
      elsif Tmp.Trigger.Kind=Operation and then Tmp.Trigger.Params.all'Length>1 then
         Result := Append (Result,Tmp.Trigger.Name.all & "(" &
                             Args_Image(Tmp.Trigger.Params(2..Tmp.Trigger.Params.all'Length)) & ")" );
      end if;
      --
      if Tmp.Guard /= null then
         Result := Append (Result, "\n  [" & BoolBoolExpr_Image(Tmp.Guard.all) & "]\n");
      end if;
      if Tmp.Actions.all'length >0 then
         Result := Append (Result, "/\n");
         Result := Append (Result, Dot_Format(Actions_Image(Tmp.Actions.all)));
      end if;
      --
      declare
         Static: String := Result.all;
      begin
         Free (Result);
         return Static;
      end;
   end Transition_DotImage;

   
   function Transition_Image (This: Transition_Ref;
                              Label_Included: Boolean := True) return String is
      Result: String_Ref := new String'("");
      Owner_Name: String := This.Owner.FullName.all;
      From: Positive := Owner_Name'Length + 2; -- if we want to skip the full name
      To: Positive;
      Tmp: Transition_Ref := This;
   begin
      --  but we want the full name except the first "Top" (or other defined toplike)
      for K in Owner_Name'Range loop
         if Owner_Name(K)= '.' then
            From := K+1;
            exit;
         end if;
      end loop;
      -- display the label
      --
      if Label_Included then
         Result := Append (Result, Transition_Label(This));
         Result := Append (Result,  ": ");
      end if;
      --
      -- display the sources
      --
      if Tmp.Source.all'Length > 1 then
         Result := Append (Result,  " (" );
      end if;
      To := Tmp.Source(1).FullName.all'Length;
      Result := Append (Result, Tmp.Source(1).FullName(From..To));
      for L in 2..Tmp.Source.all'Length loop
         To := Tmp.Source(L).FullName.all'Length;
         Result := Append (Result, "," & Tmp.Source(L).FullName(From..To));
      end loop;
      if Tmp.Source.all'Length > 1 then
         Result := Append (Result,  ")" );
      end if;
      --
      Result := Append (Result,  " -> ");
      --
      -- Display the targets
      --
      if Tmp.Target.all'Length > 1 then
         Result := Append (Result,  "(" );
      end if;
      To := Tmp.Target(1).FullName.all'Length;
      Result := Append (Result, Tmp.Target(1).FullName(From..To));
      for L in 2..Tmp.Target.all'Length loop
         To := Tmp.Target(L).FullName.all'Length;
         Result := Append (Result, "," & Tmp.Target(L).FullName(From..To));
      end loop;
      if Tmp.Target.all'Length > 1 then
         Result := Append (Result,  ")" );
      end if;
      --
      -- display the trigger/guard/actions
      --
      --  Result := Append (Result,  " -( ");
      Result := Append (Result,  " { ");
      if Tmp.Trigger.Params.all'Length = 0  or else
        (Tmp.Trigger.Kind=Operation and then Tmp.Trigger.Params.all'Length=1) then
         Result := Append (Result,Tmp.Trigger.Name.all);
      elsif Tmp.Trigger.Kind=Signal then
         Result := Append (Result,Tmp.Trigger.Name.all & "(" &
                             Args_Image(Tmp.Trigger.Params.all) & ")" );
      elsif Tmp.Trigger.Kind=Operation and then Tmp.Trigger.Params.all'Length>1 then
         Result := Append (Result,Tmp.Trigger.Name.all & "(" &
                             Args_Image(Tmp.Trigger.Params(2..Tmp.Trigger.Params.all'Length)) & ")" );
      end if;
      --  Result := Append (Result, Event_Image(Tmp.Trigger.all));
      if Tmp.Guard /= null then
         Result := Append (Result, " [" & BoolBoolExpr_Image(Tmp.Guard.all) & "]");
      end if;
      Result := Append (Result, " / ");
      Result := Append (Result, Actions_Image(Tmp.Actions.all));
      --  Result := Append (Result,  " )-> ");
      Result := Append (Result,  "} ");
      --
      declare
         Static: String := Result.all;
      begin
         Free (Result);
         return Static;
      end;
   end Transition_Image;

   
   function Transition_ShortImage (This: Transition_Ref;
                                   Label_Included: Boolean := True) return String is
      Result: String_Ref := new String'("");
      Tmp: Transition_Ref := This;
   begin
      -- display the label
      --
      if Label_Included then
         Result := Append (Result, Transition_Label(This));
         Result := Append (Result,  ": ");
      end if;
      --  -- display the trigger/guard
      --
      Result := Append (Result,Tmp.Trigger.Name.all & " / ");

      declare
         Static: String := Result.all;
      begin
         Free (Result);
         return Static;
      end;
   end Transition_ShortImage;


   function Is_Active_Chart(The_Chart: Natural) return Boolean is
   begin
      if The_Chart not in All_Charts.all'Range  then
         return False;
      else
         return All_Charts(The_Chart).Top_State_Num /= 0;
      end if;
   end Is_Active_Chart;


   function Args_Count (The_Signal: Int_Table) return Natural is
      This_Event: Event_Ref;
      I: Natural := The_Signal'First;
   begin
      if The_Signal'Length = 0 then
         return 0;
      end if;
      if The_Signal(I) not in
        All_Events.all'Range then
         return 0;
         --     raise UML_Error;
      end if;
      This_Event := All_Events(The_Signal(I));
      if This_Event.Kind = Signal or This_Event.Kind = Undefined then
         if This_Event.Params /= null then
            return This_Event.Params.all'Length;
         else
            return 0;
         end if;
      else
         return This_Event.Params.all'Length-1;
      end if;
   end Args_Count;

   
   function Literal_Value (Literal:String) return Integer is
   begin
      if Literal(Literal'First) in '0'..'9'  or else
        (Literal'Last >1 and then
         Literal(Literal'First) ='-' and then
         Literal(Literal'First+1) in '0'..'9') then
         return Integer'Value(Literal);
         --
      elsif Literal = "true"  then
         return  UML_Types.IntTrue;
         --
      elsif Literal = "false"  then
         return UML_Types.IntFalse;
         --
      elsif Literal= "null"  then
         return UML_Types.IntNullObject;
         --
      elsif Literal= "undefined"  then
         return UML_Types.IntUndefinedValue;
         --
      elsif Literal = "[]" then
         return UML_Types.IntEmptyStruct;
         --
      else
         --  obj, token
         for I in UML_Types.All_Charts.all'Range loop
            if UML_Types.All_Charts(I).Name.all = Literal then
               return ObjectBase-I;
            end if;
         end loop;
         return Integer'First;
         --  or raise Runtime_Error ?????!!!!
      end if;
      --
   exception
      when others => return Integer'First;
   end Literal_Value;


   function is_Operation (The_Signal: Int_Table) return Boolean is
      This_Event: Event_Ref;
      I: Natural := The_Signal'First;
   begin
      if The_Signal'Length = 0 then
         return False;
      end if;
      if The_Signal(I) not in
        All_Events.all'Range then
         return False;
         --     raise UML_Error;
      end if;
      This_Event := All_Events(The_Signal(I));
      if This_Event.Kind = Signal or This_Event.Kind = Undefined then
         return False;
      else
         return True;
      end if;
   end is_Operation;

   
   function Is_Default_Initial (This: State_Ref) return Boolean is
   begin
      if This.FullName.all'Length > 8 and then
        This.FullName.all(This.FullName.all'Length-7 ..
                            This.FullName.all'Length ) = ".initial" then
         return True;
      else
         return False;
      end if;
   end Is_Default_Initial;

   
   function Is_Final (This: State_Ref) return Boolean is
   begin
      if This.FullName.all'Length > 6 and then
        This.FullName.all(This.FullName.all'Length-5 ..
                            This.FullName.all'Length ) = ".final" then
         return True;
      else
         return False;
      end if;
   end Is_Final;


   function  Normalized_Literal(Source: String) return String is
   begin
      if Source="false" or
        Source="FALSE" or
        Source="False" then
         return "False" ;
      elsif Source="true" or
        Source="TRUE" or
        Source="True" then
         return "True";
      elsif Source="null" or
        Source="NULL" or
        Source="Null" then
         return "null";
      else
         return Source;
      end if;
   end Normalized_Literal;


   function Eval_Literal (Id: String) return Integer is
   begin
      if id="false" or
        id="FALSE" or
        id="False" then
         return IntFalse;
      elsif id="true" or
        id="TRUE" or
        id="True" then
         return IntTrue;
      elsif id="null" or
        id="NULL" or
        id="Null" then
         return 0;
      elsif id(Id'First) in '0'..'9' then
         return Integer'Value(id);
      elsif id'Length >1 and then
        id(Id'First)= '-' and then
        id(Id'First+1) in '0'..'9' then
         return Integer'Value(id);
      else
         -- must be an object name, (object attribues NOT allowed so far)
         for J in All_Charts.all'Range loop
            if All_Charts(J).Name.all=id then
               return J;
            end if;
         end loop;
         if Runtime_Errors_Enabled then
            Put_Line (Current_Error,
                      "Error in ASSERT expression: unexpected object name");
         end if;
         raise UCTL_Error;
      end if;
   end Eval_Literal;


   function Active_Object_Names return String is
      Str: String_Ref := new String'("");
   begin
      for I in Active_Charts.all'range loop
         if I = 1 then
            Str := new String'(All_Charts(Active_Charts(I)).Name.all);
         else
            Str := new String'(Str.all & "," & All_Charts(Active_Charts(I)).Name.all);
         end if;
      end loop;
      declare
         Result: String := Str.all;
      begin
         Free(Str);
         return Result;
      end;
   end Active_Object_Names;

   
   function Entity_Names return String is
      Str: String_Ref := new String'("");
      Prev: String_Ref;
      First:Boolean := True;
   begin
      for I in All_Classes.all'Range loop
         for J in All_Classes(I).ChartVars.all'Range loop
            if First then
               Free(Str);
               Str := new String'(All_Classes(I).ChartVars(J).Name.all);
               First := False;
            else
               Prev := Str;
               Str := new String'(Str.all & "," & All_Classes(I).ChartVars(J).Name.all);
               Free(Prev);
            end if;
         end loop;
      end loop;
      for I in OpReturnBoolC_Event.Num_key+1  .. All_Events.all'Length loop
         if First then
            Free(Str);
            Str := new String'(All_Events(I).Name.all);
            First := False;
         else
            Prev := Str;
            Str := new String'(Str.all & "," & All_Events(I).Name.all);
            Free(Str);
         end if;
      end loop;
      declare
         Result: String := Str.all;
      begin
         Free(Str);
         return Result;
      end;
   end Entity_Names;

   
   function Has_Random_Queues(Index:Natural) return Boolean is
      oldstyle: Boolean :=False;
   begin
      if All_Charts(Index).ChartVars /= null  and then
        All_Charts(Index).ChartVars.all'Length >0 and then
        All_Charts(Index).ChartVars(1).Name.all = "RANDOMQUEUE" then
         oldstyle := True;
      elsif All_Charts(Index).ChartVars /= null  and then
        All_Charts(Index).ChartVars.all'Length >1 and then
        All_Charts(Index).ChartVars(2).Name.all = "RANDOMQUEUE" then
         oldstyle := True;
      else
         oldstyle := False;
      end if;
      return All_Charts(Index).IsRandom or oldstyle;
   end Has_Random_Queues;

begin
   null;
   --  NullStruct.Simple.Literal_Value := StructBase - Vectors_DB.NickNum(Empty_Int_Table);

end UML_Types;
