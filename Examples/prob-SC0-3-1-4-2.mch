//---------------------------------------------
// SCENARIO 0
//  Config: max_receive_timer = 3,
//          max_send_timer    = 1,
//          max_conect_timer  = 4,
//          SAI.N             = 2,
//  Initiator RBC sends PRE_Announcement, and waits ACK.
//    in case of failure (Disconnection) retries a second time
//  Called RBC executes a loop waiting for PRE_Announcement and sending Ack.
//  In case of failures the loop is restarted
//
//  NO DEADLOKS
//  No loss of event
//   (states generated= 18634)
//---------------------------------------------


MACHINE HANDOVER

SETS

  CSL_SIGNALS =
   {SAI_CONNECT_confirm,    // only ISAI
    SAI_CONNECT_indication, // only CSAI
    SAI_DISCONNECT_indication,
    SAI_Error_report,
    SAI_DATA_indication,   // +arg
    //
    RBC_User_Data_request, // +arg
    icsl_tick,
    ccsl_tick};

  SAI_SIGNALS =
   {SAI_CONNECT_request,   // only ISAI
    SAI_DISCONNECT_request,
    SAI_DATA_request, // +arg  
    Sa_CONN_confirm,       // only ISAI
    Sa_CONN_request,       // only CSAI
    Sa_DATA,    // +arg
    Sa_CONN_terminate,
    isai_tick,
    csai_tick};

  RBC_SIGNALS =
   {RBC_User_Connect_indication,
//    RBC_User_Error_indication,   NO MORE in v17
    RBC_User_Disconnect_indication,
    RBC_User_Data_indication,
    irbc_tick,
    crbc_tick};
    
 Timer_SIGNALS =
   {ok_icsl, 
    ok_isai, 
    ok_irbc, 
    ok_ccsl,
    ok_csai, 
    ok_crbc};
  
  CSL_STATES = 
   {NOCOMMS, 
   COMMS};
    
  SAI_STATES =
  {Disconnected,
   Connecting,
   Connected
  };
  
  RBC_STATES =
  {idle, disconnected, connected};
  
  TIMER_STATES =
  {s0}
  
DEFINITIONS
  SET_PREF_MAXINT == 30;
  SET_PREF_MININT == 0;
  
// ASSERT_CTL == "EX[R1_Timer_irbc_tick]"

CONSTANTS
  LifeSign,
  // I_CSL
  icsl_max_receive_timer,
  icsl_max_send_timer,
  icsl_max_connect_timer,
  // I-SAI
  isai_N,
  // C_CSL
  ccsl_max_receive_timer,
  ccsl_max_send_timer,
  // C-SAI
  csai_N
  
PROPERTIES
  LifeSign = 1 &
  // I_CSL
  icsl_max_receive_timer = 3 &
  icsl_max_send_timer = 1 &
  icsl_max_connect_timer = 4 &
  // I-SAI
  isai_N = 2 &
  // C_CSL
  ccsl_max_receive_timer = 3 &
  ccsl_max_send_timer = 1 &
  // C-SAI
  csai_N = 2
  
VARIABLES
  // I_CSL
  icsl_buff,
  icsl_databuff,
  ICSL_STATUS,
  icsl_receive_timer,
  icsl_send_timer,
  icsl_connect_timer,
  // I_SAI
  isai_buff,
  isai_databuff,
  ISAI_STATUS,
  isai_sent_but_not_yet_received,
  // C_CSL
  ccsl_buff,
  ccsl_databuff,
  CCSL_STATUS,
  ccsl_receive_timer,
  ccsl_send_timer,
  // C_SAI
  csai_buff,
  csai_databuff,
  CSAI_STATUS,
  csai_sent_but_not_yet_received,
  // IRBC
  irbc_buff,
  irbc_databuff,
  IRBC_STATUS,
  // CRBC
  crbc_buff,
  crbc_databuff,
  CRBC_STATUS,
  // Timer
  timer_buff,
  TIMER_STATUS,
  timer_starting

INVARIANT
  // I_CSL
  icsl_buff: seq(CSL_SIGNALS) &
  icsl_databuff: seq(0..5) &
  ICSL_STATUS: CSL_STATES &
  icsl_receive_timer: 0..10 &
  icsl_send_timer: 0..10 &
  icsl_connect_timer: 0..20 &
  // I_SAI
  isai_buff: seq(SAI_SIGNALS) &
  isai_databuff: seq(0..5) &
  ISAI_STATUS: SAI_STATES &
  isai_sent_but_not_yet_received:  0..isai_N   & 
  // C_CSL
  ccsl_buff: seq(CSL_SIGNALS) &
  ccsl_databuff: seq(0..5) &
  CCSL_STATUS: CSL_STATES &
  ccsl_receive_timer: 0..10 &
  ccsl_send_timer: 0..10 &
  // C_SAI
  csai_buff: seq(SAI_SIGNALS) &
  csai_databuff: seq(0..5) &
  CSAI_STATUS: SAI_STATES &
  csai_sent_but_not_yet_received:  0..csai_N   & 
  // IRBC
  irbc_buff: seq(RBC_SIGNALS) &
  irbc_databuff: seq(0..5) &
  IRBC_STATUS: RBC_STATES &
  // CRBC
  crbc_buff: seq(RBC_SIGNALS) &
  crbc_databuff: seq(0..5) &
  CRBC_STATUS: RBC_STATES &
  // Timer
  timer_buff : seq(Timer_SIGNALS) &
  TIMER_STATUS: TIMER_STATES &
  timer_starting : BOOL
  
INITIALISATION
  // I_CSL
  icsl_buff := [];
  icsl_databuff := [];
  ICSL_STATUS := NOCOMMS;
  icsl_receive_timer := 0;
  icsl_send_timer := 0;
  icsl_connect_timer := icsl_max_connect_timer;
  // I_SAI
  isai_buff := [];
  isai_databuff := [];
  ISAI_STATUS := Disconnected;
  isai_sent_but_not_yet_received :=0;
  // C_CSL
  ccsl_buff := [];
  ccsl_databuff := [];
  CCSL_STATUS := NOCOMMS;
  ccsl_receive_timer := 0;
  ccsl_send_timer := 0;
  // C_SAI
  csai_buff := [];
  csai_databuff := [];
  CSAI_STATUS := Disconnected;
  csai_sent_but_not_yet_received :=0;
  // IRBC
  irbc_buff := [];
  irbc_databuff := [];
  IRBC_STATUS := idle;
  // RBC
  crbc_buff := [];
  crbc_databuff := [];
  CRBC_STATUS := idle;
  // Timer
  timer_buff := [];
  TIMER_STATUS := s0;
  timer_starting := TRUE
  

OPERATIONS

/////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////
//  ICSL
/////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////

// -------------------------------------------------------------
// when disconnetted ignore NRBC_MSGs and SAI_Notifications,
// -------------------------------------------------------------

R1_ICSL_discard_userdata =
PRE
  ICSL_STATUS = NOCOMMS &
  icsl_buff /= [] &
  first(icsl_buff) =  RBC_User_Data_request 
THEN
  ICSL_STATUS := NOCOMMS;
  //
  icsl_buff := tail(icsl_buff);
  icsl_databuff := tail(icsl_databuff)
END;

R2_ICSL_discard_disconnectindication =
PRE
  ICSL_STATUS = NOCOMMS &
  icsl_buff /= [] &
  first(icsl_buff) =  SAI_DISCONNECT_indication 
THEN
  ICSL_STATUS := NOCOMMS;
  //
  icsl_buff := tail(icsl_buff)
END;

R3_ICSL_discard_errorreport =
PRE
  ICSL_STATUS = NOCOMMS &
  icsl_buff /= [] &
  first(icsl_buff) =  SAI_Error_report 
THEN
  ICSL_STATUS := NOCOMMS;
  //
  icsl_buff := tail(icsl_buff)
END;

R4_ICSL_discard_dataindication =
PRE
  ICSL_STATUS = NOCOMMS &
  icsl_buff /= [] &
  first(icsl_buff) =  SAI_DATA_indication 
THEN
  ICSL_STATUS := NOCOMMS;
  //
  icsl_buff := tail(icsl_buff);
  icsl_databuff := tail(icsl_databuff)
END;

// -------------------------------------------------------------
// establishing connections 
// -------------------------------------------------------------

// when disconnected issue new SAI connection request
//
R6_ICSL_Timer_okicsl_ISAI_connectrequest =
PRE
  ICSL_STATUS = NOCOMMS & 
  icsl_connect_timer = icsl_max_connect_timer &
  icsl_buff /= [] &
  first(icsl_buff) =  icsl_tick 
THEN
  ICSL_STATUS := NOCOMMS; 
  //
  icsl_connect_timer := 0;
  timer_buff := timer_buff <- ok_icsl;
  isai_buff := isai_buff <- SAI_CONNECT_request;
  //
  icsl_buff := tail(icsl_buff)
END;

// when connecting handle clock cycles 
//   and ignore all other events but connection confirmations
R7_ICSL_Timer_okicsl =
PRE
  ICSL_STATUS = NOCOMMS & 
  icsl_buff /= [] &
  icsl_connect_timer < icsl_max_connect_timer &
  first(icsl_buff) =  icsl_tick &
  icsl_connect_timer < icsl_max_connect_timer
THEN
  ICSL_STATUS := NOCOMMS;
  //
  timer_buff := timer_buff <- ok_icsl;
  icsl_connect_timer := icsl_connect_timer + 1;
  //
  icsl_buff := tail(icsl_buff)
END;


// when connecting handle connection confirmation
//
R8_ICSL_IRBC_rbcuserconnectindication =
PRE
  ICSL_STATUS = NOCOMMS &
  icsl_buff /= [] &
  first(icsl_buff) =  SAI_CONNECT_confirm 
THEN
  ICSL_STATUS :=COMMS;
  //
  icsl_connect_timer := icsl_max_connect_timer;
  icsl_receive_timer := 0;
  icsl_send_timer := 0; 
  irbc_buff := irbc_buff <- RBC_User_Connect_indication;
  //
  icsl_buff := tail(icsl_buff)
END;


//--------------------------------
//-- handling active connections 
//--------------------------------
  
// -- when connected forward User NRBC_MSGs     
// --
R10_ICSL_ISAI_saidatarequest =
PRE
  ICSL_STATUS = COMMS &
  icsl_buff /= [] &
  first(icsl_buff) =  RBC_User_Data_request 
THEN
  ICSL_STATUS :=COMMS;
  //
  icsl_send_timer := 0;
  isai_buff := isai_buff <- SAI_DATA_request;
  isai_databuff := isai_databuff <- first(icsl_databuff);
  //
  icsl_buff := tail(icsl_buff);
  icsl_databuff := tail(icsl_databuff)
END;

// -- when connected, in no send or receive timers are expired, adjust timers
//
R11_ICSL_Timer_okicsl =
PRE
  ICSL_STATUS =COMMS &
  icsl_buff /= [] &
  first(icsl_buff) =  icsl_tick &
  icsl_receive_timer < icsl_max_receive_timer &
  icsl_send_timer < icsl_max_send_timer 
THEN
  ICSL_STATUS :=COMMS;
  //
  timer_buff := timer_buff <- ok_icsl;
  icsl_send_timer := icsl_send_timer + 1;
  icsl_receive_timer := icsl_receive_timer+1;
  //
  icsl_buff := tail(icsl_buff)
END;

//  when connected, if the receive timer is not expired but send timer is expired, 
//   send lifesign
R12_ICSL_Timer_okicsl_ISAI_saidatarequest =
PRE
  ICSL_STATUS =COMMS &
  icsl_buff /= [] &
  first(icsl_buff) =  icsl_tick &
  icsl_receive_timer < icsl_max_receive_timer &
  icsl_send_timer = icsl_max_send_timer 
THEN
  ICSL_STATUS :=COMMS;
  //
  timer_buff := timer_buff <- ok_icsl;
  icsl_send_timer := 0;
  icsl_receive_timer := icsl_receive_timer+1;
  isai_buff := isai_buff <- SAI_DATA_request;
  isai_databuff := isai_databuff <- LifeSign;
  //
  icsl_buff := tail(icsl_buff)
END;


// -- when connected, is msg received from sai,forward to user and 
// -- reset receive timer;
// 
R13_ICSL_IRBC_rbcuserdataindication =
PRE
  ICSL_STATUS =COMMS &
  icsl_buff /= [] &
  first(icsl_buff) =  SAI_DATA_indication &
  first(icsl_databuff) /=  LifeSign
THEN
  ICSL_STATUS :=COMMS;
  //
  icsl_receive_timer := 0;
  irbc_buff := irbc_buff <- RBC_User_Data_indication;
  irbc_databuff := irbc_databuff <- first(icsl_databuff);
  //
  icsl_buff := tail(icsl_buff);
  icsl_databuff := tail(icsl_databuff)
END;
  
  
R14_ICSL_handle_lifesign =
PRE
  ICSL_STATUS =COMMS &
  icsl_buff /= [] &
  first(icsl_buff) =  SAI_DATA_indication &
  first(icsl_databuff) =  LifeSign 
THEN
  ICSL_STATUS :=COMMS;
  //
  icsl_receive_timer := 0;
  //
  icsl_buff := tail(icsl_buff);
  icsl_databuff := tail(icsl_databuff)
END;

// when connected, forward SAI msg notifications
//
R15_ICSL_IRBC_rbcusererrorindication =
PRE
  ICSL_STATUS =COMMS &
  icsl_buff /= [] &
  first(icsl_buff) =  SAI_Error_report
THEN
  ICSL_STATUS :=COMMS;
  //
  icsl_buff := tail(icsl_buff)
END;

// when connected, disconnect and forward notification when notified by SAI
//
R16_ICSL_IRBC_rbcuserdisconnectindication =
PRE
  ICSL_STATUS =COMMS &
  icsl_buff /= [] &
  first(icsl_buff) =  SAI_DISCONNECT_indication
THEN
  ICSL_STATUS := NOCOMMS;
  //
  irbc_buff := irbc_buff <- RBC_User_Disconnect_indication;
  icsl_receive_timer := 0;
  icsl_send_timer := 0;
  //
  icsl_buff := tail(icsl_buff)
END;

// when connected, if receive timer expires,disconnect, notify User and 
// request SAI termination
//
R17_ICSL_Timer_okicsl_IRBC_rbcuserdisconnectindication_ISAI_saidisconnectrequest =
PRE
  ICSL_STATUS =COMMS &
  icsl_buff /= [] &
  first(icsl_buff) =  icsl_tick &
  icsl_receive_timer = icsl_max_receive_timer
THEN
  ICSL_STATUS := NOCOMMS;
  //
  timer_buff := timer_buff <- ok_icsl;
  irbc_buff := irbc_buff <- RBC_User_Disconnect_indication;
  isai_buff := isai_buff <- SAI_DISCONNECT_request;
  icsl_receive_timer := 0;
  icsl_send_timer := 0;
  //
  icsl_buff := tail(icsl_buff)
END;

/////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////
//  ISAI
/////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////

// --------------------------------
// -- handling disconnections 
// --------------------------------


// -- when disconnected handle clock cycles 
//
R1_ISAI_Timer_okisai =
PRE
  ISAI_STATUS = Disconnected & 
  isai_buff /= [] &
  first(isai_buff) =  isai_tick 
THEN
  ISAI_STATUS := Disconnected; 
  //
  timer_buff := timer_buff <- ok_isai;
  //
  isai_buff := tail(isai_buff)
END;

// when disconnected ignore Sa_CONN_terminate
//
R2_ISAI_discard_saconnterminate =
PRE
  ISAI_STATUS = Disconnected &
  isai_buff /= [] &
  first(isai_buff) =  Sa_CONN_terminate 
THEN
  ISAI_STATUS := Disconnected;
  //
  isai_buff := tail(isai_buff)         // discard signal
END;

// when disconnected ignore Sa_DATA(nrbc_msg)
//
R3_ISAI_discard_sadata =
PRE
  ISAI_STATUS = Disconnected &
  isai_buff /= [] &
  first(isai_buff) =  Sa_DATA 
THEN
  ISAI_STATUS := Disconnected;
  //
  isai_buff := tail(isai_buff);         // discard signal
  isai_databuff := tail(isai_databuff) // discard arg
END;

// when disconnected ignore SAI_DATA_request(nrbc_msg)
//
R4_ISAI_discard_saidatarequest =
PRE
  ISAI_STATUS = Disconnected &
  isai_buff /= [] &
  first(isai_buff) =  SAI_DATA_request 
THEN
  ISAI_STATUS := Disconnected;
  //
  isai_buff := tail(isai_buff);         // discard signal
  isai_databuff := tail(isai_databuff) // discard arg
END;

// when disconnected ignore SAI_DISCONNECT_request
//
R5_ISAI_discard_saidisconnectrequest =
PRE
  ISAI_STATUS = Disconnected &
  isai_buff /= [] &
  first(isai_buff) =  SAI_DISCONNECT_request 
THEN
  ISAI_STATUS := Disconnected;
  //
  isai_buff := tail(isai_buff)        // discard signal
END;

// when disconnected ignore Sa_CONN_confirm
//
R6_ISAI_discard_saconnconfirm =
PRE
  ISAI_STATUS = Disconnected &
  isai_buff /= [] &
  first(isai_buff) =  Sa_CONN_confirm 
THEN
  ISAI_STATUS := Disconnected;
  //
  isai_buff := tail(isai_buff)         // discard signal
END;

//  -- when disconnected accept connection requests from RBC
//
R7_ISAI_CSAI_sacconnrequest =
PRE
  ISAI_STATUS = Disconnected &
  isai_buff /= [] &
  first(isai_buff) =  SAI_CONNECT_request 
THEN
  ISAI_STATUS := Connecting;
  //
  isai_sent_but_not_yet_received :=0;
  csai_buff := csai_buff <- Sa_CONN_request;
  //
  isai_buff := tail(isai_buff)         // discard signal
END;

// --------------------------------
// -- handling preconnections 
// --------------------------------

// -- when connecting handle clock cycles
// --  and ignore all events but connection confirmations 
// -- Notice:  also repeated RBC_Conect_request are ignored.

R8_ISAI_Timer_okisai =
PRE
  ISAI_STATUS = Connecting & 
  isai_buff /= [] &
  first(isai_buff) =  isai_tick 
THEN
  ISAI_STATUS := Connecting; 
  //
  timer_buff := timer_buff <- ok_isai;
  //
  isai_buff := tail(isai_buff)
END;


// when connecting ignore Sa_CONN_terminate
//
R9_ISAI_discard_saconnterminate =
PRE
  ISAI_STATUS = Connecting &
  isai_buff /= [] &
  first(isai_buff) =  Sa_CONN_terminate 
THEN
  ISAI_STATUS := Connecting;
  //
  isai_buff := tail(isai_buff)         // discard signal
END;

// when connecting ignore Sa_DATA(nrbc_msg)
//
R10_ISAI_discard_sadata =
PRE
  ISAI_STATUS = Connecting &
  isai_buff /= [] &
  first(isai_buff) =  Sa_DATA 
THEN
  ISAI_STATUS := Connecting;
  //
  isai_buff := tail(isai_buff);         // discard signal
  isai_databuff := tail(isai_databuff) // discard arg
END;

// when connecting ignore SAI_DATA_request(nrbc_msg)
//
R11_ISAI_discard_SAI_DATA_request =
PRE
  ISAI_STATUS = Connecting &
  isai_buff /= [] &
  first(isai_buff) =  SAI_DATA_request 
THEN
  ISAI_STATUS := Connecting;
  //
  isai_buff := tail(isai_buff);         // discard signal
  isai_databuff := tail(isai_databuff) // discard arg
END;

// when connecting ignore SAI_DISCONNECT_request
//
R12_ISAI_discard_saidisconnectrequest =
PRE
  ISAI_STATUS = Connecting &
  isai_buff /= [] &
  first(isai_buff) =  SAI_DISCONNECT_request 
THEN
  ISAI_STATUS := Connecting;
  //
  isai_buff := tail(isai_buff)         // discard signal
END;

// -- when requested by, try connection again.
R13_ISAI_CSAI_saconnrequest =
PRE
  ISAI_STATUS = Connecting &
  isai_buff /= [] &
  first(isai_buff) =  SAI_CONNECT_request 
THEN
  ISAI_STATUS := Connecting;
  //
  isai_sent_but_not_yet_received :=0;
  csai_buff := csai_buff <- Sa_CONN_request;
  //
  isai_buff := tail(isai_buff)         // discard signal
END;

// -- connection confirmation arrived from neighbor SAI
R15_ISAI_ICSL_saiconnectconfirm =
PRE
  ISAI_STATUS = Connecting &
  isai_buff /= [] &
  first(isai_buff) =  Sa_CONN_confirm 
THEN
  ISAI_STATUS := Connected;
  //
  icsl_buff := icsl_buff <- SAI_CONNECT_confirm;
  //
  isai_buff := tail(isai_buff)         // discard signal
END;

// -- ER loss of connection confirmation message
R14_ISAI_ndloss_saconnconfirm =
PRE
  ISAI_STATUS = Connecting &
  isai_buff /= [] &
  first(isai_buff) =  Sa_CONN_confirm 
THEN
  ISAI_STATUS := Connecting;
  //
  isai_buff := tail(isai_buff)         // discard signal
END;


// --------------------------------
// -- handling connections 
// --------------------------------

// -- when connected handle clock cycles 
R16_ISAI_Timer_okisai =
PRE
  ISAI_STATUS = Connected & 
  isai_buff /= [] &
  first(isai_buff) =  isai_tick 
THEN
  ISAI_STATUS := Connected; 
  //
  timer_buff := timer_buff <- ok_isai;
  //
  isai_buff := tail(isai_buff)
END;

// -- when already connected further repeated CSL Conect_request are ignored.
R17_ISAI_discard_saiconnectrequest=
PRE
  ISAI_STATUS = Connected & 
  isai_buff /= [] &
  first(isai_buff) =  SAI_CONNECT_request 
THEN
  ISAI_STATUS := Connected; 
  //
  isai_buff := tail(isai_buff)
END;

// ---------------------------------
// -- sending CSL messages NSAI
// --------------------------------

// -- send NRBC msg to CSAI
R18_ISAI_CSAI_sadata =
PRE
  ISAI_STATUS = Connected & 
  isai_buff /= [] &
  first(isai_buff) =  SAI_DATA_request 
THEN
  ISAI_STATUS := Connected; 
  //
  csai_buff := csai_buff <- Sa_DATA;
  csai_databuff := csai_databuff <- first(isai_databuff);
  //
  isai_buff := tail(isai_buff);
  isai_databuff := tail(isai_databuff)
END;

// ---------------------------------
// -- receiving or losing messages
// --------------------------------
  

// -- deliver NRBC Msg to APP
// --  with no other notification if not invalid and not out of sequence
R21_ISAI_ICSL_dataindication =
PRE
  ISAI_STATUS = Connected & 
  isai_buff /= [] &
  first(isai_buff) =  Sa_DATA &
  isai_sent_but_not_yet_received = 0
THEN
  ISAI_STATUS := Connected; 
  //
  icsl_buff := icsl_buff <- SAI_DATA_indication;
  icsl_databuff := icsl_databuff <- first(isai_databuff);
  //
  isai_buff := tail(isai_buff);
  isai_databuff := tail(isai_databuff)
END;

// -- deliver NRBC Msg if not invalid but slightly out of sequence, 
// --  and notify the error
R22_ISAI_ICSL_saidataindication_ICSL_saierrorreport =
PRE
  ISAI_STATUS = Connected & 
  isai_buff /= [] &
  first(isai_buff) =  Sa_DATA &
  isai_sent_but_not_yet_received < isai_N &
  isai_sent_but_not_yet_received > 0 
THEN
  ISAI_STATUS := Connected; 
  //
  icsl_buff := icsl_buff <- SAI_DATA_indication;
  icsl_databuff := icsl_databuff <- first(isai_databuff);
  icsl_buff := icsl_buff <- SAI_Error_report;
  isai_sent_but_not_yet_received :=0;
  //
  isai_buff := tail(isai_buff);
  isai_databuff := tail(isai_databuff)
END;


// -- not yet evident msg delay or loss (abstraction of ER layer)
// --  NOTICE: no need to increment counet to a value greater than N
R19_ISAI_nsloss_sadata =
PRE
  ISAI_STATUS = Connected & 
  isai_buff /= [] &
  first(isai_buff) =  Sa_DATA &
  isai_sent_but_not_yet_received < isai_N
THEN
  ISAI_STATUS := Connected; 
  //
  isai_sent_but_not_yet_received := isai_sent_but_not_yet_received + 1;
  isai_buff := tail(isai_buff);
  isai_databuff := tail(isai_databuff)
END;

// -- do not deliver to APP invalid msgs (too late but in correct sequence)
R20_ISAI_ndinvalis_sadata_ICSL_saierrorreport =
PRE
  ISAI_STATUS = Connected & 
  isai_buff /= [] &
  first(isai_buff) =  Sa_DATA &
  isai_sent_but_not_yet_received = 0
THEN
  ISAI_STATUS := Connected; 
  //
  icsl_buff := icsl_buff <- SAI_Error_report;
  //
  isai_buff := tail(isai_buff);
  isai_databuff := tail(isai_databuff)
END;

// -- do not deliver NRBC Msg if invalid, when slightly out of sequence, 
// --  but notify both error situations
R23_ISAI_ICSL_saierrorreport =
PRE
  ISAI_STATUS = Connected & 
  isai_buff /= [] &
  first(isai_buff) =  Sa_DATA &
  isai_sent_but_not_yet_received < isai_N &
  isai_sent_but_not_yet_received > 0 
THEN
  ISAI_STATUS := Connected; 
  //
  icsl_buff := icsl_buff <- SAI_Error_report;
  isai_sent_but_not_yet_received :=0;
  //
  isai_buff := tail(isai_buff);
  isai_databuff := tail(isai_databuff)
END;

// ---------------------------------
// -- handling connection failures
// --------------------------------

// -- notify disconnection if the last msgs shows an excessive msg delay or loss
R24_ISAI_CSAI_saconnterminate_ICSL_saidisconnectindication =
PRE
  ISAI_STATUS = Connected & 
  isai_buff /= [] &
  first(isai_buff) =  Sa_DATA &
  isai_sent_but_not_yet_received = isai_N
THEN
  ISAI_STATUS := Disconnected; 
  //
  csai_buff := csai_buff <- Sa_CONN_terminate;
  icsl_buff := icsl_buff <- SAI_DISCONNECT_indication;
  isai_sent_but_not_yet_received :=0;
  //
  isai_buff := tail(isai_buff);
  isai_databuff := tail(isai_databuff)
END;

// ---------------------------------
// -- handling disconnection requests
// --------------------------------

// -- when connected, become disconnected upon termination request from RBC
R25_ISAI_CSAI_sconnterminate =
PRE
  ISAI_STATUS = Connected & 
  isai_buff /= [] &
  first(isai_buff) =  SAI_DISCONNECT_request 
THEN
  ISAI_STATUS := Disconnected; 
  //
  csai_buff := csai_buff <- Sa_CONN_terminate;
  isai_sent_but_not_yet_received :=0;
  //
  isai_buff := tail(isai_buff)
END;


// -- when connected, become disconnected upon termination request from NSAI
R26_ISAI_ICSL_saidisconnectindication =
PRE
  ISAI_STATUS = Connected & 
  isai_buff /= [] &
  first(isai_buff) =  Sa_CONN_terminate 
THEN
  ISAI_STATUS := Disconnected; 
  //
  icsl_buff := icsl_buff <- SAI_DISCONNECT_indication;
  isai_sent_but_not_yet_received :=0;
  //
  isai_buff := tail(isai_buff)
END;

// -- But disconnection indication from ER can get lost!!
R27_ISAI_ndloss_saconnterminate =
PRE
  ISAI_STATUS = Connected & 
  isai_buff /= [] &
  first(isai_buff) =  Sa_CONN_terminate 
THEN
  ISAI_STATUS := Connected; 
  //
  isai_buff := tail(isai_buff)
END;


/////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////
//  CSAI
/////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////

// --------------------------------
// -- handling disconnections 
// --------------------------------

// -- when disconnected handle clock cycles 
//
R1_CSAI_Timer_okcsai =
PRE
  CSAI_STATUS = Disconnected & 
  csai_buff /= [] &
  first(csai_buff) =  csai_tick 
THEN
  CSAI_STATUS := Disconnected; 
  //
  timer_buff := timer_buff <- ok_csai;
  //
  csai_buff := tail(csai_buff)
END;

// when disconnected ignore Sa_CONN_terminate
//
R2_CSAI_discard_saconnterminate =
PRE
  CSAI_STATUS = Disconnected &
  csai_buff /= [] &
  first(csai_buff) =  Sa_CONN_terminate 
THEN
  CSAI_STATUS := Disconnected;
  //
  csai_buff := tail(csai_buff)         // discard signal
END;

// when disconnected ignore Sa_DATA(nrbc_msg)
//
R3_CSAI_discard_sadata =
PRE
  CSAI_STATUS = Disconnected &
  csai_buff /= [] &
  first(csai_buff) =  Sa_DATA 
THEN
  CSAI_STATUS := Disconnected;
  //
  csai_buff := tail(csai_buff);         // discard signal
  csai_databuff := tail(csai_databuff) // discard arg
END;

// when disconnected ignore SAI_DATA_request(nrbc_msg)
//
R4_CSAI_discard_saidatarequest =
PRE
  CSAI_STATUS = Disconnected &
  csai_buff /= [] &
  first(csai_buff) =  SAI_DATA_request 
THEN
  CSAI_STATUS := Disconnected;
  //
  csai_buff := tail(csai_buff);         // discard signal
  csai_databuff := tail(csai_databuff) // discard arg
END;

// when disconnected ignore SAI_DISCONNECT_request
//
R5_CSAI_discard_saidisconnectrequest =
PRE
  CSAI_STATUS = Disconnected &
  csai_buff /= [] &
  first(csai_buff) =  SAI_DISCONNECT_request 
THEN
  CSAI_STATUS := Disconnected;
  //
  csai_buff := tail(csai_buff)        // discard signal
END;

// -- wait NSAI connection requests to establish connection
R6_CSAI_ISAI_saconnconfirm_CCSL_saiconnectindication =
PRE
  CSAI_STATUS = Disconnected &
  csai_buff /= [] &
  first(csai_buff) =  Sa_CONN_request 
THEN
  CSAI_STATUS := Connected;
  //
  csai_sent_but_not_yet_received :=0;
  isai_buff := isai_buff <- Sa_CONN_confirm;
  ccsl_buff := ccsl_buff <- SAI_CONNECT_indication;
  //
  csai_buff := tail(csai_buff)         // discard signal
END;

// --  model also the ER level loss of connection request
R7_CSAI_discard_saconnrequest =
PRE
  CSAI_STATUS = Disconnected &
  csai_buff /= [] &
  first(csai_buff) =  Sa_CONN_request 
THEN
  CSAI_STATUS := Disconnected;
  //
  csai_buff := tail(csai_buff)         // discard signal
END;

// --------------------------------
// -- handling connections 
// --------------------------------

// -- when connected handle clock cycles 
R8_CSAI_Timer_okcsai =
PRE
  CSAI_STATUS = Connected & 
  csai_buff /= [] &
  first(csai_buff) =  csai_tick 
THEN
  CSAI_STATUS := Connected; 
  //
  timer_buff := timer_buff <- ok_csai;
  //
  csai_buff := tail(csai_buff)
END;

// -- connection reset by NSAI initiator
// -- notice: this should never happen if new connection request must be preceded 
// --          by previous connection termination!!
R9_CSAI_ISAI_saconnconfirm_CCSL_saiconnectindication =
PRE
  CSAI_STATUS = Connected &
  csai_buff /= [] &
  first(csai_buff) =  Sa_CONN_request 
THEN
  CSAI_STATUS := Connected;
  //
  csai_sent_but_not_yet_received :=0;
  isai_buff := isai_buff <- Sa_CONN_confirm;
  ccsl_buff := ccsl_buff <- SAI_CONNECT_indication;
  //
  csai_buff := tail(csai_buff)         // discard signal
END;

// -- ER loss of sa_conn_request
R10_CSAI_ndloss_saconnrequest =
PRE
  CSAI_STATUS = Connected &
  csai_buff /= [] &
  first(csai_buff) =  Sa_CONN_request 
THEN
  CSAI_STATUS := Connected;
  //
  csai_buff := tail(csai_buff)         // discard signal
END;  


// ---------------------------------
// -- sending CSL messages NSAI
// --------------------------------

// -- send NRBC msg to NSAI
R11_CSAI_ISAI_sadata =
PRE
  CSAI_STATUS = Connected & 
  csai_buff /= [] &
  first(csai_buff) =  SAI_DATA_request 
THEN
  CSAI_STATUS := Connected; 
  //
  isai_buff := isai_buff <- Sa_DATA;
  isai_databuff := isai_databuff <- first(csai_databuff);
  //
  csai_buff := tail(csai_buff);
  csai_databuff := tail(csai_databuff)
END;

// ---------------------------------
// -- receiving or losing messages
// --------------------------------
  
// -- not yet evident msg delay or loss (abstraction of ER layer)
// --  NOTICE: no need to increment counet to a value greater than N
R12_CSAI_nsloss_sadata =
PRE
  CSAI_STATUS = Connected & 
  csai_buff /= [] &
  first(csai_buff) =  Sa_DATA &
  csai_sent_but_not_yet_received < csai_N
THEN
  CSAI_STATUS := Connected; 
  //
  csai_sent_but_not_yet_received := csai_sent_but_not_yet_received + 1;
  //
  csai_buff := tail(csai_buff);
  csai_databuff := tail(csai_databuff)
END;

// -- do not deliver to APP invalid msgs (too late but in correct sequence)
R13_CSAI_ndinvalis_sadata_CCSL_saierrorreport =
PRE
  CSAI_STATUS = Connected & 
  csai_buff /= [] &
  first(csai_buff) =  Sa_DATA &
  csai_sent_but_not_yet_received = 0
THEN
  CSAI_STATUS := Connected; 
  //
  ccsl_buff := ccsl_buff <- SAI_Error_report;
  //
  csai_buff := tail(csai_buff);
  csai_databuff := tail(csai_databuff)
END;

// -- deliver NRBC Msg to APP
// --  with no other notification if not invalid and not out of sequence
R14_CSAI_CCSL_dataindication =
PRE
  CSAI_STATUS = Connected & 
  csai_buff /= [] &
  first(csai_buff) =  Sa_DATA &
  csai_sent_but_not_yet_received = 0
THEN
  CSAI_STATUS := Connected; 
  //
  ccsl_buff := ccsl_buff <- SAI_DATA_indication;
  ccsl_databuff := ccsl_databuff <- first(csai_databuff);
  //
  csai_buff := tail(csai_buff);
  csai_databuff := tail(csai_databuff)
END;

// -- deliver NRBC Msg if not invalid but slightly out of sequence, 
// --  and notify the error
R15_CSAI_CCSL_saidataindication_CCSL_saierrorreport =
PRE
  CSAI_STATUS = Connected & 
  csai_buff /= [] &
  first(csai_buff) =  Sa_DATA &
  csai_sent_but_not_yet_received < csai_N &
  csai_sent_but_not_yet_received > 0 
THEN
  CSAI_STATUS := Connected; 
  //
  ccsl_buff := ccsl_buff <- SAI_DATA_indication;
  ccsl_databuff := ccsl_databuff <- first(csai_databuff);
  ccsl_buff :=ccsl_buff <- SAI_Error_report;
  csai_sent_but_not_yet_received :=0;
  //
  csai_buff := tail(csai_buff);
  csai_databuff := tail(csai_databuff)
END;

// -- do not deliver NRBC Msg if invalid, when slightly out of sequence, 
// --  but notify both error situations
R16_CSAI_CCSL_saierrorreport =
PRE
  CSAI_STATUS = Connected & 
  csai_buff /= [] &
  first(csai_buff) =  Sa_DATA &
  csai_sent_but_not_yet_received < csai_N &
  csai_sent_but_not_yet_received > 0 
THEN
  CSAI_STATUS := Connected; 
  //
  ccsl_buff := ccsl_buff <- SAI_Error_report;
  csai_sent_but_not_yet_received :=0;
  //
  csai_buff := tail(csai_buff);
  csai_databuff := tail(csai_databuff)
END;

// ---------------------------------
// -- handling connection failures
// --------------------------------

// -- notify disconnection if the last msgs shows an excessive msg delay or loss
R17_CSAI_ISAI_saconnterminate_CCSL_saidisconnectindication =
PRE
  CSAI_STATUS = Connected & 
  csai_buff /= [] &
  first(csai_buff) =  Sa_DATA &
  csai_sent_but_not_yet_received = csai_N
THEN
  CSAI_STATUS := Disconnected; 
  //
  isai_buff := isai_buff <- Sa_CONN_terminate;
  ccsl_buff := ccsl_buff <- SAI_DISCONNECT_indication;
  csai_sent_but_not_yet_received :=0;
  //
  csai_buff := tail(csai_buff);
  csai_databuff := tail(csai_databuff)
END;

// ---------------------------------
// -- handling disconnection requests
// --------------------------------


// -- when connected, become disconnected upon termination request from RBC
R18_CSAI_ISAI_saconnterminate =
PRE
  CSAI_STATUS = Connected & 
  csai_buff /= [] &
  first(csai_buff) =  SAI_DISCONNECT_request 
THEN
  CSAI_STATUS := Disconnected; 
  //
  isai_buff := isai_buff <- Sa_CONN_terminate;
  csai_sent_but_not_yet_received :=0;
  //
  csai_buff := tail(csai_buff)
END;

// -- when connected, become disconnected upon termination request from NSAI
R19_CSAI_CCSL_saidisconnectindication =
PRE
  CSAI_STATUS = Connected & 
  csai_buff /= [] &
  first(csai_buff) =  Sa_CONN_terminate 
THEN
  CSAI_STATUS := Disconnected; 
  //
  ccsl_buff := ccsl_buff <- SAI_DISCONNECT_indication;
  csai_sent_but_not_yet_received :=0;
  //
  csai_buff := tail(csai_buff)
END;

// -- But disconnection indication from ER can get lost!!
R20_CSAI_ndloss_saconnterminate =
PRE
  CSAI_STATUS = Connected & 
  csai_buff /= [] &
  first(csai_buff) =  Sa_CONN_terminate 
THEN
  CSAI_STATUS := Connected; 
  //
  csai_buff := tail(csai_buff)
END;


/////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////
//  CCSL
/////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////

// -------------------------------------------------------------
// -- when disconnetted ignore NRBC_MSGs and SAI_Notifications, 
// --  just handle clock cycles
// -------------------------------------------------------------

R1_CCSL_discard_userdata =
PRE
  CCSL_STATUS = NOCOMMS &
  ccsl_buff /= [] &
  first(ccsl_buff) =  RBC_User_Data_request 
THEN
  CCSL_STATUS := NOCOMMS;
  //
  ccsl_buff := tail(ccsl_buff);
  ccsl_databuff := tail(ccsl_databuff)
END;

R2_CCSL_discard_disconnectindication =
PRE
  CCSL_STATUS = NOCOMMS&
  ccsl_buff /= [] &
  first(ccsl_buff) =  SAI_DISCONNECT_indication 
THEN
  CCSL_STATUS := NOCOMMS;
  //
  ccsl_buff := tail(ccsl_buff)
END;

R3_CCSL_discard_errorreport =
PRE
  CCSL_STATUS = NOCOMMS &
  ccsl_buff /= [] &
  first(ccsl_buff) =  SAI_Error_report 
THEN
  CCSL_STATUS := NOCOMMS;
  //
  ccsl_buff := tail(ccsl_buff)
END;

R4_CCSL_discard_dataindication =
PRE
  CCSL_STATUS = NOCOMMS &
  ccsl_buff /= [] &
  first(ccsl_buff) =  SAI_DATA_indication 
THEN
  CCSL_STATUS := NOCOMMS;
  //
  ccsl_buff := tail(ccsl_buff);
  ccsl_databuff := tail(ccsl_databuff)
END;

// --------------------------------
// -- establishing connections 
// --------------------------------

// when disconnected just wait for SAI_CONNECT_indication
//
R7_CCSL_Timer_okccsl =
PRE
  CCSL_STATUS = NOCOMMS& 
  ccsl_buff /= [] &
  first(ccsl_buff) =  ccsl_tick 
THEN
  CCSL_STATUS := NOCOMMS;
  //
  timer_buff := timer_buff <- ok_ccsl;
  //
  ccsl_buff := tail(ccsl_buff)
END;

// --------------------------------
// -- handling connection indications 
// --------------------------------

// when disconnected, accept SAI_CONNECT_indication and reset timers
//
R8_CCSL_CRBC_rbcuserconnectindication =
PRE
  CCSL_STATUS = NOCOMMS &
  ccsl_buff /= [] &
  first(ccsl_buff) =  SAI_CONNECT_indication 
THEN
  CCSL_STATUS :=COMMS;
  //
  ccsl_receive_timer := 0;
  ccsl_send_timer := 0; 
  crbc_buff := crbc_buff <- RBC_User_Connect_indication;
  //
  ccsl_buff := tail(ccsl_buff)
END;

// -- when connected, still accept SAI connection notifications and reset timers
R9_CCSL_CRBC_rbcuserconnectindication =
PRE
  CCSL_STATUS =COMMS &
  ccsl_buff /= [] &
  first(ccsl_buff) =  SAI_CONNECT_indication 
THEN
  CCSL_STATUS :=COMMS;
  //
  ccsl_receive_timer := 0;
  ccsl_send_timer := 0; 
  crbc_buff := crbc_buff <- RBC_User_Connect_indication;
  //
  ccsl_buff := tail(ccsl_buff)
END;

// --------------------------------
// -- handling active connections 
// --------------------------------

//  when connected forward RBC User NRBC_MSGs     
// 
R10_CCSL_CSAI_saidatarequest =
PRE
  CCSL_STATUS = COMMS &
  ccsl_buff /= [] &
  first(ccsl_buff) =  RBC_User_Data_request 
THEN
  CCSL_STATUS :=COMMS;
  //
  ccsl_send_timer := 0;
  csai_buff := csai_buff <- SAI_DATA_request;
  csai_databuff := csai_databuff <- first(ccsl_databuff);
  //
  ccsl_buff := tail(ccsl_buff);
  ccsl_databuff := tail(ccsl_databuff)
END;


// when connected, in no send or receive timers are expired, adjust timers
//
R11_CCSL_Timer_okccsl =
PRE
  CCSL_STATUS = COMMS &
  ccsl_buff /= [] &
  first(ccsl_buff) =  ccsl_tick &
  ccsl_receive_timer < ccsl_max_receive_timer &
  ccsl_send_timer < ccsl_max_send_timer 
THEN
  CCSL_STATUS := COMMS;
  //
  timer_buff := timer_buff <- ok_ccsl;
  ccsl_send_timer := ccsl_send_timer + 1;
  ccsl_receive_timer := ccsl_receive_timer+1;
  //
  ccsl_buff := tail(ccsl_buff)
END;


// -- when connected, if the receive timer is not expired but send timer is expired, 
// --   send lifesign
R12_CCSL_Timer_okccsl_CSAI_saidatarequest =
PRE
  CCSL_STATUS =COMMS &
  ccsl_buff /= [] &
  first(ccsl_buff) =  ccsl_tick &
  ccsl_receive_timer < ccsl_max_receive_timer &
  ccsl_send_timer = ccsl_max_send_timer 
THEN
  CCSL_STATUS :=COMMS;
  //
  timer_buff := timer_buff <- ok_ccsl;
  ccsl_send_timer := 0;
  ccsl_receive_timer := ccsl_receive_timer + 1;
  csai_buff := csai_buff <- SAI_DATA_request;
  csai_databuff := csai_databuff <- LifeSign;
  //
  ccsl_buff := tail(ccsl_buff)
END;


// -- when connected, is msg received from sai,forward to user and 
// -- reset receive timer;
// 
R13_CCSL_CRBC_rbcuserdataindication =
PRE
  CCSL_STATUS =COMMS &
  ccsl_buff /= [] &
  first(ccsl_buff) =  SAI_DATA_indication &
  first(ccsl_databuff) /=  LifeSign
THEN
  CCSL_STATUS :=COMMS;
  //
  ccsl_receive_timer := 0;
  crbc_buff := crbc_buff <- RBC_User_Data_indication;
  crbc_databuff := crbc_databuff <- first(ccsl_databuff);
  //
  ccsl_buff := tail(ccsl_buff);
  ccsl_databuff := tail(ccsl_databuff)
END;

// -- when connected, is lifesign received from sai, reset receive timer; 
R14_CCSL_handle_lifesign =
PRE
  CCSL_STATUS =COMMS &
  ccsl_buff /= [] &
  first(ccsl_buff) =  SAI_DATA_indication &
  first(ccsl_databuff) =  LifeSign 
THEN
  CCSL_STATUS :=COMMS;
  //
  ccsl_receive_timer := 0;
  //
  ccsl_buff := tail(ccsl_buff);
  ccsl_databuff := tail(ccsl_databuff)
END;

// when connected, ignore SAI_Error_report
//
R15_CCSL_CRBC_rbcusererrorindication =
PRE
  CCSL_STATUS = COMMS &
  ccsl_buff /= [] &
  first(ccsl_buff) =  SAI_Error_report
THEN
  CCSL_STATUS :=COMMS;
  //
  ccsl_buff := tail(ccsl_buff)
END;


// when connected, disconnect and forward notification when notified by SAI
//
R16_CCSL_IRBC_rbcuserdisconnectindication =
PRE
  CCSL_STATUS = COMMS &
  ccsl_buff /= [] &
  first(ccsl_buff) =  SAI_DISCONNECT_indication
THEN
  CCSL_STATUS := NOCOMMS;
  //
  crbc_buff := crbc_buff <- RBC_User_Disconnect_indication;
  ccsl_receive_timer := 0;
  ccsl_send_timer := 0;
  //
  ccsl_buff := tail(ccsl_buff)
END;

// when connected, if receive timer expires,disconnect, notify User and 
//  request SAI termination
//
R17_CCSL_Timer_okccsl_CRBC_rbcuserdisconnectindication_CSAI_saidisconnectrequest =
PRE
  CCSL_STATUS = COMMS &
  ccsl_buff /= [] &
  first(ccsl_buff) =  ccsl_tick &
  ccsl_receive_timer = ccsl_max_receive_timer
THEN
  CCSL_STATUS := NOCOMMS;
  //
  timer_buff := timer_buff <- ok_ccsl;
  crbc_buff := crbc_buff <- RBC_User_Disconnect_indication;
  csai_buff := csai_buff <- SAI_DISCONNECT_request;
  ccsl_receive_timer := 0;
  ccsl_send_timer := 0;
  //
  ccsl_buff := tail(ccsl_buff)
END;


/////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////
//  CLOCK
/////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////


R1_Timer_irbc_tick =
PRE
  TIMER_STATUS = s0 &
  timer_starting = TRUE 
THEN
  TIMER_STATUS := s0;
  //
  timer_starting := FALSE;
  irbc_buff := irbc_buff <- irbc_tick
  //
END;

R2_Timer_icsl_tick =
PRE
  TIMER_STATUS = s0 &
  timer_buff /= [] &
  first(timer_buff) =  ok_irbc
THEN
  TIMER_STATUS := s0;
  //
  icsl_buff := icsl_buff <- icsl_tick;
  //
  timer_buff := tail(timer_buff)
END;


R3_Timer_isai_tick =
PRE
  TIMER_STATUS = s0 &
  timer_buff /= [] &
  first(timer_buff) =  ok_icsl
THEN
  TIMER_STATUS := s0;
  //
  isai_buff := isai_buff <- isai_tick;
  //
  timer_buff := tail(timer_buff)
END;


R5_Timer_ccsl_tick =
PRE
  TIMER_STATUS = s0 &
  timer_buff /= [] &
  first(timer_buff) =  ok_isai
THEN
  TIMER_STATUS := s0;
  //
  ccsl_buff := ccsl_buff <- ccsl_tick;
  //
  timer_buff := tail(timer_buff)
END;


R4_Timer_csai_tick =
PRE
  TIMER_STATUS = s0 &
  timer_buff /= [] &
  first(timer_buff) =  ok_ccsl
THEN
  TIMER_STATUS := s0;
  //
  csai_buff := csai_buff <- csai_tick;
  //
  timer_buff := tail(timer_buff)
END;



R6_Timer_crbc_tick =
PRE
  TIMER_STATUS = s0 &
  timer_buff /= [] &
  first(timer_buff) =  ok_csai
THEN
  TIMER_STATUS := s0;
  //
  crbc_buff := crbc_buff <- crbc_tick;
  //
  timer_buff := tail(timer_buff)
END;


R7_Timer_irbc_tick =
PRE
  TIMER_STATUS = s0 &
  timer_buff /= [] &
  first(timer_buff) =  ok_crbc
THEN
  TIMER_STATUS := s0;
  //
  irbc_buff := irbc_buff <- irbc_tick;
  //
  timer_buff := tail(timer_buff)
END;


/////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////
//  IRBC
/////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////

R1_IRBC_Timer_okirbc =
PRE
  IRBC_STATUS = idle &
  irbc_buff /= [] &
  first(irbc_buff) =  irbc_tick
THEN
  IRBC_STATUS := idle;
  //
  timer_buff := timer_buff <- ok_irbc;
  //
  irbc_buff := tail(irbc_buff)
END;


R2_IRBC_discard_connectindication =
PRE
  IRBC_STATUS = idle &
  irbc_buff /= [] &
  first(irbc_buff) =  RBC_User_Connect_indication
THEN
  IRBC_STATUS := idle;
  //
  irbc_buff := tail(irbc_buff)
END;


R3_IRBC_discard_dataindication =
PRE
  IRBC_STATUS = idle &
  irbc_buff /= [] &
  first(irbc_buff) =  RBC_User_Data_indication
THEN
  IRBC_STATUS := idle;
  //
  irbc_buff := tail(irbc_buff);
  irbc_databuff := tail(irbc_databuff)
END;


R4_IRBC_discard_disconnectindication =
PRE
  IRBC_STATUS = idle &
  irbc_buff /= [] &
  first(irbc_buff) =  RBC_User_Disconnect_indication
THEN
  IRBC_STATUS := idle;
  //
  irbc_buff := tail(irbc_buff)
END;


/////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////
//  CRBC
/////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////

R1_CRBC_Timer_okcrbc =
PRE
  CRBC_STATUS = idle &
  crbc_buff /= [] &
  first(crbc_buff) =  crbc_tick
THEN
  CRBC_STATUS := idle;
  //
  timer_buff := timer_buff <- ok_crbc;
  //
  crbc_buff := tail(crbc_buff)
END;


R2_CRBC_discard_connectindication =
PRE
  CRBC_STATUS = idle &
  crbc_buff /= [] &
  first(crbc_buff) =  RBC_User_Connect_indication
THEN
  CRBC_STATUS := idle;
  //
  crbc_buff := tail(crbc_buff)
END;


R3_CRBC_discard_dataindication =
PRE
  CRBC_STATUS = idle &
  crbc_buff /= [] &
  first(crbc_buff) =  RBC_User_Data_indication
THEN
  CRBC_STATUS := idle;
  //
  crbc_buff := tail(crbc_buff);
  crbc_databuff := tail(crbc_databuff)
END;

R4_CRBC_discard_disconnectindication =
PRE
  CRBC_STATUS = idle &
  crbc_buff /= [] &
  first(crbc_buff) =  RBC_User_Disconnect_indication
THEN
  CRBC_STATUS := idle;
  //
  crbc_buff := tail(crbc_buff)
END

END