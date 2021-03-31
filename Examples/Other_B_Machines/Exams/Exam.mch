/*

	Questa macchina tiene traccia dell'analisi statistica dei risultati ottenuti
	da ogni candidato (marks), e accetta informazioni su questi. Permette anche interrogazioni
	sui risultati di ogni candidato e può restituirne la media dei risultati raggiunti.
	
	
	
	Notazione:
	& 			      	congiunzione
	:	      			  appartenenza
	/:			        non appartenenza
	:= 				      assegnamento
	+->				      funzione parziale
	SIGMA(zz).(P|E)	sommatoria di elementi di un insieme

*/

MACHINE Exam
	
SETS CANDIDATE

VARIABLES marks

INVARIANT marks : CANDIDATE +-> 0..100

INITIALISATION marks := {}

OPERATIONS
	enter (cc, nn) = 
		PRE  cc : CANDIDATE & cc /: dom(marks) & nn : 0..100
		THEN marks(cc):= nn
		END;
	
	aa <-- average = 
		PRE  marks /: {}
		THEN aa := SIGMA(zz).(zz : dom(marks) | marks(zz)) / card(dom(marks))
	END;
	
	nn <-- number = 
		nn := card(dom(marks))
END
