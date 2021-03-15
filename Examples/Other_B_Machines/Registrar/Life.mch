/*


	Questo è un esempio di utilizzo della clausola USES. La macchina Life, definita nel seguito, corrisponde alla macchina che viene usata 
	dalla macchina Marriage. Infine la macchina Registrar estende la macchina Marriage e include la macchina Life per farsi carico della 
	proof obligation a garanzia della preservazione dell'invariante della macchina che usa attraverso il monitoraggio delle operazioni della
	macchina usata.
	
	Questo esempio mostra come sia possibile rimpiazzare la clausola SEES con una clausola USES attraverso la definizione di una macchina
	astratta coerente.
	
	
	Le macchine descrivono il comportamento di un registro delle nascite. Si considerano due aspetti che un qualsiasi registro delle 
	nascite dovrebbe rispettare: la registrazione di nascita e morte, e la registrazione dei matrimoni.
	
	La macchina Life tiene traccia di tutte le persone viventi in due insiemi disgiunti maschi e femmine, e offre due operazioni:
	  1 - "born" che registra la nascita e il sesso di una nuova persona;
	  2 - "die"  che registra una morte, e la rimuove dallo stato della macchina.
	
	
	Notazione
	
	A||B			operazioni eseguite in parallelo
	S<:T			sottoinsieme di
	s/\t			intersezione
	s\/t			unione
	A&B				congiunzione
	E:T				appartenenza
	E/:T			non appartenenza
	S-t				sottrazione di un elemento da un insieme
*/



MACHINE Life

SETS PERSON; SEX = {boy, girl}

VARIABLES male, female

INVARIANT male<:PERSON & female <: PERSON & male/\female = {} 

INITIALISATION male := {} || female := {}

OPERATIONS
	born (nn, ss) =
			PRE nn:PERSON & nn/:(male\/female) & ss : SEX
			THEN
				IF 	  ss = boy
				THEN  male   := male   \/ {nn}
				ELSE  female := female \/ {nn}
				END
			END;

	die (nn) = 
			PRE nn : PERSON & nn : male \/ female 
			THEN
				IF    nn : male
				THEN  male := male - {nn}
				ELSE  female := female - {nn}
				END
			END

END

