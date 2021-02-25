/*

  Un esempio di macchine nidificate (questa è la macchina inclusa)

	Si considera un sistema progettato per controllare l'apertura e la chiusura di un insieme di porte di sicurezza 
	del caveau di una banca. Occorre aprire e chiudere le porte così come permetterne il loro bloccaggio e sbloccaggio.
	
	La seguente macchina introduce l'insieme DOOR di tutte le porte e l'insieme enumerativo POSITION di possibili posizioni
	in cui ogni porta può trovarsi (aperta o chiusa). La macchina permette da ogni porta di essere aperta o chiusa in qualsiasi
	posizione essa si trovi.
	
	La robustezza di questa macchina risiede nel fatto che essa non impone requisiti sulle sue operazioni. Una richiesta per una porta
	di raggiungere una posizione che già detiene, lascerà semplicemente lo stato immutato.
	
	La macchina Doors verrà inclusa all'interno di un'altra macchina, chiamata "Locks", che tiene traccia dello stato in cui si trovano
	le porte: serrate o meno.
	Si osservi che tutte le informazioni presenti in Doors sono visibili in Locks. L'insieme DOOR è riferito nell'invariante, 
	nell'inizializzazione e nelle precondizioni di ogni operazione.
	
	
	La sintassi degli operatori utilizzati per questa macchina è la seguente:
	
		:		appartenenza
		s<->t	mappa a
		*		prodotto cartesiano
		
*/


MACHINE Doors

SETS DOOR; POSITION = {open, closed}

VARIABLES pos

INVARIANT pos:DOOR<->POSITION

INITIALISATION pos:= DOOR*{closed}

OPERATIONS
	opening   (dd) =  PRE  dd : DOOR
		           THEN pos(dd) := open
		           END;
	closedoor (dd) = PRE dd : DOOR
		            THEN pos(dd) := closed
		            END
					  
END
			
