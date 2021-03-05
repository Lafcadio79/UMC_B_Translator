/*

	La seguente macchina è integrativa della specifica "Doors and Locks" e vi introduce una macchina Keys
	che vincola, con la presenza di chiavi, l'apertura e la chiusura delle porte.
	
	La macchina Keys introduce un nuovo set di chiavi (KEY) e offre due operazioni per inserire e rimuovere le chiavi.
	Le chiavi possono essere inserite in una porta (se corrispondono alla serratura) che dovrebbe essere sbloccata solo
	se c'è una chiave inserita.
	Il sistema compessivo sarà gestito da un'ulteriore macchina, "Safes", che include Keys e Locks (e per transitività Doors).
	
	La sintassi degli operatori (relazionali e non) utilizzati per questa macchina è:
	
	:= 			assegnamento
	S<:T 		sottoinsieme di
	{}			insieme vuoto
	S\/T		unione di insiemi
	S-T 		insieme differenza	

*/


MACHINE Keys

SETS KEY

VARIABLES keys

INVARIANT keys <: KEY

INITIALISATION keys := {}

OPERATIONS
	insertkey(kk) = 
		PRE  kk : KEY 
		THEN keys := keys\/{kk}
		END;
	removekey(kk) = 
		PRE  kk : KEY
		THEN keys := keys - {kk}
		END
END


