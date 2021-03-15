/*

	Questo è un esempio di utilizzo della clausola USES. La macchina Registrar, definita nel seguito, corrisponde alla macchina che estende 
	la macchina Marriage e include la macchina Life per farsi carico della proof obligation a garanzia della preservazione dell'invariante
	della macchina che usa attraverso il monitoraggio delle operazioni della macchina usata.
	
	Questo esempio mostra come sia possibile rimpiazzare la clausola SEES con una clausola USES attraverso la definizione di una macchina
	astratta coerente.
	
	Sta di fatto che la macchina Registrar include entrambe le macchine per transitività e promuove tutte le loro operazioni ad eccezione di 
	"die". Non introduce nuovi insiemi, costanti o variabili, né presenta la sua clausola INVARIANT. Tuttavia, di fatto eredita l'invariante
	della macchina Marriage e dovrà assicurarsi che questa sia preservata da tutte le sue operazioni.
	
	La macchina in questione fornisce un'unica nuoova operazione:
		1 - dies, esegue l'aggiornamento dello stato dell'esito dell'operazione "die" della macchina Life per garantire la preservazione
		          dell'invariante della macchina Marriage al variare delo stato di Life.
		
		
	Notazione

	:				  appartenenza
	S\/T			unione
	dom(r)		dominio di una relazione
	ran(r)		rango (o campo) di una relazione
	~				  relazione inversa (con tilde da posporre al nome della funzione)
	A||B			operazioni eseguite in parallelo

*/


MACHINE Registrar

EXTENDS Marriage

INCLUDES Life

PROMOTES born

OPERATIONS
	dies (nn) = 
			PRE  nn:male\/female
			THEN
				die(nn) || 
				IF   nn:dom(marriage) THEN part(nn, marriage(nn))
				ELSIF nn:ran(marriage) THEN part(marriage~(nn), nn)
				END
			END
			
END
