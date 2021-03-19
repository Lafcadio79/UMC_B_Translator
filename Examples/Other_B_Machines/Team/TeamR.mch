/*
	
	Esempio di macchina di raffinamento!
		
	La macchina "TeamR" raffina l'esecuzione della macchina Team che gestisce un insieme di giocatori presenti 
	sul campo da football durante una partita.
	Sono presenti 22 giocatori complessivamente, numerati da 1 a 22, e ogni team è costituito da 11 di loro.
	Il team inizia con i primi 11 giocatori. la macchina offre due operazioni:
		1 - substitute: che permette la sostituzione dei giocatori
		2 - query: che permette di verificare se un particolare giocatore è attualmente nel team. Fornisce un
			output dall'insieme enumerativo ANSWER fornito in precedenza.

	Un rafinamento di questa macchina deve tenere traccia delle informazioni del "team" affinché possa rispondere correttamente alle
	interrogazioni, e in questo modo interagire con l'utente nel modo specificato dalla macchina Team. Tuttavia, esistono molti modi
	per tenere traccia di questa informazione. Una possibilità è quella di far uso di un array di dimensione 11 per tenere traccia
	dei giocatori effettivamente in campo.

	L'invariante di collegamento, necessaria in una macchina che raffina, è in questo esempio data dal vincolo ran(teamr)=team e 
	riflette la relazione tra lo spazio degli stati delle due macchine.

	Notazione utilizzata:
		:			appartenenza
		:=			assegnamento
		ran(S) 		rango (o campo)
		~			relazione inversa (con tilde da posporre al nome della funzione)
		%x.(P|E)	lambda abstraction
		
*/


REFINEMENT TeamR

REFINES Team

VARIABLES teamr

INVARIANT teamr:1..11<->1..22 & ran(teamr)=team

INITIALISATION teamr:=%nn.(nn:1..11|nn)

OPERATIONS
	substitute (pp, rr) = 
		teamr(teamr~(pp)):= rr;
	aa <-- query(pp) = 
		IF   pp : ran(teamr)
		THEN aa := in
		ELSE aa := out
		END
END