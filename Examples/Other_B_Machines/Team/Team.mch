/*
			
	La macchina "Team" gestisce le sostituzioni un insieme di giocatori che sono su un campo da football durante una partita.
	Sono presenti 22 giocatori complessivamente, numerati da 1 a 22, e ogni team è costituito da 11 di loro.
	Il team inizia con i primi 11 giocatori. la macchina offre due operazioni:
		1 - substitute: che permette la sostituzione dei giocatori
		2 - query: che permette di verificare se un particolare giocatore è attualmente nel team. Fornisce un
			output dall'insieme enumerativo ANSWER fornito in precedenza.


	Notazione utilizzata:
	:			  appartenenza
	:=			assegnamento
	/:			non appartenenza
	S<:T		sottoinsieme di 
	card(S) cardinalità
	\/			unione

*/



MACHINE Team

SETS ANSWER={in, out}

VARIABLES team

INVARIANT team <: 1..22 & card(team) = 11

INITIALISATION team:=1..11

OPERATIONS

	substitute (pp, rr) = 
		PRE  pp : team & rr : 1..22 & rr /: team
		THEN team := (team\/{rr}) - {pp}
		END;
	aa <-- query(pp) = 
		PRE  pp : 1..22
		THEN
			IF   pp : team
			THEN aa:= in
			ELSE aa:= out
			END
		END
		
		
END
