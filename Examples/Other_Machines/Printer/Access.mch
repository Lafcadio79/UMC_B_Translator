/*

	Una macchina che tiene traccia dei permessi di accesso da parte degli utenti
	alle stampanti di un sistema
	
	La sintassi degli operatori (relazionali e non) utilizzati per questa macchina è:
	
	ran(S)		campo
	dom(S)  	dominio
	card(S)		cardinalità

	S\/T 		unione
	s|->t		mappa a
	{}			insieme
	S<->T		relazione	

	=			uguaglianza
	:=			assegnamento
	& 			congiunzione
	:			appartenenza

	r[S]		immagine relazionale
	r|>S		restrizione del campo
	(r_1;r_2)	composizione relazionale
	
	
	La macchina in esame introduce 4 insiemi, tre dei quali (USER, PRINTER e OPTION) sono differiti e semplicemente dichiarati.
	Il quarto (PERMISSION) dà l'insieme dei possibili permessi che possono essere registrati dalla macchina ed è definito
	come un insieme enumerativo di due elementi.
	Esiste una variabile "access" che tiene traccia di quali utenti hanno il permesso di accedere alle stampanti. E' una relazione
	tra USER e PRINTER, e l'intenzione è che una particolare coppia uu|->pp è in questa relazione precisamente quando l'utente uu 
	ha il permesso di usare la stampante pp. Al momento dell'inizializzazione non contiene nessun permesso.
	Le prime due operazioni "add" e "block" semplicemente permettono alla relazione "access" di essere aggiornata aggiungendo o rimuovendo
	un particolare permesso dell'utente ad accedere a una particolare stampante. L'assegnamento fa uso di una notazione insiemistica
	per aggiungere o rimuovere un singolo elemento dall'insieme denotato da "access" (che in questo casao è una coppia).
	L'operazione "ban" rimuove l'accesso di un utente a tutte le stampanti in un colpo solo. Questo è espresso da un singolo aggiornamento 
	sulla variabile "access".
	Più complesse alterazioni ad "access" possono essere espresse all'interno di un'operazione. L'aggiornato "unify" dà i permessi 
	di accesso a due utenti combinati. In questo modo aggiunge tutti i permessi dell'utente 1 all'utente 2 e vice versa in un singolo
	assegnamento.
	Esistono due interrogazioni tra le operazioni: "optionquery" e "printnumquery". La prima fornisce informazioni sulla possibilità
	che un utente ha di accedere o meno a determinate operazioni, attraverso qualsiasi stampante tra quelle a cui ha accesso. Questo è realizzato
	attraverso la composizione relazionale della variabile relazionale "access" e la costante relazionale "options". La seconda interrogazione
	restituisce il numero di utenti che hanno accesso a una particolare stampante.
	
*/

MACHINE Access

SETS USER; PRINTER; OPTION; PERMISSION = {ok, noaccess}

CONSTANTS options

PROPERTIES options:PRINTER<->OPTION & dom(options) = PRINTER & ran(options) = OPTION

VARIABLES access

INVARIANT access:USER<->PRINTER

INITIALISATION access := {}

OPERATIONS
	add(uu, pp) =
		PRE uu:USER & pp:PRINTER
		THEN access := access\/{uu|->pp}
		END;
	block(uu, pp) = 
		PRE uu:USER & pp:PRINTER
		THEN access := access - {uu|->pp}
		END;
	ban(uu) = 
		PRE uu:USER
		THEN access := {uu}<|access
		END;
	unify(u1, u2) = 
		PRE u1:USER & u2:USER
		THEN access := access \/{u1}*access[{u2}]\/{u2}*access[{u1}]
		END;
	ans <-- optionquery(uu, oo) = 
		PRE uu:USER & oo:OPTION
		THEN IF uu|->oo:(access;options)
			THEN ans := ok
			ELSE ans := noaccess
			END
		END;
	nn <-- printnumquery(pp) = 
		PRE pp:PRINTER
		THEN nn := card(access|>{pp})
		END

END
