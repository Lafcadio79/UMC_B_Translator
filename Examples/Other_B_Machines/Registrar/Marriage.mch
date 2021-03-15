/*

	Questo è un esempio di utilizzo della clausola USES. La macchina Marriage, definita nel seguito, corrisponde alla macchina che usa 
	la macchina Life. Infine la macchina Registrar estende la macchina Marriage e include la macchina Life per farsi carico della 
	proof obligation a garanzia della preservazione dell'invariante della macchina che usa attraverso il monitoraggio delle operazioni della
	macchina usata.
	
	Questo esempio mostra come sia possibile rimpiazzare la clausola SEES con una clausola USES attraverso la definizione di una macchina
	astratta coerente.
	
	La registrazione dei matrimoni è dipendente dalel informazioni contenute nello stato della macchina Life. La macchina Marriage mantiene
	una relazione "marriage" che è ben rappresentata da una funzione uno-a-uno, da maschio a femmina. Così l'invariante di Marriage mette
	in relazione il suo stato "marriage" allo stato della macchina Life. Così come è richiesto dalla relazione essere una funzione iniettiva
	parziale, implicitamente ne consegue che il dominio di "marriage" è sottoinsieme o insieme coincidente di "male" e il suo rango, o campo,
	sarà sottoinsieme o insieme coincidente di "female". Questa proprietà è richiesta all'interno della specifica complessiva, anche se non
	può essere garantita da Marriage.
	
	La macchina in questione fornisce tre operazioni:
		1 - wed, che aggiunge una coppia alla funzione "marriage";
		2 - part, che rimuove una coppia dalla funzione "marriage";
		3 - partner, un'operazione di query che restituisce il coniuge della persona data in input.
		
		
	Notazione

	:			appartenenza
	S>+>T			funzione iniettiva parziale
	dom(r)			dominio di una relazione
	ran(r)			rango (o campo) di una relazione
	|->			mappa in una relazione 1-a-1
	~			relazione inversa (con tilde da posporre al nome della funzione)
*/



MACHINE Marriage

USES Life

VARIABLES marriage

INVARIANT marriage:male>+>female 

INITIALISATION marriage := {}

OPERATIONS
	wed (mm, ff) = 
				PRE  mm:male & mm/:dom(marriage) & ff:female & ff/:ran(marriage) 
				THEN marriage(mm) := ff
				END;

	part (mm, ff) = 
				PRE  mm:male & ff:female & mm|->ff:marriage
				THEN marriage := marriage - {mm|->ff}
				END;
	pp <-- partner (nn) = 
	
				PRE  nn:dom(marriage) \/ ran(marriage)
				THEN
					IF   nn:dom(marriage)
					THEN pp := marriage(nn)
					ELSE pp := marriage~(nn)
					END
				END

END
