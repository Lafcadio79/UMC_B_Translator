/*
 *
 *	<<Overview>>
 *	Esempio di utilizzo di array.
 *	Viene definita di seguito una Macchina che traccia il numero di ospiti che albergano nelle stanze di un hotel.
 *	La Macchina prende un parametro "sze", corrispondente alla capienza dell'hotel misurata in base al suo numero di stanze.
 *	Le stanze sono introdotte da un insieme ROOM la cui cardinalità è "sze".
 *	
 *
 *	<<Specifiche>>
 *	Si tiene traccia del numero di ospiti presenti in ogni stanza dell'hotel attraverso un array di numeri di dimensione variabile.
 *	Le stanze possono essere di due tipi (o classi): small (capienza massima 4 persone) e large (capienza massima 6 persone).
 *	Le stanze piccole sono date dall'insieme "small" che è dichiarato nella clausola CONSTANTS e identificato come sottoinsieme
 *	di ROOM nella clausola PROPERTIES. In questo modo il massimo numero di persone che può essere registrato in ogni stanza è 
 *	di sei unità. Così "numbers" è dichiarato come una funzione che va da ROOM a 0..6. La dichiarazione è espressa come funzione totale, perché
 *	deve riflettere il numero di occupanti in ogni stanza. Una funzione parziale non sarebbe appropriata.
 *	Il requisito addizionale prevede che le stanze piccole non superino la capienza massima di 4 persone. Questo può essere
 *	espresso come il fatto che l'immagine di "small" attraverso "numbers" possa al massimo contenere un valore compreso tra 0 e 4 (ossia numbers <: 0..4).
 *	Non c'è bisogno di dichiarare che le altre stanze possano ospitare fino ad un massimo di 6 persone dato che questo dettglio è già espresso
 *	attraverso l'intervallo di tipo con cui "numbers" è dichiarato.
 *	L'array è inizializzato con tutti 0, riflettendo un hotel vuoto. Inoltre, nella clausola INITIALISATION sono inizializzati tutti gli
 *	elementi dell'array a 0 con un singolo assegnamento. In un programma canonico questo sarebbe generalmente realizzato attraverso un ciclo
 *	impostando ad ogni iterazione l'elemento i-esimo dell'array a 0.
 *	
 *	<<Operazioni>>
 *	Lo operazioni offerte dalla Macchina sono diverse:
 *
 *		- checkin (rr, nn) 		-> prende come input un numero di stanza e un numero di ospiti (fino a 6) da assegnargli. Questa operazione richiede come precondizione
 *						   che la stanza non debba essere già occupata da qualcuno e che il numero di ospiti sia adatto alla capienza della stanza. Se tutte queste condizioni
 *						   sono verificate, allora l'operazione aggiurna le informazioni nell'array di conseguenza.
 *
 *		- checkout (rr)         	-> accetta un qualsiasi numero di stanza come input e resetta a 0 il numero di ospiti registrato in quella stanza.
 *
 *		- roomquery (rr)   		-> accetta un numero di stanza come input e restituisce il numero di occupanti come output. Questa operazione, assieme alla
 *						   precedente illustra comel'array possa essere aggiornato e interrogato, come ci si aspetterebbe.
 *		
 *		- nn <-- totalguests    	-> è un'interrogazione che restituisce il numero totale di ospiti che attualmente sono presenti nell'hotel. Questo valore
 *						   è specificato come somma del numero di occupanti in ogni stanza. Come specifica, è necessario affermare solamente quello che dovrebbe essere l'output
 *						   la modalità con cui questo valore viene calcolato è irrilevante. Questa somma è facilmente definibile con l'array "numbers".
 *								   
 *		- nn <-- vacancies      	-> restituisce il numero di stanze vacanti, ossia, il numero di stanze che hanno 0 occupanti. L'insieme di queste stanze corrisponderà 
 *						   all'immagine relazionale inversa dell'insieme delle stanze, e quindi il numero di stanze vacanti corrisponderà alla cardinalità di questo insieme.
 *						   Infatti, la dimensione di questo insieme sarà della stessa dimensione del suo reciproco a cui sono assegnati i valori di 0, e che consiste
 *						   per l'appunto dall'insieme di tutte le mappature corrispondenti al valore 0. DIfatti esso corrisponde al campo della funzione "numbers" limitata a 0!
 *						   Così il numero di stanze vacanti sarà dato dall'espressione card(numbers |> {0}) che descrive l'operazione.
 *	
 		- swap (rr, ss)			-> inverte gli occupanti delle stanze rr e ss. La precondizione richiede che le stanze debbano avere la stessa dimensione: o entrambi piccole,
 *						   o entrambi grandi. L'array "numbers" traccia il numero di occupanti di ogni stanza scambiando i valori di "numbers(rr)" e "numbers(ss)".
 *						   Questo deve essere specificato come un assegnamento direttamente alla variabile array.
 *								   
 *	
 *	<<Notazione>>	
 *	:				appartenenza
 *	:= 				assegnamento
 *	<:				sottoinsieme o insieme coincidente
 *	{}				insieme vuoto
 *	<=				minore o uguale
 *	=>				implicazione
 *	<=>				implicazione doppia (equivalenza)
 *	card()				cardinalità
 *	S[T]				immagine relazionale
 *	|>				restrizione di campo
 *	SIGMA(z).(P|E)			sommatoria di insieme
 *	|->				mappatura
 *	--> 				funzione totale
 *	
 */


MACHINE hotel ( sze )

CONSTRAINTS	sze : NATURAL

SETS ROOM
	
CONSTANTS small

/* 
 * <<Nota>>
 * Il parametro sze in quanto parametro macchina non può essere acceduto nella sezione PROPERTIES perciò non si può definire il vincolo card(ROOM) <= sze 
 * E' possibile tuttavia assegnare un valore assoluto alla cardinalità di ROOM, indicandone in modo fittizio la capienza complessiva.
 *
 */	
 
PROPERTIES  small <: ROOM & card(ROOM) = 120

VARIABLES numbers

INVARIANT
	numbers : ROOM --> 0..6 & numbers[small] <: 0..4
	
INITIALISATION
	numbers := ROOM * {0}

OPERATIONS

	checkin (rr, nn) = 
		PRE 	rr : ROOM & nn : 1..6 & numbers(rr) = 0 & (rr : small => nn <= 4)
		THEN	numbers(rr) := nn
		END;	
	
	checkout (rr) = 
		PRE		rr :ROOM
		THEN	numbers(rr) := 0
		END;
	
	nn <-- roomquery (rr) = 
		PRE		rr : ROOM			
		THEN	nn := numbers(rr)
		END;	

	nn <-- vacancies = 
				nn := card(numbers |> {0});

	nn <-- totalguests = 
				nn := SIGMA(zz).(zz : ROOM | numbers(zz));
		
	swap (rr, ss) = 
		PRE		rr : ROOM & ss : ROOM & (rr : small <=> ss : small)
		THEN	numbers := numbers <+ {rr |-> numbers(ss), ss |-> numbers(rr)} 
		END

END
