/*  
 *
 * La macchina Reading tiene traccia sia dei libri che gli alunni hanno letto
 * che di quelli ancora in fase di lettura 
 *
 */


/*

    =           uguaglianza

    :=          assegnamento

	&           congiunzione
	
	Relazioni
	
    S<->T     	relazione   
	
    r[S]        immagine relazionale

    r|>S     	restrizione del campo

    r<<|S     	sottrazione dal dominio (o anti-restrizione del dominio)

    (r_1;r_2)   composizione relazionale


    Insiemi
	
    S\/T        unione

    s|->t       mappa a

    card(S)     cardinalità

    S/\T	intersezione

    {}          insieme vuoto

    s:S         appartenenza
	
    s/:S	non appartenenza
	

    Funzioni
	
    ran(S)      campo

    dom(S)      dominio

    -->>	funzione suriettiva totale (ogni elemento del codominio è immagine di almeno un elemento del dominio)

    >+>		funzione iniettiva parziale (associa a elementi distinti del dominio elementi distinti del codominio)
	
	
    Descrizione:
	
	Di ogni libro possono esserci più copie quindi vengono introditti gli insiemi COPY e BOOK. Il primo (COPY) tiene
	traccia delle copie individuali in circolazione, il secondo (BOOK) tiene traccia dei libri letti.
	La funzione costante "copyof" lega questi due insiemi. La clausola PROPERTIES contiene le informazioni su che tipo
	di funzione essa sia, una funzione totale suriettiva. Dal fatto che si tratti di una funzione, se ne deduce che
	ogni copia può essere associata a non più di un libro; il fatto che sia totale ci dice che ogni copia è associata
	con esattamente un libro; e il fatto che sia suriettiva ci dice che esiste almeno una copia di ogni libro.
	Dato che "copyof" è una costante, significa che essa è fissata una volta per tutte e che rimarrà tale per tutta la
	durata dell'esecuzione della macchina. Perciò questa macchina non permette di aggiungere alla libreria né nuovi
	libri né copie di libri esistenti. In altre parole, lo stock della biblioteca è statico.
	L' informazione da tenere in memoria è un record che tiene traccia di quali libri sono stati precedentemente letti
	da un bambino e quali libri quel bambino sta attualmente leggendo. Per questo sono introdotte due variabili:
	"hashread" e "reading". L'inveriante ci dà l'informazione che "hashread" dovrebbe trattarsi di una relazione tra 
	lettori e libri. Il fatto che "reading" sia una funzione iniettiva parziale ci dice che nessun lettore possa 
	attualmente leggere più di un libro; il fatto che sia iniettiva ci dice che differenti lettori stanno leggendo
	differenti libri o copie differnti di libri.
	L'ultima riga delle invarianti evidenzia il requisito che i lettori non dovrebbero leggere copie di libri che
	abbiano già letto. La funzione "reading;copyof" identifica il libro che è attualmente in fase di lettura da parte
	di un lettore, e rappresenta quindi una relazione tra lettori e libri. L'invariante ci dice che nessun elemento di
	questa relazione (mappatura della forma rr |-> bb) si trova nella realzione "hashread".
	La clausala INITIALISATION semplicemente inizializza queste due relazioni alla funzione vuota.
	
	L'operazione "start" prende come input un lettore e una copia di un libro, imponendo poi una precondizione restrittiva
	perché possano essere accettati come nuove informazioni che aggiornino la relazione "reading": che rr non appartenga al
	dominio di "reading", ossia che il lettore non sia già nella lista di quelli che stanno già leggendo una copia di qualche
	libro; che cc non appartenga al campo (o rango) di "reading", ossia che la cpia non sia già letta da qualcuno; e che il
	libro non sia già stato letto dal lettore. Se tutte queste precondizioni sono rispettate, allora la funzione "reading"
	può essere aggiornata semplicemente aggiungendovi la nuova coppia, (lettore, libro).
	
	In modo simile l'operazione "finished" prende come input un lettore e una copia di un libro, e aggiorna le relazioni in
	modo che la copia del libro sia registrata in "hashread" come letta e che non figuri più in "reading" come copia 
	effettivamente in fase di lettura.
	
	Le altre tre operazioni sono tutte interrogazioni (query). In esse si trovano condizioni booleane su funzioni (in 
	precurrentquery e hashreadquery), e variabili a cui sono assegnati i valori di una espressione su una funzione currentquery.
	
	La macchina potrebbe beneficiare anche di un'operazione non-deterministica cc <-- choosebook(rr) che imposta cc come
	elemento di COPY che non sia ancora stato letto dal dato rr, e del quale esiste una copia a disposizione (ossia una copia
	che nessuno stia attualmente leggendo).
	
*/

MACHINE Reading

SETS 
	READER; BOOK; COPY; RESPONSE = {yes, no}

CONSTANTS
	copyof

PROPERTIES 
	copyof : COPY -->> BOOK

VARIABLES 
	hashread, reading

INVARIANT
	hashread : READER <-> BOOK & 
	reading  : READER >+> COPY &
	(reading; copyof) /\ hashread = {}

INITIALISATION
	hashread := {} || reading := {}
	
OPERATIONS
	start(rr, cc) = 
		PRE
			rr : READER & cc : COPY & copyof(cc) /: hashread[{rr}] & rr /: dom(reading) & cc /: ran(reading)
		THEN
			reading := reading \/ {rr |-> cc}
		END;
	finished(rr, cc) = 
		PRE
			rr : READER & cc : COPY & cc = reading(rr)
		THEN
			hashread := hashread \/ {rr |-> copyof(cc)} || 
			reading  := {rr} <<| reading
		END;
	resp <-- precurrentquery(rr) = 
		PRE 
			rr : READER
		THEN
			IF rr : dom(reading)
			THEN 
				resp := yes
			ELSE
				resp := no
			END
		END;
	bb <-- currentquery(rr) = 
		PRE 
			rr : READER & rr : dom(reading)
		THEN
			bb := copyof(reading(rr))
		END;
	resp <-- hashreadquery(rr,bb) = 
		PRE
			rr : READER & bb : BOOK
		THEN
			IF bb : hashread[{rr}]				
			THEN
				resp := yes
			ELSE
				resp := no
			END
		END

END
