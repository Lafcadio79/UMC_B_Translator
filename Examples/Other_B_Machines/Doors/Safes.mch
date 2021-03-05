/*

	La seguente macchina è integrativa della specifica "Doors, Locks e Keys" e introduce una macchina Safes
	usata per mettere in relazione le chiavi alle serrature, alle porte.

	La macchina Safes non mantiene nessuno stato a sé stante, ma è responsabile di assicurarsi la coerenza di
	tutti gli stati che include. Innanzitutto definisce una funzione biiettiva costante "unlocks" che mappa le
	chiavi alle porte che esse sbloccano: ogni chiave è associata con una porta differente, e ogni porta ha
	una chiave. Di conseguenza, questa relazione impone la condizione che gli insiemi di KEY e DOOR debbano
	essere della stessa dimensione.
	
	L'invariante ci dice che ogni porta che è sbloccata deve avere la sua corrispondente chiave inserita nella serratura.
	Tre operazioni di Locks preservano questa invariante, per cui possono essere promosse a operazioni di Safes. Si osservi
	che closedoor è dichiarata due livelli più sotto, nella macchina Doors. Le operazioni possono essere promosse a qualsiasi
	livello purché preservino l'invariante.
	Diversamente, unlockdoor ha il potenziale di violare l'invariante se viene chiamata in uno stato dove la chiave non è nella 
	porta. Per questo deve apparire con una precondizione più restrittiva all'interno dell'operazione unlock della macchina Safes.
	L'inserimento e l'estrazione della chiave deve essere permessa soltanto quando l'appropriata porta è provvista di chiave, così
	il requisito unlock(kk) = dd appare come parte della precondizione delle operazioni insert ed extract. Quest'ultima richiede anche
	che la porta dd debba essere serrata prima che la chiave venga estratta, dato che tutte le porte sbloccate devono avere le loro chiavi inserite.
	Infine, l'operazione quicklock aggiorna tutte le macchine incluse in Safes all'unisono, permettendo a una porta di essere chiusa e alla rispettiva
	chiave di essere rimossa dalla porta in un singolo passaggio. Questo è descritto da una combinazione parallela di due operazioni incluse, una da ogni macchina.
	La precondizione di quicklock esprime un requisito sula variabile position, che è inclusa da Doors.
	La visibilità dello stato di Doors è transitivo attraverso la relazione di inclusione, così è ancora visibile a Safes. La precondizione position(dd) = closed di
	lockdoor non viene scartata quando è chiamata all'interno del corpo di quicklock, è invece elevata nella precondizione di quicklock per assicurare che la sua chiamata 
	di lockdoor sia valida.
	
	Si osservi che non è possibile fornire un'operazione all'interno di Safes che al contempo, in una sola operazione, chiuda e blocchi la porta. Questo richiederebbe l'uso parallelo di
	due operazioni della macchina inclusa Locks all'iterno di una singola operazione di Safes, e questo non è permesso. Locks può eseguire una sola operazione alla volta.
	L'operazione parallela usata all'interno di un'operazione deve essere sempre collegata a differenti macchine. Per mettere a disposizione questa operazione, la macchina Locks
	dovrebbe avere bisogno di una singola operazione che la realizza.
	
	
	La sintassi degli operatori (relazionali e non) utilizzati per questa macchina è:

	: 			appartenenza
	S<:T 		sottoinsieme di
	S-T 		insieme differenza	
	>->>		funzione biiettiva totale
	~			relazione inversa
	||			operazione parallela
	&	 		congiunzione


*/

MACHINE Safes

INCLUDES Locks, Keys

PROMOTES opendoor, closedoor, lockdoor

CONSTANTS unlocks

PROPERTIES unlocks : KEY >->> DOOR

INVARIANT status~[{unlocked}] <: unlocks[keys]

OPERATIONS
	insert(kk, dd) = 
		PRE  kk : KEY & dd : DOOR & unlocks(kk) = dd
		THEN insertkey(kk)
		END;

	extract(kk, dd) = 
		PRE  kk : KEY & dd : DOOR & unlocks(kk) = dd & status(dd) = locked
		THEN removekey(kk)
		END;

	unlock(dd) = 	
		PRE  dd : DOOR & unlocks~(dd) : keys
		THEN unlockdoor(dd)
		END;

	quicklock(dd) = 	
		PRE  dd : DOOR & pos(dd) = closed
		THEN lockdoor(dd) || removekey(unlocks~(dd))
		END

END