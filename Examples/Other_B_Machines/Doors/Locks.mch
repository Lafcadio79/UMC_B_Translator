/*

        Un esempio di macchine nidificate (questa è la macchina che include)

	Di seguito è definita la macchina Locks, che include la macchina Doors e ne "eredita", per così dire,
	tutti i requisiti, compreso l'onere di garantirne l'invariante.
	

	
	La sintassi degli operatori utilizzati per questa macchina è la seguente:

	:		appartenenza
	~		relazione inversa
	<->		relazione
	S<:T		sottoinsieme di
	=		uguaglianza
	:=		assegnamento
	& 		congiunzione


*/


MACHINE Locks

INCLUDES Doors

PROMOTES closedoor

SETS STATUS = {locked, unlocked}

VARIABLES status

INVARIANT status : DOOR <-> STATUS & pos~[{open}]<:status~[{unlocked}]

INITIALISATION status := DOOR * {locked}

OPERATIONS
	opendoor   (dd) = PRE  dd:DOOR & status(dd) = unlocked
			  THEN opening(dd)
			  END;
	unlockdoor (dd) = PRE  dd:DOOR
 			  THEN status(dd) := unlocked
			  END;
	lockdoor   (dd) = PRE  dd:DOOR & pos(dd) = closed
			  THEN status(dd) := locked
			  END
					  
END
