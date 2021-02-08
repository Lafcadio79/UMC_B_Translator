MACHINE Ticket

VARIABLES serve, next

INVARIANT serve : 0..100000 & next : 0..100000 & serve <= next

INITIALISATION serve, next := 0, 0

OPERATIONS
   serve_next = 
	PRE serve < next 
	THEN serve, next := serve + 1, serve + 1 
	END;
   take_ticket = 
	PRE serve <= next 
	THEN serve, next := next, next + 1 
	END
END
