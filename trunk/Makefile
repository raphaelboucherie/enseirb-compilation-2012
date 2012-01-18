LEX=lex
YACC=yacc
CFLAGS=-Wall -ggdb
CC=gcc

all:frontend backend

frontend:frontend.c table.c scanner_frontend.c
	$(CC) $(CFLAGS) -lm -o $@ $^ 

backend:backend.c scanner_backend.c
	$(CC) $(CFLAGS) -o $@ $^

frontend.c:frontend.y 
	$(YACC) -o $@ --defines=frontend.tab.h $^

backend.c:backend.y
	$(YACC) -o $@ --defines=backend.tab.h $^

%.c:%.l
	$(LEX) -o $@ $^

clean:
	rm -f frontend.c backend.c
save:frontend.y table.c table.h
	cp frontend.c frontend.save
	cp table.c table_c.save
	cp table.h table_h.save	