all:
	bison --yacc --defines --output=parser.c parser.y
	flex --outfile=scanner.c scanner.l
	gcc -o calc scanner.c parser.c

clean:
	rm -rf *.c
	rm -rf *.output
	rm -rf calc
