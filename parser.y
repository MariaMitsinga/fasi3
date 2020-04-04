%{
	#include <stdio.h>
	#include <stdlib.h>
	#include <string.h>
	#include <assert.h>	
	#include "SymTable.h"
	
	int yyerror (char* yaccProvidedMessage);
	int yylex (void);
	
	extern int yylineno;
	extern char * yyval;
	extern char * yytext;
	extern FILE * yyin;
	extern FILE * yyout;
	
	int scope=0;
	int numname=0;
%}

%start program


%token	id
%token	NUMBER
%token	FLOAT		
%token	STRING			
%token	NEWLINE
%token	NEWTAB
%token	IF              
%token	ELSE            
%token	WHILE           
%token	FOR            
%token	FUNCTION       
%token	RETURN          
%token	BREAK          
%token	CONTINUE        
%token	AND     
%token	NOT             
%token	OR          
%token	LOCAL      
%token	TRUE       
%token	FALSE         
%token	NIL         
%token 	SPACE
%token	EQUAL
%token	PLUS		
%token	MINUS		
%token	MULTIPLE	
%token	FORWARD_SLASH	
%token	BACKWARD_SLASH	
%token	PERCENT		
%token	DOUBLE_EQUAL	
%token	NOT_EQUAL	
%token	DOUBLE_PLUS	
%token	DOUBLE_MINUS	
%token	GREATER		
%token	LESS	
%token	GREATER_EQUAL	
%token	LESS_EQUAL	
%token	LEFT_CURLY_BRACKET	
%token	RIGHT_CURLY_BRACKET     
%token	LEFT_SQUARE_BRACKET	
%token	RIGHT_SQUARE_BRACKET
%token	LEFT_PARENTHESES	
%token	RIGHT_PARENTHESES	
%token	SEMI_COLON		
%token	COMMA		
%token	COLON		
%token	NAMESPACE_ALIAS_QUALIFIER 
%token	DOT			
%token	DOUBLE_DOT	
%token	LINE_COMMENT 	
%token	MULTI_COMMENT 	
%token	CARRIAGE_RETURN	
%token	OTHER

%right	EQUAL
%left	OR
%left	AND
%nonassoc	DOUBLE_EQUAL NOT_EQUAL
%nonassoc	GREATER GREATER_EQUAL LESS LESS_EQUAL
%left	PLUS MINUS
%left	MULTIPLE FORWARD_SLASH PERCENT
%right	NOT DOUBLE_PLUS DOUBLE_MINUS UMINUS
%left	DOT DOUBLE_DOT
%left	LEFT_SQUARE_BRACKET RIGHT_SQUARE_BRACKET
%left	LEFT_PARENTHESES RIGHT_PARENTHESES


%union
{
	char* strVal;
	int intVal;
	double dbVal;
	struct SymTableEntry* tmpnode;
}

%type <tmpnode> lvalue

%%

program:	stamt {fprintf(yyout," program ==> stmt \n");}
		;

stamt:		stmt stamt {fprintf(yyout," stamt ==> stmt stamt\n");}
		| /* empty*/ {fprintf(yyout,"stamt ==> empty \n");}
		;

stmt:		expr SEMI_COLON {fprintf(yyout," stmt ==> expr ;\n");}
		|ifstmt	{fprintf(yyout," stmt ==> ifstmt ;\n");}
		|whilestmt {fprintf(yyout," stmt ==> whilestmt ;\n");}
		|forstmt {fprintf(yyout," stmt ==> forstmt ;\n");}
		|returnstmt {fprintf(yyout," stmt ==> returnstmt ;\n");}
		|BREAK SEMI_COLON {fprintf(yyout," stmt ==> break; \n");}
		|CONTINUE SEMI_COLON {fprintf(yyout," stmt ==> break; \n");}
		|block {fprintf(yyout," stmt ==> {} \n");}
		|funcdef {fprintf(yyout," stmt ==> funcdef \n");}
		|SEMI_COLON {fprintf(yyout," stmt ==> ; \n");}
		;

expr:		assgnexpr {fprintf(yyout," expr ==> assgnexpr \n");}
		|expr PLUS expr {fprintf(yyout," expr ==> expr + expr \n");}
		|expr MINUS expr {fprintf(yyout," expr ==> expr - expr \n");}	
		|expr MULTIPLE expr {fprintf(yyout," expr ==> expr * expr \n");}
		|expr FORWARD_SLASH expr {fprintf(yyout," expr ==> expr / expr \n");}
		|expr PERCENT expr {fprintf(yyout," expr ==> expr % expr \n");}
		|expr GREATER expr {fprintf(yyout," expr ==> expr > expr \n");}
		|expr GREATER_EQUAL expr {fprintf(yyout," expr ==> expr >= expr \n");}
		|expr LESS  expr {fprintf(yyout," expr ==> expr < expr \n");}
		|expr LESS_EQUAL expr {fprintf(yyout," expr ==> expr <= expr \n");}
		|expr DOUBLE_EQUAL expr {fprintf(yyout," expr ==> expr == expr \n");}
		|expr NOT_EQUAL expr {fprintf(yyout," expr ==> expr != expr \n");}
		|expr AND expr {fprintf(yyout," expr ==> expr && expr \n");}
		|expr OR expr {fprintf(yyout," expr ==> expr || expr \n");}
		| term {fprintf(yyout," expr ==> term \n");}
		;

term:		LEFT_PARENTHESES expr RIGHT_PARENTHESES {fprintf(yyout," term ==> (expr) \n");}
		| MINUS expr %prec UMINUS {fprintf(yyout," term ==> -expr \n");}
		| NOT expr {fprintf(yyout," term ==> !expr \n");}
		| DOUBLE_PLUS lvalue 	{ if($2!=NULL){
					  if(strcmp($2->type,"user function")==0 || strcmp("library function", $2->type)==0)
					  fprintf(yyout,"\n\nERROR: value is a function so we cannot assigned: %s in line: %d\n\n",$2->name,yylineno);}
					  fprintf(yyout," term ==> ++lvalue \n");}
		| lvalue DOUBLE_PLUS	{ if($1!=NULL){
					  if(strcmp($1->type,"user function")==0 || strcmp("library function", $1->type)==0)
					  fprintf(yyout,"\n\nERROR: value is a function so we cannot assigned: %s in line: %d\n\n",$1->name,yylineno);}
					  fprintf(yyout," term ==> lvalue++ \n");}
		| DOUBLE_MINUS lvalue	{ if($2!=NULL){
					  if(strcmp($2->type,"user function")==0 || strcmp("library function", $2->type)==0)
					  fprintf(yyout,"\n\nERROR: value is a function so we cannot assigned: %s in line: %d\n\n",$2->name,yylineno);}
					  fprintf(yyout," term ==> --lvalue \n");}
		| lvalue DOUBLE_MINUS	{ if($1!=NULL){
					  if(strcmp($1->type,"user function")==0 || strcmp("library function", $1->type)==0)
					  fprintf(yyout,"\n\nERROR: value is a function so we cannot assigned: %s in line: %d\n\n",$1->name,yylineno);}
					  fprintf(yyout," term ==> lvalue-- \n");}
		| primary {fprintf(yyout," term ==> primary \n");}
		;

assgnexpr:	lvalue EQUAL expr {	//fprintf(yyout,"\n\nlvalue:%d\n\n",$1);
					if($1!=NULL){
					if(strcmp($1->type,"user function")==0 || strcmp("library function", $1->type)==0)
					fprintf(yyout,"\n\nERROR: value is a function so we cannot assigned %s in line %d\n\n",$1->name,yylineno);}
					fprintf(yyout," assgnexpr ==> Ivalue=expr \n");}
		;

primary:  	lvalue	{fprintf(yyout," primary ==> Ivalue \n");}
		| call {fprintf(yyout," primary ==> call \n");}
		| objectdef {fprintf(yyout," primary ==> objectdef \n");}
		| LEFT_PARENTHESES funcdef RIGHT_PARENTHESES {fprintf(yyout," primary ==> (funcdef) \n");}
		| const {fprintf(yyout," primary ==> const \n");}
	 	;

lvalue:		id	{
				fprintf(yyout," lvalue ==> id \n");
				int i=scope,j,flag=0;
				struct SymTableEntry *tmp,*tmp2;	
				for(i=scope;i>-1;i--)
				{
					tmp=NameLookUpInScope(ScopeTable,i,yytext);
					$$=tmp;
					if(tmp!=NULL) //ean brethke kati me idio onoma
					{	
						if( ( strcmp("global variable", tmp->type)==0 || strcmp("local variable", tmp->type)==0 || strcmp("formal argument", tmp->type)==0 ) && i!=scope && i!=0)//ean afora metablhth tote psaxnw gia thn lathos periptwsh
						{
							for(j=scope-1;j>=i;j--)
							{
								tmp2=ScopeTable->head[j];
								while(tmp2!=NULL)
								{
									if(strcmp("user function", tmp2->type)==0 && tmp2->isActive==1)
									{
										flag=1;
										fprintf(yyout,"\n\nERROR: Can not access %s in line %d\n\n",tmp->name,yylineno);
										break;
									}
									tmp2=tmp2->nextScopeList;
								}
								if(flag==1)
									break;
							}
						}
						break;
					}
				}
				if(i==-1)
				{
					if(scope==0)
						$$=insertNodeToHash(Head,yytext,"global variable",scope,yylineno,1);
					else
						$$=insertNodeToHash(Head,yytext,"local variable",scope,yylineno,1);
				}
			}
		| LOCAL id	{	
					fprintf(yyout," Ivalue ==> local \n");
					struct SymTableEntry *tmp=NameLookUpInScope(ScopeTable,scope,yytext);
					$$=tmp;
					if(tmp==NULL && collisionLibFun(ScopeTable,yytext)==1)
						fprintf(yyout,"\n\nERROR: local %s: Trying to shadow Library Function in line %d\n\n",yytext,yylineno);
					if(tmp==NULL && collisionLibFun(ScopeTable,yytext)==0)
					{
						if(scope==0)
							$$=insertNodeToHash(Head,yytext,"global variable",scope,yylineno,1);
						else
							$$=insertNodeToHash(Head,yytext,"local variable",scope,yylineno,1);

					}

				}
		| NAMESPACE_ALIAS_QUALIFIER id	{
							fprintf(yyout," Ivalue ==> ::id \n");
							struct SymTableEntry *tmp=NameLookUpInScope(ScopeTable,0,yytext);
							$$=tmp;
							if(tmp==NULL)
								printf("\n\nERROR: There is no member on global scope with the name %s in Line %d\n\n", yytext,yylineno);
						}
		| member	{fprintf(yyout," Ivalue ==> member \n");}
		;

member:		lvalue DOT id	{fprintf(yyout," Member ==> .id \n");}
		| lvalue LEFT_SQUARE_BRACKET expr RIGHT_SQUARE_BRACKET	{fprintf(yyout," Member ==> [expr] \n");}
		| call DOT id	{fprintf(yyout," Member ==> call.id \n");}
		| call LEFT_SQUARE_BRACKET expr RIGHT_SQUARE_BRACKET {fprintf(yyout," Member ==> call[expr] \n");}
		;

call:		call LEFT_PARENTHESES elist RIGHT_PARENTHESES	{fprintf(yyout," CALL ==> call(elist) \n");}
		| lvalue callsuffix	{fprintf(yyout," call ==> ivalue callsuffix \n");} 
		| LEFT_PARENTHESES funcdef RIGHT_PARENTHESES LEFT_PARENTHESES elist RIGHT_PARENTHESES {fprintf(yyout," call ==> (funcdef)(elist) \n");}
		;

callsuffix:	normcall	{fprintf(yyout," callsuffix ==> normcall \n");}
		| methodcall	{fprintf(yyout," callsuffix ==> methodcall \n");}
		;

normcall:	LEFT_PARENTHESES elist RIGHT_PARENTHESES {fprintf(yyout," normcall ==> (elist) \n");}
		;	

methodcall:	DOUBLE_DOT id LEFT_PARENTHESES elist RIGHT_PARENTHESES {fprintf(yyout," methodcall ==> ..id(elist) \n");}
		;

elist:	 	expr elist1	{fprintf(yyout," elist ==> expr elist1 \n");}
		| /* empty */	{fprintf(yyout," elist ==>  \n");}
		;

elist1:		COMMA expr elist1	{fprintf(yyout," elist ==> , expr elist1 \n");}
		| /* empty */	{fprintf(yyout," elist ==>  \n");}
		;

objectdef:	LEFT_SQUARE_BRACKET elist RIGHT_SQUARE_BRACKET	{fprintf(yyout," objectdef ==> [elist] \n");}
		| LEFT_SQUARE_BRACKET indexed RIGHT_SQUARE_BRACKET {fprintf(yyout," objectdef ==> [indexed] \n");}
		;

indexed:	indexedelem indexed1	{fprintf(yyout," indexed ==> indexedelem indexed1 \n");}
		;

indexed1: 	COMMA indexedelem indexed1	{fprintf(yyout," indexed ==> indexedelem indexed1 \n");}
		| /* empty */	{fprintf(yyout," indexed ==>   \n");}
		;

indexedelem: 	LEFT_CURLY_BRACKET expr COLON expr RIGHT_CURLY_BRACKET	{fprintf(yyout," indexedelem ==> { expr : expr } \n");}
		;

block:		LEFT_CURLY_BRACKET {scope++; } stamt RIGHT_CURLY_BRACKET {	Hide(ScopeTable,scope);
										scope--; 
										fprintf(yyout," block ==> { [stmt] } \n");}
		;

funcdef: 	FUNCTION {
			char* name=(char *)malloc(sizeof(char));
		 	char* num=(char *)malloc(sizeof(char));
			sprintf(name, "%s", "$f");
			sprintf(num, "%d", numname);			
			strcat(name,num);			
			insertNodeToHash(Head,name,"user function",scope,yylineno,1);
			free(name);
			free(num);
			numname++;
			//scope++;	
		}
		LEFT_PARENTHESES {scope++;} idlist RIGHT_PARENTHESES {scope--;} block	{fprintf(yyout," funcdef ==> function(){} \n");}
		|FUNCTION id {
				struct SymTableEntry *tmp;
				tmp=NameLookUpInScope(ScopeTable,scope,yytext);
				if(tmp!=NULL && strcmp("library function", tmp->type)!=0 )
					fprintf(yyout,"\n\nERROR: name %s already exists in same scope in line %d\n\n",yytext,yylineno);
				if(collisionLibFun(ScopeTable,yytext)==1)
					fprintf(yyout,"\n\nERROR: function %s: Trying to shadow Library Function in line %d\n\n",yytext,yylineno);
				else if (tmp==NULL && collisionLibFun(ScopeTable,yytext)==0)
					insertNodeToHash(Head,yytext,"user function",scope,yylineno,1);
			      } LEFT_PARENTHESES {scope++;} idlist RIGHT_PARENTHESES {scope--;} block	{fprintf(yyout," funcdef ==> function id(){} \n");}
		;

const:		NUMBER {fprintf(yyout," const ==> number \n");}
		| STRING {fprintf(yyout," const ==> string \n");}
		| NIL {fprintf(yyout," const ==> nil \n");}
		| TRUE {fprintf(yyout," const ==> true \n");}
		| FALSE {fprintf(yyout," const ==> false \n");}
		| FLOAT {fprintf(yyout," const ==> float \n");}
		;

idlist:		id {
		   	insertArgToNode(ScopeTable,yytext,scope);
			struct SymTableEntry *tmp;
			tmp=NameLookUpInScope(ScopeTable,scope,yytext);
			
			if(tmp!=NULL && strcmp("library function", tmp->type)!=0)
				fprintf(yyout,"\n\nERROR: name %s: formal redeclaration in line %d\n\n",yytext,yylineno);
			if(collisionLibFun(ScopeTable,yytext)==1)
				fprintf(yyout,"\n\nERROR: function %s: Trying to shadow Library Function in line %d\n\n",yytext,yylineno);
			else if (tmp==NULL && collisionLibFun(ScopeTable,yytext)==0)
				insertNodeToHash(Head,yytext,"formal argument",scope,yylineno,1);
			 
		   } idlist1	{fprintf(yyout, " idlist ==> id,id*;\n");}
		| /* empty */	{fprintf(yyout, " idlist ==> \n");}
		;	

idlist1:	COMMA id {
		   	insertArgToNode(ScopeTable,yytext,scope);
			struct SymTableEntry *tmp;
			tmp=NameLookUpInScope(ScopeTable,scope,yytext);
			if(tmp!=NULL && strcmp("library function", tmp->type)!=0)
				fprintf(yyout,"\n\nERROR: name %s: formal redeclaration in line %d\n\n",yytext,yylineno);
			if(collisionLibFun(ScopeTable,yytext)==1)
				fprintf(yyout,"\n\nERROR: function %s: Trying to shadow Library Function in line %d\n\n",yytext,yylineno);
			else if (tmp==NULL && collisionLibFun(ScopeTable,yytext)==0)
				insertNodeToHash(Head,yytext,"formal argument",scope,yylineno,1);
			 
		   } idlist1	{fprintf(yyout, " idlist ==> id,id*;\n");}
		| /* empty */	{fprintf(yyout, " idlist ==>   \n");}
		;

ifstmt:		IF LEFT_PARENTHESES expr RIGHT_PARENTHESES stmt			{fprintf(yyout, " ifstmt ==> IF THEN;\n");}
		| IF LEFT_PARENTHESES expr RIGHT_PARENTHESES stmt ELSE stmt	{fprintf(yyout, " ifstmt ==> IF THEN ELSE;\n");}
		;

whilestmt :	WHILE LEFT_PARENTHESES expr RIGHT_PARENTHESES stmt {fprintf(yyout," whilestmt==> while(expr) stmt \n");}
		;

forstmt:	FOR LEFT_PARENTHESES elist SEMI_COLON expr SEMI_COLON elist RIGHT_PARENTHESES stmt {fprintf(yyout," forstmt ==> (elist;expr;elist)stmt \n");}
		;

returnstmt:	RETURN SEMI_COLON {fprintf(yyout," returnstmt ==> return ;\n");}
		| RETURN expr SEMI_COLON {fprintf(yyout," returnstmt ==> return expr;\n");}
		;

%%

int yyerror (char* yaccProvidedMessage)
{
	fprintf(stderr, "%s: at line %d, before token: '%s'\n", yaccProvidedMessage, yylineno, yytext);
	fprintf(stderr, "INPUT NOT VALID\n");
}

int main(int argc, char** argv)
{
	if(argc > 1)
	{
		if(!(yyin = fopen(argv[1],"r")))
		{
			fprintf(stderr,"Cannot read file: %s\n",argv[1]);
			return 0;
		}
	}
	else 
	{
		printf("Give an input from command line\n");
		return 0;	
	}
	
	Head=SymTable_new(509);
	ScopeTable=SymTable_new(100);
	insertNodeToHash(Head,"print","library function",0,0,1);
	insertNodeToHash(Head,"input","library function",0,0,1);
    	insertNodeToHash(Head,"objectmemberkeys","library function",0,0,1);
	insertNodeToHash(Head,"objecttotalmembers","library function",0,0,1);
    	insertNodeToHash(Head,"objectcopy","library function",0,0,1);
    	insertNodeToHash(Head,"totalarguments","library function",0,0,1);
	insertNodeToHash(Head,"arguments","library function",0,0,1);
    	insertNodeToHash(Head,"typeof","library function",0,0,1);
    	insertNodeToHash(Head,"stronum","library function",0,0,1);
    	insertNodeToHash(Head,"sqrt","library function",0,0,1);
    	insertNodeToHash(Head,"cos","library function",0,0,1);
    	insertNodeToHash(Head,"sin","library function",0,0,1);
    	
	yyparse();
	printf("\n\n");
	printScopeTable(ScopeTable);
	return 0;
}
