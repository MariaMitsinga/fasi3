%{
	#include <stdio.h>
	#include <stdlib.h>
	#include <string.h>
	#include <assert.h>
	#include "SymTable.h"
	#include "quads.h"

	int yyerror (char* yaccProvidedMessage);
	int yylex (void);
	
	extern counter;
	extern int yylineno;
	extern char * yyval;
	extern char * yytext;
	extern FILE * yyin;
	extern FILE * yyout;
	
	char* krifi;
	int i=0;
	int scope=0;
	int numname=0;
	int offset_arg=0;
	int infunction=0;
%}

%start program


%token	<strVal> STRING
%token	<uncharVal> FALSE
%token	<uncharVal> TRUE
%token	<strVal> NIL
%token	<dbVal> FLOAT
%token	<strVal> id
%token  <intVal> NUMBER			
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
	unsigned char uncharVal;
	int intVal;
	double dbVal;
	struct SymTableEntry* tmpnode;
	struct expr* expr;
	struct call* call;
}

%type <expr> lvalue
%type <expr> member
%type <expr> primary
%type <expr> assgnexpr
%type <expr> call
%type <expr> term
%type <expr> objectdef
%type <expr> const
%type <expr> expr
%type <expr> elist
%type <expr> elist1
%type <expr> indexedelem
%type <expr> indexed
%type <expr> indexed1

%type <call> callsuffix
%type <call> normcall
%type <call> methodcall

%%

program:	stamt {fprintf(yyout," program ==> stmt \n");}
		;

stamt:		stmt stamt {fprintf(yyout," stamt ==> stmt stamt\n");}
		| /* empty*/ {fprintf(yyout,"stamt ==> empty \n");}
		;

stmt:		expr SEMI_COLON {counter=0; fprintf(yyout," stmt ==> expr; \n");}
		|ifstmt	{fprintf(yyout," stmt ==> ifstmt \n");}
		|whilestmt {fprintf(yyout," stmt ==> whilestmt \n");}
		|forstmt {fprintf(yyout," stmt ==> forstmt \n");}
		|returnstmt {fprintf(yyout," stmt ==> returnstmt \n");}
		|BREAK SEMI_COLON {fprintf(yyout," stmt ==> break; \n");}
		|CONTINUE SEMI_COLON {fprintf(yyout," stmt ==> continue; \n");}
		|block {fprintf(yyout," stmt ==> {} \n");}
		|funcdef {fprintf(yyout," stmt ==> funcdef \n");}
		|SEMI_COLON {fprintf(yyout," stmt ==> ; \n");}
		;

expr:		assgnexpr {
			$$=newexpr(arithexpr_e);
			if(funcounter>0){
				$$->sym=CreateSecretVar(counter, scope, yylineno,funcounter,functionoffset,"function locals");
			}else{
				$$->sym=CreateSecretVar(counter, scope, yylineno,funcounter,functionoffset,"program variables");
			}
			counter++;
			$$->numConst=$1->numConst;
			addquad(tablecounter,assign,$$,$1,NULL,-1,yylineno);
			tablecounter++;
			fprintf(yyout," expr ==> assgnexpr \n");
		}
		|expr PLUS expr {
			$$=newexpr(arithexpr_e);
			if(funcounter>0){
				$$->sym=CreateSecretVar(counter, scope, yylineno,funcounter,functionoffset,"function locals");
			}else{
				$$->sym=CreateSecretVar(counter, scope, yylineno,funcounter,functionoffset,"program variables");
			}
			//counter++;
			$$->numConst=$1->numConst + $3->numConst;
			addquad(tablecounter,add,$$,$1,$3,-1,yylineno);
			tablecounter++;
			fprintf(yyout," expr ==> expr + expr \n");
		}
		|expr MINUS expr {//counter=CreateSecretVar(counter, scope, yylineno);
			$$=newexpr(arithexpr_e);
			if(funcounter>0){
				$$->sym=CreateSecretVar(counter, scope, yylineno,funcounter,functionoffset,"function locals");
			}else{
				$$->sym=CreateSecretVar(counter, scope, yylineno,funcounter,functionoffset,"program variables");
			}
			$$->numConst=$1->numConst - $3->numConst;
			addquad(tablecounter,sub,$$,$1,$3,-1,yylineno);
			tablecounter++;
			fprintf(yyout," expr ==> expr - expr \n");}	
		|expr MULTIPLE expr {//counter=CreateSecretVar(counter, scope, yylineno);
			$$=newexpr(arithexpr_e);
			if(funcounter>0){
				$$->sym=CreateSecretVar(counter, scope, yylineno,funcounter,functionoffset,"function locals");
			}else{
				$$->sym=CreateSecretVar(counter, scope, yylineno,funcounter,functionoffset,"program variables");
			}
			$$->numConst=$1->numConst * $3->numConst;
			addquad(tablecounter,mul,$$,$1,$3,-1,yylineno);
			tablecounter++;
			fprintf(yyout," expr ==> expr * expr \n");}
		|expr FORWARD_SLASH expr {//counter=CreateSecretVar(counter, scope, yylineno);
			$$=newexpr(arithexpr_e);
			if(funcounter>0){
				$$->sym=CreateSecretVar(counter, scope, yylineno,funcounter,functionoffset,"function locals");
			}else{
				$$->sym=CreateSecretVar(counter, scope, yylineno,funcounter,functionoffset,"program variables");
			}
			$$->numConst=$1->numConst / $3->numConst;
			addquad(tablecounter,DIV,$$,$1,$3,-1,yylineno);
			tablecounter++;
			fprintf(yyout," expr ==> expr / expr \n");}
		|expr PERCENT expr {//counter=CreateSecretVar(counter, scope, yylineno);
			$$=newexpr(arithexpr_e);
			if(funcounter>0){
				$$->sym=CreateSecretVar(counter, scope, yylineno,funcounter,functionoffset,"function locals");
			}else{
				$$->sym=CreateSecretVar(counter, scope, yylineno,funcounter,functionoffset,"program variables");
			}
			$$->numConst=(int)($1->numConst) % (int)($3->numConst);
			addquad(tablecounter,mod,$$,$1,$3,-1,yylineno);
			tablecounter++;
			fprintf(yyout," expr ==> expr % expr \n");}
		|expr GREATER expr { 
			$$=newexpr(boolexpr_e);
			if(funcounter>0){
				$$->sym=CreateSecretVar(counter, scope, yylineno,funcounter,functionoffset,"function locals");
			}else{
				$$->sym=CreateSecretVar(counter, scope, yylineno,funcounter,functionoffset,"program variables");
			}
			$$->numConst=$1->numConst > $3->numConst;
			addquad(tablecounter,if_greater,$$,$1,$3,-1,yylineno);
			tablecounter++;
			fprintf(yyout," expr ==> expr > expr \n");}
		|expr GREATER_EQUAL expr { 
			$$=newexpr(boolexpr_e);
			if(funcounter>0){
				$$->sym=CreateSecretVar(counter, scope, yylineno,funcounter,functionoffset,"function locals");
			}else{
				$$->sym=CreateSecretVar(counter, scope, yylineno,funcounter,functionoffset,"program variables");
			}
			$$->numConst=$1->numConst >= $3->numConst;
			addquad(tablecounter,if_greatereq,$$,$1,$3,-1,yylineno);
			tablecounter++;
			fprintf(yyout," expr ==> expr >= expr \n");}
		|expr LESS  expr { 
			$$=newexpr(boolexpr_e);
			if(funcounter>0){
				$$->sym=CreateSecretVar(counter, scope, yylineno,funcounter,functionoffset,"function locals");
			}else{
				$$->sym=CreateSecretVar(counter, scope, yylineno,funcounter,functionoffset,"program variables");
			}
			$$->numConst=$1->numConst < $3->numConst;
			addquad(tablecounter,if_less,$$,$1,$3,-1,yylineno);
			tablecounter++;
			fprintf(yyout," expr ==> expr < expr \n");}
		|expr LESS_EQUAL expr { 
			$$=newexpr(boolexpr_e);
			if(funcounter>0){
				$$->sym=CreateSecretVar(counter, scope, yylineno,funcounter,functionoffset,"function locals");
			}else{
				$$->sym=CreateSecretVar(counter, scope, yylineno,funcounter,functionoffset,"program variables");
			}
			$$->numConst=$1->numConst <= $3->numConst;
			addquad(tablecounter,if_lesseq,$$,$1,$3,-1,yylineno);
			tablecounter++;
			fprintf(yyout," expr ==> expr <= expr \n");}
		|expr DOUBLE_EQUAL expr { 
			$$=newexpr(boolexpr_e);
			if(funcounter>0){
				$$->sym=CreateSecretVar(counter, scope, yylineno,funcounter,functionoffset,"function locals");
			}else{
				$$->sym=CreateSecretVar(counter, scope, yylineno,funcounter,functionoffset,"program variables");
			}
			if ($1->numConst == $3->numConst){
				$$->numConst=1;
			}
			else $$->numConst=0;
			addquad(tablecounter,if_eq,$$,$1,$3,-1,yylineno);
			tablecounter++;
			fprintf(yyout," expr ==> expr == expr \n");}
		|expr NOT_EQUAL expr { 
			$$=newexpr(boolexpr_e);
			if(funcounter>0){
				$$->sym=CreateSecretVar(counter, scope, yylineno,funcounter,functionoffset,"function locals");
			}else{
				$$->sym=CreateSecretVar(counter, scope, yylineno,funcounter,functionoffset,"program variables");
			}
			if ($1->numConst != $3->numConst){
				$$->numConst=1;
			}
			else $$->numConst=0;
			addquad(tablecounter,if_noteq,$$,$1,$3,-1,yylineno);
			tablecounter++;
			fprintf(yyout," expr ==> expr != expr \n");}
		|expr AND expr { 
			$$=newexpr(boolexpr_e);
			if(funcounter>0){
				$$->sym=CreateSecretVar(counter, scope, yylineno,funcounter,functionoffset,"function locals");
			}else{
				$$->sym=CreateSecretVar(counter, scope, yylineno,funcounter,functionoffset,"program variables");
			}
			$$->numConst=$1->numConst && $3->numConst;
			addquad(tablecounter,and,$$,$1,$3,-1,yylineno);
			tablecounter++;
			fprintf(yyout," expr ==> expr && expr \n");}
		|expr OR expr { 
			$$=newexpr(boolexpr_e);
			if(funcounter>0){
				$$->sym=CreateSecretVar(counter, scope, yylineno,funcounter,functionoffset,"function locals");
			}else{
				$$->sym=CreateSecretVar(counter, scope, yylineno,funcounter,functionoffset,"program variables");
			}
			$$->numConst=$1->numConst || $3->numConst;
			addquad(tablecounter,or,$$,$1,$3,-1,yylineno);
			tablecounter++;
			fprintf(yyout," expr ==> expr || expr \n");}
		| term { fprintf(yyout," expr ==> term \n");}
		;

term:		LEFT_PARENTHESES expr RIGHT_PARENTHESES {$$=$2;fprintf(yyout," term ==> (expr) \n");}
		| MINUS expr %prec UMINUS {
						istempexpr($2);
						$$=newexpr(arithexpr_e);
						if(funcounter>0){
							$$->sym= istempexpr($2) ? $2->sym : CreateSecretVar(counter, scope, yylineno,funcounter,functionoffset,"function locals");
						}else{
							$$->sym= istempexpr($2) ? $2->sym : CreateSecretVar(counter, scope, yylineno,funcounter,functionoffset,"program variables");
						}
						addquad(tablecounter,uminus,$$,$2,NULL,-1,yylineno);
						tablecounter++;
						fprintf(yyout," term ==> -expr \n");}
		| NOT expr {fprintf(yyout," term ==> !expr \n");}
		| DOUBLE_PLUS lvalue 	{ if($2->sym!=NULL){
						struct expr* tmp,*num;
						tmp=newexpr(arithexpr_e);
						if(funcounter>0){
							tmp->sym=CreateSecretVar(counter, scope, yylineno,funcounter,functionoffset,"function locals");
						}else{
							tmp->sym=CreateSecretVar(counter, scope, yylineno,funcounter,functionoffset,"program variables");
						}
						//tmp->numConst=$2->numConst; prepei na alaxtei ama xreiazonantai kai oi times
						addquad(tablecounter,assign,tmp,$2,NULL,-1,yylineno);
						tablecounter++;
						num=newexpr(arithexpr_e);
						num->numConst=1;
						addquad(tablecounter,add,$2,$2,num,-1,yylineno);
						tablecounter++;
					  	if(strcmp($2->sym->type,"user function")==0 || strcmp("library function", $2->sym->type)==0)
					  		fprintf(yyout,"\n\nERROR: value is a function so we cannot assigned: %s in line: %d\n\n",$2->sym->name,yylineno);
						}
					  fprintf(yyout," term ==> ++lvalue \n");}
		| lvalue DOUBLE_PLUS	{ if($1->sym!=NULL){
						struct expr* tmp,*num;
						tmp=newexpr(arithexpr_e);
						if(funcounter>0){
							tmp->sym=CreateSecretVar(counter, scope, yylineno,funcounter,functionoffset,"function locals");
						}else{
							tmp->sym=CreateSecretVar(counter, scope, yylineno,funcounter,functionoffset,"program variables");
						}
						//tmp->numConst=$1->numConst; prepei na alaxtei ama xreiazonantai kai oi times
						addquad(tablecounter,assign,tmp,$1,NULL,-1,yylineno);
						tablecounter++;
						num=newexpr(arithexpr_e);
						num->numConst=1;
						addquad(tablecounter,add,$1,$1,num,-1,yylineno);
						tablecounter++;
					  	if(strcmp($1->sym->type,"user function")==0 || strcmp("library function", $1->sym->type)==0)
					  		fprintf(yyout,"\n\nERROR: value is a function so we cannot assigned: %s in line: %d\n\n",$1->sym->name,yylineno);
						}
					  fprintf(yyout," term ==> lvalue++ \n");}
		| DOUBLE_MINUS lvalue	{ if($2->sym!=NULL){
						struct expr* tmp,*num;
						tmp=newexpr(arithexpr_e);
						if(funcounter>0){
							tmp->sym=CreateSecretVar(counter, scope, yylineno,funcounter,functionoffset,"function locals");
						}else{
							tmp->sym=CreateSecretVar(counter, scope, yylineno,funcounter,functionoffset,"program variables");
						}
						//tmp->numConst=$2->numConst; prepei na alaxtei ama xreiazonantai kai oi times
						addquad(tablecounter,assign,tmp,$2,NULL,-1,yylineno);
						tablecounter++;
						num=newexpr(arithexpr_e);
						num->numConst=1;
						addquad(tablecounter,sub,$2,$2,num,-1,yylineno);
						tablecounter++;
					  	if(strcmp($2->sym->type,"user function")==0 || strcmp("library function", $2->sym->type)==0)
					  		fprintf(yyout,"\n\nERROR: value is a function so we cannot assigned: %s in line: %d\n\n",$2->sym->name,yylineno);
						}
					  fprintf(yyout," term ==> --lvalue \n");}
		| lvalue DOUBLE_MINUS	{ if($1->sym!=NULL){
						struct expr* tmp,*num;
						tmp=newexpr(arithexpr_e);
						if(funcounter>0){
							tmp->sym=CreateSecretVar(counter, scope, yylineno,funcounter,functionoffset,"function locals");
						}else{
							tmp->sym=CreateSecretVar(counter, scope, yylineno,funcounter,functionoffset,"program variables");
						}
						//tmp->numConst=$1->numConst; prepei na alaxtei ama xreiazonantai kai oi times
						addquad(tablecounter,assign,tmp,$1,NULL,-1,yylineno);
						tablecounter++;
						num=newexpr(arithexpr_e);
						num->numConst=1;
						addquad(tablecounter,sub,$1,$1,num,-1,yylineno);
						tablecounter++;
					  	if(strcmp($1->sym->type,"user function")==0 || strcmp("library function", $1->sym->type)==0)
					 		fprintf(yyout,"\n\nERROR: value is a function so we cannot assigned: %s in line: %d\n\n",$1->sym->name,yylineno);
						}
					  fprintf(yyout," term ==> lvalue-- \n");}
		| primary {fprintf(yyout," term ==> primary \n");}
		;

assgnexpr:	lvalue EQUAL expr {	
					if($1->type==tableitem_e){
						fprintf(yyout," bhke sto ==\n");
						emit(tablesetelem,$1->index,$3,$1,-1,yylineno);
						//$$=emit_iftableitem($$)
						if(funcounter>0)
							$$=emit_iftableitem($1,counter,scope,yylineno,funcounter,functionoffset,"function locals");
						else
							$$=emit_iftableitem($1,counter,scope,yylineno,funcounter,functionoffset,"program variables");
						$$->type=assignexpr_e;
					}else if($1->type!=tableitem_e){
						fprintf(yyout," bhke sto !=\n");

						addquad(tablecounter,assign,$1,$3,NULL,-1,yylineno);
						$$=$1;
						tablecounter++;
					}
					
					if($1->sym!=NULL){
					if(strcmp($1->sym->type,"user function")==0 || strcmp("library function", $1->sym->type)==0)
					fprintf(yyout,"\n\nERROR: value is a function so we cannot assigned %s in line %d\n\n",$1->sym->name,yylineno);}
					fprintf(yyout," assgnexpr ==> Ivalue=expr \n");
		};

primary:  	lvalue	{
				//fprintf(yyout," $1->name:%s \n",$1->sym->name);
				if(funcounter>0)
					$$=emit_iftableitem($1,counter,scope,yylineno,funcounter,functionoffset,"function locals");
				else
					$$=emit_iftableitem($1,counter,scope,yylineno,funcounter,functionoffset,"program variables");
				//fprintf(yyout," $$->name:%s \n",$$->sym->name);
				fprintf(yyout," primary ==> Ivalue \n");}
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
					$$->sym=tmp;
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
						if(flag==0)
						{
							$$=newexpr(var_e);
							$$->sym=tmp;
						}
						$$->sym=tmp;
						break;
					}
				}
				if(i==-1){
					if(scope==0 && funcounter>0){
						$$=newexpr(var_e);
						$$->sym=insertNodeToHash(Head,yytext,"global variable",scope,yylineno,functionoffset[funcounter],"function locals",1);
					}
					if(scope!=0 && funcounter>0){
						$$=newexpr(var_e);
						$$->sym=insertNodeToHash(Head,yytext,"local variable",scope,yylineno,functionoffset[funcounter],"function locals",1);
					}
					if(scope==0 && funcounter==0){
						$$=newexpr(var_e);
						$$->sym=insertNodeToHash(Head,yytext,"global variable",scope,yylineno,functionoffset[funcounter],"program variables",1);
					}
					if(scope!=0 && funcounter==0){
						$$=newexpr(var_e);
						$$->sym=insertNodeToHash(Head,yytext,"local variable",scope,yylineno,functionoffset[funcounter],"program variables",1);				
					}
					functionoffset[funcounter]=functionoffset[funcounter]+1;
				}
			}
		| LOCAL id	{	
					fprintf(yyout," Ivalue ==> local \n");
					struct SymTableEntry *tmp=NameLookUpInScope(ScopeTable,scope,yytext);
					$$=newexpr(var_e);
					$$->sym=tmp;
					if(tmp==NULL && collisionLibFun(ScopeTable,yytext)==1)
						fprintf(yyout,"\n\nERROR: local %s: Trying to shadow Library Function in line %d\n\n",yytext,yylineno);
					if(tmp==NULL && collisionLibFun(ScopeTable,yytext)==0)
					{
						if(scope==0 && funcounter>0)
							$$->sym=insertNodeToHash(Head,yytext,"global variable",scope,yylineno,functionoffset[funcounter],"function locals",1);
						if(scope!=0 && funcounter>0)
							$$->sym=insertNodeToHash(Head,yytext,"local variable",scope,yylineno,functionoffset[funcounter],"function locals",1);
						if(scope==0 && funcounter==0)
							$$->sym=insertNodeToHash(Head,yytext,"global variable",scope,yylineno,functionoffset[funcounter],"program variables",1);
						if(scope!=0 && funcounter==0)
							$$->sym=insertNodeToHash(Head,yytext,"local variable",scope,yylineno,functionoffset[funcounter],"program variables",1);
						functionoffset[funcounter]=functionoffset[funcounter]+1;
					}

				}
		| NAMESPACE_ALIAS_QUALIFIER id	{
							fprintf(yyout," Ivalue ==> ::id \n");
							struct SymTableEntry *tmp=NameLookUpInScope(ScopeTable,0,yytext);
							$$=newexpr(var_e);
							$$->sym=tmp;
							if(tmp==NULL)
								printf("\n\nERROR: There is no member on global scope with the name %s in Line %d\n\n", yytext,yylineno);
						}
		| member	{$$=$1;fprintf(yyout," Ivalue ==> member \n");}
		;

member:		lvalue DOT id	{
					//printf("$3=%s\n$1=%s\n",$3,$1->sym->name);
					if(funcounter>0)
						$$=member_item($1,$3,counter, scope, yylineno,funcounter,functionoffset,"function locals");
					else
						$$=member_item($1,$3,counter, scope, yylineno,funcounter,functionoffset,"program variables");
					//fprintf(yyout,"pointer $$ : %d \n",$$);
					fprintf(yyout," Member ==> lvalue.id \n");}
		| lvalue LEFT_SQUARE_BRACKET expr RIGHT_SQUARE_BRACKET	{
										if(funcounter>0)
											$$=member_item($1,$3->sym->name, counter, scope, yylineno,funcounter,functionoffset,"function locals");
										else
											$$=member_item($1, $3->sym->name, counter, scope, yylineno,funcounter,functionoffset,"program variables");
										fprintf(yyout," Member ==> [expr] \n");}
		| call DOT id	{fprintf(yyout," Member ==> call.id \n");}
		| call LEFT_SQUARE_BRACKET expr RIGHT_SQUARE_BRACKET {fprintf(yyout," Member ==> call[expr] \n");}
		;

call:		call LEFT_PARENTHESES elist RIGHT_PARENTHESES	{//$$=make_call($$,$3);
								fprintf(yyout," CALL ==> call(elist) \n");}
		| lvalue callsuffix	{fprintf(yyout," call ==> ivalue callsuffix \n");} 
		| LEFT_PARENTHESES funcdef RIGHT_PARENTHESES LEFT_PARENTHESES elist RIGHT_PARENTHESES {
														/*struct expr* func=newexpr(programfunc_e);
														func->sym=$2;
														$$=make_call(func,$5);*/
														fprintf(yyout," call ==> (funcdef)(elist) \n");}
		;

callsuffix:	normcall	{fprintf(yyout," callsuffix ==> normcall \n");}
		| methodcall	{fprintf(yyout," callsuffix ==> methodcall \n");}
		;

normcall:	LEFT_PARENTHESES elist RIGHT_PARENTHESES {fprintf(yyout," normcall ==> (elist) \n");}
		;	

methodcall:	DOUBLE_DOT id LEFT_PARENTHESES elist RIGHT_PARENTHESES{
										/*$$->elist=$4;
										$$->method=1;
										$$->name=$2;*/
										fprintf(yyout," methodcall ==> ..id(elist) \n");}
		;

elist:	 	expr elist1	{
					if($2==NULL)
					{
						$1->next=$2;
						$$=$1;
					}
					else
					{
						$1->next=$2;
						$$=$1;
					}			
					fprintf(yyout," elist ==> expr elist1 \n");}
		| /* empty */	{fprintf(yyout," elist ==>  \n");}
		;

elist1:		COMMA expr elist1{
					$2->next=$3;
					$$=$2;
					fprintf(yyout," elist ==> ,expr elist1 \n");}
		| /* empty */	{$$=NULL;fprintf(yyout," elist1 ==>  \n");}
		;

objectdef:	LEFT_SQUARE_BRACKET elist RIGHT_SQUARE_BRACKET	{
									int i=0;
									//struct expr* rev,*tmp2;
									struct expr* t=newexpr(newtable_e);
									struct expr* tmp=$2;
									//printf("LALALALA\n");
									//printf("pointer: %d\n",tmp->next);
									//printf("next pointer: %d\n",tmp->next->next);
									t->sym=CreateSecretVar(counter, scope, yylineno,funcounter,functionoffset,"function locals");
									addquad(tablecounter,tablecreate,NULL,t,NULL,-1,yylineno);
									tablecounter++;
									while(tmp!=NULL)
									{
										//printf("auto... %d\n",tmp);
										addquad(tablecounter,tablesetelem,t,newexpr_constnum(i),tmp,-1,yylineno);
										tablecounter++;
										//printf("object... %s\n",tmp->sym->name);
										tmp=tmp->next;
										i++;
									}
									/*rev=reverseList($2);
									tmp2=rev;
									while(tmp2!=NULL)
									{
										printf("from reverse...%s\n",tmp2->sym->name);
										tmp2=tmp2->next;
									}*/
									$$=t;
									fprintf(yyout," objectdef ==> [elist] \n");}
		| LEFT_SQUARE_BRACKET indexed RIGHT_SQUARE_BRACKET {
									struct expr* tmp=$2;
									struct expr* t=newexpr(newtable_e);
									t->sym=CreateSecretVar(counter, scope, yylineno,funcounter,functionoffset,"function locals");
									addquad(tablecounter,tablecreate,NULL,t,NULL,-1,yylineno);
									tablecounter++;
									while(tmp!=NULL)
									{
										//printf("index:%.0f, value:%.0f\n",tmp->index->numConst,tmp->numConst);
										addquad(tablecounter,tablesetelem,t,tmp->index,tmp,-1,yylineno);
										tablecounter++;
										tmp=tmp->next;
									}
									$$=t;
									fprintf(yyout," objectdef ==> [indexed] \n");}
		;

indexed:	indexedelem indexed1	{$1->next=$2;$$=$1;fprintf(yyout," indexed ==> indexedelem indexed1 \n");}
		;

indexed1: 	COMMA indexedelem indexed1{	$2->next=$3;
						$$=$2;
						fprintf(yyout," indexed ==> indexedelem indexed1 \n");}
		| /* empty */	{$$=NULL;fprintf(yyout," indexed ==>   \n");}
		;

indexedelem: 	LEFT_CURLY_BRACKET expr COLON expr RIGHT_CURLY_BRACKET{ $$=$4;
									$$->index=$2; 
									/*if(expr->type==constnum_e)
										$$->index->numConst=$4->numConst;
									if(expr->type==conststring_e)
										$$->index->strConst=$4->strConst;
									if(expr->type==constbool_e)
										$$->index->boolConst=$4->boolConst;
									else
										$$->index->sym=$4->sym;
									*/
									fprintf(yyout," indexedelem ==> { expr : expr } \n");}
		;

block:		LEFT_CURLY_BRACKET {scope++; } stamt RIGHT_CURLY_BRACKET {	Hide(ScopeTable,scope);
										scope--;
										counter=0;
										fprintf(yyout," block ==> { [stmt] } \n");}
		;

funcdef: 	FUNCTION {
			char* name=(char *)malloc(sizeof(char));
		 	char* num=(char *)malloc(sizeof(char));
			sprintf(name, "%s", "$f");
			sprintf(num, "%d", numname);			
			strcat(name,num);			
			insertNodeToHash(Head,name,"user function",scope,yylineno, -1,"",1);
			funcounter++;
			free(name);
			free(num);
			numname++;
			//scope++;
		}
		LEFT_PARENTHESES {scope++;} idlist RIGHT_PARENTHESES {offset_arg=0; scope--; infunction++;} block { functionoffset[funcounter]=0; 
													funcounter--; 
													infunction--;
													fprintf(yyout," funcdef ==> function(){} \n");
													}
		|FUNCTION id {
				struct SymTableEntry *tmp;
				tmp=NameLookUpInScope(ScopeTable,scope,yytext);
				if(tmp!=NULL && strcmp("library function", tmp->type)!=0 )
					fprintf(yyout,"\n\nERROR: name %s already exists in same scope in line %d\n\n",yytext,yylineno);
				if(collisionLibFun(ScopeTable,yytext)==1)
					fprintf(yyout,"\n\nERROR: function %s: Trying to shadow Library Function in line %d\n\n",yytext,yylineno);
				else if (tmp==NULL && collisionLibFun(ScopeTable,yytext)==0)
					insertNodeToHash(Head,yytext,"user function",scope,yylineno, -1,"",1);
				funcounter++;
			      } LEFT_PARENTHESES {scope++;} idlist RIGHT_PARENTHESES {offset_arg=0; scope--; infunction++;} block {functionoffset[funcounter]=0; 
															funcounter--; 
															infunction--;
															fprintf(yyout," funcdef ==> function id(){} \n");
															}
		;

const:		NUMBER {$$=newexpr(constnum_e); $$->numConst=$1; fprintf(yyout," const ==> number \n");}
		| STRING {$$=newexpr(conststring_e); $$->strConst=$1; fprintf(yyout," const ==> string \n");}
		| NIL {$$=newexpr(nil_e); $$->strConst=yytext; fprintf(yyout," const ==> nil \n");}
		| TRUE {$$=newexpr(constbool_e); $$->numConst=1.0; $$->boolConst='1'; fprintf(yyout," const ==> true \n");}
		| FALSE {$$=newexpr(constbool_e); $$->numConst=0.0; $$->boolConst='0'; fprintf(yyout," const ==> false \n");}
		| FLOAT {$$=newexpr(constnum_e); $$->numConst=$1; fprintf(yyout," const ==> float \n");}
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
				{insertNodeToHash(Head,yytext,"formal argument",scope,yylineno, offset_arg,"formal arguments",1);
				 offset_arg++;}
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
				{insertNodeToHash(Head,yytext,"formal argument",scope,yylineno, offset_arg,"formal arguments",1);
				 offset_arg++;}
			 
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

returnstmt:	RETURN SEMI_COLON {//counter=CreateSecretVar(counter, scope, yylineno);
			fprintf(yyout," returnstmt ==> return ;\n");}
		| RETURN expr SEMI_COLON {//counter=CreateSecretVar(counter, scope, yylineno);
			fprintf(yyout," returnstmt ==> return expr;\n");}
		;

%%

int yyerror (char* yaccProvidedMessage)
{
	fprintf(stderr, "%s: at line %d, before token: '%s'\n", yaccProvidedMessage, yylineno, yytext);
	fprintf(stderr, "INPUT NOT VALID\n");
}

int main(int argc, char** argv)
{
	int lol=0;
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
	functionoffsetcreation();
	tablequadcreation();
	Head=SymTable_new(509);
	ScopeTable=SymTable_new(100);
	insertNodeToHash(Head,"print","library function",0,0,0,"",1);
	insertNodeToHash(Head,"input","library function",0,0,0,"",1);
    	insertNodeToHash(Head,"objectmemberkeys","library function",0,0,0,"",1);
	insertNodeToHash(Head,"objecttotalmembers","library function",0,0,0,"",1);
    	insertNodeToHash(Head,"objectcopy","library function",0,0,0,"",1);
    	insertNodeToHash(Head,"totalarguments","library function",0,0,0,"",1);
	insertNodeToHash(Head,"arguments","library function",0,0,0,"",1);
    	insertNodeToHash(Head,"typeof","library function",0,0,0,"",1);
    	insertNodeToHash(Head,"stronum","library function",0,0,0,"",1);
    	insertNodeToHash(Head,"sqrt","library function",0,0,0,"",1);
    	insertNodeToHash(Head,"cos","library function",0,0,0,"",1);
    	insertNodeToHash(Head,"sin","library function",0,0,0,"",1);
	
	yyparse();
	//printfunstionoffset();
	printf("\n\n");
	printScopeTable(ScopeTable);
	printf("\n\n");
	//printf("\nop: %d, arg1:%s\n",quadtable[0]->op,quadtable[0]->arg1->sym->name);
	printQuad();
	return 0;
}
