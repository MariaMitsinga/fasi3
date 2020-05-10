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
	
	int block_flag=0;
	int return_flag=0;
	int flag_not=0;
	int flag_cont_break=0;
	char* krifi;
	int i=0;
	int scope=0;
	int numname=0;
	int offset_arg=0;
	int infunction=0;
	int flag_Array=0;
	int other_than_OrAnd_flag=0;
	int logic_expr_flag=0;
	int while_counter=0;
	int for_counter=0;
	int tablecreate_flag=0;
	
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
	struct quad* quad;
	struct stmt_t* stmt;
	struct forpref* forpref;
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

%type <expr> stmt
%type <expr> stamt
%type <expr> block
%type <expr> loopstmt

%type <strVal> funcname
%type <expr> funprefix
%type <expr> funcdef

%type <call> callsuffix
%type <call> normcall
%type <call> methodcall

%type <forpref> forprefix
%type <expr> forstmt

%type <intVal> M
%type <intVal> N

%type <intVal> whilestart
%type <intVal> whilecond

%type <expr> break
%type <expr> continue
%type <expr> whilestmt

%type <expr> ifstmt
%type <intVal> ifprefix
%type <intVal> elseprefix

%%

program:	stamt {fprintf(yyout," program ==> stmt \n");}
		;

stamt:		stmt stamt {fprintf(yyout," stamt ==> stmt stamt\n");}
		| /* empty*/ {fprintf(yyout,"stamt ==> empty \n");}
		;

stmt:		expr SEMI_COLON {	
					if($1->type==boolexpr_e)
					{	
						struct expr* tmp=newexpr(constbool_e);
						struct expr* tmp2=newexpr(boolexpr_e);
						struct expr* tmp3=newexpr(constbool_e);
						if(funcounter>0){
							tmp2->sym=CreateSecretVar(counter, scope, yylineno,funcounter,functionoffset,"function locals");
						}else{
							tmp2->sym=CreateSecretVar(counter, scope, yylineno,funcounter,functionoffset,"program variables");
						}
						tmp->boolConst='1';			
						addquad(tablecounter,assign,tmp2,tmp,NULL,-1,yylineno);
						backpatch($1->truelist,tablecounter-1);
						addquad(tablecounter,jump,NULL,NULL,NULL,tablecounter+2,yylineno);
						tmp3->boolConst='0';
						addquad(tablecounter,assign,tmp2,tmp3,NULL,-1,yylineno);
						backpatch($1->falselist,tablecounter-1);
					}
					//$$->breakList=make_stmt($$->breakList);
					counter=0;
					//other_than_OrAnd_flag=0;	
				 	fprintf(yyout," stmt ==> expr; \n");}
		|ifstmt	{$$=$1;counter=0;fprintf(yyout," stmt ==> ifstmt \n");}
		|whilestmt {$$=$1;counter=0;fprintf(yyout," stmt ==> whilestmt \n");}
		|forstmt {$$=$1;counter=0;fprintf(yyout," stmt ==> forstmt \n");}
		|returnstmt {counter=0;fprintf(yyout," stmt ==> returnstmt \n");}
		|break {$$=$1;counter=0;fprintf(yyout," stmt ==> break \n");}
		|continue {$$=$1;counter=0;fprintf(yyout," stmt ==> continue; \n");}
		|block {$$=$1;counter=0;fprintf(yyout,"stmt ==> {} \n");}
		|funcdef {$$=$1;counter=0;fprintf(yyout," stmt ==> funcdef \n");}
		|SEMI_COLON {$$=NULL;counter=0;fprintf(yyout," stmt ==> ; \n");}
		;

break:		BREAK SEMI_COLON{	struct loopbc* tmp;
					tmp=whereat();
					if(tmp->loopcounter==0){
						myerror++;
						printf("\n\nERROR: break statement not in loop in line %d\n\n",yylineno);
					}
					
					//printf("BHKE\n");
					/*$$->breakList=make_stmt($$->breakList);
					//printf("BHKE\n");
					addquad(tablecounter,jump,NULL,NULL,NULL,-1,yylineno);
					$$->breakList->index= tablecounter-1;
					$$->breakList->while_counter=while_counter;
					//printf("from break--->%d\n",$$->breakList->index);*/

					//$$=malloc(sizeof(struct expr));
					if(place!=NULL)
					{
						addquad(tablecounter,jump,NULL,NULL,NULL,-1,yylineno);
						if(place->Id==0)//while case
						{
							/*$$->breakList_for=NULL;
							$$->contList_for=NULL;
							$$->contList_while=NULL;
							$$->breakList_while=make_stmt($$->breakList_while);
							$$->breakList_while->index= tablecounter-1;
							$$->breakList_while->while_counter=while_counter;*/

							breakList_while=addToBreakContList(breakList_while,tablecounter-1,while_counter);
						}
						if(place->Id==1)//for case
						{
							/*$$->breakList_while=NULL;
							$$->contList_while=NULL;
							$$->contList_for=NULL;
							$$->breakList_for=make_stmt($$->breakList_for);
							$$->breakList_for->index= tablecounter-1;
							$$->breakList_for->for_counter=for_counter;*/

							breakList_for=addToBreakContList(breakList_for,tablecounter-1,for_counter);
						}
					}
					if(place==NULL)
						addquad(tablecounter,jump,NULL,NULL,NULL,-1,yylineno);
					fprintf(yyout,"break==> break; \n");
				}
		;

continue:	CONTINUE SEMI_COLON{
					struct loopbc* tmp;
					tmp=whereat();
					if(tmp->loopcounter==0){
						myerror++;
						printf("\n\nERROR: continue statement not in loop in line %d\n\n",yylineno);
					}
					/*flag_cont_break++;
					$$=make_stmt($$);
					$$->contList= newlist(tablecounter);
					addquad(tablecounter,jump,NULL,NULL,NULL,-1,yylineno);*/

					//$$=malloc(sizeof(struct expr));
					if(place!=NULL)
					{
						addquad(tablecounter,jump,NULL,NULL,NULL,-1,yylineno);
						if(place->Id==0)//while case
						{							
							contList_while=addToBreakContList(contList_while,tablecounter-1,while_counter);
						}
						if(place->Id==1)//for case
						{
							contList_for=addToBreakContList(contList_for,tablecounter-1,for_counter);
						}
					}
				}
		;

expr:		assgnexpr 	{
					$$=$1;
					fprintf(yyout," expr ==> assgnexpr \n");
				}
		|expr PLUS expr {
			if (checkArithmOperation($1,$3,"arithmetic operator")){
				$$=newexpr(arithexpr_e);
				if(funcounter>0){
					$$->sym=CreateSecretVar(counter, scope, yylineno,funcounter,functionoffset,"function locals");
				}else{
					$$->sym=CreateSecretVar(counter, scope, yylineno,funcounter,functionoffset,"program variables");
				}
				//counter++;
				//$$->numConst=$1->numConst + $3->numConst;
				addquad(tablecounter,add,$$,$1,$3,-1,yylineno);
			}
			//else printMerror();
			fprintf(yyout," expr ==> expr + expr \n");
		}
		|expr MINUS expr {
			if (checkArithmOperation($1,$3,"arithmetic operator")){
				$$=newexpr(arithexpr_e);
				if(funcounter>0){
					$$->sym=CreateSecretVar(counter, scope, yylineno,funcounter,functionoffset,"function locals");
				}else{
					$$->sym=CreateSecretVar(counter, scope, yylineno,funcounter,functionoffset,"program variables");
				}
				//$$->numConst=$1->numConst - $3->numConst;
				addquad(tablecounter,sub,$$,$1,$3,-1,yylineno);
			}
			//else printMerror();
			fprintf(yyout," expr ==> expr - expr \n");}	
		|expr MULTIPLE expr {
			if (checkArithmOperation($1,$3,"arithmetic operator")){
				$$=newexpr(arithexpr_e);
				if(funcounter>0){
					$$->sym=CreateSecretVar(counter, scope, yylineno,funcounter,functionoffset,"function locals");
				}else{
					$$->sym=CreateSecretVar(counter, scope, yylineno,funcounter,functionoffset,"program variables");
				}
				//$$->numConst=$1->numConst * $3->numConst;
				addquad(tablecounter,mul,$$,$1,$3,-1,yylineno);
			}
			//else printMerror();
			fprintf(yyout," expr ==> expr * expr \n");}
		|expr FORWARD_SLASH expr {
			if (checkArithmOperation($1,$3,"arithmetic operator")){
				$$=newexpr(arithexpr_e);
				if(funcounter>0){
					$$->sym=CreateSecretVar(counter, scope, yylineno,funcounter,functionoffset,"function locals");
				}else{
					$$->sym=CreateSecretVar(counter, scope, yylineno,funcounter,functionoffset,"program variables");
				}
				//$$->numConst=$1->numConst / $3->numConst;
				addquad(tablecounter,DIV,$$,$1,$3,-1,yylineno);
			}
			//else printMerror();
			fprintf(yyout," expr ==> expr / expr \n");}
		|expr PERCENT expr {
			if (checkArithmOperation($1,$3,"arithmetic operator")){
				$$=newexpr(arithexpr_e);
				if(funcounter>0){
					$$->sym=CreateSecretVar(counter, scope, yylineno,funcounter,functionoffset,"function locals");
				}else{
					$$->sym=CreateSecretVar(counter, scope, yylineno,funcounter,functionoffset,"program variables");
				}
				//$$->numConst=(int)($1->numConst) % (int)($3->numConst);
				addquad(tablecounter,mod,$$,$1,$3,-1,yylineno);
			}
			//else printMerror();
			fprintf(yyout," expr ==> expr % expr \n");}
		|expr GREATER expr {
			if (checkArithmOperation($1,$3,"comparison operator")){
				$$=newexpr(boolexpr_e);
				logic_expr_flag=1;
				other_than_OrAnd_flag=1;
				struct truefalse* true=NULL,*false=NULL;
				addquad(tablecounter,if_greater,$1,$3,NULL,-1,yylineno);
				$$->truelist=AddTrueFalseList(true, quadtable[tablecounter-1]);
				addquad(tablecounter,jump,NULL,NULL,NULL,-1,yylineno);
				$$->falselist=AddTrueFalseList(false, quadtable[tablecounter-1]);
			}
			//else printMerror();
			
			fprintf(yyout," expr ==> expr > expr \n");}
		|expr GREATER_EQUAL expr {
			if (checkArithmOperation($1,$3,"comparison operator")){
				$$=newexpr(boolexpr_e);
				logic_expr_flag=1;
				other_than_OrAnd_flag=1;
				struct truefalse* true=NULL,*false=NULL;
				addquad(tablecounter,if_greatereq,$1,$3,NULL,-1,yylineno);
				$$->truelist=AddTrueFalseList(true, quadtable[tablecounter-1]);
				addquad(tablecounter,jump,NULL,NULL,NULL,-1,yylineno);
				$$->falselist=AddTrueFalseList(false, quadtable[tablecounter-1]);
			}
			//else printMerror();
			fprintf(yyout," expr ==> expr >= expr \n");}
		|expr LESS expr {
			if (checkArithmOperation($1,$3,"comparison operator")){
				$$=newexpr(boolexpr_e);
				logic_expr_flag=1;
				other_than_OrAnd_flag=1;
				struct truefalse* true=NULL,*false=NULL;
				addquad(tablecounter,if_less,$1,$3,NULL,-1,yylineno);
				$$->truelist=AddTrueFalseList(true, quadtable[tablecounter-1]);
				addquad(tablecounter,jump,NULL,NULL,NULL,-1,yylineno);
				$$->falselist=AddTrueFalseList(false, quadtable[tablecounter-1]);
			}
			//else printMerror();
			fprintf(yyout," expr ==> expr < expr \n");}
		|expr LESS_EQUAL expr {
			if (checkArithmOperation($1,$3,"comparison operator")){
				$$=newexpr(boolexpr_e);
				logic_expr_flag=1;
				other_than_OrAnd_flag=1;
				struct truefalse* true=NULL,*false=NULL;
				addquad(tablecounter,if_lesseq,$1,$3,NULL,-1,yylineno);
				$$->truelist=AddTrueFalseList(true, quadtable[tablecounter-1]);
				addquad(tablecounter,jump,NULL,NULL,NULL,-1,yylineno);
				$$->falselist=AddTrueFalseList(false, quadtable[tablecounter-1]);
			}
			//else printMerror();
			fprintf(yyout," expr ==> expr <= expr \n");}
		|expr DOUBLE_EQUAL {
			if($1->type==boolexpr_e){	
				struct expr* tmp=newexpr(constbool_e);
				struct expr* tmp2=newexpr(boolexpr_e);
				struct expr* tmp3=newexpr(constbool_e);
				if(funcounter>0){
					tmp2->sym=CreateSecretVar(counter, scope, yylineno,funcounter,functionoffset,"function locals");
				}else{
					tmp2->sym=CreateSecretVar(counter, scope, yylineno,funcounter,functionoffset,"program variables");
				}
				tmp->boolConst='1';			
				addquad(tablecounter,assign,tmp2,tmp,NULL,-1,yylineno);
				backpatch($1->truelist,tablecounter-1);
				addquad(tablecounter,jump,NULL,NULL,NULL,tablecounter+2,yylineno);
				tmp3->boolConst='0';
				addquad(tablecounter,assign,tmp2,tmp3,NULL,-1,yylineno);
				backpatch($1->falselist,tablecounter-1);
				$1=tmp2;
			}
		}expr { 
			if($4->type==boolexpr_e){	
				struct expr* tmp=newexpr(constbool_e);
				struct expr* tmp2=newexpr(boolexpr_e);
				struct expr* tmp3=newexpr(constbool_e);
				if(funcounter>0){
					tmp2->sym=CreateSecretVar(counter, scope, yylineno,funcounter,functionoffset,"function locals");
				}else{
					tmp2->sym=CreateSecretVar(counter, scope, yylineno,funcounter,functionoffset,"program variables");
				}
				tmp->boolConst='1';			
				addquad(tablecounter,assign,tmp2,tmp,NULL,-1,yylineno);
				backpatch($4->truelist,tablecounter-1);
				addquad(tablecounter,jump,NULL,NULL,NULL,tablecounter+2,yylineno);
				tmp3->boolConst='0';
				addquad(tablecounter,assign,tmp2,tmp3,NULL,-1,yylineno);
				backpatch($4->falselist,tablecounter-1);
				$4=tmp2;
			}
			$$=newexpr(boolexpr_e);
			logic_expr_flag=1;
			other_than_OrAnd_flag=1;
			struct truefalse* true=NULL,*false=NULL;
			addquad(tablecounter,if_eq,$1,$4,NULL,-1,yylineno);
			$$->truelist=AddTrueFalseList(true, quadtable[tablecounter-1]);
			addquad(tablecounter,jump,NULL,NULL,NULL,-1,yylineno);
			$$->falselist=AddTrueFalseList(false, quadtable[tablecounter-1]);

			fprintf(yyout," expr ==> expr == expr \n");}
		|expr NOT_EQUAL {
			if($1->type==boolexpr_e){	
				struct expr* tmp=newexpr(constbool_e);
				struct expr* tmp2=newexpr(boolexpr_e);
				struct expr* tmp3=newexpr(constbool_e);
				if(funcounter>0){
					tmp2->sym=CreateSecretVar(counter, scope, yylineno,funcounter,functionoffset,"function locals");
				}else{
					tmp2->sym=CreateSecretVar(counter, scope, yylineno,funcounter,functionoffset,"program variables");
				}
				tmp->boolConst='1';			
				addquad(tablecounter,assign,tmp2,tmp,NULL,-1,yylineno);
				backpatch($1->truelist,tablecounter-1);
				addquad(tablecounter,jump,NULL,NULL,NULL,tablecounter+2,yylineno);
				tmp3->boolConst='0';
				addquad(tablecounter,assign,tmp2,tmp3,NULL,-1,yylineno);
				backpatch($1->falselist,tablecounter-1);
				$1=tmp2;
			}
		}expr { 
			if($4->type==boolexpr_e){	
				struct expr* tmp=newexpr(constbool_e);
				struct expr* tmp2=newexpr(boolexpr_e);
				struct expr* tmp3=newexpr(constbool_e);
				if(funcounter>0){
					tmp2->sym=CreateSecretVar(counter, scope, yylineno,funcounter,functionoffset,"function locals");
				}else{
					tmp2->sym=CreateSecretVar(counter, scope, yylineno,funcounter,functionoffset,"program variables");
				}
				tmp->boolConst='1';			
				addquad(tablecounter,assign,tmp2,tmp,NULL,-1,yylineno);
				backpatch($4->truelist,tablecounter-1);
				addquad(tablecounter,jump,NULL,NULL,NULL,tablecounter+2,yylineno);
				tmp3->boolConst='0';
				addquad(tablecounter,assign,tmp2,tmp3,NULL,-1,yylineno);
				backpatch($4->falselist,tablecounter-1);
				$4=tmp2;
			}
 
			$$=newexpr(boolexpr_e);
			logic_expr_flag=1;
			other_than_OrAnd_flag=1;
			struct truefalse* true=NULL,*false=NULL;
			addquad(tablecounter,if_noteq,$1,$4,NULL,-1,yylineno);
			$$->truelist=AddTrueFalseList(true, quadtable[tablecounter-1]);
			addquad(tablecounter,jump,NULL,NULL,NULL,-1,yylineno);
			$$->falselist=AddTrueFalseList(false, quadtable[tablecounter-1]);

			fprintf(yyout," expr ==> expr != expr \n");}
		|expr AND {  	if($1->type!=boolexpr_e)
				{
					struct expr* tmp=newexpr(constbool_e);
					tmp->boolConst='1';
					addquad(tablecounter,if_eq,$1,tmp,NULL,-1,yylineno);
					addquad(tablecounter,jump,NULL,NULL,NULL,-1,yylineno);

					struct truefalse* true=NULL,*false=NULL;
					$1->truelist=AddTrueFalseList(true, quadtable[tablecounter-2]);
					$1->falselist=AddTrueFalseList(false, quadtable[tablecounter-1]);

		        	}
		     } M expr { 
			$$=newexpr(boolexpr_e);
			if($5->type!=boolexpr_e)
			{
				struct expr* tmp=newexpr(constbool_e);
				tmp->boolConst='1';
				addquad(tablecounter,if_eq,$5,tmp,NULL,-1,yylineno);
				addquad(tablecounter,jump,NULL,NULL,NULL,-1,yylineno);
				struct truefalse* true=NULL,*false=NULL;
				$5->truelist=AddTrueFalseList(true, quadtable[tablecounter-2]);
				$5->falselist=AddTrueFalseList(false, quadtable[tablecounter-1]);
			}
			other_than_OrAnd_flag=1;
			//printf("edwww1\n");
			backpatch($1->truelist,$4);
			//printf("edwww2\n");
			$$->truelist=$5->truelist;
			$$->falselist=merge($1->falselist,$5->falselist);
			logic_expr_flag=1;
			printf("tablecounter=%d\n",tablecounter);
			fprintf(yyout," expr ==> expr && expr \n");}
		|expr OR {  	if($1->type!=boolexpr_e)
				{
					struct expr* tmp=newexpr(constbool_e);
					tmp->boolConst='1';
					addquad(tablecounter,if_eq,$1,tmp,NULL,-1,yylineno);
					addquad(tablecounter,jump,NULL,NULL,NULL,-1,yylineno);

					struct truefalse* true=NULL,*false=NULL;
					$1->truelist=AddTrueFalseList(true, quadtable[tablecounter-2]);
					$1->falselist=AddTrueFalseList(false, quadtable[tablecounter-1]);
				}
		     	} M expr { 
			$$=newexpr(boolexpr_e);
			if($5->type!=boolexpr_e)
			{
				struct expr* tmp=newexpr(constbool_e);
				tmp->boolConst='1';
				addquad(tablecounter,if_eq,$5,tmp,NULL,-1,yylineno);
				addquad(tablecounter,jump,NULL,NULL,NULL,-1,yylineno);
				struct truefalse* true=NULL,*false=NULL;
				$5->truelist=AddTrueFalseList(true, quadtable[tablecounter-2]);
				$5->falselist=AddTrueFalseList(false, quadtable[tablecounter-1]);
			}
			other_than_OrAnd_flag=1;

			backpatch($1->falselist,$4);
			$$->truelist=merge($1->truelist,$5->truelist);
			$$->falselist=$5->falselist;
			logic_expr_flag=1;
			
			fprintf(yyout," expr ==> expr || expr \n");}
		| term { $$=$1; other_than_OrAnd_flag=0;fprintf(yyout," expr ==> term \n");}
		;

M:		/*empty*/ {$$=tablecounter;printf("M->empty\n");}
		;

term:		LEFT_PARENTHESES expr RIGHT_PARENTHESES {$$=$2; flag_not=1; fprintf(yyout," term ==> (expr) \n");}
		| MINUS expr %prec UMINUS {
						if(checkUminusOperation($2));
						{
							istempexpr($2);
							$$=newexpr(arithexpr_e);
							if(funcounter>0){
								$$->sym= istempexpr($2) ? $2->sym : CreateSecretVar(counter, scope, yylineno,funcounter,functionoffset,"function locals");
							}else{
								$$->sym= istempexpr($2) ? $2->sym : CreateSecretVar(counter, scope, yylineno,funcounter,functionoffset,"program variables");
							}
							addquad(tablecounter,uminus,$$,$2,NULL,-1,yylineno);
						}
						fprintf(yyout," term ==> -expr \n");}
		| NOT expr {
			/*if($2->type!=arithexpr_e && flag_not!=1) {
				myerror++;
				flag_not=0;
			}*/
			struct expr* tmp=newexpr(constbool_e);
			struct expr* tmp2;
			struct truefalse* t=NULL,*f=NULL;
			tmp2=newexpr(assignexpr_e);
			tmp->boolConst='1';
			tmp->truelist=NULL;
			tmp->falselist=NULL;
			if ($2->type!=boolexpr_e){
				addquad(tablecounter,if_eq,$2,tmp,NULL,-1,yylineno);
				$2->truelist=AddTrueFalseList(t,quadtable[tablecounter-1]);
				addquad(tablecounter,jump,NULL,NULL,NULL,-1,yylineno);
				$2->falselist=AddTrueFalseList(f,quadtable[tablecounter-1]);
			}	
			$$=newexpr(boolexpr_e);
			logic_expr_flag=1;
			$$->truelist=$2->falselist;
			$$->falselist=$2->truelist;
			fprintf(yyout," term ==> !expr \n");}
		| DOUBLE_PLUS lvalue 	{ if(check_arith($2, "++lvalue")==1){
						struct expr* tmp,*num;
						tmp=newexpr(arithexpr_e);
				 		if ($lvalue->type == tableitem_e) {
							struct expr* val;
							num=newexpr(constnum_e);
							if(funcounter>0)
								val=emit_iftableitem($2,counter,scope,yylineno,funcounter,functionoffset,"function locals");
							else
								val=emit_iftableitem($2,counter,scope,yylineno,funcounter,functionoffset,"program variables");
							num->numConst=1;
							addquad(tablecounter,add, val, val, num,-1,yylineno);
							addquad(tablecounter,tablesetelem, $2, $2->index, val,-1,yylineno);
							$$=val;
						}else{
							if(funcounter>0){
								tmp->sym=CreateSecretVar(counter, scope, yylineno,funcounter,functionoffset,"function locals");
							}else{
								tmp->sym=CreateSecretVar(counter, scope, yylineno,funcounter,functionoffset,"program variables");
							}
							num=newexpr(constnum_e);
							num->numConst=1;
							addquad(tablecounter,add,$2,$2,num,-1,yylineno);
							addquad(tablecounter,assign,tmp,$2,NULL,-1,yylineno);
							$$=tmp;
						}
					  	if(strcmp($2->sym->type,"user function")==0 || strcmp("library function", $2->sym->type)==0){
					  		fprintf(yyout,"\n\nERROR: value is a function so we cannot assigned: %s in line: %d\n\n",$2->sym->name,yylineno);
							myerror++;
						}	
					   }
					  fprintf(yyout," term ==> ++lvalue \n");}
		| lvalue DOUBLE_PLUS	{ if(check_arith($1, "lvalue++")==1){
						struct expr* tmp,*num;
						tmp=newexpr(arithexpr_e);
						if(funcounter>0){
							tmp->sym=CreateSecretVar(counter, scope, yylineno,funcounter,functionoffset,"function locals");
						}else{
							tmp->sym=CreateSecretVar(counter, scope, yylineno,funcounter,functionoffset,"program variables");
						}
				 		if ($lvalue->type == tableitem_e) {
							struct expr* val;
							num=newexpr(constnum_e);
							if(funcounter>0)
								val=emit_iftableitem($1,counter,scope,yylineno,funcounter,functionoffset,"function locals");
							else
								val=emit_iftableitem($1,counter,scope,yylineno,funcounter,functionoffset,"program variables");
							num->numConst=1;
							addquad(tablecounter,assign, tmp, val,NULL,-1,yylineno);
							addquad(tablecounter,add, val, val, num,-1,yylineno);
							addquad(tablecounter,tablesetelem, $1, $1->index, val,-1,yylineno);
							$$=val;
						}else{
							addquad(tablecounter,assign,tmp,$1,NULL,-1,yylineno);
							num=newexpr(constnum_e);
							num->numConst=1;
							addquad(tablecounter,add,$1,$1,num,-1,yylineno);
							$$=tmp;
						}
					  	if(strcmp($1->sym->type,"user function")==0 || strcmp("library function", $1->sym->type)==0){
					  		fprintf(yyout,"\n\nERROR: value is a function so we cannot assigned: %s in line: %d\n\n",$1->sym->name,yylineno);
							myerror++;
						}	
					  }
					  fprintf(yyout," term ==> lvalue++ \n");}
		| DOUBLE_MINUS lvalue	{ if(check_arith($2, "--lvalue")==1){
						struct expr* tmp,*num;
						tmp=newexpr(arithexpr_e);
				 		if ($lvalue->type == tableitem_e) {
							struct expr* val;
							num=newexpr(constnum_e);
							if(funcounter>0)
								val=emit_iftableitem($2,counter,scope,yylineno,funcounter,functionoffset,"function locals");
							else
								val=emit_iftableitem($2,counter,scope,yylineno,funcounter,functionoffset,"program variables");
							num->numConst=1;
							addquad(tablecounter,sub, val, val, num,-1,yylineno);
							addquad(tablecounter,tablesetelem, $2, $2->index, val,-1,yylineno);
							$$=val;
						}else{
							if(funcounter>0){
								tmp->sym=CreateSecretVar(counter, scope, yylineno,funcounter,functionoffset,"function locals");
							}else{
								tmp->sym=CreateSecretVar(counter, scope, yylineno,funcounter,functionoffset,"program variables");
							}
							num=newexpr(constnum_e);
							num->numConst=1;
							addquad(tablecounter,sub,$2,$2,num,-1,yylineno);
							addquad(tablecounter,assign,tmp,$2,NULL,-1,yylineno);
							$$=tmp;
						}
					  	if(strcmp($2->sym->type,"user function")==0 || strcmp("library function", $2->sym->type)==0){
					  		fprintf(yyout,"\n\nERROR: value is a function so we cannot assigned: %s in line: %d\n\n",$2->sym->name,yylineno);
							myerror++;
						}
					  }
					  fprintf(yyout," term ==> --lvalue \n");}
		| lvalue DOUBLE_MINUS	{ if(check_arith($1, "lvalue--")==1){
						struct expr* tmp,*num;
						tmp=newexpr(arithexpr_e);
						if(funcounter>0){
							tmp->sym=CreateSecretVar(counter, scope, yylineno,funcounter,functionoffset,"function locals");
						}else{
							tmp->sym=CreateSecretVar(counter, scope, yylineno,funcounter,functionoffset,"program variables");
						}
				 		if ($lvalue->type == tableitem_e) {
							struct expr* val;
							num=newexpr(constnum_e);
							if(funcounter>0)
								val=emit_iftableitem($1,counter,scope,yylineno,funcounter,functionoffset,"function locals");
							else
								val=emit_iftableitem($1,counter,scope,yylineno,funcounter,functionoffset,"program variables");
							num->numConst=1;
							addquad(tablecounter,assign, tmp,val ,NULL,-1,yylineno);
							addquad(tablecounter,sub, val, val, num,-1,yylineno);
							addquad(tablecounter,tablesetelem, $1, $1->index, val,-1,yylineno);
							$$=val;
						}else{
							addquad(tablecounter,assign,tmp,$1,NULL,-1,yylineno);
							num=newexpr(constnum_e);
							num->numConst=1;
							addquad(tablecounter,sub,$1,$1,num,-1,yylineno);
							$$=tmp;
						}
					  	if(strcmp($1->sym->type,"user function")==0 || strcmp("library function", $1->sym->type)==0){
					 		fprintf(yyout,"\n\nERROR: value is a function so we cannot assigned: %s in line: %d\n\n",$1->sym->name,yylineno);
							myerror++;
						}	
					  }
					  fprintf(yyout," term ==> lvalue-- \n");}
		| primary {$$=$1;fprintf(yyout," term ==> primary \n");}
		;

assgnexpr:	lvalue EQUAL expr {	
					struct expr* hval;
					if($1->type==tableitem_e){
						flag_Array=1;
						//fprintf(yyout," bhke sto ==\n");
						$$=newexpr(assignexpr_e);
						if($3->type==boolexpr_e)
						{
							struct expr* tmp=newexpr(constbool_e);
							struct expr* tmp2=newexpr(boolexpr_e);
							struct expr* tmp3=newexpr(constbool_e);
							if(funcounter>0){
								tmp2->sym=CreateSecretVar(counter, scope, yylineno,funcounter,functionoffset,"function locals");
							}else{
								tmp2->sym=CreateSecretVar(counter, scope, yylineno,funcounter,functionoffset,"program variables");
							}
							tmp->boolConst='1';			
							addquad(tablecounter,assign,tmp2,tmp,NULL,-1,yylineno);
							backpatch($3->truelist,tablecounter-1);
							addquad(tablecounter,jump,NULL,NULL,NULL,tablecounter+2,yylineno);
							tmp3->boolConst='0';
							addquad(tablecounter,assign,tmp2,tmp3,NULL,-1,yylineno);
							backpatch($3->falselist,tablecounter-1);

							emit(tablesetelem,$1->index,tmp2,$1,-1,yylineno);
							
							if($3->type!=nil_e)
							{if(funcounter>0)
								$$=emit_iftableitem($1,counter,scope,yylineno,funcounter,functionoffset,"function locals");
							else
								$$=emit_iftableitem($1,counter,scope,yylineno,funcounter,functionoffset,"program variables");}
							$$->type=assignexpr_e;
						}
						else{
							emit(tablesetelem,$1->index,$3,$1,-1,yylineno);
						
							if($3->type!=nil_e)
							{if(funcounter>0)
								$$=emit_iftableitem($1,counter,scope,yylineno,funcounter,functionoffset,"function locals");
							else
								$$=emit_iftableitem($1,counter,scope,yylineno,funcounter,functionoffset,"program variables");}
						}
					}else if($1->type!=tableitem_e){

						if($3->type==boolexpr_e)
						{
							struct expr* tmp=newexpr(constbool_e);
							struct expr* tmp2=newexpr(boolexpr_e);
							struct expr* tmp3=newexpr(constbool_e);
							if(funcounter>0){
								tmp2->sym=CreateSecretVar(counter, scope, yylineno,funcounter,functionoffset,"function locals");
							}else{
								tmp2->sym=CreateSecretVar(counter, scope, yylineno,funcounter,functionoffset,"program variables");
							}
							tmp->boolConst='1';			
							addquad(tablecounter,assign,tmp2,tmp,NULL,-1,yylineno);
							backpatch($3->truelist,tablecounter-1);
							addquad(tablecounter,jump,NULL,NULL,NULL,tablecounter+2,yylineno);
							tmp3->boolConst='0';
							addquad(tablecounter,assign,tmp2,tmp3,NULL,-1,yylineno);
							backpatch($3->falselist,tablecounter-1);
							
							addquad(tablecounter,assign,$1,tmp2,NULL,-1,yylineno);
							hval=newexpr(assignexpr_e);
							if(funcounter>0){
								hval->sym=CreateSecretVar(counter, scope, yylineno,funcounter,functionoffset,"function locals");
							}else{
								hval->sym=CreateSecretVar(counter, scope, yylineno,funcounter,functionoffset,"program variables");
							}
							$$=hval;
							//$$->numConst=$1->numConst;
							addquad(tablecounter,assign,$$,$1,NULL,-1,yylineno);
						}
						else{	
							addquad(tablecounter,assign,$1,$3,NULL,-1,yylineno);
							$$=newexpr(assignexpr_e);
							if(funcounter>0){
								$$->sym=CreateSecretVar(counter, scope, yylineno,funcounter,functionoffset,"function locals");
							}else{
								$$->sym=CreateSecretVar(counter, scope, yylineno,funcounter,functionoffset,"program variables");
							}
							//$$->numConst=$1->numConst;
							addquad(tablecounter,assign,$$,$1,NULL,-1,yylineno);	
						}
					}
					if($1->sym!=NULL){
						if(strcmp($1->sym->type,"user function")==0 || strcmp("library function", $1->sym->type)==0){
							fprintf(yyout,"\n\nERROR: value is a function so we cannot assigned %s in line %d\n\n",$1->sym->name,yylineno);
							myerror++;
						}
					}
					fprintf(yyout," assgnexpr ==> Ivalue=expr \n");
		};

primary:  	lvalue	{
				if(funcounter>0)
					$$=emit_iftableitem($1,counter,scope,yylineno,funcounter,functionoffset,"function locals");
				else
					$$=emit_iftableitem($1,counter,scope,yylineno,funcounter,functionoffset,"program variables");
				fprintf(yyout," primary ==> Ivalue \n");}
		| call {$$=$1;fprintf(yyout," primary ==> call \n");}
		| objectdef {$$=$1;fprintf(yyout," primary ==> objectdef \n");}
		| LEFT_PARENTHESES funcdef RIGHT_PARENTHESES {$$=$2;fprintf(yyout," primary ==> (funcdef) \n");}
		| const {$$=$1;fprintf(yyout," primary ==> const \n");}
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
										myerror++;
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
							if(collisionLibFun(ScopeTable,yytext)==1)
								$$=newexpr(libraryfunc_e);
							else if(tmp!=NULL && strcmp("user function", tmp->type)==0)	
								$$=newexpr(programfunc_e);
							else
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
					if(collisionLibFun(ScopeTable,yytext)==1){
						fprintf(yyout,"\n\nERROR: local %s: Trying to shadow Library Function in line %d\n\n",yytext,yylineno);
						myerror++;
					}
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
							struct SymTableEntry *tmp=NameLookUpInScope(ScopeTable,0,yytext);
							if(collisionLibFun(ScopeTable,yytext)==1)
								$$=newexpr(libraryfunc_e);
							else if(tmp!=NULL && strcmp("user function", tmp->type)==0)	
								$$=newexpr(programfunc_e);
							else
								$$=newexpr(var_e);
							$$->sym=tmp;
				
							if(tmp==NULL){
								printf("\n\nERROR: There is no member on global scope with the name %s in Line %d\n\n", yytext,yylineno);
								myerror++;
							}
							fprintf(yyout," Ivalue ==> ::id \n");
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
										if($3->type==boolexpr_e)
										{	
											struct expr* tmp=newexpr(constbool_e);
											struct expr* tmp2=newexpr(boolexpr_e);
											struct expr* tmp3=newexpr(constbool_e);
											if(funcounter>0){
												tmp2->sym=CreateSecretVar(counter, scope, yylineno,funcounter,functionoffset,"function locals");
											}else{
												tmp2->sym=CreateSecretVar(counter, scope, yylineno,funcounter,functionoffset,"program variables");
											}
											tmp->boolConst='1';			
											addquad(tablecounter,assign,tmp2,tmp,NULL,-1,yylineno);
											backpatch($3->truelist,tablecounter-1);
											addquad(tablecounter,jump,NULL,NULL,NULL,tablecounter+2,yylineno);
											tmp3->boolConst='0';
											addquad(tablecounter,assign,tmp2,tmp3,NULL,-1,yylineno);
											backpatch($3->falselist,tablecounter-1);
											if(funcounter>0)
												$1=emit_iftableitem($1, counter, scope, yylineno,funcounter,functionoffset,"function locals");
											else
												$1=emit_iftableitem($1, counter, scope, yylineno,funcounter,functionoffset,"program variables");
											$$=newexpr(tableitem_e);
											$$->sym=$1->sym;
											$$->index=tmp2;
										}
										else{
											if(funcounter>0)
												$1=emit_iftableitem($1, counter, scope, yylineno,funcounter,functionoffset,"function locals");
											else
												$1=emit_iftableitem($1, counter, scope, yylineno,funcounter,functionoffset,"program variables");
											$$=newexpr(tableitem_e);
											$$->sym=$1->sym;
											$$->index=$3;
										}
										fprintf(yyout," Member ==> lvalue[expr] \n");}
		| call DOT id	{
					if(funcounter>0)
						$$=member_item($1,$3,counter, scope, yylineno,funcounter,functionoffset,"function locals");
					else
						$$=member_item($1,$3,counter, scope, yylineno,funcounter,functionoffset,"program variables");
					//fprintf(yyout,"pointer $$ : %d \n",$$);
					fprintf(yyout," Member ==> call.id \n");}
		| call LEFT_SQUARE_BRACKET expr RIGHT_SQUARE_BRACKET {
										if($3->type==boolexpr_e)
										{	
											struct expr* tmp=newexpr(constbool_e);
											struct expr* tmp2=newexpr(boolexpr_e);
											struct expr* tmp3=newexpr(constbool_e);
											if(funcounter>0){
												tmp2->sym=CreateSecretVar(counter, scope, yylineno,funcounter,functionoffset,"function locals");
											}else{
												tmp2->sym=CreateSecretVar(counter, scope, yylineno,funcounter,functionoffset,"program variables");
											}
											tmp->boolConst='1';			
											addquad(tablecounter,assign,tmp2,tmp,NULL,-1,yylineno);
											backpatch($3->truelist,tablecounter-1);
											addquad(tablecounter,jump,NULL,NULL,NULL,tablecounter+2,yylineno);
											tmp3->boolConst='0';
											addquad(tablecounter,assign,tmp2,tmp3,NULL,-1,yylineno);
											backpatch($3->falselist,tablecounter-1);

											if(funcounter>0)
												$1=emit_iftableitem($1, counter, scope, yylineno,funcounter,functionoffset,"function locals");
											else
												$1=emit_iftableitem($1, counter, scope, yylineno,funcounter,functionoffset,"program variables");
											$$=newexpr(tableitem_e);
											$$->sym=$1->sym;
											$$->index=tmp2;
										}
										else{
											if(funcounter>0)
												$1=emit_iftableitem($1, counter, scope, yylineno,funcounter,functionoffset,"function locals");
											else
												$1=emit_iftableitem($1, counter, scope, yylineno,funcounter,functionoffset,"program variables");
											$$=newexpr(tableitem_e);
											$$->sym=$1->sym;
											$$->index=$3;
										}

										fprintf(yyout," Member ==> call[expr] \n");}
		;

call:		call LEFT_PARENTHESES elist RIGHT_PARENTHESES	{
									if(funcounter>0)
										$$=make_call(tablecounter,counter, scope, yylineno, funcounter, functionoffset,"function locals",$$,reverseList($3));
									else
										$$=make_call(tablecounter,counter, scope, yylineno, funcounter, functionoffset,"program variables", $$,reverseList($3));					
									fprintf(yyout," CALL ==> call(elist) \n");}
		| lvalue callsuffix	{
						if(funcounter>0)
							$1=emit_iftableitem($1,counter,scope,yylineno,funcounter,functionoffset,"function locals");
						else
							$1=emit_iftableitem($1,counter,scope,yylineno,funcounter,functionoffset,"program variables");
						if($2->method)
						{
							struct expr* t=$1;
							if(funcounter>0)
								$1=emit_iftableitem(member_item(t,$2->name,counter,scope,yylineno,funcounter,functionoffset,"function locals"),counter,scope,yylineno,funcounter,functionoffset,"function locals");
							else
								$1=emit_iftableitem(member_item(t,$2->name,counter,scope,yylineno,funcounter,functionoffset,"program variables"),counter,scope,yylineno,funcounter,functionoffset,"program variables");
							t->next=$2->elist;
							$2->elist=t;

						}
						//printf("AAAAAA...%s\n",$1->sym->name);
						if(funcounter>0)
							$$=make_call(tablecounter,counter, scope, yylineno, funcounter, functionoffset,"function locals", $1,reverseList($2->elist));
						else
							$$=make_call(tablecounter,counter, scope, yylineno, funcounter, functionoffset,"program variables", $1, reverseList($2->elist));
						//printf("AAAAAA...%s\n",$$->sym->name);
						fprintf(yyout," call ==> ivalue callsuffix \n");} 
		| LEFT_PARENTHESES funcdef RIGHT_PARENTHESES LEFT_PARENTHESES elist RIGHT_PARENTHESES { struct expr* func=newexpr(programfunc_e);
													func=$2;
													if(funcounter>0)
														$$=make_call(tablecounter,counter, scope, yylineno, funcounter, functionoffset,"function locals", func,reverseList($5));
													else
														$$=make_call(tablecounter,counter, scope, yylineno, funcounter, functionoffset,"program variables", func, reverseList($5));
													fprintf(yyout," call ==> (funcdef)(elist) \n");
		}
		;

callsuffix:	normcall	{$$=$1;fprintf(yyout," callsuffix ==> normcall \n");}
		| methodcall	{$$=$1;fprintf(yyout," callsuffix ==> methodcall \n");}
		;

normcall:	LEFT_PARENTHESES elist RIGHT_PARENTHESES {$$=(struct call*)malloc(sizeof(struct call));
								//printf("%s\n",getExpr_t($2->type));
							  $$->elist=$2;
							  $$->method=0;
							  $$->name=NULL;
							  fprintf(yyout," normcall ==> (elist) \n");
		}
		;	

methodcall:	DOUBLE_DOT id LEFT_PARENTHESES elist RIGHT_PARENTHESES{
										$$=(struct call*)malloc(sizeof(struct call));
										$$->elist=$4;
										$$->method=1;
										$$->name=$2;
										fprintf(yyout," methodcall ==> ..id(elist) \n");
		}
		;

elist:	 	expr{
			if($1->type==boolexpr_e)
			{	
						struct expr* tmp=newexpr(constbool_e);
						struct expr* tmp2=newexpr(boolexpr_e);
						struct expr* tmp3=newexpr(constbool_e);
						if(funcounter>0){
							tmp2->sym=CreateSecretVar(counter, scope, yylineno,funcounter,functionoffset,"function locals");
						}else{
							tmp2->sym=CreateSecretVar(counter, scope, yylineno,funcounter,functionoffset,"program variables");
						}
						tmp->boolConst='1';			
						addquad(tablecounter,assign,tmp2,tmp,NULL,-1,yylineno);
						backpatch($1->truelist,tablecounter-1);
						addquad(tablecounter,jump,NULL,NULL,NULL,tablecounter+2,yylineno);
						tmp3->boolConst='0';
						addquad(tablecounter,assign,tmp2,tmp3,NULL,-1,yylineno);
						backpatch($1->falselist,tablecounter-1);
						//logic_expr_flag=0;
						$1=tmp2;
			}
		      } elist1	{
					$1->next=$3;
					$$=$1;			
					fprintf(yyout," elist ==> expr elist1 \n");}
		| /* empty */	{$$=NULL;fprintf(yyout," elist ==>  \n");}
		;

elist1:		COMMA expr{
			if($2->type==boolexpr_e)
			{	
				struct expr* tmp=newexpr(constbool_e);
				struct expr* tmp2=newexpr(boolexpr_e);
				struct expr* tmp3=newexpr(constbool_e);
				if(funcounter>0){
					tmp2->sym=CreateSecretVar(counter, scope, yylineno,funcounter,functionoffset,"function locals");
				}else{
					tmp2->sym=CreateSecretVar(counter, scope, yylineno,funcounter,functionoffset,"program variables");
				}
				tmp->boolConst='1';			
				addquad(tablecounter,assign,tmp2,tmp,NULL,-1,yylineno);
				backpatch($2->truelist,tablecounter-1);
				addquad(tablecounter,jump,NULL,NULL,NULL,tablecounter+2,yylineno);
				tmp3->boolConst='0';
				addquad(tablecounter,assign,tmp2,tmp3,NULL,-1,yylineno);
				backpatch($2->falselist,tablecounter-1);
				$2=tmp2;
			}
		      } elist1{
					$2->next=$4;
					$$=$2;
					fprintf(yyout," elist ==> ,expr elist1 \n");}
		| /* empty */	{$$=NULL;fprintf(yyout," elist1 ==>  \n");}
		;

objectdef:	LEFT_SQUARE_BRACKET elist RIGHT_SQUARE_BRACKET	{
									int i=0;
									tablecreate_flag=1;
									if (block_flag==1)
										counter=0;
									struct expr* t=newexpr(newtable_e);
									struct expr* tmp=$2;
									
									t->sym=CreateSecretVar(counter, scope, yylineno,funcounter,functionoffset,"function locals");
									addquad(tablecounter,tablecreate,NULL,t,NULL,-1,yylineno);
									while(tmp!=NULL)
									{
										addquad(tablecounter,tablesetelem,t,newexpr_constnum(i),tmp,-1,yylineno);
										tmp=tmp->next;
										i++;
									}
									$$=t;
									fprintf(yyout," objectdef ==> [elist] \n");}
		| LEFT_SQUARE_BRACKET indexed RIGHT_SQUARE_BRACKET {
									struct expr* tmp=$2;
									struct expr* t=newexpr(newtable_e);
									t->sym=CreateSecretVar(counter, scope, yylineno,funcounter,functionoffset,"function locals");
									addquad(tablecounter,tablecreate,NULL,t,NULL,-1,yylineno);
									while(tmp!=NULL)
									{
										//printf("index:%.0f, value:%.0f\n",tmp->index->numConst,tmp->numConst);
										addquad(tablecounter,tablesetelem,t,tmp->index,tmp,-1,yylineno);
										tmp=tmp->next;
									}
									$$=t;
									fprintf(yyout," objectdef ==> [indexed] \n");}
		;

indexed:	indexedelem indexed1	{$1->next=$2;
					 $$=$1;
					 fprintf(yyout," indexed ==> indexedelem indexed1 \n");
		}
		;

indexed1: 	COMMA indexedelem indexed1{	$2->next=$3;
						$$=$2;
						fprintf(yyout," indexed ==> indexedelem indexed1 \n");}
		| /* empty */	{$$=NULL;fprintf(yyout," indexed ==>   \n");}
		;

indexedelem: 	LEFT_CURLY_BRACKET expr COLON expr RIGHT_CURLY_BRACKET{ 

									if($2->type==boolexpr_e)
									{	
										struct expr* tmp=newexpr(constbool_e);
										struct expr* tmp2=newexpr(boolexpr_e);
										struct expr* tmp3=newexpr(constbool_e);
										if(funcounter>0){
											tmp2->sym=CreateSecretVar(counter, scope, yylineno,funcounter,functionoffset,"function locals");
										}else{
											tmp2->sym=CreateSecretVar(counter, scope, yylineno,funcounter,functionoffset,"program variables");
										}
										tmp->boolConst='1';			
										addquad(tablecounter,assign,tmp2,tmp,NULL,-1,yylineno);
										backpatch($2->truelist,tablecounter-1);
										addquad(tablecounter,jump,NULL,NULL,NULL,tablecounter+2,yylineno);
										tmp3->boolConst='0';
										addquad(tablecounter,assign,tmp2,tmp3,NULL,-1,yylineno);
										backpatch($2->falselist,tablecounter-1);
										$2=tmp2;
									}
									if($4->type==boolexpr_e)
									{	
										struct expr* tmp=newexpr(constbool_e);
										struct expr* tmp2=newexpr(boolexpr_e);
										struct expr* tmp3=newexpr(constbool_e);
										if(funcounter>0){
											tmp2->sym=CreateSecretVar(counter, scope, yylineno,funcounter,functionoffset,"function locals");
										}else{
											tmp2->sym=CreateSecretVar(counter, scope, yylineno,funcounter,functionoffset,"program variables");
										}
										tmp->boolConst='1';			
										addquad(tablecounter,assign,tmp2,tmp,NULL,-1,yylineno);
										backpatch($4->truelist,tablecounter-1);
										addquad(tablecounter,jump,NULL,NULL,NULL,tablecounter+2,yylineno);
										tmp3->boolConst='0';
										addquad(tablecounter,assign,tmp2,tmp3,NULL,-1,yylineno);
										backpatch($4->falselist,tablecounter-1);
										$4=tmp2;
									}
									$$=$4;
									$$->index=$2; 
									fprintf(yyout," indexedelem ==> { expr : expr } \n");
		}
		;

block:		LEFT_CURLY_BRACKET {scope++; } stamt RIGHT_CURLY_BRACKET {
										Hide(ScopeTable,scope);
										scope--;
										
										/*if(counter==0 && return_flag!=0)
										{
											printf("\n\n\n\n MESA STO IF\n\n\n");
											//counter=LastTempInScope(ScopeTable,scope)+1;
											counter=CreateSecretScope(counter, scope);
											counter--;
											block_flag=1;
										}
										
										return_flag=0;
										*/
										$$=$3;
										fprintf(yyout," block ==> { [stmt] } \n");}
		;

funcname:	id 		{$$=yytext;}
		| /* empty */	{char* name=(char *)malloc(sizeof(char));
		 		 char* num=(char *)malloc(sizeof(char));
				 sprintf(name, "%s", "$f");
				 sprintf(num, "%d", numname);			
				 strcat(name,num);
				 $$=name;
				 numname++;
				}
		;

funprefix:	FUNCTION funcname{
				//addfuncjump(tablecounter);
				//addquad(tablecounter,jump,NULL,NULL,NULL,-1,yylineno);
				struct SymTableEntry *tmp;
				tmp=NameLookUpInScope(ScopeTable,scope,yytext);
				$$=NULL;
				if(tmp!=NULL && strcmp("library function", tmp->type)!=0 ){
					fprintf(yyout,"\n\nERROR: name %s already exists in same scope in line %d\n\n",yytext,yylineno);
					myerror++;
				}
				if(collisionLibFun(ScopeTable,yytext)==1){
					fprintf(yyout,"\n\nERROR: function %s: Trying to shadow Library Function in line %d\n\n",yytext,yylineno);
					myerror++;
				}else if (tmp==NULL && collisionLibFun(ScopeTable,yytext)==0){		
					$$=newexpr(programfunc_e);
					$$->sym=insertNodeToHash(Head,$2,"user function",scope,yylineno, -1,"",1);
					/*$$->breakList_while=NULL;
					$$->contList_while=NULL;
					$$->breakList_for=NULL;
					$$->contList_for=NULL;*/
					addquad(tablecounter,funcstart,$$,NULL,NULL,-1,yylineno);
				}
				funcounter++;
		}

funcblockstart:	/* empty */  { addtoloopbc(); 
			       //printloops();
				fprintf(yyout," funcblockstart\n");
		}
		;

funcblockend: 	/* empty */  {subloopbc(); 
			      //printloops();
				fprintf(yyout," funcblockend\n");
		}
		;


funcargs:	LEFT_PARENTHESES {scope++;} idlist RIGHT_PARENTHESES { offset_arg=0; 
								       scope--; 
								       infunction++;
								     }
		;

funcbody:	funcblockstart block funcblockend 
		;


funcdef: 	funprefix funcargs funcbody {	if($1!=NULL)
						{functionoffset[funcounter]=0; 
			 	     		funcounter--; 
			  			infunction--;
			  			addquad(tablecounter,funcend,$1,NULL,NULL,-1,yylineno);
						//quadtable[subfuncjump()]->label=tablecounter;
						$$=$1;}
			 			fprintf(yyout," funcdef ==> function(){} \n");
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
			
			if(tmp!=NULL && strcmp("library function", tmp->type)!=0){
				fprintf(yyout,"\n\nERROR: name %s: formal redeclaration in line %d\n\n",yytext,yylineno);
				myerror++;
			}
			if(collisionLibFun(ScopeTable,yytext)==1){
				fprintf(yyout,"\n\nERROR: function %s: Trying to shadow Library Function in line %d\n\n",yytext,yylineno);
				myerror++;
			}else if (tmp==NULL && collisionLibFun(ScopeTable,yytext)==0)
				{insertNodeToHash(Head,yytext,"formal argument",scope,yylineno, offset_arg,"formal arguments",1);
				 offset_arg++;}
		   } idlist1	{fprintf(yyout, " idlist ==> id,id*;\n");}
		| /* empty */	{fprintf(yyout, " idlist ==> \n");}
		;	

idlist1:	COMMA id {
		   	insertArgToNode(ScopeTable,yytext,scope);
			struct SymTableEntry *tmp;
			tmp=NameLookUpInScope(ScopeTable,scope,yytext);
			if(tmp!=NULL && strcmp("library function", tmp->type)!=0){
				fprintf(yyout,"\n\nERROR: name %s: formal redeclaration in line %d\n\n",yytext,yylineno);
				myerror++;
			}
			if(collisionLibFun(ScopeTable,yytext)==1){
				fprintf(yyout,"\n\nERROR: function %s: Trying to shadow Library Function in line %d\n\n",yytext,yylineno);
				myerror++;
			}else if (tmp==NULL && collisionLibFun(ScopeTable,yytext)==0)
				{insertNodeToHash(Head,yytext,"formal argument",scope,yylineno, offset_arg,"formal arguments",1);
				 offset_arg++;}
			 
		   } idlist1	{fprintf(yyout, " idlist ==> id,id*;\n");}
		| /* empty */	{fprintf(yyout, " idlist ==>   \n");}
		;

ifstmt:		ifprefix stmt{
					quadtable[$1]->label=tablecounter;	//patchlabel
					$$=$2;
					fprintf(yyout, " ifstmt ==> IF THEN;\n");}
		| ifprefix stmt elseprefix stmt	{
					quadtable[$1]->label=$3+1;
					quadtable[$3]->label=tablecounter;
					fprintf(yyout, " ifstmt ==> IF THEN ELSE;\n");
		}
		;
		
elseprefix: ELSE{
					$$=tablecounter;
					addquad(tablecounter,jump,NULL,NULL,NULL,0,yylineno);
			};
		

		
ifprefix: IF LEFT_PARENTHESES expr RIGHT_PARENTHESES{
					struct truefalse *true=NULL, *false=NULL;
					struct expr* tmp=newexpr(boolexpr_e);
					struct expr* tempo=newexpr(boolexpr_e);
					tmp->boolConst='1';	
					backpatch($3->truelist,tablecounter);
					backpatch($3->falselist,tablecounter+2);
					//$3=tmp;
					if($3->type==boolexpr_e){
						struct expr* tmp1=newexpr(constbool_e);
						struct expr* tmp2=newexpr(boolexpr_e);
						struct expr* tmp3=newexpr(constbool_e);
						if(funcounter>0){
							tmp2->sym=CreateSecretVar(counter, scope, yylineno,funcounter,functionoffset,"function locals");
						}else{
							tmp2->sym=CreateSecretVar(counter, scope, yylineno,funcounter,functionoffset,"program variables");
						}
						tmp1->boolConst='1';			
						addquad(tablecounter,assign,tmp2,tmp1,NULL,-1,yylineno);
						backpatch($3->truelist,tablecounter-1);
						addquad(tablecounter,jump,NULL,NULL,NULL,tablecounter+2,yylineno);
						tmp3->boolConst='0';
						addquad(tablecounter,assign,tmp2,tmp3,NULL,-1,yylineno);
						backpatch($3->falselist,tablecounter-1);
						addquad(tablecounter,if_eq,tmp2,newexpr_constbool('1'),NULL,tablecounter+2,yylineno);
						$$=tablecounter;
						addquad(tablecounter,jump,NULL,NULL,NULL,0,yylineno);
					}
					else{
						addquad(tablecounter,if_eq,$3,newexpr_constbool('1'),NULL,tablecounter+2,yylineno);
						$$=tablecounter;
						addquad(tablecounter,jump,NULL,NULL,NULL,0,yylineno);
					}
		};

loopstart: 	/* empty */ {	//printloops();
				struct loopbc* tmp;
				tmp=whereat();
				tmp->loopcounter=tmp->loopcounter+1;
				fprintf(yyout," loopstart\n");
				//printloops();
		}
		;

loopend: 	/* empty */ {	//printloops();
				struct loopbc* tmp;
				tmp=whereat();
				tmp->loopcounter=tmp->loopcounter-1;
				fprintf(yyout," loopend\n");
				//printloops();
		}
		;

loopstmt: 	loopstart stmt loopend	{$$=$2;   fprintf(yyout," loopstmt\n");}
		;


whilestart:	WHILE{	$$=tablecounter;}
		;

whilecond:	LEFT_PARENTHESES expr RIGHT_PARENTHESES{
								
								if($2->type==boolexpr_e)
								{	
									struct expr* tmp=newexpr(constbool_e);
									struct expr* tmp2=newexpr(boolexpr_e);
									if(funcounter>0){
										tmp2->sym=CreateSecretVar(counter, scope, yylineno,funcounter,functionoffset,"function locals");
									}else{
										tmp2->sym=CreateSecretVar(counter, scope, yylineno,funcounter,functionoffset,"program variables");
									}
									tmp->boolConst='1';			
									addquad(tablecounter,assign,tmp2,tmp,NULL,-1,yylineno);
									backpatch($2->truelist,tablecounter-1);
									addquad(tablecounter,jump,NULL,NULL,NULL,tablecounter+2,yylineno);
									
									struct expr* tmp3=newexpr(constbool_e);
									tmp3->boolConst='0';
									addquad(tablecounter,assign,tmp2,tmp3,NULL,-1,yylineno);
									backpatch($2->falselist,tablecounter-1);
									

									addquad(tablecounter,if_eq,tmp2,newexpr_constbool('1'),NULL,tablecounter+2,yylineno);
									$$=tablecounter;
									addquad(tablecounter,jump,NULL,NULL,NULL,0,yylineno);
									
								}
								else{

									addquad(tablecounter,if_eq,$2,newexpr_constbool('1'),NULL,tablecounter+2,yylineno);
									$$=tablecounter;
									addquad(tablecounter,jump,NULL,NULL,NULL,-1,yylineno);
								}
								//logic_expr_flag=0;
							}
		;



whilestmt :	whilestart whilecond {while_counter++;addToPlaceList(0);} loopstmt {
						
						addquad(tablecounter,jump,NULL,NULL,NULL,$1,yylineno);
						quadtable[$2]->label=tablecounter;
						/*if($4!=NULL)
						{if($4->breakList_while!=NULL)
							patchlist($4->breakList_while,tablecounter,while_counter,0);
						if($4->contList_while!=NULL)
							patchlist($4->contList_while,$1,while_counter,0);}*/
						//$$=$3;

						if(breakList_while!=NULL)
							breakList_while=patchlist(breakList_while,tablecounter,while_counter,0);
						if(contList_while!=NULL)
							contList_while=patchlist(contList_while,$1,while_counter,0);
						$$=$4;
						while_counter--;
						removeFirstInPlaceList();
						fprintf(yyout," whilestmt==> while(expr) stmt \n");
					  }
		;


forprefix:	FOR LEFT_PARENTHESES elist SEMI_COLON M expr SEMI_COLON {
				for_counter++;
				addToPlaceList(1);
				struct expr* tmp=newexpr(constbool_e);
				struct expr* tmp2=newexpr(boolexpr_e);
				struct expr* tmp3=newexpr(constbool_e);
				if($6->type==boolexpr_e)
				{
					if(funcounter>0){
						tmp2->sym=CreateSecretVar(counter, scope, yylineno,funcounter,functionoffset,"function locals");
					}else{
						tmp2->sym=CreateSecretVar(counter, scope, yylineno,funcounter,functionoffset,"program variables");
					}
					tmp->boolConst='1';	
						
					addquad(tablecounter,assign,tmp2,tmp,NULL,-1,yylineno);
					backpatch($6->truelist,tablecounter-1);
					addquad(tablecounter,jump,NULL,NULL,NULL,tablecounter+2,yylineno);
					tmp3->boolConst='0';
					addquad(tablecounter,assign,tmp2,tmp3,NULL,-1,yylineno);
					backpatch($6->falselist,tablecounter-1);
				}
				if(funcounter>0){
					$6->sym=CreateSecretVar(counter, scope, yylineno,funcounter,functionoffset,"function locals");
				}else{
					$6->sym=CreateSecretVar(counter, scope, yylineno,funcounter,functionoffset,"program variables");
				}

				struct forpref* tmp1= (struct forpref*)malloc(sizeof(struct forpref));
				$$=tmp1;
				$$->test = $5;
				$$->enter = tablecounter;
				addquad(tablecounter,if_eq,$6,newexpr_constbool('1'),NULL,-1,yylineno);
				fprintf(yyout," forstmt ==> (elist;expr; \n");
		}
		;

forstmt: 	forprefix N elist RIGHT_PARENTHESES N loopstmt N {
				quadtable[$1->enter]->label=$5+1;
				quadtable[$2]->label=tablecounter;
				quadtable[$5]->label=$1->test;
				quadtable[$7]->label=$2+1;
				
				/*if($6!=NULL)
				{if($6->breakList_for!=NULL) 
					patchlist($6->breakList_for,tablecounter,for_counter,1);
				if($6->contList_for!=NULL)
					patchlist($6->contList_for,$2+1,for_counter,1);}*/

				if(breakList_for!=NULL)
					breakList_for=patchlist(breakList_for,tablecounter,for_counter,1);
				if(contList_for!=NULL)
					contList_for=patchlist(contList_for,$2+1,for_counter,1);
				
				$$=$6;
				for_counter--;
				removeFirstInPlaceList();
				fprintf(yyout," forstmt ==> (elist;expr;elist)stmt \n");
		}
		;

N: 		/* empty */ { $$ = tablecounter; 
			      addquad(tablecounter,jump,NULL,NULL,NULL,-1,yylineno); 
		}
		;


returnstmt:	RETURN SEMI_COLON {	
					return_flag=1;
					if(funcounter>0){
						addquad(tablecounter,Return,NULL,NULL,NULL,-1,yylineno);
					}else{
						printf("\n\nERROR: return; is not inside a function in line %d\n\n",yylineno);
						myerror++;
					}
					fprintf(yyout," returnstmt ==> return ;\n");
		}
		| RETURN expr SEMI_COLON {	
						return_flag=1;
						if((funcounter>0) && strcmp(getExpr_t($2->type),"nil_e")==0){
							addquad(tablecounter,Return,NULL,NULL,NULL,-1,yylineno);
						}else if(funcounter>0){
							if($2->type==boolexpr_e)
							{	
								struct expr* tmp=newexpr(constbool_e);
								struct expr* tmp2=newexpr(boolexpr_e);
								struct expr* tmp3=newexpr(constbool_e);
								if(funcounter>0){
									tmp2->sym=CreateSecretVar(counter, scope, yylineno,funcounter,functionoffset,"function locals");
								}else{
									tmp2->sym=CreateSecretVar(counter, scope, yylineno,funcounter,functionoffset,"program variables");
								}
								tmp->boolConst='1';			
								addquad(tablecounter,assign,tmp2,tmp,NULL,-1,yylineno);
								backpatch($2->truelist,tablecounter-1);
								addquad(tablecounter,jump,NULL,NULL,NULL,tablecounter+2,yylineno);
								tmp3->boolConst='0';
								addquad(tablecounter,assign,tmp2,tmp3,NULL,-1,yylineno);
								backpatch($2->falselist,tablecounter-1);
								addquad(tablecounter,Return,tmp2,NULL,NULL,-1,yylineno);
							}
							else
								addquad(tablecounter,Return,$2,NULL,NULL,-1,yylineno);
						}else{
							printf("\n\nERROR: return expr; is not inside a function in line %d\n\n",yylineno);
							myerror++;
						}
						fprintf(yyout," returnstmt ==> return expr;\n");
		}
		;

%%

int yyerror (char* yaccProvidedMessage)
{
	fprintf(stderr, "%s: at line %d, before token: '%s'\n", yaccProvidedMessage, yylineno, yytext);
	fprintf(stderr, "INPUT NOT VALID\n");
	myerror++;
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
	createloopbc();
	createfuncjump();
	yyparse();
	if(myerror>0){
		exit(1);
	}
	printf("\n\n");
	printScopeTable(ScopeTable);
	printf("\n\n");
	printQuad();
	makeQuadtxt();
	return 0;
}
