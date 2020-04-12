int total=1;
int tablecounter=0;
extern counter;
#define EXPAND_SIZE 40
#define CURR_SIZE (total*EXPAND_SIZE)
#define NEW_SIZE (EXPAND_SIZE + CURR_SIZE)

enum iopcode{	
	assign, add, sub, mul, DIV, mod,
	uminus, and, or, not, if_eq, if_noteq,
	if_lesseq, if_greatereq, if_less,
	if_greater, jump, call,
	param, Return, getretval,
	funcstart, funcend, tablecreate,
	tablegetelem, tablesetelem
};

enum expr_t{
	var_e,
	tableitem_e,
	programfunc_e,
	libraryfunc_e,
	arithexpr_e,
	boolexpr_e,
	assignexpr_e,
	newtable_e,
	constnum_e,
	constbool_e,
	conststring_e,
	nil_e
};

struct expr{
	enum expr_t type;
	struct SymTableEntry* sym;
	struct expr* index;
	double numConst;
	char* strConst;
	unsigned char boolConst;
	struct expr* next;
};

struct quad{
	enum iopcode op;
	struct expr* result;
	struct expr* arg1;
	struct expr* arg2;
	unsigned int label;
	unsigned int line;
};

int funcounter=0;
int *functionoffset; 
typedef struct quad *temptq;
temptq *quadtable;

const char* getiopcode(enum iopcode op){
	switch (op){
      		case assign:		return "assign";
		case add: 		return "add";
		case sub: 		return "sub";
		case mul: 		return "mul";
		case DIV: 		return "div";
		case mod: 		return "mod";
		case uminus: 		return "uminus";
		case and: 		return "and";
		case or: 		return "or"; 
		case not: 		return "not";
		case if_eq: 		return "if_eq";
		case if_noteq: 		return "if_noteq";
		case if_lesseq: 	return "if_lesseq";
		case if_greatereq: 	return "if_greatereq";	
		case if_less: 		return "if_less";
		case if_greater: 	return "if_greater";
		case jump: 		return "jump";
		case call: 		return "call";
		case param: 		return "param";
		case Return: 		return "return";
		case getretval: 	return "getretval";
		case funcstart: 	return "funcstart";
		case funcend: 		return "funcend";
		case tablecreate: 	return "tablecreate";
		case tablegetelem: 	return "tablegetelem";
		case tablesetelem: 	return "tablesetelem";
   	}
}


const char* getExpr_t(enum expr_t type){
	switch (type){
      		case var_e:			return "var_e";
		case tableitem_e: 		return "tableitem_e";
		case programfunc_e: 		return "programfunc_e";
		case libraryfunc_e: 		return "libraryfunc_e";
		case arithexpr_e: 		return "arithexpr_e";
		case boolexpr_e: 		return "boolexpr_e";
		case assignexpr_e: 		return "assignexpr_e";
		case newtable_e: 		return "newtable_e";
		case constnum_e: 		return "constnum_e"; 
		case constbool_e: 		return "constbool_e";
		case conststring_e: 		return "conststring_e";
		case nil_e: 			return "nil_e";
   	}
}



void tablequadcreation(){
	int i;
	quadtable =malloc(EXPAND_SIZE * sizeof(temptq));
	for(i=0;i<40;i++) {
		quadtable[i]=NULL;	
	}
}

void domalloc(int size){
	quadtable[size]=malloc(sizeof(struct quad));
}

void addquad(int size,enum iopcode op,struct expr* result,struct expr* arg1,struct expr* arg2,unsigned int label,unsigned int line){
	quadtable[size]=malloc(sizeof(struct quad));
	quadtable[size]->op=op;
	quadtable[size]->result=result;
	quadtable[size]->arg1=arg1;
	quadtable[size]->arg2=arg2;
	quadtable[size]->label=label;
	quadtable[size]->line=line;
}

void realloctablequad(){
	int i;
	quadtable=realloc(quadtable, NEW_SIZE * sizeof(temptq)); 
	for(i=CURR_SIZE;i<NEW_SIZE;i++){
		quadtable[i]=NULL;
	}
	total++;
}

void printfunstionoffset(){
	int i=0;
	while(tablecounter>i){
		if(quadtable[i]->arg2!=NULL)
			printf("\n%s %s %.0f %.0f",getiopcode(quadtable[i]->op),quadtable[i]->result->sym->name,quadtable[i]->arg1->numConst,quadtable[i]->arg2->numConst);
		else
			printf("\n%s %s %.0f",getiopcode(quadtable[i]->op),quadtable[i]->result->sym->name,quadtable[i]->arg1->numConst);
		i++;
	}
}


void functionoffsetcreation(){
	int i=0;
	functionoffset=(int*)malloc(40 * sizeof(int)); 
	for(i=0;i<40;i++) functionoffset[i]=0;
}

struct expr* newexpr(enum expr_t type)
{
	struct expr* tmp= (struct expr*)malloc(sizeof(struct expr));
	tmp->type=type;
	return tmp;
}

struct SymTableEntry * CreateSecretVar(int coun, int scope, int yylineno,int funcounter,int functionoffset[],char* space){
	int i;
	char* name=(char *)malloc(sizeof(char));
	char* num=(char *)malloc(sizeof(char));	
	struct SymTableEntry *tmp,*tmp2;		
	sprintf(name, "%s", "_t");
	sprintf(num, "%d", coun);			
	strcat(name,num);
	counter++; 
	
	tmp=insertNodeToHash(Head,name,"hidden variable",scope,yylineno,functionoffset[funcounter],space,1);
	free(name);
	free(num);
	functionoffset[funcounter]=functionoffset[funcounter]+1;
	return tmp;
}

struct expr* setTypeOfExpr(struct expr* expr,struct SymTableEntry * sym)
{
	if ( strcmp("global variable", sym->type)==0 || strcmp("local variable", sym->type)==0 || strcmp("formal argument", sym->type)==0 || strcmp("hidden variable", sym->type)==0)
		expr->type=var_e;
	if ( strcmp("user function", sym->type)==0 )
		expr->type=programfunc_e;
	if( strcmp("library function", sym->type)==0 )
		expr->type=libraryfunc_e;
	return expr;
}

void emit(enum iopcode op,struct expr* arg1,struct expr* arg2,struct expr* result,unsigned int label,unsigned int line)
{
	
	domalloc(tablecounter);
	//printf("emit\n");
	quadtable[tablecounter]->op=op;
	quadtable[tablecounter]->arg1=arg1;
	//printf("%s\n",arg1->sym->name);
	quadtable[tablecounter]->arg2=arg2;
	quadtable[tablecounter]->result=result;
	quadtable[tablecounter]->label=label;
	quadtable[tablecounter]->line=line;
	tablecounter++;
}


struct expr* emit_iftableitem(struct expr* e,int counter, int scope, int yylineno,int funcounter,int functionoffset[],char* space)
{
	if(e->type!=tableitem_e)
		return e;
	else
	{
		struct expr* result=newexpr(var_e);
		result->sym=CreateSecretVar(counter,scope,yylineno,funcounter,functionoffset,space);
		counter++;
		//printf("from emit_iftableitem: %s\n",e->sym->name);
		emit(tablegetelem,e,e->index,result,-1,yylineno);
		return result;
	}
	return NULL;
}


struct expr * newexpr_conststring(const char* s)
{
	struct expr *e = newexpr(conststring_e);
	e->strConst = strdup(s);
	//printf("from quad:%s\n",e->strConst);
	return e;	
}

struct expr * member_item(struct expr * lv,const char*name,int counter, int scope, int yylineno,int funcounter,int functionoffset[],char* space)
{
	lv=emit_iftableitem(lv,counter,scope,yylineno,funcounter,functionoffset,space);
	struct expr* ti=newexpr(tableitem_e);
	ti->sym=lv->sym;
	//printf("from quad member :%s\n",ti->sym->name);
	ti->index=newexpr_conststring(name);
	//printf("from quad member :%d\n",ti);
	return ti;
}


void printQuad()
{
	int i=0;
	printf("quad# \t opcode \t result \t arg1 \t arg2 \t label\n");
	printf("--------------------------------------------------------------------------------------\n");
	for(i=0;i<40;i++)
	{
		if(quadtable[i]==NULL)
			continue;
		switch(quadtable[i]->op)
		{	
			case tablegetelem:
				printf("%d:\t %s \t %s \t\t %s \t \"%s\" \n",i,"tablegetelem",quadtable[i]->result->sym->name,quadtable[i]->arg1->sym->name,quadtable[i]->arg1->index->strConst);
				break;
			case add:
				//printf("op: %s\n",getExpr_t(quadtable[i]->arg1->type));
				if( strcmp(getExpr_t(quadtable[i]->arg1->type),"constnum_e")==0 && strcmp(getExpr_t(quadtable[i]->arg2->type),"constnum_e")==0 ){	
					printf("%d:\t %s \t\t %s\t\t %.0f\t %.0f\n",i,"add",quadtable[i]->result->sym->name,quadtable[i]->arg1->numConst,quadtable[i]->arg2->numConst);
				}
				if( strcmp(getExpr_t(quadtable[i]->arg1->type),"var_e")==0 && strcmp(getExpr_t(quadtable[i]->arg2->type),"constnum_e")==0 ){	
					printf("%d:\t %s \t\t %s\t\t %s\t %.0f\n",i,"add",quadtable[i]->result->sym->name,quadtable[i]->arg1->sym->name,quadtable[i]->arg2->numConst);
				}
				if( strcmp(getExpr_t(quadtable[i]->arg1->type),"constnum_e")==0 && strcmp(getExpr_t(quadtable[i]->arg2->type),"var_e")==0 ){	
					printf("%d:\t %s \t\t %s\t\t %.0f\t %s\n",i,"add",quadtable[i]->result->sym->name,quadtable[i]->arg1->numConst,quadtable[i]->arg2->sym->name);
				}
				if( strcmp(getExpr_t(quadtable[i]->arg1->type),"var_e")==0 && strcmp(getExpr_t(quadtable[i]->arg2->type),"var_e")==0 ){	
					printf("%d:\t %s \t\t %s\t\t %s\t %s\n",i,"add",quadtable[i]->result->sym->name,quadtable[i]->arg1->sym->name,quadtable[i]->arg2->sym->name);
				}
				if( strcmp(getExpr_t(quadtable[i]->arg1->type),"arithexpr_e")==0 && strcmp(getExpr_t(quadtable[i]->arg2->type),"constnum_e")==0 ){	
					printf("%d:\t %s \t\t %s\t\t %s\t %.0f\n",i,"add",quadtable[i]->result->sym->name,quadtable[i]->arg1->sym->name,quadtable[i]->arg2->numConst);
				}
				break;
			case tablesetelem:
				printf("%d:\t %s \t %s \t\t \"%s\" \t %s \n",i,"tablesetelem",quadtable[i]->result->sym->name,quadtable[i]->result->index->strConst,quadtable[i]->arg2->sym->name);
				break;
			case assign:
				if(quadtable[i]->arg1->sym==NULL)
					printf("%d:\t %s \t %s \t\t %d \n",i,"assign",quadtable[i]->result->sym->name,quadtable[i]->arg1->numConst);
				else	
					printf("%d:\t %s \t %s \t\t %s \n",i,"assign",quadtable[i]->result->sym->name,quadtable[i]->arg1->sym->name);
				break;
			default:
				printf("the end\n");
		}
	}

}

