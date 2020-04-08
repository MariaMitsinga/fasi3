int total=1;
int tablecounter=0;
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

void tablequadcreation(){
	int i;
	quadtable =malloc(EXPAND_SIZE * sizeof(temptq));
	for(i=0;i<40;i++) {
		quadtable[i]=NULL;	
	}
}

/*void domalloc(int size){
	quadtable[size]=malloc(sizeof(struct quad));
}*/

void addtofunctionoffset(int size,enum iopcode op,struct expr* result,struct expr* arg1,struct expr* arg2,unsigned int label,unsigned int line){
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

struct SymTableEntry * CreateSecretVar(int counter, int scope, int yylineno,int funcounter,int functionoffset[],char* space){
	int i;
	char* name=(char *)malloc(sizeof(char));
	char* num=(char *)malloc(sizeof(char));	
	struct SymTableEntry *tmp,*tmp2;		
	sprintf(name, "%s", "_t");
	sprintf(num, "%d", counter);			
	strcat(name,num);
	//counter++; 
	
	tmp=insertNodeToHash(Head,name,"hidden variable",scope,yylineno,functionoffset[funcounter],space,1);
	free(name);
	free(num);
	functionoffset[funcounter]=functionoffset[funcounter]+1;
	return tmp;
}
