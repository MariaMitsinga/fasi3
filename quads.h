int total=1;
#define EXPAND_SIZE 40
#define CURR_SIZE (total*EXPAND_SIZE)
#define NEW_SIZE (EXPAND_SIZE + CURR_SIZE)

enum iopcode{	
	assign, add, sub, mul, DIV, mod,
	uminus, and, or, not, if_eq, if_noteq,
	if_lesseq, if_geatereq,	if_less,
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
	costnum_e,
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
	for(i=0;i<40;i++) quadtable[i]=NULL;
	quadtable[20]=malloc(sizeof(struct quad));
	quadtable[20]->line=5;
	printf("\n%d\n",quadtable[20]->line);
}
void addtotablequad(){
	int i;
	quadtable=realloc(quadtable, NEW_SIZE * sizeof(temptq)); 
	for(i=CURR_SIZE;i<NEW_SIZE;i++) quadtable[i]=NULL;
	quadtable[70]=malloc(sizeof(struct quad));
	quadtable[70]->line=20;
	printf("\n%d\n",quadtable[20]->line);
	printf("\n%d\n",quadtable[70]->line);
	total++;
}

struct expr* newexpr(enum expr_t type)
{
	struct expr* tmp= (struct expr*)malloc(sizeof(struct expr));
	tmp->type=type;
	return tmp;
}

void functionoffsetcreation(){
	int i=0;
	functionoffset=(int*)malloc(40 * sizeof(int)); 
	for(i=0;i<40;i++) functionoffset[i]=0;
}

int CreateSecretVar(int counter, int scope, int yylineno,int funcounter,int functionoffset[],char* space){
	int i;
	char* name=(char *)malloc(sizeof(char));
	char* num=(char *)malloc(sizeof(char));	
	struct SymTableEntry *tmp,*tmp2;	
			
	//do{
		
		sprintf(name, "%s", "_t");
		sprintf(num, "%d", counter);			
		strcat(name,num);
		counter++; 
		//tmp=NameLookUpInScope(ScopeTable,scope,name);
		//for (i=scope; i>-1;i--){
			//tmp2=NameLookUpInScope(ScopeTable,i,name);
			//if (tmp2!=NULL)
				//break;
		//}
	//}while(!(tmp==NULL && tmp2==NULL));
	//}while(tmp!=NULL);
	
	insertNodeToHash(Head,name,"hidden variable",scope,yylineno,functionoffset[funcounter],space,1);
	free(name);
	free(num);
	functionoffset[funcounter]=functionoffset[funcounter]+1;
	return counter;
}
