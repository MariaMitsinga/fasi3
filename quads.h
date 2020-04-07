enum iopcode{	
	assign, add, sub, mul, Div, mod,
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


int CreateSecretVar(int counter, int scope, int yylineno){
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
	
	insertNodeToHash(Head,name,"hidden variable",scope,yylineno,1);
	free(name);
	free(num);
	return counter;
}
