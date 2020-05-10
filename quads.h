int total=1;
int tablecounter=0;
extern counter;
int myerror=0;
#define EXPAND_SIZE 1000
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

struct breakContList{
	int index;
	int counter;
	struct breakContList* next;
};

struct for_staff{
	int index;
	int for_counter;
	struct for_staff* next;
};

struct whileForList{
	int Id;
	struct whileForList* next;
};


struct whileForList* place=NULL;
struct breakContList* breakList_while=NULL;
struct breakContList* contList_while=NULL;
struct breakContList* breakList_for=NULL;
struct breakContList* contList_for=NULL;

struct expr{
	enum expr_t type;
	struct SymTableEntry* sym;
	struct expr* index;
	double numConst;
	char* strConst;
	unsigned char boolConst;
	struct expr* next;
	struct truefalse* truelist;
	struct truefalse* falselist;
	/*struct stmt_t* breakList_while;
	struct stmt_t* contList_while;
	struct stmt_t* breakList_for;
	struct stmt_t* contList_for;*/
};

struct quad{
	enum iopcode op;
	struct expr* result;
	struct expr* arg1;
	struct expr* arg2;
	unsigned int label;
	unsigned int line;
};

struct truefalse{
	struct quad* quad;
	struct truefalse* next;
};

struct call{
	struct expr* elist;
	unsigned char method;
	const char* name;
};

struct forpref{
	int test;
	int enter;
};


struct loopbc{
	int loopcounter;
	struct loopbc* next;
};

struct funcjump{
	int endfunc;
	struct funcjump* next;
};


int funcounter=0;
int *functionoffset; 
typedef struct quad *temptq;
temptq *quadtable;
struct funcjump* myfuncjump;
struct loopbc* myloops;

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

void printMerror(){
	printf("\n<MERROR>: Incompatible types comparison!\n\n");
	myerror++;
}

int CreateSecretScope(int coun, int scope){
	int i;
	char* name=(char *)malloc(sizeof(char));
	char* num=(char *)malloc(sizeof(char));	
	struct SymTableEntry *tmp,*tmp2;	
			
	do{

		sprintf(name, "%s", "_t");
		sprintf(num, "%d", coun);			
		strcat(name,num);
		coun++; 
		tmp=NameLookUpInScope(ScopeTable,scope,name);
		//for (i=scope; i>-1;i--){
			if (scope>0){
			i=scope-1;
			tmp2=NameLookUpInScope(ScopeTable,i,name);
			if (tmp2!=NULL)
				break;
			}
		//}
	//}while(!(tmp==NULL && tmp2==NULL));
	}while(tmp!=NULL);
	
	//insertNodeToHash(Head,name,"hidden variable",scope,yylineno,1);
	free(name);
	free(num);
	return coun;
}

int checkUminusOperation(struct expr * q1)
{
	switch(q1->type)
	{
		case programfunc_e:
			printf("\n<MERROR>: invalid use of unary minus on user function\n\n");
			myerror++;
			break;
		case libraryfunc_e:
			printf("\n<MERROR>: invalid use of unary minus on library function\n\n");
			myerror++;
			break;
		case boolexpr_e:
			printf("\n<MERROR>: invalid use of unary minus on boolean expression\n\n");
			myerror++;
			break;
		case newtable_e:
			printf("\n<MERROR>: invalid use of unary minus on table\n\n");
			myerror++;
			break;
		case constbool_e:
			printf("\n<MERROR>: invalid use of unary minus on constant boolean \n\n");
			myerror++;
			break;
		case conststring_e:
			printf("\n<MERROR>: invalid use of unary minus on constant string \"%s\" \n\n",q1->strConst);
			myerror++;
			break;
		case nil_e:
			printf("\n<MERROR>: invalid use of unary minus on constant nill \n\n");
			myerror++;
			break;
		default:
			break;
	}
}

int checkArithmOperation(struct expr * q1,struct expr * q2,char* op)
{
	switch(q1->type)
	{
		case programfunc_e:
			printf("\n<MERROR>: invalid use of %s on user function\n\n",op);
			myerror++;
			break;
		case libraryfunc_e:
			printf("\n<MERROR>: invalid use of %s on library function\n\n",op);
			myerror++;
			break;
		case boolexpr_e:
			printf("\n<MERROR>: invalid use of %s on boolean expression\n\n",op);
			myerror++;
			break;
		case newtable_e:
			printf("\n<MERROR>: invalid use of %s on table\n\n",op);
			myerror++;
			break;
		case constbool_e:
			printf("\n<MERROR>: invalid use of %s on constant boolean \n\n",op);
			myerror++;
			break;
		case conststring_e:
			printf("\n<MERROR>: invalid use of %s on constant string \"%s\" \n\n",op,q1->strConst);
			myerror++;
			break;
		case nil_e:
			printf("\n<MERROR>: invalid use of %s on constant nill \n\n",op);
			myerror++;
			break;
		default:
			break;
	}
	switch(q2->type)
	{
		case programfunc_e:
			printf("\n<MERROR>: invalid use of %s on user function\n\n",op);
			myerror++;
			break;
		case libraryfunc_e:
			printf("\n<MERROR>: invalid use of %s on library function\n\n",op);
			myerror++;
			break;
		case boolexpr_e:
			printf("\n<MERROR>: invalid use of %s on boolean expression\n\n",op);
			myerror++;
			break;
		case newtable_e:
			printf("\n<MERROR>: invalid use of %s on table\n\n",op);
			myerror++;
			break;
		case constbool_e:
			printf("\n<MERROR>: invalid use of %s on constant boolean \n\n",op);
			myerror++;
			break;
		case conststring_e:
			printf("\n<MERROR>: invalid use of %s on constant string \"%s\" \n\n",op,q2->strConst);
			myerror++;
			break;
		case nil_e:
			printf("\n<MERROR>: invalid use of %s on constant nill \n\n",op);
			myerror++;
			break;
		default:
			break;
	}
	if(myerror==0)
		return 1;
	return 0;
}

int SigkrisiTypes(struct expr * q1,struct expr * q2){
	if(q1->type==conststring_e){
		return 0;
	}
	if(q2->type==conststring_e)
		return 0;
	else return 1;
	
}

void createfuncjump(){
	myfuncjump=(struct funcjump*)malloc(sizeof(struct funcjump));
	myfuncjump->next=NULL;
	myfuncjump->endfunc=0;
}


void addfuncjump(int endfunc){
	struct funcjump* tmp,*tmp2;
	tmp=myfuncjump;
	tmp2=(struct funcjump*)malloc(sizeof(struct funcjump));
	tmp2->next=NULL;
	tmp2->endfunc=endfunc;
	while(tmp->next!=NULL){
		tmp=tmp->next;
	}	
	tmp->next=tmp2;
}

int subfuncjump(){
	struct funcjump* tmp,*tmp2;
	int i=0;
	tmp=myfuncjump;
	while(tmp->next!=NULL){
		tmp2=tmp;
		tmp=tmp->next;
	}
	tmp2->next=NULL;
	i=tmp->endfunc;
	free(tmp);
	return i;
}


void createloopbc(){
	myloops=(struct loopbc*)malloc(sizeof(struct loopbc));
	myloops->next=NULL;
	myloops->loopcounter=0;
}

struct loopbc* whereat(){
	struct loopbc* tmp;
	tmp=myloops;
	while(tmp->next!=NULL){
		tmp=tmp->next;
	}
	return tmp;
}

void printloops(){
	struct loopbc* tmp;
	tmp=myloops;
	printf("\n\n");
	while(tmp!=NULL){
		printf(" [%d]",tmp->loopcounter);
		tmp=tmp->next;
	}
	printf("\n\n");
}

void addtoloopbc(){
	struct loopbc* tmp,*tmp2;
	tmp=myloops;
	tmp2=(struct loopbc*)malloc(sizeof(struct loopbc));
	tmp2->next=NULL;
	tmp2->loopcounter=0;
	while(tmp->next!=NULL){
		tmp=tmp->next;
	}	
	tmp->next=tmp2;
}

void subloopbc(){
	struct loopbc* tmp,*tmp2;
	tmp=myloops;
	while(tmp->next!=NULL){
		tmp2=tmp;
		tmp=tmp->next;
	}
	tmp2->next=NULL;
	free(tmp);
}


int check_arith (struct expr* check, const char* context) {
	if ( (check->type == constbool_e) || (check->type == conststring_e) || (check->type == nil_e) || (check->type == newtable_e) ||
	(check->type == programfunc_e) || (check->type == libraryfunc_e) || (check->type == boolexpr_e) ){
		myerror++;
		printf("Illegal expr used in %s!", context);
		return 0;
	}
	return 1;
}

void tablequadcreation(){
	int i;
	quadtable=malloc(CURR_SIZE * sizeof(temptq));
	for(i=0;i<CURR_SIZE;i++) {
		quadtable[i]=NULL;	
	}
}

void realloctablequad(){
	int i;
	quadtable=realloc(quadtable, NEW_SIZE * sizeof(temptq)); 
	for(i=CURR_SIZE;i<NEW_SIZE;i++){
		quadtable[i]=NULL;
	}
	total++;
}

void domalloc(int size){
	if(tablecounter>=CURR_SIZE) realloctablequad();
	quadtable[size]=malloc(sizeof(struct quad));
	quadtable[size]->label=-1;
}

void addquad(int size,enum iopcode op,struct expr* result,struct expr* arg1,struct expr* arg2,unsigned int label,unsigned int line){
	if(tablecounter>=CURR_SIZE) realloctablequad();
	if(quadtable[size]==NULL)
	{	quadtable[size]=malloc(sizeof(struct quad));
		quadtable[size]->label=label;
	}
	quadtable[size]->op=op;
	quadtable[size]->result=result;
	quadtable[size]->arg1=arg1;
	quadtable[size]->arg2=arg2;
	quadtable[size]->line=line;
	tablecounter++;
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
	functionoffset=(int*)malloc(1000 * sizeof(int)); 
	for(i=0;i<1000;i++) functionoffset[i]=0;
}

/*struct stmt_t* make_stmt(struct stmt_t* s){
	s=(struct stmt_t*)malloc(sizeof(struct stmt_t));
	s->index=-1;
	s->next=NULL;
	return s;
}*/

int newlist(int i){
	quadtable[i]->label=-1;
	return i;
}


struct breakContList* patchlist(struct breakContList* list,int label,int counter,int id)
{
	struct breakContList* tmp=list,*prev=NULL;
	if(id==0)//while case
	{
		while(tmp!=NULL)
		{
			if(tmp->counter==counter)
			{	
				quadtable[tmp->index]->label=label;
				if(tmp==list)
				{
					list=tmp->next;
					free(tmp);
				}
				else{
					prev->next=tmp->next;
					free(tmp);
				}
			}
			prev=tmp;
			tmp=tmp->next;
		}
	}
	if(id==1)//for case
	{
		while(tmp!=NULL)
		{
			if(tmp->counter==counter)
			{	
				quadtable[tmp->index]->label=label;
				if(tmp==list)
				{
					list=tmp->next;
					free(tmp);
				}
				else{
					prev->next=tmp->next;
					free(tmp);
				}
			}
			prev=tmp;
			tmp=tmp->next;
		}
	}

	return list;
	
	/*if(id==1)//for case
	{
		while(tmp)
		{
			if(tmp->for_counter==counter)
				quadtable[tmp->index]->label=label;
			tmp=tmp->next;
		}
	}*/
}

/*struct stmt_t* mergelist(struct stmt_t* l1,struct stmt_t* l2)
{
	if(l1==NULL)
		return l2;
	else if(l2==NULL)
		return l1;
	else
	{
		struct stmt_t* tmp=l1;
		while(tmp->next!=NULL)	
			tmp=tmp->next;
		tmp->next=l2;
		return l1;
	}
	return NULL;
}*/

struct expr* newexpr(enum expr_t type)
{
	struct expr* tmp= (struct expr*)malloc(sizeof(struct expr));
	tmp->type=type;
	tmp->next=NULL;
	tmp->strConst=NULL;
	tmp->sym=NULL;
	tmp->truelist=NULL;
	tmp->falselist=NULL;
	/*tmp->breakList_while=NULL;
	tmp->contList_while=NULL;
	tmp->breakList_for=NULL;
	tmp->contList_for=NULL;*/
	return tmp;
}

struct SymTableEntry * CreateSecretVar(int coun, int scope, int yylineno,int funcounter,int functionoffset[],char* space){
	int i;
	char* name=(char *)malloc(sizeof(char));
	char* num=(char *)malloc(sizeof(char));	
	struct SymTableEntry *tmp,*tmp2,*tmp3;		
	sprintf(name, "%s", "_t");
	sprintf(num, "%d", coun);			
	strcat(name,num);
	counter++; 
	
	tmp3=NameLookUpInScope(ScopeTable,scope,name);
	if(tmp3==NULL)
	{	tmp=insertNodeToHash(Head,name,"hidden variable",scope,yylineno,functionoffset[funcounter],space,1);
		functionoffset[funcounter]=functionoffset[funcounter]+1;}
	free(name);
	free(num);
	
	if(tmp3==NULL)
		return tmp;
	return tmp3;
}

int LastTempInScope(struct SymTable* root,unsigned int scope)
{
	int num=-1;
	struct SymTableEntry *tmp=root->head[scope];
    	while(tmp)
    	{
        	if(strstr(tmp->name, "_t") != NULL)
            		//num=tmp->name[2]-'0';
			num=atoi(tmp->name);
        	tmp=tmp->nextScopeList;
    	}
	//printf("\nnum=%d\n",num);
	return num;
}

int istempname(const char* s)
{
	return *s== '_';
}

int istempexpr(struct expr* e)
{
	return e->sym && istempname(e->sym->name);
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
	if(tablecounter>=CURR_SIZE) realloctablequad();
	domalloc(tablecounter);
	quadtable[tablecounter]->op=op;
	quadtable[tablecounter]->arg1=arg1;
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
		//printf("from emit_iftableitem: %g\n",e->index->numConst);
		return result;
	}
	return NULL;
}


struct expr * newexpr_constnum(double i)
{
	struct expr *e = newexpr(constnum_e);
	e->numConst = i;
	//printf("from quad:%s\n",e->strConst);
	return e;	
}

struct expr * newexpr_conststring(const char* s)
{
	struct expr *e = newexpr(conststring_e);
	e->strConst = strdup(s);
	//printf("from quad:%s\n",e->strConst);
	return e;	
}

struct expr * newexpr_constbool(unsigned char s)
{
	struct expr *e = newexpr(constbool_e);
	e->boolConst = s;
	//printf("from quad:%s\n",e->strConst);
	return e;	
}

struct truefalse* AddTrueFalseList(struct truefalse* arxi, struct quad* quadd)
{
	int i=0;
	struct truefalse *curr=arxi, *prev;
	struct truefalse *tmp =(struct truefalse*)malloc(sizeof(struct truefalse));

	tmp->quad=quadd;
	tmp->next=NULL;

	if (arxi==NULL){
		arxi=tmp;
	}
	else{
		while(curr!=NULL){
			prev=curr;
			i++;
			curr=curr->next;
		}
		prev->next=tmp;
	}
	return arxi;
}


struct truefalse* merge(struct truefalse* list1,struct truefalse* list2)
{
	struct truefalse* tmp=list1;
	while(tmp->next!=NULL)	
		tmp=tmp->next;
	tmp->next=list2;
	return list1;
}

struct expr* reverseList(struct expr* elist)
{
	struct expr* reverse_elist=NULL,*tmp=elist;
	while(tmp!=NULL)
	{
		struct expr* tmp2;
		tmp2=(struct expr*)malloc(sizeof(struct expr));
		tmp2->sym=tmp->sym;
		tmp2->type=tmp->type;
		tmp2->index=tmp->index;
		tmp2->numConst=tmp->numConst;
		tmp2->strConst=tmp->strConst;
		tmp2->boolConst=tmp->boolConst;
		//printf("from .h ... %s\n",tmp->sym->name);
		tmp2->next=reverse_elist;
		reverse_elist=tmp2;
		tmp=tmp->next;
	}
	return reverse_elist;
}

struct expr* make_call(int size,int count, int scope,int line, int funcounter,
 int functionoffset[],char* space, struct expr *lv, struct expr *elista){
	struct expr* result=newexpr(var_e);
	struct expr* func = emit_iftableitem(lv,count,scope,line,funcounter,functionoffset,space);
	struct expr* curr=elista;
	
	while(curr!=NULL){
		addquad(tablecounter, param,NULL, curr,NULL,-1, line);
		curr=curr->next;
	}
	addquad(tablecounter, call,NULL, func, NULL, -1, line);
	result->sym=CreateSecretVar(count, scope, line, funcounter,functionoffset, space);
	addquad(tablecounter, getretval,result, NULL,NULL,-1, line);
	return result;
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

void backpatch(struct truefalse* list,int M){
	while(list!=NULL){
		list->quad->label=M;
		list=list->next;
	}
}

void addToPlaceList(int id)
{
	struct whileForList* tmp=(struct whileForList*)malloc(sizeof(struct whileForList));
	tmp->Id=id;
	if(place==NULL)
		tmp->next=NULL;
	else
		tmp->next=place;
	place=tmp;
}

struct breakContList* addToBreakContList(struct breakContList* list,int index,int while_counter)
{
	struct breakContList* tmp=(struct breakContList*)malloc(sizeof(struct breakContList));
	tmp->index=index;
	tmp->counter=while_counter;
	if(list==NULL)
		tmp->next=NULL;
	else
		tmp->next=list;
	list=tmp;
	return list;
}

void addToForStaffList(struct for_staff* list,int index,int for_counter)
{
	struct for_staff* tmp=(struct for_staff*)malloc(sizeof(struct for_staff));
	tmp->index=index;
	tmp->for_counter=for_counter;
	if(list==NULL)
		tmp->next=NULL;
	else
		tmp->next=list;
	list=tmp;
}

void removeFirstInPlaceList()
{
	struct whileForList* tmp=place;
	place=place->next;
	free(tmp);
}

void makeQuadtxt()
{
	int i=0;
	FILE* fp;
	fp=fopen("quads.txt","w+");
	fprintf(fp,"quad# \t opcode \t result \t arg1 \t arg2 \t label\n");
	fprintf(fp,"----------------------------------------------------------------------------\n");
	for(i=0;i<CURR_SIZE;i++)
	{
		if(quadtable[i]==NULL)
			continue;
		switch(quadtable[i]->op)
		{
			case uminus:
				fprintf(fp,"%d:\t %s \t %s ",i+1,"uminus",quadtable[i]->result->sym->name);
				if(strcmp(getExpr_t(quadtable[i]->arg1->type),"constnum_e")==0)
					fprintf(fp,"\t\t %g\n",quadtable[i]->arg1->numConst);
				else
					fprintf(fp,"\t\t %s \n",quadtable[i]->arg1->sym->name);
				break;
			case tablegetelem:
				//printf("%d:\t %s \t %s \t\t %s \t \"%s\" \n",i+1,"tablegetelem",quadtable[i]->result->sym->name,quadtable[i]->arg1->sym->name,quadtable[i]->arg1->index->strConst);
				fprintf(fp,"%d:\t %s \t %s \t\t %s \t ",i+1,"tablegetelem",quadtable[i]->result->sym->name,quadtable[i]->arg1->sym->name);
				if(strcmp(getExpr_t(quadtable[i]->arg2->type),"constnum_e")==0)
						fprintf(fp,"%g \n",quadtable[i]->arg2->numConst );
					else if(strcmp(getExpr_t(quadtable[i]->arg2->type),"conststring_e")==0)
						fprintf(fp,"\"%s\" \n",quadtable[i]->arg2->strConst);
					else if(strcmp(getExpr_t(quadtable[i]->arg2->type),"nil_e")==0)
						fprintf(fp,"nil \n");
					else if(strcmp(getExpr_t(quadtable[i]->arg2->type),"constbool_e")==0){
						if(quadtable[i]->arg2->boolConst=='1')
							fprintf(fp,"'true' \n");
						else
							fprintf(fp,"'false' \n");
					}else
						fprintf(fp,"%s \n",quadtable[i]->arg2->sym->name);
				break;
			case add:
				fprintf(fp,"%d:\t %s \t\t %s\t\t ",i+1,"add",quadtable[i]->result->sym->name);
				if( strcmp(getExpr_t(quadtable[i]->arg1->type),"constnum_e")==0) 
					fprintf(fp,"%g\t ",quadtable[i]->arg1->numConst);
				else 
					fprintf(fp,"%s\t ",quadtable[i]->arg1->sym->name);

				if( strcmp(getExpr_t(quadtable[i]->arg2->type),"constnum_e")==0) 
					fprintf(fp,"%g\n",quadtable[i]->arg2->numConst);
				else 
					fprintf(fp,"%s\n",quadtable[i]->arg2->sym->name);

				break;
			case sub:
				fprintf(fp,"%d:\t %s \t\t %s\t\t ",i+1,"sub",quadtable[i]->result->sym->name);
				if( strcmp(getExpr_t(quadtable[i]->arg1->type),"constnum_e")==0) 
					fprintf(fp,"%g\t ",quadtable[i]->arg1->numConst);
				else 
					fprintf(fp,"%s\t ",quadtable[i]->arg1->sym->name);

				if( strcmp(getExpr_t(quadtable[i]->arg2->type),"constnum_e")==0) 
					fprintf(fp,"%g\n",quadtable[i]->arg2->numConst);
				else 
					fprintf(fp,"%s\n",quadtable[i]->arg2->sym->name);
				break;
			case mul:
				fprintf(fp,"%d:\t %s \t\t %s\t\t ",i+1,"mul",quadtable[i]->result->sym->name);
				if( strcmp(getExpr_t(quadtable[i]->arg1->type),"constnum_e")==0) 
					fprintf(fp,"%g\t ",quadtable[i]->arg1->numConst);
				else 
					fprintf(fp,"%s\t ",quadtable[i]->arg1->sym->name);

				if( strcmp(getExpr_t(quadtable[i]->arg2->type),"constnum_e")==0) 
					fprintf(fp,"%g\n",quadtable[i]->arg2->numConst);
				else 
					fprintf(fp,"%s\n",quadtable[i]->arg2->sym->name);
				break;
			case DIV: 
				fprintf(fp,"%d:\t %s \t\t %s\t\t ",i+1,"div",quadtable[i]->result->sym->name);
				if( strcmp(getExpr_t(quadtable[i]->arg1->type),"constnum_e")==0) 
					fprintf(fp,"%g\t ",quadtable[i]->arg1->numConst);
				else 
					fprintf(fp,"%s\t ",quadtable[i]->arg1->sym->name);

				if( strcmp(getExpr_t(quadtable[i]->arg2->type),"constnum_e")==0) 
					fprintf(fp,"%g\n",quadtable[i]->arg2->numConst);
				else 
					fprintf(fp,"%s\n",quadtable[i]->arg2->sym->name);
				break;
			case mod:
				fprintf(fp,"%d:\t %s \t\t %s\t\t ",i+1,"mod",quadtable[i]->result->sym->name);
				if( strcmp(getExpr_t(quadtable[i]->arg1->type),"constnum_e")==0) 
					fprintf(fp,"%g\t ",quadtable[i]->arg1->numConst);
				else 
					fprintf(fp,"%s\t ",quadtable[i]->arg1->sym->name);

				if( strcmp(getExpr_t(quadtable[i]->arg2->type),"constnum_e")==0) 
					fprintf(fp,"%g\n",quadtable[i]->arg2->numConst);
				else 
					fprintf(fp,"%s\n",quadtable[i]->arg2->sym->name);				
				break;
			case tablesetelem:
				fprintf(fp,"%d:\t %s \t %s \t\t ",i+1,"tablesetelem",quadtable[i]->result->sym->name);
				if(strcmp(getExpr_t(quadtable[i]->arg1->type),"constnum_e")==0)
					fprintf(fp,"%g ",quadtable[i]->arg1->numConst);
				else if(strcmp(getExpr_t(quadtable[i]->arg1->type),"conststring_e")==0)
					fprintf(fp," \"%s\" ",quadtable[i]->arg1->strConst);
				else if(strcmp(getExpr_t(quadtable[i]->arg1->type),"nil_e")==0)
					fprintf(fp,"nil ");
				else if(strcmp(getExpr_t(quadtable[i]->arg1->type),"constbool_e")==0){
					if(quadtable[i]->arg1->boolConst=='1')
						fprintf(fp,"'true' ");
					else
						fprintf(fp,"'false' ");
				}else
					fprintf(fp," %s ",quadtable[i]->arg1->sym->name);

				if(strcmp(getExpr_t(quadtable[i]->arg2->type),"constnum_e")==0)
					fprintf(fp,"\t%g \n",quadtable[i]->arg2->numConst );
				else if(strcmp(getExpr_t(quadtable[i]->arg2->type),"conststring_e")==0)
					fprintf(fp,"\t\"%s\" \n",quadtable[i]->arg2->strConst);
				else if(strcmp(getExpr_t(quadtable[i]->arg2->type),"nil_e")==0)
					fprintf(fp,"\tnil \n");
				else if(strcmp(getExpr_t(quadtable[i]->arg2->type),"constbool_e")==0){
				if(quadtable[i]->arg2->boolConst=='1')
						fprintf(fp,"\t 'true' \n");
					else
						fprintf(fp,"\t 'false' \n");
				}else
				fprintf(fp,"\t %s \n",quadtable[i]->arg2->sym->name);		
				break;
			case assign:
				fprintf(fp,"%d:\t %s \t %s",i+1,"assign",quadtable[i]->result->sym->name);
				if(strcmp(getExpr_t(quadtable[i]->arg1->type),"constnum_e")==0)
						fprintf(fp," \t\t %g \n",quadtable[i]->arg1->numConst);
					else if(strcmp(getExpr_t(quadtable[i]->arg1->type),"conststring_e")==0)
						fprintf(fp," \t\t \"%s\" \n",quadtable[i]->arg1->strConst);
					else if(strcmp(getExpr_t(quadtable[i]->arg1->type),"nil_e")==0)
						fprintf(fp," \t\t nil \n");
					else if(strcmp(getExpr_t(quadtable[i]->arg1->type),"constbool_e")==0){
						if(quadtable[i]->arg1->boolConst=='1')
							fprintf(fp," \t\t 'true' \n");
						else
							fprintf(fp," \t\t 'false' \n");
					}else
						fprintf(fp," \t\t %s \n",quadtable[i]->arg1->sym->name);
				break;
			case tablecreate:
				fprintf(fp,"%d:\t %s \t %s \n",i+1,"tablecreate",quadtable[i]->arg1->sym->name);
				break;
			case funcstart:
				fprintf(fp,"%d:\t %s \t %s \n",i+1,"funcstart",quadtable[i]->result->sym->name);	
				break;
			case funcend:				
				fprintf(fp,"%d:\t %s \t %s \n",i+1,"funcend",quadtable[i]->result->sym->name);
				break;
			case Return:
				if(quadtable[i]->result==NULL)
					fprintf(fp,"%d:\t %s \n",i+1,"return");
				else{
					if(strcmp(getExpr_t(quadtable[i]->result->type),"constnum_e")==0)
						fprintf(fp,"%d:\t %s \t %g \n",i+1,"return",quadtable[i]->result->numConst);
					else if(strcmp(getExpr_t(quadtable[i]->result->type),"conststring_e")==0)
						fprintf(fp,"%d:\t %s \t %s \n",i+1,"return",quadtable[i]->result->strConst);
					else if(strcmp(getExpr_t(quadtable[i]->result->type),"nil_e")==0)
						fprintf(fp,"%d:\t %s \t nil \n");
					else if(strcmp(getExpr_t(quadtable[i]->result->type),"constbool_e")==0){
						if(quadtable[i]->result->boolConst=='1')
							fprintf(fp,"%d:\t %s \t 'true' \n",i+1,"return");
						else
							fprintf(fp,"%d:\t %s \t 'false' \n",i+1,"return");
					}else
						fprintf(fp,"%d:\t %s \t %s \n",i+1,"return",quadtable[i]->result->sym->name);
				
				}
				break;
			case param:
				fprintf(fp,"%d:\t %s ",i+1,"param");
				if(strcmp(getExpr_t(quadtable[i]->arg1->type),"constnum_e")==0)
						fprintf(fp,"\t\t\t\t %g \n",quadtable[i]->arg1->numConst);
					else if(strcmp(getExpr_t(quadtable[i]->arg1->type),"conststring_e")==0)
						fprintf(fp,"\t\t\t\t \"%s\"\n",quadtable[i]->arg1->strConst);
					else if(strcmp(getExpr_t(quadtable[i]->arg1->type),"nil_e")==0)
						fprintf(fp,"\t\t\t\t nil \n");
					else if(strcmp(getExpr_t(quadtable[i]->arg1->type),"constbool_e")==0){
						if(quadtable[i]->arg1->boolConst=='1')
							fprintf(fp,"\t\t\t\t 'true' \n");
						else
							fprintf(fp,"\t\t\t\t 'false' \n");
					}else
						fprintf(fp,"\t\t\t\t %s \n",quadtable[i]->arg1->sym->name);
				break;
			case call:
				fprintf(fp,"%d:\t %s \t\t\t\t %s \n",i+1,"call",quadtable[i]->arg1->sym->name);
				break;
			case getretval:
				fprintf(fp,"%d:\t %s \t %s \n",i+1,"getretval",quadtable[i]->result->sym->name);
				break;
			case if_greater:
				fprintf(fp,"%d:\t %s \t\t \t",i+1,"if_greater");
				if(strcmp(getExpr_t(quadtable[i]->result->type),"constnum_e")==0)
					fprintf(fp," %g\t",quadtable[i]->result->numConst);
				else if(strcmp(getExpr_t(quadtable[i]->result->type),"conststring_e")==0)
					fprintf(fp," \"%s\"\t",quadtable[i]->result->strConst);
				else if(strcmp(getExpr_t(quadtable[i]->result->type),"constbool_e")==0){
					if(quadtable[i]->result->boolConst=='1')
						fprintf(fp," 'true'\t");
					else
						fprintf(fp," 'false'\t");
				}else 
					fprintf(fp," %s\t",quadtable[i]->result->sym->name);
				
				if(strcmp(getExpr_t(quadtable[i]->arg1->type),"constnum_e")==0)
					fprintf(fp," %g\t",quadtable[i]->arg1->numConst);
				else if(strcmp(getExpr_t(quadtable[i]->arg1->type),"conststring_e")==0)
					fprintf(fp," \"%s\"\t",quadtable[i]->arg1->strConst);
				else if(strcmp(getExpr_t(quadtable[i]->arg1->type),"constbool_e")==0){
					if(quadtable[i]->arg1->boolConst=='1')
						fprintf(fp," 'true'\t");
					else
						fprintf(fp," 'false'\t");
				}else 
					fprintf(fp," %s\t",quadtable[i]->arg1->sym->name);
				if(quadtable[i]->label>0)
					fprintf(fp," %d",quadtable[i]->label+1);
				fprintf(fp,"\n");
				break;
			case if_greatereq:
				fprintf(fp,"%d:\t %s \t\t \t",i+1,"if_greatereq");
				if(strcmp(getExpr_t(quadtable[i]->result->type),"constnum_e")==0)
					fprintf(fp," %g\t",quadtable[i]->result->numConst);
				else if(strcmp(getExpr_t(quadtable[i]->result->type),"conststring_e")==0)
					fprintf(fp," \"%s\"\t",quadtable[i]->result->strConst);
				else if(strcmp(getExpr_t(quadtable[i]->result->type),"constbool_e")==0){
					if(quadtable[i]->result->boolConst=='1')
						fprintf(fp," 'true'\t");
					else
						fprintf(fp," 'false'\t");
				}else 
					fprintf(fp," %s\t",quadtable[i]->result->sym->name);
				
				if(strcmp(getExpr_t(quadtable[i]->arg1->type),"constnum_e")==0)
					fprintf(fp," %g\t",quadtable[i]->arg1->numConst);
				else if(strcmp(getExpr_t(quadtable[i]->arg1->type),"conststring_e")==0)
					fprintf(fp," \"%s\"\t",quadtable[i]->arg1->strConst);
				else if(strcmp(getExpr_t(quadtable[i]->arg1->type),"constbool_e")==0){
					if(quadtable[i]->arg1->boolConst=='1')
						fprintf(fp," 'true'\t");
					else
						fprintf(fp," 'false'\t");
				}else 
					fprintf(fp," %s\t",quadtable[i]->arg1->sym->name);
				if(quadtable[i]->label>0)
					fprintf(fp," %d",quadtable[i]->label+1);
				fprintf(fp,"\n");
				break;
			case if_less:
				fprintf(fp,"%d:\t %s \t\t \t",i+1,"if_less");
				if(strcmp(getExpr_t(quadtable[i]->result->type),"constnum_e")==0)
					fprintf(fp," %g\t",quadtable[i]->result->numConst);
				else if(strcmp(getExpr_t(quadtable[i]->result->type),"conststring_e")==0)
					fprintf(fp," \"%s\"\t",quadtable[i]->result->strConst);
				else if(strcmp(getExpr_t(quadtable[i]->result->type),"constbool_e")==0){
					if(quadtable[i]->result->boolConst=='1')
						fprintf(fp," 'true'\t");
					else
						fprintf(fp," 'false'\t");
				}else 
					fprintf(fp," %s\t",quadtable[i]->result->sym->name);
				
				if(strcmp(getExpr_t(quadtable[i]->arg1->type),"constnum_e")==0)
					fprintf(fp," %g\t",quadtable[i]->arg1->numConst);
				else if(strcmp(getExpr_t(quadtable[i]->arg1->type),"conststring_e")==0)
					fprintf(fp," \"%s\"\t",quadtable[i]->arg1->strConst);
				else if(strcmp(getExpr_t(quadtable[i]->arg1->type),"constbool_e")==0){
					if(quadtable[i]->arg1->boolConst=='1')
						fprintf(fp," 'true'\t");
					else
						fprintf(fp," 'false'\t");
				}else 
					fprintf(fp," %s\t",quadtable[i]->arg1->sym->name);
				if(quadtable[i]->label>0)
					fprintf(fp," %d",quadtable[i]->label+1);
				fprintf(fp,"\n");
				break;
			case if_lesseq:
				fprintf(fp,"%d:\t %s \t\t \t",i+1,"if_lesseq");
				if(strcmp(getExpr_t(quadtable[i]->result->type),"constnum_e")==0)
					fprintf(fp," %g\t",quadtable[i]->result->numConst);
				else if(strcmp(getExpr_t(quadtable[i]->result->type),"conststring_e")==0)
					fprintf(fp," \"%s\"\t",quadtable[i]->result->strConst);
				else if(strcmp(getExpr_t(quadtable[i]->result->type),"constbool_e")==0){
					if(quadtable[i]->result->boolConst=='1')
						fprintf(fp," 'true'\t");
					else
						fprintf(fp," 'false'\t");
				}else
					fprintf(fp," %s\t",quadtable[i]->result->sym->name);
				
				if(strcmp(getExpr_t(quadtable[i]->arg1->type),"constnum_e")==0)
					fprintf(fp," %g\t",quadtable[i]->arg1->numConst);
				else if(strcmp(getExpr_t(quadtable[i]->arg1->type),"conststring_e")==0)
					fprintf(fp," \"%s\"\t",quadtable[i]->arg1->strConst);
				else if(strcmp(getExpr_t(quadtable[i]->arg1->type),"constbool_e")==0){
					if(quadtable[i]->arg1->boolConst=='1')
						fprintf(fp," 'true'\t");
					else
						fprintf(fp," 'false'\t");
				}else 
					fprintf(fp," %s\t",quadtable[i]->arg1->sym->name);
				if(quadtable[i]->label>0)
					fprintf(fp," %d",quadtable[i]->label+1);
				fprintf(fp,"\n");
				break;
			case if_eq:
				fprintf(fp,"%d:\t %s \t\t \t\t",i+1,"if_eq");
				if(strcmp(getExpr_t(quadtable[i]->result->type),"constnum_e")==0)
					fprintf(fp," %g\t",quadtable[i]->result->numConst);
				else if(strcmp(getExpr_t(quadtable[i]->result->type),"conststring_e")==0)
					fprintf(fp," \"%s\"\t",quadtable[i]->result->strConst);
				else if(strcmp(getExpr_t(quadtable[i]->result->type),"nil_e")==0)
						fprintf(fp," nil\t");
				else if(strcmp(getExpr_t(quadtable[i]->result->type),"constbool_e")==0){
					if(quadtable[i]->result->boolConst=='1')
						fprintf(fp," 'true'\t");
					else
						fprintf(fp," 'false'\t");
				}else 
					fprintf(fp," %s\t",quadtable[i]->result->sym->name);
				
				if(strcmp(getExpr_t(quadtable[i]->arg1->type),"constnum_e")==0)
					fprintf(fp," %g\t",quadtable[i]->arg1->numConst);
				else if(strcmp(getExpr_t(quadtable[i]->arg1->type),"conststring_e")==0)
					fprintf(fp," \"%s\"\t",quadtable[i]->arg1->strConst);
				else if(strcmp(getExpr_t(quadtable[i]->arg1->type),"nil_e")==0)
						fprintf(fp," nil\t");
				else if(strcmp(getExpr_t(quadtable[i]->arg1->type),"constbool_e")==0){
					if(quadtable[i]->arg1->boolConst=='1')
						fprintf(fp," 'true'\t");
					else
						fprintf(fp," 'false'\t");
				}else 
					fprintf(fp," %s\t",quadtable[i]->arg1->sym->name);

				if(quadtable[i]->label>0)
					fprintf(fp," %d",quadtable[i]->label+1);
				fprintf(fp,"\n");
				break;			
			case if_noteq:
				fprintf(fp,"%d:\t %s \t\t \t",i+1,"if_noteq");
				if(strcmp(getExpr_t(quadtable[i]->result->type),"constnum_e")==0)
					fprintf(fp," %g\t",quadtable[i]->result->numConst);
				else if(strcmp(getExpr_t(quadtable[i]->result->type),"conststring_e")==0)
					fprintf(fp," \"%s\"\t",quadtable[i]->result->strConst);
				else if(strcmp(getExpr_t(quadtable[i]->result->type),"nil_e")==0)
						fprintf(fp," nil\t");
				else if(strcmp(getExpr_t(quadtable[i]->result->type),"constbool_e")==0){
					if(quadtable[i]->result->boolConst=='1')
						fprintf(fp," 'true'\t");
					else
						fprintf(fp," 'false'\t");
				}else
					fprintf(fp," %s\t",quadtable[i]->result->sym->name);
				
				if(strcmp(getExpr_t(quadtable[i]->arg1->type),"constnum_e")==0)
					fprintf(fp," %g\t",quadtable[i]->arg1->numConst);
				else if(strcmp(getExpr_t(quadtable[i]->arg1->type),"conststring_e")==0)
					fprintf(fp," \"%s\"\t",quadtable[i]->arg1->strConst);
				else if(strcmp(getExpr_t(quadtable[i]->arg1->type),"nil_e")==0)
						fprintf(fp," nil\t");
				else if(strcmp(getExpr_t(quadtable[i]->arg1->type),"constbool_e")==0){
					if(quadtable[i]->arg1->boolConst=='1')
						fprintf(fp," 'true'\t");
					else
						fprintf(fp," 'false'\t");
				}else 
					fprintf(fp," %s\t",quadtable[i]->arg1->sym->name);
				if(quadtable[i]->label>0)
					fprintf(fp," %d",quadtable[i]->label+1);
				fprintf(fp,"\n");
				break;
			case jump:
				fprintf(fp,"%d:\t %s \t\t \t\t \t \t %d\n",i+1,"jump",quadtable[i]->label+1);
				break;
			default:
				fprintf(fp,"the end\n");
		}
	}
	fclose(fp);
}


void printQuad()
{
	int i=0;
	printf("quad# \t opcode \t result \t arg1 \t arg2 \t label\n");
	printf("----------------------------------------------------------------------------\n");
	for(i=0;i<CURR_SIZE;i++)
	{	//printf("\nHERE:%s\n",getiopcode(quadtable[i]->op));
		if(quadtable[i]==NULL)
			continue;
		switch(quadtable[i]->op)
		{
			case uminus:
				printf("%d:\t %s \t %s ",i+1,"uminus",quadtable[i]->result->sym->name);
				if(strcmp(getExpr_t(quadtable[i]->arg1->type),"constnum_e")==0)
						printf("\t\t %g\n",quadtable[i]->arg1->numConst);
				else
					printf("\t\t %s \n",quadtable[i]->arg1->sym->name);
				break;
			case tablegetelem:
				//printf("%d:\t %s \t %s \t\t %s \t \"%s\" \n",i+1,"tablegetelem",quadtable[i]->result->sym->name,quadtable[i]->arg1->sym->name,quadtable[i]->arg1->index->strConst);
				printf("%d:\t %s \t %s \t\t %s \t ",i+1,"tablegetelem",quadtable[i]->result->sym->name,quadtable[i]->arg1->sym->name);
				if(strcmp(getExpr_t(quadtable[i]->arg2->type),"constnum_e")==0)
						printf("%g \n",quadtable[i]->arg2->numConst );
					else if(strcmp(getExpr_t(quadtable[i]->arg2->type),"conststring_e")==0)
						printf("\"%s\" \n",quadtable[i]->arg2->strConst);
					else if(strcmp(getExpr_t(quadtable[i]->arg2->type),"nil_e")==0)
						printf("nil \n");
					else if(strcmp(getExpr_t(quadtable[i]->arg2->type),"constbool_e")==0){
						if(quadtable[i]->arg2->boolConst=='1')
							printf("'true' \n");
						else
							printf("'false' \n");
					}else
						printf("%s \n",quadtable[i]->arg2->sym->name);
				break;
			case add:
				printf("%d:\t %s \t\t %s\t\t ",i+1,"add",quadtable[i]->result->sym->name);
				if( strcmp(getExpr_t(quadtable[i]->arg1->type),"constnum_e")==0) 
					printf("%g\t ",quadtable[i]->arg1->numConst);
				else 
					printf("%s\t ",quadtable[i]->arg1->sym->name);

				if( strcmp(getExpr_t(quadtable[i]->arg2->type),"constnum_e")==0) 
					printf("%g\n",quadtable[i]->arg2->numConst);
				else 
					printf("%s\n",quadtable[i]->arg2->sym->name);

				break;
			case sub:
				printf("%d:\t %s \t\t %s\t\t ",i+1,"sub",quadtable[i]->result->sym->name);
				if( strcmp(getExpr_t(quadtable[i]->arg1->type),"constnum_e")==0) 
					printf("%g\t ",quadtable[i]->arg1->numConst);
				else 
					printf("%s\t ",quadtable[i]->arg1->sym->name);

				if( strcmp(getExpr_t(quadtable[i]->arg2->type),"constnum_e")==0) 
					printf("%g\n",quadtable[i]->arg2->numConst);
				else 
					printf("%s\n",quadtable[i]->arg2->sym->name);
				break;
			case mul:
				printf("%d:\t %s \t\t %s\t\t ",i+1,"mul",quadtable[i]->result->sym->name);
				if( strcmp(getExpr_t(quadtable[i]->arg1->type),"constnum_e")==0) 
					printf("%g\t ",quadtable[i]->arg1->numConst);
				else 
					printf("%s\t ",quadtable[i]->arg1->sym->name);

				if( strcmp(getExpr_t(quadtable[i]->arg2->type),"constnum_e")==0) 
					printf("%g\n",quadtable[i]->arg2->numConst);
				else 
					printf("%s\n",quadtable[i]->arg2->sym->name);
				break;
			case DIV: 
				printf("%d:\t %s \t\t %s\t\t ",i+1,"div",quadtable[i]->result->sym->name);
				if( strcmp(getExpr_t(quadtable[i]->arg1->type),"constnum_e")==0) 
					printf("%g\t ",quadtable[i]->arg1->numConst);
				else 
					printf("%s\t ",quadtable[i]->arg1->sym->name);

				if( strcmp(getExpr_t(quadtable[i]->arg2->type),"constnum_e")==0) 
					printf("%g\n",quadtable[i]->arg2->numConst);
				else 
					printf("%s\n",quadtable[i]->arg2->sym->name);
				break;
			case mod:
				printf("%d:\t %s \t\t %s\t\t ",i+1,"mod",quadtable[i]->result->sym->name);
				if( strcmp(getExpr_t(quadtable[i]->arg1->type),"constnum_e")==0) 
					printf("%g\t ",quadtable[i]->arg1->numConst);
				else 
					printf("%s\t ",quadtable[i]->arg1->sym->name);

				if( strcmp(getExpr_t(quadtable[i]->arg2->type),"constnum_e")==0) 
					printf("%g\n",quadtable[i]->arg2->numConst);
				else 
					printf("%s\n",quadtable[i]->arg2->sym->name);				
				break;
			case tablesetelem:
				//if(quadtable[i]->result->index==NULL)
				//{	
					//printf("lalala....%d\n",i);
					/*if(quadtable[i]->arg2->sym!=NULL)
						printf("%d:\t %s\t %s \t\t %.0f\t %s \n",i+1,"tablesetelem",quadtable[i]->result->sym->name,quadtable[i]->arg1->numConst,quadtable[i]->arg2->sym->name);
					else
						printf("%d:\t %s\t %s \t\t %.0f\t %.4f \n",i+1,"tablesetelem",quadtable[i]->result->sym->name,quadtable[i]->arg1->numConst,quadtable[i]->arg2->numConst);
					*/
					printf("%d:\t %s \t %s \t\t ",i+1,"tablesetelem",quadtable[i]->result->sym->name);
					if(strcmp(getExpr_t(quadtable[i]->arg1->type),"constnum_e")==0)
						printf("%g ",quadtable[i]->arg1->numConst);
					else if(strcmp(getExpr_t(quadtable[i]->arg1->type),"conststring_e")==0)
						printf(" \"%s\" ",quadtable[i]->arg1->strConst);
					else if(strcmp(getExpr_t(quadtable[i]->arg1->type),"nil_e")==0)
						printf("nil ");
					else if(strcmp(getExpr_t(quadtable[i]->arg1->type),"constbool_e")==0){
						if(quadtable[i]->arg1->boolConst=='1')
							printf("'true' ");
						else
							printf("'false' ");
					}else
						printf(" %s ",quadtable[i]->arg1->sym->name);
						
					if(strcmp(getExpr_t(quadtable[i]->arg2->type),"constnum_e")==0)
						printf("\t%g \n",quadtable[i]->arg2->numConst );
					else if(strcmp(getExpr_t(quadtable[i]->arg2->type),"conststring_e")==0)
						printf("\t\"%s\" \n",quadtable[i]->arg2->strConst);
					else if(strcmp(getExpr_t(quadtable[i]->arg2->type),"nil_e")==0)
						printf("\tnil \n");
					else if(strcmp(getExpr_t(quadtable[i]->arg2->type),"constbool_e")==0){
						if(quadtable[i]->arg2->boolConst=='1')
							printf("\t 'true' \n");
						else
							printf("\t 'false' \n");
					}else
						printf("\t %s \n",quadtable[i]->arg2->sym->name);
						//printf("%d:\t %s\t %s \t\t %.0f\t%s \n",i+1,"tablesetelem",quadtable[i]->result->sym->name,quadtable[i]->arg1->numConst,quadtable[i]->arg2->sym->name);	


				//}
				//else	
					//printf("%d:\t %s \t %s \t\t \"%s\" \t %s \n",i+1,"tablesetelem",quadtable[i]->result->sym->name,quadtable[i]->result->index->strConst,quadtable[i]->arg2->sym->name);
				break;
			case assign:
				printf("%d:\t %s \t %s",i+1,"assign",quadtable[i]->result->sym->name);
				if(strcmp(getExpr_t(quadtable[i]->arg1->type),"constnum_e")==0)
						printf(" \t\t %g \n",quadtable[i]->arg1->numConst);
					else if(strcmp(getExpr_t(quadtable[i]->arg1->type),"conststring_e")==0)
						printf(" \t\t \"%s\" \n",quadtable[i]->arg1->strConst);
					else if(strcmp(getExpr_t(quadtable[i]->arg1->type),"nil_e")==0)
						printf(" \t\t nil \n");
					else if(strcmp(getExpr_t(quadtable[i]->arg1->type),"constbool_e")==0){
						if(quadtable[i]->arg1->boolConst=='1')
							printf(" \t\t 'true' \n");
						else
							printf(" \t\t 'false' \n");
					}else
						printf(" \t\t %s \n",quadtable[i]->arg1->sym->name);
				break;
			case tablecreate:
				printf("%d:\t %s \t %s \n",i+1,"tablecreate",quadtable[i]->arg1->sym->name);
				break;
			case funcstart:
				printf("%d:\t %s \t %s \n",i+1,"funcstart",quadtable[i]->result->sym->name);	
				break;
			case funcend:				
				printf("%d:\t %s \t %s \n",i+1,"funcend",quadtable[i]->result->sym->name);
				break;
			case Return:
				if(quadtable[i]->result==NULL)
					printf("%d:\t %s \n",i+1,"return");
				else{
					if(strcmp(getExpr_t(quadtable[i]->result->type),"constnum_e")==0)
						printf("%d:\t %s \t %g \n",i+1,"return",quadtable[i]->result->numConst);
					else if(strcmp(getExpr_t(quadtable[i]->result->type),"conststring_e")==0)
						printf("%d:\t %s \t %s \n",i+1,"return",quadtable[i]->result->strConst);
					else if(strcmp(getExpr_t(quadtable[i]->result->type),"nil_e")==0)
						printf("%d:\t %s \t nil \n");
					else if(strcmp(getExpr_t(quadtable[i]->result->type),"constbool_e")==0){
						if(quadtable[i]->result->boolConst=='1')
							printf("%d:\t %s \t 'true' \n",i+1,"return");
						else
							printf("%d:\t %s \t 'false' \n",i+1,"return");
					}else
						printf("%d:\t %s \t %s \n",i+1,"return",quadtable[i]->result->sym->name);
				
				}
				break;
			case param:
				printf("%d:\t %s ",i+1,"param");
				if(strcmp(getExpr_t(quadtable[i]->arg1->type),"constnum_e")==0)
						printf("\t\t\t\t %g \n",quadtable[i]->arg1->numConst);
					else if(strcmp(getExpr_t(quadtable[i]->arg1->type),"conststring_e")==0)
						printf("\t\t\t\t \"%s\"\n",quadtable[i]->arg1->strConst);
					else if(strcmp(getExpr_t(quadtable[i]->arg1->type),"nil_e")==0)
						printf("\t\t\t\t nil \n");
					else if(strcmp(getExpr_t(quadtable[i]->arg1->type),"constbool_e")==0){
						if(quadtable[i]->arg1->boolConst=='1')
							printf("\t\t\t\t 'true' \n");
						else
							printf("\t\t\t\t 'false' \n");
					}else
						printf("\t\t\t\t %s \n",quadtable[i]->arg1->sym->name);
				break;
			case call:
				printf("%d:\t %s \t\t\t\t %s \n",i+1,"call",quadtable[i]->arg1->sym->name);
				break;
			case getretval:
				printf("%d:\t %s \t %s \n",i+1,"getretval",quadtable[i]->result->sym->name);
				break;
			case if_greater:
				printf("%d:\t %s \t\t \t",i+1,"if_greater");
				if(strcmp(getExpr_t(quadtable[i]->result->type),"constnum_e")==0)
					printf(" %g\t",quadtable[i]->result->numConst);
				else if(strcmp(getExpr_t(quadtable[i]->result->type),"conststring_e")==0)
					printf(" \"%s\"\t",quadtable[i]->result->strConst);
				else if(strcmp(getExpr_t(quadtable[i]->result->type),"constbool_e")==0){
					if(quadtable[i]->result->boolConst=='1')
						printf(" 'true'\t");
					else
						printf(" 'false'\t");
				}else 
					printf(" %s\t",quadtable[i]->result->sym->name);
				
				if(strcmp(getExpr_t(quadtable[i]->arg1->type),"constnum_e")==0)
					printf(" %g\t",quadtable[i]->arg1->numConst);
				else if(strcmp(getExpr_t(quadtable[i]->arg1->type),"conststring_e")==0)
					printf(" \"%s\"\t",quadtable[i]->arg1->strConst);
				else if(strcmp(getExpr_t(quadtable[i]->arg1->type),"constbool_e")==0){
					if(quadtable[i]->arg1->boolConst=='1')
						printf(" 'true'\t");
					else
						printf(" 'false'\t");
				}else 
					printf(" %s\t",quadtable[i]->arg1->sym->name);
				if(quadtable[i]->label>0)
					printf(" %d",quadtable[i]->label+1);
				printf("\n");
				break;
			case if_greatereq:
				printf("%d:\t %s \t\t \t",i+1,"if_greatereq");
				if(strcmp(getExpr_t(quadtable[i]->result->type),"constnum_e")==0)
					printf(" %g\t",quadtable[i]->result->numConst);
				else if(strcmp(getExpr_t(quadtable[i]->result->type),"conststring_e")==0)
					printf(" \"%s\"\t",quadtable[i]->result->strConst);
				else if(strcmp(getExpr_t(quadtable[i]->result->type),"constbool_e")==0){
					if(quadtable[i]->result->boolConst=='1')
						printf(" 'true'\t");
					else
						printf(" 'false'\t");
				}else 
					printf(" %s\t",quadtable[i]->result->sym->name);
				
				if(strcmp(getExpr_t(quadtable[i]->arg1->type),"constnum_e")==0)
					printf(" %g\t",quadtable[i]->arg1->numConst);
				else if(strcmp(getExpr_t(quadtable[i]->arg1->type),"conststring_e")==0)
					printf(" \"%s\"\t",quadtable[i]->arg1->strConst);
				else if(strcmp(getExpr_t(quadtable[i]->arg1->type),"constbool_e")==0){
					if(quadtable[i]->arg1->boolConst=='1')
						printf(" 'true'\t");
					else
						printf(" 'false'\t");
				}else 
					printf(" %s\t",quadtable[i]->arg1->sym->name);
				if(quadtable[i]->label>0)
					printf(" %d",quadtable[i]->label+1);
				printf("\n");
				break;
			case if_less:
				printf("%d:\t %s \t\t \t",i+1,"if_less");
				if(strcmp(getExpr_t(quadtable[i]->result->type),"constnum_e")==0)
					printf(" %g\t",quadtable[i]->result->numConst);
				else if(strcmp(getExpr_t(quadtable[i]->result->type),"conststring_e")==0)
					printf(" \"%s\"\t",quadtable[i]->result->strConst);
				else if(strcmp(getExpr_t(quadtable[i]->result->type),"constbool_e")==0){
					if(quadtable[i]->result->boolConst=='1')
						printf(" 'true'\t");
					else
						printf(" 'false'\t");
				}else 
					printf(" %s\t",quadtable[i]->result->sym->name);
				
				if(strcmp(getExpr_t(quadtable[i]->arg1->type),"constnum_e")==0)
					printf(" %g\t",quadtable[i]->arg1->numConst);
				else if(strcmp(getExpr_t(quadtable[i]->arg1->type),"conststring_e")==0)
					printf(" \"%s\"\t",quadtable[i]->arg1->strConst);
				else if(strcmp(getExpr_t(quadtable[i]->arg1->type),"constbool_e")==0){
					if(quadtable[i]->arg1->boolConst=='1')
						printf(" 'true'\t");
					else
						printf(" 'false'\t");
				}else 
					printf(" %s\t",quadtable[i]->arg1->sym->name);
				if(quadtable[i]->label>0)
					printf(" %d",quadtable[i]->label+1);
				printf("\n");
				break;
			case if_lesseq:
				printf("%d:\t %s \t\t \t",i+1,"if_lesseq");
				if(strcmp(getExpr_t(quadtable[i]->result->type),"constnum_e")==0)
					printf(" %g\t",quadtable[i]->result->numConst);
				else if(strcmp(getExpr_t(quadtable[i]->result->type),"conststring_e")==0)
					printf(" \"%s\"\t",quadtable[i]->result->strConst);
				else if(strcmp(getExpr_t(quadtable[i]->result->type),"constbool_e")==0){
					if(quadtable[i]->result->boolConst=='1')
						printf(" 'true'\t");
					else
						printf(" 'false'\t");
				}else
					printf(" %s\t",quadtable[i]->result->sym->name);
				
				if(strcmp(getExpr_t(quadtable[i]->arg1->type),"constnum_e")==0)
					printf(" %g\t",quadtable[i]->arg1->numConst);
				else if(strcmp(getExpr_t(quadtable[i]->arg1->type),"conststring_e")==0)
					printf(" \"%s\"\t",quadtable[i]->arg1->strConst);
				else if(strcmp(getExpr_t(quadtable[i]->arg1->type),"constbool_e")==0){
					if(quadtable[i]->arg1->boolConst=='1')
						printf(" 'true'\t");
					else
						printf(" 'false'\t");
				}else 
					printf(" %s\t",quadtable[i]->arg1->sym->name);
				if(quadtable[i]->label>0)
					printf(" %d",quadtable[i]->label+1);
				printf("\n");
				break;
			case if_eq:
				printf("%d:\t %s \t\t \t\t",i+1,"if_eq");
				if(strcmp(getExpr_t(quadtable[i]->result->type),"constnum_e")==0)
					printf(" %g\t",quadtable[i]->result->numConst);
				else if(strcmp(getExpr_t(quadtable[i]->result->type),"conststring_e")==0)
					printf(" \"%s\"\t",quadtable[i]->result->strConst);
				else if(strcmp(getExpr_t(quadtable[i]->result->type),"nil_e")==0)
						printf(" nil\t");
				else if(strcmp(getExpr_t(quadtable[i]->result->type),"constbool_e")==0){
					if(quadtable[i]->result->boolConst=='1')
						printf(" 'true'\t");
					else
						printf(" 'false'\t");
				}else 
					printf(" %s\t",quadtable[i]->result->sym->name);
				
				if(strcmp(getExpr_t(quadtable[i]->arg1->type),"constnum_e")==0)
					printf(" %g\t",quadtable[i]->arg1->numConst);
				else if(strcmp(getExpr_t(quadtable[i]->arg1->type),"conststring_e")==0)
					printf(" \"%s\"\t",quadtable[i]->arg1->strConst);
				else if(strcmp(getExpr_t(quadtable[i]->arg1->type),"nil_e")==0)
						printf(" nil\t");
				else if(strcmp(getExpr_t(quadtable[i]->arg1->type),"constbool_e")==0){
					if(quadtable[i]->arg1->boolConst=='1')
						printf(" 'true'\t");
					else
						printf(" 'false'\t");
				}else 
					printf(" %s\t",quadtable[i]->arg1->sym->name);

				if(quadtable[i]->label>0)
					printf(" %d",quadtable[i]->label+1);
				printf("\n");
				break;			
			case if_noteq:
				printf("%d:\t %s \t\t \t",i+1,"if_noteq");
				if(strcmp(getExpr_t(quadtable[i]->result->type),"constnum_e")==0)
					printf(" %g\t",quadtable[i]->result->numConst);
				else if(strcmp(getExpr_t(quadtable[i]->result->type),"conststring_e")==0)
					printf(" \"%s\"\t",quadtable[i]->result->strConst);
				else if(strcmp(getExpr_t(quadtable[i]->result->type),"nil_e")==0)
						printf(" nil\t");
				else if(strcmp(getExpr_t(quadtable[i]->result->type),"constbool_e")==0){
					if(quadtable[i]->result->boolConst=='1')
						printf(" 'true'\t");
					else
						printf(" 'false'\t");
				}else
					printf(" %s\t",quadtable[i]->result->sym->name);
				
				if(strcmp(getExpr_t(quadtable[i]->arg1->type),"constnum_e")==0)
					printf(" %g\t",quadtable[i]->arg1->numConst);
				else if(strcmp(getExpr_t(quadtable[i]->arg1->type),"conststring_e")==0)
					printf(" \"%s\"\t",quadtable[i]->arg1->strConst);
				else if(strcmp(getExpr_t(quadtable[i]->arg1->type),"nil_e")==0)
						printf(" nil\t");
				else if(strcmp(getExpr_t(quadtable[i]->arg1->type),"constbool_e")==0){
					if(quadtable[i]->arg1->boolConst=='1')
						printf(" 'true'\t");
					else
						printf(" 'false'\t");
				}else 
					printf(" %s\t",quadtable[i]->arg1->sym->name);
				if(quadtable[i]->label>0)
					printf(" %d",quadtable[i]->label+1);
				printf("\n");
				break;
			case jump:
				printf("%d:\t %s \t\t \t\t \t \t %d\n",i+1,"jump",quadtable[i]->label+1);
				break;
			default:
				printf("the end\n");
		}
	}

}

