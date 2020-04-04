#define HASH_MULTIPLIER 65599
#define SIZE 509
int perv_step=0;
int step=100;
/* Return a hash code for name. */
static unsigned int SymTable_hash(const char *name)
{
    size_t ui;
    unsigned int uiHash;
    uiHash = 0U;
    for (ui = 0U; name[ui] != '\0'; ui++)
    uiHash = uiHash * HASH_MULTIPLIER + name[ui];
    return uiHash%SIZE;
}

/* Einai gia ta arguments enos function */
struct ArgFunction
{
    char* name;
    struct ArgFunction* next;
};

/* einai gia tis metablhtes kai function mesa sto symbol table */
struct SymTableEntry
{
    const char *name;
    char* type;
    unsigned int scope;
    unsigned int line;
    int isActive; /* gia to hide */
    struct ArgFunction* arg; /* pointer gia ta arguments an to stoixeio einai function alliws einai panta null */
    struct SymTableEntry* nextScopeList; /* pointer gia to scope list opws sthn eikwna tou front */
    struct SymTableEntry* next; /* pointer sto deksio stoixeio */
};

/* einai gia to pinaka tou hash dld ta portokali buckets tou front*/
struct SymTable
{
    struct SymTableEntry** head;
    unsigned int size;
};

/* kanw malloc ton hashtable */
struct SymTable* SymTable_new(int size)
{
    int i=0;
    struct SymTable* node= malloc(sizeof(struct SymTable));
    node->head=malloc(size*sizeof(struct SymTableEntry*));
    for (i=0;i<size;i++) node->head[i]=NULL;
    node->size=0;
    return  node;
}

struct SymTable* ScopeTable; //pointer gia to deutero hashtable  poy deixnei sto scope list
struct SymTable* Head;
/* edw kanw eisagwgh sto scope hashtable
   Koita ti kanw...
   Otan prostithetai arxika enas kombos sto Symbol Table kanw malloc gia to node kai meta kalw auth thn synarthsh
   kai bazw sto scope hashtable ena pointer pou na deixnei sto node pou molis prosthesa sto Symbol Table
   den kanw duo nodes gia to idio stoixeio
   Ola ta stoixeia se auto to table einai pointers kai deixnoun stous kombous tou Symbol Table
    O logos pou ekana allo nextScopeList einai giati ama ekana xrhsh tou next blekontan kai ginotan xamos
    */
struct SymTable* insertNodeToScope(struct SymTable* root,struct SymTableEntry *node,int scope)
{
    if(step<=scope)
    {
        int prev_step=step;
        while(step<=scope)
            step=2*step;
        ScopeTable->head=(struct SymTableEntry**)realloc(ScopeTable->head,step*sizeof(struct SymTableEntry));
        while(prev_step<step)
        {
            ScopeTable->head[prev_step]=NULL;
            prev_step++;
        }
    }
    struct SymTableEntry *tmp=root->head[scope];
    node->nextScopeList=NULL;
    if(root->head[scope]==NULL)
    {
        root->head[scope]=node;
    }// prosthetw sthn arxh ths listas
    else
    {
        while(tmp->nextScopeList!=NULL)
            tmp=tmp->nextScopeList;
        tmp->nextScopeList=node;
    }
    return root;
}

/* eisagwgh sto Symbol Table
    einai mai aplh eisagwgh se hash table den xreiazetai na pw kati
*/
struct SymTableEntry* insertNodeToHash(struct SymTable* root,const char* name,char* type,unsigned int scope,unsigned int line,int isActive)
{
    unsigned int position= SymTable_hash(name);
    struct SymTableEntry *tmp=root->head[position];
    struct SymTableEntry *node=(struct SymTableEntry*)malloc(sizeof(struct SymTableEntry));

    assert(root);
    assert(name);
    assert(node);

    node->name=strdup(name);
    node->type=strdup(type);
    node->scope=scope;
    node->line=line;
    node->isActive=isActive;
    node->arg=NULL;
    node->next=NULL;

    if(root->head[position]==NULL)
    {
        root->head[position]=node;
    }// prosthetw sthn arxh ths listas
    else
    {
        while(tmp->next!=NULL)
            tmp=tmp->next;
        tmp->next=node;
    }

    //root->head[position]=node;

    ScopeTable=insertNodeToScope(ScopeTable,node,node->scope);

    root->size++;
    return node;
}

/* kanw hide ta stoixeia enos sygkekrimenou scope
    Epeidh ta stoixeia deixnoun sto Symbol Table, otan allazw kati sto Scope Table ginetai automata allagh kai
    ston Symbol Table*/
void Hide(struct SymTable* root,int scope)
{
    struct SymTableEntry *tmp=root->head[scope];
    while(tmp)
    {
        tmp->isActive=0;
        tmp=tmp->nextScopeList;
    }
    return;
}

/* psaxnw an uparxei hdh h metablhth sto Symbol Table */
int NameLookUpInHash(struct SymTable* root,const char* name)
{
    struct SymTableEntry* tmp;
    int i;
    for(i=0;i<SIZE;i++)
    {
        tmp=root->head[i];
        if(root->head[i]==NULL)
            continue;
        while(tmp)
        {
            if(tmp->isActive==1 && strcmp(name, tmp->name)==0)
                return 1;
            tmp=tmp->next;
        }
    }
    return 0;
}

/* psaxnw an uparxei hdh h metablhth se sugkekrimeno scope */
struct SymTableEntry * NameLookUpInScope(struct SymTable* root,unsigned int scope,const char* name)
{
    struct SymTableEntry *tmp=root->head[scope];
    while(tmp)
    {
        if(tmp->isActive==1 && strcmp(name, tmp->name)==0)
            return tmp;
        tmp=tmp->nextScopeList;
    }
    return NULL;
}

/*  eisagwgh arg se function
    to function pou antistoixei se auta nomizw oti einai to pio prosfato pou prostethke sto prohgoumeno scope,opote
    etsi briskw ton patera kai meta prosthetw se auton ta paidia tou.Epishs ta prosthetw sto telos ths listas
 */
void insertArgToNode(struct SymTable* scopeTable,const char* name,int scope)
{
    struct SymTableEntry* fatherNode=scopeTable->head[scope-1];
    struct ArgFunction* tmp=fatherNode->arg;
    struct ArgFunction* Arg=(struct ArgFunction*)malloc(sizeof(struct ArgFunction));
    Arg->name=strdup(name);
    Arg->next=NULL;

    if(fatherNode->arg==NULL)
    {
        fatherNode->arg=Arg;
    }
    else
    {
        while(tmp->next!=NULL)
            tmp=tmp->next;
        tmp->next=Arg;
    }
    return;
}
/* briskw to length ths scope listas me scope=0 kathws ekei briskontai ta lib functions(12 se arithmo)*/
int LengthOfList(struct SymTable* scopeTable)
{
    int length=0;
    struct SymTableEntry* tmp=scopeTable->head[0];
    while(tmp!=NULL)
    {
        length++;
        tmp=tmp->nextScopeList;
    }
    return length;
}

/* eksetazw an ginetai shadow libfunc */
int collisionLibFun(struct SymTable* scopeTable,const char* name)
{
    struct SymTableEntry* tmp=scopeTable->head[0];
    int skipLength=12;
    while(skipLength!=0)
    {
        if(strcmp(name, tmp->name)==0)
            return 1;
        tmp=tmp->nextScopeList;
        skipLength--;
    }
    return 0;
}



void printHash(struct SymTable* root)
{
    struct SymTableEntry* tmp;
    struct ArgFunction* arguments;
    int i;
    for(i=0;i<SIZE;i++)
    {
        tmp=root->head[i];

        if(root->head[i]==NULL)
            continue;
        printf("index%d:",i);
        while(tmp)
        {
            if(tmp != root->head[i])
                printf(",");
            printf("<%s, %s, %d, %d, %d>",tmp->name,tmp->type,tmp->scope,tmp->line,tmp->isActive);
            if(tmp->arg!=NULL)
            {
                arguments=tmp->arg;
                printf(" ( ");
                while(arguments)
                {
                    printf("%s ",arguments->name);
                    arguments=arguments->next;
                }
                printf(" ) ");
            }
            tmp=tmp->next;
        }
        printf("\n");
    }
}

void printScopeTable(struct SymTable* ScopeTable)
{
    struct SymTableEntry* tmp;
    int i;
    for(i=0;i<step;i++)
    {
        //printf("i=%d\n",i);
        tmp=ScopeTable->head[i];

        if(ScopeTable->head[i]==NULL)
            continue;
        printf("------------ Scope #%d ------------\n",i);
        while(tmp)
        {
            printf(" \"%s\"  [%s]  (line %d)  (scope %d)\n",tmp->name,tmp->type,tmp->line,tmp->scope);
            tmp=tmp->nextScopeList;
        }
        printf("\n");
    }
}

/*int main()
{
    struct SymTable* Head=SymTable_new(509);
    ScopeTable=SymTable_new(100);
    Head=insertNodeToHash(Head,"input","funLib",0,0,1);
    Head=insertNodeToHash(Head,"x","local",0,1,1);
    Head=insertNodeToHash(Head,"x","global",0,5,1);
    Head=insertNodeToHash(Head,"print","funLib",1,2,1);
    Head=insertNodeToHash(Head,"if","funLib",1,2,1);
    Head=insertNodeToHash(Head,"if","funLib",2,2,1);
    Head=insertNodeToHash(Head,"input","funLib",2,3,1);
    printHash(Head);
    printf("\n\n");
    printScopeTable(ScopeTable);
    Hide(ScopeTable,1);
    printf("\n\n");
    printScopeTable(ScopeTable);
    printf("\n\n");
    printHash(Head);
    printf("\n\n");
    printf("%s",NameLookUpInScope(ScopeTable,0,"x")->name);
    printf("\n\n");
    printHash(Head);
    printf("\n\n\n");
    insertArgToNode(ScopeTable,"alex",3);
    insertArgToNode(ScopeTable,"maria",3);
    insertArgToNode(ScopeTable,"marianthi",2);
    printScopeTable(ScopeTable);
    printf("\n\n");
    printHash(Head);
    printf("\n\n");
    printf("collision: %d\n",collisionLibFun(ScopeTable,"input"));
    Head=insertNodeToHash(Head,"y","local",150,2,1);
    printHash(Head);
    printf("\n\n");
    printScopeTable(ScopeTable);
    printf("\n\n");
    Head=insertNodeToHash(Head,"k","local",250,2,1);
    printHash(Head);
    printf("\n\n");
    printScopeTable(ScopeTable);
    printf("\n\n");
    Head=insertNodeToHash(Head,"z","local",567,2,1);
    printHash(Head);
    printf("\n\n");
    printScopeTable(ScopeTable);
    printf("\n\n");
    Head=insertNodeToHash(Head,"w","local",800,2,1);
    Head=insertNodeToHash(Head,"w","local",1601,2,1);
    Head=insertNodeToHash(Head,"w","local",800,2,1);
    Head=insertNodeToHash(Head,"w","local",602,2,1);
    Head=insertNodeToHash(Head,"w","local",3200,2,1);
    printHash(Head);
    printf("\n\n");
    printScopeTable(ScopeTable);
    return 0;
}*/
