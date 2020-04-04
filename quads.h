enum iopcode{	
	assign, add, sub, mul, div, mod,
	uminus, and, or, not, if_eq, if_noteq,
	if_lesseq, if_geatereq,	if_less,
	if_greater, jump, call,
	param, return, getretval,
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
	unsigned label;
	unsigned line;
};

