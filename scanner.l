%{
	
	#include <stdio.h>
	#include <string.h>
	#include <stdlib.h>
	#include "parser.h"
	/*#define  YY_DECL int alpha_yylex (void* ylval)*/
	
	int comm=0;
	int newwhite=0;
	int newspace=0;
	int newtab=0;
	int newline=0;
	unsigned int numToken=1;
	struct alpha_token_t * Head=NULL;

	struct alpha_token_t {
		unsigned int numline;
		unsigned int numToken;
		char *content;
		char *type;
		char *value;
		char *type2;
		struct alpha_token_t *alpha_yylex;
	};

	void AddToList(unsigned int numline,unsigned int numToken,char *content,char *type,char* value,char* type2)
	{
		
		char* test;
		char *temporary = test;
		struct alpha_token_t * node=NULL;
		struct alpha_token_t * tmp=(struct alpha_token_t*)malloc(sizeof(struct alpha_token_t));
       		tmp->content=strdup(content);
		tmp->type=strdup(type);
		tmp->type2=strdup(type2);
		if (strcmp(type,"CONST_INT")==0 || strcmp(type,"REAL_CONST")==0 || strcmp(type,"STRING")==0 || strcmp(type,"ID")==0){
			tmp->value=strdup(content);
		}
		else{
			tmp->value=strdup(value);
		}
		tmp->numline=numline;
		tmp->numToken=numToken;
		tmp->alpha_yylex=NULL;
		if(Head==NULL)
		{
			Head=tmp;
		}
		else
		{
			node=Head;
			while(node->alpha_yylex!=NULL){
				node=node->alpha_yylex;
			}
			node->alpha_yylex=tmp;
		}
		/*if(strcmp(tmp->type2,"char*")==0)
			printf("%u: #%u \"%s\" %s  \"%s\" <- %s\n",tmp->numline,tmp->numToken,tmp->content,tmp->type,tmp->value,tmp->type2);
		else
                       	printf("%u: #%u \"%s\" %s  %s <- %s\n",tmp->numline,tmp->numToken,tmp->content,tmp->type,tmp->value,tmp->type2);*/

		return;
	}

%}

%option noyywrap
%option yylineno 


id		[a-zA-Z][a-zA-Z_0-9]*
NUMBER		[0-9]+
FLOAT		([0-9]+([.][0-9]*)?|[.][0-9]+)
STRING			\"
NEWLINE			"\n"
NEWTAB			"\t"
IF              "if"
ELSE            "else"
WHILE           "while"
FOR             "for"
FUNCTION        "function"
RETURN          "return"
BREAK           "break"
CONTINUE        "continue"
AND             "&&"
NOT             "!"
OR              "||"
LOCAL           "local"
TRUE            "true"
FALSE           "false"
NIL             "nil"
SPACE		" "
EQUAL		"="
PLUS		"+"
MINUS		"-"
MULTIPLE	"*"
FORWARD_SLASH	"/"
BACKWARD_SLASH	"\"
PERCENT		"%"
DOUBLE_EQUAL	"=="
NOT_EQUAL	"!="
DOUBLE_PLUS	"++"
DOUBLE_MINUS	"--"
GREATER		">"
LESS		"<"
GREATER_EQUAL	">="
LESS_EQUAL	"<="

LEFT_CURLY_BRACKET	"{"
RIGHT_CURLY_BRACKET     "}"
LEFT_SQUARE_BRACKET	"["
RIGHT_SQUARE_BRACKET	"]" 
LEFT_PARENTHESES	"(" 
RIGHT_PARENTHESES	")"
SEMI_COLON		";" 
COMMA			"," 
COLON			":" 
NAMESPACE_ALIAS_QUALIFIER "::" 
DOT			"." 
DOUBLE_DOT		".."
LINE_COMMENT 	"//"
MULTI_COMMENT 	"/*"
CARRIAGE_RETURN	"\r"
OTHER	.

%%
{CARRIAGE_RETURN}	{newwhite++;}
{LEFT_CURLY_BRACKET}  	{AddToList(yylineno,numToken,"{","PUNCTUATION","LEFT_CURLY_BRACKET","enumerated"); numToken++; return LEFT_CURLY_BRACKET;}
{RIGHT_CURLY_BRACKET}  	{AddToList(yylineno,numToken,"}","PUNCTUATION","RIGHT_CURLY_BRACKET","enumerated"); numToken++; return RIGHT_CURLY_BRACKET;}
{LEFT_SQUARE_BRACKET}  	{AddToList(yylineno,numToken,"[","PUNCTUATION","LEFT_SQUARE_BRACKET","enumerated"); numToken++; return LEFT_SQUARE_BRACKET;}
{RIGHT_SQUARE_BRACKET}  {AddToList(yylineno,numToken,"]","PUNCTUATION","RIGHT_SQUARE_BRACKET","enumerated"); numToken++; return RIGHT_SQUARE_BRACKET;}
{LEFT_PARENTHESES}  	{AddToList(yylineno,numToken,"(","PUNCTUATION","LEFT_PARENTHESES","enumerated"); numToken++; return LEFT_PARENTHESES;}
{RIGHT_PARENTHESES}  	{AddToList(yylineno,numToken,")","PUNCTUATION","RIGHT_PARENTHESES","enumerated"); numToken++; return RIGHT_PARENTHESES;}
{SEMI_COLON}  	{AddToList(yylineno,numToken,";","PUNCTUATION","SEMI_COLON","enumerated"); numToken++; return SEMI_COLON;}
{COMMA}  	{AddToList(yylineno,numToken,",","PUNCTUATION","COMMA","enumerated"); numToken++; return COMMA;}
{COLON}  	{AddToList(yylineno,numToken,":","PUNCTUATION", "COLON","enumerated"); numToken++; return COLON;}
{NAMESPACE_ALIAS_QUALIFIER}  {AddToList(yylineno,numToken,"::","PUNCTUATION","NAMESPACE_ALIAS_QUALIFIER","enumerated"); numToken++; return NAMESPACE_ALIAS_QUALIFIER;}
{DOT}  		{AddToList(yylineno,numToken,".","PUNCTUATION","DOT","enumerated"); numToken++; return DOT;}
{DOUBLE_DOT} 	{AddToList(yylineno,numToken,"..","PUNCTUATION","DOUBLE_DOT","enumerated"); numToken++; return DOUBLE_DOT;}

{SPACE}		{newspace=+1; }
{IF}            {AddToList(yylineno,numToken,"if","KEY WORD","IF","enumerated"); numToken++; return IF;}

{ELSE}          {AddToList(yylineno,numToken,"else","KEYWORD","ELSE" ,"enumerated"); numToken++; return ELSE;}
{WHILE}         {AddToList(yylineno,numToken,"while","KEYWORD","WHILE" ,"enumerated"); numToken++; return WHILE;}
{FOR}           {AddToList(yylineno,numToken,"for","KEYWORD","FOR" ,"enumerated"); numToken++; return FOR;}
{FUNCTION}      {AddToList(yylineno,numToken,"function","KEYWORD","FUNCTION" ,"enumerated"); numToken++; return FUNCTION;}
{RETURN}        {AddToList(yylineno,numToken,"return","KEYWORD","RETURN" ,"enumerated"); numToken++; return RETURN;}
{BREAK}         {AddToList(yylineno,numToken,"break","KEYWORD","BREAK" ,"enumerated"); numToken++; return BREAK;}
{CONTINUE}      {AddToList(yylineno,numToken,"continue","KEYWORD","CONTINUE" ,"enumerated"); numToken++; return CONTINUE;}
{AND}           {AddToList(yylineno,numToken,"and","KEYWORD","AND", "enumerated"); numToken++; return AND;}
{NOT}           {AddToList(yylineno,numToken,"not","KEYWORD","NOT" ,"enumerated"); numToken++; return NOT;}
{OR}            {AddToList(yylineno,numToken,"or","KEYWORD","OR" ,"enumerated"); numToken++; return OR;}
{LOCAL}         {AddToList(yylineno,numToken,"local","KEYWORD","LOCAL" ,"enumerated"); numToken++; return LOCAL;}
{TRUE}          {AddToList(yylineno,numToken,"true","KEYWORD","TRUE" ,"enumerated"); numToken++; return TRUE;}
{FALSE}         {AddToList(yylineno,numToken,"false","KEYWORD","FALSE" ,"enumerated"); numToken++; return FALSE;}
{NIL}           {AddToList(yylineno,numToken,"nil","KEYWORD","NIL", "enumerated"); numToken++; return NIL;}
{EQUAL}         {AddToList(yylineno,numToken,yytext,"OPERATOR","EQUAL" ,"enumerated"); numToken++; return EQUAL;}
{PLUS}          {AddToList(yylineno,numToken,yytext,"OPERATOR","PLUS" ,"enumerated"); numToken++; return PLUS;}
{MINUS}         {AddToList(yylineno,numToken,yytext,"OPERATOR","MINUS" ,"enumerated"); numToken++; return MINUS;}
{MULTIPLE}		{AddToList(yylineno,numToken,yytext,"OPERATOR","MULTIPLE" ,"enumerated"); numToken++; return MULTIPLE;}
{FORWARD_SLASH} {AddToList(yylineno,numToken,yytext,"OPERATOR","FORWARD_SLASH" ,"enumerated"); numToken++; return FORWARD_SLASH;}
{PERCENT}       {AddToList(yylineno,numToken,yytext,"OPERATOR","PERCENT" ,"enumerated"); numToken++; return PERCENT;}
{DOUBLE_EQUAL}  {AddToList(yylineno,numToken,yytext,"OPERATOR","DOUBLE_EQUAL" ,"enumerated"); numToken++; return DOUBLE_EQUAL;}
{NOT_EQUAL}     {AddToList(yylineno,numToken,yytext,"OPERATOR","NOT_EQUAL" ,"enumerated"); numToken++; return NOT_EQUAL;}
{DOUBLE_PLUS}   {AddToList(yylineno,numToken,yytext,"OPERATOR","DOUBLE_PLUS" ,"enumerated"); numToken++; return DOUBLE_PLUS;}
{DOUBLE_MINUS}  {AddToList(yylineno,numToken,yytext,"OPERATOR","DOUBLE_MINUS" ,"enumerated"); numToken++; return DOUBLE_MINUS;}
{GREATER}       {AddToList(yylineno,numToken,yytext,"OPERATOR","GREATER" ,"enumerated"); numToken++; return GREATER;}
{LESS}		{AddToList(yylineno,numToken,yytext,"OPERATOR","LESS" ,"enumerated"); numToken++; return LESS;}
{GREATER_EQUAL}   {AddToList(yylineno,numToken,yytext,"OPERATOR","GREATER_EQUAL" ,"enumerated"); numToken++; return GREATER_EQUAL;}
{LESS_EQUAL}      {AddToList(yylineno,numToken,yytext,"OPERATOR","LESS_EQUAL" ,"enumerated"); numToken++; return LESS_EQUAL;}

{NEWTAB}	{newtab=+1; }
{NEWLINE}	{newline=+1; }

{id}		{AddToList(yylineno,numToken,yytext,"ID",yytext,"char*"); numToken++; yylval.strVal =strdup(yytext);
			 return id;}
{NUMBER}    	{AddToList(yylineno,numToken,yytext,"CONST_INT",yytext,"integer"); numToken++; yylval.intVal = atoi(yytext); 
			 return NUMBER;}
{FLOAT}		{AddToList(yylineno,numToken,yytext,"REAL_CONST",yytext,"float"); numToken++; yylval.dbVal= atof(yytext);
			 return FLOAT;}
{STRING}    {
			//printf("yytext=%s\n",yytext);
			int a=0;
			int step = 30;
			char* word=(char *)malloc(sizeof(char)*step);
			char c;
			int flag=1,i=0,slash=0;
			
						
			while((c=input())!=EOF)
			{
				if(i>step){
					step*=2;
					word = (char *)realloc(word, sizeof(char)*step);
				}
			
				
				if (c=='\"' && slash==0)
                                {
                                        /*printf("telos");*/
                                        
					*(word+i)='\0';
                                        //printf("---word=%s---\n",word);
					//char* input = (char *)malloc(sizeof(char)*i);
					AddToList(yylineno,numToken,word,"STRING",word,"char*"); 
					numToken++;
					yylval.strVal =strdup(word);
					free(word);
                                      	flag=0;
					return STRING;
					break;
                                }
				else if((c== '\\' ) && (slash==0))    
				{
					/*printf("Proto Slash\n");*/	
					slash=1;
					
				}
				else if(c=='\\' && slash==1)
				{	/*printf("diplo slash\n");*/
					*(word+i)=c;
					i++;
					slash=0;
			
				}
				else if(slash==0 && c!='\\')
				{	/*printf("opiosdipote allos xaraktiras\n");*/
					*(word+i)=c;
					i++;
                                        
                                        
				}
				else if(slash==1 && c=='\"')
				{	/*printf("slash autakia");*/
					*(word+i)=c;
					i++;
                                        
                                        slash=0;
                                        	
				}
				else if(slash==1 && c!='\\'){
                                        if (c=='n')
					{	*(word+i)='\n';
						i++;
						
					}
					else if (c=='t')
					{	
						*(word+i)='\t';
						i++;
						
					}
					else {
						*(word+i)='\\';
						i++;
        	                                
						*(word+i)=c;
						i++;

					}
					
					slash=0;
				}
				else {

					printf("AGNOSTI PERIPTOSI, break\n");
					break;}
				
			}
				
				if (flag==1)
				{
					printf("\nERROR: string doesn't close\n\n");
					return;
				}
			
	}

{LINE_COMMENT} 	{char c='a';
			AddToList(yylineno,numToken,yytext,"COMMENT","ONE_LINE_COMMENT","enumerated"); 
			comm=yylineno; 
			while(comm==yylineno && c!=EOF) c=input(); 
			numToken++;
		}
{MULTI_COMMENT} {int c;
		 int j=0;
		 int com=1;
		 int n1;
		 int i=0;
		 char p;
		 char lol[10];
		 char* wher=(char *)malloc(sizeof(char));
		 char* num=(char *)malloc(sizeof(char));
		 *(num+i)=yylineno+'0';
			while(j==0){
				while(c!='/' && c!='*' && c!=EOF) c=input();
				if(c=='/'){
					c=input();
					if(c!=EOF && c=='*'){ 
						com++;
						i++;
						*(num+i)=yylineno+'0';
						c = input();
					}	
				}else if(c=='*'){
					c = input();
					if(c!=EOF && c=='/'){
						com--;
						p=*(num+i);
						n1=p-'0'; 
						sprintf(wher, "%d", n1);					
						strcat(wher,"-");
						sprintf(lol, "%d", yylineno);	
						strcat(wher,lol);
						n1=strlen(wher);
						*(wher+n1)='\0';
						i--;
						if(com==0){
							AddToList(yylineno,numToken,wher,"COMMENT","MULTILINE_COMMENT","enumerated");
							numToken++;
							break;
						}else{
							AddToList(yylineno,numToken,wher,"NESTED_COMMENT","MULTILINE_COMMENT","enumerated");
							numToken++;
						}
						c = input();
						strcpy(wher,"");
						strcpy(lol,"");
					}
				}
				if(c==EOF){
					printf("\nERROR: THE COMMENTS ON LINES ");
					while(i>-1){
						p=*(num+i);
						n1=p-'0';
						sprintf(lol, "%d", n1);
						printf("%s ",lol);
						strcpy(lol,"");
						i--;    
					}
					printf("DOESNT CLOSE\n\n");
					break;
				}				
			}	
			//free(num);
			//free(wher);
		}
{OTHER}	{printf("ERROR: Undefine character \"%s\"\n", yytext); }

%%
/*
int main(int argc,char** argv)
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
		printf("Give an input from here\n");
		yyin=stdin;	
	}
	alpha_yylex(Head);
	return 1;
}
*/