/* A Bison parser, made by GNU Bison 3.0.2.  */

/* Bison interface for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2013 Free Software Foundation, Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

#ifndef YY_YY_PARSER_H_INCLUDED
# define YY_YY_PARSER_H_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
extern int yydebug;
#endif

/* Token type.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    STRING = 258,
    FALSE = 259,
    TRUE = 260,
    NIL = 261,
    FLOAT = 262,
    id = 263,
    NUMBER = 264,
    NEWLINE = 265,
    NEWTAB = 266,
    IF = 267,
    ELSE = 268,
    WHILE = 269,
    FOR = 270,
    FUNCTION = 271,
    RETURN = 272,
    BREAK = 273,
    CONTINUE = 274,
    AND = 275,
    NOT = 276,
    OR = 277,
    LOCAL = 278,
    SPACE = 279,
    EQUAL = 280,
    PLUS = 281,
    MINUS = 282,
    MULTIPLE = 283,
    FORWARD_SLASH = 284,
    BACKWARD_SLASH = 285,
    PERCENT = 286,
    DOUBLE_EQUAL = 287,
    NOT_EQUAL = 288,
    DOUBLE_PLUS = 289,
    DOUBLE_MINUS = 290,
    GREATER = 291,
    LESS = 292,
    GREATER_EQUAL = 293,
    LESS_EQUAL = 294,
    LEFT_CURLY_BRACKET = 295,
    RIGHT_CURLY_BRACKET = 296,
    LEFT_SQUARE_BRACKET = 297,
    RIGHT_SQUARE_BRACKET = 298,
    LEFT_PARENTHESES = 299,
    RIGHT_PARENTHESES = 300,
    SEMI_COLON = 301,
    COMMA = 302,
    COLON = 303,
    NAMESPACE_ALIAS_QUALIFIER = 304,
    DOT = 305,
    DOUBLE_DOT = 306,
    LINE_COMMENT = 307,
    MULTI_COMMENT = 308,
    CARRIAGE_RETURN = 309,
    OTHER = 310,
    UMINUS = 311
  };
#endif
/* Tokens.  */
#define STRING 258
#define FALSE 259
#define TRUE 260
#define NIL 261
#define FLOAT 262
#define id 263
#define NUMBER 264
#define NEWLINE 265
#define NEWTAB 266
#define IF 267
#define ELSE 268
#define WHILE 269
#define FOR 270
#define FUNCTION 271
#define RETURN 272
#define BREAK 273
#define CONTINUE 274
#define AND 275
#define NOT 276
#define OR 277
#define LOCAL 278
#define SPACE 279
#define EQUAL 280
#define PLUS 281
#define MINUS 282
#define MULTIPLE 283
#define FORWARD_SLASH 284
#define BACKWARD_SLASH 285
#define PERCENT 286
#define DOUBLE_EQUAL 287
#define NOT_EQUAL 288
#define DOUBLE_PLUS 289
#define DOUBLE_MINUS 290
#define GREATER 291
#define LESS 292
#define GREATER_EQUAL 293
#define LESS_EQUAL 294
#define LEFT_CURLY_BRACKET 295
#define RIGHT_CURLY_BRACKET 296
#define LEFT_SQUARE_BRACKET 297
#define RIGHT_SQUARE_BRACKET 298
#define LEFT_PARENTHESES 299
#define RIGHT_PARENTHESES 300
#define SEMI_COLON 301
#define COMMA 302
#define COLON 303
#define NAMESPACE_ALIAS_QUALIFIER 304
#define DOT 305
#define DOUBLE_DOT 306
#define LINE_COMMENT 307
#define MULTI_COMMENT 308
#define CARRIAGE_RETURN 309
#define OTHER 310
#define UMINUS 311

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE YYSTYPE;
union YYSTYPE
{
#line 99 "parser.y" /* yacc.c:1909  */

	char* strVal;
	unsigned char uncharVal;
	int intVal;
	double dbVal;
	struct SymTableEntry* tmpnode;
	struct expr* expr;
	struct call* call;

#line 176 "parser.h" /* yacc.c:1909  */
};
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;

int yyparse (void);

#endif /* !YY_YY_PARSER_H_INCLUDED  */
