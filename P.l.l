%option yylineno
%{
        #include<stdio.h>
        #include<string.h>
        #include "y.tab.h"
	#include<math.h>
        extern int var_count;
	char cname[]="notcl";
	char classn[]="";
	int fcount=0;
	char new_dd_list[5][5];
	char uddstore[]="notud";
	int uflag=0;
	int ucount=0;
	char funcn[5][5];
	char fname[]="notfunc";
%}

%%
"if" {return IF;}
"#"[a-zA-Z0-9] {printf("Comment");}
"else" {return ELSE;}
"repeat" {return DO;}
"switch" {return SWITCH;}
"for" {return FOR;}//for declaring for loop construct in our program
"while" {return WHILE;}
"to" {return TO;}
"read" {return READ;}
"class" {strcpy(cname,"class");
	return CLASS;}
"new" {return NEW;}
"uddcreate" {strcpy(uddstore,"udd");return UDD;}
"func" {strcpy(fname,"func"); 
	return FUNC;}
"return" {return RETURN;}
"write" {return PRINT;}
"case" {return CASE;}
"break" {return BREAK;}
"int" {yylval.data_type=0;return INT;}
"character" {yylval.data_type=1; return CHAR;}
"double" {yylval.data_type=2; return DOUBLE;}
"float" {yylval.data_type=3; return FLOAT;}
"boolean" {return BOOL;}
"main" {strcpy(fname,"notfunc");return MAIN;}
"default" {return DEFAULT;}
"." {return CATT;}
"(" {return LB;}
")" {return RB;}
"{" {return LCB;}
"}" {if(strcmp(uddstore,"udd")==0){uflag=1;} return RCB;}
"," {return COMA;}
";" {return SC;}
"[" {return DIMO;}
"'" {return OPEN;}
"]" {return DIMC;}
"++" {return UPLUS;}
"--" {return UMINUS;}
"&" {return PTRA;}
"^" {return EXP;}
[+] {return PLUS;}
[-] {return MINUS;}
"*" {return MUL;}
"%" {return MOD;}
[/] {return DIV;}
"<" {return LT;}
"<=" {return LE;}
">" {return GT;}
">=" {return GE;}
"==" {return EQL;}
"!=" {return NE;}
"=" {return EQ;}
"&&" {return AND;}
"||" {return OR;}
"!" {return NOT;}
"?" {return CONDIF;}
":" {return CONDTHEN;}
[0-9]+ {yylval.data_type=yytext;return VAL;}
[a-z]+ {strcpy(yylval.var_name,yytext);
	if(strcmp("class",cname)==0)
	{strcpy(classn,yytext);strcpy(cname,"notc1");return CLASSNAME;} 
	if(strcmp(fname,"func")==0) 
	{strcpy(funcn[fcount],yytext);fcount=fcount+1;strcpy(fname,"notfunc"); return FUNCNAME;}
	if(strcmp(classn,yytext)==0)
	{return CLASSNAME;}
	for(int i=0;i<=fcount;i++)
	{if(strcmp(yytext,funcn[i])==0)
	{return FUNCNAME;
	}
	}
	if(uflag==1)
	{strcpy(uddstore,"notudd"); uflag=0; strcpy(new_dd_list[ucount],yytext);ucount=ucount+1;return NEWDD;}
	for(int i=0;i<=ucount;i=i+1)
	{if(strcmp(new_dd_list[i],yytext)==0)
	{return NEWDD;
	}
	}
	return VAR;}
[\n\t ]+ {/*new line or space*/}
. {printf("invalid character sequence %s\n",yytext); exit(0);}
%%
int yywrap(void) {
    return 1;
}

