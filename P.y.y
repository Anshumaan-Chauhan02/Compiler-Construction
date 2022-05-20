%{
        #include<stdio.h>
        #include<stdlib.h>
        #include<string.h>
        int yylex(void);
        int yyerror(const char *s);
        int success = 1;
        int current_data_type;
        int expn_type=-1;
        int temp;
	int dimc[5]={-1,-1,-1,-1,-1};
	int cdim=0;
	int adj=0;
        struct symbol_table{char var_name[30]; int type; int dim; int dimb[5]; int depth;}var_list[20];
        int var_count=-1;
	int dimcheckr=-1;
	int ptdep=0;
	int pcheckr;
	char func_table[5][5];
	char new_dd[5][5];
	int func_ret[5]={-1,-1,-1,-1,-1};
	int func_param[5][5]={{-1,-1,-1,-1,-1},{-1,-1,-1,-1,-1},{-1,-1,-1,-1,-1},{-1,-1,-1,-1,-1},{-1,-1,-1,-1,-1}};
	int fc=0;
	char printudd[5];
	char pst[5][5];
	int psti=0;
	int pc=0;
	int dc=0;
	int top=0;
	int flag=1;
	int checkpara[5]={-1,-1,-1,-1,-1};
	int cpc=0;
	extern int lookup_depth(char var[30]);
        extern int lookup_in_table(char var[30]);
	extern int lookup_dim(char var[30]);
        extern void insert_to_table(char var[30], int type, int dim, int dimb[5], int ptrdepth);
%}

%union{
int data_type;
char var_name[30];
}
%token OBJNAME TO OPEN CONS CLASSNAME FUNCNAME DEFAULT PRINT FUNC CLASSNAME BOOL RETURN SWITCH READ CATT NEWDD UDD CLASS NEW PTRA LT LE GT GE EQL NE FOR DIMO DIMC DO BREAK CASE UDHEAD WHILE CONDIF CONDTHEN MAIN VAL IF LB RB LCB RCB SC COMA VAR EQ PLUS MINUS ELSE MUL DIV EXP UPLUS UMINUS

%right NOT
%right EXP
%left PLUS MINUS
%left MUL DIV MOD
%left AND OR
%token<data_type>INT
%token<data_type>CHAR
%token<data_type>FLOAT
%token<data_type>DOUBLE
%type<data_type>DATA_TYPE
%type<data_type>A_EXPN
%type<var_name>VAR
%type<var_name>NEWDD
%type<var_name>VARARR
%type<var_name>PTR
%type<var_name>FUNCNAME
%type<var_name>CLASSNAME
%type<data_type>VAL
%start prm
%%
prm     : HEAD FUNC_DEC FUNC MAIN_TYPE MAIN_CONS LB RB LCB BODY RCB {printf("}");} RCB {
                                                       printf("\n parsing successful\n");
                                                  }
HEAD : CLASS CLASSNAME LCB {printf("#include<stdio.h>\n");}

MAIN_CONS : MAIN { printf("main()\n");
		   printf("{");}

BODY : DECLARATION_STATEMENTS BODY|  PROGRAM_STATEMENTS BODY| DECLARATION_STATEMENTS|PROGRAM_STATEMENTS

DECLARATION_STATEMENTS : DECLARATION_STATEMENT DECLARATION_STATEMENTS
                                                  {
                                                      
                                                  }
                        | DECLARATION_STATEMENT {}
DECLARATION_STATEMENT: DATA_TYPE VAR_LIST SC {
			printf(";\n");
			} | UDDCONS {}
VAR_LIST : VAR COMA {printf(",");} VAR_LIST {
                                insert_to_table($1,current_data_type,cdim,dimc,ptdep);
				cdim=0;
				ptdep=0;
				printf($1);
                             }
        | VAR {
                insert_to_table($1,current_data_type,cdim,dimc,ptdep);
              cdim=0;
	      ptdep=0;
	      printf($1);
	      }
	|VARARR {insert_to_table($1,current_data_type,cdim,dimc,ptdep);
	cdim=0;
	ptdep=0;
	}
	|VARARR COMA {printf(",");} VAR_LIST {
		insert_to_table($1,current_data_type,cdim,dimc,ptdep);
	cdim=0;
	ptdep=0;
	}
	|PTR {insert_to_table($1,current_data_type,cdim,dimc,ptdep);
	cdim=0;
	ptdep=0;
	}

VARARR : VAR DIMO VAL DIMC {strcpy($$,$1);cdim++;dimc[cdim]=$3;printf($1);
	printf("[");
	printf($3);
	 }
	|	VARARR DIMO VAL DIMC {cdim++; dimc[cdim]=$3; printf("[");
	printf($3);
	printf("]");}
	|VAR DIMO VAR DIMC {strcpy($$,$1);cdim++; printf($1);
	printf("[");
	printf($3);
	printf("]");}
	|VARARR DIMO VAR DIMC {cdim++;printf("[");
	printf($3);
	printf("]");}
PTR : MUL {printf("*");} PTR {ptdep++; /*printf("Ptr value=%d",ptdep);*/ strcpy($$,$3);}|VAR {strcpy($$,$1); printf($1);}

UDDCONS : UDD {printf("struct{\n");} 
	LCB DECLARATION_STATEMENTS RCB NEWDD { printf("}"); printf($6);} 
	SC {
	printf(";\n");
	strcpy(new_dd[dc],$6);
	dc=dc+1;
	}


OBJCR : CLASSNAME VAR EQ NEW CLASSNAME LB RB SC 
	{
	if(lookup_in_table($2)==-1)
	{
	insert_to_table($2,7,cdim,dimc,ptdep);
	cdim=0;
	ptdep=0;
	}
	else
	{printf("\nName already used for other purpose\n");
	exit(0);
	}
	}

PASSPARA : VAR {checkpara[0]=lookup_in_table($1); printf($1);}| VAR COMA PASSPARA {checkpara[cpc]=lookup_in_table($1);cpc=cpc+1; printf($1); printf(",");}

FUNCCALL : VAR EQ VAR CATT FUNCNAME {printf($5); printf("(");} LB PASSPARA RB SC
		{
		printf(");\n");
		for(int i=0;i<=4;i=i+1)
		{if(strcmp(func_table[i],$5)==0)
		{for(int j=0;j<=4;j=j+1)
		{if(func_param[i][j]!=checkpara[j])
		{printf("\nWrong parameters\n");
		exit(0);
		}
		}
		if(func_ret[i]==lookup_in_table($1))
		{
		}
		else
		{printf("\nWrong variable for storing\n");
		exit(0);
		}
		}
		}
		if(lookup_in_table($3)==7)
		{
		}
		else
		{printf("\nVariable used instead of an Object/ Object not defined\n");
		exit(0);
		}
		}

PARAMETERS : DATA_TYPE VAR {
			func_param[fc][0]=current_data_type;
			insert_to_table($2,current_data_type,cdim,dimc,ptdep);
			cdim=0;
			ptdep=0;
			printf($2);
			printf(")\n");
			printf("{");
			}
		| DATA_TYPE VAR COMA PARAMETERS
		{ func_param[fc][pc]=current_data_type;
		pc=pc+1;
		printf($2);
		printf(",");
		}


FUNC_DEC : FUNC DATA_TYPE FUNCNAME {printf($3); printf("(");} LB PARAMETERS RB LCB PROGRAM_STATEMENTS RCB
		{
		
		strcpy(func_table[fc],$3);
		func_ret[fc]=$2;
		fc=fc+1;
		printf("\n}\n");
		}

MAIN_TYPE : INT {int flag=0;printf("int ");}

DATA_TYPE : INT {
                        $$=$1;
                        current_data_type=$1;
			printf("int ");
                }
        | CHAR {
                        $$=$1;
                        current_data_type=$1;
			printf("char ");
                }
      | FLOAT {
                        $$=$1;
                        current_data_type=$1;
			printf("float ");
                }
        | DOUBLE {
                        $$=$1;
                        current_data_type=$1;
			printf("double ");
                }
	| NEWDD { for(int i=0;i<=4;i=i+1)
		{if(strcmp($1,new_dd[i])==0)
		{
		current_data_type=8+i;
		$$=current_data_type;
		printf(new_dd[i]);
		printf(" ");
		}
		else
		{printf("Datatype not declared");
		exit(0);
		}
		}
		}

IFCONS : IF {printf("if(");} LB L_EXPNS RB LCB {printf(")\n{");} BODY {printf("\n");} RCB {printf("}\n");} ELSE LCB {printf("else{\n");} BODY RCB {printf("\n}");}
	
SWITCHC : SWITCHCONS {printf("}\n");}
SWITCHCONS : SWITCH LB VAR RB LCB {printf("\nswitch("); printf($3); printf(")\n{");} SWITCHCONS RCB | CASECONS SWITCHCONS| DEFAULTCONS
CASECONS : CASE OPEN VAL {printf("case \""); printf($3); printf("\":");} OPEN CONDTHEN PROGRAM_STATEMENTS BREAK SC {printf("\nbreak;\n");}
DEFAULTCONS : DEFAULT CONDTHEN {printf("default:");} PROGRAM_STATEMENTS {}

WHILECONS : WHILE {printf("while(");} LB L_EXPNS RB LCB {printf(")\n{\n");} BODY RCB {
		printf("\n}\n");
		}

DOCONS : DO {printf("do{\n");} LCB BODY RCB WHILE {printf("\n}while(");} LB L_EXPNS RB SC {printf(");\n");}

RETURNCONS : RETURN SC {printf("\nreturn"); printf(";\n");} | RETURN VAR SC {printf("\nreturn "); printf($2); printf(";\n");}

FORCONS : FOR {printf("for(");} LB PROGRAM_STATEMENT {int xyz=0; while(xyz<psti){printf("%s",pst[xyz]); xyz=xyz+1;} psti=0; printf(";");} L_EXPNS SC {printf(";");} A_EXPN RB LCB {printf(")\n{\n");} BODY RCB {printf("\n}");}  

INPUTCONS : READ LB VAR RB SC {printf("scanf(\"%%"); int t=lookup_in_table($3);
                                 if(t==0)
                                  {printf("d");
                                 }
                                 else
                                 {if(t==1)
                                 {printf("s");
                                 }
                                 else
                                 {if(t==2)
                                 {printf("f");
                                 }
                                 else
                                 {printf("f");
                                 }
                                 }
                                 }
				printf("\",&");
				printf($3);
				printf(");\n");
				}

PRINTCONS : PRINT LB VAR RB SC {printf(" printf(\"%%"); int t=lookup_in_table($3);
				if(t==0)
				{printf("d");
				}
				else
				{if(t==1)
				{printf("s");
				}
				else
				{if(t==2)
				{printf("f");
				}
				else
				{printf("f");
				}
				}
				}
				printf("\",");
				printf($3);
				printf(")");
				printf(";\n");
				}

PROGRAM_STATEMENTS : PROGRAM_STATEMENT PROGRAM_STATEMENTS
                                                  {
                                                   
                                                  }
                        |PROGRAM_STATEMENT
                        |IFCONS
                        |WHILECONS
                        |DOCONS
                        |FORCONS
			|SWITCHC

PROGRAM_STATEMENT : VAR EQ VAL SC {printf($1);printf("=");printf($3); printf("\n");			
					}
		   | VAR EQ A_EXPN SC { printf($1);
					printf("=");
					int xyz=0;
					while(xyz<psti)
					{
					printf("%s",pst[xyz]);
					xyz=xyz+1;
					}
					psti=0;
					if((temp=lookup_in_table($1))!=-1)
                                          {      if(expn_type==-1)
                                                {
                                                        expn_type=temp;
							int pt1=lookup_depth($1);
							if(pt1==dimcheckr||pt1==pcheckr)
							{
							}
							else
							{printf("\nPointer depth mismatch\n");
							exit(0);
							}
                                                }else if(expn_type!=temp)
                                                {
                                                        printf("\ntype mismatch in the expression\n");
                                                        exit(0);
                                                }
                                        }else
                                        {
                                                printf("\n variable \"%s\" undeclared\n",$1);exit(0);
                                       }
                                        expn_type=-1;
                      
		      }
		      |PTR EQ  A_PTR SC
		      |INPUTCONS {}
		      |PRINTCONS {}
		      |OBJCR {}
		      |RETURNCONS
		      |FUNCCALL
		      |VAR EQ PTRA LB A_EXPN RB SC
		      { if((temp=lookup_in_table($1))!=-1)
  {      if(expn_type==-1)
        {
                expn_type=temp;
        }else if(expn_type!=temp)
        {
                printf("\ntype mismatch in the expression\n");
                exit(0);
        }
}else
{
        printf("\n variable \"%s\" undeclared\n",$1);exit(0);
 }
  expn_type=-1;
		      }
		      |VAR EQ A_MUL_PTR SC {
		      
		      int x=lookup_dim($1);
		      if(x==(dimcheckr-adj))
		      {printf("Successfully done special treatment");
		      }
		      else
		      {printf("Error in special treatment");
		      exit(0);
		      }
		      }
                      |VARARR EQ {printf("=");} A_EXPN SC 
		      {int xyz=1; while(xyz<psti){printf("%s",pst[xyz]); xyz=xyz+1;} psti=0; printf("\n");
		      if((temp=lookup_in_table($1))!=-1)
		      { if(expn_type==-1)
		      {
		      expn_type=temp ;
			int x=lookup_dim($1);
			if(x==dimcheckr||dimcheckr==0)
			{
			}
			else
			{printf("\nDimensional mismatch\n");
			exit(0);
			}
			}
		      else if(expn_type!=temp)
			{
		      printf("\ntype mistmatch in the expression\n");
		     exit(0);
		      }
		      }
		      else
		      {printf("\n variable \"%s\" undeclared\n",$1);
		      exit(0);
		      }
		      expn_type=-1;
		      }				      
			|A_EXPN SC {int xyz=0; while(xyz<psti){printf("%s",pst[xyz]); xyz=xyz+1;} psti=0;}
                        |VAR CONDTHEN {top=1;} EQ L_EXPNS CONDIF LCB {printf($1); printf("=(");int xyz=0; while(xyz<psti){printf("%s",pst[xyz]); xyz=xyz+1;} psti=0; printf(")?{");} PROGRAM_STATEMENT RCB CONDTHEN LCB {printf("}:{");} PROGRAM_STATEMENT RCB
			{ printf("}\n");
			top=0;
			}
L_EXPNS : L_EXPNS AND {printf("&&");} L_EXPNS
	| L_EXPNS OR {printf("||");} L_EXPNS
	| NOT {printf("!");} L_EXPNS
	|L_EXPN
	|LB L_EXPN RB 
L_EXPN : VAR LT VAR {if(top==1) {strcpy(pst[psti],$1);psti++; strcpy(pst[psti],"<"); psti++; strcpy(pst[psti],$3);  psti++;} else{printf($1); printf("<"); printf($3);}}
	| VAR LE VAR {if(top==1) {strcpy(pst[psti],$1); psti++;strcpy(pst[psti],"<="); psti++; strcpy(pst[psti],$3);   psti++;} else{printf($1); printf("<="); printf($3);}}
	| VAR GT VAR {if(top==1) {strcpy(pst[psti],$1); psti++;strcpy(pst[psti],">"); psti++; strcpy(pst[psti],$3);   psti++;} else{printf($1); printf(">"); printf($3);}}
	| VAR GE VAR  {if(top==1) {strcpy(pst[psti],$1); psti++;strcpy(pst[psti],">="); psti++; strcpy(pst[psti],$3);   psti++;} else{printf($1); printf(">="); printf($3);}}
	| VAR EQL VAR {if(top==1) {strcpy(pst[psti],$1); psti++;strcpy(pst[psti],"=="); psti++; strcpy(pst[psti],$3);   psti++;} else{printf($1); printf("=="); printf($3);}}
	| VAR NE VAR {if(top==1) {strcpy(pst[psti],$1); psti++;strcpy(pst[psti],"!="); psti++; strcpy(pst[psti],$3);   psti++;} else{printf($1); printf("!="); printf($3);}}
	|VAR LT VAL {if(top==1) {strcpy(pst[psti],$1); psti++;strcpy(pst[psti],"<"); psti++; strcpy(pst[psti],$3);   psti++;} else{printf($1); printf("<"); printf($3);}}
	| VAR LE VAL {if(top==1) {strcpy(pst[psti],$1); psti++;strcpy(pst[psti],"<="); psti++; strcpy(pst[psti],$3);   psti++;} else{printf($1); printf("<="); printf($3);}}
	| VAR GT VAL {if(top==1) {strcpy(pst[psti],$1); psti++;strcpy(pst[psti],">"); psti++; strcpy(pst[psti],$3);   psti++;} else{printf($1); printf(">"); printf($3);}}
	| VAR GE VAL {if(top==1) {strcpy(pst[psti],$1); psti++;strcpy(pst[psti],">="); psti++; strcpy(pst[psti],$3);  psti++;} else{printf($1); printf(">="); printf($3);}}
	|VAR EQL VAL {if(top==1) {strcpy(pst[psti],$1); psti++;strcpy(pst[psti],"=="); psti++; strcpy(pst[psti],$3);   psti++;} else{printf($1); printf("=="); printf($3);}}
	| VAR NE VAL {if(top==1) {strcpy(pst[psti],$1); psti++;strcpy(pst[psti],"!="); psti++; strcpy(pst[psti],$3);  psti++;} else{printf($1); printf("!="); printf($3);}}

A_MUL_PTR : MUL A_MUL_PTR {adj=adj+1;}|LB A_EXPN RB
A_PTR : PTR | PTR PLUS VAL | PTR PLUS VAR | PTR MINUS VAR | PTR MINUS VAL 
A_EXPN : A_EXPN UPLUS {strcpy(pst[psti],"++"); psti++; int xyz=0; while(xyz<psti){printf("%s",pst[xyz]); xyz=xyz+1;} psti=0;} 
			| A_EXPN UMINUS {strcpy(pst[psti],"--"); psti++; int xyz=0; while(xyz<psti){printf("%s",pst[xyz]); xyz=xyz+1;} psti=0;}
			| A_EXPN EXP A_EXPN {strcpy(pst[psti],"^"); psti++; strcpy(pst[psti],$3); $3=0;  psti++;}
			| A_EXPN PLUS A_EXPN {strcpy(pst[psti],"+"); psti++; strcpy(pst[psti],$3); $3=0; psti++;}
                	|A_EXPN MINUS A_EXPN {strcpy(pst[psti],"-"); psti++; strcpy(pst[psti],$3); $3=0;psti++;} 
			|A_EXPN MUL A_EXPN {strcpy(pst[psti],"*"); psti++; strcpy(pst[psti],$3); $3=0;psti++; } 
			|A_EXPN DIV A_EXPN {strcpy(pst[psti],"/"); psti++; strcpy(pst[psti],$3); $3=0; psti++;}
			| A_EXPN MOD A_EXPN {
			strcpy(pst[psti],"%"); psti++; strcpy(pst[psti],$3); $3=0; psti++;

                if(($1!=0) || ($3!=0))
                {printf("Invalid datatypes used");
                exit(0);
                }
                else
                {
                }
                }
               |LB A_EXPN RB {$$=$2;}
		| VAL {$$=$1;}
		|VARARR {
		strcpy(pst[psti],$1);
		psti++;
		$$=lookup_in_table($1);
      if((temp=lookup_in_table($1))!=-1)
      {
              if(expn_type==-1)
              {
                      expn_type=temp;
                      if(dimcheckr==-1)
                      {dimcheckr=lookup_dim($1);
                      }
                      else
                      { int dim1=lookup_dim($1);
                      if(dim1==0||dimcheckr==0||dim1==dimcheckr)
                      {if(dim1>dimcheckr)
		{dimcheckr=dim1;
		pcheckr=lookup_depth($1);
		}	
                      }
                      }
              }else if(expn_type!=temp)
             {
                      printf("\ntype mismatch in the expression\n");
                      exit(0);
              }
    } else
      {
              printf("\n variable \"%s\" undeclared\n",$1);exit(0);
      }
}
                | VAR { strcpy(pst[psti],$1);
			psti++;
			$$=lookup_in_table($1);
                        if((temp=lookup_in_table($1))!=-1)
                        {
                                if(expn_type==-1)
                                {
                                        expn_type=temp;
					if(dimcheckr==-1)
					{dimcheckr=lookup_dim($1);
					}
					else
					{ int dim1=lookup_dim($1);
					if(dim1==0||dimcheckr==0||dimcheckr==dim1)
					{if(dim1>dimcheckr)
					{
					dimcheckr=dim1;
					pcheckr=lookup_depth($1);
					}
					}
					}
					}
					else if(expn_type!=temp)
					{printf("\ntype mismatch in the experssion\n");
					exit(0);
					}
					}
					else
					{printf("\n variable \"%s\"undeclared\n",$1);
					exit(0);
					}
					}
%%
int lookup_in_table(char var[30])
{ if(var_count==-1)
{return -1;
}
           else
	   { for(int i=0; i<=var_count;i++)
	   { if(strcmp(var,var_list[i].var_name)==0)
	   {return var_list[i].type;
	   }
                        else
                                { continue;
                                }

                }
        return -1;
}
}

int lookup_dim(char var[30])
{if(var_count==-1)
{return -1;
}
else
{ for(int i=0;i<=var_count;i++)
{if(strcmp(var,var_list[i].var_name)==0)
{return var_list[i].dim;
}
else
{continue;
}
}
return -1;
}
}
int lookup_depth(char var[30])
{if(var_count==-1)
{return -1;
}
else
{ for(int i=0;i<=var_count;i++)
{ if(strcmp(var,var_list[i].var_name)==0)
{return var_list[i].depth;
}
else
{continue;
}
}
return -1;
}
}
void insert_to_table(char var[30], int type , int x, int y[5], int d) {
if(var_count==-1)
{var_count++;
strcpy(var_list[var_count].var_name,var);
var_list[var_count].type=type;
var_list[var_count].dim=x;
var_list[var_count].depth=d;
for(int i=0;i<=4;i++)
{var_list[var_count].dimb[i]=y[i];
}
}
else
{
for(int i=0;i<=var_count;i++)
        {
                                if(strcmp(var_list[i].var_name,var)==0)
                                        {printf("Multiple declaration");
                                        return;
                                        }

                        else
                                {break;
                               }
                        }
var_count++;
strcpy(var_list[var_count].var_name,var);
var_list[var_count].type=type;
var_list[var_count].dim=x;
var_list[var_count].depth=d;
for(int i=0;i<=4;i++)
{var_list[var_count].dimb[i]=y[i];
}
//printf("Variable name =%s and depth=%d",var_list[var_count].var_name,var_list[var_count].depth);
}
}
int main() {
    yyparse(); /* if(success)
        printf("Parsing Successful\n");*/
    return 0;
}
int yyerror(const char *msg) {
        extern int yylineno;
        printf("Parsing Failed\nLine Number: %d %s\n",yylineno,msg);
        success = 0;
        return 0;
}


