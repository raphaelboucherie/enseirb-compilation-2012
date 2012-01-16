%{
#include <stdio.h>
#include <stdlib.h> 
#include <string.h>
#include <math.h>
#include "table.h"
  extern int yylineno;
  int yylex ();
  int yyerror ();
  FILE* out;
  struct table *T;
  struct type * type;
  int tmp;
  int tmpmul;
  int tmpadd;
  int tmpaddsave;
  int tmpcomp;
  int digit_number(int );
  int compteurfor;
  int compteurif;
  int compteurwhile;
  
  %}

%token <str> IDENTIFIER CONSTANT
%token INC_OP DEC_OP LE_OP GE_OP EQ_OP NE_OP
%token SUB_ASSIGN MUL_ASSIGN ADD_ASSIGN
%token TYPE_NAME
%token INT FLOAT VOID
%token IF ELSE WHILE RETURN FOR
%union { 
  char * str;
  struct data {
    char * code;
    char * val;
    int appel; 
    struct type{
      enum _type {_INT,_FLOAT,_VOID,_FONCTION}t; //0:void,1:int,2:float,3:fonction
      int dimension;//0:primitif,>0: tableau
      int* dimensions;
      struct type * retour;//null sauf pour les fonctions
      int nb_parametres;
      struct type * parametres;//null sauf pour les fonctions 

    }*t;
    
    char * id;
  }data;
  struct decla {
    char * code;
    struct symbole{
      char * id;
      struct type *t;
      struct symbole *suivant;
    }*s;
  }d;
  
  
}
%type<data> primary_expression
%type<data> postfix_expression
%type<data> argument_expression_list
%type<data> unary_expression
%type<data> unary_operator
%type<data> multiplicative_expression
%type<data> additive_expression
%type<data> comparison_expression
%type<data> expression
%type<data> assignment_operator
%type<data> declaration
%type<data> declarator_list
%type<data> type_name
%type<data> declarator
%type<data> parameter_list
%type<data> parameter_declaration
%type<data> statement
%type<data> compound_statement
%type<data> declaration_list
%type<data> statement_list
%type<data> expression_statement
%type<data> selection_statement
%type<data> iteration_statement
%type<data> jump_statement
%type<data> program
%type<data> external_declaration
%type<data> function_definition

%start program
%%

primary_expression
: IDENTIFIER                                             
{
  $<data.code>$=malloc(1);
  *($<data.code>$)='\0';
  $<data.val>$=$1;
  struct type *t=cherche_symbole(T,$1);
  $<data.t>$=malloc(sizeof(struct type));
  memcpy($<data.t>$,t,sizeof(struct type));
  //$<data.id>$=$1;

}
| CONSTANT                                               
{
  $<data.code>$=malloc(1);
  *($<data.code>$)='\0';
  $<data.val>$=$1;
  $<data.t>$=malloc(sizeof(struct type));
  $<data.t>$->t=_INT;
  $<data.t>$->dimension=0;
  $<data.t>$->dimensions=NULL;
  $<data.t>$->retour=NULL;
  $<data.t>$->parametres=NULL;
  $<data.t>$->nb_parametres=0;
}
| '(' expression ')'                                      
{
  
  $$=$2;
}
| IDENTIFIER '(' ')'                                       
{
  $<data.appel=1>$;
  $<data.code>$=malloc(1);
  *($<data.code>$)='\0';
  $<data.val>$=malloc((1+strlen($1)+2));
  sprintf($<data.val>$,"%s()",$1);
  struct type* t= cherche_symbole(T,$1);
  if (NULL==t)
    fprintf(stderr,"fonction non déclarée %s",$1);
  else
    $<data.t>$=cherche_symbole(T,$1)->retour;
  //  $<data.id>$=$1;
}
| IDENTIFIER '(' argument_expression_list ')'              
{
  $<data.appel=1>$;
  $<data.code>$=$<data.code>3;
  $<data.val>$=malloc(1+strlen($1)+2+1+strlen($<data.val>3));
  sprintf($<data.val>$,"%s(%s)",$1,$<data.val>3); 
  struct type* t= cherche_symbole(T,$1);
  if (NULL==t)
    fprintf(stderr,"fonction non déclarée %s",$1);
  else
    $<data.t>$=cherche_symbole(T,$1)->retour;
  //$<data.id>$=$1;
}
| IDENTIFIER INC_OP                                     
{
  $<data.code>$=malloc(1);
  *($<data.code>$)='\0';
  $<data.val>$=malloc(1+strlen($1)+2);
  sprintf($<data.val>$,"%s++",$1);
  //$<data.id>$=$1; 
  struct type *t=cherche_symbole(T,$1);
  $<data.t>$=malloc(sizeof(struct type));
  memcpy($<data.t>$,t,sizeof(struct type));
  //$<data.t>$=cherche_symbole(T,$1);
  $<data.appel>$=1;
  
}
| IDENTIFIER DEC_OP                                        
{
  $<data.appel>$=1;
  $<data.code>$=malloc(1);
  *($<data.code>$)='\0';
  $<data.val>$=malloc(1+strlen($1)+2);  
  struct type *t=cherche_symbole(T,$1);
  $<data.t>$=malloc(sizeof(struct type));
  memcpy($<data.t>$,t,sizeof(struct type));
  sprintf($<data.val>$,"%s--",$1);
  //$<data.t>$=cherche_symbole(T,$1);
  //$<data.id>$=$1;
  
}
;

postfix_expression
: primary_expression
{
  $<data.code>$=$<data.code>1;
  $<data.val>$=$<data.val>1;
  $<data.t>$=$<data.t>1;
}
| postfix_expression '[' expression ']'        
{
  $<data.code>$=malloc(1+strlen($<data.code>1)+1+1+strlen($<data.code>3));
  sprintf($<data.code>$,"%s %s",$<data.code>1,$<data.code>3);
  $<data.val>$=malloc(1+strlen($<data.val>1)+1+strlen($<data.val>3)+2);
  sprintf($<data.val>$,"%s[%s]",$<data.val>1,$<data.val>3);
  $<data.t>$=$<data.t>1;
  $<data.t>$->dimension--;
  $<data.t>$->dimensions++;
  
  free($<data.code>1);
  free($<data.code>3);
}
;

argument_expression_list
: expression                                    
{
  $<data.code>$=$<data.code>1;
  $<data.val>$=malloc(1+strlen($<data.val>1));
  sprintf($<data.val>$,"%s",$<data.val>1);
}
| argument_expression_list ',' expression
{
  $<data.code>$=malloc(1+strlen($<data.code>1)+1+1+strlen($<data.code>3));
  sprintf($<data.code>$,"%s %s",$<data.code>1,$<data.code>3);
  $<data.val>$=malloc(1+strlen($<data.val>1)+1+strlen($<data.val>3));
  sprintf($<data.val>$,"%s,%s",$<data.val>1,$<data.val>3);
  free($<data.code>1);
  free($<data.code>3);
}
;

unary_expression
: postfix_expression                             

{
  $$=$1;
}
| INC_OP unary_expression 
{
  $<data.appel>$=1;
  $<data.code>$=$<data.code>2;
  $<data.val>$=malloc(1+strlen($<data.val>2)+2);
  $<data.id>$=$<data.id>2;
  sprintf($<data.val>$,"++%s",$<data.val>2);
  $<data.t>$=$<data.t>2;
}
| DEC_OP unary_expression                        

{
  $<data.t>$=$<data.t>2;
  $<data.appel>$=1;
  $<data.id>$=$<data.id>2;
  $<data.code>$=$<data.code>2;
  $<data.val>$=malloc(1+strlen($<data.val>2)+2);
  sprintf($<data.val>$,"--%s",$<data.val>2);

}
| unary_operator unary_expression                 

{
  $<data.t>$=$<data.t>2;
  $<data.id>$=$<data.id>2;
  $<data.code>$=$<data.code>2;
  $<data.val>$=malloc(1+strlen($<data.val>2)+1+strlen($<data.code>1));
  sprintf($<data.val>$,"%s%s",$<data.code>1,$<data.val>2);
 
}



;

unary_operator
: '*'                            
{$<data.code>$=malloc(2);
  sprintf($<data.code>$,"*");
}
| '+'  
{
  $<data.code>$=malloc(2);
  sprintf($<data.code>$,"+");
}
| '-'
{
  $<data.code>$=malloc(2);
  sprintf($<data.code>$,"-");
}
;

multiplicative_expression
: unary_expression                                            
  
{ 
  fprintf(stderr,"**mul->unary**\n");
  if ($<data.appel>1==1)
    {
      $$=$1;
    }
  else
    {
      
      $<data.val>$=malloc(8+digit_number(tmpmul));
      sprintf($<data.val>$,"tmpmul%d",tmpmul);
      $<data.code>$=malloc(1+strlen($<data.code>1)+12+digit_number(tmpmul)+strlen($<data.val>1)+1);
      sprintf($<data.code>$,"%stmpmul%d=%s;\n",$<data.code>1,tmpmul,$<data.val>1);
      ajout_symbole(T,$<data.val>$,$<data.t>1);
      $<data.t>$=$<data.t>1;
      tmpmul++;free($<data.code>1);
    }
}


| multiplicative_expression '*' unary_expression  

{
  int k=verif_type_operation($<data.t>1,$<data.t>3,'*');
  if(k==0)
    {
     yyerror("mul->mul*unary opérande incompatible");
      exit(EXIT_FAILURE);
    }
  
  fprintf(stderr,"**mul->mul*unary**\n"); 
  
      if(k==1)
	{
	  if($<data.appel>1==1)
	    {
	      $<data.val>$=malloc(8+digit_number(tmpmul));
	      sprintf($<data.val>$,"tmpmul%d",tmpmul);
	      $<data.code>$=malloc(1+strlen($<data.code>1)+23+1+strlen($<data.val>$)+strlen($<data.val>1)+1+strlen($<data.code>3)+1+digit_number(tmp)+strlen($<data.val>3)+1);
	      sprintf($<data.code>$,"%s%s%s=%s;tmp%d=%s;%s*=tmp%d;\n",$<data.code>1,$<data.code>3,$<data.val>$,$<data.val>1,tmp,$<data.val>3,$<data.val>$,tmp);
	      ajout_symbole(T,$<data.val>$,$<data.t>3);
	      char *c;
	      c=malloc(3+digit_number(tmp));
	      sprintf(c,"tmp%d",tmp);
	      ajout_symbole(T,c,$<data.t>3);
	      tmp++;tmpmul++;
	    }
	  else
	    {
	      $<data.val>$=malloc(1+strlen($<data.val>1));
	      sprintf($<data.val>$,"%s",$<data.val>1);
	      $<data.code>$=malloc(13+1+strlen($<data.code>1)+1+strlen($<data.code>3)+2*digit_number(tmp)+1+strlen($<data.val>3)+1+strlen($<data.val>1));
	      sprintf($<data.code>$,"%s%stmp%d=%s;%s*=tmp%d;\n",$<data.code>1,$<data.code>3,tmp,$<data.val>3,$<data.val>1,tmp);
	      char *c;
	      c=malloc(4+digit_number(tmp));
	      sprintf(c,"tmp%d",tmp);
	      ajout_symbole(T,c,$<data.t>3);
	      tmp++;free($<data.code>1);free($<data.code>3);
	    }
	  $<data.t>$=$<data.t>1;
	}
      else
	{
	      $<data.val>$=malloc(5+digit_number(tmp)); 
	      sprintf($<data.val>$,"tmp%d",tmp);
	      $<data.code>$=malloc(1+strlen($<data.code>1)+6+2*(1+strlen($<data.val>$))+strlen($<data.val>1)+1+strlen($<data.code>3)+strlen($<data.val>3)+1);
	      sprintf($<data.code>$,"%s%s%s=%s;%s*=%s;\n",$<data.code>1,$<data.code>3,$<data.val>$,$<data.val>3,$<data.val>$,$<data.val>1);
	      ajout_symbole(T,$<data.val>$,$<data.t>3);
	      tmp++;
	      $<data.t>$=$<data.t>3;
	}
}


| multiplicative_expression '|' unary_expression                  

{
  int k=verif_type_prod_scalaire($<data.t>1,$<data.t>3);
  if(k==0)
    {
     yyerror("opérande incompatible");
      exit(EXIT_FAILURE);
    }
  
  $<data.val>$=malloc(7+digit_number(tmpmul));
  sprintf($<data.val>$,"tmpmul%d",tmpmul);
  $<data.code>$=malloc(46+strlen($<data.code>1)+strlen($<data.code>3)+3*digit_number(tmp)+2*digit_number(tmpmul));
  sprintf($<data.code>$,"%s%stmp%d=%s;tmp%d*=%s;tmpmul%d=0;tmpmul%d+=tmp%d;\n",$<data.code>1,$<data.code>3,tmp,$<data.val>1,tmp,$<data.val>3,tmpmul,tmpmul,tmp);
  char *c;
  c=malloc(3+digit_number(tmp));
  sprintf(c,"tmp%d",tmp);
  ajout_symbole(T,c,$<data.t>1);
  c=realloc(c,6+digit_number(tmpmul));
  sprintf(c,"tmpmul%d",tmpmul);
  struct type *t=malloc(sizeof(*t));
  t->t=_FLOAT;
  t->dimension=0;
  t->dimensions=NULL;
  t->retour=NULL;
  t->nb_parametres=0;
  t->parametres=NULL;
  ajout_symbole(T,c,t);
  $<data.t>$=t;
  tmpmul++;tmp++;
  free($<data.code>1);free($<data.code>3);
}
;

additive_expression
: multiplicative_expression

{
  fprintf(stderr,"**add->mul**\n");
  $$=$1;/*malloc(strlen($<data.code>1)+16+digit_number(tmpadd)+digit_number(tmpmul-1));
	  sprintf($<data.code>$,"%s\ntmpadd%d=tmpmul%d;",$<data.code>1,tmpadd,tmpmul-1);
	  tmpaddsave=tmpadd;
	  tmpadd++;
	  free($<data.code>1);*/
}
| additive_expression '+' multiplicative_expression              
{ 
  int k=verif_type_operation($<data.t>1,$<data.t>3,'+');
  if(k==0)
    {
     yyerror("add->add+mul opérande incompatible");
      exit(EXIT_FAILURE);
    }
  
  if(k==1)
    {
      if($<data.appel>1==1)
	{
	  $<data.val>$=malloc(8+digit_number(tmpmul));
	  sprintf($<data.val>$,"tmpmul%d",tmpmul);
	  $<data.code>$=malloc(1+strlen($<data.code>1)+23+1+strlen($<data.val>$)+strlen($<data.val>1)+1+strlen($<data.code>3)+1+digit_number(tmp)+strlen($<data.val>3)+1);
	  sprintf($<data.code>$,"%s%s%s=%s;tmp%d=%s;%s+=tmp%d;\n",$<data.code>1,$<data.code>3,$<data.val>$,$<data.val>1,tmp,$<data.val>3,$<data.val>$,tmp);
	  ajout_symbole(T,$<data.val>$,$<data.t>3);
	  char *c;
	  c=malloc(4+digit_number(tmp));
	  sprintf(c,"tmp%d",tmp);
	  ajout_symbole(T,c,$<data.t>3);
	  tmp++;tmpmul++;
	}
      else
	{
	  fprintf(stderr,"**add->add+mul**\n");
	  $<data.val>$=$<data.val>1;/*malloc(8+digit_number(tmpmul));
				      sprintf($<data.val>$,"tmpmul%d",tmpmul);*/
	  $<data.code>$=malloc(strlen($<data.code>1)+1+strlen($<data.code>3)+1+5+strlen($<data.val>1)+1+strlen($<data.val>3));
	  sprintf($<data.code>$,"%s%s %s+=%s;\n",$<data.code>3,$<data.code>1,$<data.val>1,$<data.val>3);
	  free($<data.code>1);free($<data.code>3);
	}
    }
  else
    {
      $<data.val>$=malloc(5+digit_number(tmp)); 
      sprintf($<data.val>$,"tmp%d",tmp);
      $<data.code>$=malloc(1+strlen($<data.code>1)+6+2*(1+strlen($<data.val>$))+strlen($<data.val>1)+1+strlen($<data.code>3)+strlen($<data.val>3)+1);
      sprintf($<data.code>$,"%s%s%s=%s;%s+=%s;\n",$<data.code>1,$<data.code>3,$<data.val>$,$<data.val>3,$<data.val>$,$<data.val>1);
      ajout_symbole(T,$<data.val>$,$<data.t>3);
      tmp++;
      $<data.t>$=$<data.t>3;
    }
}
| additive_expression '-' multiplicative_expression                
{
  int k=verif_type_operation($<data.t>1,$<data.t>3,'-');
  if(k==0)
    {
     yyerror("add->add-mul opérande incompatible");
      exit(EXIT_FAILURE);
    }
  
  if(k==1)
    {
      if($<data.appel>1==1)
	{
	  $<data.val>$=malloc(8+digit_number(tmpmul));
	  sprintf($<data.val>$,"tmpmul%d",tmpmul);
	  $<data.code>$=malloc(1+strlen($<data.code>1)+23+1+strlen($<data.val>$)+strlen($<data.val>1)+1+strlen($<data.code>3)+1+digit_number(tmp)+strlen($<data.val>3)+1);
	  sprintf($<data.code>$,"%s%s%s=%s;tmp%d=%s;%s-=tmp%d;\n",$<data.code>1,$<data.code>3,$<data.val>$,$<data.val>1,tmp,$<data.val>3,$<data.val>$,tmp);
	  ajout_symbole(T,$<data.val>$,$<data.t>3);
	  char *c;
	  c=malloc(3+digit_number(tmp));
	  sprintf(c,"tmp%d",tmp);
	  ajout_symbole(T,c,$<data.t>3);
	  tmp++;tmpmul++;
	}
      else
	{
	  fprintf(stderr,"**add->add-mul**\n");
	  $<data.val>$=$<data.val>1;/*malloc(8+digit_number(tmpmul));
				      sprintf($<data.val>$,"tmpmul%d",tmpmul);*/
	  $<data.code>$=malloc(strlen($<data.code>1)+1+strlen($<data.code>3)+1+9+digit_number(tmpmul-1)+digit_number(tmpadd-1));
	  sprintf($<data.code>$,"%s%s %s-=%s;\n",$<data.code>3,$<data.code>1,$<data.val>1,$<data.val>3);
	  free($<data.code>1);free($<data.code>3);
	}
    }
  else
    {
     $<data.val>$=malloc(5+digit_number(tmp)); 
      sprintf($<data.val>$,"tmp%d",tmp);
      $<data.code>$=malloc(1+strlen($<data.code>1)+6+2*(1+strlen($<data.val>$))+strlen($<data.val>1)+1+strlen($<data.code>3)+strlen($<data.val>3)+1);
      sprintf($<data.code>$,"%s%s%s=%s;%s-=%s;\n",$<data.code>1,$<data.code>3,$<data.val>$,$<data.val>3,$<data.val>$,$<data.val>1);
      ajout_symbole(T,$<data.val>$,$<data.t>3);
      tmp++;
      $<data.t>$=$<data.t>3; 
    }
}
;

comparison_expression
: additive_expression 
{
  fprintf(stderr,"**comp ->add**\n");
 

  $$=$1;/*malloc(strlen($<data.code>1)+1+17+digit_number(tmpadd-1)+digit_number(tmpcomp-1));
	  sprintf($<data.code>$,"%s\ntmpcomp%d=tmpadd%d;\n",$<data.code>1,tmpcomp,tmpadd-1);
	  tmpcomp++;
	  free($<data.code>1);
	*/
}
| additive_expression '<' additive_expression
{
  if(verif_type_comp($<data.t>1,$<data.t>3))
    {
     yyerror("add<add opérande incompatible");
      exit(EXIT_FAILURE);
    }
  
  fprintf(stderr,"**comp ->add<add**\n");
  $<data.val>$=malloc(8+digit_number(tmpcomp));
  sprintf($<data.val>$,"tmpcomp%d",tmpcomp);
  $<data.code>$=malloc(1+strlen($<data.code>1)+1+strlen($<data.code>3)+30+1+strlen($<data.val>1)+1+strlen($<data.val>3)+2*(1+strlen($<data.val>$)));
  sprintf($<data.code>$,"%s%s%s=0;\nif (%s<%s) %s=1;\n",$<data.code>1,$<data.code>3,$<data.val>$,$<data.val>1,$<data.val>3,$<data.val>$);
  
  char *c;
  c=malloc(7+digit_number(tmpcomp));
  sprintf(c,"tmpcomp%d",tmpcomp);
  struct type *t=malloc(sizeof(*t));
  t->t=_INT;
  t->dimension=0;
  t->dimensions=NULL;
  t->retour=NULL;
  t->nb_parametres=0;
  t->parametres=NULL;
  ajout_symbole(T,c,t);
  
  $<data.t>$=t;
  
  tmpcomp++;
  free($<data.code>1);
  free($<data.code>3);
}

| additive_expression '>' additive_expression           
{ 
  if(verif_type_comp($<data.t>1,$<data.t>3))
    {
     yyerror("opérande incompatible");
      exit(EXIT_FAILURE);
    }
  fprintf(stderr,"**comp ->add<add**\n");
  $<data.val>$=malloc(8+digit_number(tmpcomp));
  sprintf($<data.val>$,"tmpcomp%d",tmpcomp);
  $<data.code>$=malloc(1+strlen($<data.code>1)+1+strlen($<data.code>3)+30+1+strlen($<data.val>1)+1+strlen($<data.val>3)+2*(1+strlen($<data.val>$)));
  sprintf($<data.code>$,"%s%s%s=0;\nif (%s>%s) %s=1;\n",$<data.code>1,$<data.code>3,$<data.val>$,$<data.val>1,$<data.val>3,$<data.val>$);
  
  char *c;
  c=malloc(7+digit_number(tmpcomp));
  sprintf(c,"tmpcomp%d",tmpcomp);
  struct type *t=malloc(sizeof(*t));
  t->t=_INT;
  t->dimension=0;
  t->dimensions=NULL;
  t->retour=NULL;
  t->nb_parametres=0;
  t->parametres=NULL;
  ajout_symbole(T,c,t);
  
   $<data.t>$=t;
  
  tmpcomp++;
  free($<data.code>1);
  free($<data.code>3);
  fprintf(stderr,"**comp ->add>add**\n");/*
					   $<data.code>$=malloc(1+strlen($<data.code>1)+1+strlen($<data.code>3)+42+digit_number(tmpaddsave-1)+digit_number(tmpadd)+2*digit_number(tmpcomp));
					   sprintf($<data.code>$,"%s%stmpcomp%d=0;\nif (tmpadd%d>tmpadd%d) tmpcomp%d=1;\n",$<data.code>1,$<data.code>3,tmpcomp,tmpaddsave-1,tmpadd-1,tmpcomp);
					   free($<data.code>1);free($<data.code>3);tmpcomp++;*/
}
| additive_expression LE_OP additive_expression     
{
   if(verif_type_comp($<data.t>1,$<data.t>3))
    {
     yyerror("opérande incompatible");
      exit(EXIT_FAILURE);
    }
   
  $<data.val>$=malloc(8+digit_number(tmpcomp));
  sprintf($<data.val>$,"tmpcomp%d",tmpcomp);
  $<data.code>$=malloc(1+1+strlen($<data.code>1)+1+strlen($<data.code>3)+30+1+strlen($<data.val>1)+1+strlen($<data.val>3)+2*(1+strlen($<data.val>$)));
  sprintf($<data.code>$,"%s%s%s=0;\nif (%s<=%s) %s=1;\n",$<data.code>1,$<data.code>3,$<data.val>$,$<data.val>1,$<data.val>3,$<data.val>$);
  
  char *c;
  c=malloc(7+digit_number(tmpcomp));
  sprintf(c,"tmpcomp%d",tmpcomp);
  struct type *t=malloc(sizeof(*t));
  t->t=_INT;
  t->dimension=0;
  t->dimensions=NULL;
  t->retour=NULL;
  t->nb_parametres=0;
  t->parametres=NULL;
  ajout_symbole(T,c,t);
  
  
   $<data.t>$=t;
  tmpcomp++;
  free($<data.code>1);
  free($<data.code>3);

		       
  fprintf(stderr,"**comp ->add<=add**\n");





  /*
    $<data.code>$=malloc(1+strlen($<data.code>1)+1+strlen($<data.code>3)+42+digit_number(tmpaddsave-1)+digit_number(tmpadd)+2*digit_number(tmpcomp));
    sprintf($<data.code>$,"%s%stmpcomp%d=0;\nif (tmpadd%d<=tmpadd%d) tmpcomp%d=1;\n",$<data.code>1,$<data.code>3,tmpcomp,tmpaddsave-1,tmpadd-1,tmpcomp);
    free($<data.code>1);free($<data.code>3);tmpcomp++;*/
}

| additive_expression GE_OP additive_expression 
{
  if(verif_type_comp($<data.t>1,$<data.t>3))
    {
     yyerror("opérande incompatible");
      exit(EXIT_FAILURE);
    }

  $<data.val>$=malloc(8+digit_number(tmpcomp));
  sprintf($<data.val>$,"tmpcomp%d",tmpcomp);
  $<data.code>$=malloc(1+1+strlen($<data.code>1)+1+strlen($<data.code>3)+30+1+strlen($<data.val>1)+1+strlen($<data.val>3)+2*(1+strlen($<data.val>$)));
  sprintf($<data.code>$,"%s%s%s=0;\nif (%s>=%s) %s=1;\n",$<data.code>1,$<data.code>3,$<data.val>$,$<data.val>1,$<data.val>3,$<data.val>$);
  
  char *c;
  c=malloc(7+digit_number(tmpcomp));
  sprintf(c,"tmpcomp%d",tmpcomp);
  struct type *t=malloc(sizeof(*t));
  t->t=_INT;
  t->dimension=0;
  t->dimensions=NULL;
  t->retour=NULL;
  t->nb_parametres=0;
  t->parametres=NULL;
  ajout_symbole(T,c,t);
  
   $<data.t>$=t;
  
  tmpcomp++;
  free($<data.code>1);
  free($<data.code>3);

  fprintf(stderr,"**comp ->add>=add**\n");

 

  /*
    $<data.code>$=malloc(1+strlen($<data.code>1)+1+strlen($<data.code>3)+42+digit_number(tmpaddsave-1)+digit_number(tmpadd)+2*digit_number(tmpcomp));
    sprintf($<data.code>$,"%s%stmpcomp%d=0;\nif (tmpadd%d>=tmpadd%d) tmpcomp%d=1;\n",$<data.code>1,$<data.code>3,tmpcomp,tmpaddsave-1,tmpadd-1,tmpcomp);
    free($<data.code>1);free($<data.code>3);tmpcomp++;*/
}
| additive_expression EQ_OP additive_expression            
{
 if(verif_type_comp($<data.t>1,$<data.t>3))
    {
     yyerror("opérande incompatible");
      exit(EXIT_FAILURE);
    }
 
  $<data.val>$=malloc(8+digit_number(tmpcomp));
  sprintf($<data.val>$,"tmpcomp%d",tmpcomp);
  $<data.code>$=malloc(1+1+strlen($<data.code>1)+1+strlen($<data.code>3)+30+1+strlen($<data.val>1)+1+strlen($<data.val>3)+2*(1+strlen($<data.val>$)));
  sprintf($<data.code>$,"%s%s%s=0;\nif (%s==%s) %s=1;\n",$<data.code>1,$<data.code>3,$<data.val>$,$<data.val>1,$<data.val>3,$<data.val>$);
  
  char *c;
  c=malloc(7+digit_number(tmpcomp));
  sprintf(c,"tmpcomp%d",tmpcomp);
  struct type *t=malloc(sizeof(*t));
  t->t=_INT;
  t->dimension=0;
  t->dimensions=NULL;
  t->retour=NULL;
  t->nb_parametres=0;
  t->parametres=NULL;
  ajout_symbole(T,c,t);
		       
   $<data.t>$=t;
  
  tmpcomp++;
  free($<data.code>1);
  free($<data.code>3);
  fprintf(stderr,"**comp ->add==add**\n");
  
  

  /*
    $<data.code>$=malloc(1+strlen($<data.code>1)+1+strlen($<data.code>3)+42+digit_number(tmpaddsave-1)+digit_number(tmpadd)+2*digit_number(tmpcomp));
    sprintf($<data.code>$,"%s%stmpcomp%d=0;\nif (tmpadd%d==tmpadd%d) tmpcomp%d=1;\n",$<data.code>1,$<data.code>3,tmpcomp,tmpaddsave-1,tmpadd-1,tmpcomp);
    free($<data.code>1);free($<data.code>3);tmpcomp++;*/
}
| additive_expression NE_OP additive_expression             
{
   if(verif_type_comp($<data.t>1,$<data.t>3))
    {
      yyerror("opérande incompatible");
      exit(EXIT_FAILURE);
    }
   
  fprintf(stderr,"**comp ->add!=add**\n");

  $<data.val>$=malloc(8+digit_number(tmpcomp));
  sprintf($<data.val>$,"tmpcomp%d",tmpcomp);
  $<data.code>$=malloc(1+1+strlen($<data.code>1)+1+strlen($<data.code>3)+30+1+strlen($<data.val>1)+1+strlen($<data.val>3)+2*(1+strlen($<data.val>$)));
  sprintf($<data.code>$,"%s%s%s=0;\nif (%s!=%s) %s=1;\n",$<data.code>1,$<data.code>3,$<data.val>$,$<data.val>1,$<data.val>3,$<data.val>$);
  
  char *c;
  c=malloc(7+digit_number(tmpcomp));
  sprintf(c,"tmpcomp%d",tmpcomp);
  struct type *t=malloc(sizeof(*t));
  t->t=_INT;
  t->dimension=0;
  t->dimensions=NULL;
  t->retour=NULL;
  t->nb_parametres=0;
  t->parametres=NULL;
  ajout_symbole(T,c,t);
		      

 $<data.t>$=t;
  tmpcomp++;
  free($<data.code>1);
  free($<data.code>3);
  /*
    $<data.code>$=malloc(1+strlen($<data.code>1)+1+strlen($<data.code>3)+42+digit_number(tmpaddsave-1)+digit_number(tmpadd)+2*digit_number(tmpcomp));
    sprintf($<data.code>$,"%s%stmpcomp%d=0;\nif (tmpadd%d!=tmpadd%d) tmpcomp%d=1;\n",$<data.code>1,$<data.code>3,tmpcomp,tmpaddsave-1,tmpadd-1,tmpcomp);
    free($<data.code>1);free($<data.code>3);tmpcomp++;*/
}
;

expression
: unary_expression assignment_operator comparison_expression            
{
  if(!verif_type_affect(
			$<data.t>1,
			$<data.t>3,
			$<data.code>2))
    {
     yyerror("affect opérande incompatible");
      exit(EXIT_FAILURE);
    }
  
  fprintf(stderr,"expression assignment\n");
  $<data.code>$=malloc(15+strlen($<data.code>1)+strlen($<data.code>2)+strlen($<data.code>3)+1+strlen($<data.val>3)+1+strlen($<data.val>1));
  sprintf($<data.code>$,/*"int tmp%d;\n*/"%s%s%s%s%s;",/*tmp2,*/$<data.code>3,$<data.code>1,$<data.val>1,$<data.code>2,$<data.val>3);
  free($<data.code>1);
  free($<data.code>3);
  free($<data.code>2);
  fprintf(stderr,"%s\n",$<data.code>$);
  $<data.val>$=malloc(2);
  sprintf($<data.val>$,"1");
}

| comparison_expression                                             
{$$=$1;
  /*
    $<data.code>$=$<data.code>1;
    $<data.val>$=malloc(8+digit_number(tmpcomp-1));
    sprintf($<data.val>$,"tmpcomp%d",tmpcomp-1);
  */
}
;

assignment_operator
: '='
{
  $<data.code>$=malloc(2);
  sprintf($<data.code>$,"=");
}
| MUL_ASSIGN 
{
  $<data.code>$=malloc(3);
  sprintf($<data.code>$,"*=");
}
| ADD_ASSIGN
{
  $<data.code>$=malloc(3);
  sprintf($<data.code>$,"+=");
}
| SUB_ASSIGN
{
  $<data.code>$=malloc(3);
  sprintf($<data.code>$,"-=");
}
;

declaration
: type_name declarator_list ';'
{
  $<data.code>$=malloc(1+strlen($<data.code>1)+2+1+strlen($<d.code>2));
  sprintf($<data.code>$,"%s %s;\n",$<data.code>1,$<d.code>2);
  struct symbole *s=$<d.s>2;
  enum _type t;
    
  switch (*$<data.code>1)
    {
    case('i'):
      t=_INT;
      break;
    case('f'):
      t=_FLOAT;
      break;
    default:
      fprintf(stderr,"déclaration de variable void%s%s",$<data.code>1,$<data.code>2);
    }
  while(s!=NULL)
    {
      s->t->t=t;
      ajout_symbole(T,s->id,s->t);
      s=s->suivant;
    }
  free($<data.code>1);
  //  free($<data.code>2);
}
;

declarator_list
: declarator                            
{
  $<d.code>$=$<data.code>1;
  $<d.s>$=malloc(sizeof(struct symbole));
  $<d.s>$->id=$<data.id>1;
  $<d.s>$->t=$<data.t>1;
  $<d.s>$->suivant=NULL;
  
}
| declarator_list ',' declarator
{ 
  $<d.s>$=malloc(sizeof(struct symbole));
  $<d.s>$->id=$<data.id>3;
  $<d.s>$->t=$<data.t>3;
  
  $<d.s>$->suivant=$<d.s>1;
  $<d.code>$=malloc(1+strlen($<d.code>1)+1+1+strlen($<data.code>3));
  sprintf($<d.code>$,"%s,%s",$<d.code>1,$<data.code>3);

  //free($<data.code>1);
  //free($<data.code>3);
}
;

type_name
: VOID
{
  $<data.code>$=malloc(5);
  sprintf($<data.code>$,"void");
}
| INT
{
  $<data.code>$=malloc(4);
  sprintf($<data.code>$,"int");
}
| FLOAT
{
  $<data.code>$=malloc(6);
  sprintf($<data.code>$,"float");
}
;

declarator
: IDENTIFIER
{
  $<data.code>$=$1;
  $<data.id>$=$1;
  $<data.t>$=malloc(sizeof(struct type));
  $<data.t>$->dimension=0;
  $<data.t>$->dimensions=NULL;
  $<data.t>$->retour=NULL;
  $<data.t>$->parametres=NULL;
  $<data.t>$->nb_parametres=0;  
}
| '(' declarator ')'
{
  $<data.code>$=malloc(1+strlen($<data.code>2)+2);
  sprintf($<data.code>$,"(%s)",$<data.code>2);
  free($<data.code>2);
}
| declarator '[' CONSTANT ']'                  
{
  $<data.t>$=$<data.t>1;
  $<data.t>$->dimension++;
  fprintf(stderr,"------------------------------------------------------------------\n");
  $<data.t>$->dimensions=realloc($<data.t>$->dimensions,sizeof(int)*$<data.t>$->dimension);
  $<data.t>$->dimensions[$<data.t>$->dimension-1]=atoi($3);
  $<data.code>$=malloc(1+strlen($<data.code>1)+2+1+strlen($3));
  sprintf($<data.code>$,"%s[%s]",$<data.code>1,$3);
  //free($<data.code>1);
  free($3);
}
| declarator '[' ']'
{
  $<data.code>$=malloc(1+strlen($<data.code>1)+2);
  sprintf($<data.code>$,"%s[]",$<data.code>1);
  free($<data.code>1);
}
| declarator '(' parameter_list ')'
{
  $<data.t>$=$<data.t>1;
  $<data>$.id=$<data>1.id;
  $<data.t>$->t=_FONCTION;
  $<data.t>$->nb_parametres=$<data.t>3->nb_parametres;
  $<data.t>$->parametres=$<data.t>3->parametres;
  $<data.code>$=malloc(1+strlen($<data.code>1)+2+1+strlen($<data.code>3));
  sprintf($<data.code>$,"%s(%s)",$<data.code>1,$<data.code>3);//free($<data.code>1);free($<data.code>3);
}
| declarator '(' ')'          
{  
  $<data.t>$=$<data.t>1;
  $<data.t>$->t=_FONCTION;
  $<data.code>$=malloc(1+strlen($<data.code>1)+2);
  sprintf($<data.code>$,"%s()",$<data.code>1);
  free($<data.code>1);
}
;

parameter_list
: parameter_declaration
{
  $<data.t>$=malloc(sizeof(struct type));
  $<data.t>$->nb_parametres=1;
  $<data.t>$->parametres=$<data.t>1;  
  $<data.code>$=$<data.code>1;
}
| parameter_list ',' parameter_declaration
{
  $<data.code>$=malloc(1+strlen($<data.code>1)+1+1+strlen($<data.code>3));
  sprintf($<data.code>$,"%s,%s",$<data.code>1,$<data.code>3);
 
  fprintf(stderr,"+++++++++++++++++%p++++++++++++%p+++++++++++++\n",$<data.t>$->parametres,$<data.t>1->parametres);
  int k=$<data.t>1->nb_parametres+1;
  //$<data.t>$->parametres=realloc( $<data.t>$->parametres,sizeof(struct type)*k);
  struct type *t=malloc(sizeof(struct type)*k);
  memcpy(t,$<data.t>1->parametres,sizeof(struct type)*(k-1));
  //$<data.t>$->parametres=malloc(sizeof(struct type)*k); 
  t[k-1]=*($<data.t>3);
  $<data.t>$->parametres=t;
  $<data.t>$->nb_parametres=k;
  //fprintf(stderr,"!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!%p!!!!!!!!!!!!!\n",$<data.t>$->parametres);
  //memcpy($<data.t>$->parametres,$<data.t>1->parametres,sizeof(struct type)*(k-1));
  //$<data.t>$->parametres[k-1]=*($<data.t>3);
  //free($<data.code>3);
  //free($<data.code>1);
}
;

parameter_declaration
: type_name declarator
{
  
 
  
  $<data.code>$=malloc(1+strlen($<data.code>1)+2+1+strlen($<data.code>2));
  sprintf($<data.code>$,"%s %s",$<data.code>1,$<data.code>2);
  $<data.t>$=$<data.t>2;
  switch (*$<data.code>1)
    {
    case ('f'):
      $<data.t>2->t=_FLOAT;
      break;
    case ('i'):
      $<data.t>2->t=_INT;
      break;
    default:
      fprintf(stderr,"déclaration paramètres éronée%s",$<data.code>$);
    }
  fprintf(stderr,"id=%s\ntype=%d;nb_param=%d",$<data.id>2,$<data.t>2->t,$<data.t>2->nb_parametres); 
  ajout_symbole(T,$<data.id>2,$<data.t>2);
  free($<data.code>1);
  //free($<data.code>2);
}
;

statement
: compound_statement
{
  $<data.code>$=$<data.code>1;
}
| expression_statement
{
  //$<data.code>$=$<data.code>1;
  if ($<data.appel>$==1)
    {
      $<data.code>$=malloc(1+strlen($<data.code>1)+1+strlen($<data.val>1));
      sprintf($<data.code>$,"%s%s;",$<data.code>1,$<data.val>1);
    }
  else
    $<data.code>$=$<data.code>1;  
}
| selection_statement
{
  $<data.code>$=$<data.code>1;
}
| iteration_statement
{
  $<data.code>$=$<data.code>1;
}
| jump_statement
{
  $<data.code>$=$<data.code>1;
}
;

compound_statement
: '{' '}'                                              
{
  $<data.code>$=malloc(2);
  sprintf($<data.code>$,"{}");
}
| '{' statement_list '}'                               
{
  $<data.code>$=malloc(1+strlen($<data.code>2)+2);
  sprintf($<data.code>$,"{%s}",$<data.code>2);
  free($<data.code>2);
}
| '{' declaration_list statement_list '}'              
{
  $<data.code>$=malloc(1+strlen($<data.code>2)+3+1+strlen($<data.code>3));
  sprintf($<data.code>$,"{%s %s}",$<data.code>2,$<data.code>3);
  free($<data.code>2);
  free($<data.code>3);
}
;

declaration_list
: declaration                            
{

  $<data.code>$=$<data.code>1;
}
| declaration_list declaration           
{


  $<data.code>$=malloc(1+strlen($<data.code>1)+1+1+strlen($<data.code>2));
  sprintf($<data.code>$,"%s %s",$<data.code>1,$<data.code>2);
  free($<data.code>2);
  free($<data.code>1);
}
;

statement_list
: statement
{
  $<data.code>$=$<data.code>1;
}
| statement_list statement
{
  $<data.code>$=malloc(1+strlen($<data.code>1)+1+1+strlen($<data.code>2));
  sprintf($<data.code>$,"%s %s",$<data.code>1,$<data.code>2);
  free($<data.code>2);
  free($<data.code>1);
}
;

expression_statement
: ';'   
{
  fprintf(stderr,"**exp_stmt->;**\n");
  $<data.code>$=malloc(3);
  sprintf($<data.code>$,";\n");
}
| expression ';'       

{
  $<data.val>$=$<data.val>1;
  $<data.appel>$=$<data.appel>1;
  fprintf(stderr,"**exp_stmt->exp;**\n");
  $<data.code>$=malloc(2+strlen($<data.code>1)+1);
  sprintf($<data.code>$,"%s\n",$<data.code>1);
  free($<data.code>1);
  fprintf(stderr,"%s\n",$<data.code>$);
}
;

selection_statement
: IF '(' expression ')' statement
{
  $<data.code>$=malloc(14+1+strlen($<data.code>3)+1+strlen($<data.code>5)+strlen($<data.val>3)+1);
  sprintf($<data.code>$,"%s if (%s)\n%s",$<data.code>3,$<data.val>3,$<data.code>5);
  free($<data.code>3);free($<data.code>5);
}
| IF '(' expression ')' statement ELSE statement    
{
  $<data.code>$=malloc(49+2+2*strlen($<data.code>3)+1+strlen($<data.code>5)+1+strlen($<data.code>7)+1+strlen($<data.val>3)+1+4*digit_number(compteurif));
  sprintf($<data.code>$,
	  "%s if (%s)goto then%d;\n%s\ngoto endif%d;\nthen%d:;%s\nendif%d:;",
	  $<data.code>3,$<data.val>3,compteurif,$<data.code>7,compteurif,compteurif,$<data.code>5,compteurif);
  free($<data.code>7);
  free($<data.code>3);
  free($<data.code>5);
  compteurif++;
}
| FOR '(' expression_statement expression_statement expression ')' statement 
{
  
  fprintf(stderr,"**selection->for**\n");
  $<data.code>4[strlen($<data.code>4)-2]='\0';
  if ($<data.appel==1>5)
    {
      $<data.code>$=malloc(90+1+strlen($<data.val>4)+strlen($<data.code>3)+strlen($<data.code>4)+strlen($<data.code>5)+1+strlen($<data.val>5)+strlen($<data.code>7)+6*digit_number(compteurfor));
      sprintf($<data.code>$,
	      "%s\nbeginfor%d:;%sif (%s) goto blocfor%d;\ngoto endfor%d;\nblocfor%d:;%s \n%s%s;goto beginfor%d;\nendfor%d:;",
	      $<data.code>3,compteurfor,$<data.code>4,$<data.val>4,compteurfor,compteurfor,compteurfor,$<data.code>7,$<data.code>5,$<data.val>5,compteurfor,compteurfor);
    }
  else
    {
      $<data.code>$=malloc(90+1+strlen($<data.val>4)+strlen($<data.code>3)+strlen($<data.code>4)+strlen($<data.code>5)+strlen($<data.code>7)+6*digit_number(compteurfor));
      sprintf($<data.code>$,
	      "%s\nbeginfor%d:;%sif (%s) goto blocfor%d;\ngoto endfor%d;\nblocfor%d:;%s \n%s;goto beginfor%d;\nendfor%d:;",
	      $<data.code>3,compteurfor,$<data.code>4,$<data.val>4,compteurfor,compteurfor,compteurfor,$<data.code>7,$<data.code>5,compteurfor,compteurfor);
    }
  free($<data.code>3);
  free($<data.code>4);
  free($<data.code>5);
  free($<data.code>7);
  compteurfor++;
}
;

iteration_statement
: WHILE '(' expression ')' statement        
{
  $<data.code>$=malloc(90+strlen($<data.code>3)+strlen($<data.code>5)+6*digit_number(compteurwhile)+1+strlen($<data.val>3)); 
  sprintf($<data.code>$,
	  "startwhile%d:;%s if (%s) goto blocwhile%d;\ngoto endwhile%d;\nblocwhile%d:;%s \ngoto startwhile%d;\nendwhile%d:;",
	  compteurwhile,$<data.code>3,$<data.val>3,compteurwhile,compteurwhile,compteurwhile,$<data.code>5,compteurwhile,compteurwhile);
  free($<data.code>3);
  free($<data.code>5);
  compteurwhile++;
}
;

jump_statement
: RETURN ';'                      
{
  $<data.code>$=malloc(9);
  sprintf($<data.code>$,"return;\n");
}
| RETURN expression ';'
{
  $<data.code>$=malloc(2+strlen($<data.code>2)+8+1+strlen($<data.val>2));
  sprintf($<data.code>$,"%s return %s;\n",$<data.code>2,$<data.val>2);
  free($<data.code>2);
}
;

program
: external_declaration                
{
  fprintf(out,"%s",$<data.code>1);
  free(($<data.code>1));
}
| program external_declaration
{
  fprintf(out,"%s",$<data.code>2);
  /* $<data.code>$=malloc(1+strlen($<data.code>1)+1+1+strlen($<data.code>2));
  sprintf($<data.code>$,"%s %s",$<data.code>1,$<data.code>2);
  free($<data.code>1);
  free($<data.code>2);*/
}
;

external_declaration
: function_definition          
{
  $<data.code>$=$<data.code>1;
}
| declaration
{
  $<data.code>$=$<data.code>1;
}
;

function_definition
: type_name declarator compound_statement               
{
  $<data.code>$=malloc(1+strlen($<data.code>1)+2+1+strlen($<data.code>2)+1+strlen($<data.code>3));
  sprintf($<data.code>$,"%s %s %s",$<data.code>1,$<data.code>2,$<data.code>3);
  $<data.t>$=$<data.t>2;
  $<data.t>$->retour=malloc(sizeof(struct type));
  $<data.t>$->retour->dimension=0;
  $<data.t>$->retour->dimensions=NULL;
  $<data.t>$->retour->retour=NULL;
  $<data.t>$->retour->nb_parametres=0;
  $<data.t>$->retour->parametres=NULL;
  switch (*$<data.code>1)
    {
    case('i'):
      $<data.t>$->retour->t=_INT;
      break;
    case('v'):
      $<data.t>$->retour->t=_VOID;
      break;
    default:
      fprintf(stderr,"mauvaise définition de fonction : %s",$<data.code>$);
    }
  ajout_symbole(T,$<data.id>2,$<data.t>$);
  free($<data.code>1);
  free($<data.code>2);
  free($<data.code>3);
}
;

%%
#include <stdio.h>
#include <string.h>

extern char yytext[];
extern int column;
extern int yylineno;
extern FILE *yyin;

char *file_name = NULL;

int yyerror (char *s) {
  fflush (stdout);
  fprintf (stderr, "%s:%d:%d: %s\n", file_name, yylineno, column, s);
  return 0;
}
int digit_number(int k){
  if (k==0)
    k++;
  double d=1.0*k;
  d=log(d)/log(10.0);
  d=d+1.0;
  return (int) d;
}


int main (int argc, char *argv[]) {
  T=nouvelle_table(NULL);
  tmp=1;
  tmpmul=1;
  tmpadd=1;
  tmpcomp=1;
  compteurfor=1;
  compteurif=1;
  compteurwhile=1;
  FILE *input = NULL;
  if (argc==2) {
    input = fopen (argv[1], "r");
    file_name = strdup (argv[1]);
    if (input) {
      yyin = input;
    }
    else {
      fprintf (stderr, "Could not open %s\n", argv[1]);
      return 1;
    }
  }
  else {
    fprintf (stderr, "%s: error: no input file\n", *argv);
    return 1;
  }
  out=fopen("ri.txt","w+");
  
  yyparse ();
  cherche_symbole(T,"azer");
  fclose(out);
  fclose(input);
  free (file_name);
  return 0;
}
