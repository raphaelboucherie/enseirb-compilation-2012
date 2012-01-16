#include "table.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
enum _type {_INT,_FLOAT,_VOID,_FONCTION};
  struct type{
    enum _type t; //0:void,1:int,2:float,3:fonction
    int dimension;//0:primitif,>0: tableau
    int* dimensions;
    struct type * retour;//null sauf pour les fonctions
    int nb_parametres;
    struct type * parametres;//null sauf pour les fonctions
    };
struct symbole
  {
    char * id;
    struct type *t;
    struct symbole *suivant;
    };
struct type *cherche_symbole(struct table *t,char* id){
 
  struct symbole *s=t->premier;
  while(s!=NULL)
    {
      fprintf(stderr,"%s==%s?",s->id,id);
      if (strcmp(s->id,id)==0)
	return s->t;
      s=s->suivant;
    }
  
  if(t->englobante==NULL)
    return NULL;
  return cherche_symbole(t->englobante,id);
}
int verif_type_operation(struct type* a,//type de la première opérande
			 struct type* b,//type de la deuxième opérande
			 char c//opération : * + -
			 )
{
  switch (a->t){
    
  case _VOID :
    return 0;
    break;
  case _INT:
    if (c=='*')
      {
	if((b->t)==_INT)
	  return 1;
	if(b->t==_FLOAT&&b->dimension==1)
	  return 2;
	else
	  return 0;
      }
    else 
      return (b->t)==_INT;
    break;
  case _FLOAT:
    if (c=='*'&&b->t==_INT&&a->dimension==1)//vecteur * int 
      return 1;
    else
      {
	if (b->t!=_FLOAT)
	  return 0;
	if(a->dimension>1||b->dimension>1)
	  return 0;
	if(b->dimension==0)//b float non vecteur
	  return 1;
	if(a->dimension==0)//a float non vecteur et b float vecteur
	  return 2;
	if(a->dimensions[0]>b->dimensions[0])//a float vecteur et b float vecteur avec taille de a > taille de b
	  return 1;
	else //a float vecteur et b float vecteur avec taille de a < taille de b
	  return 2;
      
      }
  
    break;
  default :
    return 0;     
  }
}
int verif_type_moins(struct type *t){
  return( t->t==_INT||(t->t==_FLOAT&&t->dimension==1));
}
int verif_type_prod_scalaire(struct type *a,struct type *b){
  return (a->dimension==1&&b->dimension==1);
}
int verif_type_comp(struct type *a,struct type *b){
  return ((a->t==_INT||a->t==_FLOAT)&&a->dimension==0&&(b->t==_INT||b->t==_FLOAT)&&b->dimension==0);
}
int verif_type_access(struct type *a){
  return(a->t==_FLOAT&&a->dimension>1);
}
int verif_type_affect(struct type *a,struct type *b,char * affect_type){
  
  switch (a->t)
    {
    case _VOID:
      return 0;
      break;
    case _INT:
      return (b->t==_INT);
      break;
    case _FLOAT:
      return (b->t==_FLOAT&&
	      (
	       (*affect_type=='+'
		&&b->dimension==1)
	       ||
	       
	       (a->dimension==0
		&&b->dimension==0)
	       ||
	       (a->dimension==1
		&&b->dimension==1
		&&a->dimensions[0]<b->dimensions[0])));
      break;
    default:
      return 0;
    }
}
      
    
void ajout_symbole(struct table * courante,char * id,struct type *t)
{
  if (courante->premier==NULL)
    {
      courante->premier=malloc(sizeof(*(courante->premier)));
      courante->premier->id=id;
      courante->premier->t=t;
      courante->premier->suivant=NULL;
    }
  else 
    {
      struct symbole*s=courante->premier;
      while(NULL!=s->suivant)
	{
	  s=s->suivant;
	}
      s->suivant=malloc(sizeof(*s));
      s->suivant->id=id;
      s->suivant->t=t;
      s->suivant->suivant=NULL;
    }
}
struct table *nouvelle_table(struct table * englo){
  struct table * t=malloc(sizeof(*t));
  t->englobante=englo;
  t->premier=NULL;
  return t;
}
void delete_type(struct type * t)
{
  if(t->dimensions!=NULL)
    free(t->dimensions);
  if(t->retour!=NULL)
    free(t->retour);
  free(t);
}
void delete_symboles(struct symbole * s)
{
  if(NULL!=s)
    {
      if (s->t!=NULL)
	delete_type(s->t);
      free(s->id);
      delete_symboles(s->suivant);
      free(s);
    }
}
struct table * delete_table(struct table * courante)
{
  struct table * englo=courante->englobante;
  delete_symboles(courante->premier);
  free(courante);
  return(englo);
}
