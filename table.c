#include "table.h"
#include <stdlib.h>
#include <string.h>
enum _type {_INT,_FLOAT,_VOID,_FONCTION};
  struct type{
    enum _type t; //0:void,1:int,2:float,3:fonction
    int dimension;//0:primitif,>0: tableau
    int* dimensions;
    struct type * retour;//null sauf pour les fonctions
    int nb_parametres;
    struct type * parametres;//null sauf pour les fonctions
    };
struct type *cherche_symbole(struct table *t,char* id){
 
  struct symbole *s=t->premier;
  while(s!=NULL)
    {
      if (strcmp(s->id,id)==0)
	return s->t;
    }
  if(t->englobante==NULL)
    return NULL;
  return cherche_symbole(t->englobante,id);
}
int verif_type_operation(struct type* a,struct type* b)
{
  switch (a->t){
    
  case _VOID :
    return 0;
    break;
  case _INT:
    return (b->t)==_INT||(b->t==_FLOAT&&b->dimension==1);
    break;
  case _FLOAT:
    return (b->t==_FLOAT&&(a->dimension==0||b->dimension==0||(a->dimension==1&&b->dimension==1)));
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
      return (b->t==_INT)||((*affect_type=='+')&&(b->t==_FLOAT)&&(b->dimension==1));
      break;
    case _FLOAT:
      return (b->t==_FLOAT&&((a->dimension==0&&b->dimension==0)||(a->dimension==1&&b->dimension==1&&a->dimensions[0]<b->dimensions[0])));
      break;
    default:
      return 0;
    }
}
      
    
void ajout_symbole(struct table * courante,char * id,struct type *t)
{
  struct symbole*s=courante->premier;
  while(NULL!=s)
    {
      s=s->suivant;
    }
  s=malloc(sizeof(*s));
  s->id=id;
  s->t=t;
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