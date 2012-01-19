/*enum _type {_INT,_FLOAT,_VOID,_FONCTION};
  struct type{
    enum _type t; //0:void,1:int,2:float,3:fonction
    int dimension;//0:primitif,>0: tableau
    int* dimensions;
    struct type * retour;//null sauf pour les fonctions
    int nb_parametres;
    struct type * parametres;//null sauf pour les fonctions
    };*/
/* struct symbole
  {
    char * id;
    struct type *t;
    struct symbole *suivant;
    };*/
  struct table{
    struct symbole *premier;
    struct table * englobante;
  };

struct type *cherche_symbole(struct table *t,char* id,int bool;);
int verif_type_operation(struct type* a,struct type* b,char c);
int verif_type_moins(struct type *t);
int verif_type_prod_scalaire(struct type *a,struct type *b);
int verif_type_comp(struct type *a,struct type *b);
int verif_type_access(struct type *a);
int verif_type_affect(struct type *a,struct type *b,char * affect_type);
void ajout_symbole(struct table * courante,char * id,struct type *t);
struct table * nouvelle_table(struct table * englo);
void delete_type(struct type * t);
void delete_symboles(struct symbole * s);
struct table *delete_table(struct table * courante);
int compare_type_arguments(struct type*a,struct type *b);
