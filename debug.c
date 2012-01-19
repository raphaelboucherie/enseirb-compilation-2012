/** \file debug.cpp
 * \brief Fonctions d'erreur et de debug du compilateur.
 */


#include <stdio.h>
#include <stdlib.h>
#include "debug.h"


/// Niveau de debug
int debugLevel = 0;


/** \brief Indique s'il y a déjà eu une erreur ou pas
 * 
 */
bool errorIsSet = false;



void errorWarning(char *s)
{
  fprintf(stderr,"Warning: %s\n",s);
}



void errorError(string str)
{
  fprintf(stderr,"Error: %s\n",s);
  errorIsSet = true;
}



void errorFatal(string str)
{
  fprintf(stderr,"Fatal error: %s\n",s);
  freeMemory();
  exit(EXIT_FAILURE);
}



void freeMemory()
{

}



int yyerror(char* s)
{
  errorFatal(s);
  return EXIT_FAILURE;
}



void setDebugLevel( int argc, char **argv )
{
  int i;

  for( i=0; i<argc; i++ )
  {
    if( strcmp(argv[i],"-d") == 0 )
      debugLevel |= DEBUG_MSG;
  }
}

