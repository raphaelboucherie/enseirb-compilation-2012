/** \file debug.h
 * \brief Fonctions d'erreur et de debug du compilateur.
 */


#ifndef _DEBUG_H_
#define _DEBUG_H_

#include <string.h>


/// Affichage des messages de debug
#define DEBUG_MSG      0x01



/** \brief Erreur de type warning (niveau 1)
 *
 * Un warning prévient l'utilisateur d'une erreur possible ou d'une opération
 * illégale mais que le compilateur a tout de même pu analyser.
 * Après un warning, le code continue à être analysé et la compilation est
 * toujours possible.
 */
void errorWarning(char *s);


/** \brief Erreur de type error (niveau 2)
 *
 * Une erreur est envoyée quand une partie du code source n'est pas
 * interprétable. Elle rend impossible la compilation finale, mais le code
 * continue d'être analysé.
 */
void errorError(char *s);


/** \brief Erreur de type fatal (niveau 3)
 *
 * Lorsqu'une erreur fatal survient, le compilateur ne peut continuer son
 * travail. Il libère la mémoire du mieux qu'il peut et quitte.
 */
void errorFatal(char *s);


/// Libère la mémoire pour quitter le programme proprement.
void freeMemory();


/// Fonction d'erreur de yacc/bison, on "redirige" l'erreur vers errorFatal
int yyerror(char *s);


/** \brief Fixe le niveau de debug
 *
 * -d : afficher les messages de debug
 * -l : afficher les lexèmes lus
 */
void setDebugLevel( int argc, char** argv );

/// Affiche un message de debug
void debugEcho(string);

/// Affiche un lexème lu,
void debugLexeme(char *);

#endif
