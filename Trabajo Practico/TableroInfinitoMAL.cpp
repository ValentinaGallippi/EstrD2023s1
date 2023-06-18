#include <iostream>
#include <iomanip>
#include <algorithm>
#include "TiposBasicos.h"
#include "TableroInfinito.h"
#include "BiBST.h"
using namespace std;

//==========================================================================
// Implementación de TableroInfinito
//==========================================================================
struct TableroInfinitoHeader {
  int x; // coordenada x de la posicion actual.
  int y; // coordenada y de la posicion actual.
  BiBST celdas; 
}; 
/* INV.REP.:
  * x,y son siempre la coordenada de la posicion actual. 
  * en el tablero tiene que existir la posicion actual. 
*/

//--------------------------------------------------------------------------
TableroInfinito TInfInicial(){
  TableroInfinitoHeader* t = new TableroInfinitoHeader;
  t->celdas = insertBBNode(EMPTYBB,0,0);
  t->x = 0;
  t->y = 0;
  return t;
}

//--------------------------------------------------------------------------
void PonerNTInf(TableroInfinito t, Color color, int n){
  // PRECOND: el color es válido
  if (t==NULL){
    TableroInfinitoHeader* t = new TableroInfinitoHeader;
    BBNode* celda = insertBBNode(EMPTYBB,0,0);
    t->celdas = celda;
    t->x = 0;
    t->y = 0;
    celda->bolitas[color] = n;
  } else {
    BBNode* actual = findBBNode(t->celdas,t->x,t->y);
    actual->bolitas[color] = actual->bolitas[color] + n;
  }
}

//--------------------------------------------------------------------------
void SacarNTInf(TableroInfinito t, Color color, int n){
  // PRECOND:
  //  * el color es válido
  //  * hay al menos n bolitas en la celda actual en t
  if (t==NULL) {
    BOOM("tablero vacio");
  } else {
    BBNode* actual = findBBNode(t->celdas,t->x,t->y);
    if (actual->bolitas[color]>= n)  {
      actual->bolitas[color] = actual->bolitas[color] - n;
    } else {
      BOOM("la celda actual no tiene la cantidad dadas de bolitas del color dado.");
    }
  }
}

//--------------------------------------------------------------------------
void MoverNTInf(TableroInfinito t, Dir dir, int n){
  // PRECOND: la dirección dada es válida
  if (dir == NORTE) {
    t->y = t->y + n;
  } else if (dir == SUR) {
    t->y = t->y - n;
  } else if (dir == ESTE) {
    t->x = t->x + n;
  } else {
    t->x = t->x - n;
  }
  BBNode* n = insertBBNode(t->celdas,t->x,t->y)
}

//--------------------------------------------------------------------------
int nroBolitasTInf(TableroInfinito t, Color color) {
  // PRECOND: el color es válido
  if (t=NULL) {
    return 0;
  } else {
    BBNode* actual = findBBNode(t->celdas,t->x,t->y);
    return actual->bolitas[color];
  }
}

//--------------------------------------------------------------------------
void LiberarTInf(TableroInfinito t){
  LiberarBiBST(t->celdas);
  delete t; 
}

//==========================================================================
// Impresión para verificaciones
//==========================================================================
void PrintRepTInf(TableroInfinito t) {
  cout << "Celda actual: (" << t->x << ","<< t->y << ")" << endl;
  PrintBB(t->celdas);
}
