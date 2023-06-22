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
  * solamente son parte del BiBST los nodos que tienen por lo menos una bolita de algun color, o si es el nodo de la coordenada (0,0).
*/

/* OBSERVACIÓN:
Al ser un tablero infinito, todas las celdas posibles son parte del tablero. Pero, solamente las celdas en las cuales hay por lo menos una bolita de un color forman parte del BiBST. 
Por eso, los nodos son insertados cuando hay que añadir bolitas en la celda actual. El nodo con la coordenada (0,0) es el único que siempre esta.
*/

//--------------------------------------------------------------------------
TableroInfinito TInfInicial(){
  TableroInfinitoHeader* tablero = new TableroInfinitoHeader;
  tablero->x = 0;
  tablero->y = 0;
  tablero->celdas = insertBBNode(EMPTYBB,0,0);
  return tablero;
}

//--------------------------------------------------------------------------
void PonerNTInf(TableroInfinito t, Color color, int n){
  // PRECOND: el color es válido
  BiBST actual = findBBNode(t->celdas,t->x,t->y);
  if (actual == EMPTYBB){
    actual = insertBBNode(t->celdas,t->x,t->y);
    actual->bolitas[color] = n;
  } else {
  actual->bolitas[color] += n;
  }
}

//--------------------------------------------------------------------------
void SacarNTInf(TableroInfinito t, Color color, int n){
  // PRECOND:
  //  * el color es válido
  //  * hay al menos n bolitas en la celda actual en t
  BiBST actual = findBBNode(t->celdas,t->x,t->y);
  if (actual == EMPTYBB) {
    BOOM("no hay la cantidad de bolitas dadas en la celda actual");
  } else if (actual->bolitas[color] < n) {
    BOOM("no hay la cantidad de bolitas dadas en la celda actual");
  } else {
    actual->bolitas[color] -= n;
  }
}

//--------------------------------------------------------------------------
void MoverNTInf(TableroInfinito t, Dir dir, int n){
  // PRECOND: la dirección dada es válida
  if (dir == NORTE) {
    t->y += n;
  } else if (dir == SUR) {
    t->y -= n;
  } else if (dir == OESTE){
    t->x -= n;
  } else if (dir == ESTE) {
    t->x += n;
  } else {
    BOOM("la direccion dada no es valida");
  }
}

//--------------------------------------------------------------------------
int nroBolitasTInf(TableroInfinito t, Color color) {
  // PRECOND: el color es válido
  BiBST actual = findBBNode(t->celdas,t->x,t->y);
  if (actual == EMPTYBB) {
    return 0;
  }
  else {
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
