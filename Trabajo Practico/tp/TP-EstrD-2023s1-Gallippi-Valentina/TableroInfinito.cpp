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
  --
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
  if (VALIDCOLOR(color)){
    BiBST actual = findBBNode(t->celdas,t->x,t->y);
    if (actual == EMPTYBB){
      actual = insertBBNode(t->celdas,t->x,t->y);
      actual->bolitas[color] = n;
    } else {
      actual->bolitas[color] += n;
    }
  }
}

//--------------------------------------------------------------------------
void SacarNTInf(TableroInfinito t, Color color, int n){
  // PRECOND:
  //  * el color es válido
  //  * hay al menos n bolitas en la celda actual en t
  if (VALIDCOLOR(color)) {
    BiBST actual = findBBNode(t->celdas,t->x,t->y);
    if (actual == EMPTYBB) {
      BOOM("no hay la cantidad de bolitas dadas en la celda actual");
    } else if (actual->bolitas[color] < n) {
      BOOM("no hay la cantidad de bolitas dadas en la celda actual");
    } else {
      actual->bolitas[color] -= n;
    } 
  } else { 
    BOOM("el color dado no es valido");
  } 
}
//--------------------------------------------------------------------------
void MoverNTInf(TableroInfinito t, Dir dir, int n){
  // PRECOND: la dirección dada es válida
  if (VALIDDIR(dir)){
    if (dir == NORTE) {
      t->y += n;
    } else if (dir == SUR) {
      t->y -= n;
    } else if (dir == OESTE){
      t->x -= n;
    } else if (dir == ESTE) {
      t->x += n;
    } 
  }
}

//--------------------------------------------------------------------------
int nroBolitasTInf(TableroInfinito t, Color color) {
  // PRECOND: el color es válido
  if (VALIDCOLOR(color)){
    BiBST actual = findBBNode(t->celdas,t->x,t->y);
    if (actual == EMPTYBB) {
      return 0;
    }
    else {
      return actual->bolitas[color];
    }
  } else {
    return 0; // el color no es valido
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
