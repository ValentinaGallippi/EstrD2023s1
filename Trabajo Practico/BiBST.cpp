#include <iostream>
#include <algorithm>
#include "BiBST.h"
using namespace std;


//==========================================================================
// Invariante de representación
//==========================================================================

/* INV.REP.
  * dentro del BiBST no hay coordenadas repetidas. 
  * si x > kx && y > ky, entonces el nodo con la coordenada (x,y) va a estar en el cuadrante NE.
  * si x > kx && y <= ky, entonces el nodo con la coordenada (x,y) va a estar en el cuadrante SE. 
  * si x <= kx && y > ky, entonces el nodo con la coordenada (x,y) va a estar en el cuadrante NO. 
  * si x <= kx && y <= ky, entonces el nodo con la coordenada (x,y) va a estar en el cuadrante SO. 
*/

//==========================================================================
// Implementación
//==========================================================================


Cuadrante indiceCuadrante(BBNode* nodo, int x,int y){
  if (x > nodo->kx && y > nodo->ky){
    return NE;
  } else if (x > nodo->kx && y <= nodo->ky){
    return SE;
  } else if (x <= nodo->kx && y > nodo->ky){
    return NO;
  } else {
    return SO;
  }
}

BBNode* findBBNode(BBNode* nodo, int x, int y) { 
  if(nodo == EMPTYBB) {
    return EMPTYBB;
  } else {
    if (x==nodo->kx && y==nodo->ky){
      return nodo; 
    } else {
      return findBBNode(nodo->hijo[indiceCuadrante(nodo,x,y)],x,y);
    }
  }
}

BBNode* insertBBNode(BBNode* nodo, int x, int y) {
  if (nodo == EMPTYBB){
    BBNode* nuevo = new BBNode;
    nuevo->kx = x;
    nuevo->ky = y;
    return nuevo;
  } else {
    BBNode* actual   = nodo;
    BBNode* anterior = EMPTYBB;
    while (actual!= EMPTYBB && (actual->kx!= x || actual->ky!=y)){
      anterior = actual;
      actual   = actual->hijo[indiceCuadrante(actual,x,y)];
    } 
    if (actual == EMPTYBB) {
      BBNode* nuevo = new BBNode;
      nuevo->kx = x;
      nuevo->ky = y;
      anterior->hijo[indiceCuadrante(anterior,x,y)] = nuevo;
      return nuevo;
    } else  {
      return actual;
    }
  }
}

void LiberarBiBST(BiBST t) {
  if (t!=EMPTYBB){
  LiberarBiBST(t->hijo[0]);
  LiberarBiBST(t->hijo[1]); 
  LiberarBiBST(t->hijo[2]); 
  LiberarBiBST(t->hijo[3]);
  delete t; 
  }
}

//==========================================================================
// Impresión para verificaciones
//==========================================================================
void PrintBBNode(BBNode* t, int tab) {
  if (t == EMPTYBB) { return; }
  INDENT(tab)
  cout << "  (" << t->kx << "," << t->ky << "): ";
  PRINTCOLORN(AZUL , t->bolitas[AZUL ]); 
  cout << ", "; PRINTCOLORN(NEGRO, t->bolitas[NEGRO]); 
  cout << ", "; PRINTCOLORN(ROJO , t->bolitas[ROJO ]); 
  cout << ", "; PRINTCOLORN(VERDE, t->bolitas[VERDE]); 
  cout << endl;
  PrintBBNode(t->hijo[NE], ++tab);
  PrintBBNode(t->hijo[SE], tab);
  PrintBBNode(t->hijo[NO], tab);
  PrintBBNode(t->hijo[SO], tab);
}

void PrintBB(BiBST t) {
  cout << "BiBST:" << endl;
  PrintBBNode(t, 0);
}

