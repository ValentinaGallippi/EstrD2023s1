#include <iostream>
#include <algorithm>
#include "BiBST.h"
using namespace std;


//==========================================================================
// Invariante de representación
//==========================================================================


/* INV.REP.
  * no puede haber 2 coordenadas que sean iguales dentro del BiBST. 
  * si x > kx && y > ky, entonces el nodo con la coordenada (x,y) va a estar en el la posicion 0 del array hijo, siendo el cuadrante NE.
  * si x > kx && y <= ky, entonces el nodo con la coordenada (x,y) va a estar en el la posicion 1 del array hijo, siendo el cuadrante SE. 
  * si x <= kx && y > ky, entonces el nodo con la coordenada (x,y) va a estar en el la posicion 2 del array hijo, siendo el cuadrante NO. 
  * si x <= kx && y <= ky, entonces el nodo con la coordenada (x,y) va a estar en el la posicion 3 del array hijo, siendo el cuadrante SO. 
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
  if(nodo == NULL) {
    return NULL;
  } else {
    if (x==nodo->kx && y==nodo->ky){
      return nodo; 
    } else {
      return findBBNode(nodo->hijo[indiceCuadrante(nodo,x,y)],x,y);
    }
  }
}

BBNode* insertBBNode(BBNode* nodo, int x, int y) {
    BBNode* nuevo = new BBNode;
    nuevo->kx = x;
    nuevo->ky = y;
    nuevo->bolitas[ROJO] = 0;
    nuevo->bolitas[VERDE]= 0;
    nuevo->bolitas[NEGRO]= 0;
    nuevo->bolitas[AZUL] = 0;
    nuevo->hijo[NE] = NULL;
    nuevo->hijo[SE] = NULL;
    nuevo->hijo[NO] = NULL;
    nuevo->hijo[SO] = NULL;
    if (nodo == EMPTYBB){
      return nuevo;
    } else {
    BBNode* actual   = nodo;
    BBNode* anterior = NULL;
    while (actual!= NULL && (actual->kx!= x || actual->ky!=y)){
      anterior = actual;
      actual   = actual->hijo[indiceCuadrante(actual,x,y)];
    } 
    if (actual == NULL) {
      anterior->hijo[indiceCuadrante(anterior,x,y)] = nuevo;
      return nuevo;
    } else  {
      return actual;
    }
    }
}

void LiberarBiBST(BiBST t) {
  if (t!=NULL){
  LiberarBiBST(t->hijo[0]);
  LiberarBiBST(t->hijo[1]); 
  LiberarBiBST(t->hijo[2]); 
  LiberarBiBST(t->hijo[3]); 
  delete t->hijo[0];
  delete t->hijo[1];
  delete t->hijo[2];
  delete t->hijo[3];
  delete t;
  }
}

//==========================================================================
// Impresión para verificaciones
//==========================================================================
void PrintBBNode(BBNode* t, int tab) {
  if (t == NULL) { return; }
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

