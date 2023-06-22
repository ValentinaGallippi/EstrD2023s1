#include <iostream>
#include <algorithm>
#include "BiBST.h"
#include "TableroInfinito.cpp"
using namespace std;

void PonerDeColor(BBNode* n) {
    n->bolitas[ROJO] = 2;
}

int main() {
    BBNode* nodo = insertBBNode(EMPTYBB,2,3);
    BBNode* nodo2 = insertBBNode(nodo,-1,-1);
    BBNode* nodo3 = insertBBNode(nodo,-2,-3);
    PrintBB(nodo);
    LiberarBiBST(nodo);
    PrintBB(nodo);
}

