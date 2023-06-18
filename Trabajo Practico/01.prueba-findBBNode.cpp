#include <iostream>
#include <algorithm>
#include "BiBST.h"
#include "TableroInfinito.cpp"
using namespace std;

void PonerDeColor(BBNode* n) {
    n->bolitas[ROJO] = 2;
}

int main() {
    BBNode* nodo = insertBBNode(EMPTYBB,0,0);
    BBNode*  nodox = insertBBNode(nodo,-1,0);
    PonerDeColor(nodox);
    PrintBB(nodo);
}

