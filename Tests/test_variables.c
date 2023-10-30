int passageEtRetour(int n){
    return 2*n;
}

int nonEcrasementDeValeur(int n){
    for(int i=0 ; i < 5 ; i++){
        int j = 3;
    }
    return n;
}

int modificationLocale(int n){
    n = 2*n;
    return n;
}

int composition(int n){
    return 2*n;
}

int main() {
    int n = 2;
    printint(passageEtRetour(n) == 2 * n);
    printint(nonEcrasementDeValeur(n) == n);
    printint(modificationLocale(n) == 2*n);
    printint(composition(composition(n)) == 4*n);
    return 0;
}
