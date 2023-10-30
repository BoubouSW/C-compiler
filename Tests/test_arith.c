int main(){
    //Test d'ordre des opérations sans parenthèses
    printint((1 * 8 + 6 - 5 / 3 % 2) == 13);
    printint((5 / 8 * 2 % 7 - 1 + 9) == 8);
    printint((-8 * 2 % 3 + 5 - 1 / 4) == 4);
    printint((1 - 9 % 4 / 6 + 2 * 4) == 9);

    //Test d'ordre des opérations avec parenthèses
    printint(((2 + 3) * 4) == 20);
    printint((-(-5)) == 5);

    //Test divers
    printint(((-16) % 3) == -1);
    return 0;
}
