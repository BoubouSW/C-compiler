int main() {
    
    printint(23*6+2*4-8 == 138);
    printint(23*6+2*4-8 < 139);
    printint(23*6+2*4-8 > 137);
    printint(1 && 1);
    printint(1 && 0);
    printint(0 && 0);
    printint(1 || 1);
    printint(1 || 0);
    printint(0 || 0);
    printint((1+1==2) || (2 == 4 ));
    printint(!((2+2!=4) && (1+2==3)));

    return 0;
}
