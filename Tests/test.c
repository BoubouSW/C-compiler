int test(int x) {
    int y = 56;
    int z;
    z = 3;
    printint(z);
    printint(x);
    printint(z+x);
    printint(y + 1);
    return 1; //ici la printint if if else
}

int main() {
    printint(test(4));
    return 0;
}