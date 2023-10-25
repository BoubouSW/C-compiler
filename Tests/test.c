int test(int x) {
    int y = 5;
    while (y) {
        printint(y);
        y = y - 1;
    }
    return 20; //ici la printint if if else
}

int main() {
    printint(test(1));
    return 0;
}