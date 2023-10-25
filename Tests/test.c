int test(int x) {
    if (x) {
        return 10;
    } else{
        return 50;
    }
    return 20; //ici la printint if if else
}

int main() {
    printint(test(1));
    return 0;
}