int test(int x) {
    printint(x);
    return 1;
}

int main() {
    printint(test(1));
    return 0;
}