int fact(int n) {
    if (n<=1) {
        return 1;
    }else {
        return n*fact(n-1);
    }
}

int fibo(int n) {
    if (n <= 2) {
        return 1;
    }else {
        return fibo(n-1) + fibo(n-2);
    }
}

int main() {
    printint(fact(12)); // 479001600
    printint(fibo(15)); //610
    return 0;
}