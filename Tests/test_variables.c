int f(int x){
    return x+1;
}

int g(int x){
    return 2*x;
}

int main() {
    int x;
    x = 10;
    int y;
    y = 9;
    printint(x+y); // 19
    printint(g(f(x+3))); // 28

    return 0;
}
