int fact(int n) {
    if (n) {
        &n;
        return n*fact(n-1); 
    }
    else { return n+1; }
}

int main() {
    printint(fact(12));
    return 0;
}