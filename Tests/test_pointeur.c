int main(){
    int x=5;
    int y  = &x;
    *y = 6;
    printint(x);
    return 0;
} 