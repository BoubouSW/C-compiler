void fibo(int n,int* ret){
    if(n<=1){
        *ret=1;
    }

    if (n>1){ 
        int y;
        int z;
        fibo(n-1,&y);
        fibo(n-2,&z);
        *ret= y+z;
    }  
} 


int main(){
    int x;
    int* y=&x;
    int** z=&y;
    fibo(10,*z);
    printint(**z);
    return 0;
} 