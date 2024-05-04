// Result: 32

int main(void) {
    int a = 1;
    while (a < 30) {
        if (a % 2 == 0)
            a *= 2;
        else
            a += 1;
    }
    return a;
}