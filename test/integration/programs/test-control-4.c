// Result: 100000

int main(void) {
    int a = 0;
    int limit = 10;
    for (int i = 0; i < limit; i++) {
        for (int i = 0; i < limit; i++) {
            for (int i = 0 ; i < limit; i++) {
                for (int i = 0; i < limit; i++) {
                    for (int i = limit - 1; i >= 0; i--) {
                        a += a % 2 + (1 - a % 2);
                    }
                }
            }
        }
    }
    return a;
}