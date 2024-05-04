// Result: 20

int main(void) {
    int a = 0;
    for (int i = 0; i < 10; i++) {
        if (i % 2 == 0) {
            int x = 0;
            while (x < 3) {
                for (int i = 0; i < 3; i++) {
                    a += i;
                }
                x++;
            }
        } else {
            bool y = false;
            while (!y) {
                for (int i = 3; i >= -1; i--) {
                    a--;
                }
                y = true;
                while (!y) {}
            }
        }
    }
    return a;
}