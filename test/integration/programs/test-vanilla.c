// Result: 17

int main (void) {
    int x = 0;
    int y = 3;
    for (int i = 0; i < 3; i++) {
        if (i < 1) {
            int z = 5;
            x += 1;
            for (int j = 3; j >= 0; j--) {
                x += 3;
            }
        } else {
            x += 2;
            y += 3;
            int r = 15;
        }
    }
    return x;
}
