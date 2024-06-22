// Result: 3

int fn3() {
    return 5;
}

int fn2(int w, int z) {
    int a = fn3() + 5;
    return w == z ? w + a : a - z;
}

void fn1(int x, int y, bool c) {
    if (c) {
        int b = fn2(3, 7);
        if (b != 3) {
            return;
        }
    }
}

int x(int x) {
    return x + 1;
}

int main() {
    int x = x(3);
    fn1(x, x, false);
    return x - 1;
}
