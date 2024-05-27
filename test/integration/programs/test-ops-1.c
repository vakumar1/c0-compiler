// Result: -1

int main() {
    int x = 3 + 4 * 5 - 16 + 1;     // 8
    int y = 6 / 3 + 2 + (4 * 5);    // 24
    int z = x + (-y);               // -16
    z %= 5;                         // -1
    return z;
}