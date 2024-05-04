// Result: 5

int main(void) {
    int x = (1 << 2) | (4 >> 2);    // 5
    int y = ((3 ^ 3) + (x & 2)) | (~-1);      // 0
    return x | y;
}