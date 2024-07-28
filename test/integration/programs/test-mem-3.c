// Result: 125

int main() {
    int[5][5][5] arr;
    for (int i = 0; i < 5; i++) {
        for (int j = 0; j < 5; j++) {
            for (int k = 0; k < 5; k++) {
                arr[i][j][k] = 1;
            }
        }
    }
    int sum = 0;
    for (int i = 0; i < 5; i++) {
        for (int j = 0; j < 5; j++) {
            for (int k = 0; k < 5; k++) {
                sum += arr[i][j][k];
            }
        }
    }
    return sum;
}
