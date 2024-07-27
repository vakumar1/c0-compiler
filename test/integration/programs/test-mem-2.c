// Result: 15

int main() {
    int[5] arr;
    for (int i = 0; i < 5; i++) {
        arr[i] = i;
    }
    for (int i = 0; i < 5; i++) {
        arr[i]++;
    }
    int sum = 0;
    for (int i = 0; i < 5; i++) {
        sum += arr[i];
    }
    arr[0] = sum;
    return arr[0];
}
