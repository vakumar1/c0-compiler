// Result: 25

void incr_arr(int[5][5]* arr_ref) {
    for (int i = 0; i < 5; i++) {
        for (int j = 0; j < 5; j++) {
            (*arr_ref)[i][j]++;
        }
    }
}

int main() {
    int[5][5] arr;
    for (int i = 0; i < 5; i++) {
        for (int j = 0; j < 5; j++) {
            arr[i][j] = 0;
        }
    }
    int[5][5]* arr_ref = &arr;
    incr_arr(arr_ref);
    int sum = 0;
    for (int i = 0; i < 5; i++) {
        for (int j = 0; j < 5; j++) {
            sum += arr[i][j];
        }
    }
    return sum;
}
