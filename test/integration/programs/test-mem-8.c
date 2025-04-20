// Result: 5

int main() {
    int*[5][5]*[5] pointer_arr_ref;
    int*[5][5] pointer_arr;
    int a = 3;
    for (int i = 0; i < 5; i++) {
        for (int j = 0; j < 5; j++) {
            pointer_arr[i][j] = &a;
        }
    }
    for (int i = 0; i < 5; i++) {
        pointer_arr_ref[i] = &pointer_arr;
    }
    a = 5;
    return *((*pointer_arr_ref[0])[1][2]);
}
