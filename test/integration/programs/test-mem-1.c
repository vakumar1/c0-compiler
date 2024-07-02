// Result: 1

void fn(int* a) {
    *a++;
}

int main() {
    int a = 0;
    int* ref = &a;
    fn(ref);
    return a;
}
