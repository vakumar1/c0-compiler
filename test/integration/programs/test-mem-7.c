// Result: 5

int main() {
    int a = 3;
    int* ptr = &a;
    a = 5;
    return *ptr;
}
