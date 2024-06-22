// Result: -1

int fn(int a, int b) {
    assert(true);
    assert(a != b);
    return a - b;
}

int main() {
    return fn(1, 2);
}
