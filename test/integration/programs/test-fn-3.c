// Result: -14

int args1(int x, bool y, int z) {
    if (y) {
        return 3 * x + z + 1;
    } else {
        return x - z;
    }
}

bool args2(int a, int b, bool c, bool d) {
    if (c || a >= 0) {
        if (d) {
            return false;
        } else {
            return true;
        }
    }
    return true;
}

int main() {
    int s = args1(3, true, 4);
    bool t = args2(5, 6, false, true);
    if (t) {
        return s;
    } else {
        return -s;
    }
}
