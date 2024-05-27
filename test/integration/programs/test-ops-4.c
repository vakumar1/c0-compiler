// Result: 11

int main() {
    bool x = true || (false && true || true); // true
    bool y = (false && true) && (true || true || true); // false
    if (x && y) {
        return 1;
    } else {
        int a = 0;
        bool z = false;
        while (!(x && y)) {
            if (z) {
                y = true;
            } else {
                z = true;
            }
            a++;
        }
        if (z == true) {
            a *= 2;
            if (x == (!!y || false)) {
                a += 5;
            }
            int incr = (true || (y && false) || x) 
                            ? (false && true ? 1 : 2)
                            : (false ? 3 : 4);
            return a + incr;
        }
        return a;
    }
}