// Result: 3

int main(void) {
    bool a = 1 != 2;
    if (!!a) {
        if (1 > 3) {
            return 1;
        } else if (4 + 5 <= -1) {
            return 2;
        } else {
            int x = 3 + 4 * 5;
            if (x == 23) {
                if (3 - 5 >= -16) {
                    bool b = !(4 < 5 - 3);
                    if (b) {
                        int x = 2;
                        x++;
                        return x;
                    } else {
                        return 4;
                    }
                } else {
                    return 5;
                }
            } else {
                return 6;
            }
        }
    } else {
        return 7;
    }
}