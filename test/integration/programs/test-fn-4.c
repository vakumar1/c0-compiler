// Result: 50

int handle_odd(int curr_sum, int index);

typedef int INTEGER;

int handle_even(INTEGER curr_sum, INTEGER index) {
    curr_sum += (index + 1);
    if (index == 0) {
        return curr_sum;
    }
    return handle_odd(curr_sum, index - 1);
    
}

typedef int INTEGER2;

int handle_odd(INTEGER2 curr_sum, INTEGER2 index) {
    curr_sum += index;
    return handle_even(curr_sum, index - 1);
}

int main() {
    return handle_odd(0, 9);
}
