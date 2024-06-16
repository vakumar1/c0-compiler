// Result: 50

int handle_odd(int curr_sum, int index);

int handle_even(int curr_sum, int index) {
    curr_sum += (index + 1);
    if (index == 0) {
        return curr_sum;
    }
    return handle_odd(curr_sum, index - 1);
    
}

int handle_odd(int curr_sum, int index) {
    curr_sum += index;
    return handle_even(curr_sum, index - 1);
}

int main() {
    return handle_odd(0, 9);
}
