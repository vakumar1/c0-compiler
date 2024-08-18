// Result: 1

struct simple_struct;

struct nested_struct {
    int val1;
    int val2;
    struct simple_struct ss;
};

struct simple_struct {
    int val1;
    int val2;
    bool cond;
};

int main() {
    struct nested_struct ns;
    ns.ss.cond = true;
    ns.ss.val1 = 1;
    ns.ss.val2 = 2;
    if (ns.ss.cond) {
        return ns.ss.val1;
    } else {
        return ns.ss.val2;
    }
}