// Result: 1

struct simple_struct;

struct nested_struct;

struct super_nested_struct {
    bool b1;
    bool b2;
    struct nested_struct* ns;
};

struct nested_struct {
    int val1;
    int val2;
    struct simple_struct* ss;
};

struct simple_struct {
    int val1;
    int val2;
    bool cond;
};

int main() {
    struct super_nested_struct sns;
    struct nested_struct ns;
    struct simple_struct ss;
    sns.ns = &ns;
    ns.ss = &ss;
    sns.ns->ss->cond = true;
    sns.ns->ss->val1 = 1;
    sns.ns->ss->val2 = 2;
    if (sns.ns->ss->cond) {
        if (sns.ns->ss->val1 == ss.val1) {
            return sns.ns->ss->val1;
        } else {
            return 0;
        }
    } else {
        return sns.ns->ss->val2;
    }
}
