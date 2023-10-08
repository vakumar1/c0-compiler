s = """while for continue break
return assert true false NULL alloc alloc_array
int bool void char string"""

s = s.split()
for tok in s:
    res = f"| {tok.upper()}"
    print(res)