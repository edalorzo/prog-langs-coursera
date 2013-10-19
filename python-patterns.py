
world = "world"
w,o,r,l,d = world

def sum(a_list):
    if a_list == []: # or "if not a_list:"
        return 0
    return a_list[0] + sum(a_list[1:])

print sum([1,2,3,4,5])

