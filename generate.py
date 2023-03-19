import random
def get(x, y, r):
    lst = ["N", "S", "W", "E"]
    l = []
    m = [1, 0]
    for i in range(x):
        for j in range(y):
            p = random.choices(m, weights = [r, 1-r])
            print(p)
            if p == [1]:
                dir = random.choice(lst)	
                print(dir)
                l.append(((i, j), dir))
    return l 
    
x, y, r = map(float, input().split())
print((get(int(x), int(y), r)))


