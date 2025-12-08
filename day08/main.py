from heapq import heappop, heapify
from math import prod
import sys

class UnionFind:
    def __init__(self, elems):
        self.parent = { elem: elem for elem in elems }
        self.size = { elem: 1 for elem in elems }

    def find(self, x):
        if self.parent[x] == x:
            return x
        return self.find(self.parent[x])

    def unite(self, x, y):
        rep_x = self.find(x)
        rep_y = self.find(y)

        if rep_x != rep_y:
            self.parent[rep_y] = rep_x
            self.size[rep_x] += self.size[rep_y]

        return self.size[rep_x]


def dist(p, q):
    x, y, z = p
    x2, y2, z2 = q
    return ((x-x2)**2 + (y-y2)**2 + (z-z2)**2)**(1/2)

nums = []
for line in sys.stdin.readlines():
    nums.append(tuple(map(int, line.split(','))))

h = []
for i in range(len(nums)):
    for j in range(i+1, len(nums)):
        h.append((dist(nums[i], nums[j]), nums[i], nums[j]))

heapify(h)

uf = UnionFind(nums)
for _ in range(1000):
    d, p, q = heappop(h)
    uf.unite(p, q)

size = [uf.size[p] for p in uf.size if uf.parent[p] == p]
size.sort(key=lambda x: -x)
print('Part 1:', prod(size[:3]))

while True:
    d, p, q = heappop(h)
    if uf.unite(p, q) == len(nums):
        print('Part 2:', p[0]*q[0])
        break
