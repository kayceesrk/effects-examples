from math import *
import numpy as np

def get_random():
	a = np.random.randn() / 4 
	return a

ls = []
cn = 50

mu = 0.5
sig = 1
th1 = 2
th2 = 1

eps = []
eps.append(get_random())
eps.append(get_random())

ls.append(mu+eps[0])
ls.append(mu+th1*eps[0]+eps[1])

for i in range(cn):
	nxt = mu
	nxt += th1*eps[-1]
	nxt += th2*eps[-2]
	nxt += get_random()
	ls.append(nxt)

print("[", end="")
for i in range(len(ls)):
	print(ls[i], end=" ;")
print("\b]")