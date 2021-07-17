from math import *
import numpy as np

ls = [1]
cn = 100
alp = 0.5
bet = 1.03

for i in range(cn):
	nxt = np.random.randn() / 5 
	prv = ls[-1]
	nxt += prv * bet
	nxt += alp
	ls.append(nxt)

print("[", end="")
for i in range(len(ls)):
	print(ls[i], end=" ;")
print("\b]")

print("[", end="")
for i in range(len(ls)):
	print(ls[i], end=" ,")
print("\b]")