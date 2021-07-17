import numpy as np
import pandas as pd

le = 100 
ls = []
while len(ls) < le :
	a = np.random.randn()
	# if (a < 2.0 and a> -2.0):
	if True:
		ls.append(a)

print("[", end="")
for i in range(len(ls)):
	print(ls[i], end=" ;")
print("\b]")

print("[", end="")
for i in range(len(ls)):
	print(ls[i], end=" ,")
print("\b]")