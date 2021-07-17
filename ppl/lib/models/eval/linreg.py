import matplotlib
import matplotlib.pyplot as plt
import seaborn as sns
import pandas as pd
import numpy as np
import sys
import time

n = (int)(sys.argv[1]) 
for i in range(n):
	print("\tlet* m"+str(i)+" = normal 0.0 3.0 in")
print("\tlet* c = normal 1.0 5.0 in")
print('\tfor i = 0 to (obs_points-1) do ')
print("\t\tobserve (mk ay.(i) -. c ", end="")
for i in range(n):
	print("-. m"+str(i)+"*.mk ax.(i).("+str(i)+")", end="")

print(") (logpdf Primitive.(normal 0. 3.))")

print('\tdone ;\n\tm0')