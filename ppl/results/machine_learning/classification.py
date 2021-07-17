import pystan
import pickle
import matplotlib
import matplotlib.pyplot as plt
import seaborn as sns
import pandas as pd
import numpy as np
import sys


sns.set()  
np.random.seed(101)

plt.xlabel('$x$')
plt.ylabel('$y$')
plt.title('Binary Classification')


xr = []
yr =[]
xg =[]
yg =[]
for i1 in range(6):
	for j1 in range(6):
		i = i1/5 
		j = j1/5 
		if(i+j < 1.1):
			xr.append(i)
			yr.append(j)
		else:
			xg.append(i)
			yg.append(j)

xr = np.asarray(xr)
yr = np.asarray(yr)
xg = np.asarray(xg)
yg = np.asarray(yg)


plt.scatter(xr, yr, color='green')
plt.scatter(xg, yg, color='red')


fname = sys.argv[1] 
my_file = open(fname, "r")
content_list = my_file.readlines()

ls = []
for x in content_list:
    x = x.strip()
    ls.append(float(x))

beta = np.asarray(ls)

fname = sys.argv[2] 
my_file = open(fname, "r")
content_list = my_file.readlines()

ls = []
for x in content_list:
    x = x.strip()
    ls.append(float(x))

alpha = np.asarray(ls)

x_min, x_max = -0.5, 1.5

x_plot = np.linspace(x_min, x_max, 50)

np.random.shuffle(alpha), np.random.shuffle(beta)
for i in range(2000):
	plt.plot(x_plot, -1/beta[i] - alpha[i] * x_plot / beta[i], color='lightsteelblue', alpha=0.002 )

s1 = -0.93
s2 = -0.9

x = 1.4 * np.random.rand(100) -0.2


plt.plot(x, -1/s2 - s1* x/s2, color='black')

plt.xlim([-0.2, 1.2])
plt.ylim([-0.2, 1.2])
plt.savefig('class.png')
plt.show()


