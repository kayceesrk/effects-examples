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
plt.title('Fitted line')

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

x_min, x_max = -0.5, 10.5
x_plot = np.linspace(x_min, x_max, 100)

np.random.shuffle(alpha), np.random.shuffle(beta)
for i in range(len(alpha)):
  plt.plot(x_plot, alpha[i] + beta[i] * x_plot, color='lightsteelblue', alpha=0.003 )


# x = 10 * np.random.rand(100)
# y = alpha + beta * x

alpha = 2.0
beta = 3.0
sigma = 1.0

am = 1.83
bm = 3.00

x = 10 * np.random.rand(100)
y = alpha + beta * x
y = np.random.normal(y, scale=sigma)
plt.scatter(x, y)


plt.plot(x, am + bm * x, color='black')
plt.savefig('linreg.png')
plt.show()
