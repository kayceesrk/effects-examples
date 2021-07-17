import sys
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from scipy.stats import norm
from scipy.stats import chi2
from scipy.stats import expon
from scipy.stats import beta
from scipy.stats import kstest

def func(x):
	# return (norm.cdf([x], loc=2, scale=5))[0]
	# return (chi2.cdf([x], df=4))[0]
	# return (expon.cdf([x]))[0]
	# return (beta.cdf([x], a=1, b=1))[0]
	# return (beta.cdf([x], a=1, b=2))[0]
	# return (beta.cdf([x], a=2, b=1))[0]
	return (x-2)/3

fname = sys.argv[1] 
my_file = open(fname, "r")
content_list = my_file.readlines()

ls = []
for x in content_list:
	x = x.strip()
	ls.append(float(x))

nls = np.asarray(ls)
# cdfs =

print(len(ls), kstest(ls, func).statistic)

# mn = np.mean(nls)
# md = np.median(nls)
# # print(ls)
# plt.hist(ls, bins=(int)(sys.argv[2]), range=((float)(sys.argv[3]), (float)(sys.argv[4])))
# plt.axvline(x=mn, label="mean at {}".format(round(mn,2)), c='r')
# plt.axvline(x=md, label="median at {}".format(round(md,2)), c='k')
# plt.legend()
# plt.savefig(fname[0:-4]+'.png')
# plt.show()

