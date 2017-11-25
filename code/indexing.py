# Note to any one looking at this and coming from the R notes, python utilizes 0 based indexing, which means that things start with 0 instead of 1, like most other data science programming languages (and regular human, as opposed to mathematical, experience). Also, when dealing with ranges, the last number is left open, i.e. not included as part of the range. So if you want the first 10 things (I'm using 'first' loosely), you'd need the range(0, 10), which would give you the zeroeth through 9th indices, i.e. the first 10 elements. I assure you though, this approach will cost no programming time whatsoever, nor any issue at all on projects that require multiple programming languages.

import string
import numpy as np
import pandas as pd

letters_list = list(string.ascii_lowercase)
letters_dict = {'a':1, 'b':2, 'c':3, 'd':4, 'e':5}
letters = np.array(list(string.ascii_lowercase))

## ----vectorSlice, eval=TRUE----------------------------------------------
letters[3:6]
letters[[12,9,2]]

## example ranges
[i for i in (range(0,10))]  # or range(10)
np.arange(10)

myMatrix = np.arange(1,13).reshape(3,4)
mydf = pd.DataFrame({'a': [1,5,2],
                     'b': [3,8,1]}, index=['row1', 'row2', 'row3'])

## ----matrixSlice---------------------------------------------------------
myMatrix[0, 2:4]

## ----dfindexlabel--------------------------------------------------------
mydf.loc['row1', 'b']

## ----dfindexpos----------------------------------------------------------
mydf.iloc[0, 1]

## ----dfindexmix----------------------------------------------------------
mydf.ix['row1', 1]  # deprecated due to reasons of 'user confusion', do by label for reproducibility anyway

## ----dfindexslice--------------------------------------------------------
mydf.loc['row1',:]
mydf.loc[:,'b']

## ----dfindexnoncont------------------------------------------------------
mydf.iloc[[0,2],:]

## ----dfindexbool---------------------------------------------------------
mydf[lambda df: df.a >= 2]

## ----dflistslice---------------------------------------------------------
## Note, pandas dataFrames are not lists, but you could get something similar by converting it to a dictionary via mydf.to_dict().  These examples will just regard a list/dict where noted.
#my_list_or_df[2:4]
letters_list[1:4]


## ----dflistextract-------------------------------------------------------
letters_dict['d']   # dict
mydf['a']           # dataframe

## ----dflistextract2------------------------------------------------------
mydf.a
letters_dict.pop('a')   # pop by name


## ------------------------------------------------------------------------
mymatrix = np.matrix(np.random.normal(size=100)).reshape(10, 10)
mydf = pd.read_csv('data/cars.csv')
my_matdf_list = {'thismat':mymatrix, 'thisdf': mydf}  # all have to be named

## ---- echo=F-------------------------------------------------------------
mymatrix[0:5, ]
mymatrix[:,0:5]
mymatrix[0,1]

## ---- echo=F-------------------------------------------------------------
mydf.disp
mydf.iloc[:,3]
mydf['disp']

## ---- echo=F-------------------------------------------------------------
my_matdf_list.pop('thisdf')






