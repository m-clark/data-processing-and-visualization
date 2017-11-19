import numpy as np
import pandas as pd


x = np.asmatrix([1, 3, 2, 5, 4])
x.dtype

# character vector as list
x = ['... Of Your Fake Dimension', 'Ephemeron', 'Dryswch', 'Isotasy', 'Memory']
x
# as np object
x = np.asarray(['... Of Your Fake Dimension', 'Ephemeron', 'Dryswch', 'Isotasy', 'Memory'])
x


## ----factors -----------------------------------------
x_string = np.repeat(['a', 'b', 'c'], 10)
x_factor = pd.Categorical(x_string)
x_factor
x_factor.dtype
np.sum(x_factor)             # not what you want
x_num = x_factor.codes
np.sum(x_num)                # can use as numeric 


## ----createMatrix--------------------------------------------------------
# vectors; note that the sequence is open on the right, i.e. will not include the stop number
x = np.arange(1,5)
y = np.arange(5,9)
z = np.arange(9,13)

np.vstack((x,y,z))   
np.hstack((x,y,z))

# or reshape to 2d matrix and use concatenate
x = np.arange(1,5).reshape(1,4)
y = np.arange(5,9).reshape(1,4)
z = np.arange(9,13).reshape(1,4)

np.concatenate((x,y,z), axis=0)


## ----list----------------------------------------------------------------
x = [1, 'apple', [3, 'cat']]
x


## ----listloop------------------------------------------------------------
for elem in x: print(type(elem))


## ----namedlist-----------------------------------------------------------
x = {'a':25, 'b':-1, 'c':0}
x['b']


## ----createdf-------------------------------------------------
mydf = pd.DataFrame({'a': [1,5,2],
                     'b': [3,8,1]})

## ----dfrownames-----------------------------------------------
mydf.index  = {'row' + str(x) for x in np.arange(1,4)}


## ----dsex1, echo=F-------------------------------------------------------
mydf = pd.DataFrame({'A':[1,2,3], 'B':['a','b','c']})


## ----dsex2, echo=F-------------------------------------------------------
mylist = [['a','b'], [1,2,3], mydf]
