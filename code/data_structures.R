## ----vec-------------------------------------------------------------------------------------------
x = c(1, 3, 2, 5, 4)
x


## ----create_a_char, eval=T-------------------------------------------------------------------------
x = c('... Of Your Fake Dimension', 'Ephemeron', 'Dryswch', 'Isotasy', 'Memory')
class(x)


## ----factor_atts-----------------------------------------------------------------------------------
x = factor(rep(letters[1:3], e = 10))
x
attributes(x)


## ----factors, eval=F, echo=FALSE-------------------------------------------------------------------
## x = factor(1:3, labels = c('q', 'V', 'what the heck?'))
## x


## ----factors2, error=TRUE--------------------------------------------------------------------------
x_num = as.numeric(x)  # convert to a numeric object
sum(x_num)
sum(x)


## ----logical---------------------------------------------------------------------------------------
my_logic = c(TRUE, FALSE, TRUE, FALSE, TRUE, TRUE)


## ----logical_demo----------------------------------------------------------------------------------
!my_logic
as.numeric(my_logic)
mean(my_logic)


## ----int_num---------------------------------------------------------------------------------------
ints = -3:3   # integer sequences are easily constructed with the colon operator
class(ints)


x = rnorm(5)  # 5 random values from the standard normal distribution

x

typeof(x)
class(x)

typeof(ints)
is.numeric(ints)  # also numeric!


## ----dates-----------------------------------------------------------------------------------------
Sys.Date()

x = as.Date(c(Sys.Date(), '2020-09-01'))

x


## ----lubridate-------------------------------------------------------------------------------------
library(lubridate)

month(Sys.Date())

day(Sys.Date())

wday(Sys.Date(), label = TRUE )

quarter(Sys.Date())

as_date('2000-01-01') + 100


## ----numeric-date----------------------------------------------------------------------------------
as.numeric(Sys.Date())

as.Date(10, origin = '2000-01-01')  # 10 days after a supplied origin


## ----createMatrix----------------------------------------------------------------------------------
# create vectors
x = 1:4
y = 5:8
z = 9:12

rbind(x, y, z)   # row bind

cbind(x, y, z)   # column bind

matrix(
  c(x, y, z),
  nrow = 3,
  ncol = 4,
  byrow = TRUE
)


## ----list------------------------------------------------------------------------------------------
x = list(1, "apple", list(3, "cat"))
x


## ----listloop--------------------------------------------------------------------------------------
for (element in x) print(class(element))


## ----namedlist-------------------------------------------------------------------------------------
x = list("a" = 25, "b" = -1, "c" = 0)
x[["b"]]


## ----createdf, eval=TRUE---------------------------------------------------------------------------
mydf = data.frame(
  a = c(1, 5, 2),
  b = c(3, 8, 1)
)


## ----dfrownames, eval=TRUE-------------------------------------------------------------------------
rownames(mydf) = paste0('row', 1:3)
mydf


## ----dsex1, echo=F---------------------------------------------------------------------------------
mydf = data.frame(A = 1:3, B = letters[1:3])


## ----dsex2, echo=F---------------------------------------------------------------------------------
mylist = list(c('a', 'b'), 1:3, mydf)

