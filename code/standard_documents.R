## ----rchunk, echo=FALSE, results='markup'--------------------------------------
cat("```{r}
x = rnorm(10)
```")


## ----rchunk-opts, echo=FALSE, results='markup'---------------------------------
cat("```{r mylabel, echo = TRUE, eval = TRUE, cache = FALSE, out.width = '50%'}
# code
```")


## ----rchunk-default, echo=FALSE, results='markup'------------------------------
cat("```{r setup}
knitr::opts_chunk$set(
  echo = T,
  message = F,
  warning = F,
  error = F,
  comment = NA,
  R.options = list(width = 220),
  dev.args = list(bg = 'transparent')
)
```")


## ----rinline, results='markup', echo=FALSE-------------------------------------
x = rnorm(10)
cat("Here is a sentence whose sum is `r 2 + 2`.")
cat("This sentence has a value of `r x[1]`.")

