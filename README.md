# resstars

Package to create Coef and SE table for LaTeX. Requires a matrix printable for
`printCoefmat()`.

# Example

```R
library(resstars)

mm <- summary(lm(cars))$coefficients
resstars(mm)

rownames(mm) <- c("a_variable1", "b_variable1")
resstars2(mm)

```