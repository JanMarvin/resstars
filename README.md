# resstars

Package to create Coef and SE table for LaTeX. Requires a matrix printable for
`printCoefmat()`.

# Example

```R
library(resstars)

mm <- summary(lm(cars))$coefficients
resstars(mm)
```