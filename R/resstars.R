resstars <- function(mat, nams, digits = 3){

  cat("%%%%%%%%%%%%%%%%%%% \n")
  cat("% Coef und SE table \n")

  stopifnot(is.matrix(mat))

  if (missing(nams))
    nams <- c("Coef", "&", "SE", "\\\\ ")
  else
    nams <- c(nams[1], "&", nams[2], "\\\\ ")


  m <- 3 #ncol(mat)
  n <- nrow(mat)

  mat <- round(mat, digits = digits)

  cat("\\begin{table}[ht] \n")
  cat("\\centering \n")
  cat("\\begin{tabular}{", rep("r",m),"} \n")

  fill <- rep("&", n)
  end  <- rep("\\\\ ", n)



  stars <- vector("character", length = n)

  pval <- mat[,4]

  for (i in 1:n){
    if (pval[i] <= 0.000)
      stars[i] <- "$^{***}$"
    if (pval[i] > 0.000 & pval[i] <= 0.001)
      stars[i] <- "$^{**}$"
    if (pval[i] > 0.001 & pval[i] <= 0.01)
      stars[i] <- "$^{*}$"
    if (pval[i] > 0.01 & pval[i] <= 0.05)
      stars[i] <- "$^{.}$"
    if(pval[i] == 99999)
      stars[i] <- "$^{@}$"
  }


  z <- cbind(mat[, 1], stars, fill, mat[, 2], end)

  cat("\\toprule \n")

  cat(" & ", nams, "\n")

  cat("\\midrule \n")

  colnames(z) <- NULL


  for (row in rownames(z)){
    cat(row, "& ")
    cat(z[row,], "\n")
  }
  cat("\\bottomrule \n")

  cat("\\addlinespace[1ex] \n")
  cat("\\multicolumn{3}{l}{\\textsuperscript{***}$p<0.000$, ")
  cat("\\textsuperscript{**}$p<0.001$, ")
  cat("\\textsuperscript{*}$p<0.01$} \n")


  cat("\\end{tabular} \n")
  cat("\\end{table} \n")

}

