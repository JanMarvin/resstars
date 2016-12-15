# Copyright (C) 2014-2015 Jan Marvin Garbuszus
#
# This program is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation; either version 2 of the License, or (at your
# option) any later version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
# more details.
#
# You should have received a copy of the GNU General Public License along
# with this program. If not, see <http://www.gnu.org/licenses/>.

#' Create LaTeX regression coefficient tables from matrixes useabel with
#'  `printCoeftable()`
#'
#' \code{resstars} prints a regression table containing coef and se and
#' stars and dots to indicate significance
#'
#' @param mat  \emph{character.} Matrix to be converted to LaTeX table.
#' @param nams \emph{character.} Character vector of length two containing
#' colnames.
#' @param digits \emph{integer.} Number of digits to be presented.
#' @seealso \code{\link[xtable]{xtable}} in package \code{xtable} and
#'  \code{stargazer} for different approaches
#' @author Jan Marvin Garbuszus \email{jan.garbuszus@@ruhr-uni-bochum.de}
#' @export

resstars <- function(mat, nams, digits = 3){

  cat("%%%%%%%%%%%%%%%%%%% \n")
  cat("% Coef und SE table \n")

  stopifnot(is.matrix(mat))

  if (missing(nams))
    nams <- c("\\textnormal{Coef}", "&", "\\textnormal{SE}", "\\\\ ")
  else
    nams <- c(paste0("\\textnormal{",nams[1],"}"), "&",
              paste0("\\textnormal{",nams[2],"}"), "\\\\ ")


  n <- nrow(mat)
  rnams <- rownames(mat)

  mat <- round(mat, digits = digits)

  cat("\\begin{table}[ht] \n")
  cat("\\centering \n")
  cat("\\begin{tabular}{ l S S} \n")

  fill <- rep("&", n)
  end  <- rep("\\\\ ", n)

  stars <- vector("character", length = n)

  dgts <- paste0("%.", digits, "f")

  v_coef <- sprintf(dgts, mat[, 1])
  v_se   <- sprintf(dgts, mat[, 2])

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
      v_se[i] <- "$@$"
  }

  z <- cbind(v_coef, stars, fill, v_se, end)

  cat("\\toprule \n")

  cat(" & ", nams, "\n")

  cat("\\midrule \n")

  colnames(z) <- NULL

  rownames(z) <- paste0("$",rnams,"$")

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

#' Combine two variables next to each other
#'
#' \code{resstars2} prints a regression table containing coef and se and
#' stars and dots to indicate significance
#'
#' @param mat  \emph{character.} Matrix to be converted to LaTeX table.
#' @param nams \emph{character.} Character vector of length two containing
#' colnames.
#' @param digits \emph{integer.} Number of digits to be presented.
#' @seealso \code{\link[xtable]{xtable}} in package \code{xtable} and
#'  \code{stargazer} for different approaches
#' @author Jan Marvin Garbuszus \email{jan.garbuszus@@ruhr-uni-bochum.de}
#' @export
resstars2 <- function(mat, nams, digits = 3){

  cat("%%%%%%%%%%%%%%%%%%% \n")
  cat("% Coef und SE table \n")

  stopifnot(is.matrix(mat))

  if (missing(nams))
    nams <- c("Coef", "SE")

  nams <- c(paste0("\\textnormal{",nams[1],"}"), "&",
            paste0("\\textnormal{",nams[2],"}"), "&",
            paste0("\\textnormal{",nams[1],"}"), "&",
            paste0("\\textnormal{",nams[2],"}"), "\\\\ ")


  n <- nrow(mat)
  rnams <- rownames(mat)

  # rnams <- c("a_bla", "b_bla")
  rnams <- strsplit(rnams, "_")

  modname <- NULL
  varname <- NULL
  for (i in 1:length(rnams)){
    mod_i <- rnams[[i]][1]
    var_i <- rnams[[i]][2]

    modname <- c(modname, mod_i)
    varname <- c(varname, var_i)
  }

  rnams <- unique(varname)

  mat <- round(mat, digits = digits)

  cat("\\begin{table}[ht] \n")
  cat("\\centering \n")
  cat("\\begin{tabular}{ l S S S S} \n")

  fill <- rep("&", n/2)
  end  <- rep("\\\\ ", n/2)

  stars <- vector("character", length = n)

  dgts <- paste0("%.", digits, "f")

  v_coef <- sprintf(dgts, mat[, 1])
  v_se   <- sprintf(dgts, mat[, 2])

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
      v_se[i] <- "$@$"
  }


  upr <- 1:(n/2)
  lwr <- ((n/2)+1):n

  z <- cbind(v_coef[upr], stars[upr], fill, v_se[upr], fill,
             v_coef[lwr], stars[lwr], fill, v_se[lwr], end)

  cat("\\toprule \n")

  cat("& \\multicolumn{2}{c}{$", modname[1],"$} & \\multicolumn{2}{c}{$",modname[2],"$} \\\\ \n")

  cat(" & ", nams, "\n")

  cat("\\midrule \n")

  colnames(z) <- NULL

  rownames(z) <- paste0("$",rnams,"$")

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
