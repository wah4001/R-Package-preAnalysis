#' @title  Compare continuous data based on groups
#' @name groupsum
#' @description  This function will write a summary tables for continous variables by groups.
#' This function can maximum take two factors to write out a summary table. The summary table
#' has the number of varibales, the missing values, mean, standard deviation, median and
#' quantile for all groups. If there's one factor, table will show the p-values of ANOVA
#' and Kruskal-Wallis test at the last row. If there are two factors, the table will only show
#' the global p-value from the ANOVA test.
#' @usage groups.summary(x, g1, g2 = NULL, ...)
#' @param x Response variable required to be analyzed
#' @param g1 Required explanatory variable (first predictor)
#' @param g2 Optional explanatory variable (second predictor)
#' @return table summary for groups of continous variables
#' @keywords groups
#' @examples
#' groupsum(poisons$time,poisons$treat)
#' groupsum(poisons$time,poisons$treat, poisons$poison)
#' @export
groupsum <- function(x, g1, g2 = NULL, ...) {
  .pcheck <- function(p.val) {
    p.txt <- ifelse(p.val < 0.001, "<0.001", format(round(p.val, 3), nsmall = 3))
    return(p.txt)
  }

  is.valid <- function(x) {
    is.null(shiny::need(x, message = FALSE))
  }
  g1name <- deparse(substitute(g1))
  g2name <- deparse(substitute(g2))
  g1name <- strsplit(g1name, split = "\\$")[[1]][2]
  if (is.valid(g2name))
    g2name <- strsplit(g2name, split = "\\$")[[1]][2]

  g1 <- factor(g1)

  if (is.null(g2)) {
    empty <- rep("", (length(unique(g1)) - 1))
    pval.anova <- anova(lm(x ~ g1, ...))$`Pr(>F)`[1]
    pval.KW <- kruskal.test(x ~ g1, ...)$p.value
    p.val <- data.frame(pval.anova = c(empty, .pcheck(pval.anova)),
                        pval.KW = c(empty, .pcheck(pval.KW)))

    ddt <- data.frame(x, g1)
    ddt <- data.table::data.table(data.frame(x, g1))
    table <- ddt[, list(n = length(x), n.done = length(x[!is.na(x)]), mean = mean(x,na.rm = T),
                        sd = sd(x, na.rm = T), q1 = quantile(x, probs = 0.25, na.rm = T),
                        q2 = quantile(x, probs = 0.5, na.rm = T),
                        q3 = quantile(x, probs = 0.75,na.rm = T)), by = list(g1)]
    out <- data.frame(table)
    reuslt <- data.frame(Group1 = out[, 1], n = out[, 2], n.complete = out[, 3],
                         mean.sd = paste0(round(out[, 4], 2), " +/- ", round(out[, 5], 2)),
                         median.IQR = paste0(round(out[, 7], 2), " (", round(out[, 6], 2),
                                             ", ", round(out[, 8], 2), ")"))
    names(reuslt)[1] <- g1name
  } else {
    g2 <- factor(g2)
    data.table <- data.frame(x, g1, g2)
    empty <- rep("", (length(unique(g1)) * length(unique(g2)) - 1))
    p.anova <- anova(lm(formula = x ~ g1 + g2 + g1 * g2, data.table))[1, ]$"Pr(>F)"
    p.anova <- .pcheck(p.anova)
    p.val <- data.frame(pval.anova = c(empty, p.anova))

    ddt <- data.frame(x, g1, g2)
    ddt <- data.table::data.table(data.frame(x, g1, g2))
    table <- ddt[, list(n = length(x), n.done = length(x[!is.na(x)]), mean = mean(x, na.rm = T),
                        sd = sd(x, na.rm = T), q1 = quantile(x, probs = 0.25, na.rm = T),
                        q2 = quantile(x, probs = 0.5, na.rm = T),
                        q3 = quantile(x, probs = 0.75, na.rm = T)), by = list(g1, g2)]
    out <- data.frame(table)
    reuslt <- data.frame(Group1 = out[, 1], Group2 = out[, 2], n = out[, 3],
                         n.complete = out[, 4], mean.sd = paste0(round(out[, 5], 2), " +/- ", round(out[, 6], 2)),
                         median.IQR = paste0(round(out[, 8], 2), " (", round(out[, 7], 2), ", ", round(out[, 9], 2), ")"))
    names(reuslt)[1] <- g1name
    names(reuslt)[2] <- g2name
  }
  reuslt <- data.frame(reuslt, p.val)
  return(reuslt)
}
