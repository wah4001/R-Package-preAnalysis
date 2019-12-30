#' @title  Check assumption for parametrics tests
#' @name assumption.check
#' @description  This function will return the p values of  Shapiro–Wilk test
#' result and Bartlett's Test result. At the same time, this fucntion will output
#' residual plot with QQ plot by package ggfortify. It will also write suggestion
#' about normality and equality of variance of the dataset. User can maximally
#' imput two factors.
#' @param x Response variable required to be analyzed
#' @param g1 Required explanatory variable (first predictor)
#' @param g2 Optional explanatory variable (second predictor)
#' @return Shapiro–Wilk test result and Bartlett's Test result with graphic checks
#' @keywords groups
#' @examples
#' assumption.check(poisons$time, poisons$treat)
#' assumption.check(poisons$time, poisons$treat, poisons$poison)
#' @export
assumption.check <- function(x, g1, g2=NULL,  ...) {

  .pcheck <- function(p.val) {
    p.txt <- ifelse(p.val < 0.001, "<0.001", format(round(p.val, 3), nsmall = 3))
    return(p.txt)
  }

  g1 <- factor(g1)

  if (is.null(g2)) {
    out.lm <- lm(formula = x ~ g1, ...)
    bartlett <- bartlett.test(x, g1)$p.value
  }else {
    g2 <- factor(g2)
    testdata <- data.frame(x,g1, g2)
    out.lm <- lm(formula = x ~ g1 * g2, ...)
    bartlett <- bartlett.test(x ~ interaction(g1,g2), data=testdata)$p.value
  }

  res = residuals(out.lm)
  shapiro <- shapiro.test(res)$p.value

  recommed.shapiro <- ifelse(shapiro < 0.05, "Not Normal", "Approximately Normal")
  recommed.bartlett <- ifelse(bartlett < 0.05, "Variance is not constant", "Variance is constant")
  shapiro <- .pcheck(shapiro)
  bartlett <- .pcheck(bartlett)
  result <- matrix(c("Normality Check", "Variance Check", "Shapiro-Wilk Normality Test",
                     "Bartlett Test of Homogeneity of Variances", shapiro, bartlett, recommed.shapiro,
                     recommed.bartlett), nrow = 2)
  colnames(result) <- c("Check", "Test Applied", "p-values", "Recommend Result")
  plot<-ggplot2::autoplot(out.lm)
  return(list(result, plot))
}


