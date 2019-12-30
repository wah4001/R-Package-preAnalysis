#' Adjusted p values for pairwise comparisons
#' @description Multiple types of adjusted p values of pairwise comparisons between group levels.
#' @usage p.adj.compare(x, g, control = levels(g)[1], level.names = levels(g),
#'                      n = match(control,level.names),p.digit = 3, paired = FALSE,
#'                      compareall = FALSE, t.test = TRUE, wilcoxon = TRUE,
#'                       wilcoxon.holm = TRUE, holm = TRUE, bonferroni = FALSE,
#'                       Dunnett = TRUE, hochberg = TRUE, hommel=TRUE,
#'                       BH = TRUE, BY = TRUE, Tukey = TRUE, ...)
#' @param x Response variable required to be analyzed.
#' @param g Required explanatory variable (grouping vector or factor).
#' @param paired a logical indicating whether you want a paired test. Defaults to FALSE.
#' @param compareall if TRUE do pairwise comparsion for all groups and return p values. Defaults to FALSE.
#' @param control indicate one group to compare with all other groups. Defaults to first group.
#' @param p.digit the decimal places for p.values. Defaults to 3.
#' @param t.test a logical indicating whether you want to do two sample t.test result p.values without
#' adjustment. Defaults to TRUE. If FALSE, then this function can only show Wilcoxon tests result p.values
#' and Holm-Bonferroni adjusted p.values for Wilcoxon tests.
#' @param wilcoxon a logical indicating whether you want show two sample Wilcoxon tests result p.values
#'  without adjustment. Defaults to TRUE.
#' @param wilcoxon.holm a logical indicating whether you want show Holm-Bonferroni adjusted p.values for
#' Wilcoxon tests.
#' @param holm a logical indicating whether you want show Holm-Bonferroni adjusted p.values for t.test.
#' Defaults to FALSE.
#' @param hochberg a logical indicating whether you want show Hochberg's adjusted p.values for t.test.
#' Defaults to FALSE.
#' @param hommel a logical indicating whether you want show Hommel's adjusted p.values for t.test.
#' Defaults to FALSE.
#' @param bonferroni a logical indicating whether you want show Bonferroni adjusted p.values for t.test.
#' Defaults to FALSE.
#' @param BH a logical indicating whether you want show Benjamini adjusted p.values for t.test.
#' Defaults to FALSE.
#' @param BY a logical indicating whether you want show Yekutieli adjusted p.values for t.test.
#' Defaults to FALSE.
#' @param dunnett a logical indicating whether you want show Dunnett's Test adjusted p.values. Defaults
#' to TRUE.
#' @param tukey a logical indicating whether you want show Tukey's test adjusted p.values. Defaults to TRUE.
#' @details Only if t.test is True, this program can show adjusted p value for Holm-Bonferroni,
#'  Bonferroni, Dunnett's Test and Tukey's test. If compareall is TRUE, Dunnett's Test adjusted p.values
#'  will not show up. We can have the Tukey's test adjusted p value only if compareall is TRUE.
#'  "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr" adjustment applied by p.adjust from
#'  p.adjust package.
#' @return table (data.frame) of p values
#' @keywords groups compare
#' @examples
#' p.adj.compare(poisons$time, poisons$treat)
#' p.adj.compare(poisons$time, poisons$treat, control="C")
#' p.adj.compare(poisons$time, poisons$treat, compareall=TRUE, paired=TRUE)
#' @export
p.adj.compare <- function(x, g, control = levels(g)[1], level.names = levels(g), n = match(control,level.names),
                          p.digit = 3, paired = FALSE, compareall = FALSE, t.test = TRUE, wilcoxon = TRUE,
                          wilcoxon.holm = TRUE,holm = TRUE, hochberg = FALSE, hommel = FALSE, BH = FALSE, BY =FALSE,
                          bonferroni = FALSE, dunnett = TRUE, tukey = TRUE, ...) {
  g <- factor(g)
  if(!is.element(control, level.names)) {
    control <- levels(g)[1]
    message("This is not a group name in the data set. Please enter a valid group name!")
  }

  ddt <- data.frame(x, g)
  ddt <- data.table::data.table(data.frame(x, g))
  table <- ddt[, list(n.done = length(x[!is.na(x)])), by = list(g)]
  b <- table[, 2]
  if (count(unique(b)) != 1 && paired == TRUE) {
    paired = FALSE
    message("We cannot apply paired test here, the number of elements in the groups are different! ")
  }

  .pcheck <- function(p.val) {
    p.txt <- ifelse(p.val < 0.001, "<0.001", format(round(p.val, p.digit), nsmall = p.digit))
    return(p.txt)
  }

  .numName <- function(n) {
    if (n == 1) {
      ix <- setNames(seq_along(level.names), level.names)
    } else if (n == length(level.names)) {
      ix <- setNames(1, level.names[n])
      ix <- c(ix, setNames(2:n, level.names[1:(n - 1)]))
    } else {
      ix <- setNames(1, level.names[n])
      ix <- c(ix, setNames(2:n, level.names[1:(n - 1)]))
      ix <- c(ix, setNames((n + 1):length(level.names), level.names[(n + 1):length(level.names)]))
    }
    return(ix)
  }

  .compare.levels <- function(i, j, method) {
    xi <- x[as.integer(g) == i]
    xj <- x[as.integer(g) == j]
    ifelse(method == "t.test", t.test(xi, xj, paired = paired)$p.value,
           wilcox.test(xi, xj, paired = paired)$p.value)
  }

  .compareall <- function(ix, method) {
    pp <- outer(ix[-1L], ix[-length(ix)], function(ivec, jvec) sapply(seq_along(ivec),
                                                                      function(k) {
                                                                        i <- ivec[k]
                                                                        j <- jvec[k]
                                                                        if (i > j) {
                                                                          .compare.levels(i, j, method)
                                                                        } else {
                                                                          NA
                                                                        }
                                                                      }))
    pp <- as.data.frame(as.table(pp))
    pp <- dplyr::filter(pp, !is.na(Freq))
    rownam <- paste0(pp$Var1, " - ", pp$Var2)
    PVAL <- matrix(unlist((pp[3])), ncol = 1, byrow = TRUE)
    rownames(PVAL) <- rownam
    return(PVAL)
  }

  .compareone <- function(ix, method) {
    pp <- outer(ix[1], ix[-1L], function(ivec, jvec) sapply(seq_along(ivec),
                                                            function(k) {
                                                              i <- ivec[k]
                                                              j <- jvec[k]
                                                              .compare.levels(i, j, method)
                                                            }))
    PVAL <- t(pp)
    names <- colnames(pp)
    rownames(PVAL) <- c(sapply(names, function(names) paste0(names, " - ", rownames(pp))))
    return(PVAL)
  }

  if(t.test == TRUE){
    if (compareall == TRUE) {
      ix <- setNames(seq_along(level.names), level.names)
      PVAL.t <- .compareall(ix, method = "t.test")
      colnames(PVAL.t) <- "pm.p"
      PVAL <- data.frame(PVAL.t)
      if (tukey == TRUE) {
        out <- lm(formula = x ~ g, data = ..., contrasts = list(g = "contr.sum"))
        out1 <- summary(glht(out, linfct = mcp(g = "Tukey")))
        pm.p.Tukey <- data.frame(out1$test$pvalues)
        colnames(pm.p.Tukey) <- "pm.p.Tukey"
        if(paired==TRUE){
          PVAL<-PVAL
          message("Cannot apply Tukey adjustment on Paired data.")
        }else{
          PVAL <- data.frame(PVAL, pm.p.Tukey)
        }
      }
    } else {
      ix <- .numName(n)
      PVAL.t <- .compareone(ix, method = "t.test")
      colnames(PVAL.t) <- "pm.p"
      PVAL <- data.frame(PVAL.t)
      if (dunnett == TRUE) {
        names(.numName(n))
        out <- lm(formula = x ~ g, data = ..., contrasts = list(g = "contr.sum"))
        out1 <- summary(glht(out, linfct = mcp(g = "Dunnett")))
        pm.p.Dunnett <- out1$test$pvalues
        if(paired==TRUE){
          PVAL<-PVAL
          message("Cannot apply Dunnett adjustment on Paired data.")
        }else{
          PVAL <- data.frame(PVAL, pm.p.Dunnett)
        }
      }
    }
    if (holm == TRUE) {
      pm.p.holm <- p.adjust(PVAL.t, "holm")
      PVAL <- data.frame(PVAL, pm.p.holm)
    }
    if (bonferroni == TRUE) {
      pm.p.bonferroni <- p.adjust(PVAL.t, "bonferroni")
      PVAL <- data.frame(PVAL, pm.p.bonferroni)
    }
    if (hommel == TRUE) {
      pm.p.hommel <- p.adjust(PVAL.t, "hommel")
      PVAL <- data.frame(PVAL, pm.p.hommel)
    }
    if (hochberg == TRUE) {
      pm.p.hochberg <- p.adjust(PVAL.t, "hochberg")
      PVAL <- data.frame(PVAL, pm.p.hochberg)
    }
    if (BH == TRUE) {
      pm.p.BH <- p.adjust(PVAL.t, "BH")
      PVAL <- data.frame(PVAL, pm.p.BH)
    }
    if (BY == TRUE) {
      pm.p.BY <- p.adjust(PVAL.t, "BY")
      PVAL <- data.frame(PVAL, pm.p.BY)
    }
  }
  if (wilcoxon == TRUE || wilcoxon.holm == TRUE) {
    if (compareall == TRUE) {
      ix <- setNames(seq_along(level.names), level.names)
      PVAL.w <- .compareall(ix, method = "wilcox")
    } else {
      ix <- .numName(n)
      PVAL.w <- .compareone(ix, method = "wilcox")
    }
    colnames(PVAL.w) <- "wilcoxon.p"
    if (t.test == FALSE)
      PVAL <- data.frame(PVAL.w) else if (wilcoxon == TRUE)
        PVAL <- data.frame(PVAL, PVAL.w) else PVAL <- PVAL
    if (wilcoxon.holm == TRUE) {
      wilcoxon.p.holm <- p.adjust(PVAL.w, "holm")
      PVAL <- data.frame(PVAL, wilcoxon.p.holm)
    }
  }
  PVAL[] <- lapply(PVAL, .pcheck)
  return(PVAL)
}

#' @title  Compare four groups of continous varibles by two factors
#' @name compare.2.factors
#' @description  This function will write a summary tables of p values and adjusted
#'  p values to show the main effect of the two factors and the interaction between the two factors.
#' @usage compare.2.factors(x, g, holm = TRUE, bonferroni = FALSE, ...)
#' @param x Response variable required to be analyzed
#' @param g Required explanatory variable
#' @param bonferroni a logical indicating whether you want show Bonferroni adjusted p.values for t.test.
#' Defaults to FALSE.
#' @param holm a logical indicating whether you want show Holm-Bonferroni adjusted p.values for t.test.
#' Defaults to FALSE.
#' @return table of p values
#' @keywords groups
#' @examples
#' compare.2.factors(poisons$time, poisons$treat)
#' @export

compare.2.factors <- function(x, g, holm = TRUE, bonferroni = FALSE, ...) {

  .pcheck <- function(p.val) {
    p.txt <- ifelse(p.val < 0.001, "<0.001", format(round(p.val, 3), nsmall = 3))
    return(p.txt)
  }
  TwofactorCompare <- function(x, g, ...) {
    g <- factor(g)
    level.names <- levels(g)
    if (length(level.names) != 4)
      message(" This function only can do comparison between two factors (four groups).")

    out1a.lm <- lm(formula = x ~ g - 1, ...)
    K <- rbind(c(-1, 1, 0, 0), c(0, 0, -1, 1), c(1, 0, -1, 0), c(0, 1, 0, -1),
               c(-1, 1, -1, 1), c(1, 1, -1, -1), c(1, 1, -1, -1))

    rownames(K) <- c(paste0(level.names[2], " - ", level.names[1]),
                     paste0(level.names[4], " - ", level.names[3]),
                     paste0(level.names[1], " - ", level.names[3]),
                     paste0(level.names[2], " - ", level.names[4]),
                     paste0(level.names[2], " + ", level.names[4], " - ", level.names[1], " - ", level.names[3]),
                     paste0(level.names[1], " + ", level.names[2], " - ", level.names[3], " - ", level.names[4]),
                     paste0(level.names[2], " - ", level.names[1], " - ", level.names[4], " + ", level.names[3]))
    return(summary(glht(out1a.lm, linfct = mcp(g = K)), test = adjusted(type = "none")))
  }

  pm.p <- TwofactorCompare(x, g, ...)
  pm.p <- data.frame(pm.p$test$pvalues)
  names <- row.names(pm.p)
  PVAL <- matrix(unlist((pm.p)), ncol = 1, byrow = TRUE)
  rownames(PVAL) <- names
  colnames(PVAL) <- "pm.p"
  if (holm == TRUE) {
    pm.p.holm <- p.adjust(PVAL, "holm")
    PVAL <- data.frame(PVAL, pm.p.holm)
  }
  if (bonferroni == TRUE) {
    pm.p.bonferroni <- p.adjust(PVAL, "bonferroni")
    PVAL <- data.frame(PVAL, pm.p.bonferroni)
  }
  PVAL[] <- lapply(PVAL, .pcheck)
  return(PVAL)
}

