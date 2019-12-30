#' Data transformation
#' @description Transform continous data by Box-Cox Transformation
#' @name boxcoxTrans
#' @usage boxcoxTrans(x, g1, g2=NULL, ...)
#' @param x Response variable required to be analyzed
#' @param g1 Required explanatory variable (first predictor)
#' @param g2 Optional explanatory variable (second predictor)
#' @param lb plot start at this x value
#' @param ub plot end at this x value
#' @param bk the interval of x values in the plot
#' @param plotit 	logical which controls whether the result should be plotted.
#' @lambda vector of values of < code >lambda – default (-2, 2) in steps of 0.1
#' @details This function return a data frame with one more colume of the transformed reponse variable.
#' This function automatically take the imputs varible names. For example, if the impute is x=data$nameA,
#' the this function will know the name of varible x is nameA. The transformed data name will be
#' nameA.trans in the output data frame.
#' @return table (data.frame) with adjusted resonse variable
#' @keywords transformation
#' @export
#' @examples
#' boxcoxTrans(poisons$time, poisons$treat)
#' boxcoxTrans(poisons$time, poisons$treat, poisons$poison)

boxcoxTrans <- function(x, g1, g2=NULL, lb = -2, ub = 2, bk = 1/10,
                         plotit = F, ...){

  is.valid <- function(n)
  {
    is.null(shiny::need(n, message = FALSE))
  }
  xname <- deparse(substitute(x))
  g1name <- deparse(substitute(g1))
  g2name <- deparse(substitute(g2))

  xname <- strsplit(xname, split = "\\$")[[1]][2]
  g1name <- strsplit(g1name, split = "\\$")[[1]][2]
  xname1 <- paste0(xname, "_trans")

  if(is.valid(g2name)) g2name <- strsplit(g2name, split = "\\$")[[1]][2]

  if (is.null(g2))
  {
    my_data<-data.frame(x,g1)
    bc<-MASS::boxcox(x ~ g1, data = my_data,
                     lambda = seq(lb, ub, bk), plotit = plotit)
    max.point <- bc$x[which.max(bc$y)]
    my_data$xname1 <- my_data$x^max.point
    colnames(my_data) <- c(xname, g1name, xname1)
  }
  else
  {
    my_data<-as.data.table(data.frame(x,g1,g2))
    bc<-MASS::boxcox(x ~ g1 + g2, data = my_data,
                     lambda = seq(lb, ub, bk), plotit = plotit)
    max.point <- bc$x[which.max(bc$y)]
    my_data$xname1 <- my_data$x^max.point
    colnames(my_data) <- c(xname, g1name, g2name, xname1)
  }

  return(my_data)
}


