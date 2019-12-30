#' Boxplot and Barchart
#' @title Boxplot and Barchart visualization of the data summary
#' @name barboxGraph
#' @usage barboxGraph(y, x1, x2 = NULL, barchart = T, boxplot = T, normal = T,
#'                    edgecol = 1, fillcol1 = c(5:(length(unique(x1)) + 4)), 
#'                    fillcol2 = c(5:(length(unique(x2)) + 4)), 
#'                    boxmain = "BOXPLOT", barmain = 'BARCHART', titlesize = 13, 
#'                    titleface = 'bold', titlefont = 'Times New Roman', titlecol = 1, 
#'                    titlehjust = 0.5, subtitle = NULL, subsize = 10, subface = NULL, 
#'                    subfont = 'Times New Roman', subcol = 1, subhjust = 0.5, 
#'                    caption = NULL, capsize = 9, capfont = 'Times New Roman', 
#'                    capcol = "grey48", caphjust = 0, legname = NULL, 
#'                    legsize = 15, legcol = 1, legface = 'bold', 
#'                    legfont = 'Times New Roman', leglab = unique(x2), 
#'                    legtxsize = 9, legtxcol = 1, legtxface = NULL, 
#'                    legtxfont = 'Times New Roman', legpos = "right", 
#'                    xname = NULL, xnamesize = 12, xnamecol = 1, 
#'                    xnameface = 'bold', xnamefont = 'Times New Roman', 
#'                    xlabels = unique(x1), xlabelsize = 10, xlabelcol = "grey48", 
#'                    xlabelface = NULL, xlabelfont = 'Arial', yname = NULL, 
#'                    ynamesize = 12, ynamecol = 1, ynameface = 'bold', 
#'                    ynamefont = 'Times New Roman', ylabelsize = NULL, 
#'                    ylabelcol = "grey48", ylabelface = NULL, ylabelfont = 'Arial', 
#'                    pvalpos = min(y, na.rm = T), pfont = 'Times New Roman', 
#'                    psize = 4, globalp_pos = max(y, na.rm = T), 
#'                    pointcol = 'darkred', pointsize = 3, pointpos = 1, na.rm = T, ...)
#' @description Summarize continuous response variable by categorical explanatory variable(s) with boxplot and barchart
#' @param y Response variable required to be analyzed
#' @param x1 Required explanatory variable (first predictor)
#' @param x2 Optional explanatory variable (second predictor)
#' @param barchart To display barchart or not
#' @param boxplot To display boxplot or not
#' @param normal ANOVA test will be procesed if TRUE or Kruskal-Waills test will be processed if FALSE
#' @param edgecol The egde color of the boxplots (default is black)
#' @param fillcol1 The fill-in color of the boxplots when SINGLE predictor (default is white)
#' @param fillcol2 The fill-in color of the boxplots when TWO predictors (default depends on the total number of levels)
#' @param boxmain Title of the boxplot
#' @param barmain Title of the barchart
#' @param titlesize Font size of title
#' @param titleface Font style of title (e.g. 'bold')
#' @param titlefont Font of title (e.g. 'Times New Roman')
#' @param titlecol Color of title (default is black)
#' @param titlehjust Position of title (default is middle)
#' @param subtitle Subtitle of the boxplot
#' @param subsize Font size of subtitle
#' @param subface Font style of the subtitle
#' @param subfont Font of subtitle
#' @param subcol Color of subtitle (default is black)
#' @param subhjust Position of subtitle (default is middle)
#' @param caption Caption of the boxplot
#' @param capsize Font size of caption
#' @param capfont Font of caption
#' @param capcol Color of caption (default is 'gray48')
#' @param caphjust Position of caption (default is left)
#' @param legname Name of legend when 2 predictors
#' @param leglab Labels of each level in legend
#' @param legsize Font size of legend title
#' @param legcol Color of legend title (default is black)
#' @param legface Font style of legend title
#' @param legfont Font of legend title
#' @param legtxsize Font size of legend levels
#' @param legtxcol Font color of legend levels
#' @param legtxface Font style of legend levels
#' @param legtxfont Font of legend levels
#' @param legpos Position of legend (default is right)
#' @param xname Name of the variable on the x-asis
#' @param xnamesize Font size of the variable on the x-asis
#' @param xnamecol Font color of the variable on the x-asis
#' @param xnameface Font style of the variable on the x-asis
#' @param xnamefont Font of the variable on the x-asis
#' @param xlabels Labels of the levels on the x-asis
#' @param xlabelsize Font size of the levels on the x-asis
#' @param xlabelcol Font color of the levels on the x-asis
#' @param xlabelface Font style of the levels on the x-asis
#' @param xlabelfont Font of the levels on the x-asis
#' @param yname Name of the variable on the y-asis
#' @param ynamesize Font size of the variable on the y-asis
#' @param ynamecol Font color of the variable on the y-asis
#' @param ynameface Font style of the variable on the y-asis
#' @param ynamefont Font of the variable on the y-asis
#' @param ylabelsize Labels of the levels on the y-asis
#' @param ylabelcol Font color of the levels on the y-asis
#' @param ylabelface Font style of the levels on the y-asis
#' @param ylabelfont Font of the levels on the y-asis
#' @param psize Text size of p-value(s)
#' @param pfont Font of p-values
#' @param pvalpos Position of p-values for 2 predictors (default is at the bottom)
#' @param globalp_pos Position of global p-value
#' @param pointcol The color of mean point for each levels
#' @param pointsize The size of mean point for each levels
#' @param pointpos The position of mean point for each levels
#' @param na.rm Whether to include missing values (default is TRUE)
#' @param ... Further arguments to be passed to or from methods.
#' @details The boxbarGraph function return the boxplot and barchart from ggplot2. 
#'          Response variable should be continuous. Predictor(s) can be ONE or TWO, but 
#'          all required to be categorical variable(s). Levels more than 5 are not 
#'          recommended for this function in order to have good visualization. Cleaned
#'          data are welcome to perform on this function. Uncleaned data are also
#'          able to perform on this function, however, data that are not qualified for
#'          doing tests will stop the function with error messages. It is the upgrade
#'          version which combines `boxcomp` and `barchart` functions together from 
#'          `sumcomp` package (Yu and He, 2019).
#' @keywords barchart boxplot summary comparison ggplot2 sumcomp
#' @export
#' @examples

barboxGraph <- function(y, x1, x2 = NULL, barchart = T, boxplot = T, normal = T, edgecol = 1, 
                        fillcol1 = c(5:(length(unique(x1)) + 4)), 
                        fillcol2 = c(5:(length(unique(x2)) + 4)), 
                        boxmain = "BOXPLOT", barmain = 'BARCHART', titlesize = 13, 
                        titleface = 'bold', titlefont = 'Times New Roman', 
                        titlecol = 1, titlehjust = 0.5, 
                        subtitle = NULL, subsize = 10, subface = NULL, 
                        subfont = 'Times New Roman', subcol = 1, subhjust = 0.5, 
                        caption = NULL, capsize = 9, capfont = 'Times New Roman', 
                        capcol = "grey48", caphjust = 0, legname = NULL, 
                        legsize = 15, legcol = 1, legface = 'bold', 
                        legfont = 'Times New Roman', leglab = unique(x2), 
                        legtxsize = 9, legtxcol = 1, legtxface = NULL, 
                        legtxfont = 'Times New Roman', legpos = "right", 
                        xname = NULL, xnamesize = 12, xnamecol = 1, 
                        xnameface = 'bold', xnamefont = 'Times New Roman', 
                        xlabels = unique(x1), xlabelsize = 10, xlabelcol = "grey48", 
                        xlabelface = NULL, xlabelfont = 'Arial', yname = NULL, 
                        ynamesize = 12, ynamecol = 1, ynameface = 'bold', 
                        ynamefont = 'Times New Roman', ylabelsize = NULL, 
                        ylabelcol = "grey48", ylabelface = NULL, ylabelfont = 'Arial', 
                        pvalpos = min(y, na.rm = T), pfont = 'Times New Roman', 
                        psize = 4, globalp_pos = max(y, na.rm = T), 
                        pointcol = 'darkred', pointsize = 3, pointpos = 1, na.rm = T, ...)
{
  
  pcheck <- function(p)
  {
    if (p < 1e-04) pval <- "<0.0001"
    else pval <- paste("=", round(p,4))
    return(pval)
  }
  
  is.valid <- function(n) 
  {
    require(shiny)
    is.null(need(n, message = FALSE))  
  }
  
  nlvl1 <- length(t(unique(x1)))
  
  if (is.null(x2)) {
    if (nlvl1 > 1) {
      if (normal == TRUE) p <- anova(lm(formula = y ~ x1, ...))[1, ]$"Pr(>F)"
      else p <- kruskal.test(..., formula = y ~ as.factor(x1))$p.value
      p_val_1 <- pcheck(p)
    } else {
      stop("Levels of x1 should be at least 2 or more")
    }
    
  } else {
    
    nlvl2 <- length(t(unique(x2)))
    
    if (nlvl1 > 1 & nlvl2 > 1){
      x1_len <- c()
      for (i in 1:length(unique(x1))){
        x1_len[i] <- sum(x1 == unique(x1)[i])
      }
      
      x2_len <- c()
      for (i in 1:length(unique(x2))){
        x2_len[i] <- sum(x2 == unique(x2)[i])
      }
      
      for (i in (length(x1_len)-1)){
        for (j in (i+1):length(x1_len)){
          prop1 <- abs((x1_len[i]-x1_len[j])/length(x1))
          if (prop1 > 0.95) stop("Number of observations between groups are unbalanced")
        }
      }
      
      for (i in (length(x2_len)-1)){
        for (j in (i+1):length(x2_len)){
          prop2 <- abs((x2_len[i]-x2_len[j])/length(x2))
          if (prop2 > 0.95) stop("Number of observations between groups are unbalanced")
        }
      }
      
    }else{
      stop("Levels of explanatory variables should be at least 2 or more")
    }
    
    nrows1 <- length(x1)
    nrows2 <- length(x2)
    
    if (nrows1 > 10 & nrows2 > 10){
      p <- c()
      p_val_2 <- c()
      dat_1 <- data.frame(y, x1, x2)
      
      if (normal == TRUE){
        
        glo_p <- anova(lm(formula = y ~ x1 * x2, dat_1))[1,]$"Pr(>F)"
        
        for (i in unique(x1)){
          dat_2 <- filter(dat_1, x1 == i)
          if (length(t(unique(dat_2$x2))) < 2){
            stop("Levels of x2 should be at least 2 or more")
          }else{
            p[i] <- anova(lm(y ~ as.factor(x2), dat_2))[1,]$"Pr(>F)"
          }
        }
        
        p2 <- unname(p)
        p_val_1 <- pcheck(glo_p)
        p_val_2 <- sapply(p2, pcheck)
        
      } else {
        
        p_val_1 <- NULL
        
        for (i in unique(x1))
        {
          dat_2 <- filter(dat_1, x1 == i)
          if (length(t(unique(dat_2$x2))) < 2){
            stop("Levels of x2 should be at least 2 or more")
          }else{
            p[i] <- kruskal.test(y ~ as.factor(x2), dat_2)$p.value
          }
        }
        p2 <- unname(p)
        p_val_2 <- sapply(p2, pcheck)
      }
      
    }else{
      
      if(nlvl1 < 2 | nlvl2 < 2){
        stop("Levels of explanatory variables should be at least 2 or more")
      }else{ 
        stop("Observations in at least one group are not enough")
      }
    }
  }
  
  ynam <- deparse(substitute(y))
  xnam <- deparse(substitute(x1))
  legnam<-deparse(substitute(x2))
  
  if(!is.valid(yname)) yname <- strsplit(ynam, split = "\\$")[[1]][2]
  if(!is.valid(xname)) xname <- strsplit(xnam, split = "\\$")[[1]][2]
  if(!is.valid(legname)) legname <- strsplit(legnam, split = "\\$")[[1]][2]
  
  if (is.null(x2)) {
    
    plot_box <- ggplot(..., mapping = aes(x = x1, y = y)) + geom_boxplot(fill = fillcol1, color = edgecol)
    
    dat <- data.frame(y, x1)
    sum_dat <- dat %>%
      group_by(x1) %>%
      summarise(mean = mean(y, na.rm = T), sds = sd(y, na.rm = T))
    
    plot_bar <- ggplot(sum_dat, aes(x = x1, y = mean)) +
      geom_bar(fill = fillcol1, color = edgecol, stat = "identity", position = "dodge")
    
  }else{
    
    plot_box <- ggplot(..., mapping = aes(x = x1, y = y, fill = x2)) +
      geom_boxplot(color = edgecol) +
      scale_fill_manual(name = legname, labels = leglab, values = fillcol2) +
      theme(legend.title = element_text(size = legsize, color = legcol, face = legface, family = legfont),
            legend.text = element_text(color = legtxcol, size = legtxsize, face = legtxface, family = legtxfont),
            legend.position = legpos)
    
    x1 <- factor(x1, levels = c(names(p)), ordered = T)
    labcomb <- c()
    for (i in p_val_2) 
      labcomb <- paste("p", p_val_2)
    for (i in 1:length(unique(p_val_2))) 
      plot_box <- plot_box + annotate(geom = "text", x = i, y = pvalpos, label = labcomb[i], size = psize, family = pfont)
    
    dat <- data.frame(y, x1, x2)
    sum_dat <- dat %>%
      group_by(x1, x2) %>%
      summarise(mean = mean(y,na.rm = T), sds = sd(y, na.rm = T))
    
    plot_bar <- ggplot(sum_dat, aes(x = x1, y = mean, fill = x2)) +
      geom_bar(color = edgecol, stat = "identity", position = "dodge") +
      scale_fill_manual(legname, labels=leglab, values = fillcol2) +
      theme(legend.title=element_text(size=legsize, color=legcol, face=legface, family=legfont),
            legend.text=element_text(size=legtxsize, color=legtxcol, face=legtxface, family=legtxfont),
            legend.position=legpos)
    x1 <- factor(x1, levels = c(names(p)), ordered = T)
    labcomb <- c()
    for (i in p_val_2) labcomb <- paste("p", p_val_2)
    for (i in 1:length(unique(p_val_2)))
    {
      plot_bar <- plot_bar + annotate(geom = "text", x = i, y = pvalpos, label = labcomb[i], size = psize)
    }
  }
  
  plot_box <- plot_box + 
    theme_bw() + 
    stat_summary(fun.y = mean, colour = pointcol, geom ="point", shape = 18, 
                 size = pointsize, position = position_dodge2(pointpos)) + 
    scale_y_continuous(yname) +
    scale_x_discrete(name = xname, label = xlabels) +
    labs(title = boxmain, subtitle = subtitle, caption = caption) +
    theme(
      axis.text.x = element_text(size = xlabelsize, color = xlabelcol, face = xlabelface, family = xlabelfont), 
      axis.title.x = element_text(size = xnamesize, color = xnamecol, face = xnameface, family = xnamefont),
      axis.text.y = element_text(size = ylabelsize, color = ylabelcol, face = ylabelface, family = ylabelfont), 
      axis.title.y = element_text(size = ynamesize, color = ynamecol, face = ynameface, family = ynamefont)) +
    theme(plot.title = element_text(size = titlesize, face = titleface, family = titlefont, color = titlecol, hjust = titlehjust),
          plot.subtitle = element_text(size = subsize, face = subface, family = subfont, color = subcol, hjust = subhjust),
          plot.caption = element_text(size = capsize, family = capfont, color = capcol, hjust = caphjust))
  
  if (is.null(p_val_1)) {
    plot_box <- plot_box
  }else{ 
    plot_box <- plot_box + 
      annotate(geom = "text", x = length(unique(x1)), y = globalp_pos, 
               label = paste("Global p", p_val_1), size = psize, family = pfont)
    
    plot_bar <- plot_bar + geom_errorbar(aes(ymin = mean - sds, ymax = mean + sds), 
                                         position = position_dodge(0.9), width = 0.1, color = 'grey48') +
      scale_y_continuous(yname) + 
      theme_bw() +
      labs(title = barmain, subtitle = subtitle, caption = caption) +
      scale_x_discrete(name = xname, label = xlabels) +
      theme(plot.title = element_text(size = titlesize, face = titleface, family = titlefont, color = titlecol, hjust = titlehjust),
            plot.subtitle = element_text(size = subsize, face = subface, family = subfont, color = subcol, hjust = subhjust),
            plot.caption = element_text(size = capsize, family = capfont, color = capcol,hjust = caphjust)) +
      theme(axis.text.x = element_text(size = xlabelsize, color = xlabelcol, face = xlabelface, family = xlabelfont), 
            axis.title.x = element_text(size = xnamesize, color = xnamecol, face = xnameface, family = xnamefont),
            axis.text.y = element_text(size = ylabelsize, color = ylabelcol, face = ylabelface, family = ylabelfont), 
            axis.title.y = element_text(size = ynamesize, color = ynamecol, face = ynameface, family = ynamefont)) 
  }
  
  if (is.null(p_val_1)) plot_bar <- plot_bar
  else plot_bar <- plot_bar + annotate(geom = "text", x = length(unique(x1)), y = globalp_pos,
                                       label = paste("Global p", p_val_1), size = psize, family = pfont)
  
  if (boxplot == T & barchart == T) return(list(plot_bar,plot_box))
  else if (boxplot == T & barchart == F) return(plot_box)
  else if (boxplot == F & barchart == T) return(plot_bar)
  else return(message("nothing is shown since both table and plot are turned off"))
}
