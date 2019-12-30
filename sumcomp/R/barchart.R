#' A Bar Chart Function
#' @title Bar chart visualiztion of the data summary
#' @name barchart
#' @usage barchart(y, x1, x2 = NULL, normal = T, edgecol = 1,
#'                fillcol1 = rep(5:(length(unique(x1)) + 4)), 
#'                fillcol2 = rep(5:(length(unique(x2)) + 4)), 
#'                main = "BAR CHART", titlesize = 13, titleface = "bold", 
#'                titlefont = 'Times New Roman', titlecol = 1, 
#'                titlehjust = 0.5, subtitle = NULL, subsize = NULL,
#'                subface = NULL, subfont = NULL, subcol = 1, 
#'                subhjust = 0.5, caption = NULL, capsize = NULL, 
#'                capfont = NULL, capcol = "gray48", caphjust = 0, 
#'                legname = NULL, leglab = unique(x2), legsize = 11,
#'                legcol = 1, legface = NULL, legfont = NULL, legtxsize = 10,
#'                legtxcol = 1, legtxface = NULL, legtxfont = NULL, 
#'                legpos = "right", psize = 4, pvalpos = min(y, na.rm = T), 
#'                pfont = 'Times New Roman',globalp_pos = max(y, na.rm = T), 
#'                na.rm = T, xname = NULL, xnamesize = 12, xnamecol = 1, 
#'                xnameface = 'bold', xnamefont = 'Times New Roman', 
#'                xlabels = unique(x1), xlabelsize = 10, xlabelcol = "grey48", 
#'                xlabelface = NULL, xlabelfont = 'Arial', yname = NULL, 
#'                ynamesize = 12, ynamecol = 1, ynameface = 'bold', 
#'                ynamefont = 'Times New Roman', ylabelsize = NULL, 
#'                ylabelcol = "grey48", ylabelface = NULL, ylabelfont = 'Arial', .
#'                na.rm = T, ...)
#' @description Summarize continuous response variable by categorical explanatory variable(s) with bar chart(s)
#' @param y Response variable required to be analyzed
#' @param x1 Required explanatory variable (first predictor)
#' @param x2 Optional explanatory variable (second predictor)
#' @param normal ANOVA test will be procesed if TRUE or Kruskal-Waills test will be processed if FALSE
#' @param edgecol The egde color of the boxplots (default is black)
#' @param fillcol1 The fill-in color of the boxplots when SINGLE predictor (default is white)
#' @param fillcol2 The fill-in color of the boxplots when TWO predictors (default depends on the total number of levels)
#' @param main Title of the boxplot
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
#' @param legname Name of legend when TWO predictors
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
#' @param pvalpos Position of p-values for TWO predictors (default is at the bottom)
#' @param globalp_pos Position of global p-value
#' @param na.rm Whether to include missing values (default is TRUE)
#' @param ... Further arguments to be passed to or from methods.
#' @details The barchart function return the barchart from ggplot2. Response variable should be continuous. 
#'          Predictor(s) can be ONE or TWO, but all required to be categorical variable(s). Levels more than 
#'          10 are not recommended for this function in order to have good visualization.
#' @keywords barchart summary comparison
#' @export
#' @example
#' barchart(poisons$time, poisons$poison, poisons$treat, normal = F, 
#'          xname = "Poison Type", labels = c("Baseline","Medium Point",
#'          "Endpoint"), yname = "Time", fillcol2 = c('coral','coral1',
#'          'coral2','coral3'), edgecol ='coral4', titlesize = 14, 
#'          main = "BARCHART of ANIMAL SURVIVAL TIMES", legname = "Treatment Type",
#'          subtitle = 'Poison Type versus Treatment Type', subcol = 'grey48',
#'          caption="*Data from 'boot' library in R", pvalpos = 1.3)

barchart <- function(y, x1, x2 = NULL, normal = T, edgecol = 1,
                     fillcol1 = rep(5:(length(unique(x1)) + 4)), 
                     fillcol2 = rep(5:(length(unique(x2)) + 4)), 
                     main = "BAR CHART", titlesize = 13, titleface = 'bold', 
                     titlefont = 'Times New Roman', titlecol = 1, titlehjust = 0.5, 
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
                     psize = 4, pvalpos = min(y, na.rm = T), pfont = 'Times New Roman', 
                     globalp_pos = max(y, na.rm = T), na.rm = T, ...)
{
  
  pcheck <- function(p)
  {
    if (p < 1e-04) pval <- "<0.0001"
    else pval <- paste("=", format(p, digits = 4))
    return(pval)
  }
  
  is.valid <- function(n) 
  {
    is.null(shiny::need(n, message = FALSE))  
  }
  
  if (is.null(x2))
  {
    if (normal == T) p <- anova(lm(formula = y ~ x1, ...))[1, ]$"Pr(>F)"
    else p <- kruskal.test(..., formula = y ~ as.factor(x1))$p.value
    p_val_1 <- pcheck(p)
  } else
  {
    p <- c()
    p_val_2 <- c()
    dat_1 <- data.frame(y, x1, x2)
    if (normal == T)
    {
      glo_p <- anova(lm(formula = y ~ x1 + x2 + x1 * x2, ...))[1,]$"Pr(>F)"
      for (i in unique(x1))
      {
        dat_2 <- filter(dat_1, x1 == i)
        p[i] <- anova(lm(y ~ as.factor(x2), dat_2))[1, ]$"Pr(>F)"
      }
      p2 <- unname(p)
      p_val_1 <- pcheck(glo_p)
      p_val_2 <- sapply(p2, pcheck)
    } else
    {
      p_val_1 <- NULL
      for (i in unique(x1))
      {
        dat_2 <- filter(dat_1, x1 == i)
        p[i] <- kruskal.test(y ~ as.factor(x2), dat_2)$p.value
      }
      p2 <- unname(p)
      p_val_2 <- sapply(p2, pcheck)
    }
  }
  
  ynam<-deparse(substitute(y))
  xnam<-deparse(substitute(x1))
  legnam<-deparse(substitute(x2))
  
  if(!is.valid(yname)) yname<-strsplit(ynam, split = "\\$")[[1]][2]
  if(!is.valid(xname)) xname<-strsplit(xnam, split = "\\$")[[1]][2]
  if(!is.valid(legname)) legname<-strsplit(legnam, split = "\\$")[[1]][2]
  
  if (is.null(x2))
  {
    dat <- data.frame(y, x1)
    sum_dat <- dat %>%
      group_by(x1) %>%
      summarise(mean = mean(y, na.rm = T), sds = sd(y, na.rm = T))
    plot <- ggplot(sum_dat, aes(x = x1, y = mean)) +
      geom_bar(fill = fillcol1, color = edgecol, stat = "identity", position = "dodge")
  } else
  {
    dat <- data.frame(y, x1, x2)
    sum_dat <- dat %>%
      group_by(x1, x2) %>%
      summarise(mean = mean(y,na.rm = T), sds = sd(y, na.rm = T))
    plot <- ggplot(sum_dat, aes(x = x1, y = mean, fill = x2)) +
      geom_bar(color = edgecol, stat = "identity", position = "dodge") +
      scale_fill_manual(legname, labels=leglab, values = fillcol2) +
      theme(legend.title=element_text(size=legsize, color=legcol, 
                                      face=legface, family=legfont),
            legend.text=element_text(size=legtxsize, color=legtxcol, 
                                     face=legtxface, family=legtxfont),
            legend.position=legpos)
    x1 <- factor(x1, levels = c(names(p)), ordered = T)
    labcomb <- c()
    for (i in p_val_2) labcomb <- paste("p", p_val_2)
    for (i in 1:length(unique(p_val_2)))
    {
      plot <- plot +
        annotate(geom = "text", x = i, y = pvalpos, label = labcomb[i], size = psize)
    }
  }
  plot <- plot + geom_errorbar(aes(ymin = mean - sds, ymax = mean + sds), 
                               position = position_dodge(0.9), width = 0.1, color = 'grey48') +
    scale_y_continuous(yname) + theme_bw() +
    labs(title = main, subtitle = subtitle, caption = caption) +
    scale_x_discrete(name = xname, label = xlabels) +
    theme(plot.title = element_text(size = titlesize, face = titleface, 
                                    family = titlefont, color = titlecol, hjust = titlehjust),
          plot.subtitle = element_text(size = subsize, face = subface, 
                                       family = subfont, color = subcol, hjust = subhjust),
          plot.caption = element_text(size = capsize, family = capfont, color = 
                                        capcol,hjust = caphjust)) +
    theme(axis.text.x = element_text(size = xlabelsize, color = xlabelcol, 
                                     face = xlabelface, family = xlabelfont), 
          axis.title.x = element_text(size = xnamesize, color = xnamecol, 
                                      face = xnameface, family = xnamefont),
          axis.text.y = element_text(size = ylabelsize, color = ylabelcol, 
                                     face = ylabelface, family = ylabelfont), 
          axis.title.y = element_text(size = ynamesize, color = ynamecol, 
                                      face = ynameface, family = ynamefont)) 
  
  if (is.null(p_val_1)) plot <- plot
  else plot <- plot + annotate(geom = "text", x = length(unique(x1)), y = globalp_pos,
                               label = paste("Global p", p_val_1), size = psize, family = pfont)
  
  return(plot)
}
