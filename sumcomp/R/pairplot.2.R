#' A Two Sample Paired Function
#' @title Paired visualiztion of the data summary
#' @name pairplot.2
#' @usage pairplot.2(x1, x2, y = as.vector(cbind(x1, x2)), 
#'                   linecol = c(5:(length(unique(pair_id)) + 4)), 
#'                   main = "PAIRED PLOT", titlesize = 13, titleface = 'bold', 
#'                   titlefont = 'Times New Roman', titlecol = 1, titlehjust = 0.5, 
#'                   subtitle = NULL, subsize = 10, subface = NULL, 
#'                   subfont = 'Times New Roman', subcol = 1, subhjust = 0.5, 
#'                   caption = NULL, capsize = 9, capfont = 'Times New Roman', 
#'                   capcol = "grey48", caphjust = 0, xname = NULL, xnamesize = 12, 
#'                   xnamecol = 1, xnameface = 'bold', xnamefont = 'Times New Roman', 
#'                   xlabels = unique(x), xlabelsize = 10, xlabelcol = "grey48",
#'                   xlabelface = NULL, xlabelfont = 'Arial', yname = NULL, 
#'                   ynamesize = 12, ynamecol = 1, ynameface = 'bold',  yname = NULL,
#'                   ynamefont = 'Times New Roman', ylabelcol = "grey48", 
#'                   ylabelface = NULL, ylabelfont = 'Arial', legname = NULL, 
#'                   legcol = 1, legface = 'bold', legfont = 'Times New Roman', 
#'                   leglab = unique(pair_id), legtxsize = 9, legtxcol = 1, legsize = 15, 
#'                   legtxface = NULL, legtxfont = 'Times New Roman', legpos = "right", 
#'                   globalp_pos = max(y, na.rm = T), psize = 4, pfont = 'Times New Roman', 
#'                   na.rm = T, ...)
#' @description Summarize continuous response variable by categorical explanatory variable with two sample paired plot 
#' @param x1 First group values (paired to x2)
#' @param x2 Second group values (paired to x1)
#' @param linecol The line color
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
#' @param psize Text size of p-value
#' @param pfont Text font of p-value
#' @param globalp_pos The position of p-value
#' @param na.rm Whether to include missing values (default is TRUE)
#' @param ... Further arguments to be passed to or from methods.
#' @details The pairplot.2 function return the two-sample paired plot from ggplot2. Values from both groups 
#'          should be continuous. The sample sizes from two groups should be matched and the data in two groups 
#'          should be matched. The p-value comes from t.test with paired = T.
#' @keywords paired data summary t.test
#' @export
#' @examples
#' pairplot.2(week1, week12, linecol = c('lightblue','deepskyblue2','deepskyblue4',
#'            'blue','darkblue'), main = "PAIRED PLOT OF FOOD INTAKE", 
#'            titlesize = 17, titlecol = 'dodgerblue4', xname = 'WEEK', xnamesize = 14,
#'            xlabels = c('1','12'), yname = 'FOOD INTAKE (gm)', ynamesize = 14, 
#'            legname = 'Pair ID', leglab = c('Pair 1','Pair 2','Pair 3','Pair 4','Pair 5'),
#'            subtitle = 'Paired graph of two sample t test', subsize = 13, subface = 'bold', 
#'            subfont = 'Times New Roman', subcol = 'grey48', capsize = 9, capfont = 'Times New Roman', 
#'            caption = 'Note: Data from Angelo Bisazza (1978) Development of Aggressive Behaviourin 
#'            the Mouse (Mus Musculus):\nEffects of Different Enviromental Conditions, Bolletino di 
#'            zoologia, 45:4, 353-357,DOI: 10.1080/11250007809440142', globalp_pos = 48,
#'            psize = 5)

pairplot.2 <- function(x1, x2, y = as.vector(cbind(x1, x2)), 
                       linecol = c(5:(length(unique(pair_id)) + 4)), 
                       main = "PAIRED PLOT", titlesize = 13, titleface = 'bold', 
                       titlefont = 'Times New Roman', titlecol = 1, titlehjust = 0.5, 
                       subtitle = NULL, subsize = 10, subface = NULL, 
                       subfont = 'Times New Roman', subcol = 1, subhjust = 0.5, 
                       caption = NULL, capsize = 9, capfont = 'Times New Roman', 
                       capcol = "grey48", caphjust = 0, xname = NULL, xnamesize = 12, 
                       xnamecol = 1, xnameface = 'bold', xnamefont = 'Times New Roman', 
                       xlabels = unique(x), xlabelsize = 10, xlabelcol = "grey48", 
                       xlabelface = NULL, xlabelfont = 'Arial', yname = NULL, ynamesize = 12, 
                       ynamecol = 1, ynameface = 'bold', ynamefont = 'Times New Roman',
                       ylabelsize = NULL, ylabelcol = "grey48", ylabelface = NULL, 
                       ylabelfont = 'Arial', legname = NULL, legsize = 15, legcol = 1, 
                       legface = 'bold', legfont = 'Times New Roman', leglab = unique(pair_id), 
                       legtxsize = 9, legtxcol = 1, legtxface = NULL, legtxfont = 'Times New Roman', 
                       legpos = "right", psize = 4, pfont = 'Times New Roman', 
                       globalp_pos = max(y, na.rm = T), na.rm = T, ...){
  
  pair_id <- rep(1:length(unique(x1)),2)
  x <- c(rep('group 1',length(unique(x1))), rep('group 2',length(unique(x1))))
  data <- data.frame(pair_id, x, y = as.vector(cbind(x1, x2)))
  
  pcheck <- function(p)
  {
    if (p < 1e-04) pval <- "<0.0001"
    else pval <- paste("=", format(p, digits = 4))
    return(pval)
  }
  
  p <- pcheck(t.test(y ~ x, data, paired = T)$p.value)
  
  plot <- ggplot(data, mapping = aes(x = x, y = y)) + 
    geom_point(aes(group = pair_id, color = factor(pair_id))) +
    geom_line(aes(group = pair_id, color = factor(pair_id))) +
    theme_bw() + 
    scale_y_continuous(yname) +
    scale_x_discrete(name = xname, label = xlabels) +
    labs(title = main, subtitle = subtitle, caption = caption) +
    scale_color_manual(name = legname, values = linecol, label = leglab) +
    theme(
      legend.title = element_text(size = legsize, color = legcol, 
                                      face = legface, family = legfont),
      legend.text = element_text(color = legtxcol, size = legtxsize, 
                                     face = legtxface, family = legtxfont),
      legend.position = legpos) +
    theme(
      axis.text.x = element_text(size = xlabelsize, color = xlabelcol, 
                                 face = xlabelface, family = xlabelfont), 
      axis.title.x = element_text(size = xnamesize, color = xnamecol, 
                                  face = xnameface, family = xnamefont),
      axis.text.y = element_text(size = ylabelsize, color = ylabelcol, 
                                 face = ylabelface, family = ylabelfont), 
      axis.title.y = element_text(size = ynamesize, color = ynamecol, 
                                  face = ynameface, family = ynamefont)) +
    theme(
      plot.title = element_text(size = titlesize, face = titleface, 
                                family = titlefont, color = titlecol, hjust = titlehjust),
      plot.subtitle = element_text(size = subsize, face = subface, 
                                   family = subfont, color = subcol, hjust = subhjust),
      plot.caption = element_text(size = capsize, family = capfont, 
                                  color = capcol, hjust = caphjust)) +
    annotate(geom = "text", x = length(unique(x)), label = paste("p", p), 
             y = globalp_pos, size = psize, family = pfont)
  
  return(plot)
}
