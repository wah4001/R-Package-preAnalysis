## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- echo = FALSE, include = FALSE--------------------------------------
library(sumcomp)

## ---- echo=FALSE, results='asis'-----------------------------------------
poisons <- boot::poisons
knitr::kable(head(poisons, 10), align='c')

## ----echo=FALSE----------------------------------------------------------
poison_intro <- rbind(cbind('Variable Name', 'Relationship','Variable Type', 'Description'),
                      cbind('time', 'Response', 'Continuous', 'Survival time of the animal in units of 10 hours'),
                      cbind('poison', 'Explanatory', 'Factor', 'Factor with levels 1, 2 and 3 giving the type of poison used'),
                      cbind('treat', 'Explanatory', 'Factor', 'Factor with levels A, B, C and D giving the treatment')) %>%
  kableExtra::kable(align = 'c') %>%
  kableExtra::kable_styling(full_width = F) %>%
  kableExtra::column_spec(1, width = 13, bold = T, border_right = T) %>%
  kableExtra::column_spec(2, width = 20, background = "lightyellow") %>%
  kableExtra::column_spec(3, width = 13, background = "white") %>%
  kableExtra::column_spec(4, width = 20, background = "lightyellow") %>%
  kableExtra::row_spec(1, bold = T) 
poison_intro

## ------------------------------------------------------------------------
groupsum(poisons$time, poisons$poison) %>%
  kableExtra::kable(align = 'c') %>%
  kableExtra::kable_styling(full_width = F)

## ------------------------------------------------------------------------
groupsum(poisons$time, poisons$poison, poisons$treat) %>%
  kableExtra::kable(align = 'c') %>%
  kableExtra::kable_styling(full_width = F)

## ------------------------------------------------------------------------
assumption.check(poisons$time, poisons$poison)[1] %>%  
  kableExtra::kable(align = 'c') %>%
  kableExtra::kable_styling(full_width = F)

## ----fig.height=4.5, fig.width=7-----------------------------------------
assumption.check(poisons$time, poisons$poison)[2]

## ----fig.show = 'asis'---------------------------------------------------
poisons_trans <- boxcoxTrans(poisons$time, poisons$poison)
head(poisons_trans) %>%
  kableExtra::kable(align = 'c') %>%
  kableExtra::kable_styling(full_width = F)

## ------------------------------------------------------------------------
assumption.check(poisons_trans$time_trans, poisons_trans$poison)[1] %>% 
  kableExtra::kable(align = 'c') %>%
  kableExtra::kable_styling(full_width = F)

## ----fig.height=4.5, fig.width=7-----------------------------------------
assumption.check(poisons_trans$time_trans, poisons_trans$poison)[2] 

## ------------------------------------------------------------------------
assumption.check(poisons$time, poisons$poison, poisons$treat)[1]%>%
 kableExtra::kable(align = 'c') %>%
  kableExtra::kable_styling(full_width = F)

## ----fig.height=4.5, fig.width=7-----------------------------------------
assumption.check(poisons$time, poisons$poison, poisons$treat)[2]

## ------------------------------------------------------------------------
poisons_trans_2<-boxcoxTrans(poisons$time, poisons$poison, poisons$treat)
head(poisons_trans_2) %>%
  kableExtra::kable(align = 'c') %>%
  kableExtra::kable_styling(full_width = F)

## ------------------------------------------------------------------------
assumption.check(poisons_trans_2$time_trans, poisons_trans_2$poison, poisons_trans_2$`poisons$treat`)[1] %>%
  kableExtra::kable(align = 'c') %>%
  kableExtra::kable_styling(full_width = F)

## ----fig.height=4.5, fig.width=7-----------------------------------------
assumption.check(poisons_trans_2$time_trans, poisons_trans_2$poison, poisons_trans_2$`poisons$treat`)[2]

## ------------------------------------------------------------------------
groupsum(poisons_trans_2$time_trans, poisons_trans_2$poison) %>%
  kableExtra::kable(align = 'c') %>%
  kableExtra::kable_styling(full_width = F)

## ------------------------------------------------------------------------
groupsum(poisons_trans_2$time_trans, poisons_trans_2$poison, poisons_trans_2$treat) %>%
  kableExtra::kable(align = 'c') %>%
  kableExtra::kable_styling(full_width = F)

## ------------------------------------------------------------------------
p.adj.compare(poisons$time, poisons$poison, compareall = T) %>%
  kableExtra::kable(align = 'c') %>%
  kableExtra::kable_styling(full_width = F)

## ------------------------------------------------------------------------
p.adj.compare(poisons_trans_2$time_trans, poisons_trans_2$poison, compareal=T) %>%
  kableExtra::kable(align = 'c') %>%
  kableExtra::kable_styling(full_width = F)

## ------------------------------------------------------------------------
p.adj.compare(poisons_trans_2$time_trans, poisons_trans_2$treat, compareal=T) %>%
  kableExtra::kable(align = 'c') %>%
  kableExtra::kable_styling(full_width = F)

## ----fig.height = 4.5, fig.width = 7, fig.show = 'asis'------------------
boxcomp(poisons$time, poisons$poison, normal = T, xname = "Poison Type", 
        xlabels = c("Type 1", "Type 2", "Type 3"), xlabelfont = 'Times New Roman', 
        yname = "Time", fillcol1 = c('coral','coral1','coral2'), edgecol = 'coral4', 
        titlesize = 14, main = "BOXPLOT of ANIMAL SURVIVAL TIMES", 
        caption = "*Data from 'boot' library in R")

## ----fig.height = 4.5, fig.width = 7, fig.show = 'asis'------------------
boxcomp(poisons$time, poisons$poison, poisons$treat, normal = T, xname = "Poison Type", 
        yname = "Time", xlabels = c("Type 1", "Type 2", "Type 3"), 
        xlabelfont = 'Times New Roman', fillcol2 = c('coral','coral1','coral2','coral3'), 
        edgecol ='coral4', main = "BOXPLOT of ANIMAL SURVIVAL TIMES", titlesize = 14, 
        legname = "Treatment Type", subtitle = 'Poison Type versus Treatment Type', 
        subcol = 'grey48', caption="*Data from 'boot' library in R", globalp_pos = 1.3, 
        pvalpos = 0.1, pointpos = 0.76)

## ----fig.height = 4.5, fig.width = 7, fig.show = 'asis'------------------
barchart(poisons$time, poisons$poison, normal = T, xname = "Poison Type", 
         xlabels = c("Type 1", "Type 2", "Type 3"), xlabelfont = 'Times New Roman', 
         yname = "Time", fillcol1 = c('coral','coral1','coral2'), edgecol ='coral4', 
         titlesize = 14, main = "BARCHART of ANIMAL SURVIVAL TIMES", 
         caption="*Data from 'boot' library in R", globalp_pos = 1)

## ----fig.height = 4.5, fig.width = 7, fig.show = 'asis'------------------
barchart(poisons$time, poisons$poison, poisons$treat, normal = T, xname = "Poison Type", 
         xlabels = c("Type 1", "Type 2", "Type 3"), xlabelfont = 'Times New Roman', 
         yname = "Time", fillcol2 = c('coral','coral1','coral2', 'coral3'), 
         edgecol ='coral4', titlesize = 14, main = "BARCHART of ANIMAL SURVIVAL TIMES", 
         legname = "Treatment Type", subtitle = 'Poison Type versus Treatment Type', 
         subcol = 'grey48', caption="*Data from 'boot' library in R", pvalpos = -0.05)

## ----echo = FALSE--------------------------------------------------------
Week1 = c(40,43,37,35,45)
Week12 =c(42,36,37,30,35)
data.frame(PairNum=rep(1:5), Week1) %>%
  kableExtra::kable(align = 'c') %>%
  kableExtra::kable_styling(full_width = F)
data.frame(PairNum=rep(1:5), Week12) %>%
  kableExtra::kable(align = 'c') %>%
  kableExtra::kable_styling(full_width = F)

## ----fig.height = 4.5, fig.width = 7, fig.show = 'asis'------------------
pairplot.2(Week1, Week12, linecol = c('lightblue','deepskyblue2','deepskyblue4',
           'blue','darkblue'), main = "PAIRED PLOT OF FOOD INTAKE", 
           titlesize = 17, titlecol = 'dodgerblue4', xname = 'WEEK', 
           xlabels = c('1','12'), yname = 'FOOD INTAKE (gm)', ynamesize = 15, 
           subtitle = 'Paired graph of two sample t test', subsize = 13, 
           subface = 'bold', subfont = 'Times New Roman', subcol = 'grey48', 
           capsize = 9, capfont = 'Times New Roman', legname = 'Pair ID', 
           leglab = c('Pair 1','Pair 2','Pair 3','Pair 4','Pair 5'),
           caption = 'Note: Data from WCM Spring 2019 - Foundation of Biomedical Application', 
           globalp_pos = 48, psize = 6)

