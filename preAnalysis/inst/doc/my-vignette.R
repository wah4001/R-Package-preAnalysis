## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- echo = FALSE, include = FALSE--------------------------------------
library(preAnalysis)

## ------------------------------------------------------------------------
samplesize <- sampleSize(E.start = 0.1, power.start = 0.8, E.end = NULL, power.end = NULL,  sig.level = 0.05, alternative = "two.sided")
samplesize %>%
  kableExtra::kable(align = 'c') %>%
  kableExtra::kable_styling(full_width = F)

## ------------------------------------------------------------------------
lab2003 <- nhanes('L11PSA_C')
demo2003 <- nhanes('DEMO_C')
lab2005 <- nhanes('PSA_D')
demo2005<- nhanes('DEMO_D')
head(lab2003, n=6) %>%
  kableExtra::kable(align = 'c') %>%
  kableExtra::kable_styling(full_width = F)
head(demo2003, n=6) %>%
  kableExtra::kable(align = 'c') %>%
  kableExtra::kable_styling(full_width = F)
head(lab2005, n=6) %>%
  kableExtra::kable(align = 'c') %>%
  kableExtra::kable_styling(full_width = F)
head(demo2005, n=6) %>%
  kableExtra::kable(align = 'c') %>%
  kableExtra::kable_styling(full_width = F)

## ----message=FALSE, warning=FALSE, paged.print=FALSE---------------------
merged <- showInfo(L11PSA_C, DEMO_C, PSA_D, DEMO_D)
head(merged) %>%
  kableExtra::kable(align = 'c') %>%
  kableExtra::kable_styling(full_width = F) %>%
  kableExtra::column_spec(1, width = 13, bold = T, border_right = T, background = "lightyellow") %>%
  kableExtra::column_spec(2, width = 20) %>%
  kableExtra::column_spec(3, width = 13) %>%
  kableExtra::column_spec(4, width = 20)

## ----message=FALSE, warning=FALSE----------------------------------------
nhanesTablesInfo(merged, detail = T, showfactor = 10)[[1]] %>%
  kableExtra::kable(align = 'l') %>%
  kableExtra::kable_styling(full_width = F) %>%
  kableExtra::column_spec(1, width = 12, bold = T, border_right = T, background = "lightyellow") %>%
  kableExtra::column_spec(2, width = 20) %>%
  kableExtra::column_spec(3, width = 12) %>%
  kableExtra::column_spec(4, width = 20) %>%
  kableExtra::column_spec(5, width = 20)

## ----message=FALSE, warning=FALSE, fig.height = 4.5, fig.width = 7, fig.show = 'asis'----
merged1 <- showInfo(L11PSA_C)
relationGraph(merged1, alpha = 0.01, graph = T, show.p = T, edge.curved = 0.5, vertex.label.cex = 0.8, vertex.shape = 'circle', vertex.size = 25, edge.width = 1.5, margin = 0.05)

## ----message=FALSE, warning=FALSE, fig.height = 4.5, fig.width = 7, fig.show = 'asis'----
tableTwo(formula = INDFMPIR ~ RIAGENDR+ DMDBORN + RIDAGEMN, table = merged, Multi=T) %>%
  kableExtra::kable(align = 'l') %>%
  kableExtra::kable_styling(full_width = F)
reuslt<-tableTwo(formula = RIAGENDR ~ INDFMPIR + DMDBORN + RIDAGEMN, table = merged, Multi=F) 
reuslt[[1]]%>%
  kableExtra::kable(align = 'l') %>%
  kableExtra::kable_styling(full_width = F)
reuslt[[2]]

