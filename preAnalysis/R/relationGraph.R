#' Table and Graph of Relationships/Associations
#' @title Visualization of association between variables
#' @name relationGraph
#' @usage relationGraph(dataset, alpha = alpha, table = T, graph = T, show.p = F, vertex.size = 20, 
#'                      vertex.label.cex=1.3, vertex.label.font = 2, vertex.shape = "circle", 
#'                      edge.curved = 0, edge.width = 2)
#' @description Display the relationships or associations between each pairs of the variables. 
#' @param dataset Dataset going to be analyzed
#' @param alpha User-defined significance level
#' @param table Display relation table or not
#' @param graph Display relation graph or not
#' @param show.p Display p-values in the relation graph or not
#' @param vertex.size Vertex size
#' @param vertex.label.cex Vertex label size
#' @param vertex.label.font Vertex label font style (e.g. 'bold')
#' @param vertex.shape Vertex shape (e.g. 'circle', 'rectangle')
#' @param edge.curved Curvature of edge (1 = straight line)
#' @param edge.width Width of edges
#' @details The function provides a idea of the associations between each pairs of the variables in the given dataset. 
#'          A table of pairs that associations are significant at a user-defined statistical significance level and a 
#'          corresponding relation graph will be displayed. Significance level is user-defined. Only the associations 
#'          that has a p-value less than alpha will be displayed. Note that the graph might not be clear if there exists 
#'          too many associations. Dashed edges denote that the p-value of the comparisons are greater than 0.05 while 
#'          solid edges are less than 0.05. 
#' @export
#' @examples

relationGraph <- function(dataset, alpha = alpha, table = T, graph = F, show.p = F,
vertex.size = 20, vertex.label.cex=1.3, vertex.label.font = 2, margin = 0.02,
vertex.shape = "circle", edge.curved = 0, edge.width = 2){
    
    pcheck <- function(p)
    {
        if (p < 1e-03) pval <- "< 0.001"
        else pval <- paste("=", format(p, digits = 3))
        return(pval)
    }
    
    sepType <- function(data){
        
        facTable <- data.frame(data['SEQN'])
        intTable <- data.frame(data['SEQN'])
        
        for (i in 2:length(data)) {
            
            type <- sapply(data[i], class)
            type1 <- ifelse((length(type>=1)), tail(type, n=1), type)
            if (type1 == 'factor' | type1 == 'character')
            facTable <- data.frame(facTable, data[i])
            else
            intTable <- data.frame(intTable, data[i])
        }
        
        return(list(fac_dat = facTable, int_dat = intTable))
    }
    
    fac_dat <- sepType(dataset)$fac_dat
    int_dat <- sepType(dataset)$int_dat
    
    if ('KID221' %in% names(fac_dat)) {
        fac_dat <- fac_dat %>%
        select(., -KID221)
    }else{
        fac_dat <- fac_dat
    }
    
    if ('SDDSRVYR' %in% names(int_dat)) {
        int_dat <- int_dat %>%
        select(., -SDDSRVYR)
    }else{
        int_dat <- int_dat
    }
    
    catTest <- function(data){
        
        nvar <- length(data)
        if (nvar < 3) {
            p = 1
        }else{
            box <- data.frame(x1=character(), x2=character(),
            p.value = double(), Test = character(),
            stringsAsFactors = F)
            
            for (i in 2:(length(data)-1)) {
                for (j in (i+1):length(data)) {
                    
                    k = nrow(box) + 1
                    m = names(data)[i]
                    n = names(data)[j]
                    
                    df1 <- data.frame(data[i], data[j])
                    
                    df <- df1 %>%
                    filter(complete.cases(df1) == T)
                    
                    nlvl1 <- length(t(unique(df[1])))
                    nlvl2 <- length(t(unique(df[2])))
                    nrows <- nrow(df)
                    
                    if (nrow(df) > 6){
                        
                        if (nlvl1 > 1 & nlvl2 > 1) {
                            
                            expTable <- chisq.test(table(df))$exp
                            r <- nrow(expTable)
                            c <- ncol(expTable)
                            
                            if ((sum(expTable < 5) / (r*c)) < 0.2) {
                                p <- chisq.test(table(df))$p.value
                                t <- 'Chi-square Test'
                            }else{
                                p <- fisher.test(table(df), simulate.p.value=TRUE)$p.value
                                t <- "Fisher's Exact Test"
                            }
                        }else{
                            p = 1
                            t = 'Number of level = 1'
                        }
                    }else{
                        p = 1
                        t <- 'Observations are not enough for further analysis'
                    }
                    box[k,] <- c(m, n, p, t)
                }
            }
            box$p.value <- as.numeric(box$p.value)
            return(box)
        }
    }
    
    intTest <- function(data){
        
        nvar <- length(data)
        if (nvar < 3) {
            p = 1
        }else{
            box <- data.frame(x1=character(), x2=character(),
            p.value = double(), Test = character(),
            stringsAsFactors = F)
            
            for (i in 2:(length(data)-1)) {
                for (j in (i+1):length(data)) {
                    
                    k = nrow(box) + 1
                    m = names(data)[i]
                    n = names(data)[j]
                    
                    df1 <- data.frame(data[i], data[j])
                    
                    df <- df1 %>%
                    filter(complete.cases(df1) == T)
                    
                    if (nrow(df) < 2){
                        p = 1
                    }else{
                        p <- cor.test(df[[1]], df[[2]])$p.value
                        t <- 'Pearson Test'
                    }
                    box[k,] <- c(m, n, p, t)
                }
            }
            box$p.value <- as.numeric(box$p.value)
            return(box)
        }
    }
    
    intcatTest <- function(int_dat, fac_dat){
        
        facTable <- as.data.table(fac_dat)
        intTable <- as.data.table(int_dat)
        
        box <- data.frame(x1=character(), x2=character(),
        p.value = double(), Test = character(),
        stringsAsFactors = F)
        
        for (i in 2:length(int_dat)){
            for (j in 2:length(fac_dat)){
                
                k = nrow(box) + 1
                m = names(int_dat)[i]
                n = names(fac_dat)[j]
                
                resultTable <- cbind(int_dat[i], fac_dat[j])
                
                result <- resultTable %>%
                filter(complete.cases(resultTable) == T)
                
                if (nrow(result) < 10){
                    p = 1
                    
                }else{
                    
                    formula <- as.formula(paste(names(result)[1], "~", names(result)[2]))
                    nlvl <- length(t(unique(result[2])))
                    nrows <- nrow(result)
                    
                    if (nrows >= 10) {
                        
                        if (nlvl == 2) {
                            
                            if (nrows >= 10 & nrows < 5000){
                                
                                if (shapiro.test(unlist(result[1]))$p.value > 0.1){
                                    p <- t.test(formula, data = result)$p.value
                                    t <- 'T-test'
                                }else{
                                    p <- wilcox.test(formula, data = result)$p.value
                                    t <- 'Wilcoxon test'
                                }
                                
                            }else{
                                p <- t.test(formula, data = result)$p.value
                                t <- 'T-test'
                            }
                            
                        }else if(nlvl > 2){
                            
                            if (nrows < 5000){
                                
                                if (shapiro.test(unlist(result[1]))$p.value > 0.1){
                                    p <- anova(lm(formula, data = result))$`Pr(>F)`[1]
                                    t <- 'ANOVA'
                                }else{
                                    p <- kruskal.test(formula, data = result)$p.value
                                    t <- 'Kruskal-Wills test'
                                }
                                
                            }else{
                                p <- anova(lm(formula, data = result))$`Pr(>F)`[1]
                                t <- 'ANOVA'
                            }
                            
                        }else{
                            p = 1
                            t <- 'Number of level = 1'
                        }
                        
                    }else{
                        p = 1
                        t <- 'Observations are not enough for further analysis'
                    }
                    box[k,] <- c(m, n, p, t)
                }
            }
        }
        box$p.value <- as.numeric(box$p.value)
        return(box)
    }
    
    finalTable <- function(alpha, int_dat, fac_dat) {
        
        testTable <- rbind(intTest(int_dat), catTest(fac_dat), intcatTest(int_dat, fac_dat))
        colnames(testTable) <- c('varA','varB','p.value','Test')
        
        graphTable <- testTable %>%
        filter(testTable$p.value < alpha)
        graphTable$p.value <- round(graphTable$p.value,3)
        
        return(graphTable)
    }
    
    final <- finalTable(alpha, int_dat, fac_dat)
    
    p_value <- c()
    for (i in 1:length(final$p.value)) {
        p_value[i] <- pcheck(final$p.value[i])
    }
    
    edge.type <- c()
    for (i in 1:length(final$p.value)) {
        if (final$p.value[i] < 0.05) edge.type[i] <- 1
        else edge.type[i] <- 2
    }
    
    g <- graph_from_data_frame(final, directed=F)
    g <- set_graph_attr(g, "layout", layout_with_kk(g))
    comps <- components(g)$membership
    colbar <- rainbow(max(comps) + 6)
    V(g)$color <- colbar[comps + 1]
    
    if (show.p == T){
        plot <- plot(g, vertex.size = vertex.size, edge.width = edge.width,
        asp = 0.5, margin = margin, vertex.color = V(g)$color,
        vertex.label.cex = vertex.label.cex,
        vertex.label.font = vertex.label.font,
        vertex.shape = vertex.shape, edge.curved = edge.curved,
        edge.lty=edge.type, edge.label = paste('p',p_value),
        label.cex = 30)
    }else{
        plot <- plot(g, vertex.size = vertex.size, edge.width = edge.width,
        asp = 0.5, margin = margin, vertex.color = V(g)$color,
        vertex.label.cex = vertex.label.cex,
        vertex.label.font = vertex.label.font,
        vertex.shape = vertex.shape, edge.curved = edge.curved,
        edge.lty = edge.type)
    }
    
    if (table == T & graph == F)
    return(final)
    else if (table == F & graph == T)
    return(plot)
    else if (table == T & graph == T)
    return(list(final, plot))
    else
    return(message("nothing is shown since both table and plot are turned off"))
}
