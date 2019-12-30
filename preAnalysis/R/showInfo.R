#' Get a merged NHANCE table with detail
#' @description This function download table from NHANCE website with character formate like YES/NO and Female/Male
#' instead of 0, 1. Moreover, it can download and merge mutile tables at the same time. But users need to give the
#' right table names(which can get by function in nhanesA and NHANES offical website)
#' @param tablenames The name of the specific table to retrieve. (Multiple tables are allowed)
#' @usage showInfo(...)
#' @return The table is returned as a data frame.
#' @keywords NHANES
#' @import nhanesA
#' @examples
#' \dontrun{
#' showInfo(DEMO_C)
#' showInfo(L11PSA_C, DEMO_C, PSA_D, DEMO_D)
#' }
#' @export
#'
showInfo<-function(...){

  #Get all the labels
  getlables <- function(x){
    allables<-NULL
    for(i in seq_along(x)){
      allables<-c(allables, sapply(x[i], attr, which = "label", exact = TRUE))
    }
    return(allables)
  }

  # add lables to table
  labelcol<-function(data, labe){
    name1<-names(data)
    name2<-names(labe)
    newlabels<-unname(labe)
    Hmisc::label(data) = as.list(newlabels[match(name1,name2)])
    return(data)
  }

  #Get table with label
  namednhance<-function(nh_table){
    # get table
    table <- nhanes(nh_table)
    var_name <- names(table)

    #get the labels from the table
    getlabel<-getlables(table)
    namelabel<-names(getlabel)
    newlabels<-unname(getlabel)

    #Transformed data
    trans <- nhanesTranslate(nh_table, var_name, data=table, details=T)
    Hmisc::label(trans) = as.list(newlabels[match(var_name,namelabel)])
    return(trans)
  }

  # Merge two table with lables
  mergelabel<-function(tableone,tabletwo){
    newtable <- merge(tableone, tabletwo, all=TRUE)
    numcol <- ncol(newtable)
    colnam <- names(newtable)
    namesall <- c(getlables(tableone),getlables(tabletwo))
    uniquename <- namesall[which(!duplicated(namesall))]
    for(i in seq_along(colnam)){
      for(j in seq_along(uniquename)){
        if(colnam[i]==names(uniquename)[j]){
          attr(newtable[[i]], "label") <- unname(uniquename[j])
        }
      }
    }
    return(newtable)
  }


  # Merge multipule table once
  mergeall<-function(tablelist){
    tableone <-mergelabel(tablelist[[1]],tablelist[[2]])
    for(i in seq_along(tablelist)[c(-1,-2)]){
      tableone<-mergelabel(tableone,tablelist[[i]])
    }
    return(tableone)
  }

  result <- deparse(substitute(list(...)))
  result <- unlist(strsplit(result, split='list(', fixed=TRUE))[2]
  result <- unlist(strsplit(result, split=')', fixed=TRUE))[1]
  result <- unlist(strsplit(result, split=',', fixed=TRUE))
  result <- gsub(" ", "", result)
  myList <- NULL
  for(i in seq_along(result)){
    myList <- c(myList, list(result[i]))
  }
  tablelist<-lapply(myList,namednhance)
  if(length(myList)>1){
    alltable<-mergeall(tablelist)
    return(alltable)
  }else{
    return(tablelist[[1]])
  }
}
