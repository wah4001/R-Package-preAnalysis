#' @title Show a basic information of NHANCE table
#' @name nhanesTablesInfo
#' @description This function shows all the basic information for NHANSE table.
#' @usage nhanesTablesInfo(x, detail=FALSE, showfactor=3, noMedian = F, Hid.val1="", Hid.val2="")
#' @param x The name of the specific table to retrieve
#' @param detail Option to show proportion for factors and median(min, max) for integers
#' @param showfactor Maximum levels to show for one variable
#' @param noMedian Option to hid or show median(min, max) in detailed type table
#' @param Hid.val1 Option to hid some values
#' @param Hid.val2 Option to hid some values
#' @details This function shows all the basic information for NHANSE table. There are two types of
#'  choices to show information for a table. For the first type, this table shows the number of variables
#'  in the table, the meaning and types of the variables. In the Value column in the summary table, the
#'  SEQN will always show the range of respondent sequence number. For the categorical variables, the
#'  Value column only takes smaller than three levels variables which have less or equal to 25 characters
#'  in total, otherwise the Value column will show the number of different levels for one categorical
#'  variable. For the continuous variables, the Value column will show the mean and standard deviation.
#'  Last but not least, both types of table will show missing value and valid values. For the second type,
#'  it will not show the meaning of the variables in the table, the meaning of the variables will go to
#'  another list. However, the second type of tables can show more information like the proportion and
#'  median, minimum and maximum values.
#' @return The table is returned as a data frame with list.
#' @keywords summary
#' @examples
#' \dontrun{
#' nhanesTablesInfo(table, detail=F)
#' nhanesTablesInfo(table, detail=T, showfactor=15)
#' nhanesTablesInfo(table, detail=T, noMedian = T, Hid.val1="Yes", Hid.val2="No")
#' }
#' @export
nhanesTablesInfo<-function(x, detail=FALSE, showfactor=3, noMedium = FALSE, Hid.val1="", Hid.val2=""){
  if(is.data.frame(x)==FALSE){
    message("Invalid: Please enter a valid data frame.")
  }else{
    if(detail == FALSE){
      result <- data.frame(No = numeric(), Variable = character(), Type = character(), Label = character(),
                           Values=character(), Missing=character(), Valid=character(),
                           check.names = FALSE, stringsAsFactors = FALSE)
      for(i in seq_along(x)){
        coldata<-x[[i]]
        types<-class(coldata)
        typenul<-ifelse((length(types)>=1), tail(types, n=1), types)
        labelcol<-sapply(x[i], attr, which = "label", exact = TRUE)
        if(is.null(labelcol[[1]])) labelcol<-"missing label"
        result[i,1] <- i
        result[i,2] <- names(x)[i]
        result[i,3] <- typenul
        result[i,4] <- labelcol
        if(typenul!= "factor"){
          if(i==1){
            result[i,5]<-paste0("Range: ", min(coldata, na.rm = T)," - ", max(coldata, na.rm = T))
          }else{
            meansd<-paste0("Mean(sd): ",round(mean(coldata, na.rm=T),2),
                           "(",round(sd(coldata,na.rm=T),2),")")
            if(nchar(meansd)<=26){
              result[i,5] <- meansd
            }else{
              result[i,5] <-paste0("Mean: ",round(mean(coldata, na.rm=T),2))
            }
          }
        }else{
          stat<-unique(coldata[!is.na(coldata)])
          writestring <- paste0(seq_along(stat), ". ", stat, collapse = " ")
          if(length(stat)<=3 & nchar(writestring)<=26) {
            result[i,5] <- substr(writestring, 1, 26)
          }else{
            result[i,5]<- paste0(length(stat), " distinct values.")
          }
        }
        miss <- sum(is.na(coldata))
        result[i,6] <- paste0(miss," (",round(miss/length(coldata)*100,2),"%)")
        valid <- length(coldata)-miss
        result[i,7] <- paste0(valid," (",round(valid/length(coldata)*100,2),"%)")
      }
      result[is.na(result)] <- ''
      return(result)
    }else{
      result <- data.frame(Variable = character(), Values=character(),Overall=character(), Valid=character(),
                           Missing=character(),check.names = FALSE, stringsAsFactors = FALSE)
      alllables<-NULL
      track<-1
      for(i in seq_along(x)){
        coldata <- x[[i]]
        types<-class(coldata)
        typenul<-ifelse((length(types)>=1), tail(types, n=1), types)
        result[track,1]<-names(x)[i]
        labelcol<-sapply(x[i], attr, which = "label", exact = TRUE)
        alllables<-c(alllables, paste0(names(x[i]),"--",labelcol))
        miss <- sum(is.na(coldata))
        result[track,5] <-paste0(miss," (",round(miss/length(coldata)*100,2),"%)")
        valid <- length(coldata)-miss
        result[track,4] <- paste0(valid," (",round(valid/length(coldata)*100,2),"%)")
        if(typenul=="factor"){
          stat<-unique(coldata[!is.na(coldata)])
          if(length(stat)<=showfactor){
            for(j in seq_along(stat)){
              typeone <- levels(droplevels(stat[j]))
              alonetyoe <- sum(coldata == typeone, na.rm = T)
              if(Hid.val1!=typeone & Hid.val2!=typeone ){
                result[track,2] <- typeone
                result[track,3] <- paste0(alonetyoe," (",round(alonetyoe/length(coldata)*100,2),"%)")
                track <- track+1
              }
            }
          }else{
            result[track,2]<- paste0(length(stat), " distinct values.")
            result[track,3]<- NA
            track <- track+1
          }
        }else{
          if(track==1){
            result[track,2] <- "Range"
            result[track,3] <- paste0(min(coldata, na.rm = T)," - ", max(coldata, na.rm = T))
            track <- track+1
          }else{
            if(noMedium==TRUE){
              result[track,2] <- "Mean(SD)"
              result[track,3] <- paste0(round(mean(coldata, na.rm=T),2)," (",round(sd(coldata,na.rm=T),2),")")
              track <- track+1
            }else{
              result[track,2] <- "Mean(SD)"
              result[track+1,2] <- "Median(min, max)"
              result[track,3] <- paste0(round(mean(coldata, na.rm=T),2)," (",round(sd(coldata,na.rm=T),2),")")
              result[track+1,3] <- paste0(round(median(coldata,na.rm=T),2)," (", round(min(coldata,na.rm=T),2),
                                          ",",round(max(coldata,na.rm=T),2),")")
              track <- track+2
            }
          }
        }
      }
      result[is.na(result)] <- ''
      return(list(result,alllables))
    }
  }
}

