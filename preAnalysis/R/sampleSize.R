#' @title Sample Size Table
#' @name sampleSize
#' @description This function gets a range of effect size and power from user and return a
#' 10 x 10 matrix of recommended sample size.
#' @usage sampleSize(E.start=h1, power.start=pr1, E.end=NULL, power.end=NULL, type="prop",
#'                    sig.level=0.05, alternative = "two.sided")
#' @param E.start Minimum effect size
#' @param power.start Minimum power of test
#' @param E.end Maximum effect size
#' @param power.end Maximum power of test
#' @param type Type of test : proportion test or t.test ("prop" / "t.test")
#' @param sig.level Significance level (Type I error probability)
#' @param alternative a character string specifying the alternative hypothesis,
#'                     must be one of "two.sided" (default), "greater" or "less"
#' @return sample size table
#' @keywords sample size
#' @examples
#' \dontrun{
#' sampleSize(E.start=0.1, power.start=0.8)
#' sampleSize(E.start=0.1, E.end=0.3, power.start=0.8, power.end=0.95, sig.level=0.05)
#' }
#' @export
sampleSize<-function(E.start=h1, power.start=pr1, E.end=NULL, power.end=NULL, type="prop",
                     sig.level=0.05, alternative = "two.sided"){
  if(E.start>=1 || power.start>=1){
    message("Invalid: Power must be smaller than 1 ")
  }else{
    if(is.null(E.end)){
      if(E.start<0.9){
        track1<-0.01
      }else{
        E.start<-0.9
        track1<-0.01
      }
    }else{
      track1<-(E.end-E.start)/10
    }
    if(is.null(power.end)){
      if(power.start<0.9){
        track2<-0.01
      }else{
        power.start<-0.9
        track2<-0.01
      }
      track2<-0.01
    }else{
      track2<-(power.end-power.start)/10
    }
    size<-data.frame(outcome=numeric())
    rowna<-NULL
    colna<-NULL
    for(i in 1:10){
      for(j in 1:10){
        effect_size <- E.start+i*track1
        power_size <- power.start+j*track2
        if(type=="prop"){
          size[i,j]<-ceiling(pwr::pwr.2p.test(h = effect_size, sig.level = sig.level,
                                              power = power_size, alternative = alternative)$n)
        }else if(type=="t.test"){
          size[i,j]<-ceiling(pwr::pwr.t.test(d = effect_size, sig.level = sig.level,
                                             power = power_size, alternative = alternative)$n)
        }else{
          stop("Please enter a valid method (prop/t.test).")
        }
      }
    }
    rownames(size)<-(E.start+seq_along(size)*track1)
    colnames(size)<-(power.start+seq_along(size)*track2)
    return(size)
  }
}
