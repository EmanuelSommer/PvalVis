



#'  Visualization of p-values for basic hypothesis tests with the \eqn{\chi2} distribution
#'
#' Given \eqn{\chi2 ~ \chi2(df)} the function calculates the p-value and visualizes the result as the area under the density function.
#' Furthermore the mean and the values one and two standard deviations from the mean are highlighted.
#'
#' The p-value will be calculated by P(X>\eqn{\chi2}) given X ~ \eqn{\chi2}(df).
#'
#'
#' @param chisq_value The value of a test statistic with the underlying \eqn{\chi2} distribution
#'
#' (values that are very far away from the mean - roughly more than 4 times the standard deviation - are not recommend to use as the p-value will be approximately 0 or 1 anyways)
#' @param df The degree of freedom of the underlying \eqn{\chi2} distribution (only df greater than 0).
#'
#' @return a ggplot2 object displaying the results
#' @export
#' @import ggplot2
#' @author Emanuel Sommer
#'
#' @examples chisq_pval()
#' chisq_pval(chisq_value = 23, df = 15)
chisq_pval<-function(chisq_value=4,df=1){
  require(ggplot2)
  if(chisq_value<0){stop("This chisq value is not possible.")}
  if(df<1){stop("This df is not possible.")}

  cols<-c("mean"="#990000")
  mean<-(df)
  sd<-sqrt(2*df)

  bounds<-c(chisq_value,mean+5*sd)

  sd_bounds<-c(mean+sd,mean-sd,mean+2*sd,mean-2*sd)
  sd_bounds<-sd_bounds[sd_bounds>0]

  new_data<-data.frame(a=max(0,(mean-5*sd)):(mean+5*sd))

  P_val<-round(pchisq(chisq_value,df = df,lower.tail = F),4)

  ggplot2::ggplot(new_data,aes(x=a))+
    stat_function(fun = dchisq,args = list(df=df))+
    geom_point(aes(x=mean,y=0,col="mean"),size=4,shape=17)+
    geom_vline(xintercept = sd_bounds,linetype="dotted",
               col="#990000")+
    geom_area(stat = "function", fun = dchisq,args = list(df=df), fill = "blue",
              alpha=0.3,
              xlim = bounds)+
    annotate("text",x=(mean+4*sd),y=0.75*dchisq(mean,df=df),
             label=paste("\u03C72  value:",chisq_value,"\n", "p-value: ",P_val),
             #label=bquote(chi^2~"value:"~ .(chisq_value)~"p-value"~.(P_val)),
             col="blue")+
    annotate("text",x=chisq_value,y=0,label="\u03C72 ",size=5.5,col="#333333")+
    scale_color_manual(name="",values=cols,labels="mean & dotted sd",position="bottom")+
    scale_x_continuous(limits = c(max(0,(mean-5*sd)),mean+5*sd))+
    labs(x="",y="",title = paste("One sided hypothesis test with \u03C72 ~ \u03C72(",df,")")  )+
    theme_bw()
}





