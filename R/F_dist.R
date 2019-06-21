



#'  Visualization of p-values for basic hypothesis tests with the F distribution
#'
#' Given \eqn{F ~ F(df1,df2)} the function calculates the p-value and visualizes the result as the area under the density function.
#' Furthermore the mean and the values one and two standard deviations from the mean are highlighted.
#'
#' The p-value will be calculated by P(X>F) given X ~ F(df1,df2).
#'
#'
#' @param F_value The value of a test statistic with the underlying F distribution
#'
#' (values that are very far away from the mean - roughly more than 4 times the standard deviation - are not recommend to use as the p-value will be approximately 0 or 1 anyways)
#' @param df1 The first degree of freedom of the underlying F distribution (only df1 greater than 2).
#' @param df2 The second degree of freedom of the underlying F distribution (only df2 greater than 4).
#'
#' @return a ggplot2 object displaying the results
#' @export
#' @import ggplot2
#' @author Emanuel Sommer
#'
#' @examples F_pval()
#' F_pval(F_value = 3, df1 = 5, df2 = 30)
F_pval<-function(F_value=5,df1=3,df2=5){
  require(ggplot2)
  if(F_value<0){stop("This F value is not possible.")}
  if(df1<3){stop("This df1 is not possible.")}
  if(df2<5){stop("This df2 is not possible.")}

  cols<-c("mean"="#990000")
  mean<-(df2/(df2-2))
  sd<-sqrt((2*df2^2*(df1+df2-2))/(df1*((df2-2)^2)*(df2-4)))

  bounds<-c(F_value,mean+5*sd)

  sd_bounds<-c(mean+sd,mean-sd,mean+2*sd,mean-2*sd)
  sd_bounds<-sd_bounds[sd_bounds>0]

  new_data<-data.frame(a=max(0,(mean-5*sd)):(mean+5*sd))
  ggplot2::ggplot(new_data,aes(x=a))+
    stat_function(fun = "df",args = list(df1=df1,df2=df2))+
    geom_point(aes(x=mean,y=0,col="mean"),size=4,shape=17)+
    geom_vline(xintercept = sd_bounds,linetype="dotted",
               col="#990000")+
    geom_area(stat = "function", fun = "df",args = list(df1=df1,df2=df2), fill = "blue",
              alpha=0.3,
              xlim = bounds)+
    annotate("text",x=(mean+4*sd),y=0.75*df(mean,df1 = df1,df2 = df2),
             label=paste("F value:",F_value,"\n", "p-value: ",
                         round(pf(F_value,df1 = df1,df2 = df2,lower.tail = F),4)),
             col="blue")+
    annotate("text",x=F_value,y=0,label="F",size=5.5,col="#333333")+
    scale_color_manual(name="",values=cols,labels="mean & dotted sd",position="bottom")+
    scale_x_continuous(limits = c(max(0,(mean-5*sd)),mean+5*sd))+
    labs(x="",y="",title = paste("One sided hypothesis test with F ~ F(",df1,",",df2,")"))+
    theme_bw()
}





