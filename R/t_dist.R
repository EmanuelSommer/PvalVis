

#' Visualization of p-values for basic hypothesis tests with the student-t distribution
#'
#' Given \eqn{T ~ t(df)} the function calculates the p-value and visualizes the result as the area under the density function.
#' Furthermore the mean and the values one and two standard deviations from the mean are highlighted.
#'
#'
#' @param T_value The value of a test statistic with the underlying student-t distribution
#'
#' (values that are very far away from the mean - roughly more than 4 times the standard deviation - are not recommend to use as the p-value will be approximately 0 anyways)
#' @param df The degree of freedom of the underlying student-t distribution (only df greater than 2).
#' @param direction The 'direction' of the test with respect to T:
#' \describe{
#'  \item{extreme (default)}{The p-value will be calculated using \eqn{min(P(X \le T),P(X \ge T)) with X~t(df)} }
#'  \item{less}{The p-value will be calculated using \eqn{P(X \le T) with X~t(df)}}
#'  \item{greater}{The p-value will be calculated using \eqn{P(X \ge T) with X~t(df)}}
#'  \item{both}{The p-value will be calculated using \eqn{2*min(P(X \le T),P(X \ge T)) with X~t(df)} }
#'  }
#'  So for the first three options a one sided hypothesis gets tested and for the last one a two sided hypothesis is tested.
#'
#'
#'
#' @return a ggplot2 object displaying the results
#' @export
#' @import ggplot2
#' @author Emanuel Sommer
#'
#' @examples t_pval(-2,df=20)
#' t_pval(T_value = 1, df=10, direction = "both")
t_pval<-function(T_value=0,df,direction=c("extreme","less","greater","both")){
  require(ggplot2)
  z_value<-T_value
  if(df<3){stop("This is not a possible degree of freedom.")}
  mean<-0
  sd<-sqrt(df/(df-2))
  direction<-match.arg(direction)
  if(direction=="extreme"){
    cols<-c("mean"="#990000")
    z_logic<-ifelse(z_value>=mean,FALSE,TRUE)
    if(z_logic){
      bounds<-c(mean-5*sd,z_value)
    }else{
      bounds<-c(z_value,mean+5*sd)}
    new_data<-data.frame(a=(mean-5*sd):(mean+5*sd))
    ggplot2::ggplot(new_data,aes(x=a))+
      stat_function(fun = dt,args = list(df=df))+
      geom_point(aes(x=mean,y=0,col="mean"),size=4,shape=17)+
      geom_vline(xintercept = c(mean+sd,mean-sd,mean+2*sd,mean-2*sd),linetype="dotted",
                 col="#990000")+
      geom_area(stat = "function", fun = dt,args = list(df=df), fill = "blue",
                alpha=0.3,
                xlim = bounds)+
      annotate("text",x=(mean+4*sd),y=0.75*dt(mean,df=df),
               label=paste("T value:",z_value,"\n", "p-value: ",
                           round(pt(z_value,df=df,lower.tail = z_logic),4)),
               col="blue")+
      annotate("text",x=z_value,y=0,label="T",size=5.5)+
      scale_color_manual(name="",values=cols,labels="mean & dotted sd",position="bottom")+
      labs(x="",y="",title = paste("One sided hypothesis test with T ~ t(",df,")"))+
      theme_bw()


  }else if(direction=="less"){
    cols<-c("mean"="#990000")
    z_logic<-TRUE
    if(z_logic){
      bounds<-c(mean-5*sd,z_value)
    }else{
      bounds<-c(z_value,mean+5*sd)}
    new_data<-data.frame(a=(mean-5*sd):(mean+5*sd))
    ggplot2::ggplot(new_data,aes(x=a))+
      stat_function(fun = dt,args = list(df=df))+
      geom_point(aes(x=mean,y=0,col="mean"),size=4,shape=17)+
      geom_vline(xintercept = c(mean+sd,mean-sd,mean+2*sd,mean-2*sd),linetype="dotted",
                 col="#990000")+
      geom_area(stat = "function", fun = dt,args = list(df=df), fill = "blue",
                alpha=0.3,
                xlim = bounds)+
      annotate("text",x=(mean+4*sd),y=0.75*dt(mean,df=df),
               label=paste("T value:",z_value,"\n", "p-value: ",
                           round(pt(z_value,df=df,lower.tail = z_logic),4)),
               col="blue")+
      annotate("text",x=z_value,y=0,label="T",size=5.5)+
      scale_color_manual(name="",values=cols,labels="mean & dotted sd",position="bottom")+
      labs(x="",y="",title = paste("One sided hypothesis test with T ~ t(",df,")"))+
      theme_bw()


  }else if(direction=="greater"){
    cols<-c("mean"="#990000")
    z_logic<-FALSE
    if(z_logic){
      bounds<-c(mean-5*sd,z_value)
    }else{
      bounds<-c(z_value,mean+5*sd)}
    new_data<-data.frame(a=(mean-5*sd):(mean+5*sd))
    ggplot2::ggplot(new_data,aes(x=a))+
      stat_function(fun = dt,args = list(df=df))+
      geom_point(aes(x=mean,y=0,col="mean"),size=4,shape=17)+
      geom_vline(xintercept = c(mean+sd,mean-sd,mean+2*sd,mean-2*sd),linetype="dotted",
                 col="#990000")+
      geom_area(stat = "function", fun = dt,args = list(df=df), fill = "blue",
                alpha=0.3,
                xlim = bounds)+
      annotate("text",x=(mean+4*sd),y=0.75*dt(mean,df=df),
               label=paste("T value:",z_value,"\n", "p-value: ",
                           round(pt(z_value,df=df,lower.tail = z_logic),4)),
               col="blue")+
      annotate("text",x=z_value,y=0,label="T",size=5.5)+
      scale_color_manual(name="",values=cols,labels="mean & dotted sd",position="bottom")+
      labs(x="",y="",title = paste("One sided hypothesis test with T ~ t(",df,")"))+
      theme_bw()


  }else if(direction=="both"){
    cols<-c("mean"="#990000")
    z_logic<-ifelse(z_value>=mean,FALSE,TRUE)
    if(z_logic){
      zlower<-z_value
      zupper<-mean+abs(mean-z_value)
    }else{
      zlower<-mean-abs(mean-z_value)
      zupper<-z_value
    }
    new_data<-data.frame(a=(mean-5*sd):(mean+5*sd))
    ggplot2::ggplot(new_data,aes(x=a))+
      stat_function(fun = dt,args = list(df=df))+
      geom_point(aes(x=mean,y=0,col="mean"),size=4,shape=17)+
      geom_vline(xintercept = c(mean+sd,mean-sd,mean+2*sd,mean-2*sd),linetype="dotted",
                 col="#990000")+
      geom_area(stat = "function", fun = dt,args = list(df=df), fill = "blue",
                alpha=0.3,
                xlim = c(mean-5*sd,zlower))+
      geom_area(stat = "function", fun = dt,args = list(df=df), fill = "blue",
                alpha=0.3,
                xlim = c(zupper,mean+5*sd))+
      annotate("text",x=(mean+4*sd),y=0.75*dt(mean,df=df),
               label=paste("T value:",z_value,"\n", "p-value: ",
                           round(2*pt((z_value),df=df,lower.tail = z_logic),4)),
               col="blue")+
      annotate("text",x=z_value,y=0,label="T",size=5.5)+
      scale_color_manual(name="",values=cols,labels="mean & dotted sd",position="bottom")+
      labs(x="",y="",title = paste("Two sided hypothesis test with T ~ t(",df,")"))+
      theme_bw()
  }else{
    stop("This is not a valid direction.")
  }


}



