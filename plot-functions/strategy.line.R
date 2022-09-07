PD.frequency <- function(data){
  
  require(ggplot2)
  ggplot(data=data, aes(x=generation,y=Freq, color=strategy))+
  geom_line()+
  scale_color_manual(values=c("green","red"), labels=c("cooperation","defection"))+labs(y="frequency")+theme_bw()
  
}