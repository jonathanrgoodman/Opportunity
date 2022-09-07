type.line <- function(data){
  
  require(ggplot2)
  ggplot(data=data, aes(x=generation,y=Freq, color=type))+
    geom_line()+
    scale_color_manual(values=c("blue","orange"), labels=c("honest","mimic"))+labs(y="frequency")+theme_bw()
  
}