appearance.line <- function(data){
  
  require(ggplot2)
  ggplot(data=data, aes(x=generation,y=Freq, color=appearance))+
    geom_line()+
    scale_color_manual(values=c("green","black"), labels=c("overt","silent"))+
    labs(y="frequency")+
    theme_bw()
  
}