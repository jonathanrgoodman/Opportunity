type.line <- function(data, every = 100){
  
  require(ggplot2)
  
  #subset generations by every; default = 100
  
  honest <- seq(1,nrow(data),every)
  mimic <- seq(2,nrow(data),every)
  
  data.every <- c(honest,mimic)
  data.every <- sort(data.every)
  
  ggplot(data=data[data.every,], aes(x=generation,y=Freq, color=type))+
    geom_line()+
    scale_color_manual(values=c("blue","orange"), labels=c("honest","mimic"))+labs(y="frequency")+theme_bw()
  
}
