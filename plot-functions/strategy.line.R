PD.frequency <- function(data, every = 100){
  
  require(ggplot2)
  
  #subset generations by every; default = 100
  
 coop <- seq(1,nrow(data),every)
 def <- seq(2,nrow(data),every)
 
 data.every <- c(coop,def)
 data.every <- sort(data.every)
  
  ggplot(data=data[data.every,], aes(x=generation,y=Freq, color=strategy))+
  geom_line()+
  scale_color_manual(values=c("green","red"), labels=c("cooperator","defector"))+labs(y="frequency")+theme_bw()
  
}
