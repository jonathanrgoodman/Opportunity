coop.frequency <- function(data, every = 100){
  
  require(ggplot2)
  
  #subset generations by every; default = 100
  
  coop <- seq(1,nrow(data),every)

  ggplot(data=data[coop,], aes(x=generation,y=cooperation))+
  geom_line()+
  ylim(0, 100)+
  theme_bw()
  
}
