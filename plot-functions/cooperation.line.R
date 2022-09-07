coop.frequency <- function(data){
  
  require(ggplot2)
  ggplot(data=data, aes(x=generation,y=cooperation))+
  geom_line()+
  ylim(0, 100)+
  theme_bw()
  
}