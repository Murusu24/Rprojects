library(readxl)
library(ggplot2)




plot_node$Time<-as.numeric(as.character(plot_node$Time))

Sflux<-ggplot(NULL,aes(x=Time )) +
  scale_y_continuous()+
  scale_x_continuous(limits = c(0,50))+ 
  geom_line(data=plot_node, aes(y= vTopF ,colour = ' Top Fracture'))+
  geom_line(data=plot_node,  aes(y= vTopM,colour = 'Top Matrix'),alpha=0.4)+
  ylab("v [mm/hour]")+
  ggtitle("Surface Flux (SF0.3) High Rain")
  # geom_line(data=plot_node,  aes(y= vBotF,colour = 'Bot Fracture'))
  # geom_line(data=plot_node,  aes(y= vBotM,colour = 'Bot Matrix'))
  
Sflux


#plots of SF flux 03 05 08
plot_node0<-read_excel("C:/Users/Muru/Home/igor/hydrus_input_generator/zDPRM/Tlevel.out.xlsx")
plot_node3<-read_excel("C:/Users/Muru/Home/igor/hydrus_input_generator/zDPRM/Tlevel0.3.out.xlsx")
plot_node5<-read_excel("C:/Users/Muru/Home/igor/hydrus_input_generator/zDPRM/Tlevel0.5.out.xlsx")
plot_node7<-read_excel("C:/Users/Muru/Home/igor/hydrus_input_generator/zDPRM/Tlevel0.7.out.xlsx")
plot_node1<-read_excel("C:/Users/Muru/Home/igor/hydrus_input_generator/zDPRM/Tlevel1.out.xlsx")


Sflux<-ggplot(NULL,aes(x=Time )) +
  scale_y_continuous()+
  scale_x_continuous(limits = c(0,50))+ 
  geom_line(data=plot_node0,  aes(y= vTopM,colour = '0'))+
  geom_line(data=plot_node3,  aes(y= vTopM,colour = '0.3',))+
  geom_line(data=plot_node5,  aes(y= vTopM,colour = '0.5'))+
  geom_line(data=plot_node7,  aes(y= vTopM,colour = '0.7'))+
  geom_line(data=plot_node1,  aes(y= vTopM,colour = '1'))+
  ylab("v [mm/hour]")+
  ggtitle("Actual Surface Flux Matrix")+
  labs(color="SF")
# geom_line(data=plot_node,  aes(y= vBotF,colour = 'Bot Fracture'))
# geom_line(data=plot_node,  aes(y= vBotM,colour = 'Bot Matrix'))

Sflux

Sflux<-ggplot(NULL,aes(x=Time )) +
  scale_y_continuous()+
  scale_x_continuous(limits = c(0,50))+ 
  geom_line(data=plot_node3,  aes(y= vTopM,colour = '0.3'))+
  geom_line(data=plot_node5,  aes(y= vTopM,colour = '0.5'))+
  geom_line(data=plot_node7,  aes(y= vTopM,colour = '0.7'))+
  ylab("v [mm/hour]")+
  ggtitle("Surface Flux Matrix WET")+
  labs(color="SF")
# geom_line(data=plot_node,  aes(y= vBotF,colour = 'Bot Fracture'))
# geom_line(data=plot_node,  aes(y= vBotM,colour = 'Bot Matrix'))

Sflux

