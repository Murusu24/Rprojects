library(readxl)

plot_nodeEV0.05<-read_excel("C:/Users/Muru/downloads/Nod_inf_0.05.xlsx")
plot_nodeEV0.1<-read_excel("C:/Users/Muru/downloads/Nod_inf_0.1.xlsx")
plot_nodeEV0<-read_excel("C:/Users/Muru/downloads/Nod_inf_0.xlsx")


plot_nodeEV0.1 <-filter(plot_nodeEV0.1, Depth == -1200)
plot_nodeEV0.05 <-filter(plot_nodeEV0.05, Depth == -1200)
plot_nodeEV0 <-filter(plot_nodeEV0, Depth == -1200)


# plot_node <-filter(plot_node, Depth == -100 |
#                      Depth == -300 |
#                      Depth == -600 |
#                      Depth == -900 |
#                      Depth ==-1200 )

plot_nodeEV0$Time<-as.numeric(as.character(plot_nodeEV0$Time))

plot_nodeEV0.1$Time<-as.numeric(as.character(plot_nodeEV0.1$Time))
plot_nodeEV0.05$Time<-as.numeric(as.character(plot_nodeEV0.05$Time))

WCM<-ggplot(NULL,aes(x=Time )) +
  scale_y_continuous()+
  scale_x_continuous(limits = c(0,230), breaks = seq(0,230, 50))+ 
  geom_line(data=plot_nodeEV0.05, aes(y= Moisture,colour = 'Ev 0.05'))+
  geom_line(data=plot_nodeEV0.1,  aes(y= Moisture,colour = 'Ev 0.1'))+
  geom_line(data=plot_nodeEV0,  aes(y= Moisture,colour = 'No Eva'))+
  geom_hline(yintercept=0.163, linetype="solid", color = "black")+
  annotate("text", x=100, y=0.168, label="Initial")+
  ylab("Theta [-]")+
  ggtitle("Water Content Matrix")+
  labs(color="Depth")
WCM

WCF<-ggplot(NULL,aes(x=Time )) +
  scale_y_continuous()+
  scale_x_continuous(limits = c(0,230), breaks = seq(0,230, 50))+ 
  geom_hline(yintercept=0.005, linetype="solid", color = "black")+
  annotate("text", x=100, y=0.006, label="Initial")+
  geom_line(data=plot_nodeEV0.05, aes(y= MoistureF,colour = 'Ev 0.05'))+
  geom_line(data=plot_nodeEV0.1,  aes(y= MoistureF,colour = 'Ev 0.1'))+
  geom_line(data=plot_nodeEV0,  aes(y= MoistureF,colour = 'No Eva'))+
  ylab("Theta [-]")+
  ggtitle("Water Content Fracture")
 
WCF

WMT<-ggplot(NULL,aes(x=Time )) +
  scale_y_continuous()+
  scale_x_continuous(limits = c(0,230), breaks = seq(0,230, 50))+
  geom_line(data=plot_nodeEV0.05, aes(y= Transf,colour = 'Ev 0.05'))+
  geom_line(data=plot_nodeEV0.1,  aes(y= Transf,colour = 'Ev 0.1'))+
  # geom_line(data=plot_nodeEV0,  aes(y= Transf,colour = 'No Eva'))+
  ylab("MT [1/hour]")+
  ggtitle("Water Mass Transfer")

WMT
