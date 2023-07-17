source("./igor/hydrus_input_generator/zDPRM/NodInf_outTabular.R")

library(geomtextpath)


plot_node<-node_data

plot_nodeEV0.5<-node_data
plot_nodeEV0.1<-node_data
plot_nodeEV0.1 <-filter(plot_node, Depth == -1200)
plot_nodeEV0.5 <-filter(plot_node, Depth == -1200)
plot_node <-filter(plot_node, Depth == -1200)


# plot_node <-filter(plot_node, Depth == -100 |
#                      Depth == -300 |
#                      Depth == -600 |
#                      Depth == -900 |
#                      Depth ==-1200 )

plot_node$Time<-as.numeric(as.character(plot_node$Time))

plot_nodeEV0.1$Time<-as.numeric(as.character(plot_nodeEV0.1$Time))
plot_nodeEV0.5$Time<-as.numeric(as.character(plot_nodeEV0.5$Time))


#Pressure Head Matrix
PM<-plot_node %>%
  ggplot(aes(Time)) +
  scale_y_continuous(limits = c(-600,0))+
  scale_x_continuous(limits = c(0,300), breaks = seq(0,300, 50))+ 
  geom_line(aes(y= Head, colour = as.factor(Depth)))+
  ylab("Head [mm]")+
  ggtitle("Pressure Head Matrix")+
  labs(color="Depth")
PM

#Pressure Head Fracture
PF<-plot_node %>%
  ggplot(aes(Time)) +
  scale_y_continuous(limits = c(-750,0))+
  scale_x_continuous(limits = c(0,300), breaks = seq(0,300, 100))+ 
  geom_line(aes(y= HeadF, colour = as.factor(Depth)))+
  ylab("Head [mm]")+
  ggtitle("Pressure Head Fracture")+
  labs(color="Depth")
PF

#Water Content Matrix
WCM<-plot_node %>%
  ggplot(aes(Time)) +
  scale_y_continuous()+
  scale_x_continuous(limits = c(0,300), breaks = seq(0,300, 100))+ 
  geom_line(aes(y= Moisture, colour = as.factor(Depth)))+
  geom_hline(yintercept=0.163, linetype="solid", color = "black")+
  annotate("text", x=100, y=0.168, label="Initial")+
  ylab("Theta [-]")+
  ggtitle("Water Content Matrix")+
  labs(color="Depth")
WCM

#Water Content Fracture
WCF<-plot_nodeEV0.1 %>%
  ggplot(aes(Time)) +
  scale_y_continuous()+
  scale_x_continuous(limits = c(0,300), breaks = seq(0,300, 100))+ 
  geom_hline(yintercept=0.005, linetype="solid", color = "black")+
  annotate("text", x=100, y=0.007, label="Initial")+
  geom_line(aes(y= MoistureF, colour = as.factor(Depth)))+
  geom_line(aes(y= MoistureF$plot_nodeEV0.5, colour = as.factor(Depth)))+
  ylab("Theta [-]")+
  ggtitle("Water Content Fracture")+
  labs(color="Depth")
WCF

#Water Flux Matrix

WFM<-plot_node %>%
  ggplot(aes(Time)) +
  scale_y_continuous()+
  scale_x_continuous(limits = c(0,300), breaks = seq(0,300, 100))+ 
  geom_line(aes(y= Flux, colour = as.factor(Depth)))+
  geom_hline(yintercept=0, linetype="solid", color = "black")+
  annotate("text", x=270, y=0.04, label="Initial")+
  ylab("v [mm/hour]")+
  ggtitle("Water Flux Matrix")+
  labs(color="Depth")
WFM

#Water Flux Fracture

WFF<-plot_node %>%
  ggplot(aes(Time)) +
  scale_y_continuous()+
  scale_x_continuous(limits = c(0,300), breaks = seq(0,300, 100))+ 
  geom_line(aes(y= FluxF, colour = as.factor(Depth)))+
  geom_hline(yintercept=0, linetype="solid", color = "black")+
  annotate("text", x=270, y=-0.5, label="Initial")+
  ylab("v [mm/hour]")+
  ggtitle("Water Flux Fracture")+
  labs(color="Depth")
WFF

#Water Mass Transfer
options(scipen = 999)
WMT<-plot_node %>%
  ggplot(aes(Time)) +
  scale_y_continuous(labels = scales::scientific)+
  scale_x_continuous(limits = c(0,300), breaks = seq(0,300, 100))+ 
  geom_hline(yintercept=0, linetype="solid", color = "black")+
  annotate("text", x=150, y=0.00015, label="Initial")+
  geom_line(aes(y= Transf, colour = as.factor(Depth), label=Depth ))+
  ylab("MT [1/hours]")+
  ggtitle("Water Mass Transfer")+
  labs(color="Depth") 
WMT

#---------------------------------------------


