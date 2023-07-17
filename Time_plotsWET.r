source("./igor/hydrus_input_generator/zDPRM/NodInf_outTabular.R")

library(geomtextpath)


plot_node<-node_data
plot_node <-filter(plot_node, Depth == -100 |
                                   Depth == -300 |
                                   Depth == -600 |
                                   Depth == -900 |
                                   Depth ==-1200 )

plot_node$Time<-as.numeric(as.character(plot_node$Time))

#Pressure Head Matrix
PM<-plot_node %>%
  ggplot(aes(Time)) +
  scale_y_continuous()+
  scale_x_continuous(limits = c(1343,1403), breaks = seq(1343,1403, 20))+ 
  geom_line(aes(y= Head, colour = as.factor(Depth)))+
  ylab("Head [mm]")+
  ggtitle("Pressure Head Matrix")+
  labs(color="Depth")
PM

#Pressure Head Fracture
PF<-plot_node %>%
  ggplot(aes(Time)) +
  scale_y_continuous()+
  scale_x_continuous(limits = c(1343,1403), breaks = seq(1343,1403, 20))+ 
  geom_line(aes(y= HeadF, colour = as.factor(Depth)))+
  ylab("Pressure Head Fracture [mm]")+
  labs(color="Depth")
PF

#Water Content Matrix
WCM<-plot_node %>%
  ggplot(aes(Time)) +
  scale_y_continuous()+
  scale_x_continuous(limits = c(1343,1403), breaks = seq(1343,1403, 20))+ 
  geom_line(aes(y= Moisture, colour = as.factor(Depth)))+
  ylab("Theta [-]")+
  ggtitle("Water Content Matrix")+
  labs(color="Depth")
WCM

 #Water Content Fracture
WCF<-plot_node %>%
  ggplot(aes(Time)) +
  scale_y_continuous()+
  scale_x_continuous(limits = c(1343,1403), breaks = seq(1343,1403, 20))+ 
  geom_line(aes(y= MoistureF, colour = as.factor(Depth)))+
  ylab("Theta [-]")+
  ggtitle("Water Content Fracture")+
  labs(color="Depth")
WCF

#Water Flux Matrix

WFM<-plot_node %>%
  ggplot(aes(Time)) +
  scale_y_continuous()+
  scale_x_continuous(limits = c(1343,1403), breaks = seq(1343,1403, 20))+ 
  geom_line(aes(y= Flux, colour = as.factor(Depth)))+
  ylab("v [mm/hour]")+
  ggtitle("Water Flux Matrix")+
  labs(color="Depth")
WFM

#Water Flux Fracture

WFF<-plot_node %>%
  ggplot(aes(Time)) +
  scale_y_continuous()+
  scale_x_continuous(limits = c(1343,1403), breaks = seq(1343,1403, 20))+ 
  geom_line(aes(y= FluxF, colour = as.factor(Depth)))+
  ylab("v [mm/hour]")+
  ggtitle("Water Flux Fracture")+
  labs(color="Depth")
WFF

#Water Mass Transfer

WMT<-plot_node %>%
  ggplot(aes(Time)) +
  scale_y_continuous()+
  scale_x_continuous(limits = c(1343,1403), breaks = seq(1343,1403, 20))+ 
  geom_line(aes(y= Transf, colour = as.factor(Depth), label=Depth ))+
  ylab("MT [1/hours]")+
  ggtitle("Water Mass Transfer")+
  labs(color="Depth") 
WMT


  

 