source("./igor/hydrus_input_generator/zDPRM/NodInf_outTabular.R")

library(geomtextpath)
library(scales)


plot_node<-node_data
plot_node <-filter(plot_node, Depth == -10 |
                     Depth == -20 |
                     Depth == -30|
                     Depth == -40 |
                     Depth ==-50 )

plot_node$Time<-as.numeric(as.character(plot_node$Time))

#Pressure Head Matrix
PM<-plot_node %>%
  ggplot(aes(Time)) +
  scale_y_continuous(limits=c(-800,0))+
  scale_x_continuous(limits = c(1343,1403), breaks = seq(1343,1403, 20))+ 
  geom_line(aes(y= Head, colour = as.factor(Depth)))+
  coord_cartesian(ylim = c(-800, -400))+
  ylab("Pressure Head Matrix [mm]")+
  labs(color="Depth")
PM

#Pressure Head Fracture
PF<-plot_node %>%
  ggplot(aes(Time)) +
  scale_y_continuous()+
  scale_x_continuous(limits = c(1343,1403), breaks = seq(1343,1403, 20))+
  coord_cartesian(ylim = c(-900, -800))+
  geom_line(aes(y= HeadF, colour = as.factor(Depth)))+
  ylab("Pressure Head Fracture [mm]")+
  labs(color="Depth")
PF

#Water Content Matrix
WCM<-plot_node %>%
  ggplot(aes(Time)) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.001) )+
  coord_cartesian(ylim = c(0.175, 0.2))+
  scale_x_continuous(limits = c(1343,1403), breaks = seq(1343,1403, 20))+ 
  geom_line(aes(y= Moisture, colour = as.factor(Depth)))+
  ylab("Theta [-]")+
  ggtitle("Water Content Matrix")+
  labs(color="Depth")
WCM

#Water Content Fracture
WCF<-plot_node %>%
  ggplot(aes(Time)) +
  scale_y_continuous(labels = unit_format(unit = "e-05", scale = 1 / 1e-05, digits = 1))+
  scale_x_continuous(limits = c(1343,1403), breaks = seq(1343,1403, 20))+ 
  coord_cartesian(ylim = c(0.0055,0.00625))+
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
  scale_y_continuous(labels = unit_format(unit = "e-03", scale = 1 / 1e-03, digits = 1))+
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
  geom_line(aes(y= Transf, colour = as.factor(Depth) ))+
  ylab("MT [1/hours]")+
  ggtitle("Water Mass Transfer")+
  labs(color="Depth") 
WMT




