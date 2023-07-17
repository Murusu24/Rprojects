source("./igor/hydrus_input_generator/zDPRM/NodInf_outTabular.R")


#------------------------------------------------ 3 solutes-----------------

#Prepare Plotting
plot_node_test<-node_data

#Filtering for plotting single plot according to a certain depth
# plot_d20 <-plot_node %>% filter(plot_node$Depth == -20)

#Filtering for facet plots
plot_node <-filter(plot_node_test, Depth == -10 |
                     Depth == -30 |
                     Depth==-60 |
                     Depth==-120 )
plot_node$Time<-as.numeric(as.character(plot_node$Time))


#Concentration 
#Each 3 solute Conc Matrix vs Time
plot_node %>%
  mutate(
    facet = paste0(" Depth = -", abs(Depth), "mm")
  ) %>%
  ggplot(aes(Time)) +
  geom_line(aes(y= Solute1, color ="Matrix S1"))+
  geom_line(aes(y= Solute2, color ="Matrix S2"))+
  geom_line(aes(y= Solute3, color ="Matrix S3"))+
  geom_line(aes(y= ConcF, color ="Fracture"))+
  labs(y = "Concentration [mg]")+
  ggtitle("Concentration of 3 solutes in Matrix vs Fracture region")+
  scale_x_continuous(limits = c(0,2880), breaks = seq(0,2880, 500)) +
  facet_wrap(vars(facet), ncol = 2, scales = "fixed") +
  theme_minimal() +
  theme(strip.text = element_text(size = 12, face = "bold"))

#--------------------------Water Plots----------------------

#Pressure Heads
plot_node %>%
  mutate(
    facet = paste0(" Depth = -", abs(Depth), "mm")
  ) %>%
  ggplot(aes(Time)) +
  geom_line(aes(y= HeadF, color ="Fracture"))+
  geom_line(aes(y= Head, color ="Matrix"))+
  labs(y= "Pressure Head [mm]")+
  ggtitle('Pressure Heads')+
  scale_x_continuous(limits = c(0,2880), breaks = seq(0,2880, 500)) +
  facet_wrap(vars(facet), ncol = 2, scales = "free") +
  theme_minimal() +
  theme(strip.text = element_text(size = 12, face = "bold"))

#Water contents
plot_node %>%
  mutate(
    facet = paste0(" Depth = -", abs(Depth), "mm")
  ) %>%
  ggplot(aes(Time)) +
  # geom_line(aes(y= MoistureF, color ="Fracture"))+
  geom_line(aes(y= Moisture, color ="Matrix"))+
  ylab("Theta [-]")+
  ggtitle('Water Contents')+
  scale_x_continuous(limits = c(0,1440), breaks = seq(0,1440, 200)) +
  facet_wrap(vars(facet), ncol = 2, scales = "free") +
  theme_minimal() +
  theme(strip.text = element_text(size = 12, face = "bold"))

#Fluxes
plot_node %>%
  mutate(
    facet = paste0(" Depth = -", abs(Depth), "mm")
  ) %>%
  ggplot(aes(Time)) +
  geom_line(aes(y= FluxF, color ="Fracture"))+
  geom_line(aes(y= Flux, color ="Matrix"))+
  ylab("Velocity v [mm/hours]")+
  ggtitle('Water Fluxes')+
  scale_x_continuous(limits = c(0,2880), breaks = seq(0,2880, 500)) +
  facet_wrap(vars(facet), ncol = 2, scales = "fixed") +
  theme_minimal() +
  theme(strip.text = element_text(size = 12, face = "bold"))
#add plot Flux Mvs Flux F

#Water Mass Transfer
plot_node %>%
  mutate(
    facet = paste0(" Depth = -", abs(Depth), "mm")
  ) %>%
  ggplot(aes(Time)) +
  geom_line(aes(y= Transf, color = Transf))+
  scale_color_viridis_c(option = "D") +
  ylab("Mass Tr. [1/hours]")+
  ggtitle('Water Mass Tr.')+
  # scale_x_continuous(limits = c(0,2000), breaks = seq(0,200, 500)) +
  scale_x_continuous(limits = c(0,1440), breaks = seq(0,1440, 200)) +
  facet_wrap(vars(facet), ncol = 2, scales = "fixed") +
  theme_minimal() +
  theme(strip.text = element_text(size = 10, face = "bold"))


#--------------------------Solute Plots----------------------



#Solute Mass transfer
plot_node %>%
  mutate(
    facet = paste0(" Depth = -", abs(Depth), "mm")
  ) %>%
  ggplot(aes(Time, TranS, color=Transf)) +
  geom_line(alpha = 1) +
  ggtitle("Solute Mass Transfer")+
  ylab("MT [mg/mm3/hours] ")+
  scale_color_viridis_c(option = "D") +
# scale_x_continuous(limits = c(700,1200), breaks = seq(700,1200, 100)) +
  scale_x_continuous(limits = c(1300,1800), breaks = seq(1300,1800, 100)) +
  facet_wrap(vars(facet), ncol = 2, scales = "fixed") +
  theme_minimal() +
  theme(strip.text = element_text(size = 12, face = "bold"))

# Water and Solute MT against Time
plot_node %>%
  mutate(
    facet = paste0(" Depth = -", abs(Depth), "mm")
  ) %>%
  ggplot(aes(Time)) +
  geom_line(aes(y= Transf, colour = "Water MT"))+
  geom_line(aes(y= TranS, colour = "Solute MT"))+
  ylab("Mass Tr. [1/hours]")+
  ggtitle('Water and Solute Mass Tr.')+
  scale_x_continuous(limits = c(0, 2880), breaks = seq(0, 2880, 500)) +
  facet_wrap(vars(facet), ncol = 2, scales = "fixed") +
  theme_minimal() +
  theme(strip.text = element_text(size = 12, face = "bold"))

#Evaporation to precepitation ratio plots

