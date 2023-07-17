library(data.table)
library(tidyverse)
library(writexl)
library(ggplot2)
library(dplyr)
library(car)
library(viridis)
library(reshape2)

# R code to read Hydrus-1D Nod_Inf.out files into an R data frame
# set Hydrus-1D project directory (folder)
# -=-=-=-=- EDIT THIS TO MATCH THE FOLDER ON YOUR COMPUTER ! -=-=-=-=-

setwd("C:/Users/Muru/Home")
PCdir <- paste0("igor/hydrus_input_generator/zDPRM")
# read Nod_Inf.out file into a vector of character (text) strings
Nod_Inf_out <- readLines(paste0(PCdir,"/","Nod_Inf.out"))

#View(Nod_Inf_out) # optionally check what we just read
# find indices of some rows we want to delete
head_rows <- grep("Node", Nod_Inf_out)
# use indices to delete the rows
Nod_Inf_out <- Nod_Inf_out[-c(seq(1,7), head_rows-1,
                              head_rows+1, head_rows+2)]
# find indices of some more rows we want to delete
ends <- grep("end",Nod_Inf_out)[-1*NROW(grep("end",Nod_Inf_out))]
# use indices to delete the rows
Nod_Inf_out <- Nod_Inf_out[-c(ends, ends+1, ends+2)]
# find indices of rows that have the Hydrus-1D print times
time_rows <- grep("Time:", Nod_Inf_out)
# extract the rows with Hydrus-1D print times into a vector
timz <- Nod_Inf_out[time_rows]
# get rid of text around the Hydrus-1D print time values...
timz <- gsub("Time: ","",timz)
timz <- gsub(" ","",timz)
# ...and convert to numbers...
timz <- as.numeric(timz)
# ...then delete the Hydrus-1D print time rows
Nod_Inf_out <- Nod_Inf_out[-c(time_rows)]
# find the rows with column names for each block of print data...
head_rows <- grep("Node", Nod_Inf_out)
# and remove all except the first one
Nod_Inf_out <- Nod_Inf_out[-c(head_rows[-1])]
# strip out all but single spaces from data...
while(NROW(grep("  ", Nod_Inf_out)) > 0) {
  Nod_Inf_out <- gsub("  "," ", Nod_Inf_out)
}

# ...and (finally!) delete the very last row (an 'end' statement)
Nod_Inf_out <- Nod_Inf_out[-1*NROW(Nod_Inf_out)]


df<-as.data.frame(Nod_Inf_out)
df_sort = df %>%
  separate(Nod_Inf_out, into = c("ID","Node","Depth", "Head",
                                 'Moisture',"HeadF","MoistureF", 
                                 "Flux","FluxF","Sink","Transf", 
                                 "TranS","Temp", "ConcF", 
                                 "a","b","Solute 1","Solute 2", "Solute 3"), sep = " ")
df_sort[1,15]<- paste('a')





df_sort[1,16]<- paste('b')
df_sort[1,17]<- paste('Solute1')
df_sort[1,18]<- paste('Solute2')
df_sort[1,19]<- paste('Solute3')


df_out<-unite(df_sort, col='',sep = " ", remove = TRUE, na.rm = FALSE)
Nod_Inf_out<-as.character(df_out[,1])


# write the edited output to a file...
writeLines(Nod_Inf_out, con="./igor/hydrus_input_generator/zDPRM/NodInf3.out")
# ...and read the file in as space-delimited
node_data <- read.table(file = "./igor/hydrus_input_generator/zDPRM/NodInf3.out", header = TRUE, sep = " ")
# the is a blank column 1; rename it to "Time"
colnames(node_data)[1] <- "Time"
# use the print times vector to make a vector with
# the relevant time repeated for each block
timz0 <- rep(timz[1], NROW(node_data)/NROW(timz))


for (i in 2:NROW(timz)) {
  timz0 <- append(timz0, rep(timz[i], NROW(node_data)/NROW(timz)))
}
# replace the Time colum with the vector we just made...
node_data$Time <- timz0
# ...and convert Time to a factor
node_data$Time <- as.factor(node_data$Time)
node_data$Time <- ordered(node_data$Time)
# remove temporary objects...
rm(list = c("i","head_rows","Nod_Inf_out","timz","timz0","ends"))


# typeof(node_data)

#Export xlsx file.
node_data_out <-write_xlsx(node_data, "C:/Users/Muru/Home/igor/hydrus_input_generator/zDPRM/Nod_inf_out.xlsx" ) 







#-------------------Scatter plot prepare------------------------------
options(scipen = 0)



plot_node<-node_data

plot_node$Time<-as.numeric(as.character(plot_node$Time))
plot_node$Transf<-as.numeric(as.character(plot_node$Transf))
plot_node$TranS<-as.numeric(as.character(plot_node$TranS))

#-----------------Solute Mass Transfer Plot---------------

#Method1
#Filrtering data according to a certain depth
plot_node <-filter(plot_node, Depth == -10| 
                     Depth == -30 |
                     Depth == -60 |
                     Depth == -90
                     
)
# # Water mass Transfer curve

plot_node %>%
  mutate(
    facet = paste0(" Depth = -", abs(Depth), "mm")
  ) %>%
  ggplot(aes(Time, Transf)) +
  geom_line() +
  # scale_color_viridis_c(option = "D", direction=-1,alpha = c(0,1)) +
  # scale_x_continuous(limits = c(0, 60), breaks = seq(0, 60, 10)) +
  scale_x_continuous(limits = c(0, 1440), breaks = seq(0, 1440, 200)) +
  # scale_y_continuous(limits = c(0,0.015))+
  labs(y = "Water MT [1/hours]")+
  facet_wrap(vars(facet), ncol = 2, scales = "free") +
  theme_minimal() +
  ggtitle("Intensity 0.8 mm/hour Eva= 0.1 mm/hour")+
  scale_alpha(range = c(0.1, 1), guide = "none") +
  theme(strip.text = element_text(size = 12, face = "bold"))

# scale_x_continuous(limits = c(1321, 1381), breaks = seq(1321, 1381, 20)) +
  
#Water Mass transfer Overall
plot_node %>%
  mutate %>%
  ggplot(aes(Time, Transf, color=Transf)) +
  geom_line() +
  scale_color_viridis_c(option = "D",direction=-1,alpha = c(0,1)) +
  scale_y_continuous()+
  scale_x_continuous(limits = c(0,1440), breaks = seq(0,1440, 200)) +
  theme_minimal() +
  ggtitle("Water Mass Transfer  Drying")+
  ylab("Water Mass Tr [-]")+
  scale_alpha(range = c(0.1, 1), guide = "none") +
  theme(strip.text = element_text(size = 12, face = "bold"))



    # plot_node <-filter(plot_node, Time == 481 |
#                      Time == 483 |
#                      Time == 486 |
#                      Time == 489|
#                    Time == 492)
# plot_node <- plot_node[plot_node$Time >= 481 & plot_node$Time <= 492, ]









# # ---------------Solute Mass Transfer---------------------
# 

# 
# #___________________________________
# ggpC <- ggplot(plot_node,aes(TranS, Transf))+
#   geom_point()+
#   ggtitle('Mass Transfer [depth= 20mm]')+
#   scale_x_continuous(breaks = seq(0, 1400,200))+
#   scale_y_continuous(limits = c(0, 20), breaks = seq(0,20,5))
# ggpC
# 
# #------------Concentration Plot------------------  
# 
# #Fracture vs Matrix against Time
#  ggpC <- ggplot(plot_d100,aes(Time))+
#   geom_point(aes(y=ConcF, color= "ConcF"))+
#   geom_point(aes(y=ConcM, color= "ConcM"))+
#   ylab("Concentration [mg/mm3]")+
#   ggtitle('Concentrations [depth= 100mm]')+
#   scale_x_continuous(breaks = seq(0, 1400,200))+
# scale_y_continuous(limits = c(0, 20), breaks = seq(0,20,5))
# ggpC
# 
# #------------Water content Plot------------------  
# #Fracture vs Matrix 
# ggpW <- ggplot(plot_d200,aes(Time))+
#   geom_point(aes(y=MoistureF, color= "Theta F"))+
#   geom_point(aes(y=Moisture, color= "Theta M"))+
#   ylab("Theta [-]")+
#   ggtitle('Water Content [depth= 200mm]')+
#   scale_x_continuous(breaks = seq(0, 1400,200))+
#   scale_y_continuous(limits= c(0, 0.20), breaks = seq(0, 0.20,0.05))
# ggpW
# 
# #------------Water Flux Plot------------------  
# #Fracture vs Matrix 
# ggpF <- ggplot(plot_d200,aes(Time))+
#   geom_point(aes(y=FluxF, color= "Fracture"))+
#   geom_point(aes(y=Flux, color= "Matrix"),alpha=0.1)+
#   ylab("Velocity [mm/hours]")+
#   ggtitle('Water Flux [depth= 200mm]')+
#   scale_x_continuous(breaks = seq(0, 1400,200))
# ggpF
# 
# #------------Pressure Head Plot------------------  
#   #Fracture vs Matrix 
#   ggpP <- ggplot(plot_d200,aes(Time))+
#   geom_point(aes(y=HeadF, color= "Fracture"))+
#   geom_point(aes(y=Head, color= "Matrix"))+
#   ylab("h [mm]")+
#   ggtitle('Pressure head [depth= 200mm]')+
#   scale_x_continuous(breaks = seq(0, 1400,200))
# ggpP
# 
# #____________________________________________________________________________
# #Method 2 Facet plots
# 
# 
# #________________________Main WET-DRY simulation 1 Solute___________________
# 
# plot_node1s<-node_data
# 
# plot_node1s$Time<-as.numeric(as.character(plot_node1s$Time))
# plot_node1s <-filter(plot_node1s, Depth == -20 |
#                      Depth == -100 |
#                      Depth==-200 |
#                      Depth==-400 )
# 
# # # Water mass Transfer curve
# # 
# plot_node %>%
#   mutate(
#     facet = paste0(" Depth = -", abs(Depth), "mm")
#   ) %>%
#   ggplot(aes(Time, Transf, color=TranS)) +
#   geom_point(alpha = 1) +
#   scale_color_viridis_c(option = "D") +
#   scale_x_continuous(limits = c(1, 1440), breaks = seq(1, 1440, 200)) +
#   labs(y = "Water MT [1/hours]")+
#   facet_wrap(vars(facet), ncol = 2, scales = "free") +
#   theme_minimal() +
#   theme(strip.text = element_text(size = 12, face = "bold"))
# 


# 
# #Pressure Heads 
# plot_node1s %>% 
#   mutate(
#     facet = paste0(" Depth = -", abs(Depth), "mm")
#   ) %>% 
#   ggplot(aes(Time)) +
#   geom_point(aes(y= HeadF, color ="Fracture"))+
#   geom_point(aes(y= Head, color ="Matrix"))+
#   labs(y= "Pressure Head [mm]")+
#   scale_x_continuous(limits = c(0, 2880), breaks = seq(0, 2880, 750)) +
#   facet_wrap(vars(facet), ncol = 2, scales = "free") +
#   theme_minimal() + 
#   theme(strip.text = element_text(size = 12, face = "bold"))
# 
# #Water contents 
# plot_node1s %>% 
#   mutate(
#     facet = paste0(" Depth = -", abs(Depth), "mm")
#   ) %>% 
#   ggplot(aes(Time)) +
#   geom_point(aes(y= MoistureF, color ="Fracture"))+
#   geom_point(aes(y= Moisture, color ="Matrix"))+
#   ylab("Theta [-]")+
#   scale_x_continuous(limits = c(750, 1000), breaks = seq(500, 1000, 100)) +
#   facet_wrap(vars(facet), ncol = 2, scales = "free") +
#   theme_minimal() + 
#   theme(strip.text = element_text(size = 12, face = "bold"))
# 
# #Fluxes
# plot_node1s %>% 
#   mutate(
#     facet = paste0(" Depth = -", abs(Depth), "mm")
#   ) %>% 
#   ggplot(aes(Time)) +
#   geom_point(aes(y= FluxF, color ="Fracture"))+
#   geom_point(aes(y= Flux, color ="Matrix"))+
#   ylab("Velocity [mm/hours]")+
#   scale_x_continuous(limits = c(0, 2880), breaks = seq(0, 2880, 750)) +
#   facet_wrap(vars(facet), ncol = 2, scales = "free") +
#   theme_minimal() + 
#   theme(strip.text = element_text(size = 12, face = "bold"))
# 
# #Solute related Curves
# options(scipen = 0)
# 
#Solute Mass Transfer
# plot_node %>%
#   mutate(
#     facet = paste0(" Depth = -", abs(Depth), "mm")
#   ) %>%
#   ggplot(aes(Time, TranS, color=Transf)) +
#   geom_line(alpha = 1) +
#   scale_color_viridis_c(option = "D") +
#   scale_x_continuous(limits = c(0, 1440), breaks = seq(0, 1440, 200)) +
#   facet_wrap(vars(facet), ncol = 2, scales = "free") +
#   theme_minimal() +
#   theme(strip.text = element_text(size = 12, face = "bold"))

# # Conc Matrix vs Time
# plot_node1s %>% 
#   mutate(
#     facet = paste0(" Depth = -", abs(Depth), "mm")
#   ) %>% 
#   ggplot(aes(Time, a, color=Transf)) +
#   geom_point(alpha = 1) +
#   ylab("Concentration Matrix [mg/mm3]")+
#   scale_color_viridis_c(option = "D") +
#   scale_x_continuous(limits = c(0, 2880), breaks = seq(0, 2880, 750)) +
#   facet_wrap(vars(facet), ncol = 2, scales = "free") +
#   theme_minimal() + 
#   theme(strip.text = element_text(size = 12, face = "bold"))

# Fracture vs Matrix
# plot_node_test<-plot_node
# plot_node_test <-filter(plot_node_test, Depth == -20 |
#                        Depth == -200 |
#                        Depth==-300 |
#                        Depth==-400 )
#Concentrtion
# plot_node_test %>% 
#   mutate(
#     facet = paste0(" Depth = -", abs(Depth), "mm")
#   ) %>% 
#   ggplot(aes(Time)) +
#   geom_point(aes(y= ConcF, color ="Fracture"))+
#   geom_point(aes(y= a, color ="Matrix"))+
#   ylab("Concentration [mg/mm3]")+
#   scale_x_continuous(limits = c(0, 600), breaks = seq(0, 600, 150)) +
#   facet_wrap(vars(facet), ncol = 2, scales = "free") +
#   theme_minimal() + 
#   theme(strip.text = element_text(size = 12, face = "bold"))

# #Water Content
# plot_node_test %>% 
#   mutate(
#     facet = paste0(" Depth = -", abs(Depth), "mm")
#   ) %>% 
#   ggplot(aes(Time)) +
#   geom_point(aes(y= MoistureF, color ="Fracture"))+
#   geom_point(aes(y= Moisture, color ="Matrix"))+
#   ylab("Theta [-]")+
#   scale_x_continuous(limits = c(0, 1000), breaks = seq(0, 1000, 200)) +
#   facet_wrap(vars(facet), ncol = 2, scales = "free") +
#   theme_minimal() + 
#   theme(strip.text = element_text(size = 12, face = "bold"))
# #Fluxes
# 
# plot_node %>% 
#   mutate(
#     facet = paste0(" Depth = -", abs(Depth), "mm")
#   ) %>% 
#   ggplot(aes(Time)) +
#   geom_point(aes(y= FluxF, color ="Fracture"))+
#   geom_point(aes(y= Flux, color ="Matrix"))+
#   scale_x_continuous(limits = c(720, 1500), breaks = seq(720, 1500, 200)) +
#   facet_wrap(vars(facet), ncol = 2, scales = "free") +
#   theme_minimal() + 
#   theme(strip.text = element_text(size = 12, face = "bold"))
# 
# #Einzel
# plot_d20 <-plot_node %>% filter(plot_node$Depth == -20)
# 
# ggpW <- ggplot(plot_d20,aes(Time))+
#     geom_line(aes(y=MoistureF, color= "Theta F"))+
#     geom_line(aes(y=Moisture, color= "Theta M"))+
#     ylab("Theta [-]")+
#     ggtitle('Water Content [depth= 20mm]')+
#   scale_x_continuous(limits = c(0, 2880), breaks = seq(0, 2880, 500))
# 
#   ggpW
#   
#   
#   #Flux
#   
#   ggpF <- ggplot(plot_d20,aes(Time))+
#     geom_line(aes(y=FluxF, color= "Fracture"))+
#     geom_line(aes(y=Flux, color= "Matrix"))+
#     ylab("v [mm/hours]")+
#     ggtitle('Water Fluxes [depth= 20mm]')+
#     scale_x_continuous(limits = c(0, 2880), breaks = seq(0, 2880, 500))
#   
#   ggpF
# 
# # #------------------------------------------------ 3 solutes-----------------
# # plot_node_test<-node_data
# # plot_node <-filter(plot_node_test, Depth == -20 |
# #                             Depth == -200 |
# #                             Depth==-300 |
# #                             Depth==-400 )
# # plot_node$Time<-as.numeric(as.character(plot_node$Time))
# # 
# # 
# # 

 #Water mass Transfer curve

plot_node %>%
  mutate(
    facet = paste0(" Depth = -", abs(Depth), "mm")
  ) %>%
  ggplot(aes(Time, Transf, color=Transf)) +
  geom_line(alpha = 1) +
  scale_color_viridis_c(option = "D") +
  scale_x_continuous(limits = c(0, 1440), breaks = seq(0,1440, 200)) +
  labs(y = "Water MT [1/hours]")+
  facet_wrap(vars(facet), ncol = 2, scales = "free") +
  theme_minimal() +
  theme(strip.text = element_text(size = 12, face = "bold"))

# # #Pressure Heads
# # plot_node %>%
# #   mutate(
# #     facet = paste0(" Depth = -", abs(Depth), "mm")
# #   ) %>%
# #   ggplot(aes(Time)) +
# #   geom_point(aes(y= HeadF, color ="Fracture"))+
# #   geom_point(aes(y= Head, color ="Matrix"))+
# #   labs(y= "Pressure Head [mm]")+
# #   scale_x_continuous(limits = c(720, 1500), breaks = seq(720, 1500, 200)) +
# #   facet_wrap(vars(facet), ncol = 2, scales = "free") +
# #   theme_minimal() +
# #   theme(strip.text = element_text(size = 12, face = "bold"))
# # 
# # #Water contents
# # plot_node %>%
# #   mutate(
# #     facet = paste0(" Depth = -", abs(Depth), "mm")
# #   ) %>%
# #   ggplot(aes(Time)) +
# #   geom_point(aes(y= MoistureF, color ="Fracture"))+
# #   geom_point(aes(y= Moisture, color ="Matrix"))+
# #   ylab("Theta [-]")+
# #   scale_x_continuous(limits = c(720, 1500), breaks = seq(720, 1500, 200)) +
# #   facet_wrap(vars(facet), ncol = 2, scales = "free") +
# #   theme_minimal() +
# #   theme(strip.text = element_text(size = 12, face = "bold"))
# # 
# # #Fluxes
# # plot_node %>%
# #   mutate(
# #     facet = paste0(" Depth = -", abs(Depth), "mm")
# #   ) %>%
# #   ggplot(aes(Time)) +
# #   geom_point(aes(y= FluxF, color ="Fracture"))+
# #   geom_point(aes(y= Flux, color ="Matrix"))+
# #   scale_x_continuous(limits = c(720, 1500), breaks = seq(720, 1500, 200)) +
# #   facet_wrap(vars(facet), ncol = 2, scales = "free") +
# #   theme_minimal() +
# #   theme(strip.text = element_text(size = 12, face = "bold"))
# # 
# # #Solute related Curves
# # options(scipen = 0)
# # 
# # #Solute Mass Transfer
# # plot_node %>%
# #   mutate(
# #     facet = paste0(" Depth = -", abs(Depth), "mm")
# #   ) %>%
# #   ggplot(aes(Time, TranS, color=Transf)) +
# #   geom_point(alpha = 1) +
# #   scale_color_viridis_c(option = "D") +
# #   scale_x_continuous(limits = c(150, 500), breaks = seq(150, 500, 100)) +
# #   facet_wrap(vars(facet), ncol = 2, scales = "free") +
# #   theme_minimal() +
# #   theme(strip.text = element_text(size = 12, face = "bold"))
# # 
# # #Each 3 solute Conc Matrix vs Time
# # plot_node %>%
# #   mutate(
# #     facet = paste0(" Depth = -", abs(Depth), "mm")
# #   ) %>%
# #   ggplot(aes(Transf)) +
# #   geom_point(aes(y= Solute1, color ="Solute1"))+
# #   geom_point(aes(y= Solute2, color ="Solute2"))+
# #   geom_point(aes(y= Solute3, color ="Solute3"))+
# #   labs(y = "Concentration [mg]")+
# #   scale_x_continuous(limits = c(0, 2880), breaks = seq(0, 2880, 1000)) +
# #   scale_y_continuous(limits = c(0, 0.1), breaks = seq(0, 0.1, 0.025)) +
# #   facet_wrap(vars(facet), ncol = 2, scales = "free") +
# #   theme_minimal() +
# #   theme(strip.text = element_text(size = 12, face = "bold"))
# # 
# # 
# # #   Fracture vs Matrix of each solute
# # 
# # plot_node %>%
# #   mutate(
# #     facet = paste0(" Depth = -", abs(Depth), "mm")
# #   ) %>%
# #   ggplot(aes(Time)) +
# #   geom_point(aes(y= ConcF, color ="Fracture"))+
# #   geom_point(aes(y= Solute3, color ="Solute3"))+ #Solute 1, 2, 3
# #   labs(y = "Concentration [mg]")+
# #   scale_x_continuous(limits = c(0, 2880), breaks = seq(0, 2880, 1000)) +
# #   facet_wrap(vars(facet), ncol = 2, scales = "free") +
# #   theme_minimal() +
# #   theme(strip.text = element_text(size = 12, face = "bold"))
# # 
