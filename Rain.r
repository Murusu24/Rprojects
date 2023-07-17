# Load necessary libraries
library(tidyverse)
library(readxl)

# Read in data from first file
data0.8 <- read_excel("C:/Users/Muru/downloads/0.8.xlsx")

# Read in data from second file
data4 <- read_excel("C:/Users/Muru/downloads/4.xlsx")
data2 <- read_excel("C:/Users/Muru/downloads/2.xlsx")

# Combine data into one dataframe

# Create bar chart of precipitation levels by file
values=c("#465a0a","#85eff5","#a87808")

ggplot(NULL, aes(x = Time))+
 geom_line(data=data4,aes(y=Precepitation, colour='0.4'))+
   
  geom_line(data=data2,aes(y=Precepitation, colour='0.2 '))+
  geom_line(data=data0.8,aes(y=Precepitation, colour='0.8 '))+
  theme(legend.position = "top")+
  labs(colour="intensity[mm/hour]")
   
  
  ggplot(NULL, aes(x = Time )) +
  geom_line(data= data2,aes(y = Precepitation, colour = '4'))+
  scale_y_continuous(breaks = c(0.8,2,3,4))+
  scale_x_continuous(limits=c(0,1440), breaks=seq(0,1440,200))+
  geom_line(data= data1,aes(y = Precepitation, colour = '0.8'))+
  scale_color_manual(values = c("#5b3db5","#4ca596"))+
  ggtitle("Rainfall distribution")+
  labs(colour="intensity[mm/hour]")+
  theme(strip.text = element_text(size = 45, face = "bold"))+
# 
  ylab("Precepitation [mm/hour]")
                      