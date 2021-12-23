library(tidyverse)
library(ggplot2)
library(patchwork)

mytheme <-  theme(plot.title = element_text(face = "bold",color = "#555859", hjust = 0.5, vjust = -0.25,size = 14),
                  axis.line = element_line(size = 1),
                  panel.border = element_blank(),
                  panel.background = element_rect(fill = "transparent"), # bg of the panel
                  plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
                  panel.grid.major = element_blank(), # get rid of major grid
                  panel.grid.minor = element_blank(), # get rid of minor grid
                  legend.background = element_rect(fill = "transparent"), # get rid of legend bg
                  legend.box.background = element_rect(fill = "transparent"), 
                  axis.title = element_blank(),
                  axis.text = element_text(size = rel(0.8), colour = "black"),
                  legend.position="bottom",
                  legend.title = element_blank()
                  )
                  # get rid of legend bg
# legend.box.background = element_rect(fill = "#555859") # get rid of legend panel bg


dat_2 <- read_delim("actions.csv", "\t", 
                    escape_double = FALSE, trim_ws = TRUE)
bargraph7 <- ggplot(data = dat_2, aes(x =Indicator, y =Perc, fill = Refugee_Status))+
  geom_bar(stat = "identity",position = "dodge", colour="black")+
  coord_flip()+
  scale_y_continuous(expand = c(0, 0),limits = c(0, 102))+
  # scale_fill_manual(values=c("#eb5859", "#315975"))+
  geom_text(aes(label = paste0(Perc,"%"),hjust = -0.2,vjust = 0.3),size=3.5,
            position = position_dodge(width = 1.0))+
  
  mytheme+
  scale_fill_manual(values = c("#eb5859", "#315975"))

# labs(title = "Distribution of Marital status per gender",
#      x = "Marital status",
#      y ="percentage")
bargraph7

ggsave(bargraph7, filename = "bargraph7.png",  bg = "transparent")

library(readr)
dat_3 <- read_delim("priorty.csv", "\t", 
                    escape_double = FALSE, trim_ws = TRUE)

bargraph8 <- ggplot(data = dat_3, aes(x =Indicator, y =Perc, fill = Refugee_Status))+
  geom_bar(stat = "identity",position = "dodge", colour="black")+
  coord_flip()+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 102))+
  # scale_fill_manual(values=c("#eb5859", "#315975"))+
  geom_text(aes(label = Perc, hjust = -0.2,vjust = 0.3),size=3.5,
            position = position_dodge(width = 1.0))+
  
  mytheme+
  scale_fill_manual(values = c("#eb5859", "#315975"))+
  labs(title = "% of HHs Reporting Top 3 Priorities AFTER May Escalation")
#      x = "Marital status",
#      y ="percentage")
bargraph8

ggsave(bargraph8, filename = "bargraph8.png",  bg = "transparent")



library(readr)
dat_4 <- read_delim("shocks.csv", "\t", 
                    escape_double = FALSE, trim_ws = TRUE)

dat_4$Indicator = str_wrap(dat_4$Indicator, width = 10)

bargraph9 <- ggplot(data = dat_4, aes(x =Indicator, y =Perc, fill = Refugee_Status))+
  geom_bar(stat = "identity",position = "dodge", colour="black")+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 70))+
  # scale_fill_manual(values=c("#eb5859", "#315975"))+
  geom_text(aes(label = Perc, hjust = 0.3,vjust = -0.4),size=3.5,
            position = position_dodge(width = 1.0))+
  
  mytheme+
  scale_fill_manual(values = c("#eb5859", "#315975"))+
  labs(title = "% OF HHs BY TYPE OF SHOCK EXPERIENCED DURING MPCA")
#      x = "Marital status",
#      y ="percentage")
bargraph9

ggsave(bargraph9, filename = "bargraph9.png",  bg = "transparent")

library(readxl)
dat_5 <- read_excel("joint.xlsx") %>% 
  mutate(Perc = round(Refugee*100,0))



bargraph10 <- ggplot(data=dat_5, aes(x=Indicator, y=Perc, fill=Sub_indicator)) +
  geom_bar(stat="identity", color = "black")+
  geom_text(aes(label = Perc, group = Sub_indicator),
            position = position_stack(vjust = .5))+
  scale_y_continuous(expand = c(0, 0))+
  coord_flip()+
  scale_fill_manual(values = c("#eb5859", "#315975", "#555859"))+
  mytheme +theme(legend.position = "none")
bargraph10


ggsave(bargraph10, filename = "bargraph10.png",  bg = "transparent")

library(readxl)
dat_6 <- read_excel("joint.xlsx") %>% 
  mutate(Perc = round(Non_refugee*100,0))



bargraph11 <- ggplot(data=dat_6, aes(x=Indicator, y=Perc, fill=Sub_indicator)) +
  geom_bar(stat="identity", color = "black")+
  geom_text(aes(label = Perc, group = Sub_indicator),
            position = position_stack(vjust = .5))+
  scale_y_continuous(expand = c(0, 0))+
  coord_flip()+
  scale_fill_manual(values = c("#eb5859", "#315975", "#555859"))+
  mytheme +theme(legend.position = "none")
bargraph11

ggsave(filename = "bargraph11.png",
       plot = bargraph11,
       width = 10, height = 3.5, dpi = 100, units = "in", device='png')


ggsave(bargraph11, filename = "bargraph11.png",  bg = "transparent")

dat_7 <- read_excel("coping refugee.xlsx")

dat_7$Indicator = str_wrap(dat_7$Indicator, width = 20)


bargraph12 <- ggplot(data = dat_7, aes(x =Indicator, y =Perc, fill = Survey))+
  geom_bar(stat = "identity",position = "dodge", colour="black")+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 104))+
  # scale_fill_manual(values=c("#eb5859", "#315975"))+
  geom_text(aes(label = Perc, hjust = 0.3,vjust = -0.4),size=4,
            position = position_dodge(width = 1.0))+
  
  mytheme+theme(legend.position = "none")+
  scale_fill_manual(values = c("#eb5859", "#315975","#555859"))+
  labs(title = "% of Refugee HH Resorting to Negative Coping Mechanisms")
#      x = "Marital status",
#      y ="percentage")
bargraph12

ggsave(bargraph12, filename = "bargraph12.png",  bg = "transparent")

ggsave(filename = "bargraph12.png",
       plot = bargraph12,
       width = 15, height = 4, dpi = 100, units = "in", device='png')

library(patchwork)


dat_8 <- read_excel("coping refugee_non.xlsx")

dat_8$Indicator = str_wrap(dat_8$Indicator, width = 20)


bargraph13 <- ggplot(data = dat_8, aes(x =Indicator, y =Perc, fill = Survey))+
  geom_bar(stat = "identity",position = "dodge", colour="black")+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 102))+
  # scale_fill_manual(values=c("#eb5859", "#315975"))+
  geom_text(aes(label = Perc, hjust = 0.3,vjust = -0.4),size=4,
            position = position_dodge(width = 1.0))+
  
  mytheme+theme(legend.justification = "top")+
  scale_fill_manual(values = c("#eb5859", "#315975","#555859"))+
  labs(title = "% of  Non Refugee HH Resorting to Negative Coping Mechanisms")
#      x = "Marital status",
#      y ="percentage")
bargraph13

ggsave(filename = "bargraph13.png",
       plot = bargraph13,
       width = 15, height = 4, dpi = 100, units = "in", device='png')


bargraph13 <-ggsave(bargraph13, filename = "bargraph13.png",  bg = "transparent")

ggsave(filename = "bargraph13.png",
       plot = bargraph13,
       width = 15, height = 7, dpi = 100, units = "in", device='png')

window2 <- bargraph12 / bargraph13
window2

ggsave(filename = "window2.png",
       plot = window2,
       width = 15, height = 10, dpi = 100, units = "in", device='png')





dat_9 <- read_excel("needs_non.xlsx")

bargraph14 <- ggplot(data = dat_9, aes(x =Indicator, y =Perc, fill = Survey))+
  geom_bar(stat = "identity",position = "dodge", colour="black")+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 102))+
  # scale_fill_manual(values=c("#eb5859", "#315975"))+
  geom_text(aes(label = Perc, hjust = 0.3,vjust = -0.4),size=3.5,
            position = position_dodge(width = 1.0))+
  
  mytheme+theme(legend.justification = "top")+
  scale_fill_manual(values = c("#eb5859", "#315975","#555859"))+
  labs(title = "% of Non-Refugee Households mostly or fully able to Meet their Basic Needs")
#      x = "Marital status",
#      y ="percentage")
bargraph14

ggsave(filename = "bargraph14.png",
       plot = bargraph14,
       width = 15, height = 4, dpi = 100, units = "in", device='png')

dat_10 <- read_excel("needs_r.xlsx")

bargraph15 <- ggplot(data = dat_10, aes(x =Indicator, y =Perc, fill = Survey))+
  geom_bar(stat = "identity",position = "dodge", colour="black")+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 102))+
  # scale_fill_manual(values=c("#eb5859", "#315975"))+
  geom_text(aes(label = Perc, hjust = 0.3,vjust = -0.4),size=3.5,
            position = position_dodge(width = 1.0))+
  
  mytheme+theme(legend.justification = "top")+
  scale_fill_manual(values = c("#eb5859","#555859","#315975"))+
  labs(title = "% of Refugee Households mostly or fully able to Meet their Basic Needs")
#      x = "Marital status",
#      y ="percentage")
bargraph15

ggsave(filename = "bargraph15.png",
       plot = bargraph15,
       width = 15, height = 4, dpi = 100, units = "in", device='png')

window3 <- bargraph14 / bargraph15
window3

