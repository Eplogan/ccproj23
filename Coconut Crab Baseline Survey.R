
#..COCONUT CRAB BASELINE SURVEY: DATA ANALYSIS 

#..Installing packages needed..#

setwd("~/Desktop/rwd/projects/ccproj23")
getwd()
install.packages('FSA')
#install.packages('FSAdata')
install.packages('dplyr')
install.packages('ggplot2')
install.packages('tidyr')
install.packages('magrittr')
install.packages('Matching')
install.packages('tidyverse')
install.packages('magrittr')
install.packages('Matching')
install.packages('plotrix')

library(FSA)
#library(FSAdata)
library(dplyr)
library(ggplot2)
library(tidyr)
library(magrittr)
library(Matching)
library(tidyverse)
library(magrittr)
library(Matching)
library(plotrix)

  
#29/10/21.....
#...Combined all observation data..#

comb_ob <- read.csv("Comb_Al_Obs.csv", header = T)
  
View(comb_ob)
glimpse(comb_ob)

#..01/11/21..#
#..combined data..#
comb_ob %>% 
  dplyr::select(site,sex,TL,Wgt,CWgt,count) %>%
  filter(sex %in%c("M","F")) %>% 
  group_by(site, sex) %>% 
  summarise(mean_tl = mean(TL), mean_w = mean(CWgt), abund = n(), .groups = "keep")%>%
  view() -> r1

comb_ob %>% 
  dplyr::select(site,sex,TL,Wgt,CWgt,count) %>%
  filter(sex %in%c("M","F")) %>%
  group_by(site, sex) %>% 
  summarise(mean_tl = mean(TL), mean_w = mean(CWgt), abund = n(),
            sd_tl = sd(TL), sd_w = sd(CWgt),
            se_tl=(sd_tl/(sqrt(abund))),
            se_w=(sd_w/(sqrt(abund))), .groups = "keep")%>%
  ggplot(aes(sex, abund)) +
  geom_bar(stat = "identity", position = "dodge", aes(fill=sex)) +
  facet_wrap(~site) +
  theme_bw() + theme(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     legend.position = "none") +
  labs(title = "Total Crab per Site",
       x = "Sex (M - male, F- female)",
       y = "Number of Indiv")
  

comb_ob %>% 
  dplyr::select(site,sex,TL,Wgt,CWgt,count) %>%
  filter(sex %in%c("M","F")) %>%
  group_by(site, sex) %>% 
  summarise(mean_tl = mean(TL), mean_w = mean(CWgt), abund = n(),
            sd_tl = sd(TL), sd_w = sd(CWgt),
            se_tl=(sd_tl/(sqrt(abund))),
            se_w=(sd_w/(sqrt(abund))), .groups = "keep")%>%
  ggplot(aes(sex,mean_tl))+  
  geom_bar(stat = "identity", 
           position = "dodge",
           aes(fill=sex), 
           alpha=1.5) + 
  geom_errorbar(aes(ymin=mean_tl - se_tl, 
                    ymax=mean_tl+ se_tl),
                width=.25, 
                position = position_dodge(),
                colour="navy blue") +
  facet_wrap(~site) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none") +
  labs(title = "Non-Prohibited Sites",
       x = "Sex",
       y = "Average TL (mm)")


comb_ob %>% 
  dplyr::select(site,sex,TL,Wgt,CWgt,count) %>%
  filter(sex %in%c("M","F")) %>%
  group_by(site, sex) %>%
  summarise(mean_tl = mean(TL), 
            mean_w = mean(CWgt),
            abund = n(),
            sd_tl = sd(TL), 
            sd_w = sd(CWgt),
            se_tl=(sd_tl/(sqrt(abund))),
            se_w=(sd_w/(sqrt(abund))),
            .groups = "keep")%>%
  ggplot(aes(sex, 
             mean_tl, 
             fill=sex,
             ymin=mean_tl - se_tl, 
             ymax=mean_tl+ se_tl))+
  facet_wrap(~site)+
  geom_bar(stat = "identity", 
           position = position_dodge(.4),
           aes(fill=sex), 
           width = .9, 
           alpha=.5,
           colour = "black",
           key_glyph="polygon") + 
  geom_errorbar(width=.2,
                position = position_dodge(.5),
                colour="black")+
  scale_fill_manual(values = c('#A3A3A3', "#262626"))+
  scale_y_continuous(expand = expansion(0),
                     limits = c(0, 50),
                     breaks = seq(0,50,5)) + 
  theme_bw()+
  labs(title = "",
       x = "Sex",
       y = "Average TL (mm)",
       fill=NULL)+
  theme(
    plot.margin = unit(c(1,1,1,1), "cm"),
    plot.title = element_text(size = 20,
                              colour = "black",
                              face = "bold",
                              hjust = 0.5,
                              margin = margin(15)),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.title = element_text(size = 16,
                              colour = "black"),
    axis.text.x.bottom = element_text(margin = margin(10)),
    axis.title.y.right = element_text(margin = margin(20)),
    axis.text.y.left = element_text(size = 12, 
                                    colour = "black"),
    axis.text.x = element_text(size = 14,
                              colour = "black"),
    axis.ticks.x = element_blank(),
    legend.background = element_rect(colour = "black"),
    legend.text = element_text(size = 14),
    legend.margin = margin(t=2, r=2, l=2, b=2))

ggsave("Mean_TL.png", width = 10, height = 10, dpi = 300)

###...script 2> without facet_wrap....##
comb_ob %>% 
  dplyr::select(site,sex,TL,Wgt,CWgt,count) %>%
  filter(sex %in%c("M","F")) %>%
  group_by(site, sex) %>%
  summarise(mean_tl = mean(TL), 
            mean_w = mean(CWgt),
            abund = n(),
            sd_tl = sd(TL), 
            sd_w = sd(CWgt),
            se_tl=(sd_tl/(sqrt(abund))),
            se_w=(sd_w/(sqrt(abund))),
            .groups = "keep")%>%
  ggplot(aes(site, 
             mean_tl,
             ymin=mean_tl - se_tl, 
             ymax=mean_tl+ se_tl,))+
  geom_col(width=.8,
           position = position_dodge(.91),
           aes(fill=sex),
           colour = "#18181B") + 
  geom_errorbar(position = position_dodge(.91),
                aes(fill=sex),
                width = .2)+
  scale_fill_manual(values = c('#E7E5E4', "#78716C"))+
  scale_y_continuous(expand = expansion(0),
                     limits = c(0, 45),
                     breaks = seq(0,45,5))+
  theme_bw()+
  labs(title = "",
       x = "Sex",
       y = "Average TL (mm)",
       fill=NULL)+
  theme(
    plot.margin = unit(c(1,1,1,1), "cm"),
    plot.title = element_text(size = 20,
                              colour = "black",
                              face = "bold",
                              hjust = 0.5,
                              margin = margin(10)),
    axis.text.x.bottom = element_text(margin = margin(5)),
    axis.title.y.right = element_text(margin = margin(10)),
    axis.title.x = element_text(margin = margin(10)),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.title = element_text(size = 16,
                              colour = "black"),
    axis.text.y.left = element_text(size = 12, 
                                    colour = "black"),
    axis.text.x = element_text(size = 12,
                               colour = "black"),
    axis.ticks.x = element_blank(),
    legend.background = element_rect(colour = "black"),
    legend.text = element_text(size = 14),
    legend.position = c(0.06, 0.9),
    legend.margin = margin(t=2, r=2, l=2, b=2))

ggsave("Mean_TL_Col.png", width = 10, height = 10, dpi = 300)

#...Cross checing mean values with excel database...##
comb_ob %>% 
  dplyr::select(site,sex,TL,Wgt,CWgt,count) %>%
  filter(site=="Yacata Island", sex %in%c("M","F")) %>% 
  group_by(site, sex) %>% 
  summarise(mean_tl = mean(TL), 
            mean_w = mean(CWgt),
            abund = n())%>% view() 

#Exploring Yacata island data
comb_ob %>% 
  dplyr::select(site,sex,TL,Wgt,CWgt,count) %>%
  filter(site=="Yacata Island") %>% view() 
  


##..abundance graph ...###
comb_ob %>% 
  dplyr::select(site,sex,TL,Wgt,CWgt,count) %>%
  filter(sex %in%c("M","F")) %>%
  group_by(site, sex) %>%
  summarise(mean_tl = mean(TL), 
            mean_w = mean(CWgt),
            abund = n(),
            sd_tl = sd(TL), 
            sd_w = sd(CWgt),
            se_tl=(sd_tl/(sqrt(abund))),
            se_w=(sd_w/(sqrt(abund))),
            .groups = "keep")%>% 
  ggplot(aes(site, abund))+
  geom_col(aes(fill=sex),
           position = position_dodge(.9), 
           width = .8,
           colour = "black") +
  scale_fill_manual(values = c('#D6D3D1', "#78716C"))+
  scale_y_continuous(expand = expansion(0),
                     limits = c(0, 1500),
                     breaks = seq(0,1500,500)) +
  theme_bw()+
  labs(title = "",
       x = "Site",
       y = "N.o.# of Indiv",
       fill=NULL)+
  theme(
    plot.margin = unit(c(1,1,1,1), "cm"),
    plot.title = element_text(size = 20,
                              colour = "black",
                              face = "bold",
                              hjust = 0.5,
                              margin = margin(10)),
    axis.text.x.bottom = element_text(margin = margin(5)),
    axis.title.y.right = element_text(margin = margin(10)),
    axis.title.x = element_text(margin = margin(10)),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.title = element_text(size = 16,
                              colour = "black"),
    axis.text.y.left = element_text(size = 12, 
                                    colour = "black"),
    axis.text.x = element_text(size = 12,
                               colour = "black"),
    axis.ticks.x = element_blank(),
    legend.background = element_rect(colour = "black"),
    legend.text = element_text(size = 14),
    legend.position = c(0.06, 0.9),
    legend.margin = margin(t=2, r=2, l=2, b=2))
    
ggsave("Abuind_bw_1.png", width = 10, height = 10, dpi = 300)  

##...Mean weight....##

comb_ob %>% 
  dplyr::select(site,sex,TL,Wgt,CWgt,count) %>%
  filter(sex %in%c("M","F")) %>%
  group_by(site, sex) %>%
  summarise(mean_tl = mean(TL), 
            mean_w = mean(CWgt),
            abund = n(),
            sd_tl = sd(TL), 
            sd_w = sd(CWgt),
            se_tl=(sd_tl/(sqrt(abund))),
            se_w=(sd_w/(sqrt(abund))),
            .groups = "keep")%>% 
  ggplot(aes(site, 
             mean_w,
             ymin=mean_w - se_w, 
             ymax=mean_w+ se_w,))+
  geom_col(width=.8,
           position = position_dodge(.91),
           aes(fill=sex),
           colour = "#18181B")+
  geom_errorbar(position = position_dodge(.91),
                aes(fill=sex),
                width = .2)+
  scale_fill_manual(values = c('#E7E5E4', "#78716C"))+
  scale_y_continuous(expand = expansion(0),
                     limits = c(0, 700),
                     breaks = seq(0,700,100))+
  theme_bw()+
  labs(title = "",
       x = "Site",
       y = "Average Weight (g)",
       fill=NULL)+
  theme(
    plot.margin = unit(c(1,1,1,1), "cm"),
    plot.title = element_text(size = 20,
                              colour = "black",
                              face = "bold",
                              hjust = 0.5,
                              margin = margin(10)),
    axis.text.x.bottom = element_text(margin = margin(5)),
    axis.title.y.right = element_text(margin = margin(10)),
    axis.title.x = element_text(margin = margin(10)),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.title = element_text(size = 16,
                              colour = "black"),
    axis.text.y.left = element_text(size = 12, 
                                    colour = "black"),
    axis.text.x = element_text(size = 12,
                               colour = "black"),
    axis.ticks.x = element_blank(),
    legend.background = element_rect(colour = "black"),
    legend.text = element_text(size = 14),
    legend.position = c(0.06, 0.9),
    legend.margin = margin(t=2, r=2, l=2, b=2))

ggsave("Mean_Wght_Col.png", width = 10, height = 10, dpi = 350)


##..length histograms ...#
data <- read.csv("Comb_Al_Obs.csv", header = T)
view(data)

levels(data$sex)
data %>% 
  dplyr::select(site,sex,TL)%>%
  filter( site%in%c("Vatuvara Island", "Kaibu Island"), 
          sex%in%c("F", "M"))%>% 
  mutate(lcat5=lencat(TL, w=4)) ->tl_lcat5
  

view(tl_lcat5)
rm(tl_freq2)

#...histogram stacked by site...#
ggplot(tl_lcat5, aes(lcat5, fill = site))+
  geom_histogram(binwidth = 5,
                 bins = 5,
                 colour="black",
                 alpha=.5,
                 position = 'stack')+
  scale_fill_manual(values = c("#0284C7","#0D9488"))


#...histogram dodged by site...#
#..Exploring cc weight data
ggplot(tl_lcat5, aes(lcat5, fill = site))+
  geom_histogram(binwidth = 4,
                 bins = 15,
                 colour="black",
                 alpha=.5,
                 position = 'dodge')+
  scale_fill_manual(values = c("#0284C7","#D97706"))+
  scale_y_continuous(expand = expansion(0),
                     limits = c(0, 300),
                     breaks = seq(0,300,50))+
  theme_bw()+facet_wrap(~sex)

data_new <- data
levels(data_new$sex)

levels(data_new$sex)
str(data_new)


#..05/10/10..#
comb_ob <- AllObs

data_st <- read.csv("comb_st_data.csv", header = T)

headtail(data_st, 20)

data_st %>% 
  dplyr::select(site,station,bait,zonetype,foresttype,visit,sex,tl,ccl,wgt,cwtl,count) %>%
  filter(sex%in%c("F", "M")) %>%
  group_by(site,foresttype)%>%
  drop_na(tl,cwtl)%>%
  summarise(mean_tl = mean(tl), 
            mean_w = mean(cwtl),
            mean_b=mean(bait),
            abund = n(),
            sd_tl = sd(tl), 
            sd_w = sd(cwtl),
            se_tl=(sd_tl/(sqrt(abund))),
            se_w=(sd_w/(sqrt(abund))),
            .groups = "keep")%>%view()

data_st %>% 
  dplyr::select(site,station,zonetype,foresttype,visit,sex,tl,ccl,wgt,cwtl,count) %>%
  filter(sex%in%c("F", "M")) %>%
  group_by(site,foresttype,station)%>%
  drop_na(tl,cwtl)%>%
  summarise(mean_tl = mean(tl), 
            mean_w = mean(cwtl),
            abund = n(),
            sd_tl = sd(tl), 
            sd_w = sd(cwtl),
            se_tl=(sd_tl/(sqrt(abund))),
            se_w=(sd_w/(sqrt(abund))),
            .groups = "keep")%>% view()



ggplot(tl_lcat5, aes(lcat5, fill = site))+
  geom_histogram(binwidth = 4,
                 bins = 15,
                 colour="black",
                 alpha=.5,
                 position = 'dodge')+
  scale_fill_manual(values = c("#0284C7","#D97706"))+
  scale_y_continuous(expand = expansion(0),
                     limits = c(0, 300),
                     breaks = seq(0,300,50))+
  theme_bw()+facet_wrap(~sex)


#..08/11/21....#
##..>> CPUE calculation..>>
data_st <- read.csv("comb_st_data.csv", header = T)
view(data_st)



data_st %>% 
  dplyr::select(site,station,bait,zonetype,foresttype,visit,sex,tl,ccl,wgt,cwtl,count) %>%
  filter(sex%in%c("F", "M")) %>%
  group_by(site,zonetype,foresttype,station)%>%
  drop_na(tl,cwtl)%>%
  mutate(bait)%>%
  summarise(mean_tl = mean(tl), 
            mean_w = mean(cwtl),
            abund = n(),
            mean_b = mean(bait),
            sd_tl = sd(tl), 
            sd_w = sd(cwtl),
            se_tl=(sd_tl/(sqrt(abund))),
            se_w=(sd_w/(sqrt(abund))),
            .groups = "keep")%>%
  drop_na(sd_tl,sd_w,se_tl,se_w)%>%
  view()


data_st %>% 
  dplyr::select(site,station,bait,zonetype,foresttype,visit,sex,tl,ccl,wgt,cwtl,count) %>%
  filter(sex%in%c("F", "M")) %>%
  group_by(site,zonetype,foresttype,station)%>%
  drop_na(tl,cwtl)%>%
  mutate(bait)%>%
  summarise(mean_tl = mean(tl), 
            mean_w = mean(cwtl),
            abund = n(),
            mean_b = mean(bait),
            sd_tl = sd(tl), 
            sd_w = sd(cwtl),
            se_tl=(sd_tl/(sqrt(abund))),
            se_w=(sd_w/(sqrt(abund))),
            .groups = "keep")%>%
  drop_na(sd_tl,sd_w,se_tl,se_w)-> data_reg_st

data_st %>% 
  dplyr::select(site,station,bait,zonetype,foresttype,visit,sex,tl,ccl,wgt,cwtl,count) %>%
  filter(sex%in%c("F", "M")) %>%
  group_by(site,zonetype,foresttype)%>%
  drop_na(tl,cwtl)%>%
  mutate(bait)%>%
  summarise(mean_tl = mean(tl), 
            mean_w = mean(cwtl),
            abund = n(),
            mean_b = mean(bait),
            sd_tl = sd(tl), 
            sd_w = sd(cwtl),
            se_tl=(sd_tl/(sqrt(abund))),
            se_w=(sd_w/(sqrt(abund))),
            .groups = "keep")%>%
  drop_na(sd_tl,sd_w,se_tl,se_w)-> data_zone

data_st %>% 
  dplyr::select(site,station,bait,zonetype,foresttype,visit,sex,tl,ccl,wgt,cwtl,count) %>%
  filter(sex%in%c("F", "M")) %>%
  group_by(site,zonetype,foresttype,station)%>%
  drop_na(tl,cwtl)%>%
  mutate(bait)%>%
  summarise(abund = n(),
            mean_b = mean(bait),
            .groups = "keep")%>%
  mutate(cpue=abund/mean_b) %>%
  view()

data_st %>% 
  dplyr::select(site,station,bait,zonetype,foresttype,visit,sex,tl,ccl,wgt,cwtl,count) %>%
  filter(sex%in%c("F", "M")) %>%
  group_by(site,zonetype,foresttype,station)%>%
  drop_na(tl,cwtl)%>%
  mutate(bait)%>%
  summarise(abund = n(),
            mean_b = mean(bait),
            .groups = "keep")%>%
  mutate(cpue=abund/mean_b) -> cpue_st 

write.csv(cpue_st, 'cpue_st.csv', row.names = T)



#>>>>>> 09/1121....##
data_st <- read.csv("comb_st_data.csv", header = T)

data_st %>% 
  dplyr::select(site,station,bait,zonetype,foresttype,visit,sex,tl,ccl,wgt,cwtl,count) %>%
  filter(sex%in%c("F", "M")) %>%
  drop_na(tl,cwtl)%>%
  group_by(site,zonetype,foresttype,station)%>%
  summarise(mean_tl = mean(tl), 
            mean_w = mean(cwtl),
            abund = n(),
            mean_b = mean(bait),
            sd_tl = sd(tl), 
            sd_w = sd(cwtl),
            se_tl=(sd_tl/(sqrt(abund))),
            se_w=(sd_w/(sqrt(abund))),
            .groups = "keep")%>%
  drop_na(sd_tl,sd_w,se_tl,se_w)%>%
  view()

data_st %>% 
  dplyr::select(site,station,bait,zonetype,foresttype,visit,sex,tl,ccl,wgt,cwtl,count) %>%
  filter(sex%in%c("F", "M")) %>%
  drop_na(tl,cwtl)%>%
  group_by(site,zonetype,foresttype,station)%>%
  summarise(mean_tl = mean(tl), 
            mean_w = mean(cwtl),
            abund = n(),
            mean_b = mean(bait),
            sd_tl = sd(tl), 
            sd_w = sd(cwtl),
            se_tl=(sd_tl/(sqrt(abund))),
            se_w=(sd_w/(sqrt(abund))),
            .groups = "keep")%>%
  drop_na(sd_tl,sd_w,se_tl,se_w)->dat_reg_st

view(dat_reg_st)
mutate(dat_reg_st, cpue = abund/mean_b) -> dat_re_st_cpue

data_cpue <-dat_re_st_cpue 

view(data_cpue)

write.csv(dat_reg_st, "data_cpue.csv", row.names = T)

view(data_cpue)
rm(dat_reg_st)
rm(dat_re_st_cpue)
rm(data_reg)


data_reg <- data_st %>% 
  dplyr::select(site,station,bait,zonetype,foresttype,
                visit,sex,tl,ccl,wgt,cwtl,count) %>%
  filter(sex%in%c("F", "M")) %>%
  drop_na(tl,cwtl)%>%
  group_by(site,zonetype,foresttype)%>%
  summarise(mean_tl = mean(tl), 
            mean_w = mean(cwtl),
            abund = n(),
            mean_b = mean(bait),
            sd_tl = sd(tl), 
            sd_w = sd(cwtl),
            se_tl=(sd_tl/(sqrt(abund))),
            se_w=(sd_w/(sqrt(abund))),
            .groups = "keep")%>%
  drop_na(sd_tl,sd_w,se_tl,se_w)%>%
  view()

view(data_reg)
write.csv(data_reg, "data_reg.csv", row.names = T)
data_reg <- read.csv("data_reg.csv", header = T)
view(data_reg)

data_st %>% 
  dplyr::select(site,station,bait,zonetype,foresttype,visit,sex,tl,ccl,wgt,cwtl,count) %>%
  filter(sex%in%c("F", "M")) %>%
  drop_na(tl,cwtl)%>%
  group_by(site,foresttype)%>%
  summarise(mean_tl = mean(tl), 
            mean_w = mean(cwtl),
            abund = n(),
            mean_b = mean(bait),
            sd_tl = sd(tl), 
            sd_w = sd(cwtl),
            se_tl=(sd_tl/(sqrt(abund))),
            se_w=(sd_w/(sqrt(abund))),
            .groups = "keep")->data_forest
view(data_reg)
write.csv(data_forest, "data_forest.csv", row.names = T)

data_forest <- read.csv("data_forest.csv", header = T)


headtail(data_forest,3)
view(data_forest)

#..creating a new total bait column..#
data_reg <- mutate(data_reg, totalb = 
         c(60,60,20,80,40,60,80,80,40,20,40))+view(data_reg)
