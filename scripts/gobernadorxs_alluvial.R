######################################################################
# Alluvial diagram de gubernatura en México (desde 1985)
#
#Elaborado por: Máximo Ernesto Jaramillo-Molina 
#               Twitter: @rojo_neon
#               Github: @rojoneon
######################################################################

rm(list = ls())

#Tutorial
#https://cran.r-project.org/web/packages/ggalluvial/vignettes/ggalluvial.html

#Base de datos en https://bit.ly/2U2lh1P

### Instalar una paquetería que facilita apertura de paqueterías: pacman
#install.packages("pacman") 
library(pacman)

# Abrimos las paqueterías con un sólo comando:
p_load(ineq, haven, readr, readxl, ggplot2, shiny, tidyverse, expss, DescTools, lmtest, ggalluvial)

theme_g <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "Verdana",
                          color = "#939486"),
      panel.grid.major = element_line(color = "#F5F5F3", size = 0.2),
      panel.grid.minor = element_blank(),
      # background colors
      plot.background = element_rect(fill = "#F5F5F3",
                                     color = NA),
      panel.background = element_rect(fill = "#F5F5F3",
                                      color = NA),
      legend.background = element_rect(fill = "#F5F5F3",
                                       color = NA),
      axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=.5)
    )
}


### Setup ----
options(scipen=999) # Prevenir notación científica
#-----------------


base_gobernadores <- read_csv(file = "./www/data/base_gobernadores.csv")
glimpse(base_gobernadores)



#####2019-2022
glimpse(base_gobernadores)

guber_alluvial2 <-
  base_gobernadores %>% 
  dplyr::select(`2022`,`2019`) %>% 
  mutate( n=1) %>% 
  group_by(`2022`,`2019`)  %>% 
  summarise(n=sum(n))

guber_alluvial2$`2022`=factor(guber_alluvial2$`2022`,
                              levels = c("PRI", "PAN", "PRD", "PVEM", "MC", "MORENA", "INDEPENDIENTE", "PES"),
                              labels = c("PRI", "PAN", "PRD", "PVEM", "MC", "MORENA", "INDEP.", "PES"))

guber_alluvial2$`2019`=factor(guber_alluvial2$`2019`,
                              levels = c("PRI", "PAN", "PRD", "PVEM", "MC", "MORENA", "INDEPENDIENTE", "PES"),
                              labels = c("PRI", "PAN", "PRD", "PVEM", "MC", "MORENA", "INDEP.", "PES"))

glimpse(guber_alluvial2)

guber_alluvial2 %>% 
  ggplot(aes(y = n, axis1 = `2019`, axis2 = `2022`,
             fill = after_stat(stratum),
             label = after_stat(stratum))) +
  geom_flow(width = 1/12, alpha = .5) +
  # geom_alluvium(
  #   aes(fill = `2016`),
  #   width = 1/12, alpha = .5) +
  geom_stratum(width = 1/4, fill = "#F5F5F3", color = "#939486") +
  geom_text(stat = "stratum", size = 4) +
  scale_x_discrete(limits = c("2019", "2022"),
                   expand = c(.05, .05)) +
  scale_y_continuous(breaks = seq(0,32,4)) +
  scale_fill_manual(values = c(
    "#E63946", "#118AB2", "#E9C46A", "#A8DADC", "#FFBF69", "#6A040F", "#ADB5BD", "#DEE2FF")) +
  theme_g() +
  theme(legend.position = "none") +
  ggtitle("Gubernaturas en México por partido") +
  labs(y = "Gubernaturas") 


ggsave(filename = "Alluvial_2019_2022.png", path= "./www/plots/",
       dpi = 320, width = 8.5, height = 7.5,
       bg = "transparent")


#####2016-2019-2022
glimpse(base_gobernadores)

guber_alluvial2 <-
  base_gobernadores %>% 
  dplyr::select(`2022`,`2019`,`2016`) %>% 
  mutate( n=1) %>% 
  group_by(`2022`,`2019`,`2016`)  %>% 
  summarise(n=sum(n))

guber_alluvial2$`2022`=factor(guber_alluvial2$`2022`,
                             levels = c("PRI", "PAN", "PRD", "PVEM", "MC", "MORENA", "INDEPENDIENTE", "PES"),
                             labels = c("PRI", "PAN", "PRD", "PVEM", "MC", "MORENA", "INDEP.", "PES"))

guber_alluvial2$`2019`=factor(guber_alluvial2$`2019`,
                             levels = c("PRI", "PAN", "PRD", "PVEM", "MC", "MORENA", "INDEPENDIENTE", "PES"),
                             labels = c("PRI", "PAN", "PRD", "PVEM", "MC", "MORENA", "INDEP.", "PES"))

guber_alluvial2$`2016`=factor(guber_alluvial2$`2016`,
                             levels = c("PRI", "PAN", "PRD", "PVEM", "MC", "MORENA", "INDEPENDIENTE", "PES"),
                             labels = c("PRI", "PAN", "PRD", "PVEM", "MC", "MORENA", "INDEP.", "PES"))

glimpse(guber_alluvial2)

guber_alluvial2 %>% 
  ggplot(aes(y = n, axis1 = `2016`, axis2 = `2019`, axis3 = `2022`,
             fill = after_stat(stratum),
             label = after_stat(stratum))) +
  geom_flow(width = 1/12, alpha = .5) +
  # geom_alluvium(
  #   aes(fill = `2016`),
  #   width = 1/12, alpha = .5) +
  geom_stratum(width = 1/4, fill = "#F5F5F3", color = "#939486") +
  geom_text(stat = "stratum", size = 4) +
  scale_x_discrete(limits = c("2016", "2019", "2022"),
                   expand = c(.05, .05)) +
  scale_y_continuous(breaks = seq(0,32,4)) +
  scale_fill_manual(values = c(
    "#E63946", "#118AB2", "#E9C46A", "#A8DADC", "#FFBF69", "#6A040F", "#ADB5BD", "#DEE2FF")) +
  theme_g() +
  theme(legend.position = "none") +
  ggtitle("Gubernaturas en México por partido") +
  labs(y = "Gubernaturas") 


ggsave(filename = "Alluvial_2016_2022.png", path= "./www/plots/",
       dpi = 320, width = 8.5, height = 7.5,
       bg = "transparent")






#####2013-2016-2019-2022
glimpse(base_gobernadores)

guber_alluvial2 <-
  base_gobernadores %>% 
  dplyr::select(`2022`,`2019`,`2016`,`2013`) %>% 
  mutate( n=1) %>% 
  group_by(`2022`,`2019`,`2016`, `2013`)  %>% 
  summarise(n=sum(n))

guber_alluvial2$`2022`=factor(guber_alluvial2$`2022`,
                              levels = c("PRI", "PAN", "PRD", "PVEM", "MC", "MORENA", "INDEPENDIENTE", "PES"),
                              labels = c("PRI", "PAN", "PRD", "PVEM", "MC", "MORENA", "INDEP.", "PES"))

guber_alluvial2$`2019`=factor(guber_alluvial2$`2019`,
                              levels = c("PRI", "PAN", "PRD", "PVEM", "MC", "MORENA", "INDEPENDIENTE", "PES"),
                              labels = c("PRI", "PAN", "PRD", "PVEM", "MC", "MORENA", "INDEP.", "PES"))

guber_alluvial2$`2016`=factor(guber_alluvial2$`2016`,
                              levels = c("PRI", "PAN", "PRD", "PVEM", "MC", "MORENA", "INDEPENDIENTE", "PES"),
                              labels = c("PRI", "PAN", "PRD", "PVEM", "MC", "MORENA", "INDEP.", "PES"))

guber_alluvial2$`2013`=factor(guber_alluvial2$`2013`,
                              levels = c("PRI", "PAN", "PRD", "PVEM", "MC", "MORENA", "INDEPENDIENTE", "PES"),
                              labels = c("PRI", "PAN", "PRD", "PVEM", "MC", "MORENA", "INDEP.", "PES"))


glimpse(guber_alluvial2)

guber_alluvial2 %>% 
  ggplot(aes(y = n, axis1 = `2013`, axis2 = `2016`, axis3 = `2019`, axis4 = `2022`,
             fill = after_stat(stratum),
             label = after_stat(stratum))) +
  geom_flow(width = 1/12, alpha = .5) +
  geom_stratum(width = 1/4, fill = "#F5F5F3", color = "#939486") +
  geom_text(stat = "stratum", size = 4) +
  scale_x_discrete(limits = c("2013", "2016", "2019", "2022"),
                   expand = c(.05, .05)) +
  scale_y_continuous(breaks = seq(0,32,4)) +
  scale_fill_manual(values = c(
    "#E63946", "#118AB2", "#E9C46A", "#A8DADC", "#FFBF69", "#6A040F", "#ADB5BD", "#DEE2FF")) +
  theme_g() +
  theme(legend.position = "none") +
  ggtitle("Gubernaturas en México por partido") +
  labs(y = "Gubernaturas") 


ggsave(filename = "Alluvial_2013_2022.png", path= "./www/plots/",
       dpi = 320, width = 8.5, height = 7.5,
       bg = "transparent")



#####2010-2013-2016-2019-2022
glimpse(base_gobernadores)

guber_alluvial2 <-
  base_gobernadores %>% 
  dplyr::select(`2022`,`2019`,`2016`,`2013`,`2010`) %>% 
  mutate( n=1) %>% 
  group_by(`2022`,`2019`,`2016`,`2013`,`2010`)  %>% 
  summarise(n=sum(n))

guber_alluvial2$`2022`=factor(guber_alluvial2$`2022`,
                              levels = c("PRI", "PAN", "PRD", "PVEM", "MC", "MORENA", "INDEPENDIENTE", "PES"),
                              labels = c("PRI", "PAN", "PRD", "PVEM", "MC", "MORENA", "INDEP.", "PES"))

guber_alluvial2$`2019`=factor(guber_alluvial2$`2019`,
                              levels = c("PRI", "PAN", "PRD", "PVEM", "MC", "MORENA", "INDEPENDIENTE", "PES"),
                              labels = c("PRI", "PAN", "PRD", "PVEM", "MC", "MORENA", "INDEP.", "PES"))

guber_alluvial2$`2016`=factor(guber_alluvial2$`2016`,
                              levels = c("PRI", "PAN", "PRD", "PVEM", "MC", "MORENA", "INDEPENDIENTE", "PES"),
                              labels = c("PRI", "PAN", "PRD", "PVEM", "MC", "MORENA", "INDEP.", "PES"))

guber_alluvial2$`2013`=factor(guber_alluvial2$`2013`,
                              levels = c("PRI", "PAN", "PRD", "PVEM", "MC", "MORENA", "INDEPENDIENTE", "PES"),
                              labels = c("PRI", "PAN", "PRD", "PVEM", "MC", "MORENA", "INDEP.", "PES"))

guber_alluvial2$`2010`=factor(guber_alluvial2$`2010`,
                              levels = c("PRI", "PAN", "PRD", "PVEM", "MC", "MORENA", "INDEPENDIENTE", "PES"),
                              labels = c("PRI", "PAN", "PRD", "PVEM", "MC", "MORENA", "INDEP.", "PES"))


glimpse(guber_alluvial2)

guber_alluvial2 %>% 
  ggplot(aes(y = n, axis1 = `2010`, axis2 = `2013`, axis3 = `2016`, axis4 = `2019`, axis5 = `2022`,
             fill = after_stat(stratum),
             label = after_stat(stratum))) +
  geom_flow(width = 1/12, alpha = .5) +
  geom_stratum(width = 1/4, fill = "#F5F5F3", color = "#939486") +
  geom_text(stat = "stratum", size = 4) +
  scale_x_discrete(limits = c("2010", "2013", "2016", "2019", "2022"),
                   expand = c(.05, .05)) +
  scale_y_continuous(breaks = seq(0,32,4)) +
  scale_fill_manual(values = c(
    "#E63946", "#118AB2", "#E9C46A", "#A8DADC", "#FFBF69", "#6A040F", "#ADB5BD", "#DEE2FF")) +
  theme_g() +
  theme(legend.position = "none") +
  ggtitle("Gubernaturas en México por partido") +
  labs(y = "Gubernaturas") 


ggsave(filename = "Alluvial_2010_2022.png", path= "./www/plots/",
       dpi = 320, width = 9, height = 7,
       bg = "transparent")




####1986-1989-1992-1995-1998-2001-2004-2007-2010-2013-2016-2019-2022
glimpse(base_gobernadores)

guber_alluvial2 <-
  base_gobernadores %>% 
  dplyr::select(`2022`,`2019`,`2016`,`2013`,`2010`,`2007`,
                `2004`,`2001`,`1998`,`1995`,`1992`,`1989`,`1986`) %>% 
  mutate( n=1) %>% 
  group_by(`2022`,`2019`,`2016`,`2013`,`2010`,`2007`,
           `2004`,`2001`,`1998`,`1995`,`1992`,`1989`,`1986`)  %>% 
  summarise(n=sum(n))

guber_alluvial2$`2022`=factor(guber_alluvial2$`2022`,
                              levels = c("PRI", "PAN", "PRD", "PVEM", "MC", "MORENA", "INDEPENDIENTE", "PES"),
                              labels = c("PRI", "PAN", "PRD", "PVEM", "MC", "MORENA", "INDEP.", "PES"))

guber_alluvial2$`2019`=factor(guber_alluvial2$`2019`,
                              levels = c("PRI", "PAN", "PRD", "PVEM", "MC", "MORENA", "INDEPENDIENTE", "PES"),
                              labels = c("PRI", "PAN", "PRD", "PVEM", "MC", "MORENA", "INDEP.", "PES"))

guber_alluvial2$`2016`=factor(guber_alluvial2$`2016`,
                              levels = c("PRI", "PAN", "PRD", "PVEM", "MC", "MORENA", "INDEPENDIENTE", "PES"),
                              labels = c("PRI", "PAN", "PRD", "PVEM", "MC", "MORENA", "INDEP.", "PES"))

guber_alluvial2$`2013`=factor(guber_alluvial2$`2013`,
                              levels = c("PRI", "PAN", "PRD", "PVEM", "MC", "MORENA", "INDEPENDIENTE", "PES"),
                              labels = c("PRI", "PAN", "PRD", "PVEM", "MC", "MORENA", "INDEP.", "PES"))

guber_alluvial2$`2010`=factor(guber_alluvial2$`2010`,
                              levels = c("PRI", "PAN", "PRD", "PVEM", "MC", "MORENA", "INDEPENDIENTE", "PES"),
                              labels = c("PRI", "PAN", "PRD", "PVEM", "MC", "MORENA", "INDEP.", "PES"))

guber_alluvial2$`2007`=factor(guber_alluvial2$`2007`,
                              levels = c("PRI", "PAN", "PRD", "PVEM", "MC", "MORENA", "INDEPENDIENTE", "PES"),
                              labels = c("PRI", "PAN", "PRD", "PVEM", "MC", "MORENA", "INDEP.", "PES"))

guber_alluvial2$`2004`=factor(guber_alluvial2$`2004`,
                              levels = c("PRI", "PAN", "PRD", "PVEM", "MC", "MORENA", "INDEPENDIENTE", "PES"),
                              labels = c("PRI", "PAN", "PRD", "PVEM", "MC", "MORENA", "INDEP.", "PES"))

guber_alluvial2$`2001`=factor(guber_alluvial2$`2001`,
                              levels = c("PRI", "PAN", "PRD", "PVEM", "MC", "MORENA", "INDEPENDIENTE", "PES"),
                              labels = c("PRI", "PAN", "PRD", "PVEM", "MC", "MORENA", "INDEP.", "PES"))

guber_alluvial2$`1998`=factor(guber_alluvial2$`1998`,
                              levels = c("PRI", "PAN", "PRD", "PVEM", "MC", "MORENA", "INDEPENDIENTE", "PES"),
                              labels = c("PRI", "PAN", "PRD", "PVEM", "MC", "MORENA", "INDEP.", "PES"))

guber_alluvial2$`1995`=factor(guber_alluvial2$`1995`,
                              levels = c("PRI", "PAN", "PRD", "PVEM", "MC", "MORENA", "INDEPENDIENTE", "PES"),
                              labels = c("PRI", "PAN", "PRD", "PVEM", "MC", "MORENA", "INDEP.", "PES"))

guber_alluvial2$`1992`=factor(guber_alluvial2$`1992`,
                              levels = c("PRI", "PAN", "PRD", "PVEM", "MC", "MORENA", "INDEPENDIENTE", "PES"),
                              labels = c("PRI", "PAN", "PRD", "PVEM", "MC", "MORENA", "INDEP.", "PES"))

guber_alluvial2$`1989`=factor(guber_alluvial2$`1989`,
                              levels = c("PRI", "PAN", "PRD", "PVEM", "MC", "MORENA", "INDEPENDIENTE", "PES"),
                              labels = c("PRI", "PAN", "PRD", "PVEM", "MC", "MORENA", "INDEP.", "PES"))

guber_alluvial2$`1986`=factor(guber_alluvial2$`1986`,
                              levels = c("PRI", "PAN", "PRD", "PVEM", "MC", "MORENA", "INDEPENDIENTE", "PES"),
                              labels = c("PRI", "PAN", "PRD", "PVEM", "MC", "MORENA", "INDEP.", "PES"))

glimpse(guber_alluvial2)

g <-
guber_alluvial2 %>% 
  ggplot(aes(y = n, axis1 = `1989`, axis2 = `1992`, axis3 = `1995`, axis4 = `1998`,
                    axis5 = `2001`, axis6 = `2004`, axis7 = `2007`, axis8 = `2010`, axis9 = `2013`,
                    axis10 = `2016`, axis11 = `2019`, axis12 = `2022`,
             fill = after_stat(stratum),
             label = after_stat(stratum))) +
  geom_flow(width = 1/12, alpha = .5) +
  geom_stratum(width = 1/4, fill = "#F5F5F3", color = "#939486") +
  geom_text(stat = "stratum", size = 4) +
  scale_x_discrete(limits = c("1989", "1992", "1995", "1998", "2001", "2004", "2007",
                              "2010", "2013", "2016", "2019", "2022"),
                   expand = c(0.02, 0.02)) +
  scale_y_continuous(breaks = seq(0,32,4),
                     expand = c(0.04,0)) +
  scale_fill_manual(values = c(
    "#E63946", "#118AB2", "#E9C46A", "#A8DADC", "#FFBF69", "#6A040F", "#ADB5BD", "#DEE2FF")) +
  #ggtitle("Gubernaturas en México por partido") +
  labs(y = "Total de gubernaturas",
       title = "La (cuasi)desaparición del PRI. Tres décadas de alternancia en gubernaturas.",
       subtitle = "Gubernaturas en México por partido, desde la primera alternancia (nov. de 1989)",
       caption = "Fuente: Elaborado por Máximo E. Jaramillo-Molina (@rojo_neon)") +
  theme_g() +
  theme(legend.position = "none",
        plot.title = element_text(size=22, colour = "black"),
        plot.subtitle = element_text(size=16),
        plot.caption = element_text(size=12, colour = "black"),
        axis.text.x = element_text(size = 16, colour = "black"),
        axis.text.y = element_text(size = 12)) 

# library(plotly)
# ggplotly(g)
g

ggsave(g, filename = "Alluvial_1989_2022.png", path= "./www/plots/",
       dpi = 320, width = 18, height = 7,
       bg = "transparent")

ggsave(g, filename = "Alluvial_1989_2022.svg", path= "./www/plots/",
       dpi = 320, width = 18, height = 7,
       bg = "transparent")

