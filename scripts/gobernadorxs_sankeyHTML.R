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
p_load(ineq, haven, readr, readxl, ggplot2, shiny, tidyverse, expss, DescTools, lmtest, ggalluvial,networkD3)



base_gobernadores <- read_csv(file = "./www/data/base_gobernadores.csv")
glimpse(base_gobernadores)



################################
####Preparar datos########
################################
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

################################
####Hacer nodos y eso########
################################

# From these flows we need to create a node data frame: it lists every entities involved in the flow
guber_alluvial2_s <- guber_alluvial2 %>% 
  mutate(
    `2019` = paste(`2019`, "2019", sep = "-"),
    `2022` = paste(`2022`, "2022", sep = "-")
  )

# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes2 <- data.frame(
  name=c(as.character(guber_alluvial2_s$`2019`), 
         as.character(guber_alluvial2_s$`2022`)) %>% unique()
)

# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
guber_alluvial2_s$IDsource <- match(guber_alluvial2_s$`2019`, nodes2$name)-1 
guber_alluvial2_s$IDtarget <- match(guber_alluvial2_s$`2022`, nodes2$name)-1


################################
####Modificar etiquetas
################################

glimpse(guber_alluvial2_s)
# table(guber_alluvial2_s$`2022`)
# guber_alluvial2_s$`2022`=factor(guber_alluvial2_s$`2022`,
#                                 levels = c("PRI-2022", "PAN-2022", "PRD-2022", "PVEM-2022", "MC-2022", "MORENA-2022", "INDEPENDIENTE-2022", "PES-2022"),
#                                 labels = c("PRI", "PAN", "PRD", "PVEM", "MC", "MORENA", "INDEP.", "PES"))
# 
# guber_alluvial2_s$`2019`=factor(guber_alluvial2_s$`2019`,
#                                 levels = c("PRI-2019", "PAN-2019", "PRD-2019", "PVEM-2019", "MC-2019", "MORENA-2019", "INDEPENDIENTE-2019", "PES-2019"),
#                                 labels = c("PRI", "PAN", "PRD", "PVEM", "MC", "MORENA", "INDEP.", "PES"))
# table(guber_alluvial2_s$`2019`)
# 
# glimpse(nodes2$name)
# table(nodes2$name)
# nodes2$name=factor(nodes2$name,
#                    levels = c("PRI-2019", "PAN-2019", "PRD-2019", "PVEM-2019", "MC-2019", "MORENA-2019", "INDEP.-2019", "PES-2019",
#                               "PRI-2022", "PAN-2022", "PRD-2022", "PVEM-2022", "MC-2022", "MORENA-2022", "INDEP.-2022", "PES-2022"),
#                    labels = c("PRI", "PAN", "PRD", "PVEM", "MC", "MORENA", "INDEP.", "PES",
#                               "PRI", "PAN", "PRD", "PVEM", "MC", "MORENA", "INDEP.", "PES"))
# glimpse(nodes2$name)
# table(nodes2$name)


################################
#Colores
################################
ColourScal ='d3.scaleOrdinal() .range(["#ADB5BD", "#FFBF69", "#6A040F", "#1D3557", "#E9C46A", "#E63946", "#DEE2FF", "#FFBF69",
                                       "#6A040F", "#1D3557", "#DEE2FF", "#E63946", "#A8DADC", "#E63946", "#DEE2FF", "#A8DADC"])'
#"#E63946", "#118AB2", "#E9C46A", "#A8DADC", "#FFBF69", "#6A040F", "#ADB5BD", "#DEE2FF"
#"#E63946", "#118AB2", "#E9C46A", "#A8DADC", "#FFBF69", "#6A040F", "#ADB5BD", "#DEE2FF"

################################
#Gráfica
################################
p <-
  sankeyNetwork(Links = guber_alluvial2_s, Nodes = nodes2,
                   Source = "IDsource", Target = "IDtarget",
                   Value = "n", NodeID = "name", 
                   sinksRight=FALSE, colourScale=ColourScal, nodeWidth=40, fontSize=13, nodePadding=20)

# save the widget
library(htmlwidgets)
saveWidget(p, file=paste0( getwd(), "/www/html/sankey_2019-2022_etiquetas.html"))


