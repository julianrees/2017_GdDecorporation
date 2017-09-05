library(ggplot2)
library(reshape2)
library(xlsx)
library(plyr)

import_data <- read.xlsx("../FromRebecca/data/Rimport.xlsx", 
                      header = TRUE, sheetIndex = 1)
import_data <- import_data[, -17]


mdata <- melt(import_data, id = colnames(import_data)[1:4])

reten <- cbind(import_data[,1:4], Body = rowSums(import_data[, 5:14]), 
               Excreta = rowSums(import_data[, 15:16]))
mreten <- melt(reten, id = colnames(reten)[1:4])


#---------- END OF DATA MANIPULATION !-!-! PLOTTING STARTS ----------------
w = 0.65
fwid = 9
fhei = 6
theme_set(theme_bw())
theme_update(plot.title = element_text(hjust = 0.5),
             panel.grid.major = element_blank(),
             panel.grid.minor = element_blank())


ggplot(mdata, aes(x = Time, y = value, by = variable)) + 
  geom_boxplot(aes(fill = Time)) + 
  facet_wrap(~Ligand) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(mreten, aes(x = Ligand, y = value, by = Time)) + 
  geom_boxplot(aes(fill = Time)) + 
  facet_wrap(~variable)

ggplot(mreten, aes(x = Group, y = value, by = variable)) + 
  geom_col(aes(fill = variable), position = "dodge") + 
  

ggplot(mreten, aes(x = variable, y = value, by = Time)) + 
  geom_boxplot(aes(fill = Ligand )) + 
  geom_col(aes(fill = Ligand), position = "dodge")
