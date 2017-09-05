library(ggplot2)
library(reshape2)
library(xlsx)
library(plyr)

# get the data from excel, trim sum column
import_data <- read.xlsx("../FromRebecca/data/Rimport.xlsx", 
                      header = TRUE, sheetIndex = 1)
import_data <- import_data[, -17]

# reorder the group factor to sort by ligand
import_data$Group <- factor(import_data$Group, 
                       levels = levels(import_data$Group)[c(1:3,5,7,8,4,6)])

# create new factor for treatment
import_data <- cbind(import_data, Treatment = import_data$Group)
import_data$Treatment <- mapvalues(import_data$Treatment, from = levels(import_data$Treatment),
                              to = c("Control", "HOPO, 24 h pre", "HOPO, 1 h pre", "HOPO, 1 h post", 
                                     "HOPO, 24 h post", "HOPO, 48 h post","DTPA, 1 h pre", "DTPA, 1 hr post"))


idcols = c(1:4,17)
mdata <- melt(import_data, id = colnames(import_data)[idcols])
mdata$value <- mdata$value * 100

reten <- cbind(import_data[,idcols], Body = rowSums(import_data[, 5:14]), 
               Excreta = rowSums(import_data[, 15:16]))
mreten <- melt(reten, id = colnames(reten)[1:5])
colnames(mreten) <- c(colnames(mreten)[1:5], "Location", "Value")


mreten$Location <- mapvalues(mreten$Location, from = levels(mreten$Location), 
                             c("Retained", "Excreted"))
mreten$Value <- mreten$Value * 100


internalpercent <- import_data[-15:-16]
mint <- melt(internalpercent, id = c("Group", "Ligand", "Time", "Mouse", "Treatment"))
dint <- dcast(mint, Group+Treatment+Ligand+Time~variable, mean)
dint[,5:14] <- dint[,5:14]/rowSums(dint[,5:14])
mdint <- melt(dint, id = c("Group", "Ligand", "Time", "Treatment"))

#---------- END OF DATA MANIPULATION !-!-! PLOTTING STARTS ----------------
w = 0.65
fwid = 9
fhei = 6
theme_set(theme_bw())
theme_update(plot.title = element_text(hjust = 0.5),
             panel.grid.major = element_blank(),
             panel.grid.minor = element_blank())

mdata[which(mdata$variable != "Urine" & mdata$variable != "Feces"), ]


ggplot(mdint, aes(x = "", y = value, fill = variable)) + 
  geom_col(width = 1) +
  coord_polar("y", start=0) + 
  facet_wrap(~Treatment) + 
  xlab(NULL) + 
  ylab(NULL) +
  theme(axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank()) + 
  scale_fill_brewer(palette="Spectral")



ggplot(mreten, aes(x = Treatment, y = Value, by = Location)) + 
  geom_col(aes(fill = Location), width = 0.7, position = "dodge") + 
  theme(axis.text.x = element_text(angle = 50, hjust = 1)) + 
  ylab("% Activity") + 
  theme(legend.title = element_blank()) + 
  scale_alpha_discrete(range = c(0.4, 0.9)) + 
  scale_fill_brewer(palette="Dark2")


ggplot(mreten[ which(mreten$Location == "Excreted"), ], aes(x = Treatment, y = Value, by = Location)) + 
  geom_boxplot(aes(fill = Location), width = 0.7) + 
  theme(axis.text.x = element_text(angle = 50, hjust = 1)) + 
  ylab("% Activity Excreted") + 
  theme(legend.title = element_blank()) + 
  scale_alpha_discrete(range = c(0.4, 0.9)) + 
  scale_fill_brewer(palette="Dark2")

ggplot(mdata[ which(mdata$variable == "Feces" | mdata$variable == "Urine"), ], aes(x = Treatment, y = value, by = variable)) + 
  geom_col(aes(fill = variable), width = 0.7, position = "dodge") + 
  theme(axis.text.x = element_text(angle = 50, hjust = 1)) + 
  ylab("% Activity Excreted") + 
  theme(legend.title = element_blank()) + 
  scale_alpha_discrete(range = c(0.4, 0.9)) + 
  scale_fill_brewer(palette="Dark2")


mretensc <- mreten
mretensc$Value <- mretensc$Value / 4
mdatasc <- mdata
mdatasc$value <- mdatasc$value / 4

ggplot(mdatasc[ which(mdatasc$variable == "Feces" | mdatasc$variable == "Urine"), ], aes(x = Treatment, y = value, by = variable)) + 
  geom_col(aes(fill = variable), width = 0.6) + 
  theme(axis.text.x = element_text(angle = 50, hjust = 1)) + 
  ylab("% Activity Excreted") + 
  theme(legend.title = element_blank()) + 
  scale_alpha_discrete(range = c(0.4, 0.9)) + 
  scale_fill_brewer(palette="Dark2")


ggplot(mretensc, aes(x = Treatment, y = Value, by = Location)) + 
  geom_col(aes(fill = Ligand, alpha = Location), width = 0.7) + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  ylab("% Activity") + 
  theme(legend.title = element_blank()) + 
  scale_alpha_discrete(range = c(0.2, 0.9))
