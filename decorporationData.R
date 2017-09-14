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

# make new data for daily urine and feces
idcols = c(1:4,17)
import_excreta <- import_data[idcols]
urine <- cbind(import_excreta, read.xlsx("../FromRebecca/data/Rimport.xlsx", 
                                                  header = TRUE, sheetIndex = 2)[,-1], 
                        Excretion = "Urine")
murine <- melt(urine, id = c(colnames(urine[1:5]), "Excretion"))

feces <- cbind(import_excreta, read.xlsx("../FromRebecca/data/Rimport.xlsx", 
                                         header = TRUE, sheetIndex = 3)[,-1], 
               Excretion = "Feces")
mfeces <- melt(feces, id = c(colnames(urine[1:5]), "Excretion"))

excreta <- rbind(mfeces, murine)
colnames(excreta) <- c(colnames(excreta[1:6]), "Day", "Value")
excreta$Value <- excreta$Value * 100


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



totexcreta <- excreta[ which(excreta$Excretion == "Urine"), 1:7]
totexcreta$Excretion <- "Total"
totexcreta <- cbind(totexcreta, Value = excreta$Value[ which(excreta$Excretion == "Urine") ] + 
  excreta$Value[ which(excreta$Excretion == "Feces") ])




cumexcreta <- excreta
cumexcreta$Value[ which(cumexcreta$Day == "Day.2")] <- 
  cumexcreta$Value[ which(cumexcreta$Day == "Day.1")] + cumexcreta$Value[ which(cumexcreta$Day == "Day.2")]
cumexcreta$Value[ which(cumexcreta$Day == "Day.3")] <- 
  cumexcreta$Value[ which(cumexcreta$Day == "Day.2")] + cumexcreta$Value[ which(cumexcreta$Day == "Day.3")]
cumexcreta$Value[ which(cumexcreta$Day == "Day.4")] <- 
  cumexcreta$Value[ which(cumexcreta$Day == "Day.3")] + cumexcreta$Value[ which(cumexcreta$Day == "Day.4")]

totcumexcreta <- cumexcreta[ which(cumexcreta$Excretion == "Urine"), 1:7]
totcumexcreta$Excretion <- "Total"
totcumexcreta <- cbind(totcumexcreta, Value = cumexcreta$Value[ which(cumexcreta$Excretion == "Urine") ] + 
                         cumexcreta$Value[ which(cumexcreta$Excretion == "Feces") ])


#---------- END OF DATA MANIPULATION !-!-! PLOTTING STARTS ----------------
w = 0.65
fwid = 9
fhei = 6
theme_set(theme_bw())
theme_update(plot.title = element_text(hjust = 0.5),
             panel.grid.major = element_blank(),
             panel.grid.minor = element_blank())

grouppalatte = c('#41ad5b','#08306b','#2171b5','#4292B6','#6baed6','#9ecae1','#cb181d','#fb6a4a')


#mdata[which(mdata$variable != "Urine" & mdata$variable != "Feces"), ]


ggplot(mdata[ which(mdata$variable != "Urine" & mdata$variable != "Feces"), ], 
       aes(x = variable, y = value, fill = variable)) + 
  geom_col(width = 0.8, position = "dodge") +
  theme(axis.text.x = element_text(angle = 50, hjust = 1)) + 
  theme(legend.title = element_blank()) + 
  facet_wrap(~Treatment) + 
  xlab("Organ") + 
  ylab("% Activity") +
  scale_fill_brewer(palette="Spectral")


mdata[ which(mdata$variable == "Urine" & mdata$variable != "Feces"), ]

ddply(mdata[ which(mdata$variable != "Urine" & mdata$variable != "Feces"), ], 
      .(Treatment, variable), summarize, Ave = mean(value), 
      Error = sd(value)/sqrt(length(value))
)


reltotcumexcreta <- ddply(totcumexcreta, .(Day, Treatment), summarize, Ave=mean(Value), 
                          Error = sd(Value)/sqrt(length(Value)))

ddply(mreten[which(mreten$Location == "Retained"), ], .(Treatment), summarize, Ave=mean(Value), 
      Error = sd(Value)/sqrt(length(Value)))




orgreten <- ddply(mdata[ which(mdata$variable != "Urine" & mdata$variable != "Feces"), ], 
                  .(Treatment, variable), summarize, Ave = mean(value), 
                  Error = sd(value)/sqrt(length(value)))
orgreten$variable <- with(orgreten, factor(variable, levels = rev(levels(variable))))

pos <- position_dodge(width = 0.9)

ggplot(orgreten, aes(x = Treatment, weight = Ave, ymin = Ave-Error, ymax = Ave+Error, fill = variable, group = variable)) + 
  geom_col(aes(y = Ave, fill = variable), color = 'black', position = pos) +
  geom_errorbar(position = pos) + 
  geom_col(data = ddply(mreten[which(mreten$Location == "Retained"), ], .(Treatment), summarize, Ave=mean(Value), 
                        Error = sd(Value)/sqrt(length(Value))), 
           aes(y = Ave, fill = NULL, group = NULL),
           alpha = 0.25, color = 'black') + 
  geom_errorbar(data = ddply(mreten[which(mreten$Location == "Retained"), ], .(Treatment), summarize, Ave=mean(Value), 
                             Error = sd(Value)/sqrt(length(Value))), 
                aes(fill = NULL, group = NULL, ymin = Ave - Error, ymax = Ave + Error), 
                position = pos, width = 0.4) + 
  #scale_fill_grey(end = 1) +
  scale_fill_brewer(palette = "Spectral") +
  theme(axis.text.x = element_text(angle = 35, hjust = 1)) + 
  scale_y_continuous(name = expression(''^153*Gd~'Content (% ID)'), limits = c(0,60), expand = c(0, 0)) +
  theme(legend.title = element_blank()) + 
  xlab(NULL) + 
  ggsave(filename = '../../../Manuscripts/2017_GdDecorporation/Overleaf/biodistribution.pdf', 
         width = 5.5, height = 5.5, units = "in")


ggplot(orgreten, aes(x = Treatment, weight = Ave, ymin = Ave-Error, ymax = Ave+Error, fill = variable, group = variable)) + 
  geom_col(aes(y = Ave, fill = variable), color = 'black', position = pos) +
  geom_errorbar(position = pos) + 
  geom_col(data = ddply(mreten[which(mreten$Location == "Retained"), ], .(Treatment), summarize, Ave=mean(Value), 
                        Error = sd(Value)/sqrt(length(Value))), 
           aes(y = Ave, fill = NULL, group = NULL),
           alpha = 0, color = 'black') + 
  geom_errorbar(data = ddply(mreten[which(mreten$Location == "Retained"), ], .(Treatment), summarize, Ave=mean(Value), 
                             Error = sd(Value)/sqrt(length(Value))), 
                aes(fill = NULL, group = NULL, ymin = Ave - Error, ymax = Ave + Error), 
                position = pos, width = 0.4) + 
  scale_fill_brewer(palette = "Spectral") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_y_continuous(name = expression(''^153*Gd~'Content (% ID)'), limits = c(0,60)) +
  theme(legend.title = element_blank()) + 
  xlab(NULL) + 
  ggsave(filename = '../../../Presentations/Conferences/2017_SLAC_UserMeeting/Poster/Figures/biodistribution.pdf', 
         width = 5, height = 4, units = "in")




selorgreten <- orgreten[ which(orgreten$Treatment == "HOPO, 24 h pre" | orgreten$Treatment =="HOPO, 1 h pre" | orgreten$Treatment == "DTPA, 1 h pre"), ]
selmreten <- mreten[ which(mreten$Treatment == "HOPO, 24 h pre" | mreten$Treatment =="HOPO, 1 h pre" | mreten$Treatment == "DTPA, 1 h pre"), ]

ggplot(selorgreten, aes(x = Treatment, weight = Ave, ymin = Ave-Error, ymax = Ave+Error, fill = variable, group = variable)) + 
  geom_col(aes(y = Ave, fill = variable), color = 'black', position = pos) +
  geom_errorbar(position = pos) + 
  geom_col(data = ddply(selmreten[which(selmreten$Location == "Retained"), ], .(Treatment), summarize, Ave=mean(Value), 
                        Error = sd(Value)/sqrt(length(Value))), 
           aes(y = Ave, fill = NULL, group = NULL),
           alpha = 0, color = 'black') + 
  geom_errorbar(data = ddply(selmreten[which(selmreten$Location == "Retained"), ], .(Treatment), summarize, Ave=mean(Value), 
                             Error = sd(Value)/sqrt(length(Value))), 
                aes(fill = NULL, group = NULL, ymin = Ave - Error, ymax = Ave + Error), 
                position = pos, width = 0.4) + 
  scale_fill_grey(end = 1) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_y_continuous(name = expression(''^153*Gd~'Content (% ID)'), limits = c(0,10.5), breaks = c(0,2,4,6,8,10)) +
  theme(legend.title = element_blank()) + 
  xlab(NULL) #+ 
  #ggsave(filename = '../../../Manuscripts/2017_GdDecorporation/Overleaf/biodistribution.pdf', 
   #      width = 5, height = 4, units = "in")







ggplot(mdata [ which(mdata$variable == "Brain"), ], aes(x = Treatment, y = value)) + 
  geom_boxplot(aes(fill = Treatment)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(mdata [ which(mdata$variable == "Kidneys"), ], aes(x = Treatment, y = value)) + 
  geom_boxplot(aes(fill = Treatment)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(mdata [ which(mdata$variable == "Liver"), ], aes(x = Treatment, y = value)) + 
  geom_boxplot(aes(fill = Treatment)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(mdata [ which(mdata$variable == "Liver" | mdata$variable == "Kidneys" | mdata$variable == "Brain"), ], 
       aes(x = Treatment, y = value)) + 
  geom_boxplot() + 
  scale_fill_grey(end = 1) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_y_continuous(name = expression(''^153*Gd~'Content (% ID)')) +
  facet_grid(variable ~ ., scales = "free") + 
  theme(legend.title = element_blank()) + 
  xlab(NULL) + 
  ggsave(filename = '../../../Manuscripts/2017_GdDecorporation/Overleaf/selectorgans.pdf', 
         width = 5, height = 5, units = "in")
  

ggplot(mdata [ which(mdata$variable != "Urine" & mdata$variable != "Feces"), ], 
       aes(x = Treatment, y = value)) + 
  geom_boxplot(aes(fill = Treatment), width = 0.4) + 
  #scale_fill_grey(end = 1) +
  theme(axis.text.x = element_text(angle = 35, hjust = 1)) + 
  scale_y_continuous(name = expression(''^153*Gd~'Content (% ID)'), expand = c(0.02,0)) +
  facet_grid(variable ~ ., scales = "free") + 
  theme(legend.title = element_blank()) + 
  xlab(NULL) + 
  theme(panel.spacing = unit(0.2, "lines")) + 
  scale_fill_manual(values = grouppalatte) +
  ggsave(filename = '../../../Manuscripts/2017_GdDecorporation/Overleaf/allorgans.pdf', 
         width = 6, height = 7, units = "in")


ggplot(mdata [ which(mdata$variable != "Urine" & mdata$variable != "Feces"), ], 
       aes(x = Treatment, y = value)) + 
  geom_jitter(aes(color = Treatment), width = 0.2) + 
  #scale_fill_grey(end = 1) +
  theme(axis.text.x = element_text(angle = 35, hjust = 1)) + 
  scale_y_continuous(name = expression(''^153*Gd~'Content (% ID)'), expand = c(0.02,0)) +
  facet_grid(variable ~ ., scales = "free") + 
  theme(legend.title = element_blank()) + 
  xlab(NULL) + 
  theme(panel.spacing = unit(0.2, "lines")) + 
  scale_color_manual(values = grouppalatte) +
  ggsave(filename = '../../../Manuscripts/2017_GdDecorporation/Overleaf/allorgansjitter.pdf', 
         width = 6, height = 7, units = "in")



ggplot(mdata[ which(mdata$variable != "Urine" & mdata$variable != "Feces" & mdata$Ligand != "DTPA"), ], 
       aes(x = variable, y = value, fill = variable)) + 
  geom_col(width = 0.8, position = "dodge") +
  theme(axis.text.x = element_text(angle = 50, hjust = 1)) + 
  theme(legend.title = element_blank()) + 
  facet_wrap(~Treatment) + 
  xlab("Organ") + 
  ylab("% Activity") +
  scale_fill_brewer(palette="Spectral")

ggplot(mdata[ which(mdata$variable != "Urine" & mdata$variable != "Feces" & mdata$Ligand != "HOPO"), ], 
       aes(x = variable, y = value, fill = variable)) + 
  geom_col(width = 0.8, position = "dodge") +
  theme(axis.text.x = element_text(angle = 50, hjust = 1)) + 
  theme(legend.title = element_blank()) + 
  facet_wrap(~Treatment) + 
  xlab("Organ") + 
  ylab("% Activity") +
  scale_fill_brewer(palette="Spectral")


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
  theme(legend.title = element_blank()) + 
  scale_fill_brewer(palette="Spectral")



ggplot(mreten, aes(x = Treatment, y = Value, by = Location)) + 
  geom_col(aes(fill = Location), width = 0.7, position = "dodge") + 
  theme(axis.text.x = element_text(angle = 50, hjust = 1)) + 
  ylab("% Activity") + 
  theme(legend.title = element_blank()) + 
  scale_alpha_discrete(range = c(0.4, 0.9)) + 
  scale_y_continuous(limits = c(0,100)) +
  scale_fill_brewer(palette="Dark2")


ggplot(mreten[ which(mreten$Location == "Excreted"), ], aes(x = Treatment, y = Value, by = Location)) + 
  geom_boxplot(aes(fill = Location), width = 0.7) + 
  theme(axis.text.x = element_text(angle = 50, hjust = 1)) + 
  ylab("% Activity Excreted") + 
  theme(legend.title = element_blank()) + 
  scale_alpha_discrete(range = c(0.4, 0.9)) + 
  scale_y_continuous(limits = c(0,100)) +
  scale_fill_brewer(palette="Dark2")

ggplot(mdata[ which(mdata$variable == "Feces" | mdata$variable == "Urine"), ], aes(x = Treatment, y = value, by = variable)) + 
  geom_col(aes(fill = variable), width = 0.7, position = "dodge") + 
  theme(axis.text.x = element_text(angle = 50, hjust = 1)) + 
  ylab("% Activity Excreted") + 
  theme(legend.title = element_blank()) + 
  scale_alpha_discrete(range = c(0.4, 0.9)) + 
  scale_y_continuous(limits = c(0,100)) +
  scale_fill_brewer(palette="Set2")


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
  scale_y_continuous(limits = c(0,100)) +
  scale_fill_brewer(palette="Set2")

ggplot(excreta, aes(x = Day, y = Value, by = Treatment)) + 
  geom_boxplot(aes(fill = Treatment)) + 
  theme(legend.title = element_blank()) + 
  facet_wrap(~Excretion) + 
  scale_y_continuous(limits = c(0,100)) +
  scale_fill_brewer(palette="Dark2")

ggplot(excreta, aes(x = Day, y = Value, by = Excretion)) + 
  geom_boxplot(aes(fill = Excretion)) + 
  theme(legend.title = element_blank()) + 
  facet_wrap(~Treatment) + 
  scale_y_continuous(limits = c(0,100)) +
  scale_fill_brewer(palette="Set2")

ggplot(excreta, aes(x = Day, y = Value, by = Treatment)) + 
  geom_col(aes(fill = Treatment), position = "dodge", width = 0.8) + 
  theme(legend.title = element_blank()) + 
  facet_wrap(~Excretion) + 
  scale_y_continuous(limits = c(0,100)) +
  scale_fill_brewer(palette="Dark2")

ggplot(ddply(excreta[ which(excreta$Excretion == "Urine"), ], .(Day, Treatment, Excretion), summarize, Value=mean(Value)), 
             aes(x = Day, y = Value)) + 
  geom_col(data = ddply(totexcreta, .(Day, Treatment), summarize, Value=mean(Value), 
                        se = sd(Value)/sqrt(length(Value))),
           aes(x = Day, y = Value, fill = Treatment, group = Treatment), 
           position = "dodge", width = 0.8, alpha = 0.5) + 
  geom_col(aes(fill = Treatment, group = Treatment), 
           position = "dodge", width = 0.8, alpha = 1) + 
  theme(legend.title = element_blank()) + 
  scale_fill_brewer(palette="Dark2") + 
  scale_y_continuous(name = "% Activity Excreted ", limits = c(0,100)) +
  geom_col(data = ddply(excreta, .(Day, Treatment, Excretion), summarize, Value=mean(Value)), 
           aes(y = Value / 100000, alpha = Excretion, group = Treatment), 
                    position = "dodge", width = 0.8) + 
  scale_alpha_discrete(range = c(0.5, 1))

totexcreta$Excretion <- as.factor(totexcreta$Excretion)
str(totexcreta)
allexcreta <- rbind(totexcreta, excreta)
str(allexcreta)



ggplot(ddply(allexcreta, .(Day, Treatment, Excretion), summarize, Ave = mean(Value), 
             Error = sd(Value)/sqrt(length(Value))), 
             aes(x = Day, y = Ave, by = Treatment, group = Treatment)) + 
  geom_col(aes(fill = Treatment), position = "dodge") + 
  facet_grid(. ~ Excretion) + 
  scale_x_discrete(name = "Study Day", labels = c("1", "2", "3", "4"), expand = c(0.01, 0.01)) +
  scale_y_continuous(name = expression(''^153*Gd~'Content (% ID)'), limits = c(0,100), expand = c(0, 0)) +
  geom_errorbar(aes(ymin = Ave - Error, ymax = Ave + Error), size = 0.4, width = 0.8, color = "black", position = pos) +
  theme(legend.title = element_blank()) + 
  scale_fill_manual(values = grouppalatte) + 
  theme(legend.position = "bottom") + 
  theme(panel.spacing = unit(0.3, "lines")) + 
  ggsave(filename = '../../../Manuscripts/2017_GdDecorporation/Overleaf/totalexcretions.pdf', 
         width = 5.5, height = 5, units = "in")


ggplot(allexcreta, aes(x = Day, y = Value, by = Treatment)) + 
  geom_boxplot(aes(fill = Treatment, color = Treatment)) + 
  facet_grid(. ~ Excretion) + 
  scale_x_discrete(name = "Study Day", labels = c("1", "2", "3", "4"), expand = c(0.01, 0.01)) +
  scale_y_continuous(name = expression(''^153*Gd~'Content (% ID)'), limits = c(0,100), expand = c(0, 0)) +
 # geom_errorbar(aes(ymin = Ave - Error, ymax = Ave + Error), size = 1, width = 0.05, color = "black", position = pos) +
  theme(legend.title = element_blank()) + 
  scale_fill_brewer(palette="Dark2") + 
  scale_color_brewer(palette="Dark2") + 
  theme(legend.position = "bottom") + 
  theme(panel.spacing = unit(0.3, "lines")) + 
  ggsave(filename = '../../../Manuscripts/2017_GdDecorporation/Overleaf/totalexcretionsbox.pdf', 
         width = 5, height = 5, units = "in")


ggplot(ddply(allexcreta, .(Day, Treatment, Excretion), summarize, Ave = mean(Value), 
             Error = sd(Value)/sqrt(length(Value))), 
       aes(x = Day, y = Ave, by = Treatment)) + 
  geom_point(aes(fill = Treatment, color = Treatment), size = 2) + 
  facet_grid(. ~ Excretion) + 
  scale_x_discrete(name = "Study Day", labels = c("1", "2", "3", "4"), expand = c(0.01, 0.01)) +
  scale_y_continuous(name = expression(''^153*Gd~'Content (% ID)'), limits = c(0,100), expand = c(0, 0)) +
  geom_errorbar(aes(ymin = Ave - Error, ymax = Ave + Error), size = 0.5, width = 0.3, color = "black") +
  theme(legend.title = element_blank()) + 
  scale_fill_brewer(palette="Dark2") + 
  scale_color_brewer(palette="Dark2") + 
  theme(legend.position = "right") + 
  theme(panel.spacing = unit(0.3, "lines")) + 
  ggsave(filename = '../../../Manuscripts/2017_GdDecorporation/Overleaf/totalexcretionspoint.pdf', 
         width = 5, height = 4, units = "in")



ggplot(ddply(totcumexcreta, .(Day, Treatment), summarize, Ave=mean(Value), 
             Error = sd(Value)/sqrt(length(Value))),
       aes(x = Day, y = Ave, group = Treatment)) + 
  geom_line(aes(color = Treatment), size = 1) +
  scale_x_discrete(name = "Study Day", labels = c("1", "2", "3", "4")) +
  scale_y_continuous(name = "% Activity Excreted", limits = c(0,100)) +
  #ylab("% Activity Excreted") +
  geom_point(aes(color = Treatment)) + 
  geom_errorbar(aes(color = Treatment, ymin = Ave - Error, ymax = Ave + Error), size = 1, width = 0.05) +
  theme(legend.title = element_blank()) + 
  scale_color_brewer(palette="Dark2") + 
  geom_hline(yintercept = 100, linetype = 2)






ddply(totcumexcreta, .(Day, Treatment), summarize, Ave=mean(Value), 
      Error = sd(Value)/sqrt(length(Value)))

totcumexcreta$Excretion <- as.factor(totcumexcreta$Excretion)
str(totcumexcreta)


allcumexcreta <- rbind(totcumexcreta, cumexcreta)

with(cumexcreta, factor(Excretion, levels = rev(levels(Excretion))))

     
     
     allcumexcreta$Excretion <- with(allcumexcreta)

rmdata$variable <- with(mdata, factor(variable, levels = rev(levels(variable))))

levels(allcumexcreta$Excretion)



ggplot(ddply(allcumexcreta, .(Day, Treatment, Excretion), summarize, Ave=mean(Value), 
             Error = sd(Value)/sqrt(length(Value))),
       aes(x = Day, y = Ave, group = Treatment)) + 
  geom_line(aes(color = Treatment), size = 0.7) +
  scale_x_discrete(name = "Study Day", labels = c("1", "2", "3", "4"), expand = c(0.01, 0.01)) +
  scale_y_continuous(name = expression(''^153*Gd~'Content (% ID)'), expand = c(0.05, 0.05)) +
  #ylab("% Activity Excreted") +
  geom_point(aes(color = Treatment), size = 1.7) + 
  geom_errorbar(aes(ymin = Ave - Error, ymax = Ave + Error), size = .4, width = 0.1) +
  theme(legend.title = element_blank()) + 
  scale_color_manual(values = grouppalatte) + 
  theme(panel.spacing = unit(0.6, "lines")) +
  #geom_hline(yintercept = 100, linetype = 2) + 
  facet_grid(Excretion ~ ., scales = "free") + 
  ggsave(filename = '../../../Manuscripts/2017_GdDecorporation/Overleaf/cumulativeexcretions.pdf', 
         width = 5.5, height = 5.5, units = "in")








reltotcumexcreta <- ddply(totcumexcreta, .(Day, Treatment), summarize, Ave=mean(Value), 
      Error = sd(Value)/sqrt(length(Value)))

reltotcumexcreta <- ddply(reltotcumexcreta, .(Day), transform, Ave = Ave - Ave[Treatment=="Control"])                              
reltotcumexcreta <- reltotcumexcreta[-which(reltotcumexcreta$Treatment == "Control"), ]                              
                              
                              
ggplot(reltotcumexcreta, aes(x = Day, y = Ave, group = Treatment)) + 
  geom_line(aes(color = Treatment), size = 1) +
  scale_x_discrete(name = "Study Day", labels = c("1", "2", "3", "4")) +
  scale_y_continuous(name = "% Activity Excreted Relative to Control", limits = c(-1,65)) +
  #ylab("% Activity Excreted") +
  geom_point(aes(color = Treatment)) + 
  #geom_errorbar(aes(color = Treatment, ymin = Ave - Error, ymax = Ave + Error), size = 1, width = 0.05) +
  theme(legend.title = element_blank()) + 
  scale_color_brewer(palette="Dark2")
#  geom_hline(yintercept = 100, linetype = 2)




#---- ! ! STATISTICAL ANALYSIS SECTION ! ! ----

library(multcomp)

ggplot(reten, aes(x = Treatment, y = Body)) + 
  geom_boxplot()

reten.modl <- lm(Body ~ Group, data = reten)
summary(reten.modl)
anova(reten.modl)
confint(reten.modl)


fit <- aov(Excreta ~ Group, data = reten)
coef(fit)
summary(fit)
TukeyHSD(fit)
summary(glht(fit, linfct=mcp(Group="Dunnett")))

plot(fit)


org.modl <- aov(value ~ variable, data = mdata)
summary(org.modl)
anova(org.modl)
TukeyHSD(org.modl)
summary(glht(org.modl, linfct=mcp(variable="Dunnett")))

fit <- aov(value ~ Treatment, data = mdata[ which(mdata$variable == "Brain"), ])
coef(fit)
summary(fit)
#TukeyHSD(fit)
summary(glht(fit, linfct=mcp(Treatment="Dunnett")))
summary(glht(fit, linfct=mcp(Treatment="Tukey")))




fit <- aov(value ~ Treatment, data = mdata[ which(mdata$variable == "Kidneys"), ])
coef(fit)
#plot(fit)
summary(fit)
#TukeyHSD(fit)
summary(glht(fit, linfct=mcp(Treatment="Dunnett")))
summary(glht(fit, linfct=mcp(Treatment="Tukey")))




# ---- Print means and stds for table ----
ddply(mdata[ which(mdata$variable != "Urine" & mdata$variable != "Feces"), ], 
      .(Group, Treatment, variable), summarize, Ave = mean(value), 
      Error = sd(value)/sqrt(length(value))
)


Qmdata = rm.outlier(mdata$value)
ddply(mdata[ which(mdata$variable == "Brain"), ], 
      .(Group, Treatment, variable), summarize, Ave = mean(value), 
      Error = sd(value)/sqrt(length(value))
)

ddply(reten, .(Treatment), summarize, Ave = mean(Body)*100, 
      Error = sd(Body)/sqrt(length(Body))*100)
ddply(reten, .(Treatment), summarize, Ave = mean(Excreta), 
      Error = sd(Excreta)/sqrt(length(Excreta)))
rmdata <- mdata
rmdata$variable <- with(mdata, factor(variable, levels = rev(levels(variable))))


ddply(rmdata[ which(rmdata$Group == "A"), ], 
      .(Treatment, variable), summarize, Ave = mean(value), 
      Err = sd(value)/sqrt(length(value)))


mdata[ which(mdata$Group == "C"), sum()]

library(outliers)


Qmdata$value[ which(mdata$variable == "Brain" & mdata$Group == "A") ]

dixon.test(mdata$value[ which(mdata$variable == "Brain" & mdata$Group == "A") ], type = 0)
Qmdata = rm.outlier(mdata$value[ which(mdata$variable == "Brain" & mdata$Group == "A") ])
mean(Qmdata)
sd(Qmdata)/sqrt(3)
