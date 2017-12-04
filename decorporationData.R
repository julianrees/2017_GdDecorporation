library(ggplot2)
library(reshape2)
library(xlsx)
library(plyr)
library(superheat)

# get the data from excel, trim the sum column
import_data <- read.xlsx("../FromRebecca/data/Rimport.xlsx", 
                      header = TRUE, sheetIndex = 1)
import_data <- import_data[, -17]

# create new factor for treatment
import_data <- cbind(import_data, Treatment = import_data$Group)
import_data$Treatment <- mapvalues(import_data$Treatment, from = levels(import_data$Treatment),
                              to = c("Control", "HOPO, 24 h pre", "HOPO, 1 h pre","DTPA, 1 h pre",
                                     "HOPO, 1 h post", "DTPA, 1 hr post", 
                                     "HOPO, 24 h post", "HOPO, 48 h post"))

# make new data for daily urine and feces collections
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

# scale data to be out of 100%
excreta$Value <- excreta$Value * 100
mdata <- melt(import_data, id = colnames(import_data)[idcols])
mdata$value <- mdata$value * 100

# build data for bodily Gd retention, scale to 100%
reten <- cbind(import_data[,idcols], Body = rowSums(import_data[, 5:14]), 
               Excreta = rowSums(import_data[, 15:16]))
mreten <- melt(reten, id = colnames(reten)[1:5])
colnames(mreten) <- c(colnames(mreten)[1:5], "Location", "Value")
mreten$Location <- mapvalues(mreten$Location, from = levels(mreten$Location), 
                             c("Retained", "Excreted"))
mreten$Value <- mreten$Value * 100

# build data for daily Gd excretion
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


#---------- END OF DATA PROCESSING !-!-! PLOTTING STARTS ----------------
w = 0.65
fwid = 9
fhei = 6
theme_set(theme_bw())
theme_update(plot.title = element_text(hjust = 0.5),
             panel.grid.major = element_blank(),
             panel.grid.minor = element_blank())

grouppalatte = c('#41ad5b','#08306b','#2171b5','#cb181d','#4292B6','#fb6a4a','#6baed6','#9ecae1')


# Make total biodistribution figure

orgreten <- ddply(mdata[ which(mdata$variable != "Urine" & mdata$variable != "Feces"), ], 
                  .(Treatment, variable, Ligand), summarize, Ave = mean(value), 
                  Error = sd(value))
orgreten$variable <- with(orgreten, factor(variable, levels = rev(levels(variable))))

pos <- position_dodge(width = 0.9)

p <- ggplot(orgreten[ which(orgreten$variable == "Skeleton" | orgreten$variable == "Liver" | orgreten$variable == "Soft" | orgreten$variable == "ART"), ],
       aes(x = Treatment, weight = Ave, ymin = Ave-Error, ymax = Ave+Error, fill = variable, group = variable)) +
       geom_col(aes(y = Ave, fill = variable), color = 'black', position = pos) +
         geom_errorbar(position = pos) +
         geom_col(data = ddply(mreten[which(mreten$Location == "Retained"), ], .(Treatment, Ligand), summarize, Ave=mean(Value),
                               Error = sd(Value)),
                  aes(y = Ave, fill = NULL, group = NULL),
                  alpha = 0.25, color = 'black')

p +  geom_errorbar(data = ddply(mreten[which(mreten$Location == "Retained"), ], .(Treatment), summarize, Ave=mean(Value),
                                    Error = sd(Value)),
                       aes(fill = NULL, group = NULL, ymin = Ave - Error, ymax = Ave + Error),
                       position = pos, width = 0.4) +
         #scale_fill_grey(end = 1) +
         scale_fill_brewer(palette = "Spectral") +
  scale_fill_manual(values = c("firebrick","chartreuse3","darkorchid2","blue2")) +
         theme(axis.text.x = element_text(angle = 35, hjust = 1)) +
         scale_y_continuous(name = expression(''^153*Gd~'Content (% RD)'), limits = c(0,69), expand = c(0, 0)) +
         theme(legend.title = element_blank()) +
  theme(legend.position = c(0.3,0.7)) +
  xlab(NULL)# +
          # ggsave(filename = '../../../Manuscripts/2017_GdDecorporation/Overleaf/biodistribution1_preinkscape.pdf',
          #        width = 5.5, height = 5.5, units = "in")


# Make individual organ retention figure
# reorder organ factor to sort by contribution
mdata$variable <- factor(mdata$variable, 
                        levels = levels(mdata$variable)[c(10,7,9,8,5,3,4,6,1,2)])

ggplot(mdata [ which(mdata$variable != "Urine" & mdata$variable != "Feces"), ],
       aes(x = Treatment, y = value)) +
  geom_jitter(aes(color = Treatment, shape = Ligand), width = 0.2) +
  #scale_fill_grey(end = 1) +
  theme(axis.text.x = element_text(angle = 35, hjust = 1)) +
  scale_y_continuous(name = expression(''^153*Gd~'Content (% RD)'), expand = c(0.02,0)) +
  facet_grid(variable ~ ., scales = "free") +
  theme(legend.title = element_blank()) +
  theme(legend.position = "none") +
  xlab(NULL) +
  theme(panel.spacing = unit(0.2, "lines")) +
  scale_color_manual(values = grouppalatte)# +
  ggsave(filename = '../../../Manuscripts/2017_GdDecorporation/Overleaf/allorgansjitter_preinkscapemarks.pdf',
         width = 6, height = 7, units = "in")


# Make the total excretions figure
totexcreta$Excretion <- as.factor(totexcreta$Excretion)
allexcreta <- rbind(totexcreta, excreta)

ggplot(ddply(allexcreta, .(Day, Treatment, Excretion), summarize, Ave = mean(Value),
             Error = sd(Value)),
             aes(x = Day, y = Ave, by = Treatment, group = Treatment)) +
  geom_col(aes(fill = Treatment), position = "dodge") +
  facet_grid(. ~ Excretion) +
  scale_x_discrete(name = "Study Day", labels = c("1", "2", "3", "4"), expand = c(0.01, 0.01)) +
  scale_y_continuous(name = expression(''^153*Gd~'Content (% RD)'), limits = c(0,100), expand = c(0, 0)) +
  #geom_errorbar(aes(ymin = Ave - Error, ymax = Ave + Error), size = 0.4, width = 0.8, color = "black", position = pos) +
  theme(legend.title = element_blank()) +
  scale_fill_manual(values = grouppalatte) +
  theme(legend.position = "bottom") +
  theme(panel.spacing = unit(0.3, "lines"))# +
  # ggsave(filename = '../../../Manuscripts/2017_GdDecorporation/Overleaf/totalexcretions.pdf',
  #        width = 5.5, height = 5, units = "in")


# Make the cumulative excretions figure
totcumexcreta$Excretion <- as.factor(totcumexcreta$Excretion)
allcumexcreta <- rbind(totcumexcreta, cumexcreta)

ggplot(ddply(allcumexcreta, .(Day, Treatment, Excretion, Ligand), summarize, Ave=mean(Value),
             Error = sd(Value)),
       aes(x = Day, y = Ave, group = Treatment)) +
  geom_line(aes(color = Treatment), size = 0.7) +
  scale_x_discrete(name = "Study Day", labels = c("1", "2", "3", "4"), expand = c(0.01, 0.01)) +
  scale_y_continuous(name = expression(''^153*Gd~'Content (% RD)'), expand = c(0.05, 0.05)) +
  geom_point(aes(color = Treatment, shape = Ligand), size = 1.7) +
  geom_point(data = ddply(allcumexcreta[ which(allcumexcreta$Group == "A"), ],
             .(Day, Treatment, Excretion, Ligand), summarize, Ave=mean(Value)),
             color = grouppalatte[1]) +
  geom_line(data = ddply(allcumexcreta[ which(allcumexcreta$Group == "A"), ],
                          .(Day, Treatment, Excretion, Ligand), summarize, Ave=mean(Value)),
             color = grouppalatte[1]) +
  theme(legend.title = element_blank()) +
  scale_color_manual(values = grouppalatte) +
  theme(panel.spacing = unit(0.6, "lines")) +
  facet_grid(Excretion ~ ., scales = "free") #+
  #ggsave(filename = '../../../Manuscripts/2017_GdDecorporation/Overleaf/cumulativeexcretions_marker.pdf',
  #       width = 5.5, height = 5.5, units = "in")



#---- ! ! STATISTICAL ANALYSIS SECTION ! ! ----
library(outliers)
library(multcomp)
library(xtable)

# exemplary statistical analysis of groups using Dunnett's or Tukey's tests
fit <- aov(Body ~ Group, data = reten)
summary(fit)
summary(glht(fit, linfct=mcp(Group="Dunnett")))
summary(glht(fit, linfct=mcp(Group="Tukey")))

fit <- aov(value ~ Group, data = mdata[ which(mdata$variable == "ART"), ])
summary(fit)
summary(glht(fit, linfct=mcp(Group="Dunnett")))
summary(glht(fit, linfct=mcp(Group="Tukey")))

