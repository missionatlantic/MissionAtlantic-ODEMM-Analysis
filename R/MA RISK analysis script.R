##set working directory

require (RColorBrewer)
require (plyr)
require (ggplot2)
require (gridExtra)
require (pvclust)
require (reshape2)

# read in data
library(readxl)
Input <- read_excel("Input.xlsx")
View(Input)
data = Input


### Calculate Impact Risk, Recovery Lag and log IR
data$ImpactRisk = data$Overlap*data$Frequency*data$DoI
data$LN.IR = log(data$ImpactRisk)


### set up data frame for some of the initial plots
BPIS = data
names(BPIS)[names(BPIS) == 'Ecological.Characteristic'] <- 'EcoChar'


######     Conectance Summary Plots             ########################
########################################################################
########################################################################

## Bar plots for proportional connectance, and numbers of pressures and  
## ecological characteristics effected

require (plyr)

#sapply(BPIS, class) - to check the class of the objects
BPIS$Sector = as.factor(BPIS$Sector)
BPIS$Pressure = as.factor(BPIS$Pressure)
BPIS$EcoChar = as.factor(BPIS$EcoChar)

#sapply(BPIS, class) - to check the class of the objects

d = ddply (BPIS, c("Sector", "Pressure", "EcoChar"), summarise,
           Count = length(ImpactRisk))

###########################################
#### Sectors
###########################################

SP = ddply (d, c("Sector", "Pressure"), summarise,
            Sum = sum(Count))

SE = ddply (d, c("Sector", "EcoChar"), summarise,
            Sum = sum(Count))

SecPress = ddply (SP, c("Sector"), summarise,
                  TotLinks = sum(Sum),
                  Pressures = length(Sum))

SecEco = ddply (SE, c("Sector"), summarise,
                Eco = length(Sum))

Sectors = merge (SecPress, SecEco)



Sectors$Connect = (Sectors$TotLinks/ (length (BPIS$ImpactRisk))) *100

Sectors = Sectors[order(Sectors$Connect, decreasing = TRUE), ]

write.csv (Sectors, "Data Summary Sectors.csv")
png ("Data Summary Sectors.png", width = 700, height = 700)
grid.table(Sectors)
dev.off()

## Plots

Sectors$Sector <- factor(Sectors$Sector, levels = Sectors$Sector[order(Sectors$Connect)])

pdf ("Connectance Sectors.pdf", width = 6, height = 8)
ggplot (Sectors, aes (x = Sector, y = Connect))+
  geom_bar(stat = 'identity', colour = "red", fill = "red") +
  ylab ("Proportional Connectance")+
  theme_bw() +
  coord_flip()
dev.off()


###########################################
### Pressures
###########################################

PE = ddply (d, c("Pressure", "EcoChar"), summarise,
            Sum = sum(Count))

PressSec = ddply (SP, c("Pressure"), summarise,
                  TotLinks = sum(Sum),
                  Sectors = length(Sum))

PressEco = ddply (PE, c("Pressure"), summarise,
                  Eco = length(Sum))

Pressures = merge (PressSec, PressEco)



Pressures$Connect = (Pressures$TotLinks/ (length (BPIS$ImpactRisk))) *100

Pressures = Pressures[order(Pressures$Connect, decreasing = TRUE), ]


## Plots pressure Prop Connectance

Pressures$Pressure <- factor(Pressures$Pressure, levels = Pressures$Pressure[order(Pressures$Connect)])

pdf("Connectance Pressures.pdf", width = 6, height = 8)
ggplot (Pressures, aes (x = Pressure, y = Connect))+
  geom_bar(stat = 'identity', colour = "red", fill = "red") +
  ylab ("Proportional Connectance")+
  theme_bw() +
  coord_flip()

dev.off()



###########################################
### Eco Char
###########################################

EcoSec = ddply (SE, c("EcoChar"), summarise,
                TotLinks = sum(Sum),
                Sectors = length(Sum))

EcoPress = ddply (PE, c("EcoChar"), summarise,
                  Pressure = length(Sum))

Eco = merge (EcoSec, EcoPress)


Eco$Connect = (Eco$TotLinks/ (length (BPIS$ImpactRisk))) *100

Eco = Eco[order(Eco$Connect, decreasing = TRUE), ]


## Plots EcoChar Prop Connectance

Eco$EcoChar <- factor(Eco$EcoChar, levels = Eco$EcoChar[order(Eco$Connect)])

pdf ("Connectance Eco Char.pdf", width = 6, height = 8)
ggplot (Eco, aes (x = EcoChar, y = Connect))+
  geom_bar(stat = 'identity', colour = "red", fill = "red") +
  ylab ("Proportional Connectance")+
  xlab ("Ecological Characteristic")+
  theme_bw() +
  coord_flip()
dev.off ()










#######################################################################
######     Box Plots             #######################################
########################################################################
########################################################################

### Creates box plots of the sectors, pressures and ecological characteristics
##' First use code for data input (01. Data Input and Initial Analysis.R)

require (RColorBrewer)


### Pressures  -remember to update figure in brackets to reflect number of categories

pdf ("Box Plot Pressures.pdf", width = 12, height = 8)
par(mfrow = c(1, 3), oma = c(2, 16, 2 ,2), mar = c(5,0,1,0))

boxplot(BPIS$ImpactRisk ~ BPIS$Pressure, las=1, horizontal=TRUE, 
        col=colorRampPalette(brewer.pal(9,"Pastel1"))(12), xlab="Impact Risk", cex.axis=1.4, cex.lab = 1.4,
        at = rev(seq(1, nlevels(BPIS$Pressure), 1)))

boxplot(BPIS$LN.IR ~ BPIS$Pressure, las=1, horizontal=TRUE, 
        col=colorRampPalette(brewer.pal(9,"Pastel1"))(12), xlab="Impact Rank", cex.axis=1.4, cex.lab = 1.4,
        yaxt = "n",
        at = rev(seq(1, nlevels(BPIS$Pressure), 1)))

dev.off()


#### Eco Char -remember to update figure in brackets to reflect number of categories

pdf ("Box Plot EcoChar.pdf", width = 12, height = 8)

par(mfrow = c(1, 3), oma = c(2, 15, 2 ,2), mar = c(5,0,1,0))

boxplot(BPIS$ImpactRisk ~ BPIS$EcoChar, las=1, horizontal=TRUE, 
        col=colorRampPalette(brewer.pal(9,"Pastel1"))(23), xlab="Impact Risk", cex.axis=1.4, cex.lab = 1.4,
        at = rev(seq(1, nlevels(BPIS$EcoChar), 1)))

boxplot(BPIS$LN.IR ~ BPIS$EcoChar, las=1, horizontal=TRUE, 
        col=colorRampPalette(brewer.pal(9,"Pastel1"))(23), xlab="Impact Rank", cex.axis=1.4, cex.lab = 1.4,
        yaxt = "n",
        at = rev(seq(1, nlevels(BPIS$EcoChar), 1)))

dev.off()

#### Sectors -remember to update figure in brackets to reflect number of categories

pdf ("Box Plot Sectors.pdf", width = 12, height = 8)

par(mfrow = c(1, 3), oma = c(2, 15, 2 ,2), mar = c(5,0,1,0))

boxplot(BPIS$ImpactRisk ~ BPIS$Sector, las=1, horizontal=TRUE, 
        col=colorRampPalette(brewer.pal(9,"Pastel1"))(11), xlab="Impact Risk", cex.axis=1.4, cex.lab = 1.4,
        at = rev(seq(1, nlevels(BPIS$Sector), 1)))

boxplot(BPIS$LN.IR ~ BPIS$Sector, las=1, horizontal=TRUE, 
        col=colorRampPalette(brewer.pal(9,"Pastel1"))(11), xlab="Impact Rank", cex.axis=1.4, cex.lab = 1.4,
        yaxt = "n",
        at = rev(seq(1, nlevels(BPIS$Sector), 1)))

dev.off ()





###############################################################################
#######  Box Plots and proprotional connectance plot. #########################
#######    Used for publication    ############################################
###############################################################################

require (ggpubr)
require (gridExtra)
require (RColorBrewer)
require (ggplot2)
require(plyr)
require (grid)

### 

################################################################################
#### Sectors

SecRank = ddply (data, "Sector", summarise,
                 SumIR = sum(ImpactRisk))

SecRank = SecRank[order(-SecRank$SumIR),]

BoxPlotSector = merge(BPIS, SecRank)

Sectors = merge (Sectors, SecRank)

#################################################################################
#####  Remove the x axis text for the sector and pressure plots
#################################################################################

### Sectors
Seca = ggplot (Sectors, aes (x= reorder(Sector, SumIR), y = Connect))+
  geom_bar(stat = "identity", 
           color = "black", 
           fill = colorRampPalette(brewer.pal(9,"Pastel1"))(11)) +
  ylab ("Proportional Connectance")+
  theme_bw() +
  coord_flip()+
  theme(axis.title.y=element_blank(),
        axis.text.y = element_text(size=12),
        axis.title.x=element_blank(),
        axis.text.x =element_blank(),
        axis.ticks.x=element_blank(),
        plot.margin = unit(c(5,1,0,6), "mm"))


Secb = ggplot (BoxPlotSector , aes(x= reorder(Sector, SumIR), y = ImpactRisk))+
  geom_boxplot(fill = colorRampPalette(brewer.pal(9,"Pastel1"))(11))+
  stat_boxplot(geom ='errorbar', width =0.5)+ coord_flip()+
  theme_bw()+
  ylab("Impact Risk")+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.x =element_blank(),
        axis.ticks.x=element_blank(),
        plot.margin = unit(c(5,1,0,0), "mm"))

Secc= ggplot (BoxPlotSector, aes(x= reorder(Sector, SumIR), y = LN.IR))+
  geom_boxplot(fill = colorRampPalette(brewer.pal(9,"Pastel1"))(11))+
  stat_boxplot(geom ='errorbar', width =0.5)+ 
  coord_flip()+
  theme_bw()+
  ylab("Impact Rank")+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.x =element_blank(),
        axis.ticks.x=element_blank(),
        plot.margin = unit(c(5,5,0,0), "mm"))


BoxSector = grid.arrange(Seca, Secb , Secc, ncol = 3, widths = c(6, 4, 4))

pdf ("Box Plot Sectors no x label.pdf", width = 12, height = 8)
grid.arrange(Seca, Secb , Secc, ncol = 3, widths = c(6, 4, 4))
dev.off()

###############################################################################
#### Pressures

PressRank = ddply (data, "Pressure", summarise,
                   SumIR = sum(ImpactRisk))

PressRank = PressRank[order(-PressRank$SumIR),]


BoxPlotPressure = merge(BPIS, PressRank)

Pressures = merge (Pressures, PressRank)

#### Pressures

Pressa = ggplot (Pressures, aes (x= reorder(Pressure, SumIR), y = Connect))+
  geom_bar(stat = "identity", 
           color = "black", 
           fill = colorRampPalette(brewer.pal(9,"Pastel1"))(12)) +
  ylab ("Proportional Connectance")+
  theme_bw() +
  coord_flip()+
  theme(axis.title.y=element_blank(),
        axis.text.y = element_text(size=12),
        axis.title.x=element_blank(),
        axis.text.x =element_blank(),
        axis.ticks.x=element_blank(),
        plot.margin = unit(c(5,1,5,6), "mm"))


Pressb = ggplot (BoxPlotPressure , aes(x= reorder(Pressure, SumIR), y = ImpactRisk))+
  geom_boxplot(fill = colorRampPalette(brewer.pal(9,"Pastel1"))(12))+
  stat_boxplot(geom ='errorbar', width =0.5)+ coord_flip()+
  theme_bw()+
  ylab("Impact Risk")+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.x =element_blank(),
        axis.ticks.x=element_blank(),
        plot.margin = unit(c(5,1,5,0), "mm"))

Pressc= ggplot (BoxPlotPressure, aes(x= reorder(Pressure, SumIR), y = LN.IR))+
  geom_boxplot(fill = colorRampPalette(brewer.pal(9,"Pastel1"))(12))+
  stat_boxplot(geom ='errorbar', width =0.5)+ 
  coord_flip()+
  theme_bw()+
  ylab("Impact Rank")+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.x =element_blank(),
        axis.ticks.x=element_blank(),
        plot.margin = unit(c(5,5,5,0), "mm"))

BoxPressure = grid.arrange(Pressa, Pressb , Pressc, ncol = 3, widths = c(6, 4, 4))

pdf ("Box Plot Pressures no x label.pdf", width = 12, height = 8)
grid.arrange(Pressa, Pressb , Pressc, ncol = 3, widths = c(6, 4, 4))
dev.off()

###############################################################################
#### Eco Char

EcoRank = ddply (BPIS, "EcoChar", summarise,
                 SumIR = sum(ImpactRisk))

EcoRank = EcoRank[order(-EcoRank$SumIR),]


BoxPlotEco = merge(BPIS, EcoRank)

Eco = merge (Eco, EcoRank)

#### Plots

Ecoa = ggplot (Eco, aes (x= reorder(EcoChar, SumIR), y = Connect))+
  geom_bar(stat = "identity", 
           color = "black", 
           fill = colorRampPalette(brewer.pal(9,"Pastel1"))(23)) +
  ylab ("Connectance")+
  theme_bw() +
  coord_flip()+
  theme(axis.title.y=element_blank(),
        axis.text.y = element_text(size=12),
        axis.text.x = element_text(size=12),
        plot.margin = unit(c(0,1,5,31), "mm"))

Ecob = ggplot (BoxPlotEco , aes(x= reorder(EcoChar, SumIR), y = ImpactRisk))+
  geom_boxplot(fill = colorRampPalette(brewer.pal(9,"Pastel1"))(23))+
  stat_boxplot(geom ='errorbar', width =0.5)+ coord_flip()+
  theme_bw()+
  ylab("Impact Risk")+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x = element_text(size=12),
        plot.margin = unit(c(0,1,5,0), "mm"))

Ecoc= ggplot (BoxPlotEco, aes(x= reorder(EcoChar, SumIR), y = LN.IR))+
  geom_boxplot(fill = colorRampPalette(brewer.pal(9,"Pastel1"))(23))+
  stat_boxplot(geom ='errorbar', width =0.5)+ 
  coord_flip()+
  theme_bw()+
  ylab("Impact Rank")+
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x = element_text(size=12),
        plot.margin = unit(c(0,5,5,0), "mm"))

BoxEco = grid.arrange(Ecoa, Ecob, Ecoc, ncol = 3, widths = c(6, 4, 4))

pdf ("Box Plot Eco.pdf", width = 12, height = 8)
grid.arrange(Ecoa, Ecob , Ecoc, ncol = 3, widths = c(6, 4, 4))
dev.off()

#########################################################
### Combine all together
#########################################################

pdf ("Box Plot Crazy Arrange.pdf", width = 12, height = 18)
grid.arrange(BoxSector, BoxPressure, BoxEco, ncol = 1, heights =  c(6,7,8))
grid.text("a) Sectors", x = unit(0.02, "npc"), y = unit(0.989, "npc"), just = "left", gp=gpar(fontsize=16))
grid.text("b) Pressures", x = unit(0.02, "npc"), y = unit(0.71, "npc"), just = "left", gp=gpar(fontsize=16))
#grid.text("c) Ecological Characteristic", x = unit(0.02, "npc"), y = unit(0.388, "npc"), just = "left", gp=gpar(fontsize=12))
grid.text("Characteristic", x = unit(0.035, "npc"), y = unit(0.383, "npc"), just = "left", gp=gpar(fontsize=13))
dev.off()




#######CAN END HERE#########


########################################################################
########################################################################
######     Dendrogram Plots             ################################
########################################################################
########################################################################

### A variety of dendrogram plots 
### Code created by :..........

### Sectors by Pressure

SecPress = dcast (d, Sector ~ Pressure , value.var = "Count")

SecPress$Sector = NULL
SecPress = SecPress/SecPress 

SecPress = as.matrix(SecPress)
SecPress[is.nan(SecPress)] <- 0

SecPress.pv <- pvclust(SecPress, method.dist="binary", 
                       method.hclust="average", nboot=1000)

pdf ("Dendrogram Pressures by Sector.pdf")
plot(SecPress.pv)
axis(side=4, at=c(0,1,2,3), labels=c("0","1", "2", "3"),  
     mgp = c(0, 0.5, 0))
dev.off()


### Pressures by Sector

PressSec = dcast (d, Pressure ~ Sector, value.var = "Count")

PressSec$Pressure = NULL
PressSec = PressSec/PressSec 

PressSec = as.matrix(PressSec)
PressSec[is.nan(PressSec)] <- 0

PressSec.pv <- pvclust(PressSec, method.dist="binary", 
                       method.hclust="average", nboot=1000)

pdf ("Dendrogram Sectors by Pressure.pdf")
plot(PressSec.pv)
axis(side=4, at=c(0,1,2,3), labels=c("0","1", "2", "3"),  
     mgp = c(0, 0.5, 0))
dev.off()

### Eco Char by Sector

EcoSec = dcast (d, Sector ~ EcoChar, value.var = "Count")

EcoSec$Sector = NULL
EcoSec = EcoSec/EcoSec 

EcoSec = as.matrix(EcoSec)
EcoSec[is.nan(EcoSec)] <- 0

EcoSec.pv <- pvclust(EcoSec, method.dist="binary", 
                     method.hclust="average", nboot=1000)

pdf ("Dendrogram Eco Char by Sector.pdf")
plot(EcoSec.pv)
axis(side=4, at=c(0,1,2,3), labels=c("0","1", "2", "3"),  
     mgp = c(0, 0.5, 0))
dev.off()

### Eco Char by Pressure

EcoPress = dcast (d, Pressure  ~ EcoChar, value.var = "Count")

EcoPress$Pressure = NULL
EcoPress = EcoPress/EcoPress 

EcoPress = as.matrix(EcoPress)
EcoPress[is.nan(EcoPress)] <- 0

EcoPress.pv <- pvclust(EcoPress, method.dist="binary", 
                       method.hclust="average", nboot=1000)

pdf ("Dendrogram Eco Char by Pressure.pdf")
plot(EcoPress.pv)
axis(side=4, at=c(0,1,2,3), labels=c("0","1", "2", "3"),  
     mgp = c(0, 0.5, 0))
dev.off()




#################################################################
#################################################################
##### Relative Contribition Plots EDITED FOR Impact Risk (IR) ONLY    ############
#################################################################
#################################################################

## Plots of relative contribution of risk

require (reshape2)


TotIR = sum (BPIS$ImpactRisk)

BPIS$relIR = (BPIS$ImpactRisk/TotIR)*100


relIR = (BPIS$ImpactRisk/TotIR)*100

relIR = sort (relIR, decreasing = T)


newd = as.data.frame (relIR)
newd$index = c(1:length(newd$relIR))

df = melt (newd, id.vars = "index")

df$variable = as.character(df$variable)
df$variable [df$variable == "relIR"] = "IR Relative Contribution"
df$variable = as.factor(df$variable)


pdf("Relative Contribution Plot.pdf")
ggplot (df, aes(x= index, y=value, color = variable), log = "y")+
  geom_line (size=1) +
  geom_abline(intercept = 0, slope = 0)+ ##log scale
  scale_y_continuous(trans='log10', breaks=c(10,1,0.1,0.01,0.001, 0.0001, 0.00001, 0.000001, 0.0000001))+
  theme_bw()+
  theme (axis.title.x=element_blank(),
         axis.text.x =element_blank(),
         axis.ticks.x=element_blank(),
         axis.title.y=element_blank(),
         legend.title = element_blank(),
         legend.position = c(0.7, 0.7))
dev.off()

### No legend and background removed
ggplot (df, aes(x= index, y=value, color = variable), log = "y")+
  geom_line (size=1) +
  geom_abline(intercept = 0, slope = 0)+ ##log scale
  scale_y_continuous(trans='log10', breaks=c(10,1,0.1,0.01,0.001, 0.0001, 0.00001, 0.000001, 0.0000001))+
  theme_classic()+
  theme (axis.title.x=element_blank(),
         axis.text.x =element_blank(),
         axis.ticks.x=element_blank(),
         axis.title.y=element_blank(),
         legend.position="none")

### Black and white
ggplot (df, aes(x= index, y=value, color = variable), log = "y")+
  geom_line (size=1) +
  scale_colour_manual(values=c("grey90", "grey12", "grey45"))+
  geom_abline(intercept = 0, slope = 0)+ ##log scale
  scale_y_continuous(trans='log10', breaks=c(10,1,0.1,0.01,0.001, 0.0001, 0.00001, 0.000001, 0.0000001))+
  theme_classic()+
  theme (axis.title.x=element_blank(),
         axis.text.x =element_blank(),
         axis.ticks.x=element_blank(),
         axis.title.y=element_blank(),
         legend.position="none")

### Dashed
ggplot (df, aes(x= index, y=value), log = "y")+
  geom_line (size=1, aes(linetype=variable)) +
  #  scale_colour_manual(values=c("grey45", "grey90", "grey2"))+
  geom_abline(intercept = 0, slope = 0)+ ##log scale
  scale_y_continuous(trans='log10', breaks=c(10,1,0.1,0.01,0.001, 0.0001, 0.00001, 0.000001, 0.0000001))+
  theme_classic()+
  theme (axis.title.x=element_blank(),
         axis.text.x =element_blank(),
         axis.ticks.x=element_blank(),
         axis.title.y=element_blank(),
         legend.position="none")



######################################################################################################
#########################################################################################################
#######################           Rankings Tables           ###############################################
#########################################################################################################
#########################################################################################################

require (plyr)

###########################################################
###### Need to aggregate the data to see better patterns
##########################################################
##### Let's focus on sector initially

Sec = ddply (data, "Sector", summarise,
##            AvgTR = mean(TotalRisk),
##             SumTR = sum(TotalRisk))
             AvgIR = mean(ImpactRisk),
             SumIR = sum(ImpactRisk))

Sec.Ave = Sec[order (Sec$AvgIR, decreasing = T), ]
#Sec.Sum = Sec[order (Sec$SumTR, decreasing = T), ]

#Sec.Ave[, 2:5] = round(Sec.Ave[, 2:5], 5)
#Sec.Sum[, 2:5] = round(Sec.Sum[, 2:5], 5)

row.names(Sec.Ave) <- NULL

Sec.Ave$RankAverage = rank(-Sec.Ave$AvgIR)
Sec.Ave$RankSum = rank(-Sec.Ave$SumIR)


Sec.Ave$AvgIR = formatC(Sec.Ave$AvgIR, format = "e", digits = 2)
Sec.Ave$SumIR = formatC(Sec.Ave$SumIR, format = "e", digits = 2)

Sec.Ave = Sec.Ave[,c(1,4,2,5, 3)]

pdf ("Impact Risk Ranks by Sector.pdf")
grid.table(Sec.Ave, rows = NULL)
dev.off()

# pdf ("Total Risk Ranks by Sector (Sum).pdf")
# grid.table(Sec.Sum, rows = NULL)
# dev.off()

#############################################################################
######  Ok Let's look at pressure

Pres = ddply (data, "Pressure", summarise,
#            AvgTR = mean(TotalRisk),
#             SumTR = sum(TotalRisk)) 
           AvgIR = mean(ImpactRisk),
            SumIR = sum(ImpactRisk))

Pres.Ave = Pres[order (Pres$AvgIR, decreasing = T), ]
#Pres.Sum = Pres[order (Pres$SumTR, decreasing = T), ]

# Pres.Ave[, 2:5] = round(Pres.Ave[, 2:5], 5)
# Pres.Sum[, 2:5] = round(Pres.Sum[, 2:5], 5)

Pres.Ave$RankAverage = rank(-Pres.Ave$AvgIR)
Pres.Ave$RankSum = rank(-Pres.Ave$SumIR)


Pres.Ave$AvgIR = formatC(Pres.Ave$AvgIR, format = "e", digits = 2)
Pres.Ave$SumIR = formatC(Pres.Ave$SumIR, format = "e", digits = 2)

Pres.Ave = Pres.Ave[,c(1,4,2,5, 3)]
row.names(Pres.Ave) <- NULL


pdf ("Impact Risk Ranks by Pressure.pdf")
grid.table(Pres.Ave, rows = NULL)
dev.off()

# pdf ("Total Risk Ranks by Pressure (Sum).pdf")
# grid.table(Pres.Sum, rows = NULL)
# dev.off()

######  Ok Let's look at ecological 

EcoCh = ddply (data, "EcoChar", summarise,
 #              AvgTR = mean(TotalRisk),
 #              SumTR = sum(TotalRisk)) 
           AvgIR = mean(ImpactRisk),
           SumIR = sum(ImpactRisk))

EcoCh.Ave = EcoCh[order (EcoCh$AvgIR, decreasing = T), ]
#EcoCh.Sum = EcoCh[order (EcoCh$SumTR, decreasing = T), ]

# EcoCh.Ave[, 2:5] = round(EcoCh.Ave[, 2:5], 5)
# EcoCh.Sum[, 2:5] = round(EcoCh.Sum[, 2:5], 5)

EcoCh.Ave$RankAverage = rank(-EcoCh.Ave$AvgIR)
EcoCh.Ave$RankSum = rank(-EcoCh.Ave$SumIR)

EcoCh.Ave$AvgIR = formatC(EcoCh.Ave$AvgIR, format = "e", digits = 2)
EcoCh.Ave$SumIR = formatC(EcoCh.Ave$SumIR, format = "e", digits = 2)


EcoCh.Ave = EcoCh.Ave[,c(1,4,2,5, 3)]
row.names(EcoCh.Ave) <- NULL

pdf ("Impact Risk Ranks by Ecological Characteristic.pdf")
grid.table(EcoCh.Ave, rows = NULL)
dev.off()

#pdf ("Total Risk Ranks by Ecological Characteristic (Sum).pdf", height = 10, width = 6)
#grid.table(EcoCh.Sum, rows = NULL)
#dev.off()

#write.csv(Sec.Ave, "Sectors.csv")
#write.csv(Pres.Ave, "Pressures.csv")
#write.csv(EcoCh.Ave, "Eco.csv")

