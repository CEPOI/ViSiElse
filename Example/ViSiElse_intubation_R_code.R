################################################################################
#                                                                              #
#         ViSiElse: An innovative visualization R package to ensure            #
#             behavioral raw data reliability and transparency                 #
#           Elodie M. Garnier, Nastasia Fouret, Mederic Descoins               #
#                                                                              #
################################################################################


# This is the code to replicate the analyses and figures from the 2019 paper :
# ViSiElse: An innovative visualization R package to ensure behavioral raw data 
# reliability and transparency, based on the data file 
# ViSiElse_intubation_data.csv

# Correspondence concerning this code should be addressed to Elodie Garnier,
# Centre d'Etudes Perinatales de l'Ocean Indien -  EA 7388
# CHU de La Reunion - Site Sud - BP 350 - 97 448 Saint Pierre Cedex
# La Reunion, France
# e.garnier30@gmail.com



################################ PREREQUISITES #################################

# Clean any saved variables 
rm(list=ls())

# Create a repository on your desktop called "ViSiElse Paper" and copy-paste
# the supplementary material from the paper (ViSiElse_intubation_R_code.R and 
# ViSiElse_intubation_data.csv) into the repository and then open the R file
# again.
# You can now execute all R code below.
path_data <- getwd()

# Install and load ViSiElse package
install.packages("ViSiElse", dependencies = T)
library(ViSiElse)



#################################### DATA ######################################

# Import the raw data from the repository
X <- read.csv(paste(path_data, "ViSiElse_intubation_data.csv", sep = "/"), 
              sep = ";", dec = ",", header = TRUE)
head(X)
# Define the groups
groups <- c(rep(1,10), rep(2,16), 1, rep(2,3), rep(1,7))




########################## GRAPHIC REPRESENTATIONS #############################

#### Basic visielse ####
# First visielse plot #
v1 <- visielse(X, informer = NULL) # The ViSibook is generated automatically
b1 <- ConvertFromViSibook(v1@book) # Extract the visibook in a data.frame
# Change the labels of the ViSibook and add long actions #
b1$label <- c("Decision to intubate", 
              "Stop ventilation", 
              "Laryngoscope\nblade in",
              "Insert endotracheal\ntube",
              "Laryngoscope\nblade out",
              "Restart ventilation")
b1[7,] <- c("dur_laryngoscope","Laryngoscope\nduration use","l","8",
             "blade_in","blade_out")
b1[8,] <- c("dur_intub","Intubation duration","l","9","stop_ventil",
             "restart_ventil")
# Plot with the modified ViSibook #
v2 <- visielse(X, book = b1, informer = NULL)



#### Change pixel parameter ####
# Small pixel parameter : data are not aggregated enough #
p1 <- 2
v3 <- visielse(X, book = b1, informer = NULL, pixel = p1)
# High pixel parameter : data are too aggregated #
p2 <- 100
v4 <- visielse(X, book = b1, informer = NULL, pixel = p2)


#### Define groups ####
# Groups plotted with "cut" method : each group is one under the other #
v5 <- visielse(X, book = b1, informer = NULL, group = groups, method = "cut",
               tests = F)
# Groups plotted with "join" method : group spacially mixed #
v6 <- visielse(X, book = b1, informer = NULL, group = groups, method = "join",
               tests = F)
# Groups plotted with "within" method : all data are plotted together in blue 
# and the group specified in "grwithin" is plotted again in pink #
v7 <- visielse(X, book = b1, informer = NULL, group = groups, method = "within",
               grwithin = "1", tests = F)


#### Time constraints ####
b2 <- b1
# Add definition of the green zones #
b2$GZDeb <- c(NA,NA,120,NA,NA,NA,NA,NA)
b2$GZFin <- c(NA,NA,210,NA,NA,NA,NA,NA)
# Definition of the black zones before the green one #
b2$BZBeforeDeb <- c(NA,NA,0,NA,NA,NA,NA,NA)
b2$BZBeforeFin <- c(NA,NA,120,NA,NA,NA,NA,NA)
# Add definition of the black zones after the green one #
b2$BZAfterDeb <- c(NA,NA,210,NA,NA,NA,NA,NA)
b2$BZAfterFin <- c(NA,NA,Inf,NA,NA,NA,NA,NA)
# Add definition of the time limit for long action #
b2$BZLong <- c(rep(NA,7), 30)
b2$BZLtype <- c(rep(NA,7), "span") # type should either be "span" (for a 
# duration not to exceed) or "time" (for a deadline not to cross)
b2
v8 <- visielse(X, book = b2, informer = NULL)


#### Statistical indicators ####
# Mean + standard deviation #
v9 <- visielse(X, book = b1, informer = "mean", tests = F)
# Median + IQR #
v10 <- visielse(X, book = b1, informer = "median", tests = F)
# Statistical test between groups #
v11 <- visielse(X, book = b1, informer = "mean", group = groups, method = "cut",
               tests = TRUE, threshold.test = 0.3) # run a Wilcoxon test if 
# informer is "mean" and a Mood test if informer is "median"
# threshold.test define the alpha significance level of the test


#### Paper figures ####
# Figure 1 #
v12 <- visielse(X, book = b2, informer = "median")
tiff(paste(path_data, "fig1.tif", sep = "/"), width = 2000, height = 1450,
     res = 300, compression = 'lzw')
plot(v12, scal.unit.tps = 20, rcircle = 8, vp0h = 0.65, vp0w = 0.7, 
     Fontsize.label.Action = 9, Fontsize.label.Time = 9,
     Fontsize.label.color = 9, 
     main = "Intubation process in neonatal resuscitation algorithm")
# rcircle sets the radius of the informers circle
# vp0h sets the height of the main plot window < 1
# vp0w sets the width of the main plot window < 1
# main sets the graph title
# Other arguments set the font size
dev.off()
# Figure 2 #
tiff(paste(path_data, "fig2-A.tif", sep = "/"), width = 1800, height = 1300,
     res = 300)
plot(v3, scal.unit.tps = p1, vp0h = 0.65, vp0w = 0.7, Fontsize.label.Action = 9,
     Fontsize.label.Time = 9, Fontsize.label.color = 8, Fontsize.title = 14,
     main = "(A) pixel = 2s")
dev.off()
tiff(paste(path_data, "fig2-B.tif", sep = "/"), width = 1800, height = 1300,
     res = 300)
plot(v4, scal.unit.tps = p2, vp0h = 0.65, vp0w = 0.7, Fontsize.label.Action = 9,
     Fontsize.label.Time = 9, Fontsize.label.color = 8, Fontsize.title = 14,
     main = "(B) pixel = 100s")
dev.off()
# Figure 3 #
b1$label <- c("Decision to intubate",
              "Stop ventilation",
              "Laryngoscope blade in",
              "Insert endotracheal tube",
              "Laryngoscope blade out",
              "Restart ventilation",
              "Laryngoscope duration use",
              "Intubation duration")
v13 <- visielse(X, book = b1, informer = "mean", group = groups, method = "cut",
                tests = TRUE, threshold.test = 0.3)
tiff(paste(path_data, "fig3-A.tif", sep = "/"), width = 1550, height = 850, 
     res = 300)
plot(v13, scal.unit.tps = 20, Fontsize.label.Action = 8, vp0h = 0.6, 
     vp0w = 0.6, Fontsize.label.Time = 7, Fontsize.label.color = 7, 
     lwdline = 1, main = '(A) method = "cut"')
dev.off()
v14 <- visielse(X, book = b1, informer = "mean", group = groups, 
                method = "join", tests = TRUE, threshold.test = 0.3)
tiff(paste(path_data, "fig3-B.tif", sep = "/"), width = 1550, height = 850,
     res = 300)
plot(v14, scal.unit.tps = 20, Fontsize.label.Action = 8, vp0h = 0.6, 
     vp0w = 0.6, Fontsize.label.Time = 7, Fontsize.label.color = 7, 
     lwdline = 1, rcircle = 5, main = '(B) method = "join"')
dev.off()
v15 <- visielse(X, book = b1, informer = "mean", group = groups, 
                method = "within", grwithin = "1",  tests = T,
                threshold.test = 0.3)
tiff(paste(path_data, "fig3-C.tif", sep = "/"), width = 1550, height = 850,
     res = 300)
plot(v15, scal.unit.tps = 20, Fontsize.label.Action = 8, vp0h = 0.6, 
     vp0w = 0.6, Fontsize.label.Time = 7, Fontsize.label.color = 7, 
     lwdline = 1, main = '(C) method = "within"')
dev.off()


#### Maximum plotted actions per graph ####
# On a 1920x1080 px screen : 21.5 inch #
X2 <- cbind(X, do.call(cbind, replicate(9, X[,2:7], simplify=FALSE))) # 60 V.A.
colnames(X2) <- make.names(colnames(X2), unique = T)
v16 <- visielse(X2, informer = NULL)
plot(v16, Fontsize.label.Action = 8, scal.unit.tps = 20)
#### Maximum plotted individuals per graph ####
# On a 1920x1080 px screen : 21.5 inch #
X3 <- do.call(rbind, replicate(50, X, simplify=FALSE))
X3$id2 <- 1:length(X3$id2)
v13 <- visielse(X3, book = b1, informer = NULL)


#### Other parameter of interest ####
# Visualize density insted of quantity of individuals #
visielse(X, informer = NULL, quantity = "dens") # Change the legend with '%'
visielse(X, informer = NULL, quantity = "N") # Change the legend with 'N'
# Personnalize the graph formatting options #
plot(v1, main = "Intubation process", size.main = 16, col.main = "red", 
     Fontsize.label.Action = 12, scal.unit.tps = 20)
# All formating option are describe in the CRAN pdf of the package #


# For more information see help("visielse") #


