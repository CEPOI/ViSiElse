################################################################################
#                                                                              #
#         ViSiElse: An innovative visualization R package to ensure            #
#             behavioral raw data reliability and transparency                 #
#           Elodie M. Garnier, Nastasia Fouret, Mederic Descoins               #
#                                                                              #
################################################################################


# This is the code to replicate the analyses and figures from the 2019 paper :
# ViSiElse: An innovative visualization R package to ensure behavioral raw data 
# reliability and transparency

# Correspondence concerning this code should be addressed to Elodie Garnier,
# Centre d'Etudes Perinatales de l'Ocean Indien -  EA 7388
# CHU de La Reunion - Site Sud - BP 350 - 97 448 Saint Pierre Cedex
# La Reunion, France
# e.garnier30@gmail.com OR
# mederic.descoins@chu-reunion.fr


# /!\ #
# Before running this code make sure the dataset ViSiElse_intubation_data.csv
# is located in the same repository as this R file.



################################ PREREQUISITES #################################

# Clean any saved variables 
rm(list=ls())

# Get the current directory path
path_data <- getwd()

# Install and load packages
install.packages("ViSiElse", dependencies = T)
library(ViSiElse)
library(ggplot2) # for the scatter plot and heatmap
library(reshape2) # reshape the dataset for the scatter plot and heatmap



#################################### DATA ######################################
#### Function to always generate the same data ####
normdata <- function(N, m, s){
  set.seed(11)
  return(rnorm(N, mean = m, sd = s))
}


#### Typical day dataset ####
N <- 100
id <- 1:N
start_sleep <- rep(0, N)
stop_sleep <- round(normdata(N, 390, 40), 0)
wake_up <- stop_sleep
shower <- wake_up + round(normdata(N, 15, 10), 0)
shower[shower<wake_up] <- wake_up[shower<wake_up] + 2
breakfast <- shower + round(normdata(N, 20, 10), 0)
breakfast[breakfast<shower] <- shower[breakfast<shower] + 10
start_work <- round(normdata(N, 510, 40), 0)
stop_work <- round(normdata(N, 1020, 60), 0)
start_lunch <- round(normdata(N, 750, 20), 0)
stop_lunch <- start_lunch + round(normdata(N, 60, 15), 0)
pickup_kids <- stop_work + round(normdata(N, 15, 5), 0)
pickup_kids[pickup_kids<960] <- 960
start_cook <- pickup_kids + round(normdata(N, 20, 10), 0)
stop_cook <- start_cook + round(normdata(N, 60, 20), 0)
go_sleep <- round(normdata(N, 1350, 40), 0)
go_sleep[go_sleep > 1439] <- 1439
set.seed(11)
first_coffee <- sample(c(rep(450:600, 10), rep(601:840, 4), 841:1200), 100)
typical_day <- data.frame(id, start_sleep, stop_sleep, wake_up, shower, 
                          breakfast, start_work, start_lunch, stop_lunch, 
                          stop_work, pickup_kids, start_cook, stop_cook, 
                          go_sleep, first_coffee)
head(typical_day)
# Define group of participants
group <- rep(1, N)
group[pickup_kids > 1019] <- 2


#### Intubation dataset ####
# Import the raw data from the repository
intub <- read.csv(paste(path_data, "ViSiElse_intubation_data.csv", sep = "/"), 
              sep = ";", dec = ",", header = TRUE)
head(intub)


#### Online shopping dataset ####
need <- round(normdata(N, 10, 4), 0)
start_search <- need + round(normdata(N, 10, 4), 0)
stop_search <- start_search + round(normdata(N, 30, 13), 0)
start_eval <- start_search + round(normdata(N, 20, 8), 0)
stop_eval <- start_eval + round(normdata(N, 30, 13), 0)
deci <- stop_eval + round(normdata(N, 10, 4), 0)
shop <- data.frame(id, need, start_search, stop_search, start_eval, 
                   stop_eval, deci)
head(shop)
# Define group of participants
group_shop <- c(rep(1, N/2), rep(2, N/2))



########################## GRAPHIC REPRESENTATIONS #############################

#### Basic visielse ####
# First visielse plot #
v1 <- visielse(typical_day, informer = NULL, doplot = F) # The ViSibook is generated automatically
b1 <- ConvertFromViSibook(v1@book) # Extract the visibook in a data.frame
b1 <- b1[order(as.numeric(b1$showorder)), ]
# Change the labels of the ViSibook and add long actions #
b1$label <- c("Sleep",
              "Stop sleeping",
              "Wake up", 
              "Take a shower", 
              "Eat breakfast",
              "Start working",
              "Start eating lunch",
              "End of lunch",
              "Stop working",
              "Pick up the kids",
              "Start cooking",
              "End of dinner",
              "Go to sleep",
              "First coffee")
# Define the long actions
b1[15,] <- c("sleep", "Sleeping", "l", 1, "start_sleep", "stop_sleep")
b1[16,] <- c("work", "Working", "l", 5, "start_work", "stop_work")
b1[17,] <- c("lunch", "Lunch break", "l", 6, "start_lunch", "stop_lunch")
b1[18,] <- c("cook", "Cook and eat dinner", "l", 8, "start_cook", "stop_cook")
# Define which actions should be plotted and in which order
b1$showorder <- c(NA, NA, 2, 3, 4, 5, NA, NA, 7, 9, NA, NA, 11, 12, 1, 6, 8, 10)
b1 <- b1[order(as.numeric(b1$showorder)), ]
# Plot with the modified ViSibook #
v2 <- visielse(typical_day, book = b1, informer = NULL, doplot = F, pixel = 30)
plot(v2, vp0w = 0.8, unit.tps = "min", scal.unit.tps = 30,
     main = "Typical day") #vp0w sets the width of the graph 


#### Change pixel parameter ####
# Small pixel parameter : data are not aggregated enough #
p1 <- 10
v3 <- visielse(typical_day, book = b1, informer = NULL, doplot = F, pixel = p1)
plot(v3, vp0w = 0.8, unit.tps = "min", main = "Typical day", scal.unit.tps = p1) 
# High pixel parameter : data are too aggregated #
p2 <- 120
v4 <- visielse(typical_day, book = b1, informer = NULL, doplot = F, pixel = p2)
plot(v4, vp0w = 0.8, unit.tps = "min", main = "Typical day", scal.unit.tps = p2)


#### Define groups ####
# Groups plotted with "cut" method : each group is one under the other #
v5 <- visielse(typical_day, book = b1, informer = NULL, group = group, 
               method = "cut", tests = F, pixel = 30, doplot = F)
plot(v5, vp0w = 0.8, unit.tps = "min", scal.unit.tps = 30,
     main = "Typical day") #vp0w sets the width of the graph 
# Groups plotted with "join" method : group spacially mixed #
v6 <- visielse(typical_day, book = b1, informer = NULL, group = group, 
               method = "join", tests = F, pixel = 30, doplot = F)
plot(v6, vp0w = 0.8, unit.tps = "min", scal.unit.tps = 30,
     main = "Typical day") #vp0w sets the width of the graph 
# Groups plotted with "within" method : all data are plotted together in blue 
# and the group specified in "grwithin" is plotted again in pink #
v7 <- visielse(typical_day, book = b1, informer = NULL, group = group, 
               method = "within", grwithin = "2", tests = F, pixel = 30,
               doplot = F)
plot(v7, vp0w = 0.8, unit.tps = "min", scal.unit.tps = 30,
     main = "Typical day") #vp0w sets the width of the graph 


#### Time constraints ####
b2 <- b1
b2 <- b2[order(as.numeric(b2$showorder)), ]
# Add definition of the green zones #
b2$GZDeb <- c(rep(NA, 8), 960, rep(NA, 9))
b2$GZFin <- c(rep(NA, 8), 1020, rep(NA, 9))
# Definition of the black zones before the green one #
b2$BZBeforeDeb <- c(rep(NA, 4), 600, NA, 0, NA, 0, rep(NA, 9))
b2$BZBeforeFin <- c(rep(NA, 4), Inf, NA, 960, NA, 960, rep(NA, 9))
# Add definition of the black zones after the green one #
b2$BZAfterDeb <- c(rep(NA, 8), 1020, rep(NA, 9))
b2$BZAfterFin <- c(rep(NA, 8), Inf, rep(NA, 9))
# Add definition of the time limit for long action #
b2$BZLong <- c(rep(NA, 7), 30, rep(NA, 10))
b2$BZLtype <- c(rep(NA, 7), "span", rep(NA, 10)) # type should either be "span" (for a 
# duration not to exceed) or "time" (for a deadline not to cross)
b2
v8 <- visielse(typical_day, book = b2, informer = NULL, pixel = 30, doplot = F)
plot(v8, vp0w = 0.8, unit.tps = "min", scal.unit.tps = 30,
     main = "Typical day") #vp0w sets the width of the graph 


#### Statistical indicators ####
# Mean + standard deviation #
v9 <- visielse(typical_day, book = b1, informer = "mean", tests = F, pixel = 30,
               doplot = F)
plot(v9, vp0w = 0.8, unit.tps = "min", scal.unit.tps = 30,
     main = "Typical day") #vp0w sets the width of the graph 
# Median + IQR #
v10 <- visielse(typical_day, book = b1, informer = "median", tests = F,
                pixel = 30, doplot = F)
plot(v10, vp0w = 0.8, unit.tps = "min", scal.unit.tps = 30,
     main = "Typical day") #vp0w sets the width of the graph 
# Statistical test between groups #
v11 <- visielse(typical_day, book = b1, informer = "mean", group = group,
                method = "cut", pixel = 30, doplot = F,
                tests = TRUE, threshold.test = 0.05) # run a Wilcoxon test if 
# informer is "mean" and a Mood test if informer is "median"
# threshold.test define the alpha significance level of the test
plot(v11, vp0w = 0.8, unit.tps = "min", scal.unit.tps = 30,
     main = "Typical day") #vp0w sets the width of the graph 



############################### PAPER FIGURES ##################################

#### Figure 1 ####
v12 <- visielse(typical_day, book = b2, informer = "median", pixel = 30,
                doplot = F)
png(paste(path_data, "fig1.png", sep = "/"), width = 2300, height = 1400,
     res = 300)
plot(v12, scal.unit.tps = 30, rcircle = 6, vp0h = 0.7, vp0w = 0.75, 
     Fontsize.label.Action = 9, Fontsize.label.Time = 9,
     Fontsize.label.color = 9, lwd.grid = 0.7, lwdline = 1.5, 
     main = "Typical day", unit.tps = "min")
# rcircle sets the radius of the informers circle
# vp0h sets the height of the main plot window < 1
# vp0w sets the width of the main plot window < 1
# lwdline sets the mean line width
# lwd.grid sets the grid lines width
# main sets the graph title
# unit.tps sets the time unit name
# Other arguments set the font size
dev.off()


#### Figure 2 ####
png(paste(path_data, "fig2-A.png", sep = "/"), width = 2300, height = 1300,
    res = 300)
plot(v3, scal.unit.tps = p1, vp0h = 0.7, vp0w = 0.75, Fontsize.label.Action = 9,
     Fontsize.label.Time = 9, Fontsize.label.color = 8, Fontsize.title = 14,
     lwd.grid = 0.7, main = "(A) pixel = 10min", unit.tps = "min")
dev.off()
png(paste(path_data, "fig2-B.png", sep = "/"), width = 2300, height = 1300,
    res = 300)
plot(v4, scal.unit.tps = p2, vp0h = 0.7, vp0w = 0.75, Fontsize.label.Action = 9,
     Fontsize.label.Time = 9, Fontsize.label.color = 8, Fontsize.title = 14,
     lwd.grid = 0.7, main = "(B) pixel = 120min", unit.tps = "min")
dev.off()


#### Figure 3 ####
v13 <- visielse(typical_day, book = b1, informer = "mean", pixel = 30, 
                group = group, method = "cut", doplot = F,
                tests = TRUE, threshold.test = 0.05)
png(paste(path_data, "fig3-A.png", sep = "/"), width = 1600, height = 950,
    res = 300)
plot(v13, scal.unit.tps = 30, Fontsize.label.Action = 8, vp0h = 0.65, 
     vp0w = 0.75, Fontsize.label.Time = 7, Fontsize.label.color = 7, 
     lwdline = 1, lwd.grid = 0.6, main = '(A) method = "cut"', unit.tps = "min")
dev.off()
v14 <- visielse(typical_day, book = b1, informer = "mean", pixel = 30,
                group = group, method = "join", doplot = F,
                tests = TRUE, threshold.test = 0.05)
png(paste(path_data, "fig3-B.png", sep = "/"), width = 1600, height = 950,
    res = 300)
plot(v14, scal.unit.tps = 30, Fontsize.label.Action = 8, vp0h = 0.65, 
     vp0w = 0.75, Fontsize.label.Time = 7, Fontsize.label.color = 7, 
     lwdline = 1, lwd.grid = 0.6, rcircle = 5, main = '(B) method = "join"',
     unit.tps = "min")
dev.off()
v15 <- visielse(typical_day, book = b1, informer = "mean", pixel = 30,
                group = group, method = "within", grwithin = "2", doplot = F, 
                tests = T, threshold.test = 0.05)
png(paste(path_data, "fig3-C.png", sep = "/"), width = 1600, height = 950,
    res = 300)
plot(v15, scal.unit.tps = 30, Fontsize.label.Action = 8, vp0h = 0.65, 
     vp0w = 0.75, Fontsize.label.Time = 7, Fontsize.label.color = 7, 
     lwdline = 1, lwd.grid = 0.6, main = '(C) method = "within"', 
     unit.tps = "min")
dev.off()


#### Figure 4 ####
## Scatter plot ##
# Reshape the dataset to fit the scatter plot
typDay <- typical_day[, c(1, 4:15)]
colnames(typDay) <- c("id", b1$label[c(2:5, 15, 16, 7, 9, 17, 18, 11, 12)])
typDay <- melt(typDay, id = "id")
# Graph
scatter <- ggplot(typDay, aes(x = value, y = id)) +
  facet_wrap(~variable, scales = "free") +
  geom_point(size = 0.5) +
  xlab("Time (min)") + ylab("Participants") +
  theme_minimal() +
  theme(axis.line = element_line(colour = "black"),
        axis.title = element_text(size = 10, face = "bold"),
        axis.text = element_text(colour = "black", size = 6)) +
  theme(strip.background = element_rect(fill = "#E2E8FF", colour = "#E2E8FF"), 
        strip.text = element_text(size = 8),
        strip.text.x = element_text(margin = margin(0.5, 0.5, 0.5, 0.5, "mm")))
png(paste(path_data, "fig4-A.png", sep = "/"), width = 1600, height = 950,
    res = 300)
print(scatter)
dev.off()
## Violin + scatter plot ##
violin <- ggplot(typDay, aes(x = variable, y = value)) +
  geom_violin(trim = FALSE, fill = "#E2E8FF", color = "#2D39A5", size = 0.3) +
  geom_jitter(shape = 16, position = position_jitter(0.2), color = "#2D39A5",
              size = 0.1) +
  coord_flip() +
  xlab(element_blank()) + ylab("Time (min)") +
  scale_y_continuous(breaks = seq(0, 1440, 60)) +
  scale_x_discrete(limits=levels(typDay$variable)[12:1]) +
  theme_classic() +
  theme(axis.line = element_line(colour = "black"),
        axis.title = element_text(size = 10, face = "bold"),
        axis.text = element_text(colour = "black", size = 6)) +
  theme(strip.background = element_rect(fill = "#E2E8FF", colour = "#E2E8FF"), 
        strip.text = element_text(size = 8),
        strip.text.x = element_text(margin = margin(0.5, 0.5, 0.5, 0.5, "mm")))
png(paste(path_data, "fig4-B.png", sep = "/"), width = 1600, height = 950,
    res = 300)
print(violin)
dev.off()
## Heatmap ##
# Create a frequency table for 30min time intervals
typDay <-  sapply(typical_day[,4:15], function(x){
  table(cut(x, breaks=seq(0, 1440, 30))) 
}) 
# Reshape the dataset to fit the heatmap
typDay <- data.frame(time = factor(seq(0, 1410, 30)), typDay)
rownames(typDay) <- 1:nrow(typDay)
colnames(typDay) <- c("time", 
                      b1$label[c(12, 11, 18, 17, 9, 7, 16, 15, 5, 4, 3, 2)])
typDay <- melt(typDay, id = "time")[, c(2, 1, 3)]
# Heatmap
heatmap <- ggplot(data = typDay, aes(x = time, y = variable, fill = value)) +
  geom_tile() + 
  scale_fill_gradient(low = "steelblue3", high = "red3", name = "Participants") +
  xlab("Time (min)") + ylab(element_blank()) +
  scale_x_discrete(expand = c(0, 0), breaks = seq(0, 30, 30)) +
  theme(axis.line = element_line(colour = "black"),
        axis.title = element_text(size = 10, face = "bold"),
        axis.text = element_text(colour = "black", size = 6)) +
  theme(legend.text = element_text(size = 6), 
        legend.title = element_text(size = 8),
        legend.position ="bottom", 
        legend.margin = margin(-2, 0, 0, 0, unit = "mm"),
        legend.key.width = unit(7, "mm"),
        legend.key.height = unit(2, "mm"))
png(paste(path_data, "fig4-C.png", sep = "/"), width = 1600, height = 950,
    res = 300)
print(heatmap)
dev.off()


#### Figure 5 ####
v16 <- visielse(intub, doplot = F)
b3 <- ConvertFromViSibook(v16@book)
b3$label <- c("Decision to intubate",
              "Stop ventilation", 
              "Laryngoscope\nblade in",
              "Insert endotracheal\ntube",
              "Laryngoscope\nblade out",
              "Restart ventilation")
b3[7,] <- c("dur_laryngoscope", "Laryngoscope\nduration use", "l", "8",
            "blade_in", "blade_out")
b3[8,] <- c("dur_intub", "Intubation duration", "l", "9", "stop_ventil",
            "restart_ventil")
b3$GZDeb <- c(NA, NA, 120, NA, NA, NA, NA, NA)
b3$GZDeb <- c(NA, NA, 120, NA, NA, NA, NA, NA)
b3$GZFin <- c(NA, NA, 210, NA, NA, NA, NA, NA)
b3$BZBeforeDeb <- c(NA, NA, 0, NA, NA, NA, NA, NA)
b3$BZBeforeFin <- c(NA, NA, 120, NA, NA, NA, NA, NA)
b3$BZAfterDeb <- c(NA, NA, 210, NA, NA, NA, NA, NA)
b3$BZAfterFin <- c(NA, NA, Inf, NA, NA, NA, NA, NA)
b3$BZLong <- c(rep(NA, 7), 30)
b3$BZLtype <- c(rep(NA, 7), "span")
v17 <- visielse(intub, book = b3, informer = "median", doplot = F)
png(paste(path_data, "fig5.png", sep = "/"), width = 2300, height = 1400,
    res = 300)
plot(v17, scal.unit.tps = 20, rcircle = 8, vp0h = 0.65, vp0w = 0.7, 
     Fontsize.label.Action = 9, Fontsize.label.Time = 9,
     Fontsize.label.color = 9, 
     main = "Intubation process in neonatal resuscitation algorithm")
dev.off()


#### Figure 6 ####
v18 <- visielse(shop, doplot = F)
b4 <- ConvertFromViSibook(v18@book)
b4$label <- c("Need recognition",
              "Start information search", 
              "Stop information search",
              "Start evaluation",
              "Stop evaluation",
              "Purchase decision")
b4$showorder <- c(1, NA, NA, NA, NA, 4)
b4[7,] <- c("search", "Information search", "l", "2", "start_search", 
            "stop_search")
b4[8,] <- c("eval", "Evaluation", "l", "3", "start_eval", "stop_eval")
v19 <- visielse(shop, book = b4, informer = "mean", pixel = 5, 
                group = group_shop, method = "cut")
png(paste(path_data, "fig6.png", sep = "/"), width = 2300, height = 1400,
    res = 300)
plot(v19, scal.unit.tps = 5, rcircle = 8, vp0h = 0.6, vp0w = 0.75, 
     Fontsize.label.Action = 9, Fontsize.label.Time = 9,
     Fontsize.label.color = 9, lwd.grid = 1, lwdline = 2, 
     main = "Online shopping behaviour", unit.tps = "min")
dev.off()



################################################################################

#### Maximum plotted actions per graph ####
# On a 1920x1080 px screen : 21.5 inch #
X <- cbind(typical_day[, c(1, 3:12)], 
           do.call(cbind, replicate(5, typical_day[, 3:12], simplify = F))) # 60 V.A.
X <- X - 300
colnames(X) <- make.names(colnames(X), unique = T)
v16 <- visielse(X, informer = NULL, pixel = 30, doplot = F)
plot(v16, Fontsize.label.Action = 8, scal.unit.tps = 30, 
     vp0w = 0.75, vp0h = 0.65, unit.tps = "min")


#### Maximum plotted individuals per graph ####
# On a 1920x1080 px screen : 21.5 inch #
X2 <- do.call(rbind, replicate(7, typical_day, simplify=FALSE))
X2$id <- 1:length(X2$id)
v17 <- visielse(X2, book = b1, informer = NULL, pixel = 30, doplot = F)
plot(v17, Fontsize.label.Action = 8, scal.unit.tps = 30, 
     vp0w = 0.75, vp0h = 0.65, unit.tps = "min")


#### Other parameter of interest ####
# Visualize density insted of quantity of individuals #
visielse(typical_day, book = b1,
         informer = NULL, quantity = "dens") # Change the legend with '%'
visielse(typical_day, book = b1,
         informer = NULL, quantity = "N") # Change the legend with 'N'
# Personnalize the graph formatting options #
plot(v8, main = "Typical day", size.main = 16, col.main = "darkcyan", 
     Fontsize.label.Action = 12, scal.unit.tps = 20, colblackzone = "red",
     colgreenzone = "forestgreen", unit.tps = "min")
# All formating option are describe in the CRAN pdf of the package #


#### Heatmap with ViSiElse style ####
# Reuse Figure 4 heatmap dataset and replace 0 by NA
# NA value will be colored in white and 0 value will not be taken into account 
# to create the gradient of color in the heatmap
typDay[typDay == 0] <- NA  
# Heatmap
heatmap <- ggplot(data = typDay, aes(x = time, y = variable, fill = value)) +
  geom_tile() + 
  scale_fill_gradient(low = "#E2E8FF", high = "#2D39A5", name = "Participants",
                      na.value = 'white', limit = c(0, 53)) +
  xlab("Time (min)") + ylab(element_blank()) +
  scale_x_discrete(expand = c(0, 0), breaks = seq(0, 60, 30)) +
  theme(axis.line = element_line(colour = "black"),
        axis.title = element_text(size = 12, face = "bold"),
        axis.text = element_text(colour = "black", size = 8)) +
  theme(legend.text = element_text(size = 8), 
        legend.title = element_text(size = 10),
        legend.position ="bottom", 
        legend.margin = margin(0, 0, 0, 0, unit = "mm"),
        legend.key.width = unit(1, "cm"),
        legend.key.height = unit(3, "mm"))
png(paste(path_data, "figReviewer.png", sep = "/"), width = 2300, height = 1400,
    res = 300)
print(heatmap)
dev.off()


#### To export the simulated datasets ####
# Typical day dataset
write.table(typical_day, 
            paste(path_data, "ViSiElse_typDay_data.csv", sep = "/"), 
            sep = ";", dec = ",", col.names = TRUE, row.names = FALSE)
write.table(shop, 
            paste(path_data, "ViSiElse_shopping_data.csv", sep = "/"), 
            sep = ";", dec = ",", col.names = TRUE, row.names = FALSE)



#### For more information ####
# R help function
help("visielse")

# Step by step coffee example: 
# https://cran.r-project.org/web/packages/ViSiElse/vignettes/ViSiElSe_Step_by_Step.html

# CRAN package pdf :
# https://cran.r-project.org/web/packages/ViSiElse/ViSiElse.pdf


