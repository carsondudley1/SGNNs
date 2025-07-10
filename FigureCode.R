#Libraries
library(readr)
library(tidyverse)
library(ggfittext)
library(patchwork)

###############################################
#           Figure 1                          #
###############################################
#Simulation trajectories
set.seed(42)
generate_trajectory <- function(type = "logistic", id = 1) {
  time <- seq(0, 10, length.out = 200)
  y <- switch(type,
              logistic = 1 / (1 + exp(-0.8 * (time - 5))) + rnorm(length(time), 0, 0.02),
              exponential = 0.1 * exp(0.4 * time) / max(exp(0.4 * time)),
              oscillatory = 0.5 * sin(1.5 * time) + 0.5,
              decay = exp(-0.5 * time),
              sigmoid = tanh(0.8 * (time - 5)) / 2 + 0.5,
              linear = 0.1 * time + rnorm(length(time), 0, 0.02),
              rep(NA, length(time)))
  data.frame(time = time, value = y, traj_type = type, id = id)
}


types <- c("logistic", "exponential", "oscillatory", "decay", "sigmoid", "linear")
trajectories <- do.call(rbind, lapply(seq_along(types), function(i) {
  generate_trajectory(types[i], id = i)
}))

ggplot(trajectories, aes(x = time, y = value, color = as.factor(id))) +
  geom_line(size = 4) +
  theme_classic(base_size = 14) +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_line(size = 2), 
    legend.position = "none"
  )


#Added effects
time <- seq(0, 20, length.out = 100)
true_values <- 1 / (1 + exp(-0.5 * (time - 10)))  


stochastic_noise <- rnorm(length(time), mean = 0, sd = 0.05)
underreporting_factor <- runif(length(time), min = 0.6, max = 0.9)
delayed_obs <- c(rep(NA, 3), head(true_values, -3)) 

observed_values <- delayed_obs * underreporting_factor + stochastic_noise
observed_values[is.na(observed_values)] <- NA  

df <- data.frame(
  time = time,
  truth = true_values,
  observed = observed_values
)

ggplot(df, aes(x = time)) +
  geom_point(aes(y = truth), color = "black", size = 5.5) +     
  geom_line(aes(y = observed), color = "red", size = 4, alpha = 0.8) +           
  theme_classic() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_line(size = 2),
    legend.position = "none"
  )



#Forecasts
set.seed(123)
time <- seq(0, 30, length.out = 200)
split_pt <- 20  


true_vals <- 1 / (1 + exp(-0.3 * (time - 15)))

obs_time <- time[time <= split_pt]
obs_truth <- true_vals[time <= split_pt]
obs_noise <- obs_truth + rnorm(length(obs_time), 0, 0.03)

forecast_time <- time[time >= split_pt]
forecast_mean <- tail(obs_truth, 1) + 0.02 * (forecast_time - split_pt)
forecast_sd <- seq(0.05, 0.15, length.out = length(forecast_time))
forecast_upper <- forecast_mean + 1.96 * forecast_sd
forecast_lower <- forecast_mean - 1.96 * forecast_sd

df_truth <- data.frame(time = time, value = true_vals, type = "True")
df_obs <- data.frame(time = obs_time, value = obs_noise, type = "Observed")
df_fcst <- data.frame(time = forecast_time, mean = forecast_mean,
                      lower = forecast_lower, upper = forecast_upper,
                      type = "Forecast")
ggplot() +
  geom_line(data = df_truth, aes(time, value), color = "black", size = 4) +
  geom_point(data = df_obs, aes(time, value), color = "blue", size = 4, alpha = 0.7) +
  geom_line(data = df_fcst, aes(time, mean), color = "red", linetype = "dashed", size = 4) +
  geom_ribbon(data = df_fcst, aes(time, ymin = lower, ymax = upper), fill = "red", alpha = 0.2) +
  
  theme_classic(base_size = 14) +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_line(size = 4),
    legend.position = "none"
  )




#Parameter Estimation Schematic
set.seed(42)

true_k <- 0.8

estimated_k <- rnorm(100, mean = true_k, sd = 0.05)

ggplot(data.frame(estimate = estimated_k), aes(x = estimate)) +
  geom_histogram(bins = 20, fill = "skyblue", color = "white", alpha = 0.9) +
  geom_vline(xintercept = true_k, color = "red", linetype = "dashed", size = 4) +
  theme_classic(base_size = 14) +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_line(size = 4),
    legend.position = "none"
  )





#Parameter Estimation Schematic
set.seed(123)

n <- 99
x1 <- rnorm(n, mean = rep(c(2, 6, 10), each = n %/% 3), sd = 0.5)
y1 <- rnorm(n, mean = rep(c(2, 6, 3), each = n %/% 3), sd = 0.5)
cluster <- factor(rep(1:3, each =n %/% 3))
df_cluster <- data.frame(x = x1, y = y1, cluster = cluster)


x2 <- seq(0, 10, length.out = 100)
y2 <- 0.5 * x2 + sin(x2) + rnorm(100, 0, 0.4)
df_regression <- data.frame(x = x2, y = y2)


ggplot(df_cluster, aes(x, y, color = cluster)) +
  geom_point(size = 6) +
  theme_classic() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_line(size = 4),
    legend.position = "none"
  ) 

ggplot(df_regression, aes(x, y)) +
  geom_point(color = "gray40", size = 4) +
  geom_smooth(method = "lm", se = TRUE, color = "blue", fill = "lightblue", size = 4) +
  theme_classic() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_line(size = 4),
    legend.position = "none"
  ) 





#Mechanistic Interpretability
set.seed(123)

time <- seq(0, 10, length.out = 100)
logistic <- function(t, K, r, x0) {
  K / (1 + exp(-r * (t - x0)))
}

parent_K <- 1
parent_r <- 1
parent_x0 <- 5

n_curves <- 8
curves <- lapply(1:n_curves, function(i) {
  K_i <- rnorm(1, mean = parent_K, sd = 0.1)
  r_i <- rnorm(1, mean = parent_r, sd = 0.1)
  x0_i <- rnorm(1, mean = parent_x0, sd = 0.3)
  y <- logistic(time, K_i, r_i, x0_i)
  data.frame(time = time, value = y, id = as.factor(i))
})

df <- do.call(rbind, curves)

ggplot(df, aes(x = time, y = value, group = id)) +
  geom_line(alpha = 0.8, size = 4) +
  theme_classic(base_size = 14) +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_line(size = 4),
    legend.position = "none"
  ) 



###############################################
#           Figure 2                         #
###############################################
#Read Data
#CA
CA_forecast_output <- read_csv("Data/CA_forecast_output.csv", 
                               col_types = cols(GroundTruth = col_number(), 
                                                ForecastMedian = col_number(), ForecastLower90 = col_number(), 
                                                ForecastUpper90 = col_number()))
CA_forecast_output$Week <- as.Date(CA_forecast_output$Week)

#MI
MI_forecast_output <- read_csv("Data/MI_forecast_output.csv", 
                               col_types = cols(GroundTruth = col_number(), 
                                                ForecastMedian = col_number(), ForecastLower90 = col_number(), 
                                                ForecastUpper90 = col_number()))
MI_forecast_output$Week <- as.Date(MI_forecast_output$Week)


#NY
NY_forecast_output <- read_csv("Data/NY_forecast_output.csv", 
                               col_types = cols(GroundTruth = col_number(), 
                                                ForecastMedian = col_number(), ForecastLower90 = col_number(), 
                                                ForecastUpper90 = col_number()))
NY_forecast_output$Week <- as.Date(NY_forecast_output$Week)


#TX
TX_forecast_output <- read_csv("Data/TX_forecast_output.csv", 
                               col_types = cols(GroundTruth = col_number(), 
                                                ForecastMedian = col_number(), ForecastLower90 = col_number(), 
                                                ForecastUpper90 = col_number()))
TX_forecast_output$Week <- as.Date(TX_forecast_output$Week)

#Plot Colors
start_date <- as.Date('2021-01-01')
end_date <- as.Date('2021-11-01')
all_dates <- seq(start_date, end_date, by = 'month')
axes_labs1 <- all_dates
names <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov");

all_dates <- as.numeric(all_dates)

#New Dates
Week = data.frame(Week = as.Date('2021-01-01'), GroundTruth = NA, ForecastMedian = NA, ForecastLower90 = NA, ForecastUpper90 = NA)
MI_forecast_output = rbind(Week, MI_forecast_output)
Week = data.frame(Week = as.Date('2021-11-01'), GroundTruth = NA, ForecastMedian = NA, ForecastLower90 = NA, ForecastUpper90 = NA)
MI_forecast_output = rbind(MI_forecast_output, Week)


Ribbon <- na.omit(MI_forecast_output[, c("Week", "ForecastLower90", "ForecastUpper90", "GroundTruth")])
valid <- !is.na(MI_forecast_output$ForecastLower90) & !is.na(MI_forecast_output$ForecastUpper90)
groups <- cumsum(c(TRUE, diff(valid) != 0))
unique_groups <- unique(groups[valid])

#MI Time Series 
par(mar = c(5.1, 5.5, 4.1, 2.1))
plot(MI_forecast_output$Week, MI_forecast_output$GroundTruth, type ="l", col = 'black', 
     xlab=substitute(paste(bold(''))), ylab=substitute(paste(bold('Weekly COVID-19 Deaths'))), xaxt = "n", yaxt = "n", 
     main = "Michigan", ylim  =c(0,810), frame.plot = FALSE, cex.main = 2.5, cex.lab=2.5, lwd =5)
points(MI_forecast_output$Week, MI_forecast_output$GroundTruth, pch = 19, lwd =5)
axis(2, cex = 2,  cex.axis=1.8,cex.lab=1.75, lwd = 5, font = 2);
axis(1, at= c(min(axes_labs1), axes_labs1), cex=2, cex.axis=1.8,cex.lab=2.5, labels=FALSE, lwd = 5, font = 2);
axis(1, at=axes_labs1+15.5, labels=as.character(names), cex=2, cex.axis=1.8,cex.lab=2.5, lwd.tick=0, lwd =5, font = 2);
for (g in unique_groups) {
  idx <- which(groups == g & valid)
  polygon(
    x = c(MI_forecast_output$Week[idx], rev(MI_forecast_output$Week[idx])),
    y = c(MI_forecast_output$ForecastLower90[idx], rev(MI_forecast_output$ForecastUpper90[idx])),
    col = rgb(0, 0, 1, 0.2),
    border = NA
  )
}
lines(MI_forecast_output$Week, MI_forecast_output$ForecastMedian, col = "blue", lty = 2, lwd = 5)
points(MI_forecast_output$Week, MI_forecast_output$ForecastMedian, col = "blue", pch = 19, lwd = 5)




#TX Timeseries
#New Dates
Week = data.frame(Week = as.Date('2021-01-01'), GroundTruth = NA, ForecastMedian = NA, ForecastLower90 = NA, ForecastUpper90 = NA)
TX_forecast_output = rbind(Week, TX_forecast_output)
Week = data.frame(Week = as.Date('2021-11-01'), GroundTruth = NA, ForecastMedian = NA, ForecastLower90 = NA, ForecastUpper90 = NA)
TX_forecast_output = rbind(TX_forecast_output, Week)


#Ribbon Maker
Ribbon <- na.omit(TX_forecast_output[, c("Week", "ForecastLower90", "ForecastUpper90", "GroundTruth")])
valid <- !is.na(TX_forecast_output$ForecastLower90) & !is.na(TX_forecast_output$ForecastUpper90)
groups <- cumsum(c(TRUE, diff(valid) != 0))
unique_groups <- unique(groups[valid])

#TS
pdf(file = "TexasTS.pdf", width = 13.92, height =  9.89)
par(mar = c(5.1, 5.5, 4.1, 2.1))
plot(TX_forecast_output$Week, TX_forecast_output$GroundTruth, type ="l", col = 'black', 
     xlab=substitute(paste(bold('Date'))), ylab=substitute(paste(bold('Weekly COVID-19 Deaths'))), xaxt = "n", yaxt = "n", 
     main = "Texas", ylim  =c(0,2500), frame.plot = FALSE, cex.main = 2.5, cex.lab=2.5, lwd =5)
points(TX_forecast_output$Week, TX_forecast_output$GroundTruth, pch = 19, lwd =5)
axis(2, cex = 2,  cex.axis=1.8,cex.lab=1.75, lwd = 5, font = 2);
axis(1, at= c(min(axes_labs1), axes_labs1), cex=2, cex.axis=1.8,cex.lab=2.5, labels=FALSE, lwd = 5, font = 2);
axis(1, at=axes_labs1+15.5, labels=as.character(names), cex=2, cex.axis=1.8,cex.lab=2.5, lwd.tick=0, lwd =5, font = 2);
for (g in unique_groups) {
  idx <- which(groups == g & valid)
  polygon(
    x = c(TX_forecast_output$Week[idx], rev(TX_forecast_output$Week[idx])),
    y = c(TX_forecast_output$ForecastLower90[idx], rev(TX_forecast_output$ForecastUpper90[idx])),
    col = rgb(0, 0, 1, 0.2),
    border = NA
  )
}
lines(TX_forecast_output$Week, TX_forecast_output$ForecastMedian, col = "blue", lty = 2, lwd = 5)
points(TX_forecast_output$Week, TX_forecast_output$ForecastMedian, col = "blue", pch = 19, lwd = 5)
dev.off()






#NY Timeseries
#New Dates
Week = data.frame(Week = as.Date('2021-01-01'), GroundTruth = NA, ForecastMedian = NA, ForecastLower90 = NA, ForecastUpper90 = NA)
NY_forecast_output = rbind(Week, NY_forecast_output)
Week = data.frame(Week = as.Date('2021-11-01'), GroundTruth = NA, ForecastMedian = NA, ForecastLower90 = NA, ForecastUpper90 = NA)
NY_forecast_output = rbind(NY_forecast_output, Week)


Ribbon <- na.omit(NY_forecast_output[, c("Week", "ForecastLower90", "ForecastUpper90", "GroundTruth")])
valid <- !is.na(NY_forecast_output$ForecastLower90) & !is.na(NY_forecast_output$ForecastUpper90)
groups <- cumsum(c(TRUE, diff(valid) != 0))d
unique_groups <- unique(groups[valid])

#TS
pdf(file = "NYTS.pdf", width = 13.92, height =  9.89)
par(mar = c(5.1, 5.6, 4.1, 2.1))
plot(NY_forecast_output$Week, NY_forecast_output$GroundTruth, type ="l", col = 'black', 
     xlab=substitute(paste(bold('Date'))), ylab=substitute(paste(bold('Weekly COVID-19 Deaths'))), xaxt = "n", yaxt = "n", 
     main = "New York", ylim  =c(0,1500), frame.plot = FALSE, cex.main = 2.5, cex.lab=2.5, lwd =5)
points(NY_forecast_output$Week, NY_forecast_output$GroundTruth, pch = 19, lwd =5)
axis(2, cex = 2,  cex.axis=1.8,cex.lab=1.75, lwd = 5, font = 2);
axis(1, at= c(min(axes_labs1), axes_labs1), cex=2, cex.axis=1.8,cex.lab=2.5, labels=FALSE, lwd = 5, font = 2);
axis(1, at=axes_labs1+15.5, labels=as.character(names), cex=2, cex.axis=1.8,cex.lab=2.5, lwd.tick=0, lwd =5, font = 2);
for (g in unique_groups) {
  idx <- which(groups == g & valid)
  polygon(
    x = c(NY_forecast_output$Week[idx], rev(NY_forecast_output$Week[idx])),
    y = c(NY_forecast_output$ForecastLower90[idx], rev(NY_forecast_output$ForecastUpper90[idx])),
    col = rgb(0, 0, 1, 0.2),
    border = NA
  )
}
lines(NY_forecast_output$Week, NY_forecast_output$ForecastMedian, col = "blue", lty = 2, lwd = 5)
points(NY_forecast_output$Week, NY_forecast_output$ForecastMedian, col = "blue", pch = 19, lwd = 5)
dev.off





#CA Timeseries
#New Dates
Week = data.frame(Week = as.Date('2021-01-01'), GroundTruth = NA, ForecastMedian = NA, ForecastLower90 = NA, ForecastUpper90 = NA)
CA_forecast_output = rbind(Week, CA_forecast_output)
Week = data.frame(Week = as.Date('2021-11-01'), GroundTruth = NA, ForecastMedian = NA, ForecastLower90 = NA, ForecastUpper90 = NA)
CA_forecast_output = rbind(CA_forecast_output, Week)



Ribbon <- na.omit(CA_forecast_output[, c("Week", "ForecastLower90", "ForecastUpper90", "GroundTruth")])
valid <- !is.na(CA_forecast_output$ForecastLower90) & !is.na(CA_forecast_output$ForecastUpper90)
groups <- cumsum(c(TRUE, diff(valid) != 0))
unique_groups <- unique(groups[valid])

#TS
pdf(file = "CATS.pdf", width = 13.92, height =  9.89)
par(mar = c(5.1, 5.6, 4.1, 2.1))
plot(CA_forecast_output$Week, CA_forecast_output$GroundTruth, type ="l", col = 'black', 
     xlab=substitute(paste(bold(''))), ylab=substitute(paste(bold('Weekly COVID-19 Deaths'))), xaxt = "n", yaxt = "n", 
     main = "California", ylim  =c(0,4105), frame.plot = FALSE, cex.main = 2.5, cex.lab=2.5, lwd =5)
points(CA_forecast_output$Week, CA_forecast_output$GroundTruth, pch = 19, lwd =5)
axis(2, cex = 2,  cex.axis=1.8,cex.lab=1.75, lwd = 5, font = 2);
axis(1, at= c(min(axes_labs1), axes_labs1), cex=2, cex.axis=1.8,cex.lab=2.5, labels=FALSE, lwd = 5, font = 2);
axis(1, at=axes_labs1+15.5, labels=as.character(names), cex=2, cex.axis=1.8,cex.lab=2.5, lwd.tick=0, lwd =5, font = 2);
for (g in unique_groups) {
  idx <- which(groups == g & valid)
  polygon(
    x = c(CA_forecast_output$Week[idx], rev(CA_forecast_output$Week[idx])),
    y = c(CA_forecast_output$ForecastLower90[idx], rev(CA_forecast_output$ForecastUpper90[idx])),
    col = rgb(0, 0, 1, 0.2),
    border = NA
  )
}
lines(CA_forecast_output$Week, CA_forecast_output$ForecastMedian, col = "blue", lty = 2, lwd = 5)
points(CA_forecast_output$Week, CA_forecast_output$ForecastMedian, col = "blue", pch = 19, lwd = 5)
dev.off()


###############################################
#           Forecasting Comparison           #
###############################################
FS_Model <- data.frame(
  Model = c("Chronos-Base", "CDC Hub Median", "CDC Hub Best", "SGNN"),
  `Forecasting Skill` = c(2.9, 13.0, 34, 35.3)
)


FS_Model |>
  ggplot(aes(x = factor(Model,c("Chronos-Base", "CDC Hub Median", "CDC Hub Best", "SGNN")), y= (Forecasting.Skill)))+
  geom_bar(stat = "identity", position = 'dodge',  colour="black", fill = c("#ffb000", "#dc267f", "#785ef0", "#648fff")) +
  scale_y_continuous(limits = c(0,40), expand = c(0, 0)) +
  theme_classic() +
  xlab("Model") +
  ylab("Forecasting Skill") +
  geom_text(aes(label = sprintf("%.1f", Forecasting.Skill)), vjust = -1, size = 12, fontface = "bold") +
  ggtitle("Forecasting Skill by Model") +
  theme(plot.title = element_text(size = 30, face = "bold", hjust = 0.5),
        axis.text.y =element_text(size=28, face="bold", colour = "black"),
        axis.text.x =element_text(size=0, face="bold", colour = "black"),
        axis.title=element_text(size=30,face="bold"),
        axis.line.y  = element_line(colour = 'black', size = 1),
        axis.line.x  = element_line(colour = 'black', size = 1),
        axis.ticks.x = element_line(size = 1),
        axis.ticks.length.x  = unit(7.5, "pt")
  )





FS_Model <- data.frame(
  Model = c("Mechanistic Pre-training", "Non-mechanistic Pre-training"),
  `Forecasting Skill` = c(0.647, 1.58)
)


FS_Model |>
  ggplot(aes(x = factor(Model,c("Mechanistic Pre-training", "Non-mechanistic Pre-training")), y= (Forecasting.Skill)))+
  geom_bar(stat = "identity", position = 'dodge',  colour="black", fill = c("#ffb000",  "#648fff")) +
  scale_y_continuous(limits = c(0,2), expand = c(0, 0)) +
  theme_classic() +
  xlab("") +
  ylab("Relative MAE") +
  geom_text(aes(label = sprintf("%.2f", Forecasting.Skill)), vjust = -1, size = 12, fontface = "bold") +
  theme(plot.title = element_text(size = 30, face = "bold", hjust = 0.5),
        axis.text.y =element_text(size=28, face="bold", colour = "black"),
        axis.text.x =element_text(size=0, face="bold", colour = "black"),
        axis.title=element_text(size=30,face="bold"),
        axis.line.y  = element_line(colour = 'black', size = 1),
        axis.line.x  = element_line(colour = 'black', size = 1),
        axis.ticks.x = element_line(size = 1),
        axis.ticks.length.x  = unit(7.5, "pt")
  )


###############################################
#           Dengue                            #
###############################################
forecast_comparison_full <- read_csv("Data/forecast_comparison_full.csv")
View(forecast_comparison_full)

forecast_comparison_full <- forecast_comparison_full[c(48:68),]
Ribbon <- na.omit(forecast_comparison_full[, c("week_index", "true_value", "sgnn_upper", "sgnn_lower")])

valid <- !is.na(forecast_comparison_full$sgnn_lower) & !is.na(forecast_comparison_full$sgnn_upper)


groups <- cumsum(c(TRUE, diff(valid) != 0))
unique_groups <- unique(groups[valid])

#TS
axes_labs1 <- seq(407, 428, by =1)
names <- seq(407, 428, by =1);


par(mar = c(5.1, 5.5, 4.1, 2.1))
plot(forecast_comparison_full$week_index, forecast_comparison_full$true_value, type ="l", col = 'black', 
     xlab=substitute(paste(bold('Date'))), ylab=substitute(paste(bold('Weekly Dengue Cases'))), xaxt = "n", yaxt = "n", 
     main = "Dengue", ylim  =c(0,1200), frame.plot = FALSE, cex.main = 2.5, cex.lab=2.5, lwd =5)
points(forecast_comparison_full$week_index, forecast_comparison_full$true_value, pch = 19, lwd =5)
axis(2, cex = 2,  cex.axis=1.8,cex.lab=1.75, lwd = 5, font = 2);
axis(1, at= axes_labs1, cex=2, cex.axis=1.8,cex.lab=2.5, labels=FALSE, lwd = 5, font = 2, lwd.tick=2);
axis(1, at=axes_labs1+0.5, labels=as.character(names), cex=1.5, cex.axis=1.5,cex.lab=1.5, lwd.tick=0, lwd =4, font = 2);
for (g in unique_groups) {
  idx <- which(groups == g & valid)
  polygon(
    x = c(forecast_comparison_full$week_index[idx], rev(forecast_comparison_full$week_index[idx])),
    y = c(forecast_comparison_full$sgnn_lower[idx], rev(forecast_comparison_full$sgnn_upper[idx])),
    col = rgb(0, 0, 1, 0.2),
    border = NA
  )
}
lines(forecast_comparison_full$week_index, forecast_comparison_full$sgnn_median, col =  "#648fff", lty = 2, lwd =5)
points(forecast_comparison_full$week_index, forecast_comparison_full$sgnn_median, col =  "#648fff",  pch = 19, lwd =5)
lines(forecast_comparison_full$week_index, forecast_comparison_full$ets, col = "#dc267f", lty = 2, lwd =5)
points(forecast_comparison_full$week_index, forecast_comparison_full$ets, col = "#dc267f",  pch = 19, lwd =5)
lines(forecast_comparison_full$week_index, forecast_comparison_full$einn, col = "#ffb000", lty = 2, lwd =5)
points(forecast_comparison_full$week_index, forecast_comparison_full$einn, col = "#ffb000", pch = 19, lwd =5)
legend("center", lty = c(2,2,2), lwd = 5, text.font = 2, 
       col= c("#648fff","#dc267f","#ffb000"),
       legend=c("SGNN", "ETS", "PINN"))





#Forecasting SKill
FS_Model <- data.frame(
  Model = c("PINN", "ETS", "SGNN"),
  `Forecasting Skill` = c(1.74, .99, 0.75)
)


FS_Model |>
ggplot(aes(x = factor(Model,  c("PINN", "ETS","SGNN")), y= (Forecasting.Skill)))+
  geom_bar(stat = "identity", position = 'dodge',  colour="black", fill = c("#ffb000", "#dc267f", "#648fff")) +
  ylim(0,2) +
  theme_classic() +
  xlab("Model") +
  ylab("Forecasting Skill") +
  scale_y_continuous(limits = c(0,2), expand = c(0, 0)) +
  geom_text(aes(label = Forecasting.Skill), vjust = -1, size = 12, fontface = "bold") +
  ggtitle("Forecasting Skill by Model") +
  theme(plot.title = element_text(size = 30, face = "bold", hjust = 0.5),
        axis.text.y =element_text(size=28, face="bold", colour = "black"),
        axis.text.x =element_text(size=28, face="bold", colour = "black"),
        axis.title=element_text(size=30,face="bold"),
        axis.line.y  = element_line(colour = 'black', size = 1),
        axis.line.x  = element_line(colour = 'black', size = 1),
        axis.ticks.x = element_line(size = 1),
        axis.ticks.length.x  = unit(7.5, "pt")
  )

###############################################
#           Ablation Results                  #
###############################################
ablation.forecast = data.frame(generations = c(1, 2, 3, 4, 5),
mae_values = c(1.0, 1.025, 1.54, 2.56, 5.17))


#par(mar = c(5.1, 5.1, 4.1, 2.1))
#plot(ablation.forecast$generations,  ablation.forecast$mae_values, type ="l", col = 'red', 
#     xlab=substitute(paste(bold('Generation'))), ylab=substitute(paste(bold('MAE vs Baseline'))), xaxt = "n", yaxt = "n", 
#     main = "Performance", ylim  =c(0,6), frame.plot = FALSE, cex.main = 2.5, cex.lab=2.5, lwd =5,)
#points(ablation.forecast$generations,  ablation.forecast$mae_values, pch = 19, lwd =5, col = "red")
#axis(2, cex = 2,  cex.axis=1.8,cex.lab=2.5, lwd = 5, font = 2);
#axis(1, cex = 2,  cex.axis=1.8,cex.lab=2.5, lwd = 5, font = 2);


ablation_forecast$week <- seq(1,8, by =1)


#TS Plot
all_dates <- seq(as.Date("1960/01/01"), by = 'Year', length.out = 2)
axes_labs1 <- seq(1,9, by =1)
names <- c(1,2,3,4,5,6,7,8,9);

plot(ablation_forecast$week, ablation_forecast$actual, type ="l", col = 'black', 
     xlab=substitute(paste(bold('Week'))), ylab=substitute(paste(bold('Cases'))), xaxt = "n", yaxt = "n", 
     main = "Ablation Forecast", ylim  =c(0,2000), frame.plot = FALSE, cex.lab=2.5, lwd =5, cex.main = 2)
points(ablation_forecast$week, ablation_forecast$actual, pch = 19, lwd =4)
axis(2, cex = 2,  cex.axis=1.8,cex.lab=2.5, lwd = 5, font = 2);
axis(1, at= c(min(axes_labs1), axes_labs1), cex=2, cex.axis=1.8,cex.lab=2.5, labels=FALSE, lwd = 5, font = 2);
axis(1, at=axes_labs1+0.5, labels=as.character(names), cex=2, cex.axis=1.8,cex.lab=2.5, lwd.tick=0, lwd =5, font = 2);
lines(ablation_forecast$week, ablation_forecast$forecast, col = "blue", lty = 2, lwd = 5)
points(ablation_forecast$week, ablation_forecast$forecast, col = "blue", pch = 19, lwd = 5)



###############################################
#           Heatmap                  #
###############################################
library("viridis")
relative_mae_comparison <- read_csv("Data/relative_mae_comparison.csv", 
                                    col_types = cols(chronos = col_number(), 
                                                     sgnn = col_number(), pinn = col_number()))
relative_mae_comparison$state <- c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", "GA", "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", 
                                   "MD", "ME", "MI", "MN", "MO", "MS", "MT", "NC", "ND", "NE", "NH", "NJ", "NM", "NV", "NY", "OH", "OK", "OR", "PA", "PR", 
                                   "RI", "SC", "SD", "TN", "TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY")

#colnames(relative_mae_comparison) <- c("state", "PINN", "PINN", "PINN")
colnames(relative_mae_comparison) <- c("State", "Chronos", "SGNN", "PINN")

relative_mae_comparison <- column_to_rownames(relative_mae_comparison, var = "State")

relative_mae_comparison <- as.matrix(t(relative_mae_comparison))

relative_mae_comparison[relative_mae_comparison > 1.5] <- 1.5


relative_mae_comparison <- rbind(relative_mae_comparison, relative_mae_comparison[3,])



library(ComplexHeatmap)
ComplexHeatmap::Heatmap(as.matrix(relative_mae_comparison),  
                        row_order = rownames(relative_mae_comparison[c(2,1,3),]), 
                        column_order = colnames(relative_mae_comparison),
                        name = "Z-score",  
                        col = rev(viridis(100)),
                        heatmap_legend_param = list(
                          title = "Relative\nMAE",
                          title_gp = gpar(fontsize = 10, fontface = "bold"),
                          labels_gp = gpar(fontsize = 8),
                          legend_height = unit(4, "cm")
                        )
)

hm1 +hm2

grid.arrange(grobs = list(hm2[[4]], hm1[[4]]))



###############################################
#           Figure 3                          #
###############################################

###############################################
#           Predator Prey                     #
###############################################

Species_Model <- data.frame(
  Model = c("SGNN", "SGNN", "Mechanistic", "Mechanistic", "Statistical", "Statistical"),
  Species = c("Hare", "Lynx", "Hare", "Lynx", "Hare", "Lynx"),
  `Forecasting Skill` = c(0.401, 0.48, 0.313, 0.212, 0.306, 0.237)
)


butterfly_species_results <-butterfly_species_results |>
  pivot_longer(c(SGNN,`Task-specific NN`), names_to = "Model", values_to = "Forecasting.Skill")


ribbon_data <- butterfly_species_results |>
  pivot_wider(names_from = Model, values_from = Forecasting.Skill) |>
  mutate(
    ymin = pmin(SGNN,`Task-specific NN`),  
    ymax = pmax(SGNN,`Task-specific NN`)
  )


ggplot(butterfly_species_results) +
  geom_ribbon(data = ribbon_data, aes(x = Species, ymin = ymin, ymax = ymax),
              fill = "lightblue", alpha = 0.4) +
  geom_line(aes(x = Species, y = Forecasting.Skill, colour = Model), size = 2) + 
  geom_point(aes(x = Species, y = Forecasting.Skill, colour = Model), size = 3) + 
  ylim(-0.1, 0.3) +
  theme_classic() +
  ylab("Forecasting Skill") +
  xlab("Number of Species") +
  scale_color_manual("Model", values = c("#648fff", "#ffb000")) +
  guides( x = guide_axis(cap = TRUE),  y = guide_axis(cap = TRUE)) +
  ggtitle("Forecasting Skill by Species Across Models") +
  theme(plot.title = element_text(size = 30, face = "bold", hjust = 0.5),
        axis.text.y =element_text(size=28, face="bold", colour = "black"),
        axis.text.x =element_text(size=28, face="bold", colour = "black"),
        axis.title=element_text(size=30,face="bold"),
        axis.line.y  = element_line(colour = 'black', size = 1),
        axis.line.x  = element_line(colour = 'black', size = 1),
        axis.ticks.x = element_line(size = 1),
        axis.ticks.length.x  = unit(7.5, "pt")
  )

  
  unique(butterfly_species_results$Model)


Species_Model |>
  mutate(Model = factor(Species_Model$Model, levels = c("Mechanistic", "Statistical", "SGNN"))) |>
  ggplot(aes(x = factor(Species), y= Forecasting.Skill, fill= Model))+
  geom_bar(stat = "identity", position = 'dodge',  colour="black") + theme_classic() +
  scale_y_continuous(limits = c(0,0.75), expand = c(0, 0)) +
  theme_classic() +
  xlab("Model") +
  ylab("Forecasting Skill") +
  geom_text(aes(label = Forecasting.Skill), position = position_dodge(width = 1 ), vjust = -1, size = 12, fontface = "bold") +
  ggtitle("Forecasting Skill by Model") +
  theme(plot.title = element_text(size = 30, face = "bold", hjust = 0.5),
        axis.text.y =element_text(size=28, face="bold", colour = "black"),
        axis.text.x =element_text(size=28, face="bold", colour = "black"),
        axis.title=element_text(size=30,face="bold"),
        axis.line.y  = element_line(colour = 'black', size = 1),
        axis.line.x  = element_line(colour = 'black', size = 1),
        axis.ticks.x = element_line(size = 1),
        axis.ticks.length.x  = unit(7.5, "pt")
  ) +
  scale_fill_manual(
    values = c("SGNN" = "#648fff", 
               "Mechanistic" = "#dc267f",
               "Statistical" = "#ffb000") 
  )






###############################################
#          Chemistry                          #
###############################################
yield_predictions <- read_csv("Data/yield_predictions.csv")

baseline_chem_results <- read_csv("Data/baseline_chem_results.csv")
colnames(baseline_chem_results) <- c("Actual Yield", "Predicted Yield")


yield_predictions <- yield_predictions |>
  mutate(Model = "SGNN")


baseline_chem_results <- baseline_chem_results |>
  mutate(Model = "Baseline")

chem_results <- rbind(yield_predictions, baseline_chem_results)


ggplot() +
  geom_point(data = chem_results, aes(x = `Actual Yield`, y = `Predicted Yield`, fill = Model), 
             shape = 21, size = 5, alpha = 0.6) + 
  geom_smooth(data = chem_results, aes(x = `Actual Yield`, y = `Predicted Yield`),
              method = "lm", color = "black",linetype = "dashed", fill = "grey",se = FALSE, size = 2.5) +
  scale_fill_manual(values = c("SGNN" = "#1f77b4", "Baseline" = "#ff7f0e")) +
  theme_classic() +
  ylab("Predicted Yield") +
  xlab("Actual Yield") +
  guides( x = guide_axis(cap = TRUE),  y = guide_axis(cap = TRUE)) +
  ggtitle("Chemical Yield Prediction") +
  theme(plot.title = element_text(size = 30, face = "bold", hjust = 0.5),
        axis.text.y =element_text(size=28, face="bold", colour = "black"),
        axis.text.x =element_text(size=28, face="bold", colour = "black"),
        axis.title=element_text(size=30,face="bold"),
        axis.line.y  = element_line(colour = 'black', size = 1),
        axis.line.x  = element_line(colour = 'black', size = 1),
        axis.ticks.x = element_line(size = 1),
        axis.ticks.length.x  = unit(7.5, "pt")
  ) 


chemical_yield_residual <- data.frame(Model = c("Task-Specific NN", "SGNN"),
           `Residual Variance` = c(0.15,0.10))

chemical_yield_residual |>
ggplot(aes(x = factor(Model,  c("Task-Specific NN", "SGNN")), y= Residual.Variance))+
  geom_bar(stat = "identity", position = 'dodge',  colour="black", fill = c("#ffb000", "#648fff")) + 
  theme_classic() +
  scale_y_continuous(limits = c(0,0.2), expand = c(0, 0)) +
  theme_classic() +
  xlab("Model") +
  ylab("Residual Variance") +
  geom_text(aes(label = Residual.Variance), position = position_dodge(width = 1 ), vjust = -1, size = 12, fontface = "bold") +
  ggtitle("Residual Variance by Model") +
  theme(plot.title = element_text(size = 30, face = "bold", hjust = 0.5),
        axis.text.y =element_text(size=28, face="bold", colour = "black"),
        axis.text.x =element_text(size=28, face="bold", colour = "black"),
        axis.title=element_text(size=30,face="bold"),
        axis.line.y  = element_line(colour = 'black', size = 1),
        axis.line.x  = element_line(colour = 'black', size = 1),
        axis.ticks.x = element_line(size = 1),
        axis.ticks.length.x  = unit(7.5, "pt")
  ) 





###############################################
#         Top K   Accuracy                    #
###############################################
topk_data <- data.frame(
  k = c(1, 3, 5, 10, 20, 50, 100),
  SGNN = c(0.82552, 0.8333333, 0.83802, 0.84531, 0.8489, 0.85625,0.865625),
  rumor_center = c(0.588, 0.651, 0.666, 0.689,0.721, 0.756, 0.772)
)

topk_long <- pivot_longer(topk_data, cols = c(SGNN, rumor_center),
                          names_to = "Method", values_to = "Accuracy")

topk_long |> ggplot(aes(x = k, y = Accuracy, color = Method)) +
  geom_line(size = 2) +
  geom_point(size = 3) +
  scale_x_continuous(breaks = c(1, 5, 10, 20, 50,100)) +
  scale_color_manual(values = c("SGNN" = "#1f77b4", "Rumor Center" = "#ff7f0e")) +
  labs(title = "Top-k Accuracy Comparison",
       x = "Top-k",
       y = "Accuracy",
       color = "Method") +
  theme_classic() +
  ylim(0.5, 0.9) +
  guides( x = guide_axis(cap = TRUE),  y = guide_axis(cap = TRUE)) +
  theme(plot.title = element_text(size = 30, face = "bold", hjust = 0.5),
        axis.text.y =element_text(size=28, face="bold", colour = "black"),
        axis.text.x =element_text(size=28, face="bold", colour = "black"),
        axis.title=element_text(size=30,face="bold"),
        axis.line.y  = element_line(colour = 'black', size = 1),
        axis.line.x  = element_line(colour = 'black', size = 1),
        axis.ticks.x = element_line(size = 1),
        axis.ticks.length.x  = unit(7.5, "pt")
  ) 







###############################################
#       Network.                              #
###############################################
library(igraph)
library(ggraph)
library(tidygraph)
library(igraph)


edges <- read_csv("Data/graph_edges.csv")
probs <- read_csv("Data/node_probs.csv")
infected <- read_csv("Data/infection_times.csv")
source_info <- readLines("Data/source_info.txt")
true_source <- as.integer(strsplit(source_info[1], ",")[[1]][2])
pred_source <- as.integer(strsplit(source_info[2], ",")[[1]][2])

#create subgraphs
G <- graph_from_data_frame(edges, directed = FALSE)
infected_nodes <- infected %>% filter(infection_time >= 0) %>% pull(node_id)
subgraph <- induced_subgraph(G, vids = which(V(G)$name %in% infected_nodes))

#add probabilities to subgraph node ids
match_idx <- match(V(subgraph)$name, node_df$name)
V(subgraph)$prob <- node_df$prob[match_idx]
node_df <- probs %>% mutate(name = as.character(node_id))

valid_nodes <- intersect(V(subgraph)$name, node_df$name)
node_df_filtered <- node_df %>% filter(name %in% valid_nodes)


subgraph <- set_vertex_attr(
  subgraph,
  name = "prob",
  index = node_df_filtered$name,
  value = node_df_filtered$prob
)

V(subgraph)$name
V(subgraph)$prob

#Highlight predicted vs source
V(subgraph)$true <- V(subgraph)$name == as.character(true_source)
V(subgraph)$pred <- V(subgraph)$name == as.character(pred_source)

ggraph(subgraph, layout = "kk") +
  geom_edge_link(alpha = 4, color = "grey50") +
  geom_node_point(aes(fill = prob), shape = 21,
                  color = case_when(
                    V(subgraph)$true ~ "green",
                    V(subgraph)$pred ~ "black",
                    TRUE ~ "grey30"
                  ),
                  stroke = 2,
                  size = 17) +
  scale_fill_viridis_c(option = "C", name = "Predicted\nSource\nProbability") +
  scale_size_continuous(range = c(4, 20)) +
  theme_void() +
  ggtitle("SGNN Predicted Source Probabilities (Single Cascade)") +
  theme(legend.position = "right",plot.margin = margin(10, 10, 10, 10), clip = "off")



###############################################
#           Figure 4                          #
###############################################
library(readr)
daily_outbreaks <- read_csv("Data/daily_outbreaks.csv")
View(daily_outbreaks)


daily_outbreaks = daily_outbreaks[,c(1,41)]


daily_outbreaks <- daily_outbreaks[c(21:42),]
#Week
start_date <- as.Date('2020-03-11')
end_date <- as.Date('2020-04-08')
all_dates <- seq(start_date, end_date, by = 'week')
axes_labs1 <- all_dates
names <- c("Week 1", "Week 2", "Week 3", "Week 4", "Week 5");

all_dates <- as.numeric(all_dates)

colnames(daily_outbreaks) <- c("Week", "Cases")

#TS
plot(daily_outbreaks$Week, daily_outbreaks$Cases, type ="l", col = 'black', 
     xlab=substitute(paste(bold(''))), ylab=substitute(paste(bold('Weekly COVID-19 Cases'))), xaxt = "n", yaxt = "n", 
     main = "New York City", ylim = c(0,40000),frame.plot = FALSE, cex.main = 2.5, cex.lab=2.5, lwd =5)
axis(2, cex = 2,  cex.axis=1.8,cex.lab=1.75, lwd = 5, font = 2);
axis(1, at= c(min(axes_labs1), axes_labs1), cex=2, cex.axis=1.8,cex.lab=2.5, labels=FALSE, lwd = 5, font = 2);
axis(1, at=axes_labs1+3.5, labels=as.character(names), cex=2, cex.axis=1.8,cex.lab=2.5, lwd.tick=0, lwd =5, font = 2);




###############################################
#           Estimation                        #
###############################################
EstimationPerformance <- data.frame(
  Model = c("SGNN", "MLE", "Growth Rate"),
  MSE = c(1.83, 10.40, 8.14),
  MPE = c(18.58, 44.50, 35.70)
)


EstimationPerformance |>
  ggplot(aes(x = factor(Model,  c("MLE", "Growth Rate", "SGNN")), y= MSE))+
  geom_bar(stat = "identity", position = 'dodge',  colour="black", fill = c("#648fff", "#dc267f","#ffb000")) + 
  theme_classic() +
  scale_y_continuous(limits = c(0,12), expand = c(0, 0)) +
  theme_classic() +
  xlab("") +
  ylab("Residual Variance") +
  geom_text(aes(label = MSE), position = position_dodge(width = 1 ), vjust = -1, size = 12, fontface = "bold") +
  ggtitle("Mean Squared Error by Method") +
  theme(plot.title = element_text(size = 30, face = "bold", hjust = 0.5),
        axis.text.y =element_text(size=28, face="bold", colour = "black"),
        axis.text.x =element_text(size=28, face="bold", colour = "black"),
        axis.title=element_text(size=30,face="bold"),
        axis.line.y  = element_line(colour = 'black', size = 1),
        axis.line.x  = element_line(colour = 'black', size = 1),
        axis.ticks.x = element_line(size = 1),
        axis.ticks.length.x  = unit(7.5, "pt")
  ) 




EstimationPerformance |>
  ggplot(aes(x = factor(Model,  c("MLE", "Growth Rate", "SGNN")), y= MPE))+
  geom_bar(stat = "identity", position = 'dodge',  colour="black", fill = c("#648fff", "#dc267f","#ffb000")) + 
  theme_classic() +
  scale_y_continuous(limits = c(0,50), expand = c(0, 0)) +
  theme_classic() +
  xlab("") +
  ylab("Residual Variance") +
  geom_text(aes(label = MPE), position = position_dodge(width = 1 ), vjust = -1, size = 12, fontface = "bold") +
  ggtitle("Mean Percentage Error by Method") +
  theme(plot.title = element_text(size = 30, face = "bold", hjust = 0.5),
        axis.text.y =element_text(size=28, face="bold", colour = "black"),
        axis.text.x =element_text(size=28, face="bold", colour = "black"),
        axis.title=element_text(size=30,face="bold"),
        axis.line.y  = element_line(colour = 'black', size = 1),
        axis.line.x  = element_line(colour = 'black', size = 1),
        axis.ticks.x = element_line(size = 1),
        axis.ticks.length.x  = unit(7.5, "pt")
  ) 








###############################################
#           Back2Sim                          #
###############################################
library(readr)
back2sim_params <- read_csv("Data/back2sim_params.csv")
View(back2sim_params)

colnames(back2sim_params) <- c("Population Size", "Hospitalization Fatality Rate", "Hospitalization Rate", "Asymptomatic Transmission Rate")
back2sim_params <- back2sim_params |>
  pivot_longer(cols = c("Population Size", "Hospitalization Fatality Rate", "Hospitalization Rate", "Asymptomatic Transmission Rate"), names_to = "Metric", values_to = "Rate")



metric_colors <- c("Metric1" = "gray30", "Metric2" = "gray50", "Metric3" = "gray70")  


nature_theme <- theme_classic(base_size = 30) +
  theme(
    plot.title = element_text(size = 30, face = "bold", hjust = 0.5),
    axis.text.y = element_text(size = 28, face = "bold", colour = "black"),
    axis.text.x = element_text(size = 28, face = "bold", colour = "black"),
    axis.title = element_text(size = 30, face = "bold"),
    axis.line.y = element_line(colour = 'black', size = 2.5),
    axis.line.x = element_line(colour = 'black', size = 2.5),
    axis.ticks.x = element_line(size = 2.5),
    axis.ticks.y = element_line(size = 2.5),
    axis.ticks.length.x = unit(7.5, "pt"),
    axis.ticks.length.y = unit(7.5, "pt"),
    legend.position = "none"
  )


make_violin <- function(metric_name) {
  ggplot(filter(back2sim_params, Metric == metric_name), aes(x = metric_name, y = Rate)) +
    geom_violin(fill = "#dbb0d2", color = NA, width = 0.9) +  
    geom_boxplot(width = 0.2, fill = "white", color = "black", size = 1.2, outlier.shape = NA) +  
    geom_jitter(width = 0.1, size = 3.5, alpha = 0.6, color = "black") + 
    nature_theme +
    xlab("") + 
    ylab("")
  
}

make_violin("Population Size") + geom_hline(yintercept =  10.14e6)
make_violin("Hospitalization Fatality Rate") + geom_hline(yintercept =  0.209)
make_violin("Hospitalization Rate") + geom_hline(yintercept = 0.069)
 make_violin("Asymptomatic Transmission Rate") + geom_hline(yintercept = 0.179)







