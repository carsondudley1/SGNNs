library(readr)
predator_prey_forecasts_wide <- read_csv("~/Downloads/predator_prey_forecasts_wide.csv")
View(predator_prey_forecasts_wide)


#Pivot data frame to make it longer
df_long <- predator_prey_forecasts_wide  %>%
  pivot_longer(cols = c(True, RMG, VARMA, PatchTST),
               names_to = "Model",
               values_to = "Count")

#Subset data frame into observed and predicted counts
df_true <- df_long %>% filter(Model == "True")
df_models <- df_long %>% filter(Model != "True")


#Plot ts plots
ggplot() +
  geom_line(data = df_true,
            aes(x = timestep, y = Count, group = Model),
            color = "black", linewidth = 1.5) +
  geom_point(data = df_true,
             aes(x = timestep, y = Count),
             color = "black", size = 3) +

  geom_line(data = df_models,
            aes(x = timestep, y = Count, color = Model),
            linewidth = 1.5) +
  facet_grid(species ~ window_id, switch = "y") +
  theme_classic() +
  theme(
    strip.background = element_rect(color = "black", fill = NA, linewidth = 1.2),
    strip.text = element_text(size = 24, face = "bold"),
    plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
    axis.text = element_text(size = 24, face = "bold", colour = "black"),
    axis.title = element_text(size = 24, face = "bold"),
    axis.line = element_line(colour = 'black', linewidth = 1),
    axis.ticks = element_line(linewidth = 1),
    axis.ticks.length = unit(7.5, "pt"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    panel.grid = element_blank(),
    legend.title = element_text(size = 24, face = "bold"),
    legend.text = element_text(size = 24, face = "bold")
  ) +
  labs(title = "True vs Model Predictions Over Time",
       x = "Timestep", y = "Value",
       color = "Model", linetype = "Model")
