#load function to load data from multiple .csv
source("tools/load_flow_function.R")

hours <- c(4, 20, 24, 28, 32, 44, 48, 52, 56,
           60, 68, 72, 76, 80, 92, 104, 110, 116)
#load and merge data with the 'load_flow' function
flow <- load_flow(protist = "KL", bacteria = "BAll", rep = 1:6, time = hours)

#set missing value
#flow[which(flow$code == "KLxBAll_rep2_time4"), 8:39] <- NA 
flow[which(flow$code == "KLxBAll_rep6_time52"), 8:39] <- NA 

#particles per mL in time
library(ggthemes)
ggplot(data = flow, aes(x = time, y = particles_ml)) +
  geom_point(aes(color = replicate), size = 2) +
  geom_line(aes(color = replicate), size = 1.05) +
  scale_colour_colorblind() +
  scale_x_continuous(breaks = hours, expand = c(0.03, 0.03)) +
  coord_cartesian(ylim = c(0, max(flow$particles_ml))) +
  labs(x = "Culture time (hours)", y = "Particles per ml") +
  theme(panel.background = element_rect(fill = "grey95"),
        panel.grid.minor = element_blank(),
        legend.position = c(0.325, 0.875),
        legend.background = element_rect(fill = "grey95",
                                         linetype = 1,
                                         size = 0.5,
                                         colour = "black"),
        legend.direction="horizontal")
ggsave("dataviz/KLxBall_1-6_particles_ml.png",
       plot = last_plot(),
       device = "png", units = "in",
       width = 12, height = 8, dpi = 320)

##particles (count) in time
#ggplot(data = flow, aes(x = time, y = particle_count)) +
#  geom_point(aes(color = replicate), size = 2) +
#  geom_line(aes(color = replicate), size = 1.05) +
#  scale_colour_colorblind() +
#  scale_x_continuous(breaks = hours) +
#  coord_cartesian(ylim = c(0, max(flow$particles_ml))) +
#  labs(x = "Culture time (hours)", y = "Particles (count)") +
#  theme(panel.grid.minor = element_blank())

#boxplot by time (proud of this dirty code!)
tbox <- rep(seq(2, 118, 2), each = 6)
boxplot_flow <- data.frame("time" = tbox,
                           "particles" = NA)
ref <- unique(flow$time)
n <- 1
for (i in 1:length(tbox)){
  if (n == 6){
    n <- 1
  }
  if (tbox[i] %in% ref){
    boxplot_flow$particles[i] <- flow$particles_ml[which(flow$time == tbox[i])][n]
    n <- n + 1
  }
}
ggplot(data = boxplot_flow, aes(x = as.factor(time), y = particles)) +
  geom_boxplot() +
  scale_x_discrete(breaks = hours) +
  coord_cartesian(ylim = c(0, max(flow$particles_ml))) +
  labs(x = "Culture time (hours)", y = "Particles per ml") +
  theme(panel.background = element_rect(fill = "grey95"),
        panel.grid.minor = element_blank())
ggsave("dataviz/KLxBall_1-6_particles_ml_boxplot.png",
       plot = last_plot(),
       device = "png", units = "in",
       width = 12, height = 8, dpi = 320)






