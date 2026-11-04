#### Make basic plot for Exp 1 RW culturing
####

library(ggplot2)
library(readxl)
library(patchwork)
library(FSA)      
library(rstatix)   

setwd("C:/Users/erica.nielsen/Desktop/exp_1_growth_trials_results")

Exp_1_results <- read_excel("Exp. 1 results.xlsx")

# histograms to check normality
hist(Exp_1_results$per_cov)
hist(Exp_1_results$dry_weight_g) # not normal
hist(Exp_1_results$PAM_avg)

# tests for signif


# Kruskal–Wallis test
kw_weight <- kruskal.test(dry_weight_g ~ factor(apices), data = Exp_1_results)
kw_weight

# Dunn post-hoc with Holm correction
dunn_weight <- dunnTest(dry_weight_g ~ factor(apices), data = Exp_1_results, method = "holm")
dunn_weight

# ANOVA
anova_cov <- aov(per_cov ~ factor(apices), data = Exp_1_results)
summary(anova_cov)

# Tukey post-hoc
TukeyHSD(anova_cov)

# do for PAM
anova_pam <- aov(PAM_avg ~ factor(apices), data = Exp_1_results)
summary(anova_pam)

TukeyHSD(anova_pam)


### nothing signif, not suprising w/low sample sizes

# boxplot of % cov by apices

library(ggplot2)
library(viridis)

# boxplot of % cov by apices
cov_plot <- ggplot(Exp_1_results, aes(x = factor(apices), 
                                      y = per_cov, 
                                      fill = factor(apices))) +
  geom_boxplot() +
  scale_fill_viridis_d(option = "plasma") +
  labs(x = "# of apices", y = "Percent cover") +
  theme_minimal() +
  theme(legend.position = "none")

# boxplot of PAM by apices
pam_plot <- ggplot(Exp_1_results, aes(x = factor(apices), 
                                      y = PAM_avg, 
                                      fill = factor(apices))) +
  geom_boxplot() +
  scale_fill_viridis_d(option = "plasma") +
  labs(x = "# of apices", y = "Average Fv/Fm") +
  theme_minimal() +
  theme(legend.position = "none")

# boxplot of weight by apices
weight_plot <- ggplot(Exp_1_results, aes(x = factor(apices), 
                                         y = dry_weight_g, 
                                         fill = factor(apices))) +
  geom_boxplot() +
  scale_fill_viridis_d(option = "plasma") +
  labs(x = "# of apices", y = "Dry weight") +
  theme_minimal() +
  theme(legend.position = "none")

#combine plots
apices_plots <- cov_plot | pam_plot | weight_plot

ggsave(
  filename = "combined_apices_plots.png",
  plot = apices_plots,
  width = 12,        # inches
  height = 4,        # inches (1 row)
  dpi = 600          # high resolution
)


### dense vs sparse length

# upload new DF
Exp_1_results <- read_excel("C:/Users/erica.nielsen/Desktop/RW/exp_1_results/Exp. 1 results.xlsx", sheet = "density")

# boxplot

length_plot <- ggplot(Exp_1_results, aes(x = factor(density), 
                                         y = length, 
                                         fill = factor(density))) +
  geom_boxplot() +
  labs(x = "Density", y = "Average length (mm)") +
  theme_minimal() +
  theme(legend.position = "none")

length_plot

ggsave(
  filename = "length_plot.jpg",
  plot = length_plot,
  width = 8,        # inches
  height = 5,        # inches (1 row)
  dpi = 600          # high resolution
)


### Run ANOVA
length.anova <- aov(length ~ density, data = Exp_1_results)

summary(length.anova)

