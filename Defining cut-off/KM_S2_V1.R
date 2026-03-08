

cvd.fit <- survfit(Surv(time_ms, outcome) ~ Gender,
                   data = BPV.55_3022,
                   conf.type = "log-log")

# Kaplan-Meier survival curve for men and women
ggsurvfit(cvd.fit) +
  scale_y_continuous(limits = c(0,1)) +
  labs( x = "Time (Months)", y = "Survival probability") +
  scale_colour_discrete(labels = c("Men", "Women"))


library(ggsurvfit)
library(ggplot2)

ggsurvfit(cvd.fit) +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(
    x = "Time (Months)",
    y = "Survival Probability",
    color = "Sex"
  ) +
  scale_colour_manual(values = c("blue", "red"), labels = c("Men", "Women")) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90"),
    axis.line = element_line(color = "black"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 12),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold")
  )


# KM fit with interaction
km_fit <- survfit(Surv(time_ms, outcome) ~ Gender + sd_S_cat, data = BPV.55_3022)

# Plot using survminer, faceted by sd_S_cat
ggsurvplot_facet(
  km_fit, 
  data = BPV.55_3022,
  facet.by = "sd_S_cat",
  color = "Gender",
  palette = c("blue", "red"),
  xlab = "Time (Months)",
  ylab = "Survival Probability",
  legend.title = "Sex",
  legend.labs = c("Men", "Women"),
  risk.table = TRUE,         # Add number at risk
  pval = TRUE,               # Add log-rank p-value
  ggtheme = theme_minimal(base_size = 14)
)



library(survival)
library(survminer)
library(gridExtra)

# Split data
data_0 <- subset(BPV.55_3022, sd_S_cat == 0)
data_1 <- subset(BPV.55_3022, sd_S_cat == 1)

# KM fits
km_fit_0 <- survfit(Surv(time_ms, outcome) ~ Gender, data = data_0)
km_fit_1 <- survfit(Surv(time_ms, outcome) ~ Gender, data = data_1)

# Individual plots
p0 <- ggsurvplot(
  km_fit_0, data = data_0,
  palette = c("blue", "red"),
  legend.title = "Sex",
  legend.labs = c("Men", "Women"),
  xlab = "Time (Months)", ylab = "Survival Probability",
  risk.table = TRUE, pval = TRUE,
  ggtheme = theme_minimal(base_size = 14),
  title = "Survival by Gender (Standard Deviation of\nSystolic BPV <19)",
  gp = gpar(fontsize = 16, fontface = "bold")
)

p1 <- ggsurvplot(
  km_fit_1, data = data_1,
  palette = c("blue", "red"),
  legend.title = "Sex",
  legend.labs = c("Men", "Women"),
  xlab = "Time (Months)", ylab = "Survival Probability",
  risk.table = TRUE, pval = TRUE,
  ggtheme = theme_minimal(base_size = 14),
  title = "Survival by Gender (Standard Deviation of\nSystolic BPV ≥19)",
  gp = gpar(fontsize = 16, fontface = "bold")
)

library(ggplot2)

# Add theme with grids to both plots
p0$plot <- p0$plot + theme_minimal(base_size = 14) + theme(panel.grid.major = element_line(), panel.grid.minor = element_line())
p1$plot <- p1$plot + theme_minimal(base_size = 14) + theme(panel.grid.major = element_line(), panel.grid.minor = element_line())

# Combine again
library(gridExtra)
grid.arrange(p0$plot, p1$plot, ncol = 2)


# Combine plots (patchwork)
combined_plot <- grid.arrange(p0$plot, p1$plot, ncol = 2)
combined_plot


library(gridExtra)
library(grid)
library(ggplot2)

# Remove titles from individual plots
p0$plot <- p0$plot + ggtitle(NULL)
p1$plot <- p1$plot + ggtitle(NULL)

# Combine the two plots side by side
combined_plot <- grid.arrange(
  p0$plot, p1$plot, ncol = 2,
  top = textGrob(
    "Kaplan–Meier Survival by Gender and Standard Deviation of Systolic BPV",
    gp = gpar(fontsize = 18, fontface = "bold")
  )
)

# Save high-resolution combined plot
ggsave(
  filename = "KM_survival_combined.tiff",
  plot = combined_plot,
  width = 14,
  height = 7,
  dpi = 600,
  compression = "lzw"
)


library(gridExtra)
library(grid)
library(ggplot2)

# Remove individual titles
p0$plot <- p0$plot + ggtitle(NULL)
p1$plot <- p1$plot + ggtitle(NULL)

# Create label grobs (A and B) - now left-aligned
label_A <- textGrob("A", gp = gpar(fontsize = 18, fontface = "bold"), hjust = 0, x = 0.02)
label_B <- textGrob("B", gp = gpar(fontsize = 18, fontface = "bold"), hjust = 0, x = 0.02)


# Arrange A and B above each plot
labeled_plots <- arrangeGrob(
  arrangeGrob(label_A, p0$plot, ncol = 1, heights = c(0.05, 1)),
  arrangeGrob(label_B, p1$plot, ncol = 1, heights = c(0.05, 1)),
  ncol = 2
)

# Main title
title_grob <- textGrob(
  "Kaplan–Meier Survival by Gender and Systolic BPV Variability",
  gp = gpar(fontsize = 18, fontface = "bold")
)

# Add space between title and plots
combined_plot <- grid.arrange(
  title_grob,
  labeled_plots,
  ncol = 1,
  heights = c(0.1, 1) # Increase 0.1 for more space
)

# Save high-resolution figure
ggsave(
  filename = "KM_survival_combined_AB_above.tiff",
  plot = combined_plot,
  width = 14,
  height = 7,
  dpi = 600,
  compression = "lzw"
)




