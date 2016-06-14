library(readr)
library(dplyr)
library(stringr)
library(ggplot2)

# TODO: Change all to modify base plot "p <- ggplot(data = analysis_df)"

p_rms <- ggplot(data = analysis_df) +
  geom_boxplot(aes(x = as.factor(motion.artifact), y = mean.rms)) +
  labs(
    x = "Image Quality Rating",
    y = "Mean RMSD"
  )

p_qi1 <- ggplot(data = analysis_df) +
  geom_boxplot(aes(x = as.factor(motion.artifact), y = Qi1)) +
  labs(
    x = "Image Quality Rating",
    y = "Qi1"
  )

p_snr <- ggplot(data = analysis_df) +
  geom_boxplot(aes(x = as.factor(motion.artifact), y = SNR)) +
  labs(
    x = "Image Quality Rating",
    y = "SNR"
  )

png("figs/rating-motion.png", res = 300, width = 5, height = 2.5, units = "in")
multiplot(p_rms, p_qi1, p_snr, cols = 3)
dev.off()


ggplot(data = analysis_df)
