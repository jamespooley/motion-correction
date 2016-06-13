library(readr)
library(dplyr)
library(stringr)
library(ggplot2)

pardoe_file <- "data/pardoe.motion.morphometry.20160416.csv"
pardoe_df <- read.csv(pardoe_file) %>% 
  tbl_df

pardoe_df %>% View


pardoe_file <- "data/pardoe.motion.morphometry.20160416.csv"
motion_df <- read.csv(pardoe_file) %>% 
  filter(study == "adhd") %>% 
  mutate(age_cent = age - mean(age, na.rm = TRUE),
         age_cent2 = age_cent^2,
         age_cent3 = age_cent^3)
# motion.artifact %in% c(1, 2),

# NOTE: USING ANALYSIS_DF AND NOT MOTION_DF

#############################################################
# ORDER OF BEST-FITTING NEURODEVELOPMENTAL TRAJECTORY MODEL #
#############################################################


rois <- names(pardoe_df[16:80])  # Get FreeSurfer ROI names from Pardoe et al.'s data
roi_list <- as.list(rois)  # Convert to list for easy programming

# Helper function to get p-value for regression coefficients
get_coefficient_p_value <- function(fit_summary, param, df) {
  p <- fit_summary$coefficients[param, "Pr(>|t|)"]
  p
}

# Helper function to get model's R^2 value 
get_r2 <- function(fit_summary) {
  r2 <- fit_summary$r.squared
  r2
}

# Helper function to get model's F-statistic
get_f_statistic <- function(fit_summary) {
  f <- fit_summary$fstatistic
  f
}

# TODO (DRY): Combine the following two functions into one function

# Function to get order of best model when no motion covariate used
get_best_without_motion <- function(roi, df) {
  
  fmla_linear <- as.formula(paste0(roi," ~ poly(age, 1, raw = TRUE)"))
  fmla_quadratic <- as.formula(paste0(roi," ~ poly(age, 2, raw = TRUE)"))
  fmla_cubic <- as.formula(paste0(roi," ~ poly(age, 3, raw = TRUE)"))
  
  fit_linear <- lm(fmla_linear, data = df)
  fit_quadratic <- lm(fmla_quadratic, data = df)
  fit_cubic <- lm(fmla_cubic, data = df)
  
  fits <- list(fit_linear, fit_quadratic, fit_cubic)
  # fits
  
  summaries <- lapply(fits, summary)
  p1 <- get_coefficient_p_value(summaries[[1]], "poly(age, 1, raw = TRUE)")
  p2 <- get_coefficient_p_value(summaries[[2]], "poly(age, 2, raw = TRUE)2")
  p3 <- get_coefficient_p_value(summaries[[3]], "poly(age, 3, raw = TRUE)3")
  
  
  model_comparison <- AIC(fit_linear, fit_quadratic, fit_cubic)
  
  if (p3 < .05 & model_comparison$AIC[3] < model_comparison$AIC[2] & model_comparison$AIC[3] < model_comparison$AIC[1]) {
    model_order <- 3
  } else if (p2 < .05 & model_comparison$AIC[2] < model_comparison$AIC[1]) {
    model_order <- 2
  } else
    model_order <- 1
  
  model_order
}




# Get vector of model orders using various motion estimates
best_model_without_motion <- sapply(roi_list, get_best_without_motion, pardoe_df)



# Make a nice data frame of results
roi_trajectory_results <- as.data.frame(rois) %>% 
  mutate(best_model_without_motion = best_model_without_motion)

# Inspect the data frame
roi_trajectory_results %>% View


# Cubic Trajectory

p_cubic <- ggplot(data = pardoe_df, aes(x = age, y = freesurfer.Right.Thalamus.Proper)) +
  geom_point(alpha = 0.25) +
  stat_smooth(method = "lm", se = FALSE, fill = NA, formula = y ~ poly(x, 3, raw = TRUE), colour = "red") +
  xlab("Age") + 
  ylab("Morphometric Estimate") +
  scale_y_continuous(breaks = NULL) +
  ggtitle("Cubic") +
  theme_bw()

p_quad <- ggplot(data = pardoe_df, aes(x = age, y = freesurfer.CC_Mid_Anterior)) +
  geom_point(alpha = 0.25) +
  stat_smooth(method = "lm", se = FALSE, fill = NA, formula = y ~ poly(x, 2, raw = TRUE), colour = "red") +
  xlab("Age") + 
  ylab("") +
  scale_y_continuous(breaks = NULL) +
  ggtitle("Quadratic") +
  theme_bw()

p_linear <- ggplot(data = pardoe_df, aes(x = age, y = freesurfer.Left.Pallidum)) +
  geom_point(alpha = 0.25) +
  stat_smooth(method = "lm", se = FALSE, fill = NA, formula = y ~ poly(x, 2, raw = TRUE), colour = "red") +
  xlab("Age") + 
  ylab("") +
  scale_y_continuous(breaks = NULL) +
  ggtitle("Linear") +
  theme_bw()

library(gridExtra)
p <- grid.arrange(p_cubic, p_quad, p_linear, ncol = 3, widths = rep(4, 3), heights = rep(2.5, 3))


# http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_%28ggplot2%29/
# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

png("poly-plots.png", res = 300, width = 5, height = 2.5, units = "in")
multiplot(p_cubic, p_quad, p_linear, cols = 3)
dev.off()
