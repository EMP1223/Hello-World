library(randomForest)
library(missForest)
library(MASS)
library(amdp)


###########GRID COMPUTING
LAST_NAME = "kapelner"
NOT_ON_GRID = length(grep("wharton.upenn.edu", Sys.getenv(c("HOSTNAME")))) == 0

if (NOT_ON_GRID){
	directory_where_code_is = "C:\\Users\\kapelner\\workspace\\CGMBART_GPL"
} else {
	directory_where_code_is = getwd()
}
setwd(directory_where_code_is)

source("bartMachine/R/bart_package_inits.R")
source("bartMachine/R/bart_package_builders.R")
source("bartMachine/R/bart_package_predicts.R")
source("bartMachine/R/bart_package_data_preprocessing.R")
source("bartMachine/R/bart_package_plots.R")
source("bartMachine/R/bart_package_variable_selection.R")
source("bartMachine/R/bart_package_f_tests.R")
source("bartMachine/R/bart_package_summaries.R")

#get the Boston housing data
data(Boston)
X = Boston
#X = cbind(X, rnorm(nrow(X)))
y = X$medv
X$medv = NULL

bart_machine = build_bart_machine(X, y)

windows()
investigate_var_importance(bart_machine)

#create amdp's for all features in the boston housing data
amdb_bart_objs = list()
for (j in colnames(X)){
	amdb_bart_objs[[j]] = amdp(bart_machine, X, j, num_grid_pts = 100)
}

graphics.off()
for (j in colnames(X)){
	windows()
	par(mfrow = c(1, 3))
	plot(amdb_bart_objs[[j]], frac_to_plot = 0.1)
	plot(amdb_bart_objs[[j]], frac_to_plot = 1, centered = 0.02, prop_range_y = TRUE)
	cluster_amdp(amdb_bart_objs[[j]], nClusters = 2, prop_range_y = TRUE, centered = TRUE)
}

j = "age"
plot(amdb_bart_objs[[j]], frac_to_plot = 0.1, x_quantile = FALSE)
plot(amdb_bart_objs[[j]], frac_to_plot = 1, centered = 0.02, prop_range_y = TRUE, x_quantile = FALSE, plot_orig_pts_preds = FALSE)
cluster_amdp(amdb_bart_objs[[j]], nClusters = 2, prop_range_y = TRUE, centered = TRUE, plot_legend = TRUE)

j = "rm"
plot(amdb_bart_objs[[j]], frac_to_plot = 0.1)
plot(amdb_bart_objs[[j]], frac_to_plot = 1, centered = 0.02, prop_range_y = TRUE, plot_orig_pts_preds = FALSE)
cluster_amdp(amdb_bart_objs[[j]], nClusters = 2, prop_range_y = TRUE, centered = TRUE, plot_legend = TRUE)


j = "chas"
plot(amdb_bart_objs[[j]], frac_to_plot = 0.1)
plot(amdb_bart_objs[[j]], frac_to_plot = 1, centered = 0.02, prop_range_y = TRUE, plot_orig_pts_preds = FALSE)
cluster_amdp(amdb_bart_objs[[j]], nClusters = 2, prop_range_y = TRUE, centered = TRUE, plot_legend = TRUE)

j = "black"
plot(amdb_bart_objs[[j]], frac_to_plot = 0.1)
plot(amdb_bart_objs[[j]], frac_to_plot = 1, centered = 0.02, prop_range_y = TRUE, plot_orig_pts_preds = FALSE)
cluster_amdp(amdb_bart_objs[[j]], nClusters = 2, prop_range_y = TRUE, centered = TRUE, plot_legend = TRUE)


#remind me of the linear model
lm_mod = lm(medv ~ ., Boston)
summary(lm_mod)

#pub images










