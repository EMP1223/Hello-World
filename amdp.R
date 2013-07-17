bart_machine.apdp = function(bart_machine, ...){
	apdp(bart_machine, ...)
}

COLORS = array(NA, 500)
for (i in 1 : 500){
	COLORS[i] = rgb(runif(1, 0, 0.7), runif(1, 0, 0.7), runif(1, 0, 0.7))
}

ORD_VAR_MAX_NUM_LEVELS = 5

apdp = function(model, X, j, num_grid_pts = 50, xtest_margin = 0, plot_margin = 0.05, pct_to_plot = 1, plot_orig_pts = TRUE, colors = COLORS, ...){
	if (length(colors) < nrow(X)){
		stop("color vector has length ", length(colors), " but there are ", nrow(X), " rows in the data")
	}
	
	xj = X[, j]
	n = nrow(X)
	
	#now create xj-to-predict values
	min_xj = NULL
	max_xj = NULL
	vals = sort(as.numeric(names(table(xj))))
	if (length(vals) <= ORD_VAR_MAX_NUM_LEVELS){
		pred_test_values = vals
		#override num_grid_pts because there's not too many vals
		num_grid_pts = length(pred_test_values)
	} else {
		min_xj = min(xj, na.rm = TRUE)
		max_xj = max(xj, na.rm = TRUE)
		min_xj_seq = min_xj - xtest_margin * (max_xj - min_xj)
		max_xj_seq = max_xj + xtest_margin * (max_xj - min_xj)
		pred_test_values = seq(from = min_xj_seq, to = max_xj_seq, by = (max_xj_seq - min_xj_seq) / (num_grid_pts - 1))		
	}
		
	#now pull out which xi's we're going to look at as well as their corresponding colors
	plot_points_indices = which(as.logical(rbinom(n, 1, pct_to_plot)))
	X_with_gridded_xj = as.data.frame(X)[plot_points_indices, ]
	colors = colors[plot_points_indices]
	
	apdps = matrix(NA, nrow(X_with_gridded_xj), num_grid_pts)
	colnames(apdps) = pred_test_values
	
	cat(num_grid_pts, ":", sep = "")
	for (t in 1 : num_grid_pts){
		X_with_gridded_xj[, j] = pred_test_values[t]
		apdps[, t] = predict(model, X_with_gridded_xj, ...)
#		apdps[, t] = predict(model, X_with_gridded_xj) #for debugging (same thing but without the ...)
		cat(".")
	}
	cat("\n")
	
	#now start plotting
	min_apdps = min(apdps)
	max_apdps = max(apdps)
	range_apdps = max_apdps - min_apdps
	min_apdps = min_apdps - plot_margin * range_apdps
	max_apdps = max_apdps + plot_margin * range_apdps
	if (class(j) == "character"){
		xlab = j
	} else {
		xlab = colnames(X)[j]
	}
	#plot all the prediction lines
	plot(pred_test_values, apdps[1, ], type = "n", ylim = c(min_apdps, max_apdps), xlab = xlab, ylab = expression(hat(y)))
	for (i in 1 : nrow(X_with_gridded_xj)){
		points(pred_test_values, apdps[i, ], col = colors[i], type = "l")
	}
	if (plot_orig_pts){
		#now plot all the points for the observatiosn because they are useful to see
		X_with_gridded_xj = as.data.frame(X)[plot_points_indices, ]
		xj = X_with_gridded_xj[, j]
		yhat = predict(model, X_with_gridded_xj)
		for (i in 1 : nrow(X_with_gridded_xj)){
			points(xj[i], yhat[i], col = "black", pch = 16, cex = 1.5)
			points(xj[i], yhat[i], col = colors[i], pch = 16)
		}
	}
	
	if (xtest_margin > 0 && length(vals) > ORD_VAR_MAX_NUM_LEVELS){
		abline(v = min_xj, col = "gray")
		abline(v = max_xj, col = "gray")
	}
	invisible(list(apdps = apdps, pred_test_values = pred_test_values, min_xj = min_xj, max_xj = max_xj, X = X, j = j))
}


apdp_diffs = function(apdp_obj, num_permutes = 3, plot_margin = 0.05, plot_window_width = 2){
	apdps_diffs = t(apply(apdp_obj$apdps, 1, diff))
	
	par(mfrow = c(plot_window_width, ceiling((num_permutes + 1) / plot_window_width)))
	#plot the original in the top right
	apdp_diff_plot(apdp_obj, apdps_diffs, plot_margin)
	
	for (b in 1 : num_permutes){
		#permute apdps_diffs
		for (j in 1 : ncol(apdps_diffs)){
			apdps_diffs[, j] = sample(apdps_diffs[, j])
		}		
		apdp_diff_plot(apdp_obj, apdps_diffs, plot_margin)
	}
	
}

apdp_diff_plot = function(apdp_obj, apdps_diffs, plot_margin){
	diff_pred_test_values = apdp_obj$pred_test_values[2 : length(apdp_obj$pred_test_values)]
	#now start plotting
	min_apdps = min(apdps_diffs)
	max_apdps = max(apdps_diffs)
	range_apdps = max_apdps - min_apdps
	min_apdps = min_apdps - plot_margin * range_apdps
	max_apdps = max_apdps + plot_margin * range_apdps
	plot(diff_pred_test_values, apdps_diffs[1, ], type = "n", ylim = c(min_apdps, max_apdps), xlab = apdp_obj$j, ylab = paste("dy/dx_", apdp_obj$j, sep = ""))
	for (i in 1 : nrow(apdps_diffs)){
		if (i < 10){
			points(diff_pred_test_values, apdps_diffs[i, ], col = COLORS[i %% length(COLORS)], type = "l")
		}
	}
	abline(v = apdp_obj$min_xj, col = "gray")
	abline(v = apdp_obj$max_xj, col = "gray")
	abline(a = mean(apdps_diffs), b = 0)
	invisible(apdps_diffs)
}


