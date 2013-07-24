plot.amdp = function(amdp_obj, plot_margin = 0.05, frac_to_plot = 1, plot_orig_pts_preds = TRUE,
					colorvec, x_quantile = FALSE, plot_pdp = FALSE, centered = FALSE, centered_percentile = 0.05, ...){

	#some argument checking
	if (class(amdp_obj) != "amdp"){ 
		stop("object is not of class 'amdp'")
	}
	if (frac_to_plot <= 0 || frac_to_plot > 1 ){
		stop("frac_to_plot must be in (0,1]")
	}

	#extract the grid and lines to plot	
	grid = amdp_obj$gridpts 
	n_grid = length(grid)
	ecdf_fcn = NULL
	if (x_quantile){
		ecdf_fcn = ecdf(grid)
		grid = ecdf_fcn(grid)
	}
	apdps = amdp_obj$apdps
	N = nrow(apdps)

	if (missing(colorvec)){
		colorvec = sort(rgb(runif(N, 0, 0.7), runif(N, 0, 0.7), runif(N, 0, 0.7)))
	} else {
		if (length(colorvec) < N){
			stop("color vector has length ", length(colorvec), " but there are ", N, " lines to plot")
		}
	}
	
	#pull out a fraction of the lines to plot
	plot_points_indices = which(as.logical(rbinom(N, 1, frac_to_plot)))
	apdps = apdps[plot_points_indices, ]
	if (nrow(apdps) == 0){
		stop("no rows selected: frac_to_plot too small.")
	}
	if (centered){
		apdps = apdps - apdps[, ceiling(ncol(apdps) * centered_percentile + 0.00001)]
	}
	colorvec = colorvec[plot_points_indices]
	
	##### now start plotting
	min_apdps = min(apdps)
	max_apdps = max(apdps)
	range_apdps = max_apdps - min_apdps
	min_apdps = min_apdps - plot_margin * range_apdps
	max_apdps = max_apdps + plot_margin * range_apdps

	xlab = amdp_obj$xlab
	if (x_quantile){
		xlab = paste("quantile(", xlab, ")", sep = "")
	}

	#plot all the prediction lines
	if (amdp_obj$logodds){
		ylab = "partial log-odds"
	} else {
		ylab = paste("partial yhat")
	}
	plot(grid, apdps[1, ], type = "n", ylim = c(min_apdps, max_apdps), xlab = xlab, ylab = ylab)

	for (i in 1 : nrow(apdps)){
		points(grid, apdps[i, ], col = colorvec[i], type = "l")
	}

	#if plot_pdp is true, plot actual pdp (in the sense of Friedman '01)
	if (plot_pdp){
		#compute them!
		friedman_pdp = apply(apdps, 2, mean) # pdp = average over the columns
		
		#calculate the line thickness based on how many lines there are
		num_lines = length(plot_points_indices)
		points(grid, friedman_pdp, col = "yellow", type = "l", lwd = min(5.5 + (num_lines / 100) * 0.75, 8)) #every 100 lines we get 0.5 more highlight up to 8
		points(grid, friedman_pdp, col = "BLACK", type = "l", lwd = 4)
	}

	if (plot_orig_pts_preds){ #indicate the fitted values associated with observed xj values
		yhat_actual = amdp_obj$actual_prediction[plot_points_indices]
		if (centered){
			#TO-DO
			yhat_actual = yhat_actual - apdps[, ceiling(ncol(apdps) * centered_percentile + 0.00001)]
		}
				
		if (x_quantile){
			xj = ecdf_fcn(amdp_obj$xj)[plot_points_indices]
		} else {
			xj = amdp_obj$xj[plot_points_indices]
		}
		points(xj, yhat_actual, col = "black", pch = 16, cex = 1.5)
		points(xj, yhat_actual, col = colorvec, pch = 16)

	}
	
	invisible(list(plot_points_indices = plot_points_indices))
}

