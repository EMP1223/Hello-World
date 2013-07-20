plot.amdp = function(amdpobj, xtest_margin = 0, plot_margin = 0.05, frac_to_plot = 1, plot_orig_pts = TRUE,
					colorvec, x_quantile = FALSE, plot_pdp = FALSE, ...){

	#some argument checking
	if(class(amdpobj)!="amdp")					{ stop("object is not of class 'amdp'")}
	if(frac_to_plot <= 0 || frac_to_plot > 1 )		{ stop("frac_to_plot must be in (0,1]")}

	#extract the grid and lines to plot	
	grid = amdpobj$gridpts; n_grid = length(grid)
	ecdf_fcn = NULL
	if(x_quantile){
		ecdf_fcn = ecdf(grid)
		grid = ecdf_fcn(grid)
	}
	apdps = amdpobj$apdps; N = nrow(apdps)

	if(missing(colorvec)){
		colorvec = rgb(runif(N, 0, 0.7), runif(N, 0, 0.7), runif(N, 0, 0.7))
	}else{
		if (length(colorvec) < N){
			stop("color vector has length ", length(colorvec), " but there are ", N, " lines to plot")
		}
	}
	
	#pull out a fraction of the lines to plot
	plot_points_indices = which(as.logical(rbinom(N, 1, frac_to_plot)))
	apdps = apdps[plot_points_indices, ]
	if(nrow(apdps) == 0){
		stop("frac_to_plot too small.")
	}
	colorvec = colorvec[plot_points_indices]
	
	##### now start plotting
	min_apdps = min(apdps)
	max_apdps = max(apdps)
	range_apdps = max_apdps - min_apdps
	min_apdps = min_apdps - plot_margin * range_apdps
	max_apdps = max_apdps + plot_margin * range_apdps
	if (class(amdpobj$predictor) != "character"){
		xlab = paste("x",amdpobj$predictor,sep="")  #x1, x2 etc.
	} else {
		xlab = amdpobj$predictor					#the actual name of the feature.
	}
	if(x_quantile){
		xlab = paste("quantile(", xlab, ")", sep = "")
	}

	#plot all the prediction lines
	if(amdpobj$logodds){
		ylab = "partial log-odds"
	}else{
		ylab = paste("partial",expression(hat(y)), sep=" ")
	}
	plot(grid, apdps[1, ], type = "n", ylim = c(min_apdps, max_apdps), xlab = xlab, ylab = ylab)

	for( i in 1 : nrow(apdps) ){
		points(grid, apdps[i, ], col = colorvec[i], type = "l")
	}

	#if plot_pdp is true, plot actual pdp (in the sense of Friedman '01)
	if(plot_pdp){
		#compute them!
		friedman_pdp = apply(apdps, 2, mean) # pdp = average over the columns
		points(grid, friedman_pdp, col = "BLACK", type = "l", lwd = 2.5)
	}

	if (plot_orig_pts){ #indicate the fitted values associated with observed xj values
		yhat_actual = amdpobj$actual_prediction[plot_points_indices]
		
		if(x_quantile){
			xj = ecdf_fcn(amdpobj$xj)[plot_points_indices]
		}else{
			xj = amdpobj$xj[plot_points_indices]
		}
		points(xj, yhat_actual, col = "black", pch = 16, cex = 1.5) #this cex should be a function of the number of lines.
	}
}

