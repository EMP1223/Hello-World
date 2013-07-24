DEFAULT_COLORVEC = c("red", "green", "blue", "yellow2", "black", "violetred4", "cyan", "darkgrey", "orange2", "bisque3")

cluster.amdp = function(amdp_obj, nClusters, plot = TRUE, plot_margin = 0.05, colorvec, plot_pdp = FALSE,
			x_quantile = FALSE, avg_lwd = 3, ...){
		
	if (missing(nClusters) || !(nClusters %% 1 == 0 ) || (nClusters <= 0)){
		stop("nClusters must be a positive integer.")
	}
	#make all curves have avg value = 0.
	apdps = t(scale(t(amdp_obj$apdps), center = T, scale = F))  #sum(apdps[i,]) = 0 for all i

	#cluster
	cl = kmeans(apdps, iter.max = 20, centers = nClusters, ...)
	if (missing(colorvec)){
		colorvec = DEFAULT_COLORVEC
		if(length(colorvec) < nClusters){
			colorvec = c(colorvec, rgb(runif(nClusters - 10, 0, 0.7), runif(nClusters - 10, 0, 0.7), runif(nClusters - 10, 0, 0.7)))
		}
	}
	
	if (plot){
		#y limits
		rg = range(cl$centers) 
		dist = rg[2] - rg[1]
		rg_min = rg[1] - plot_margin * dist
		rg_max = rg[2] + plot_margin * dist

		#x grid and xlab
		xlab = amdp_obj$xlab
		grid = amdp_obj$gridpts 
		if (x_quantile){
			xlab = paste("quantile(", xlab, ")", sep = "")
			ecdf_fcn = ecdf(grid)
			grid = ecdf_fcn(grid)
		}
		plot(grid, cl$centers[1,], type = 'n', ylim = c(rg_min, rg_max), ylab = paste("cluster yhat"), xlab = xlab)
		starting_y_val = as.numeric(rank(cl$centers[, 1]))
		cluster_size = cl$size / sum(cl$size)
		total_line_width = avg_lwd * nClusters
		for(i in 1 : nrow(cl$centers)){		
			#we re-order it so that when the code is rerun, randomness in kmeans
			#doesn't switch which cluster goes with which color.
			center_to_plot = which(starting_y_val == i)	
			points(grid, cl$centers[center_to_plot, ], col = colorvec[i], type = "l", 
					lwd = cluster_size[center_to_plot] * total_line_width)
		}
	}
	invisible(cl)
}
