cluster_amdp = function(amdp_obj, nClusters, plot = TRUE, plot_margin = 0.05, colorvec, plot_pdp = FALSE,
			x_quantile = FALSE, rug = TRUE, avg_lwd = 3, prop_range_y = FALSE, centered = FALSE, plot_legend = FALSE, ...){

	DEFAULT_COLORVEC = c("red", "green", "blue", "yellow2", "black", "violetred4", "cyan", "darkgrey", "orange2", "bisque3")
		
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
	
	cluster_centers = cl$centers
	
	if (centered){
		for (k in 1 : nrow(cluster_centers)){
			cluster_centers[k, ] = cluster_centers[k, ] - cluster_centers[k, 1] #ifelse(cluster_centers[o, 1] < 0, -cluster_centers[o, 1], cluster_centers[o, 1]
		}		
	}
		
	if (plot){
		#y limits
		rg = range(cluster_centers) 
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
		plot(grid, as.numeric(cluster_centers[1, ]), 
				type = 'n', 
				ylim = c(rg_min, rg_max), 
				ylab = paste("cluster yhat"), 
				xlab = xlab, 
				xaxt = ifelse(amdp_obj$nominal_axis, "n", "s"))
		starting_y_val = rank(cl$centers[, 1]) #use original, non-centered object only
		cluster_size = cl$size / sum(cl$size)
		total_line_width = avg_lwd * nClusters
		
		centers_to_plot = array(NA, nrow(cluster_centers))
		for(i in 1 : nrow(cluster_centers)){		
			#we re-order it so that when the code is rerun, randomness in kmeans
			#doesn't switch which cluster goes with which color.
			center_to_plot = which(starting_y_val == i)	
			centers_to_plot[i] = center_to_plot
			points(grid, cluster_centers[center_to_plot, ], col = colorvec[i], type = "l", 
					lwd = cluster_size[center_to_plot] * total_line_width)
		}
		
		if (prop_range_y){
			at = seq(min(apdps), max(apdps), length.out = 5)
			#we need to organize it so it's at zero
			at = at - min(abs(at))
			
			labels = round(at / amdp_obj$range_y, 2)
			axis(4, at = at, labels = labels)
		}
		
		if (amdp_obj$nominal_axis){
			axis(1, at = sort(amdp_obj$xj), labels = sort(amdp_obj$xj))
		}
		
		#legend
		if (plot_legend){
			prop_data_in_clusters = round(cl$size / sum(cl$size), 2)
			
			legend("topleft", inset = 0.01, legend = as.character(prop_data_in_clusters), fill = colorvec, cex = 0.8)
		}
	}

	invisible(cl)
}
