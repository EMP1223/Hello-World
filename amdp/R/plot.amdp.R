plot.amdp = function(amdp_obj, plot_margin = 0.05, frac_to_plot = 1, plot_orig_pts_preds = TRUE,
					colorvec, color_by = NULL, x_quantile = FALSE, plot_pdp = FALSE, plot_new_data = FALSE, 
					centered = FALSE, rug = TRUE, prop_range_y = FALSE, centered_percentile = 0.05, ...){
	
	DEFAULT_COLORVEC = c("red", "green", "blue", "yellow2", "black", "violetred4", "cyan", "darkgrey", "orange2", "bisque3")
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

	#### figure out the colorvec.
	legend_text = NULL #default is no legend.
	#case 1: random
	if (missing(colorvec) && missing(color_by)){
		colorvec = sort(rgb(runif(N, 0, 0.7), runif(N, 0, 0.7), runif(N, 0, 0.7)))
	} 
	#case 2: both colorvec and color_by specified, so print a warning but use colorvec.
	if(!missing(colorvec) && !missing(color_by)){
		if (!missing(colorvec) && length(colorvec) < N){
			stop("color vector has length ", length(colorvec), " but there are ", N, " lines to plot")
		}
		warning("Both colorvec and color_by_predictor are specified...using colorvec.")
	}	
	#case 3: colorvec missing but color_by is specified.
	if(!missing(color_by) && missing(colorvec)){
		#argument checking first:
		arg_type = class(color_by)
		if(!(arg_type %in% c("character", "numeric"))){
			stop("color_by must be a column name in X or a column index")
		}
		if(class(color_by) == "character"){
			if(!(color_by %in% names(amdp_obj$Xamdp))){
				stop("The predictor name given by color_by was not found in the X matrix")
			}
		} else{  #check numeric
			if( color_by < 1 || color_by > ncol(amdp_obj$Xamdp) || (color_by%%1 !=0)){
				stop("color_by must be a column name in X or a column index")
			}
		}
		x_color_by = amdp_obj$Xamdp[, color_by]
		x_unique = unique(x_color_by)
		num_x_color_by = length(x_unique)		
		
		
		#if there are 10 or fewer unique values of this x value, we use the
		#same color in DEFAULT_COLORVEC for each. Otherwise, we use a rainbow.
		if(num_x_color_by <= 10){
			
			which_category = match(x_color_by, x_unique)
			colorvec = DEFAULT_COLORVEC[which_category]
			
			#now make the legend.
			legend_text = as.data.frame(cbind( x_unique , DEFAULT_COLORVEC[1:num_x_color_by]))
			x_column_name = ifelse(is.character(color_by), color_by, paste("x_",color_by,sep=""))
			names(legend_text) = c(x_column_name,"color")
			cat("AMDP Color Legend\n")
			print(legend_text)			
		}
		else{
			if(is.factor(x_color_by)){
				warning("color_by is a factor with greater than 10 levels: coercing to numeric.")
				x_color_by = as.numeric(x_color_by)
			}			
			#easy, just smallest to largest with ties broken randomly.
			colorvec = rainbow(N)[rank(x_color_by, ties="random")]       
		}
	}

	
	#pull out a fraction of the lines to plot
	plot_points_indices = which(as.logical(rbinom(N, 1, frac_to_plot)))
	apdps = apdps[plot_points_indices, ]
	if (nrow(apdps) == 0){
		stop("no rows selected: frac_to_plot too small.")
	}
	if (centered){
		centering_vector = apdps[, ceiling(ncol(apdps) * centered_percentile + 0.00001)]
		apdps = apdps - centering_vector
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
		ylab = paste("partial yhat", ifelse(centered, "(centered)", ""))
	}
	plot(grid, apdps[1, ], 
			type = "n", 
			ylim = c(min_apdps, max_apdps), 
			xlab = xlab, 
			ylab = ylab, 
			xaxt = ifelse(amdp_obj$nominal_axis, "n", "s"), 
			...)
	
	if (rug){
		rug(amdp_obj$xj)	
	}
	
	
	if (amdp_obj$nominal_axis){
		axis(1, at = sort(amdp_obj$xj), labels = sort(amdp_obj$xj))
	}	
	if (centered && prop_range_y){
		at = seq(min(apdps), max(apdps), length.out = 5)
		#we need to organize it so it's at zero
		at = at - min(abs(at))
		
		labels = round(at / amdp_obj$range_y, 2)
		axis(4, at = at, labels = labels)
	}
	
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
			yhat_actual = yhat_actual - centering_vector
		}
				
		if (x_quantile){
			xj = ecdf_fcn(amdp_obj$xj)[plot_points_indices]
		} else {
			xj = amdp_obj$xj[plot_points_indices]
		}
		points(xj, yhat_actual, col = "black", pch = 16, cex = 1.5)
		points(xj, yhat_actual, col = colorvec, pch = 16)

	}
	if(is.null(legend_text)){
		invisible(list(plot_points_indices = plot_points_indices, legend_text = legend_text))
	}else{
		invisible(list(plot_points_indices = plot_points_indices, legend_text = legend_text))
	}
}

