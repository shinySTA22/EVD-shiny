## -------------------------------------- DATA --------------------------------------

#### Generate Random Data

generateRandomData <- function(n, type, s, slope) {
	if(s == "kecil") {
		sd = 2
	} else if (s == "besar") {
		sd = 15
  	} else {
		sd = 5
  	}

	x <- runif(n, 0, 10)
	
	if(type == "Dataset Bangkitan Linier") {
		y <- slope * x + rnorm(n, 0, sd)
  	} else if (type == "Dataset Bangkitan Kuadratik") {
		y <- slope * x^2 + rnorm(n, 0, sd)
	} else {
    	y <- runif(1, -5, 5) * x + runif(n, 0, 10)
  	}
  
  	return(data.frame(x = x, y = y))
}

#### Create Regression Plot

createRegressionPlot <- function(data, x_var, y_var, smoothness, show_reg_line, show_smooth_line, show_residuals, x_lab, y_lab) {
  
  	# -- model regresi
  	lm_model <- lm(formula = paste(y_var, "~", x_var), data = data)
  
  	# -- ggplot regresi
  	p <- ggplot(data, aes_string(x = x_var, y = y_var)) + geom_point(shape = 21, size = 2.5, stroke = 0.5, color = "black", fill = "#1ec289f0") + theme_minimal() + labs(y = y_lab, x = x_lab)
  
  	# -- ggplot regression line = T
  	if (show_reg_line) {
    	p <- p + geom_smooth(method = "lm", se = FALSE, color = "#6262be", size = 1)
  	}
  
  	# -- ggplot smooth line = T
  	if (show_smooth_line) {
    	p <- p + geom_smooth(method = "loess", se = FALSE, color = "#92fb51", span = smoothness, size = 1)
  	}
  
  	# -- ggplot show residuals = T
  	if (show_residuals) {
    	residuals <- residuals(lm_model)
    	data$residuals <- residuals
    	p <- p + geom_segment(aes(xend = data[[x_var]], yend = predict(lm_model)), color = "#e252bc")
  	}
  
  	## -- konversi ke plotly
  
  	return(p)
}

#### Create Residual Plot

createResidualPlot <- function(data, x_var, y_var, smoothness, show_smooth_line, x_lab, y_lab) {

	p <- ggplot(data, aes_string(x = x_var, y = y_var)) + geom_point(shape = 21, size = 2.5, stroke = 0.5, color = "black", fill = "#1ec289f0") + theme_minimal() + geom_hline(yintercept = 0) + labs(y = y_lab, x = x_lab) 
	
	if (show_smooth_line) {
		p <- p + geom_smooth(method = "loess", se = FALSE, color = "#1ba0c1", span = smoothness) 
	}
  
  	return(p)

}

#### Append Dataframe Column

makeColumn <- function(col, val) {
	df <- data.frame(val)
	names(df) <- col
	return(df)
}

#### Create Histogram of Residual

createHistogram <- function(data, var, bin, show_normal_curve) {
	if(!show_normal_curve) {
		h <- ggplot(data, aes_string(x = var)) + geom_histogram(binwidth = bin, colour = 1, fill = "#f48194") + theme_minimal()
	} else {
		h <- ggplot(data, aes_string(x = var)) + geom_histogram(aes(y = ..density..), binwidth = bin, colour = 1, fill = "#f48194") + geom_density(lwd = 1, colour = 4, fill = "#00d0ff", alpha = 0.6) + theme_minimal()
	}
		
	return(h)
}