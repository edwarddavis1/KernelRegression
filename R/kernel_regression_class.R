#' Kernel Regression Reference Class
#' 
#' @field x_df Data frame of input features 
#' @field y_df Data frame of output
#' @field kernel A string to select the kernel function to be used
#' @field b The degree of polynomial (for the polynomial kernel)
#' @field sigma A parameter for the RBF kernel
#' @field lambda Regularisation constant 
#' @field f The calculated kernel regression model
#' @method initialize Default constructor
#' @method train Trains the kernel regression; calculates the kernel matrix and the model
#' @method set_kernel Change the selected kernel and any of the parameters
#' @method plot Plots the kernel regression model with the original data
kernel_regression = setRefClass("kernel_regression",
                                fields=c(x_df="data.frame",
                                         y_df="data.frame",
                                         kernel="character",
                                         b="numeric",
                                         sigma="numeric",
                                         lambda="numeric",
                                         f="matrix"))
kernel_regression$methods(
  #' @param x_date Boolean variable stating whether the input contains dates or not
  #' @param y_date Boolean variable stating whether the output contains dates or not
  initialize = function(x_df, y_df, kernel="linear", 
                        b=2, sigma=0.105, lambda=0.01,
                        x_date=FALSE, y_date=FALSE) {
    # Check the kernel choice is okay
    if (!(kernel %in% c("linear", "poly", "RBF"))) stop("Error: kernel must be one of: linear, poly or RBF")
    
    # If x or y contain dates, set as date objects before moving to numeric
    if (x_date) .self$x_df = as.data.frame(lapply(x_df, as.Date))
    else .self$x_df = as.data.frame(lapply(x_df, as.numeric))
    
    if (y_date) .self$y_df = as.data.frame(lapply(y_df, as.Date))
    else .self$y_df = as.data.frame(lapply(y_df, as.numeric))
    
    .self$kernel = kernel
    .self$b = b
    .self$sigma = sigma
    .self$lambda = lambda
    
    .self$train()
  },
  train = function(kernel=.self$kernel) {
    
    # Construct kernel matrix
    x = as.matrix(as.data.frame(lapply(.self$x_df, as.numeric)))
    y = as.matrix(as.data.frame(lapply(.self$y_df, as.numeric)))
    n = dim(x)[1]
    K = matrix(nrow=n, ncol=n)
    for (i in 1:n) {
      for (j in 1:n) {
        if (kernel == "RBF") k_ij = RBF(as.matrix(x[i,]), as.matrix(x[j,]), sigma)     
        if (kernel == "poly") k_ij = poly(as.matrix(x[i,]), as.matrix(x[j,]), b)     
        if (kernel == "linear") k_ij = linear(as.matrix(x[i,]), as.matrix(x[j,]))     
        K[i,j] = k_ij
      }
    }
    
    # Use the input data to generate a k vector
    model = c()
    for (i in 1:n) {
      k = vector(length=n)
      x_0 = x[i,]
      for (j in 1:n) {
        if (kernel == "RBF") k[j] = RBF(as.matrix(x_0), as.matrix(x[j,]), sigma)
        if (kernel == "poly") k[j] = poly(as.matrix(x_0), as.matrix(x[j,]), b)
        if (kernel == "linear") k[j] = linear(as.matrix(x_0), as.matrix(x[j,]))
      }
      k = as.matrix(k)
      
      # Calculate model
      model[i] = as.numeric(t(k) %*% solve(K + lambda*diag(n)) %*% y)
    }
    .self$f = as.matrix(model)
  },
  #' @param retrain Boolean variable, if true the regression will be retrained immediately
  set_kernel = function(kernel=.self$kernel, b=.self$b, sigma=.self$sigma, 
                        lambda=.self$lambda, retrain=TRUE) {
    .self$b = b
    .self$sigma = sigma
    .self$lambda = lambda
    .self$kernel = kernel
    if (retrain) .self$train()
  },
  #' @param x_index Selects the input index in the case of multiple input features (plot is 2D)
  #' @param y_index Selects the output index in the case of multiple input features (plot is 2D)
  plot = function(x_index=1, y_index=1) {
    result_data = data.frame(
      x = c(.self$x_df[[x_index]]),
      y = c(.self$y_df[[y_index]]),
      model = c(.self$f)
    )
    
    plt = result_data %>%
      pivot_longer(cols=all_of(names(result_data %>% select(y, model)))) %>%
      ggplot(aes(x=x)) +
      geom_line(aes(y=value, col=name))
    return(plt)
  }
)