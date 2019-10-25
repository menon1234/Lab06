
# enumerate all different combinations using a binary representation of 1 to 2^n
# put all different combinations  in a matrix "combination"
# store all value=1 in each line of  the matrix to vector a by command which()
# calculate the corresponding weights and values in data.frame x using sum(x[a,1]) and sum(x[a,2])
# store the maximum value when weight is less than W into a list "result"
#'
#' Knapsack problem using the bruteforce approach
#'
#' @param x value
#' @param W weight
#' @param parallel for parallel computation
#'
#' @return A list giving total no of objects and the maximum weight it can hold
#' \itemize{
#' \item x -knapsack_objects
#' \item W - total weight it can hold
#' }
#' @export
#'
#'

brute_force_knapsack <- function(x,W,parallel=FALSE){
 stopifnot(is.data.frame(x))
  stopifnot(W>0)
  n <- nrow(x)

  n1 <- floor(n/2)    # for parallel, divide data for 4 parts, n1 is the half position
  n2 <- floor(n1/2)     #  n2 is the half position of the fore half position
  n3 <- floor((n+n1)/2)  # n3 is the half position of the back half position
  if(parallel==TRUE){
    cores <- parallel::detectCores()
    y <- list(knapsack_objects[1:n2,],knapsack_objects[(n2+1):n1,],knapsack_objects[(n1+1):n3,],knapsack_objects[(n3+1):n,])
    result <- parallel::mclapply(y,brute_force_knapsack,W,mc.cores = cores)
  }

  combination <- matrix(nrow = 2^n,  ncol = n)
  # combination[,] <- 0
  storebin <- vector(length = n)
  for (i in 1:2^n) {
    binary <- intToBits(i)
       for (j in 1:n) {
         storebin[j] <- as.numeric(binary[j])
       }
      combination[i,] <- storebin
   }
   best_value <- 0
   result <- list(value=0, element=0)
   for (i in 1:2^n) {
       a <- which(combination[i,]==1)
       sum_weight <- sum(x[a,1])
       sum_value <- sum(x[a,2])
       #cat(sum_weight,sum_value, "/n  ")   # test the process
       #print(a)
       if(sum_weight<=W & sum_value>best_value) {
         best_value <- sum_value
         best_items <- a
         actual_weight <- sum_weight
                  }
   }

    best_value <- round(best_value,0)
    result <- list(value=best_value,elements=best_items)



    return(result)

}
