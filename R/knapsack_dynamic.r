
#  dynamic programming method for 0-1 knapsack problem without recursive functions

suppressWarnings(RNGversion("3.5.9"))
set.seed(42)
n <- 2000
knapsack_objects <- data.frame(
  w=sample(1:4000, size = n, replace = TRUE),
  v=runif(n = n, 0, 10000)
  # w=c(1,2,5,6,7,9) ,
  # v=c(1,6,18,22,28,36)
)
#x <- knapsack_objects[1:4,]
#y <- knapsack_objects[1:8,]
#z <- knapsack_objects[1:12,]

# caluculate all the vaules of knapscak(n, W) of each situation
#   and store these value in a matrix nw

#' Knapsack problem using the dynamic approach
#'
#' @param x value
#' @param W weight
#'
#' @return A list giving total no of objects and the maximum weight it can hold
#' \itemize{
#' \item x -knapsack_objects
#' \item W - total weight it can hold
#' }
#' @export
#'
#' @examples
#' \donotrun{
#'
#' knapsack_dynamic (x = knapsack_objects[1:8,], W = 2000)
#' }
knapsack_dynamic <- function(x,W){
  stopifnot(is.data.frame(x))
  stopifnot(W>0)
  n <- nrow(x)
  max_weight <- W
  nw <- matrix(nrow = n+1, ncol = max_weight+1)
  # initiaize the first line and the first column to zero
  nw[1,] <- 0
  nw[,1] <- 0
  # calculate the other elements of the matrix nw
  for (i in 2:nrow(nw)) {
      for (j in 2:ncol(nw)) { # the meanings of variable j  and j-1
                              # In matrix nw, j presents the column number.
                                   # because the first line and first column represent 0 item and 0 weight,
                                   # j in matrix is 1 more than the weight of item i
                              # j-1  represents the maximum weight each time when we calculate nw[i,j]
          if(x[i-1,1]<=j-1) {
            a <- nw[i-1, j-x[i-1,1]] + x[i-1,2]
            b <- nw[i-1, j]
            nw[i,j] <- ifelse(a>b, a, b)
               }
           else{
                    nw[i,j] <- nw[i-1,j]
                 }
      }
   }
   #print(nw)
  i <- nrow(nw)
  j <- ncol(nw)
  best_items <- vector(length = i)
  num_of_items <- 1
  while (i>1) {
    if(nw[i,j]>nw[i-1,j]){    # with item i-1
      best_items[num_of_items] <- i-1   # store item in vector best_items
      num_of_items <- num_of_items+1    # increase num_of_items with 1
      j <- j-x[i-1, 1]
    }
      i <- i-1
  }

    a <- which(best_items!=0) # delete empty elements in vector best_items
    best_items <- best_items[a]
    b <- vector(length = length(best_items)) # reorder the items in vector best_items
      for (i in 1:length(b)) {
        b[i]=best_items[length(b)-i+1]
      }
    best_items <- b
    best_value <- nw[n+1, max_weight+1]
    best_value <- round(best_value)
    result <- list(value=best_value, elements=best_items)

  return(result)
}
