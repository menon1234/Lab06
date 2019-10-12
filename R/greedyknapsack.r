suppressWarnings(RNGversion("3.5.9"))
set.seed(42)
n <- 2000
knapsack_objects <- data.frame(
   w=sample(1:4000, size = n, replace = TRUE),
   v=runif(n = n, 0, 10000)
 # w=c(1,2,5,6,7,9) ,
 # v=c(1,6,18,22,28,36)
)
# x,y,z for test
#x <- knapsack_objects[1:4,]
#y <- knapsack_objects[1:8,]
#z <- knapsack_objects[1:12,]

# caluculate all the vaules of value(i)/weight(i) of each item
# order the data by value(i)/weight(i) decreasely
# put as many items as possible in the knapsack if the weight(i) still is less than the remainder
# the remainder = max_weight - weight(i)


#' Knapsack Problem using the greedy approach
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
#' greedy_knapsack(x = knapsack_objects[1:800,], W = 3500)
greedy_knapsack <- function(x,W){
  stopifnot(is.data.frame(x))
  stopifnot(W>0)
  max_weight <- W
  x <- cbind(x, c(1:nrow(x)))  #' add item number to each item in dataframe x, 3rd column
  a <- which(x[,1]<=max_weight)   #' delete the items with higer weight than W
  new_data <- x[a,]
  b <- new_data[,2]/new_data[1]   #' calculate the value(i)/weight(i)
  new_data <- cbind(new_data, b)  #' add this value value(i)/weight(i) to each item, 4th column
     #' order data decrasely by value(i)/weight(i)
  new_data <- new_data[order(new_data[,4],decreasing = TRUE),]
               #' Problem: value(i)/weight(i) is equal but value(i) is not equal
               #' should order decreasely by value(i) too.
  best_items <- vector(length = nrow(new_data))
  best_value <- 0
  i <- 1
  while(new_data[i,1]<= max_weight){
    best_items[i] <- i                        #' the number in new_data
    best_value <- best_value + new_data[i,2]
    max_weight <- max_weight - new_data[i,1]
    i <- i+1
    if(i>nrow(new_data)) {break}  # if it is the end of the data,stop
  }
  #' print(new_data)
  a <- which(best_items!=0)  # delete empty elements in vector best_items
  best_items <- best_items[a]
  best_items <- new_data[best_items,3]  # the real item number in x
  best_items <- best_items[order(best_items)]  # output the item number increasely
  best_value <- round(best_value,0)
  result <- list(value=best_value,elements=best_items)
  return(result)




  }
