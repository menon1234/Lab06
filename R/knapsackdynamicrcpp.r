suppressWarnings(RNGversion("3.5.9"))
set.seed(42)
n <- 2000
knapsack_objects <- data.frame(
  w=sample(1:4000, size = n, replace = TRUE),
  v=runif(n = n, 0, 10000)
  # w=c(1,2,5,6,7,9) ,         # for test
  # v=c(1,6,18,22,28,36)
)
#x <- knapsack_objects[1:6,]     # for test
#y <- knapsack_objects[1:8,]    # for test
#z <- knapsack_objects[1:12,]    # for test

# caluculate all the vaules of knapscak(n, W) of each situation
#   and store these value in a matrix nw
nw <- matrix(nrow = 2, ncol = 2)  # do not know how to change the elements of  matrix
# in parent function knapsack_dynamic
# through the child function recursive
# so, define the matrix nw outside the function
# with a random nrow and ncol
#  the matrix nw will be redefined in function knapsack_dynamic
#  by nw <<- matrix(nro=n+1,ncol = max_weight+1)
#'
#' Knapsack Problem using the dynamic approach using Rcpp

#' knapsack_dynamicrcpp(x = knapsack_objects[1:8,], W = 2000)
knapsack_dynamicrcpp <- function(x,W){
  n <- nrow(x)
  max_weight <- W
  # redefine the matrix nw with nrow=n+1 and ncol=W+1
  # initiaize the first line and the first column to zero
  #     and all the other elements to -1
  nw <<- matrix(nrow = n+1,ncol = max_weight+1)
  nw[1,] <<- 0

  # check the matrix nw to find the items which are chosen
  #  by go through the matrix nw
  # use vector best_items to store the choosing items
  # because we find the items from bottom to top, the vector best_items store the items decreasely
  # use vector b as a tempt vector to reorder the items in vector best_items
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
  best_value <- round(best_value,0)
  result <- list(value=best_value,elements=best_items)

  return(result)
}

recursive <- function(i,j) {

  # the meanings of variable j  and j-1
  # In matrix nw, j presents the column number.
  # because the first line and first column represent 0 item and 0 weight,
  # j in matrix is 1 more than the weight of item i
  # j-1  represents the maximum weight each time when we calculate nw[i,j]

  if(knapsack_objects[i-1,1]<=j-1) {
    if(nw[i-1,j]==-1) {
      nw[i-1,j]<<- recursive(i-1,j)}
    if(nw[i-1,j-knapsack_objects[i-1,1]]==-1)
    {nw[i-1,j-knapsack_objects[i-1,1]] <<- recursive(i-1, j-knapsack_objects[i-1,1])}
    a <- nw[i-1, j]
    b <- nw[i-1, j-knapsack_objects[i-1,1]] + knapsack_objects[i-1,2]
    nw[i,j] <<- ifelse(a>b, a, b)
  }
  else{
    if(nw[i-1,j]==-1) {nw[i-1,j] <<- recursive(i-1,j)}
    nw[i,j] <<- nw[i-1,j]
  }
  # print(nw)    # for test
  return(nw[i,j])
}

