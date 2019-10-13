library(Lab06)
library(microbenchmark)
RNGkind(sample.kind = "Rounding")
set.seed(42)
n<-100000
knapsack_objects<-data.frame(
  w=sample(1:4000, size = n,replace = TRUE),
  v=runif(n=n,0,10000)
)
#greedy
source("C:/Users/nandu/Documents/Lab06/R/greedyknapsack.r")
profvis::profvis(gk<-greedy_knapsack(x = knapsack_objects[1:800,], W = 3500))
gk<- microbenchmark (greedy_knapsack(x = knapsack_objects[1:800,], W = 3500), times = 5)

#dynamic
source("C:/Users/nandu/Documents/Lab06/R/knapsack_dynamic.r")
profvis::profvis(kd<-knapsack_dynamic(x = knapsack_objects[1:800,], W = 3500))
kd<-microbenchmark(kd<-knapsack_dynamic(x = knapsack_objects[1:800,], W = 3500))


#bruteforce
source("C:/Users/nandu/Documents/Lab06/R/bruteforce.r")
profvis::profvis(kd<-knapsack_dynamic(x = knapsack_objects[1:800,], W = 3500))
