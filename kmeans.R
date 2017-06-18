# This script Impplements kmeans algorithm
#--------------------------------------------
### function that calculate distance between point and matrix ###
calc.dist <- function(point , mat)
{
  # get matrix dimentions
  len <- dim(mat)
  # all dists
  all.dists <- as.matrix(0,len,1)
  #loop over the matrix
  for(i in 1:len[1])
  {
    curr <- mat[i,]
    # calc distance
    x <- curr[1] - as.numeric(point[1])
    y <- curr[2] - as.numeric(point[2])
    under.root <- x^2 + y^2
    curr.dist <- under.root^(1/2)
    all.dists[i]<- curr.dist
  }
  return(all.dists)
}

############## Load Data ###################
dataset <- as.matrix(dataModified)
dim(dataset) # 300 x 2
#--------------------------------------------
plot(dataset[,1],dataset[,2]) # plot main dataste
########  Initialize cluster center ###########
# 1) rondomly pick 1 points as center
center.one <- sample(1:300, 1, replace=F) 
# 2) calculate distances between point and matrix
distances.one <- calc.dist(dataset[center.one,],dataset)
# 3) get max distance and take its index to be 2nd cluster center
center.two <- which.max(distances.one)
# 4) calc distances for 2nd center
distances.two <- calc.dist(dataset[center.two,],dataset)
# 3) get max distance and take its index to be 2nd cluster center
center.three <- which.max(distances.two)
#--------------------------------------------------
p1 <- dataset[center.one,]
p2 <- dataset[center.two,]
p3 <- dataset[center.three,]
for(j in 1:1000)
#while(TRUE)
{
  ########## Start the algorithm #################
  # 1) calc distance betwen data and each center
  c1.dist <- calc.dist(p1,dataset)
  c2.dist <- calc.dist(p2,dataset)
  c3.dist <- calc.dist(p3,dataset)
  # 2) get min 100 dist for each center 
  c1.min <- which(c1.dist %in% sort(c1.dist)[1:100])  # this gives you an index vector
  c2.min <- which(c2.dist %in% sort(c2.dist)[1:100])  
  c3.min <- which(c3.dist %in% sort(c3.dist)[1:100])  
  # Plot results
  plot(dataset[,1],dataset[,2], type = "n", xlab = "x-axis",ylab = "y-axis")  # setting up coord. system
  points(dataset[c1.min,1], dataset[c1.min,2], col = "red")
  points(dataset[c2.min,1], dataset[c2.min,2], col = "blue")
  points(dataset[c3.min,1], dataset[c3.min,2], col = "green")
  ################ Update centers ################
  old.c1 <- c1.min
  old.c2 <- c2.min
  old.c3 <- c3.min
  # get avarage point per cluster
  avrg.c1 <- list(mean(dataset[c1.min,1]),mean(dataset[c1.min,2]) )
  avrg.c2 <- list(mean(dataset[c2.min,1]),mean(dataset[c2.min,2]) )
  avrg.c3 <- list(mean(dataset[c3.min,1]),mean(dataset[c3.min,2]) )
  if(as.numeric(avrg.c1[1]) == center.one[1])
  {
    break
  }
  else
  {
    p1 <- as.list(avrg.c1)
    p2 <- as.list(avrg.c2)
    p3 <- as.list(avrg.c3)
  }
  
}








