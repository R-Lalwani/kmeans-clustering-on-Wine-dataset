#====================================================================================================================================================#
#                                                         K-means in R
#                                                   --------------------------
# Description: 
#              
#====================================================================================================================================================#
#__________________________________________________________________________________________
#
#     Authors:        Roopali Lalwani
#     Description :   The program below perfomrs k-means clustering on Wine dataset of rattle package
#
#------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------

# Detaching all loaded packages (except for base packages) 

detachAllPackages <- function() {
  
  basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
  
  package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
  
  package.list <- setdiff(package.list,basic.packages)
  
  if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
  
}

detachAllPackages()

# list all packages we'd need
list_of_packages <- c('rattle.data', 'NbClust', 'factoextra')


# install packages, if not already installed
new_packages<- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]
 if(length(new_packages)) install.packages(new_packages, repos = "http://cran.us.r-project.org")

# Loading all reguired packages
lapply(list_of_packages, require, character.only = TRUE)


# Setting path to that of this document using rstudioapi package
path <- dirname(rstudioapi::getActiveDocumentContext()$path)
path <- paste0(path, "/")
setwd(path)

## Rattle data

data(wine)

# 178 samples of 3 types of wines
head(wine)

### make clusters of wine samples

### Data cleaning & transformation


### HANDLING MISSING VALUES

## K-means handles only numerical values - no factors or categorical varibales
str(wine)
wine <- wine[, -1]

## what if there are categorical - encode it with some other values

### missing values in k-means (k-means has 4 variables, one is NA others are non NA - eucladeia distance - x - NA == result will be NA for the part)
### all the ditances will become NA


cat("\nNumber of NA's present in each metric:\n\n")

for(i in 1:ncol(wine))
{
  cat(colnames(wine)[i],":",sum(is.na(wine[,i])),"\n")
}

wine <- na.omit(wine)

### What to do if there are NAs in the metric clustering needs to be performed on?
## remove/replace with mean? too few - remove, too much - replace with mean/mode

#### 2. outlier treatement

## k medians, k-means ++

### WHICH SHOULD BE USED WHEN?

## IQR
## U+/- 3STD
## CAPPING & FLURING OF VARIABLES - 1/5 PERCENTILE AND 95/99 PERCENTILE


find_outliers <- function(x, na.rm = TRUE, ...) {
  
  qnt <- quantile(x, probs=c(.25, .75))
  W <- 1.5 * IQR(x)
  A<-(qnt[1] - W)
  B<-(qnt[2] + W)
  
  nrow(cluster_data)
  return(c(A,B))
}


## identifying outliers
outlier <- data.frame()

for(i in 1:ncol(wine))
{
  
  qnt <- quantile(wine[, i], probs=c(0.25, 0.75))
  
  lower_value <- qnt[1] - (1.5 * IQR(wine[, i]))
  upper_value <- qnt[2] + (1.5 * IQR(wine[, i]))

  temp <- data.frame(lower_value, upper_value, i)
  names(temp) <- c('lower_value', 'upper_value', 'column_index')
  outlier <- rbind(outlier, temp)
  
}


## remove outliers
original_dataset <- wine

for(i in 1:ncol(wine))
{
  wine <- wine[which((wine[, i]>outlier[i, "lower_value"]) & 
      (wine[, i]<outlier[i, "upper_value"])),]
}


nrow(original_dataset)
nrow(wine)

#### 3. SCALING/STANDARDIZATION
## Why is scaling imp? Ecularding distance
## http://dni-institute.in/blogs/variable-standardization-and-k-means-clustering/

#x-u/sigma

scaled_wine <- scale(wine)
head(scaled_wine)

summary(scaled_wine)
## difference in clusters with scaled and non-scaled data


### NOW THE DATA IS READY FOR K-MEANS

### a drawback of k-means is that the # of clusters that need to be created needs to be
### known before you run the algorithm


## either you know you wan tto make say 5 clusters
## or you find out the ideal # of clusters your data represents

## ideal # of clutsers -- not too many clysters and good clusters

## elbow curve - tradeoff between the total (within cluster sum of squarres) for 
## the # of clusters

## bend in the elbow curve
wssplot <- function(data, nc=15, seed=100)
{
  
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc)
  {
    set.seed(seed)
    
    kms <- kmeans(data, centers=i,iter.max=1000, algorithm="Hartigan-Wong")
    algo <- "Hartigan-Wong"
    
    
    
    wss[i] <- sum(kmeans(data, centers=i,iter.max=1000,algorithm=algo)$withinss)
    
  }
  
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within Groups Sum of Squares")
  
  return(algo)
  
}

wssplot(scaled_wine)

set.seed(100)

nc <- NbClust(scaled_wine, min.nc=2, max.nc=15, method="kmeans")

barplot(table(nc$Best.n[1,]),
        xlab="Numer of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen by 26 Criteria")


###more methods?
##https://uc-r.github.io/kmeans_clustering


### now we know that 3 clusters would be best in this case

kmeans_output <- kmeans(x=scaled_wine, centers = 3, iter.max=100, nstart=5, algorithm = "Hartigan-Wong")

## each iteration of k-means would start with taking random points as centres
## the initializaio in k-means is crucial, if points which should ideally be in one clisyera re taken as centres 
## not good centres

## why 100? 

## the authors stated that K-means converges after 20-50 iterations
## in all practical situations, even on high dimensional datasets as they tested

### nstart>1 is recommended
## Hartigan-Wong is fastes of all algos
##The kmeans() function has an nstart option that attempts multiple initial configurations and reports on the best one. For example, adding nstart=25 will generate 25 initial configurations. This approach is often recommended




##The common stopping conditions I have seen:
##Convergence. (No further changes)
##Maximum number of iterations.


### UNDERSTANDING THE OUTPUT OF KMEANS

str(kmeans_output)

# The output of kmeans is a list with several bits of information. The most important being:
#   
# cluster: A vector of integers (from 1:k) indicating the cluster to which each point is allocated.
# centers: A matrix of cluster centers.
# totss: The total sum of squares (withinss +betweenss)
# withinss: Vector of within-cluster sum of squares, one component per cluster.
# tot.withinss: Total within-cluster sum of squares, i.e. sum(withinss).
# betweenss: The between-cluster sum of squares, i.e. $totss-tot.withinss$.
# size: The number of points in each cluster.
#ifault should be 0 - algorithm converded


fviz_cluster(kmeans_output, data = scaled_wine)

# It assumes prior knowledge of the data and requires the analyst to choose the appropriate number of cluster (k) in advance
# 
# The final results obtained is sensitive to the initial random selection of cluster centers. Why is it a problem? Because, for every different run of the algorithm on the same dataset, you may choose different set of initial centers. This may lead to different clustering results on different runs of the algorithm.
# It’s sensitive to outliers.
# 
# If you rearrange your data, it’s very possible that you’ll get a different solution every time you change the ordering of your data.
# 
# Possible solutions to these weaknesses, include:
#   
# Solution to issue 1: Compute k-means for a range of k values, for example by varying k between 2 and 10. Then, choose the best k by comparing the clustering results obtained for the different k values.
# 
# Solution to issue 2: Compute K-means algorithm several times with different initial cluster centers. The run with the lowest total within-cluster sum of square is selected as the final clustering solution.
# 
# To avoid distortions caused by excessive outliers, 
##it’s possible to use PAM algorithm, which is less sensitive to outliers.


## A robust alternative to k-means is PAM,
##which is based on medoids. As discussed in the next chapter, 
##the PAM clustering can be computed using the function pam() [cluster package]. 
##The function pamk( ) [fpc package] is a wrapper for PAM that also prints the suggested number
##of clusters based on optimum average silhouette width.



## CLUSTERING WITH A MIX OF CATEGORICAL AND NUMERIC
##

# k-means converting categorical data to vectors
# k-modes and heirarchical clsutering in R
