#' ---
# title: Assignment 2 
# author: "Yang Zhou"
# date: "Winter 2016"
# assignment: https://github.com/EconomiCurtis/econ294_2015/blob/master/Assignments/Econ_294_Assignment_2.pdf
# ---


##############################################################################################

##Q0##

YangZhouAssignment2 <- list(
  firstName = "Yang",
  lastName  = "Zhou",
  email     = "yzhou79@ucsc.edu",
  studentID = 1505133
)


##Q1##

diamonds <- get(
  load(
    file = url(
      "https://github.com/EconomiCurtis/econ294_2015/raw/master/data/diamonds.RData"
    )
  )
)


YangZhouAssignment2$s1a <- nrow(diamonds)
print(YangZhouAssignment2$s1a)

YangZhouAssignment2$s1b <- ncol(diamonds)
print(YangZhouAssignment2$s1b)

YangZhouAssignment2$s1c <- names(diamonds)
print(YangZhouAssignment2$s1c)

YangZhouAssignment2$s1d <- summary(diamonds$price)
print(YangZhouAssignment2$s1d)


##Q2##

diamonds2<-read.table(
  file="https://github.com/EconomiCurtis/econ294_2015/raw/master/data/NHIS_2007_TSV.txt", 
  header=TRUE)

YangZhouAssignment2$s2a <- nrow(diamonds2)
print(YangZhouAssignment2$s2a)

YangZhouAssignment2$s2b <- ncol(diamonds2)
print(YangZhouAssignment2$s2b)

YangZhouAssignment2$s2c <- names(diamonds2)
print(YangZhouAssignment2$s2c)

YangZhouAssignment2$s2d <- mean(diamonds2$weight)
print(YangZhouAssignment2$s2d)

YangZhouAssignment2$s2e <- median(diamonds2$weight)
print(YangZhouAssignment2$s2e)

hist(diamonds2$weight)
table(diamonds2$weight)

diamonds2$WeightNA<-ifelse(diamonds2$weight==996|diamonds2$weight==999,NA,diamonds2$weight)
YangZhouAssignment2$s2f <- mean(diamonds2$WeightNA,na.rm=T)
print(YangZhouAssignment2$s2f)

YangZhouAssignment2$s2g <- median(diamonds2$WeightNA,na.rm=T)
print(YangZhouAssignment2$s2g)

hist(diamonds2$WeightNA)
table(diamonds2$WeightNA)

male<-subset(diamonds2,(SEX==1))
YangZhouAssignment2$s2h <-summary(male$WeightNA) 
print(YangZhouAssignment2$s2h)

female<-subset(diamonds2,(SEX==2))
YangZhouAssignment2$s2i <- summary(female$WeightNA)
print(YangZhouAssignment2$s2i)


##Q3##

vec <- c(letters,LETTERS)
length(vec)
YangZhouAssignment2$s3a<-vec[seq(2,52,2)]
print(YangZhouAssignment2$s3a)

YangZhouAssignment2$s3b<- paste(vec[c(51,1,14)], collapse="")
print(YangZhouAssignment2$s3b)

arr <- array( c(letters,LETTERS), dim = c(3,3,3))
YangZhouAssignment2$s3c<-arr[,1,2]
print(YangZhouAssignment2$s3c)

YangZhouAssignment2$s3d<-arr[2,2,]
print(YangZhouAssignment2$s3d)

arr
YangZhouAssignment2$s3e<-paste(arr[1,3,3],arr[1,1,1],arr[2,2,2], sep = "")
print(YangZhouAssignment2$s3e)

#save as RData
save(YangZhouAssignment2,
  file = "D://各种作业//winter//lab//YangZhouAssignment2.RData")

##############################################################################################
