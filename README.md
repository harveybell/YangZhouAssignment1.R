# YangZhouAssignment1.R
##################################################Assignment 1##################################################

###0###
firstName<- "Yang"
lastName <- "Zhou"
print(paste(firstName,lastName))
StudentID<-1505133
print(StudentID)
  

###Q1###

#a#
library(foreign)
df.dta<-read.dta("http://github.com/EconomiCurtis/econ294_2015/raw/master/data/NHIS_2007_dta.dta")
#b#
df.csv <-read.csv("http://github.com/EconomiCurtis/econ294_2015/raw/master/data/NHIS_2007_CSV.csv")
#c#
df.td <- read.table("http://github.com/EconomiCurtis/econ294_2015/raw/master/data/NHIS_2007_TSV.txt")
#d#
df.rdata <-load(url("http://github.com/EconomiCurtis/econ294_2015/raw/master/data/NHIS_2007_RData.RData"))


###Q2###

#a#
print("df.dta is 188kb, df.cvs is 139kb, df.td is 139kb and NHIS_2007_RData.RData is 45.3kb")
#b#
print("NHIS_2007_RData.RData is the smallest")
#c#
print("as different file types have different way to encode,so it causes their variability ")


###Q3###

print(I(typeof(NHIS_2007_RData)))
print(I(class(NHIS_2007_RData)))
print(I(length(NHIS_2007_RData)))
print(I(dim(NHIS_2007_RData)))
print(I(nrow(NHIS_2007_RData)))
print(I(ncol(NHIS_2007_RData)))
summary(NHIS_2007_RData)


###Q4###

d.dta<-read.dta("http://github.com/EconomiCurtis/econ294_2015/raw/master/data/org_example.dta")
str(d)
print("30 variables and 1119754 observations in the dataset")
summary(d.dta$rw)
#  Min. 1st Qu.  Median   Mean   3rd Qu.  Max.   NA's 
#  1.8    10.7    15.9    19.8    24.4   354.8  521279 
print("min is 1.8, mean is 19.8, median is 15.9, max is 354.8 ,first quartile is 10.7, third quartile is 24.4, there are 521279 NAs.")


###Q5###

v<-c(1, 2, 3, 4, 5, 6, 7, 4, NULL, NA)
length(v)
print("length of v is 9, and it does not match the number of values in the vector,It’s because “NULL” is not counted in the length of v.")
summary(v)
print("the mean ignoring the NA value is 4")


###Q6###

#a#
x <- matrix(c(1, 4, 7, 2, 5, 8, 3, 6, 9), nrow=3, ncol=3) 
t(x)# transpose matrix of x
eigen(x)# the eigenvalues and eigenvectors of x
#b#
y <- matrix(   c(1, 3, 2, 2, 2, 3, 3, 1, 0),   nrow=3,   ncol=3) 
z<-solve(y)# the inverse matrix of y
y%*%z#the new matrix is called identity matrix


###Q7###

#diamonds
carat = c(5, 2, 0.5, 1.5, 5, NA, 3) 
cut = c("fair", "good", "very good", "good", "fair", "Ideal", "fair") 
clarity = c("SI1", "I1", "VI1", "VS1", "IF", "VVS2", "NA" )
price = c(850, 450, 450, 0, 750, 980, 420)
diamonds <- data.frame(carat, cut, clarity, price)
print(diamonds)
#a#
summary(diamonds$price)
print("So the mean price is 557.1")
#b#
meanprice1<-subset(diamonds,(cut=="fair"))
summary(meanprice1$price)
print("the mean price of cut fair is 673.3")
#c#
meanprice2<-subset(diamonds,(cut=="good"|cut=="very good"|cut=="Ideal"))
summary(meanprice2$price)
print(" the mean price of cut good, very good and ideal is 470.0")
#d#
meanprice3<-subset(diamonds,(carat>2))
meanprice4<-subset(meanprice3,(cut=="very good"|cut=="Ideal"))
summary(meanprice4$price)
print("the median price is not exist")

###############################################################################################################
