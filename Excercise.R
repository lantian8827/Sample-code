foo<-read.csv("C:/Users/blue-TL/Downloads/data.csv", header=T, sep=",")

###Qeustion 1###
####number of observation####
is.vector(foo)
n1<-dim(foo)
###number of row##
number1<-n1[1]
number1
###number of row##
number2<-n1[2]
number2

###Columns type
apply(foo,2,class)
mean(1,2,2)
mean(foo[:3])

###columns Mean and Median
foo1<-data.matrix(foo)
as.matrix(foo1)
is.matrix(foo1)
mean<-apply(foo1,2, mean)
median<-apply(foo1,2,median)


###Question2###
###find the persentage of missing value###

Persentage<-apply(foo,2,function(x) sum(is.na(x))/dim(foo)[1])
Persentage

###There is no missing value in the dataset that you give me###

###I make up one dataset to replace missing value by median###
makeup<- data.frame(x = c(1, 2, NA, 3, 4, NA), y = c(NA, NA, 4, 5, 3, 2))

Persentage<-apply(makeup,2,function(x) sum(is.na(x))/dim(makeup)[1])
Persentage

###replace missing value with median###

new_makeup<-apply( makeup , 2 , function(x) { x[ is.na(x) ] = median( x , na.rm =  T ); x } ) 


#########################
######Question 3####
##remove outliers and Capping on the data##
###########################

###Set the persentage to cut##
min_pctle_cut <- 0.01 
max_pctle_cut <- 0.99 
library(outliers) 

n <- 10000 
x1 <- runif(n) 
x2 <- runif(n) 
x3 <- x1 + x2 + runif(n)/10 
x4 <- x1 + x2 + x3 + runif(n)/10 
x5 <- factor(sample(c('a','b','c'),n,replace=TRUE)) 
x6 <- factor(1*(x5=='a' | x5=='c')) 
data1 <- cbind(x1,x2,x3,x4,x5,x6) 
x <- data.frame(data1) 
qs <- sapply(x, function(zx) quantile(x, c(min_pctle_cut, max_pctle_cut), na.rm = TRUE)) 
##cut outlier##
for (i in 1:ncol(x)) x[[i]] <- pmax(qs[1,i], pmin(qs[2,i], x[[i]])) 

########################
#####Question 4#####
#########################

##Merge dataset##
data1<-read.csv("C:/Users/blue-TL/Downloads/Merge1.txt", header=T, sep="\t")
data2<-read.csv("C:/Users/blue-TL/Downloads/Merge2.txt", header=T, sep="\t")
data3<-read.csv("C:/Users/blue-TL/Downloads/Merge3.txt", header=T, sep="\t")
Mergedata<-merge(data1,data2,by=c("ID"))

##Replace "zero" with "0"##
foo$children <- as.character(foo$children)
foo$children[foo$children=="Zero"] <- "0"
###Rename a column name###
names(data3)[names(data3)=="Quota"] <- "Biding"
data3


#####################
###Question 5####
###IV value###
#######################

n<-length(foo$Credit.Card.Approval.Rating);
target<-rep(0,times=n);

for (i in 1:n) {if (foo$Credit.Card.Approval.Rating[i]=='YES') target[i]=1};

###The function to calculate IV value###

iv<-function(predit,target) 
{
  data<-data.frame(predit,target);
  data_sort<-data[order(predit),]
  
  ttl_num<-length(target);
  bin<-10;
  n<-ttl_num%/%bin;
  iv_bin<-rep(0,times=bin);
  good<-rep(0,times=bin);
  bad<-rep(0,times=bin);
  
  for (i in 1:bin) # calculate PSI for ith bin
  {
    if(i!=bin) {good[i]<-sum(data_sort$target[((i-1)*n+1):(n*i)]);bad[i]<-n-good[i]} else
    {good[i]<-sum(data_sort$target[((i-1)*n+1):ttl_num]);bad[i]<-ttl_num-n*(i-1)-good[i]}
  }
  
  good_pct<-good/sum(good)
  bad_pct<-bad/sum(bad)
  for (i in 1:bin)
  {
    iv_bin[i]<-(bad_pct[i]-good_pct[i])*log(bad_pct[i]/good_pct[i])
  }
  
  iv=sum(iv_bin)
  return (iv)
}

###get iv value###
iv(foo$status,target)

###get iv value for each variable###
ll<-sapply(foo, function(x) iv(x,target))

plot(ll)
#############################
######Question 6#########
##########################

##Run the univariate regression on each variable##
lapply(c("status", "self_employed", "Balance_Transfer", 
           "Term_Deposit", "Life_Insurance", "Medical_Insurance", "Average_A_C_Balance", 
           "Personal_Loan", "Investment_in_Mutual_Fund", "Home_Loan", "Investment_in_Commodity", 
           "Investment_in_Equity", "Investment_in_Derivatives"),  
       function(var) {    
         formula    <- as.formula(paste("Credit.Card.Approval.Rating ~", var))
         res.logist <- glm(formula, data = foo, family = binomial)
         summary(res.logist)
       })

###get the correlation value with respective each response variable###
##The data I use is below###
foo2<-read.table("C:/Users/blue-TL/Downloads/sample.txt", header=T, sep="\t")
foo2m<-as.matrix(foo2)
library(Hmisc)
rcorr(foo2m, type="pearson")

###########################
########Qestion 7####
############################

V<-c(1, 2, 34, 4, 4, 6, 79, 3, 9, 30, 18, 29, 95, 204, 45)
Vnew<-sort(V, decreasing = T)
###Return the second largest value###
Vnew[2]


##############################
#######Question 8####
#############################

function1 <- function(n){
  sum=0
  for (i in 1:n){
    sum=sum+i
  }
  return(sum)
}

function1(5)


############################
#####Question 9######
#############################

###Histogram##
hsb2 <- read.table('http://www.ats.ucla.edu/stat/r/faq/hsb2.csv', header=T, sep=",")
hist(hsb2$math, xlab="math" )

###Bar Plot for the number of children###
count<-table(foo$children)
r <- barplot(count, col = rainbow(20))
###Density Plot###
den<-density(foo$Investment_in_Commodity, cut=10 )
plot(den)


#########################
#####Question 10#####
########################

###Select only numbers from the string##

lt <- c("eju3rf+dg[g]4s667", "9fd*s8fd311(5)", "2tu,g7g2dg3a8ga5")
as.numeric(gsub("\\D", "", lt))

##split the string by some special character like "_" ##
word<-colnames(foo)

split<-sapply(word,function(x) strsplit(x, "_"))
###select the third sub-string from each cell##
##there is no third sub-string if return a NA##
is.vector(split)
sapply(split, function(x) unlist(x)[3])

library(devtools)
install_github("riv","tomasgreif")
library(woe)



