getwd()
file_names <- list.files(pattern = ".csv")
Flight <- read.csv(file_names)

#Creating vectors & data-frames
A <- c(1,2,3,4)
B <- c(2,4,6,8)
levels <- factor(c("X","Y","X","Y"))
Newdataframe <- data.frame(First=A, Second=B, F=levels)
Newdataframe
str(Newdataframe)
is.numeric(A)
is.factor(A)
is.factor(levels)

#Load in-built "cats" dataset
install.packages("MASS")
library("MASS")
cats <- cats

#Write cats dataframe into a CSV file
write.csv(cats, file = "RTutorialExample.csv")

#Load the cats CSV file into R
cats2 <- read.csv(file = "RTutorialExample.csv",header=TRUE)

#Explore structure and descriptive statistics summary of cats
str(cats2)
summary(cats2)
mean(cats2$Bwt)

#PART 2
#Create a new dataframe filtering for only female cats
catsfemale <- subset(cats2, Sex == "F")

#Create a new dataframe with only the numeric variables from cats2
cats2num <- cats2[,c(3,4)] #Comma takes all the rows, c() creates vector with columns 3 & 4

#Extract specific values
head(cats2num,3) #Observe the first three observations in cats2num
tail(cats2num,5) #Observe the last five observations in cats2num

#Let's look up the help for this function
?head

#Let's remove some of the outliers in Bwt
summary (cats2$Bwt) 
hist(cats2$Bwt)
quantile(cats2$Bwt, seq (.1,1,.05)) #so it looks like weight above 3.6 is outside the 95% range
table (cats2$Bwt >3.6) #so how many rows is that? 5 cats!
cats3 <- cats2 [cats2$Bwt <3.6,] #takes rows with Bwt<3.6 and all the columns

#Correlation of cats bodyweight and heart weight
cor(cats3$Bwt,cats3$Hwt)

#Let's check the visual we have now. What's different?
hist(cats3$Bwt)

#Now, let's try to predict Body weight using a simple regression of cats2
#First we need a random sample of cats2 split 80%/20%. Let's first create row markers. 
Training_ID <- sample (rownames (cats2), nrow (cats2)*.8)
Validation_ID <- rownames (cats2) [!(rownames(cats2) %in% Training_ID)] # ! enables us to take the data that are NOT in Training_ID

#Now lets use those row markers to get the stuff in those rows and create the 80/20 samples
train <- cats2 [rownames (cats2) %in% Training_ID,]
valid <- cats2 [rownames (cats2) %in% Validation_ID,]

#Now we'll build a linear regression
Reg_1 <- lm (Bwt ~ Hwt, data=train)
Reg_1
summary (Reg_1)

#To output some default plots, you can use the plot function. E.g. plot (Reg_1)

#Now let's predict what will happen on our validation set
predict_bodyweight <- predict (Reg_1, valid)

#Let's compare the accuracy for the cat #15
predict_bodyweight[15]
valid$Bwt[15]

#Multicollinearity diagnostic (requires at least 2 independent variables)
install.packages("car")
library("car")
vif(Reg_1)