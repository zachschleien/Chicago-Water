#read in data
payment_plan <- read.csv("/Users/Zach/Sites/ist_687/City Data Raw/City Data/payment_plan.csv", check.names = FALSE, header = TRUE, sep = ",")

#part of the df to get specific rows
#show payment plan arrangement as "Yes"
payment.plan.yes.arrange <- subset(payment_plan, payment_plan$"Payment Plan Arrangement" == "Y" )

#apply function based on columns - payment plan arrangement yes and ward
#group by ward (second variable)
#use length to count the length of payment yes
yes.arrange.ward <- tapply(payment.plan.yes.arrange$"Payment Plan Arrangement", payment.plan.yes.arrange$Ward, length)

#apply function based on payment plan arrangement and ward
#group by ward
#use length to count the length of payment plan
total.arrange.ward <- tapply(payment_plan$"Payment Plan Arrangement", payment_plan$Ward, length)

#add payment arrangement yes and ward in the df
arrange.df <- data.frame(yes.arrange.ward, total.arrange.ward)

#divide payment arrangement yes by total payment * 100 
#will get % that take payment plan
arrange.df$percentage.yes <- (arrange.df$yes.arrange.ward/arrange.df$total.arrange.ward) * 100

#convert amount owed, which is a level to numeric 
payment_plan$"Amount owed" <- as.numeric(as.character(payment_plan$"Amount owed"))

#take the sum of amount owed
#group by ward
arrange.df$debt.ward <- tapply(payment_plan$"Amount owed", payment_plan$Ward, sum)

#add in debt by ward, descending in df
arrange.df.sort <- arrange.df[order(arrange.df$debt.ward, decreasing = T),]

#plot variables
plot(arrange.df.sort$debt.ward ~ arrange.df.sort$percentage.yes)

#find relationship between debt and payment plan
res <- lm(arrange.df.sort$debt.ward ~ arrange.df.sort$percentage.yes)
summary(res)

#Find SVM for payment plan and amount owed
#input column past due and output - predict the value payment arrangement

install.packages("kernlab")
library(kernlab)

#change df name so there are no spaces so the model works
names(payment_plan)[names(payment_plan) == "Payment Plan Arrangement"] <- c("Payment_Plan_Arrangement")
names(payment_plan)[names(payment_plan) == "Amount owed"] <- c("Amount_owed")

#Remove other levels
payment_plan$Payment_Plan_Arrangement[payment_plan$Payment_Plan_Arrangement == " "] <- "N"
payment_plan$Payment_Plan_Arrangement[payment_plan$Payment_Plan_Arrangement == ""] <- "N"
payment_plan$Payment_Plan_Arrangement <- factor(payment_plan$Payment_Plan_Arrangement,levels = c("Y", "N"), labels = c(1,0))

#Remove NAs in Ward values 
payment_plan <- payment_plan[!is.na(payment_plan$Ward),]

#Convert Amount owed into numeric values 
payment_plan$Amount_owed <- as.integer(payment_plan$Amount_owed)

#shows that data set has 51206 rows and 11 columns
dim(payment_plan)

#payment plan 
payment_plan$"Payment_Plan_Arrangement"

#shows yes or no payment plan
table(payment_plan$"Payment_Plan_Arrangement")

#for training-set us 2/3 of data to train

#create random data set and take 1st element of vector
randIndex <- sample(1:dim(payment_plan)[1])

#floor gets rid of decimal because an index variable needs to be an integer - training
cutPoint1_10 <- floor(1 * dim(payment_plan)[1]/10)
cutPoint1_10

#generate our test using 1/10 of data set
trainData <- payment_plan[randIndex[1:cutPoint1_10],]

#using rest of data 9/10
testData <- payment_plan[randIndex[(cutPoint1_10+1):dim(payment_plan)[1]],]


#SVM model use training data to build model
svmOutput <- ksvm(Payment_Plan_Arrangement ~ Ward + Amount_owed, data=trainData, kernel="rbfdot", 
                  kpar="automatic",C=5,cross=3,prob.model=TRUE)


#apply model to testing to help you classify payment plan arrangement, if it's accurate
svmPred <- predict(svmOutput, testData, type="votes")

#check accuracy 
#confusion matrix
compTable <- data.frame(testData[,"Payment_Plan_Arrangement"],svmPred[1,])
table(compTable)

#0 means on-time 
#1 means late payment
#out of ~10,000 customers who pay late. It got 9 correct.
#out of ~35,000 customers who pay on-time. It go
#10875 + 9 = total customer pay late - > model can predict only 9 
#35161 + 39 = total customer pay ontime _> model can predict correctly 35161 
#9 + 35161 / 10875 + 35161 + 9 + 39
#Can predict 76% of customers paying late and on-time but looking at the matrix poor
#job in predicting late paying customers

#for naive bayes
library(e1071)
library(caret)
#floor gets rid of decimal because an index variable needs to be an integer - training
cutPoint2_3 <- floor(2 * dim(payment_plan)[1]/3)
#generate our test using 1/10 of data set
trainData <- payment_plan[randIndex[1:cutPoint2_3],]

#using rest of data 
testData <- payment_plan[randIndex[(cutPoint2_3+1):dim(payment_plan)[1]],]

NBoutput1 <- naiveBayes(Payment_Plan_Arrangement ~ Amount_owed + Ward, data = trainData)

NBpredict1 <- predict(NBoutput1, testData)

compTable <- data.frame(testData[,"Payment_Plan_Arrangement"],NBpredict1)
table(compTable)

#0 means on-time 
#1 means late payment
#out of 3933 customers who pay late. It got 1 correct.
#out of 13,135 customers who pay on-time, it got 13132 customers correctly

