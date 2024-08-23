################################# 
# You can use this template to draft the script for your Assessment 2 of SIT718.
# More clarification and related resources can be found at
# https://d2l.deakin.edu.au/d2l/le/content/1422222/viewContent/7266544/View
#################################

#############################################################################################
# save your code as "name-code.R" (where ''name'' is replaced with your surname or first name).
#############################################################################################

getwd()
setwd("C:/Users/User/Music/RASSIGNMENT")


##################################
#Question 1 - Understand the Data
##################################

data.raw <- as.matrix(read.table("RedWine.txt"))

set.seed(224748948) # using your student ID number for reproducible sampling with the seed function

data.subset <- data.raw[sample(1:1599, 500), c(1:6)]

data.variable.names <- c("citric acid", "chlorides", "total sulfur dioxide", "pH", "alcohol", "quality")

colnames(data.subset) <- data.variable.names

data.subset.dataframe <- data.frame(data.subset)

head(data.subset.dataframe)


# Create 5 scatterplots function (for each X variable against the variable of interest Y) 
plot(data.subset.dataframe$citric.acid, data.subset.dataframe$quality, 
     xlab = "Citric Acid in Red Wine - citric acid(X1)", 
     ylab = "Quality of Wine (Y)",
     main = "Relationship between Citric Acid
     and Quality of Wine")

plot(data.subset.dataframe$chlorides, data.subset.dataframe$quality, 
     xlab = "Chlorides in Red Wine - chlorides(X2)", 
     ylab = "Quality of Wine (Y)",
     main = "Relationship between Chlorides
     and Quality of Wine")

plot(data.subset.dataframe$total.sulfur.dioxide, data.subset.dataframe$quality, 
     xlab = "Total Sulfur Dioxide in Red Wine - total sulfur dioxide(X3)", 
     ylab = "Quality of Wine (Y)",
     main = "Relationship between Total Sulfur Dioxide
     and Quality of Wine")

plot(data.subset.dataframe$pH, data.subset.dataframe$quality, 
     xlab = "PH in Red Wine - ph(X4)", 
     ylab = "Quality of Wine (Y)",
     main = "Relationship between PH
     and Quality of Wine")

plot(data.subset.dataframe$alcohol, data.subset.dataframe$quality, 
     xlab = "Alcohol in Red Wine - alcohol(X5)", 
     ylab = "Quality of Wine (Y)",
     main = "Relationship between alcohol
     and Quality of Wine")

# Create 6 histograms for each X variable and Y
hist(data.subset.dataframe$citric.acid, 
     xlab = "Citric Acid in Red Wine", 
     main = "Distribution of Citric Acid in Red Wine (X1)")

hist(data.subset.dataframe$chlorides, 
     xlab = "Chlorides in Red Wine", 
     main = "Distribution of Chlorides in Red Wine (X2)")

hist(data.subset.dataframe$total.sulfur.dioxide, 
     xlab = "Total Sulfur Dioxide in Red Wine", 
     main = "Distribution of Total Sulfur Dioxide in Red Wine (X3)")

hist(data.subset.dataframe$pH, 
     xlab = "PH in Red Wine", 
     main = "Distribution of PH in Red Wine (X4)")

hist(data.subset.dataframe$alcohol, 
     xlab = "Alcohol in Red Wine", 
     main = "Distribution of Alcohol in Red Wine (X5)")

hist(data.subset.dataframe$quality, 
     xlab = "Quality in Red Wine", 
     main = "Distribution of quality in Red Wine (Y)")

#check correlation for all variables
cor(data.subset.dataframe$citric.acid, data.subset.dataframe$quality, method = "pearson") #value: 0.2299132 -> weak positive correlation
cor(data.subset.dataframe$chlorides, data.subset.dataframe$quality, method = "pearson") #value: -0.1454155 -> weak negative correlation
cor(data.subset.dataframe$total.sulfur.dioxide, data.subset.dataframe$quality, method = "pearson") #value: -0.1848496 -> weak negative correlation
cor(data.subset.dataframe$pH, data.subset.dataframe$quality, method = "pearson") #value: 0.02081292 -> non linear correlation
cor(data.subset.dataframe$alcohol, data.subset.dataframe$quality, method = "pearson") #value: 0.497663 -> strong positive correlation

#check the mean and median for all the variables
mean(data.subset.dataframe$citric.acid)
median(data.subset.dataframe$citric.acid)
mean(data.subset.dataframe$chlorides)
median(data.subset.dataframe$chlorides)
mean(data.subset.dataframe$total.sulfur.dioxide)
median(data.subset.dataframe$total.sulfur.dioxide)
mean(data.subset.dataframe$pH)
median(data.subset.dataframe$pH)
mean(data.subset.dataframe$alcohol)
median(data.subset.dataframe$alcohol)
mean(data.subset.dataframe$quality) 
median(data.subset.dataframe$quality)


################################
#Question 2 - Transform the Data
################################

I <- c(1, 2, 3, 4, 6) # Choose any four X variables and Y

variables_for_transform <- data.subset.dataframe[,I]  # obtain a 400 by 5 matrix

head(variables_for_transform)

# for each variable, you need to figure out a good data transformation method, 
# such as Polynomial, log and negation transformation. The k-S test and Skewness 
# calculation may be helpful to select the transformation method

library(e1071)
skewness(variables_for_transform$citric.acid) #value: 0.186575 -> approximately positive skew
skewness(variables_for_transform$chlorides) #value: 4.319233 -> highly positively skewed   /////
skewness(variables_for_transform$total.sulfur.dioxide) #value: 1.515831 -> moderate positive skewness     //////
skewness(variables_for_transform$pH) #value: 0.5182693 -> positively skewed
skewness(variables_for_transform$quality)  #value: 0.210948 -> approximately positive skew

# A Min-Max and/or Z-score transformation should then be used to adjust the scale of each variable

# min-max normalisation
minmax <- function(x){
  (x - min(x))/(max(x)-min(x))
}

#transformation for citric Acid (X1) using polynomial transformation and min-max normalisation
polynomial.transformed.citricAcid <- (variables_for_transform$citric.acid)^(0.5)
minimum.citricAcidValue <- min(polynomial.transformed.citricAcid) #0
print(minimum.citricAcidValue)
maximum.citricAcidValue <- max(polynomial.transformed.citricAcid) #0.8602325
print(maximum.citricAcidValue)
transformed.citricAcid <- minmax(polynomial.transformed.citricAcid)
skewness(transformed.citricAcid) # -0.592539

#transformation for chlorides(X2) using log transformation and min-max normalisation
log.transformed.chloridesValues <- log10(variables_for_transform$chlorides)
minimum.chloridesValues <- min(log.transformed.chloridesValues) # -1.920819
print(minimum.chloridesValues)
maximum.chloridesValues <- max(log.transformed.chloridesValues) # -0.333482
print(maximum.chloridesValues)
transformed.chloridesValues <- minmax(log.transformed.chloridesValues)
skewness(transformed.chloridesValues) # 1.614434

#transformation for total.sulfur.dioxide(X3) using log transformation and min-max normalisation
log.transformed.sulfurValues <- log10(variables_for_transform$total.sulfur.dioxide)
minimum.sulfurValuesValues <- min(log.transformed.sulfurValues) # 0.7781513
print(minimum.sulfurValuesValues)
maximum.sulfurValuesValues <- max(log.transformed.sulfurValues) # 2.444045
print(maximum.sulfurValuesValues)
transformed.sulfurValues <- minmax(log.transformed.sulfurValues)
skewness(transformed.sulfurValues) # -0.09919197

#transformation for pH (X4) using polynomial transformation and min-max normalisation
polynomial.transformed.pH <- (variables_for_transform$pH)^(0.5)
minimum.pHValues <- min(polynomial.transformed.pH) # 1.702939
print(minimum.pHValues)
maximum.pHValues <- max(polynomial.transformed.pH) # 2.002498
print(maximum.pHValues)
transformed.pH <- minmax(polynomial.transformed.pH)
skewness(transformed.pH) # 0.4192547

#transformation for quality (y) using polynomial transformation and min-max normalisation
polynomial.transformed.quality <- (variables_for_transform$quality)^(0.5)
minimum.QualityValues <- min(polynomial.transformed.quality) # 1.732051
print(minimum.QualityValues) # 
maximum.sulfurQualityValues <- max(polynomial.transformed.quality) # 2.828427
print(maximum.sulfurQualityValues)
transformed.quality <- minmax(polynomial.transformed.quality)
skewness(transformed.quality) # -0.06219504

column.names <- c("Y", "X1", "X2", "X3", "X4")
data.transformed <- array(c(transformed.quality, transformed.citricAcid, transformed.chloridesValues, transformed.sulfurValues, 
                     transformed.pH), dim = c(400,5),
                   dimnames = list(NULL, column.names))


# Save this transformed data to a text file
file_path <- "sajana-transformed.txt"
write.table(data.transformed, file_path, sep = "\t")

# Check if the file was created successfully
if (file.exists(file_path)) {
  cat("File successfully created:", file_path, "\n")
} else {
  cat("Error: File not created. Please check your file path and permissions.\n")
}

##########################################
#Question 3 - Build models and investigate
##########################################

source("AggWaFit718.R")

data.transformed_copy <- as.matrix(read.table("sajana-transformed.txt"))  # import your saved data
head(data.transformed_copy)

# Get weights for Weighted Arithmetic Mean with fit.QAM() 
fit.QAM(data.transformed_copy[,c(1:5)])

# Get weights for Power Mean p=0.5 with fit.QAM()
fit.QAM(data.transformed_copy[,c(1:5)], g = PM05, g.inv = invPM05)

# Get weights for Power Mean p=2 with fit.QAM()
fit.QAM(data.transformed_copy[,c(1:5)], g = QM, g.inv = invQM)

# Get weights for Ordered Weighted Average with fit.OWA()
fit.OWA(data.transformed_copy[,c(1:5)])


#######################################
#Question 4 - Use Model for Prediction
#######################################

new_input <- c(0.9, 0.65, 38, 2.53, 7.1) 

new_input_for_transform <- new_input[c(1,2,3,4)] # choose the same four X variables as in Q2 

# transforming the four variables in the same way as in question 2 

#transformation for citric Acid (X1) using polynomial transformation and min-max normalisation
polynomial.transformed.citricAcid <- (new_input_for_transform[1])^(0.5)
minimum.citricAcidValue <- 0
maximum.citricAcidValue <- 0.8602325
transformed.citricAcid <- (polynomial.transformed.citricAcid - minimum.citricAcidValue) / (maximum.citricAcidValue - minimum.citricAcidValue)

#transformation for chlorides(X2) using log transformation and min-max normalisation
log.transformed.chloridesValues <- log10(new_input_for_transform[2])
minimum.chloridesValues <-  -1.920819
maximum.chloridesValues <-  -0.333482
transformed.chloridesValues <-  (log.transformed.chloridesValues - minimum.chloridesValues) / (maximum.chloridesValues - minimum.chloridesValues)

#transformation for total.sulfur.dioxide(X3) using log transformation and min-max normalisation
log.transformed.sulfurValues <- log10(new_input_for_transform[3])
minimum.sulfurValuesValues <- 0.7781513
maximum.sulfurValuesValues <- 2.444045
transformed.sulfurValues <- (log.transformed.sulfurValues - minimum.sulfurValuesValues) / (maximum.sulfurValuesValues - minimum.sulfurValuesValues)

#transformation for pH (X4) using polynomial transformation and min-max normalisation
polynomial.transformed.pH <- (new_input_for_transform[4])^(0.5)
minimum.pHValues <- 1.702939
maximum.pHValues <- 2.002498
transformed.pH <- (polynomial.transformed.pH - minimum.pHValues) / (maximum.pHValues - minimum.pHValues)


# applying the transformed variables to the best model selected from Q3 for Y prediction
transformed.values.for.prediction <- c(transformed.citricAcid, transformed.chloridesValues,
                                       transformed.sulfurValues, transformed.pH)

#applying to best fitting model, used Weighted Arithmetic Mean with fit.QAM() 
WPM.weights <- c(0.523644742527092, 0, 0.0107613641686777, 0.465593893304225)
predicted.transformed.quality <- QAM(transformed.values.for.prediction, WPM.weights)
predicted.transformed.quality

print(predicted.transformed.quality)

# Reverse the transformation to convert back the predicted Y to the original scale and then round it to integer
minimum.quality <- 1.732051
maximum.quality <- 2.828427
reversing.linear.scaling = (predicted.transformed.quality * (maximum.quality - minimum.quality)) + minimum.quality
reversing.polynomial.transformation <-  reversing.linear.scaling^2
predicted.value.quality <- round(reversing.polynomial.transformation)
predicted.value.quality

#############################################################################################
# References 
# Following Harvard style: https://www.deakin.edu.au/students/studying/study-support/referencing/harvard
#############################################################################################

# You must cite all the datasets and packages you used for this assessment.