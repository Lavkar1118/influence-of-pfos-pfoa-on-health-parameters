Code_Datawrangling_and_EDA
================
Lavanya Muthukumar
1/31/2022

``` r
#load libraries
library(tidyverse)
library(tableone)
```

``` r
#read data
DATA = read.csv("Data/Data_Renamed.csv",na="")

str(DATA)
```

    ## 'data.frame':    1993 obs. of  13 variables:
    ##  $ SNo            : int  1 2 3 4 5 6 7 8 9 10 ...
    ##  $ Sequence       : int  83736 83745 83750 83754 83762 83767 83769 83770 83774 83776 ...
    ##  $ PFOA           : num  0.9 1.4 1.2 2.8 0.5 1.7 2.3 0.5 1.2 2.9 ...
    ##  $ PFOS           : num  3.8 1.6 4.1 6.1 0.9 7 10 1.7 1.4 9.9 ...
    ##  $ Gender         : chr  "Female" "Female" "Male" "Female" ...
    ##  $ Age            : int  42 15 45 67 27 54 49 15 13 58 ...
    ##  $ Race           : chr  "Black" "White" "Other" "OtherHispanic" ...
    ##  $ Diet           : chr  "Poor" NA "VeryGood" "Good" ...
    ##  $ BMI            : num  20.3 25 24.1 43.7 38 26.3 25 18.3 19.4 22.8 ...
    ##  $ Cholestrol     : int  204 139 181 169 164 217 173 129 122 181 ...
    ##  $ Smoking        : chr  "Yes" "No" "Yes" "No" ...
    ##  $ Glycohemoglobin: num  5.6 4.8 5.5 5.7 5 6.1 6 4.8 5.6 5.4 ...
    ##  $ Sedentary      : int  540 180 300 360 600 600 420 540 600 420 ...

``` r
colnames(DATA)
```

    ##  [1] "SNo"             "Sequence"        "PFOA"            "PFOS"           
    ##  [5] "Gender"          "Age"             "Race"            "Diet"           
    ##  [9] "BMI"             "Cholestrol"      "Smoking"         "Glycohemoglobin"
    ## [13] "Sedentary"

``` r
colSums(is.na(DATA))
```

    ##             SNo        Sequence            PFOA            PFOS          Gender 
    ##               0               0               0               0               0 
    ##             Age            Race            Diet             BMI      Cholestrol 
    ##               0               0             183              16               1 
    ##         Smoking Glycohemoglobin       Sedentary 
    ##             147               2              31

``` r
#create variables 
pfoa = DATA$PFOA
pfos = DATA$PFOS
gender = DATA$Gender
age = DATA$Age
race = DATA$Race
diet = DATA$Diet
diet_ordered = ordered(diet, levels=c("Excellent", "VeryGood", "Good", "Fair", "Poor"))
bmi = DATA$BMI
cholestrol = DATA$Cholestrol
smoking = DATA$Smoking
glyco = DATA$Glycohemoglobin
sedentary = DATA$Sedentary

#image resolution specs

w = 300
h = 200
res = 300
```

Exploratory Analysis

``` r
#Frequency Distribution of demograhic variables - histograms
{
# Age 
jpeg(file="Output_Images/age_distribution.jpg", width = w, height = h, units='mm', res = res)
hist(age, main = " ",  xlab =  "Age(years)", ylab = "Frequency", col = "#bcbddc")
dev.off()

# Gender
jpeg(file="Output_Images/gender_distribution.jpg", width = w, height = h, units='mm', res = res)
boxplot(age ~ gender, xlab = "Gender", ylab = "Age", col = c("#deebf7", "#fee0d2"))
dev.off()

# Race
jpeg(file="Output_Images/race_distribution.jpg", width = w, height = h, units='mm', res = res)
barplot(prop.table(table(race)), xlab = "Race", ylab = "Proportion", col="#99d8c9")
dev.off()

# Smoking
jpeg(file="Output_Images/smoking_distribution.jpg", width = w, height = h, units='mm', res = res)
barplot(prop.table(table(smoking)),ylim = c(0,1), xlab = "Smoking status", ylab = "Proportion", col="#bdbdbd")
dev.off()

# Diet
jpeg(file="Output_Images/diet_distribution.jpg", width = w, height = h, units='mm', res = res)
barplot(prop.table(table(diet_ordered)), xlab= "Diet Quality", ylab= "Proportion", col="#ffeda0")
dev.off()

# Sedentary 
jpeg(file="Output_Images/sedantary_distribution.jpg", width = w, height = h, units='mm', res = res)
hist((sedentary), col="#fc9272", main="", ylab= "Frequency", xlab = "Minutes of sedentary activity per day")
dev.off()
}
```

    ## png 
    ##   2

``` r
#Boxplots

{
# Gender
jpeg(file="Output_Images/pfoa_across_categories.jpg", width = w, height = h, units='mm', res = res)
boxplot(pfoa~gender, at = c(1,2), xlim = c(0,18), xlab = "", ylab = "PFOA (ng/ml)", col= "#feb24c", cex.axis = 0.75)
boxplot(pfoa~race, at = c(4:9), xlab = "", ylab = "PFOA (ng/ml)", col= "#99d8c9", add = TRUE, cex.axis = 0.75, names = c("Asian", "Black", "\nMex\nAmer", "Other", "\nOther\nHisp", "White"))
boxplot(pfoa~diet_ordered, at = c(11:15), xlab = "", ylab = "PFOA (ng/ml)", col= "#bcbddc", add = TRUE, cex.axis = 0.75, names = c("Excel", "V.Good", "Good", "Fair", "Poor"))
boxplot(pfoa~smoking, at = c(17,18), xlim = c(0,20), xlab = "", ylab = "PFOA (ng/ml)", col= "#fa9fb5", cex.axis = 0.75, add = TRUE)
abline(h=median(pfoa))
dev.off()

jpeg(file="Output_Images/pfos_across_categories.jpg", width = w, height = h, units='mm', res = res)
boxplot(pfos~gender, at = c(1,2), xlim = c(0,18), xlab = "", ylab = "PFOS (ng/ml)", col= "#feb24c", cex.axis = 0.75)
boxplot(pfos~race, at = c(4:9), xlab = "", ylab = "PFOS (ng/ml)", col= "#99d8c9", add = TRUE, cex.axis = 0.75, names = c("Asian", "Black", "\nMex\nAmer", "Other", "\nOther\nHisp", "White"))
boxplot(pfos~diet_ordered, at = c(11:15), xlab = "", ylab = "PFOS (ng/ml)", col= "#bcbddc", add = TRUE, cex.axis = 0.75, names = c("Excel", "V.Good", "Good", "Fair", "Poor"))
boxplot(pfos~smoking, at = c(17,18), xlim = c(0,20), xlab = "", ylab = "PFOS (ng/ml)", col= "#fa9fb5", cex.axis = 0.75, add = TRUE)
abline(h=median(pfos))
dev.off()
}
```

    ## png 
    ##   2

``` r
#Scatter plots - Overall (with outliers)

{
# PFOA
# cholesterol
jpeg(file="Output_Images/cholestrol_pfoa_plot.jpg", width = w, height = h, units='mm', res = res)
plot(pfoa, cholestrol, xlab = "PFOA", ylab = "Cholestrol mg/Dl", pch = 19, col= "#99d8c9")
legend("topright",legend = paste("R = ", round(cor(cholestrol, pfoa, use = "complete.obs"),4)))
dev.off()

# bmi
jpeg(file="Output_Images/bmi_pfoa_plot.jpg", width = w, height = h, units='mm', res = res)
plot(pfoa, bmi, xlab = "PFOA", ylab = "BMI", pch = 19, col= "#feb24c")
legend("topright",legend = paste("R = ", round(cor(bmi, pfoa, use = "complete.obs"),4)))
dev.off()

# glyco
jpeg(file="Output_Images/glyco_pfoa_plot.jpg", width = w, height = h, units='mm', res = res)
plot(pfoa, glyco, xlab = "PFOA", ylab = "Glycohemoglobin %", pch = 19, col= "#fa9fb5")
legend("topright",legend = paste("R = ", round(cor(glyco, pfoa, use = "complete.obs"),4)))
dev.off()

# PFOS
# cholestrol
jpeg(file="Output_Images/cholestrol_pfos_plot.jpg", width = w, height = h, units='mm', res = res)
plot(pfos, cholestrol, xlab = "PFOS", ylab = "Cholestrol mg/Dl", pch = 19, col= "#99d8c9")
legend("topright",legend = paste("R = ", round(cor(cholestrol, pfos, use = "complete.obs"),4)))
dev.off()

# bmi
jpeg(file="Output_Images/bmi_pfos_plot.jpg", width = w, height = h, units='mm', res = res)
plot(pfos, bmi, xlab = "PFOS", ylab = "BMI", pch = 19, col= "#feb24c")
legend("topright",legend = paste("R = ", round(cor(bmi, pfos, use = "complete.obs"),4)))
dev.off()

# glyco
jpeg(file="Output_Images/glyco_pfos_plot.jpg", width = w, height = h, units='mm', res = res)
plot(pfos, glyco, xlab = "PFOS", ylab = "Glycohemoglobin %", pch = 19, col= "#fa9fb5")
legend("topright",legend = paste("R = ", round(cor(glyco, pfos, use = "complete.obs"),4)))
dev.off()
}
```

    ## png 
    ##   2

``` r
# Correlation plots - Overall (with outliers)
{
# PFOA
# cholestrol
jpeg(file="Output_Images/cholestrol_no_outliers_pfoa_plot.jpg", width = w, height = h, units='mm', res = res)
plot(pfoa[which(cholestrol<350)], cholestrol[which(cholestrol<350)], xlab = "PFOA", ylab = "Cholestrol mg/Dl", pch = 19, col= "#99d8c9")
legend("topright",legend = paste("R = ", round(cor(pfoa[which(cholestrol<350)], cholestrol[which(cholestrol<350)], use= "complete.obs"),4)))
dev.off()

# bmi
jpeg(file="Output_Images/bmi_no_outliers_pfoa_plot.jpg", width = w, height = h, units='mm', res = res)
plot(pfoa[which(bmi<48)], bmi[which(bmi<48)], xlab = "PFOA", ylab = "BMI", pch = 19, col= "#feb24c")
legend("topright",legend = paste("R = ", round(cor(pfoa[which(bmi<48)], bmi[which(bmi<48)], use = "complete.obs"),4)))
dev.off()

# glyco
jpeg(file="Output_Images/glyco_no_outliers_pfoa_plot.jpg", width = w, height = h, units='mm', res = res)
plot(pfoa[which(glyco<6.5)], glyco[which(glyco<6.5)], xlab = "PFOA", ylab = "Glycohemoglobin %", pch = 19, col= "#fa9fb5")
legend("topright",legend = paste("R = ", round(cor(pfoa[which(glyco<6.5)], glyco[which(glyco<6.5)], use = "complete.obs"),4)))
dev.off()

# PFOS
# cholestrol
jpeg(file="Output_Images/cholestrol_no_outliers_pfos_plot.jpg", width = w, height = h, units='mm', res = res)
plot(pfos[which(cholestrol<350)], cholestrol[which(cholestrol<350)], xlab = "PFOS", ylab = "Cholestrol mg/Dl", pch = 19, col= "#99d8c9")
legend("topright",legend = paste("R = ", round(cor(pfos[which(cholestrol<350)], cholestrol[which(cholestrol<350)], use= "complete.obs"),4)))
dev.off()

# bmi
jpeg(file="Output_Images/bmi_no_outliers_pfos_plot.jpg", width = w, height = h, units='mm', res = res)
plot(pfos[which(bmi<48)], bmi[which(bmi<48)], xlab = "PFOS", ylab = "BMI", pch = 19, col= "#feb24c")
legend("topright",legend = paste("R = ", round(cor(pfos[which(bmi<48)], bmi[which(bmi<48)], use = "complete.obs"),4)))
dev.off()

# glyco
jpeg(file="Output_Images/glyco_no_outliers_pfos_plot.jpg", width = w, height = h, units='mm', res = res)
plot(pfos[which(glyco<6.5)], glyco[which(glyco<6.5)], xlab = "PFOS", ylab = "Glycohemoglobin %", pch = 19, col= "#fa9fb5")
legend("topright",legend = paste("R = ", round(cor(pfos[which(glyco<6.5)], glyco[which(glyco<6.5)], use = "complete.obs"),4)))
dev.off()
}
```

    ## png 
    ##   2

``` r
#Correlation Matrix - To access confounders

# PFOA
# PFOA vs Cholesterol
corr_pfoa_cholestrol_overall = round(cor(cholestrol, pfoa, use = "complete.obs"),4)

corr_pfoa_cholestrol_male = round(cor(cholestrol[which(gender=='Male')], pfoa[which(gender=='Male')], use = "complete.obs"),4)

corr_pfoa_cholestrol_female = round(cor(cholestrol[which(gender=='Female')], pfoa[which(gender=='Female')], use = "complete.obs"),4)

corr_pfoa_cholestrol_smoker = round(cor(cholestrol[which(smoking=='Yes')], pfoa[which(smoking=='Yes')], use = "complete.obs"),4)
corr_pfoa_cholestrol_nonsmoker = round(cor(cholestrol[which(smoking=='No')], pfoa[which(smoking=='No')], use = "complete.obs"),4)

corr_pfoa_cholestrol_poor = round(cor(cholestrol[which(diet=='Poor')], pfoa[which(diet=='Poor')], use = "complete.obs"),4)
corr_pfoa_cholestrol_good = round(cor(cholestrol[which(diet=='Good')], pfoa[which(diet=='Good')], use = "complete.obs"),4)
corr_pfoa_cholestrol_excellent = round(cor(cholestrol[which(diet=='Excellent')], pfoa[which(diet=='Excellent')], use = "complete.obs"),4)

corr_pfoa_cholestrol_Asian = round(cor(cholestrol[which(race=='Asian')], pfoa[which(race=='Asian')], use = "complete.obs"),4)
corr_pfoa_cholestrol_Black = round(cor(cholestrol[which(race=='Black')], pfoa[which(race=='Black')], use = "complete.obs"),4)
corr_pfoa_cholestrol_White = round(cor(cholestrol[which(race=='White')], pfoa[which(race=='White')], use = "complete.obs"),4)

corr_pfoa_cholestrol = c(corr_pfoa_cholestrol_overall, 
                         corr_pfoa_cholestrol_male, corr_pfoa_cholestrol_female,
                         corr_pfoa_cholestrol_poor, corr_pfoa_cholestrol_good, corr_pfoa_cholestrol_excellent,
                         corr_pfoa_cholestrol_smoker, corr_pfoa_cholestrol_nonsmoker,
                         corr_pfoa_cholestrol_White, corr_pfoa_cholestrol_Black, corr_pfoa_cholestrol_Asian)

# PFOA vs Glyco
corr_pfoa_glyco_overall = round(cor(glyco, pfoa, use = "complete.obs"),4)

corr_pfoa_glyco_male = round(cor(glyco[which(gender=='Male')], pfoa[which(gender=='Male')], use = "complete.obs"),4)
corr_pfoa_glyco_female = round(cor(glyco[which(gender=='Female')], pfoa[which(gender=='Female')], use = "complete.obs"),4)

corr_pfoa_glyco_smoker = round(cor(glyco[which(smoking=='Yes')], pfoa[which(smoking=='Yes')], use = "complete.obs"),4)
corr_pfoa_glyco_nonsmoker = round(cor(glyco[which(smoking=='No')], pfoa[which(smoking=='No')], use = "complete.obs"),4)

corr_pfoa_glyco_poor = round(cor(glyco[which(diet=='Poor')], pfoa[which(diet=='Poor')], use = "complete.obs"),4)
corr_pfoa_glyco_good = round(cor(glyco[which(diet=='Good')], pfoa[which(diet=='Good')], use = "complete.obs"),4)
corr_pfoa_glyco_excellent = round(cor(glyco[which(diet=='Excellent')], pfoa[which(diet=='Excellent')], use = "complete.obs"),4)

corr_pfoa_glyco_Asian = round(cor(glyco[which(race=='Asian')], pfoa[which(race=='Asian')], use = "complete.obs"),4)
corr_pfoa_glyco_Black = round(cor(glyco[which(race=='Black')], pfoa[which(race=='Black')], use = "complete.obs"),4)
corr_pfoa_glyco_MexicanAmerican = round(cor(glyco[which(race=='MexicanAmerican')], pfoa[which(race=='MexicanAmerican')], use = "complete.obs"),4)
corr_pfoa_glyco_Other = round(cor(glyco[which(race=='Other')], pfoa[which(race=='Other')], use = "complete.obs"),4)
corr_pfoa_glyco_OtherHispanic = round(cor(glyco[which(race=='OtherHispanic')], pfoa[which(race=='OtherHispanic')], use = "complete.obs"),4)
corr_pfoa_glyco_White = round(cor(glyco[which(race=='White')], pfoa[which(race=='White')], use = "complete.obs"),4)

corr_pfoa_glyco = c(corr_pfoa_glyco_overall, 
                    corr_pfoa_glyco_male, corr_pfoa_glyco_female,
                    corr_pfoa_glyco_poor, corr_pfoa_glyco_good, corr_pfoa_glyco_excellent,
                    corr_pfoa_glyco_smoker, corr_pfoa_glyco_nonsmoker,
                    corr_pfoa_glyco_White, corr_pfoa_glyco_Black, corr_pfoa_glyco_Asian)

# PFOA vs bmi
corr_pfoa_bmi_overall = round(cor(bmi, pfoa, use = "complete.obs"),4)

corr_pfoa_bmi_male = round(cor(bmi[which(gender=='Male')], pfoa[which(gender=='Male')], use = "complete.obs"),4)
corr_pfoa_bmi_female = round(cor(bmi[which(gender=='Female')], pfoa[which(gender=='Female')], use = "complete.obs"),4)

corr_pfoa_bmi_smoker = round(cor(bmi[which(smoking=='Yes')], pfoa[which(smoking=='Yes')], use = "complete.obs"),4)
corr_pfoa_bmi_nonsmoker = round(cor(bmi[which(smoking=='No')], pfoa[which(smoking=='No')], use = "complete.obs"),4)

corr_pfoa_bmi_poor = round(cor(bmi[which(diet=='Poor')], pfoa[which(diet=='Poor')], use = "complete.obs"),4)
corr_pfoa_bmi_good = round(cor(bmi[which(diet=='Good')], pfoa[which(diet=='Good')], use = "complete.obs"),4)
corr_pfoa_bmi_excellent = round(cor(bmi[which(diet=='Excellent')], pfoa[which(diet=='Excellent')], use = "complete.obs"),4)

corr_pfoa_bmi_Asian = round(cor(bmi[which(race=='Asian')], pfoa[which(race=='Asian')], use = "complete.obs"),4)
corr_pfoa_bmi_Black = round(cor(bmi[which(race=='Black')], pfoa[which(race=='Black')], use = "complete.obs"),4)
corr_pfoa_bmi_MexicanAmerican = round(cor(bmi[which(race=='MexicanAmerican')], pfoa[which(race=='MexicanAmerican')], use = "complete.obs"),4)
corr_pfoa_bmi_Other = round(cor(bmi[which(race=='Other')], pfoa[which(race=='Other')], use = "complete.obs"),4)
corr_pfoa_bmi_OtherHispanic = round(cor(bmi[which(race=='OtherHispanic')], pfoa[which(race=='OtherHispanic')], use = "complete.obs"),4)
corr_pfoa_bmi_White = round(cor(bmi[which(race=='White')], pfoa[which(race=='White')], use = "complete.obs"),4)

corr_pfoa_bmi = c(corr_pfoa_bmi_overall, 
                  corr_pfoa_bmi_male, corr_pfoa_bmi_female,
                  corr_pfoa_bmi_poor, corr_pfoa_bmi_good, corr_pfoa_bmi_excellent,
                  corr_pfoa_bmi_smoker, corr_pfoa_bmi_nonsmoker,
                  corr_pfoa_bmi_White, corr_pfoa_bmi_Black, corr_pfoa_bmi_Asian)

# PFOS
# PFOS vs Cholestrol

{
corr_pfos_cholestrol_overall = round(cor(cholestrol, pfos, use = "complete.obs"),4)

corr_pfos_cholestrol_male = round(cor(cholestrol[which(gender=='Male')], pfos[which(gender=='Male')], use = "complete.obs"),4)
corr_pfos_cholestrol_female = round(cor(cholestrol[which(gender=='Female')], pfos[which(gender=='Female')], use = "complete.obs"),4)

corr_pfos_cholestrol_smoker = round(cor(cholestrol[which(smoking=='Yes')], pfos[which(smoking=='Yes')], use = "complete.obs"),4)
corr_pfos_cholestrol_nonsmoker = round(cor(cholestrol[which(smoking=='No')], pfos[which(smoking=='No')], use = "complete.obs"),4)

corr_pfos_cholestrol_poor = round(cor(cholestrol[which(diet=='Poor')], pfos[which(diet=='Poor')], use = "complete.obs"),4)
corr_pfos_cholestrol_good = round(cor(cholestrol[which(diet=='Good')], pfos[which(diet=='Good')], use = "complete.obs"),4)
corr_pfos_cholestrol_excellent = round(cor(cholestrol[which(diet=='Excellent')], pfos[which(diet=='Excellent')], use = "complete.obs"),4)

corr_pfos_cholestrol_Asian = round(cor(cholestrol[which(race=='Asian')], pfos[which(race=='Asian')], use = "complete.obs"),4)
corr_pfos_cholestrol_Black = round(cor(cholestrol[which(race=='Black')], pfos[which(race=='Black')], use = "complete.obs"),4)
corr_pfos_cholestrol_White = round(cor(cholestrol[which(race=='White')], pfos[which(race=='White')], use = "complete.obs"),4)

corr_pfos_cholestrol = c(corr_pfos_cholestrol_overall, 
                         corr_pfos_cholestrol_male, corr_pfos_cholestrol_female,
                         corr_pfos_cholestrol_poor, corr_pfos_cholestrol_good, corr_pfos_cholestrol_excellent,
                         corr_pfos_cholestrol_smoker, corr_pfos_cholestrol_nonsmoker,
                         corr_pfos_cholestrol_White, corr_pfos_cholestrol_Black, corr_pfos_cholestrol_Asian)

# PFOS vs Glyco
corr_pfos_glyco_overall = round(cor(glyco, pfos, use = "complete.obs"),4)

corr_pfos_glyco_male = round(cor(glyco[which(gender=='Male')], pfos[which(gender=='Male')], use = "complete.obs"),4)
corr_pfos_glyco_female = round(cor(glyco[which(gender=='Female')], pfos[which(gender=='Female')], use = "complete.obs"),4)

corr_pfos_glyco_smoker = round(cor(glyco[which(smoking=='Yes')], pfos[which(smoking=='Yes')], use = "complete.obs"),4)
corr_pfos_glyco_nonsmoker = round(cor(glyco[which(smoking=='No')], pfos[which(smoking=='No')], use = "complete.obs"),4)

corr_pfos_glyco_poor = round(cor(glyco[which(diet=='Poor')], pfos[which(diet=='Poor')], use = "complete.obs"),4)
corr_pfos_glyco_good = round(cor(glyco[which(diet=='Good')], pfos[which(diet=='Good')], use = "complete.obs"),4)
corr_pfos_glyco_excellent = round(cor(glyco[which(diet=='Excellent')], pfos[which(diet=='Excellent')], use = "complete.obs"),4)

corr_pfos_glyco_Asian = round(cor(glyco[which(race=='Asian')], pfos[which(race=='Asian')], use = "complete.obs"),4)
corr_pfos_glyco_Black = round(cor(glyco[which(race=='Black')], pfos[which(race=='Black')], use = "complete.obs"),4)
corr_pfos_glyco_MexicanAmerican = round(cor(glyco[which(race=='MexicanAmerican')], pfos[which(race=='MexicanAmerican')], use = "complete.obs"),4)
corr_pfos_glyco_Other = round(cor(glyco[which(race=='Other')], pfos[which(race=='Other')], use = "complete.obs"),4)
corr_pfos_glyco_OtherHispanic = round(cor(glyco[which(race=='OtherHispanic')], pfos[which(race=='OtherHispanic')], use = "complete.obs"),4)
corr_pfos_glyco_White = round(cor(glyco[which(race=='White')], pfos[which(race=='White')], use = "complete.obs"),4)

corr_pfos_glyco = c(corr_pfos_glyco_overall, 
                    corr_pfos_glyco_male, corr_pfos_glyco_female,
                    corr_pfos_glyco_poor, corr_pfos_glyco_good, corr_pfos_glyco_excellent,
                    corr_pfos_glyco_smoker, corr_pfos_glyco_nonsmoker,
                    corr_pfos_glyco_White, corr_pfos_glyco_Black, corr_pfos_glyco_Asian)

# PFOS vs bmi
corr_pfos_bmi_overall = round(cor(bmi, pfos, use = "complete.obs"),4)

corr_pfos_bmi_male = round(cor(bmi[which(gender=='Male')], pfos[which(gender=='Male')], use = "complete.obs"),4)
corr_pfos_bmi_female = round(cor(bmi[which(gender=='Female')], pfos[which(gender=='Female')], use = "complete.obs"),4)

corr_pfos_bmi_smoker = round(cor(bmi[which(smoking=='Yes')], pfos[which(smoking=='Yes')], use = "complete.obs"),4)
corr_pfos_bmi_nonsmoker = round(cor(bmi[which(smoking=='No')], pfos[which(smoking=='No')], use = "complete.obs"),4)

corr_pfos_bmi_poor = round(cor(bmi[which(diet=='Poor')], pfos[which(diet=='Poor')], use = "complete.obs"),4)
corr_pfos_bmi_good = round(cor(bmi[which(diet=='Good')], pfos[which(diet=='Good')], use = "complete.obs"),4)
corr_pfos_bmi_excellent = round(cor(bmi[which(diet=='Excellent')], pfos[which(diet=='Excellent')], use = "complete.obs"),4)

corr_pfos_bmi_Asian = round(cor(bmi[which(race=='Asian')], pfos[which(race=='Asian')], use = "complete.obs"),4)
corr_pfos_bmi_Black = round(cor(bmi[which(race=='Black')], pfos[which(race=='Black')], use = "complete.obs"),4)
corr_pfos_bmi_MexicanAmerican = round(cor(bmi[which(race=='MexicanAmerican')], pfos[which(race=='MexicanAmerican')], use = "complete.obs"),4)
corr_pfos_bmi_Other = round(cor(bmi[which(race=='Other')], pfos[which(race=='Other')], use = "complete.obs"),4)
corr_pfos_bmi_OtherHispanic = round(cor(bmi[which(race=='OtherHispanic')], pfos[which(race=='OtherHispanic')], use = "complete.obs"),4)
corr_pfos_bmi_White = round(cor(bmi[which(race=='White')], pfos[which(race=='White')], use = "complete.obs"),4)

corr_pfos_bmi = c(corr_pfos_bmi_overall, 
                  corr_pfos_bmi_male, corr_pfos_bmi_female,
                  corr_pfos_bmi_poor, corr_pfos_bmi_good, corr_pfos_bmi_excellent,
                  corr_pfos_bmi_smoker, corr_pfos_bmi_nonsmoker,
                  corr_pfos_bmi_White, corr_pfos_bmi_Black, corr_pfos_bmi_Asian)

dataframe = c(corr_pfoa_cholestrol, corr_pfoa_glyco, corr_pfoa_bmi, corr_pfos_cholestrol, corr_pfos_glyco, corr_pfos_bmi )                         
write.csv(dataframe, file="Correlation_Matrix_Raw.csv")

}
```

``` r
#Frequency Distribution of dependent and independent variables - histograms

#hist BMI
jpeg(file="Output_Images/bmi.jpg", width = w, height = h, units='mm', res = res)
hist(bmi, main= " ", xlab= "BMI", col= "#1c9099")
dev.off()
```

    ## png 
    ##   2

``` r
#hist cholestrol
jpeg(file="Output_Images/cholestrol_distribution.jpg", width = w, height = h, units='mm', res = res)
hist(cholestrol, main= " ", xlab= "Cholestrol mg/dL", col= "#a6bddb")
dev.off()
```

    ## png 
    ##   2

``` r
#hist glycohemoglobin
jpeg(file="Output_Images/glycohemoglobin_distribution.jpg", width = w, height = h, units='mm', res = res)
hist(glyco, main= " ", xlab= "Glycohemoglobin %", col= "#f7fcb9")
dev.off()
```

    ## png 
    ##   2

Creating Tableones with summary statistics

``` r
# create columns that contains pfoa/pfoas's quantile ranks

Quantile_rank = ntile(pfoa, 4)
Quantile_rank_2 = ntile(pfos,4)

# rename value
Quantile_rank[Quantile_rank == 1] = "1st quartile PFOA"
Quantile_rank[Quantile_rank == 2] = "2nd quartile PFOA"
Quantile_rank[Quantile_rank == 3] = "3rd quartile PFOA"
Quantile_rank[Quantile_rank == 4] = "4th quartile PFOA"

Quantile_rank_2[Quantile_rank_2 == 1] = "1st quartile PFOS"
Quantile_rank_2[Quantile_rank_2 == 2] = "2nd quartile PFOS"
Quantile_rank_2[Quantile_rank_2 == 3] = "3rd quartile PFOS"
Quantile_rank_2[Quantile_rank_2 == 4] = "4th quartile PFOS"

# add into dataframe
DATA = mutate(DATA, Quantile_rank)
DATA = mutate(DATA, Quantile_rank_2)

# create table one
table_vars = c("Gender", "Age", "Race", "Diet", "Smoking", "Sedentary","BMI", "Cholestrol", "Glycohemoglobin")
table_strat_1 = c("Quantile_rank")

tab1 = CreateTableOne(data = DATA, 
                      vars = table_vars,
                      strata = table_strat_1)


print(tab1)
```

    ##                              Stratified by Quantile_rank
    ##                               1st quartile PFOA 2nd quartile PFOA
    ##   n                              499               498           
    ##   Gender = Male (%)              128 (25.7)        256 (51.4)    
    ##   Age (mean (SD))              35.99 (18.48)     39.12 (19.96)   
    ##   Race (%)                                                       
    ##      Asian                        45 ( 9.0)         47 ( 9.4)    
    ##      Black                       129 (25.9)        103 (20.7)    
    ##      MexicanAmerican             142 (28.5)        102 (20.5)    
    ##      Other                        16 ( 3.2)         28 ( 5.6)    
    ##      OtherHispanic                65 (13.0)         56 (11.2)    
    ##      White                       102 (20.4)        162 (32.5)    
    ##   Diet (%)                                                       
    ##      Excellent                    26 ( 6.1)         27 ( 6.2)    
    ##      Fair                        137 (31.9)        116 (26.8)    
    ##      Good                        165 (38.5)        172 (39.7)    
    ##      Poor                         43 (10.0)         31 ( 7.2)    
    ##      VeryGood                     58 (13.5)         87 (20.1)    
    ##   Smoking = Yes (%)               76 (16.5)        103 (22.2)    
    ##   Sedentary (mean (SD))       382.59 (205.67)   394.75 (195.63)  
    ##   BMI (mean (SD))              29.44 (7.81)      28.92 (7.56)    
    ##   Cholestrol (mean (SD))      176.35 (43.09)    178.24 (38.64)   
    ##   Glycohemoglobin (mean (SD))   5.76 (1.29)       5.76 (1.26)    
    ##                              Stratified by Quantile_rank
    ##                               3rd quartile PFOA 4th quartile PFOA p      test
    ##   n                              498               498                       
    ##   Gender = Male (%)              287 (57.6)        293 (58.8)     <0.001     
    ##   Age (mean (SD))              45.69 (20.47)     52.67 (19.21)    <0.001     
    ##   Race (%)                                                        <0.001     
    ##      Asian                        64 (12.9)         64 (12.9)                
    ##      Black                       101 (20.3)        106 (21.3)                
    ##      MexicanAmerican              81 (16.3)         45 ( 9.0)                
    ##      Other                        23 ( 4.6)         19 ( 3.8)                
    ##      OtherHispanic                66 (13.3)         72 (14.5)                
    ##      White                       163 (32.7)        192 (38.6)                
    ##   Diet (%)                                                        <0.001     
    ##      Excellent                    36 ( 7.8)         54 (11.2)                
    ##      Fair                        122 (26.3)         96 (19.8)                
    ##      Good                        183 (39.4)        206 (42.6)                
    ##      Poor                         29 ( 6.2)         34 ( 7.0)                
    ##      VeryGood                     94 (20.3)         94 (19.4)                
    ##   Smoking = Yes (%)              103 (22.2)         90 (19.7)      0.097     
    ##   Sedentary (mean (SD))       400.78 (201.08)   371.24 (190.46)    0.091     
    ##   BMI (mean (SD))              28.36 (6.92)      28.70 (6.19)      0.111     
    ##   Cholestrol (mean (SD))      190.34 (42.94)    191.87 (40.75)    <0.001     
    ##   Glycohemoglobin (mean (SD))   5.76 (1.17)       5.76 (0.91)      1.000

``` r
# Exporting to excel
table1save = print(tab1, test = FALSE)
```

    ##                              Stratified by Quantile_rank
    ##                               1st quartile PFOA 2nd quartile PFOA
    ##   n                              499               498           
    ##   Gender = Male (%)              128 (25.7)        256 (51.4)    
    ##   Age (mean (SD))              35.99 (18.48)     39.12 (19.96)   
    ##   Race (%)                                                       
    ##      Asian                        45 ( 9.0)         47 ( 9.4)    
    ##      Black                       129 (25.9)        103 (20.7)    
    ##      MexicanAmerican             142 (28.5)        102 (20.5)    
    ##      Other                        16 ( 3.2)         28 ( 5.6)    
    ##      OtherHispanic                65 (13.0)         56 (11.2)    
    ##      White                       102 (20.4)        162 (32.5)    
    ##   Diet (%)                                                       
    ##      Excellent                    26 ( 6.1)         27 ( 6.2)    
    ##      Fair                        137 (31.9)        116 (26.8)    
    ##      Good                        165 (38.5)        172 (39.7)    
    ##      Poor                         43 (10.0)         31 ( 7.2)    
    ##      VeryGood                     58 (13.5)         87 (20.1)    
    ##   Smoking = Yes (%)               76 (16.5)        103 (22.2)    
    ##   Sedentary (mean (SD))       382.59 (205.67)   394.75 (195.63)  
    ##   BMI (mean (SD))              29.44 (7.81)      28.92 (7.56)    
    ##   Cholestrol (mean (SD))      176.35 (43.09)    178.24 (38.64)   
    ##   Glycohemoglobin (mean (SD))   5.76 (1.29)       5.76 (1.26)    
    ##                              Stratified by Quantile_rank
    ##                               3rd quartile PFOA 4th quartile PFOA
    ##   n                              498               498           
    ##   Gender = Male (%)              287 (57.6)        293 (58.8)    
    ##   Age (mean (SD))              45.69 (20.47)     52.67 (19.21)   
    ##   Race (%)                                                       
    ##      Asian                        64 (12.9)         64 (12.9)    
    ##      Black                       101 (20.3)        106 (21.3)    
    ##      MexicanAmerican              81 (16.3)         45 ( 9.0)    
    ##      Other                        23 ( 4.6)         19 ( 3.8)    
    ##      OtherHispanic                66 (13.3)         72 (14.5)    
    ##      White                       163 (32.7)        192 (38.6)    
    ##   Diet (%)                                                       
    ##      Excellent                    36 ( 7.8)         54 (11.2)    
    ##      Fair                        122 (26.3)         96 (19.8)    
    ##      Good                        183 (39.4)        206 (42.6)    
    ##      Poor                         29 ( 6.2)         34 ( 7.0)    
    ##      VeryGood                     94 (20.3)         94 (19.4)    
    ##   Smoking = Yes (%)              103 (22.2)         90 (19.7)    
    ##   Sedentary (mean (SD))       400.78 (201.08)   371.24 (190.46)  
    ##   BMI (mean (SD))              28.36 (6.92)      28.70 (6.19)    
    ##   Cholestrol (mean (SD))      190.34 (42.94)    191.87 (40.75)   
    ##   Glycohemoglobin (mean (SD))   5.76 (1.17)       5.76 (0.91)

``` r
write.csv(table1save, file = "Output/Lab2_Table1.csv")



table_strat_2 = c("Quantile_rank_2")

tab2 = CreateTableOne(data = DATA, 
                      vars = table_vars,
                      strata = table_strat_2)


print(tab2)
```

    ##                              Stratified by Quantile_rank_2
    ##                               1st quartile PFOS 2nd quartile PFOS
    ##   n                              499               498           
    ##   Gender = Male (%)              148 (29.7)        222 (44.6)    
    ##   Age (mean (SD))              32.79 (17.08)     36.95 (18.71)   
    ##   Race (%)                                                       
    ##      Asian                        41 ( 8.2)         55 (11.0)    
    ##      Black                       100 (20.0)         98 (19.7)    
    ##      MexicanAmerican             135 (27.1)        103 (20.7)    
    ##      Other                        19 ( 3.8)         23 ( 4.6)    
    ##      OtherHispanic                78 (15.6)         55 (11.0)    
    ##      White                       126 (25.3)        164 (32.9)    
    ##   Diet (%)                                                       
    ##      Excellent                    24 ( 5.8)         24 ( 5.5)    
    ##      Fair                        124 (30.2)        115 (26.4)    
    ##      Good                        155 (37.7)        170 (39.1)    
    ##      Poor                         32 ( 7.8)         43 ( 9.9)    
    ##      VeryGood                     76 (18.5)         83 (19.1)    
    ##   Smoking = Yes (%)               83 (18.0)         94 (20.5)    
    ##   Sedentary (mean (SD))       389.89 (196.84)   404.11 (196.67)  
    ##   BMI (mean (SD))              29.44 (8.08)      28.31 (7.41)    
    ##   Cholestrol (mean (SD))      175.08 (39.89)    181.16 (39.46)   
    ##   Glycohemoglobin (mean (SD))   5.65 (1.23)       5.64 (1.07)    
    ##                              Stratified by Quantile_rank_2
    ##                               3rd quartile PFOS 4th quartile PFOS p      test
    ##   n                              498               498                       
    ##   Gender = Male (%)              279 (56.0)        315 (63.3)     <0.001     
    ##   Age (mean (SD))              47.42 (20.17)     56.31 (17.50)    <0.001     
    ##   Race (%)                                                        <0.001     
    ##      Asian                        45 ( 9.0)         79 (15.9)                
    ##      Black                       103 (20.7)        138 (27.7)                
    ##      MexicanAmerican              85 (17.1)         47 ( 9.4)                
    ##      Other                        28 ( 5.6)         16 ( 3.2)                
    ##      OtherHispanic                67 (13.5)         59 (11.8)                
    ##      White                       170 (34.1)        159 (31.9)                
    ##   Diet (%)                                                         0.002     
    ##      Excellent                    35 ( 7.4)         60 (12.2)                
    ##      Fair                        128 (27.1)        104 (21.1)                
    ##      Good                        194 (41.1)        207 (42.1)                
    ##      Poor                         27 ( 5.7)         35 ( 7.1)                
    ##      VeryGood                     88 (18.6)         86 (17.5)                
    ##   Smoking = Yes (%)               96 (20.8)         99 (21.3)      0.590     
    ##   Sedentary (mean (SD))       388.61 (204.36)   366.71 (194.55)    0.030     
    ##   BMI (mean (SD))              28.54 (6.38)      29.12 (6.58)      0.048     
    ##   Cholestrol (mean (SD))      189.70 (44.98)    190.87 (41.33)    <0.001     
    ##   Glycohemoglobin (mean (SD))   5.75 (1.02)       6.00 (1.29)     <0.001

``` r
# Exporting to excel
table2save = print(tab2, test = FALSE)
```

    ##                              Stratified by Quantile_rank_2
    ##                               1st quartile PFOS 2nd quartile PFOS
    ##   n                              499               498           
    ##   Gender = Male (%)              148 (29.7)        222 (44.6)    
    ##   Age (mean (SD))              32.79 (17.08)     36.95 (18.71)   
    ##   Race (%)                                                       
    ##      Asian                        41 ( 8.2)         55 (11.0)    
    ##      Black                       100 (20.0)         98 (19.7)    
    ##      MexicanAmerican             135 (27.1)        103 (20.7)    
    ##      Other                        19 ( 3.8)         23 ( 4.6)    
    ##      OtherHispanic                78 (15.6)         55 (11.0)    
    ##      White                       126 (25.3)        164 (32.9)    
    ##   Diet (%)                                                       
    ##      Excellent                    24 ( 5.8)         24 ( 5.5)    
    ##      Fair                        124 (30.2)        115 (26.4)    
    ##      Good                        155 (37.7)        170 (39.1)    
    ##      Poor                         32 ( 7.8)         43 ( 9.9)    
    ##      VeryGood                     76 (18.5)         83 (19.1)    
    ##   Smoking = Yes (%)               83 (18.0)         94 (20.5)    
    ##   Sedentary (mean (SD))       389.89 (196.84)   404.11 (196.67)  
    ##   BMI (mean (SD))              29.44 (8.08)      28.31 (7.41)    
    ##   Cholestrol (mean (SD))      175.08 (39.89)    181.16 (39.46)   
    ##   Glycohemoglobin (mean (SD))   5.65 (1.23)       5.64 (1.07)    
    ##                              Stratified by Quantile_rank_2
    ##                               3rd quartile PFOS 4th quartile PFOS
    ##   n                              498               498           
    ##   Gender = Male (%)              279 (56.0)        315 (63.3)    
    ##   Age (mean (SD))              47.42 (20.17)     56.31 (17.50)   
    ##   Race (%)                                                       
    ##      Asian                        45 ( 9.0)         79 (15.9)    
    ##      Black                       103 (20.7)        138 (27.7)    
    ##      MexicanAmerican              85 (17.1)         47 ( 9.4)    
    ##      Other                        28 ( 5.6)         16 ( 3.2)    
    ##      OtherHispanic                67 (13.5)         59 (11.8)    
    ##      White                       170 (34.1)        159 (31.9)    
    ##   Diet (%)                                                       
    ##      Excellent                    35 ( 7.4)         60 (12.2)    
    ##      Fair                        128 (27.1)        104 (21.1)    
    ##      Good                        194 (41.1)        207 (42.1)    
    ##      Poor                         27 ( 5.7)         35 ( 7.1)    
    ##      VeryGood                     88 (18.6)         86 (17.5)    
    ##   Smoking = Yes (%)               96 (20.8)         99 (21.3)    
    ##   Sedentary (mean (SD))       388.61 (204.36)   366.71 (194.55)  
    ##   BMI (mean (SD))              28.54 (6.38)      29.12 (6.58)    
    ##   Cholestrol (mean (SD))      189.70 (44.98)    190.87 (41.33)   
    ##   Glycohemoglobin (mean (SD))   5.75 (1.02)       6.00 (1.29)

``` r
write.csv(table2save, file = "Output/Lab2_Table2.csv")


tab3 = CreateTableOne(data = DATA, 
                      vars = table_vars,
                      strata = table_strat_2, addOverall = TRUE)


print(tab3)
```

    ##                              Stratified by Quantile_rank_2
    ##                               Overall         1st quartile PFOS
    ##   n                             1993             499           
    ##   Gender = Male (%)              964 (48.4)      148 (29.7)    
    ##   Age (mean (SD))              43.36 (20.56)   32.79 (17.08)   
    ##   Race (%)                                                     
    ##      Asian                       220 (11.0)       41 ( 8.2)    
    ##      Black                       439 (22.0)      100 (20.0)    
    ##      MexicanAmerican             370 (18.6)      135 (27.1)    
    ##      Other                        86 ( 4.3)       19 ( 3.8)    
    ##      OtherHispanic               259 (13.0)       78 (15.6)    
    ##      White                       619 (31.1)      126 (25.3)    
    ##   Diet (%)                                                     
    ##      Excellent                   143 ( 7.9)       24 ( 5.8)    
    ##      Fair                        471 (26.0)      124 (30.2)    
    ##      Good                        726 (40.1)      155 (37.7)    
    ##      Poor                        137 ( 7.6)       32 ( 7.8)    
    ##      VeryGood                    333 (18.4)       76 (18.5)    
    ##   Smoking = Yes (%)              372 (20.2)       83 (18.0)    
    ##   Sedentary (mean (SD))       387.27 (198.45) 389.89 (196.84)  
    ##   BMI (mean (SD))              28.85 (7.15)    29.44 (8.08)    
    ##   Cholestrol (mean (SD))      184.20 (41.94)  175.08 (39.89)   
    ##   Glycohemoglobin (mean (SD))   5.76 (1.17)     5.65 (1.23)    
    ##                              Stratified by Quantile_rank_2
    ##                               2nd quartile PFOS 3rd quartile PFOS
    ##   n                              498               498           
    ##   Gender = Male (%)              222 (44.6)        279 (56.0)    
    ##   Age (mean (SD))              36.95 (18.71)     47.42 (20.17)   
    ##   Race (%)                                                       
    ##      Asian                        55 (11.0)         45 ( 9.0)    
    ##      Black                        98 (19.7)        103 (20.7)    
    ##      MexicanAmerican             103 (20.7)         85 (17.1)    
    ##      Other                        23 ( 4.6)         28 ( 5.6)    
    ##      OtherHispanic                55 (11.0)         67 (13.5)    
    ##      White                       164 (32.9)        170 (34.1)    
    ##   Diet (%)                                                       
    ##      Excellent                    24 ( 5.5)         35 ( 7.4)    
    ##      Fair                        115 (26.4)        128 (27.1)    
    ##      Good                        170 (39.1)        194 (41.1)    
    ##      Poor                         43 ( 9.9)         27 ( 5.7)    
    ##      VeryGood                     83 (19.1)         88 (18.6)    
    ##   Smoking = Yes (%)               94 (20.5)         96 (20.8)    
    ##   Sedentary (mean (SD))       404.11 (196.67)   388.61 (204.36)  
    ##   BMI (mean (SD))              28.31 (7.41)      28.54 (6.38)    
    ##   Cholestrol (mean (SD))      181.16 (39.46)    189.70 (44.98)   
    ##   Glycohemoglobin (mean (SD))   5.64 (1.07)       5.75 (1.02)    
    ##                              Stratified by Quantile_rank_2
    ##                               4th quartile PFOS p      test
    ##   n                              498                       
    ##   Gender = Male (%)              315 (63.3)     <0.001     
    ##   Age (mean (SD))              56.31 (17.50)    <0.001     
    ##   Race (%)                                      <0.001     
    ##      Asian                        79 (15.9)                
    ##      Black                       138 (27.7)                
    ##      MexicanAmerican              47 ( 9.4)                
    ##      Other                        16 ( 3.2)                
    ##      OtherHispanic                59 (11.8)                
    ##      White                       159 (31.9)                
    ##   Diet (%)                                       0.002     
    ##      Excellent                    60 (12.2)                
    ##      Fair                        104 (21.1)                
    ##      Good                        207 (42.1)                
    ##      Poor                         35 ( 7.1)                
    ##      VeryGood                     86 (17.5)                
    ##   Smoking = Yes (%)               99 (21.3)      0.590     
    ##   Sedentary (mean (SD))       366.71 (194.55)    0.030     
    ##   BMI (mean (SD))              29.12 (6.58)      0.048     
    ##   Cholestrol (mean (SD))      190.87 (41.33)    <0.001     
    ##   Glycohemoglobin (mean (SD))   6.00 (1.29)     <0.001

``` r
# Exporting to excel
table3save = print(tab3, test = FALSE)
```

    ##                              Stratified by Quantile_rank_2
    ##                               Overall         1st quartile PFOS
    ##   n                             1993             499           
    ##   Gender = Male (%)              964 (48.4)      148 (29.7)    
    ##   Age (mean (SD))              43.36 (20.56)   32.79 (17.08)   
    ##   Race (%)                                                     
    ##      Asian                       220 (11.0)       41 ( 8.2)    
    ##      Black                       439 (22.0)      100 (20.0)    
    ##      MexicanAmerican             370 (18.6)      135 (27.1)    
    ##      Other                        86 ( 4.3)       19 ( 3.8)    
    ##      OtherHispanic               259 (13.0)       78 (15.6)    
    ##      White                       619 (31.1)      126 (25.3)    
    ##   Diet (%)                                                     
    ##      Excellent                   143 ( 7.9)       24 ( 5.8)    
    ##      Fair                        471 (26.0)      124 (30.2)    
    ##      Good                        726 (40.1)      155 (37.7)    
    ##      Poor                        137 ( 7.6)       32 ( 7.8)    
    ##      VeryGood                    333 (18.4)       76 (18.5)    
    ##   Smoking = Yes (%)              372 (20.2)       83 (18.0)    
    ##   Sedentary (mean (SD))       387.27 (198.45) 389.89 (196.84)  
    ##   BMI (mean (SD))              28.85 (7.15)    29.44 (8.08)    
    ##   Cholestrol (mean (SD))      184.20 (41.94)  175.08 (39.89)   
    ##   Glycohemoglobin (mean (SD))   5.76 (1.17)     5.65 (1.23)    
    ##                              Stratified by Quantile_rank_2
    ##                               2nd quartile PFOS 3rd quartile PFOS
    ##   n                              498               498           
    ##   Gender = Male (%)              222 (44.6)        279 (56.0)    
    ##   Age (mean (SD))              36.95 (18.71)     47.42 (20.17)   
    ##   Race (%)                                                       
    ##      Asian                        55 (11.0)         45 ( 9.0)    
    ##      Black                        98 (19.7)        103 (20.7)    
    ##      MexicanAmerican             103 (20.7)         85 (17.1)    
    ##      Other                        23 ( 4.6)         28 ( 5.6)    
    ##      OtherHispanic                55 (11.0)         67 (13.5)    
    ##      White                       164 (32.9)        170 (34.1)    
    ##   Diet (%)                                                       
    ##      Excellent                    24 ( 5.5)         35 ( 7.4)    
    ##      Fair                        115 (26.4)        128 (27.1)    
    ##      Good                        170 (39.1)        194 (41.1)    
    ##      Poor                         43 ( 9.9)         27 ( 5.7)    
    ##      VeryGood                     83 (19.1)         88 (18.6)    
    ##   Smoking = Yes (%)               94 (20.5)         96 (20.8)    
    ##   Sedentary (mean (SD))       404.11 (196.67)   388.61 (204.36)  
    ##   BMI (mean (SD))              28.31 (7.41)      28.54 (6.38)    
    ##   Cholestrol (mean (SD))      181.16 (39.46)    189.70 (44.98)   
    ##   Glycohemoglobin (mean (SD))   5.64 (1.07)       5.75 (1.02)    
    ##                              Stratified by Quantile_rank_2
    ##                               4th quartile PFOS
    ##   n                              498           
    ##   Gender = Male (%)              315 (63.3)    
    ##   Age (mean (SD))              56.31 (17.50)   
    ##   Race (%)                                     
    ##      Asian                        79 (15.9)    
    ##      Black                       138 (27.7)    
    ##      MexicanAmerican              47 ( 9.4)    
    ##      Other                        16 ( 3.2)    
    ##      OtherHispanic                59 (11.8)    
    ##      White                       159 (31.9)    
    ##   Diet (%)                                     
    ##      Excellent                    60 (12.2)    
    ##      Fair                        104 (21.1)    
    ##      Good                        207 (42.1)    
    ##      Poor                         35 ( 7.1)    
    ##      VeryGood                     86 (17.5)    
    ##   Smoking = Yes (%)               99 (21.3)    
    ##   Sedentary (mean (SD))       366.71 (194.55)  
    ##   BMI (mean (SD))              29.12 (6.58)    
    ##   Cholestrol (mean (SD))      190.87 (41.33)   
    ##   Glycohemoglobin (mean (SD))   6.00 (1.29)

``` r
write.csv(table3save, file = "Output/Lab2_Table3.csv")
```

Building regression models

``` r
#change data name for easier accessibility

D <- DATA

#lm models for cholesterol vs variables

{#lm model for cholesterol vs pfos
lm.Cholesterol.pfos = lm(D$Cholestrol ~ log10(D$PFOS))
summary(lm.Cholesterol.pfos)
#lm model for cholesterol vs pfoa
lm.Cholesterol.pfoa = lm(D$Cholestrol ~log10(D$PFOA))
summary(lm.Cholesterol.pfoa)
#lm model for cholesterol vs gender
lm.Cholesterol.gender = lm(D$Cholestrol ~D$Gender)
summary(lm.Cholesterol.gender)
#lm model for cholesterol vs age
lm.Cholesterol.age = lm(D$Cholestrol ~D$Age)
summary(lm.Cholesterol.age)
race_ordered = ordered(D$Race, levels=c("White", "Black", "MexicanAmerican", "OtherHispanic", "Asian","Other"))
#lm model for cholesterol vs race
lm.Cholesterol.race = lm(D$Cholestrol ~ D$Race)
summary(lm.Cholesterol.race)
#lm model for cholesterol vs diet
lm.Cholesterol.diet = lm(D$Cholestrol ~ D$Diet)
summary(lm.Cholesterol.diet)
#lm model for cholesterol vs smoking
lm.Cholesterol.smoking = lm(D$Cholestrol ~ D$Smoking)
summary(lm.Cholesterol.smoking)
#lm model for cholesterol vs sedentary
lm.Cholesterol.sedentary = lm(D$Cholestrol ~ D$Sedentary)
summary(lm.Cholesterol.sedentary)
}
```

    ## 
    ## Call:
    ## lm(formula = D$Cholestrol ~ D$Sedentary)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -99.30 -30.44  -3.72  24.13 363.13 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 189.020122   2.072427  91.207   <2e-16 ***
    ## D$Sedentary  -0.011923   0.004764  -2.503   0.0124 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 41.85 on 1959 degrees of freedom
    ##   (32 observations deleted due to missingness)
    ## Multiple R-squared:  0.003188,   Adjusted R-squared:  0.002679 
    ## F-statistic: 6.264 on 1 and 1959 DF,  p-value: 0.0124

``` r
#lm models for BMI vs variables
{
  #lm model for BMI vs pfos
  lm.BMI.pfos = lm(D$BMI ~ log10(D$PFOS))
  summary(lm.BMI.pfos)
  #lm model for BMI vs pfoa
  lm.BMI.pfoa = lm(D$BMI ~log10(D$PFOA))
  summary(lm.BMI.pfoa)
  #lm model for BMI vs gender
  lm.BMI.gender = lm(D$BMI ~D$Gender)
  summary(lm.BMI.gender)
  #lm model for BMI vs age
  lm.BMI.age = lm(D$BMI ~D$Age)
  summary(lm.BMI.age)
  race_ordered = ordered(D$Race, levels=c("White", "Black", "MexicanAmerican", "OtherHispanic", "Asian","Other"))
  #lm model for BMI vs race
  lm.BMI.race = lm(D$BMI ~ race_ordered)
  summary(lm.BMI.race)
  #lm model for BMI vs diet
  lm.BMI.diet = lm(D$BMI ~ D$Diet)
  summary(lm.BMI.diet)
  #lm model for BMI vs smoking
  lm.BMI.smoking = lm(D$BMI ~ D$Smoking)
  summary(lm.BMI.smoking)
  #lm model for BMI vs sedentary
  lm.BMI.sedentary = lm(D$BMI ~ D$Sedentary)
  summary(lm.BMI.sedentary)
}
```

    ## 
    ## Call:
    ## lm(formula = D$BMI ~ D$Sedentary)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -14.418  -5.180  -1.018   3.912  35.551 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 2.870e+01  3.562e-01  80.594   <2e-16 ***
    ## D$Sedentary 5.102e-04  8.196e-04   0.622    0.534    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 7.155 on 1944 degrees of freedom
    ##   (47 observations deleted due to missingness)
    ## Multiple R-squared:  0.0001993,  Adjusted R-squared:  -0.000315 
    ## F-statistic: 0.3874 on 1 and 1944 DF,  p-value: 0.5337

``` r
#lm models for Glycohemoglobin vs variables
{
  #lm model for Glycohemoglobin vs pfos
  lm.Glycohemoglobin.pfos = lm(D$Glycohemoglobin ~ log10(D$PFOS))
  summary(lm.Glycohemoglobin.pfos)
  #lm model for Glycohemoglobin vs pfoa
  lm.Glycohemoglobin.pfoa = lm(D$Glycohemoglobin ~log10(D$PFOA))
  summary(lm.Glycohemoglobin.pfoa)
  #lm model for Glycohemoglobin vs gender
  lm.Glycohemoglobin.gender = lm(D$Glycohemoglobin ~D$Gender)
  summary(lm.Glycohemoglobin.gender)
  #lm model for Glycohemoglobin vs age
  lm.Glycohemoglobin.age = lm(D$Glycohemoglobin ~D$Age)
  summary(lm.Glycohemoglobin.age)
  race_ordered = ordered(D$Race, levels=c("White", "Black", "MexicanAmerican", "OtherHispanic", "Asian","Other"))
  #lm model for Glycohemoglobin vs race
  lm.Glycohemoglobin.race = lm(D$Glycohemoglobin ~ D$Race)
  summary(lm.Glycohemoglobin.race)
  #lm model for Glycohemoglobin vs diet
  lm.Glycohemoglobin.diet = lm(D$Glycohemoglobin ~ D$Diet)
  summary(lm.Glycohemoglobin.diet)
  #lm model for Glycohemoglobin vs smoking
  lm.Glycohemoglobin.smoking = lm(D$Glycohemoglobin ~ D$Smoking)
  summary(lm.Glycohemoglobin.smoking)
  #lm model for Glycohemoglobin vs sedentary
  lm.Glycohemoglobin.sedentary = lm(D$Glycohemoglobin ~ D$Sedentary)
  summary(lm.Glycohemoglobin.sedentary)
}
```

    ## 
    ## Call:
    ## lm(formula = D$Glycohemoglobin ~ D$Sedentary)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1.9110 -0.5417 -0.2687  0.0968 11.1431 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  5.8645846  0.0581656 100.826   <2e-16 ***
    ## D$Sedentary -0.0002559  0.0001338  -1.913   0.0559 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.174 on 1958 degrees of freedom
    ##   (33 observations deleted due to missingness)
    ## Multiple R-squared:  0.001865,   Adjusted R-squared:  0.001355 
    ## F-statistic: 3.658 on 1 and 1958 DF,  p-value: 0.05594

``` r
#creating new column with pfos quartiles
Quantile_rank = ntile(D$PFOS,4)
Quantile_rank[Quantile_rank == 1] = "1st quartile PFOS"
Quantile_rank[Quantile_rank == 2] = "2nd quartile PFOS"
Quantile_rank[Quantile_rank == 3] = "3rd quartile PFOS"
Quantile_rank[Quantile_rank == 4] = "4th quartile PFOS"
D = mutate(D, Quantile_rank)

#desc stats for age grouped by quartile
group_by(D, Quantile_rank) %>% summarize(mean_age=mean(Age), sd_age=sd(Age))
```

    ## # A tibble: 4 x 3
    ##   Quantile_rank     mean_age sd_age
    ##   <chr>                <dbl>  <dbl>
    ## 1 1st quartile PFOA     36.0   18.5
    ## 2 2nd quartile PFOA     39.1   20.0
    ## 3 3rd quartile PFOA     45.7   20.5
    ## 4 4th quartile PFOA     52.7   19.2

``` r
#anova for diff in means
aov.age = aov(Age ~ Quantile_rank, data=D)
summary(aov.age)   
```

    ##                 Df Sum Sq Mean Sq F value Pr(>F)    
    ## Quantile_rank    3  81971   27324   71.54 <2e-16 ***
    ## Residuals     1989 759713     382                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
#desc stats for bmi grouped by quartile
group_by(D, Quantile_rank) %>% summarize(mean_bmi=mean(BMI), na.rm = TRUE, sd_bmi=sd(BMI), na.rm = TRUE)
```

    ## # A tibble: 4 x 4
    ##   Quantile_rank     mean_bmi na.rm sd_bmi
    ##   <chr>                <dbl> <lgl>  <dbl>
    ## 1 1st quartile PFOA       NA TRUE      NA
    ## 2 2nd quartile PFOA       NA TRUE      NA
    ## 3 3rd quartile PFOA       NA TRUE      NA
    ## 4 4th quartile PFOA       NA TRUE      NA

``` r
#anova for diff in means
aov.bmi = aov(BMI ~ Quantile_rank, data=D)
summary(aov.bmi)   
```

    ##                 Df Sum Sq Mean Sq F value Pr(>F)
    ## Quantile_rank    3    307  102.32   2.004  0.111
    ## Residuals     1973 100746   51.06               
    ## 16 observations deleted due to missingness

``` r
#desc stats for cholestrol grouped by quartile
group_by(D, Quantile_rank) %>% summarize(mean_Cholestrol=mean(Cholestrol), na.rm = TRUE, sd_Cholestrol=sd(Cholestrol), na.rm = TRUE)
```

    ## # A tibble: 4 x 4
    ##   Quantile_rank     mean_Cholestrol na.rm sd_Cholestrol
    ##   <chr>                       <dbl> <lgl>         <dbl>
    ## 1 1st quartile PFOA             NA  TRUE           NA  
    ## 2 2nd quartile PFOA            178. TRUE           38.6
    ## 3 3rd quartile PFOA            190. TRUE           42.9
    ## 4 4th quartile PFOA            192. TRUE           40.7

``` r
#anova for diff in means
aov.cholestrol = aov(Cholestrol ~ Quantile_rank, data=D)
summary(aov.cholestrol)   
```

    ##                 Df  Sum Sq Mean Sq F value   Pr(>F)    
    ## Quantile_rank    3   96437   32146   18.76 5.33e-12 ***
    ## Residuals     1988 3406336    1713                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 1 observation deleted due to missingness

``` r
#desc stats for glycohemoglobin grouped by quartile
group_by(D, Quantile_rank) %>% summarize(mean_Glycohemoglobin=mean(Glycohemoglobin), na.rm = TRUE, sd_Glycohemoglobin=sd(Glycohemoglobin), na.rm = TRUE)
```

    ## # A tibble: 4 x 4
    ##   Quantile_rank     mean_Glycohemoglobin na.rm sd_Glycohemoglobin
    ##   <chr>                            <dbl> <lgl>              <dbl>
    ## 1 1st quartile PFOA                NA    TRUE              NA    
    ## 2 2nd quartile PFOA                 5.76 TRUE               1.26 
    ## 3 3rd quartile PFOA                 5.76 TRUE               1.17 
    ## 4 4th quartile PFOA                 5.76 TRUE               0.911

``` r
#anova for diff in means
aov.Glycohemoglobin = aov(Glycohemoglobin ~ Quantile_rank, data=D)
summary(aov.Glycohemoglobin)
```

    ##                 Df Sum Sq Mean Sq F value Pr(>F)
    ## Quantile_rank    3      0  0.0027   0.002      1
    ## Residuals     1987   2712  1.3649               
    ## 2 observations deleted due to missingness

``` r
#desc stats for Sedentary grouped by quartile
group_by(D, Quantile_rank) %>% summarize(mean_Sedentary=mean(Sedentary), na.rm = TRUE, sd_Sedentary=sd(Sedentary), na.rm = TRUE)
```

    ## # A tibble: 4 x 4
    ##   Quantile_rank     mean_Sedentary na.rm sd_Sedentary
    ##   <chr>                      <dbl> <lgl>        <dbl>
    ## 1 1st quartile PFOA             NA TRUE            NA
    ## 2 2nd quartile PFOA             NA TRUE            NA
    ## 3 3rd quartile PFOA             NA TRUE            NA
    ## 4 4th quartile PFOA             NA TRUE            NA

``` r
#anova for diff in means
aov.Sedentary = aov(Sedentary ~ Quantile_rank, data=D)
summary(aov.Sedentary)   
```

    ##                 Df   Sum Sq Mean Sq F value Pr(>F)  
    ## Quantile_rank    3   254212   84737   2.155 0.0914 .
    ## Residuals     1958 76973250   39312                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 31 observations deleted due to missingness

``` r
# Proportions within regions for categorical variables
#pfos vs smoking
prop.table(table(D$Quantile_rank, D$Smoking), margin = 1)
```

    ##                    
    ##                            No       Yes
    ##   1st quartile PFOA 0.8351410 0.1648590
    ##   2nd quartile PFOA 0.7784946 0.2215054
    ##   3rd quartile PFOA 0.7775378 0.2224622
    ##   4th quartile PFOA 0.8030635 0.1969365

``` r
round(prop.table(table(D$Quantile_rank, D$Smoking), margin = 1),3) *100
```

    ##                    
    ##                       No  Yes
    ##   1st quartile PFOA 83.5 16.5
    ##   2nd quartile PFOA 77.8 22.2
    ##   3rd quartile PFOA 77.8 22.2
    ##   4th quartile PFOA 80.3 19.7

``` r
chisq.test(table(D$Quantile_rank,D$Smoking))
```

    ## 
    ##  Pearson's Chi-squared test
    ## 
    ## data:  table(D$Quantile_rank, D$Smoking)
    ## X-squared = 6.3265, df = 3, p-value = 0.09676

``` r
# Proportions within regions for categorical variables
#pfos vs Gender
prop.table(table(D$Quantile_rank, D$Gender), margin = 1)
```

    ##                    
    ##                        Female      Male
    ##   1st quartile PFOA 0.7434870 0.2565130
    ##   2nd quartile PFOA 0.4859438 0.5140562
    ##   3rd quartile PFOA 0.4236948 0.5763052
    ##   4th quartile PFOA 0.4116466 0.5883534

``` r
round(prop.table(table(D$Quantile_rank, D$Gender), margin = 1),3) *100
```

    ##                    
    ##                     Female Male
    ##   1st quartile PFOA   74.3 25.7
    ##   2nd quartile PFOA   48.6 51.4
    ##   3rd quartile PFOA   42.4 57.6
    ##   4th quartile PFOA   41.2 58.8

``` r
 chisq.test(table(D$Quantile_rank,D$Gender))
```

    ## 
    ##  Pearson's Chi-squared test
    ## 
    ## data:  table(D$Quantile_rank, D$Gender)
    ## X-squared = 143.91, df = 3, p-value < 2.2e-16

``` r
 # Proportions within regions for categorical variables
 #pfos vs Diet
 prop.table(table(D$Quantile_rank, D$Diet), margin = 1)
```

    ##                    
    ##                      Excellent       Fair       Good       Poor   VeryGood
    ##   1st quartile PFOA 0.06060606 0.31934732 0.38461538 0.10023310 0.13519814
    ##   2nd quartile PFOA 0.06235566 0.26789838 0.39722864 0.07159353 0.20092379
    ##   3rd quartile PFOA 0.07758621 0.26293103 0.39439655 0.06250000 0.20258621
    ##   4th quartile PFOA 0.11157025 0.19834711 0.42561983 0.07024793 0.19421488

``` r
 round(prop.table(table(D$Quantile_rank, D$Diet), margin = 1),3) *100
```

    ##                    
    ##                     Excellent Fair Good Poor VeryGood
    ##   1st quartile PFOA       6.1 31.9 38.5 10.0     13.5
    ##   2nd quartile PFOA       6.2 26.8 39.7  7.2     20.1
    ##   3rd quartile PFOA       7.8 26.3 39.4  6.2     20.3
    ##   4th quartile PFOA      11.2 19.8 42.6  7.0     19.4

``` r
 chisq.test(table(D$Quantile_rank,D$Diet))
```

    ## 
    ##  Pearson's Chi-squared test
    ## 
    ## data:  table(D$Quantile_rank, D$Diet)
    ## X-squared = 36.085, df = 12, p-value = 0.000314

``` r
 # Proportions within regions for categorical variables
 #pfos vs Race
 prop.table(table(D$Quantile_rank, D$Race), margin = 1)
```

    ##                    
    ##                          Asian      Black MexicanAmerican      Other
    ##   1st quartile PFOA 0.09018036 0.25851703      0.28456914 0.03206413
    ##   2nd quartile PFOA 0.09437751 0.20682731      0.20481928 0.05622490
    ##   3rd quartile PFOA 0.12851406 0.20281124      0.16265060 0.04618474
    ##   4th quartile PFOA 0.12851406 0.21285141      0.09036145 0.03815261
    ##                    
    ##                     OtherHispanic      White
    ##   1st quartile PFOA    0.13026052 0.20440882
    ##   2nd quartile PFOA    0.11244980 0.32530120
    ##   3rd quartile PFOA    0.13253012 0.32730924
    ##   4th quartile PFOA    0.14457831 0.38554217

``` r
 round(prop.table(table(D$Quantile_rank, D$Race), margin = 1),3) *100
```

    ##                    
    ##                     Asian Black MexicanAmerican Other OtherHispanic White
    ##   1st quartile PFOA   9.0  25.9            28.5   3.2          13.0  20.4
    ##   2nd quartile PFOA   9.4  20.7            20.5   5.6          11.2  32.5
    ##   3rd quartile PFOA  12.9  20.3            16.3   4.6          13.3  32.7
    ##   4th quartile PFOA  12.9  21.3             9.0   3.8          14.5  38.6

``` r
 chisq.test(table(D$Quantile_rank,D$Race))
```

    ## 
    ##  Pearson's Chi-squared test
    ## 
    ## data:  table(D$Quantile_rank, D$Race)
    ## X-squared = 97.29, df = 15, p-value = 4.248e-14

``` r
 #lm plot chol vs pfos
 w = 300
 h = 200
 res = 300
 
 jpeg(file="test.1.jpg", width = w, height = h, units='mm', res = res)
plot(D$Cholestrol ~ log10(D$PFOS),col="#636363", ylab= "Cholestrol mg/dL", xlab="Log_PFOS ng/ml" , cex.lab=1.4,cex.axis=1.2)
dev.off()
```

    ## png 
    ##   2

``` r
 #residual plots for chol vs pfos
w = 300
h = 200
res = 300

jpeg(file="test.2.jpg", width = w, height = h, units='mm', res = res)
lm.cholestrol.vs.pfos = lm(D$Cholestrol ~ log10(D$PFOS))
residuals = resid(lm.cholestrol.vs.pfos)
plot(fitted(lm.cholestrol.vs.pfos),residuals, col="#636363", pch =21, xlab= "Predicted Cholestrol values", ylab="Residuals",cex.lab=1.4,cex.axis=1.2,)
abline(0,0, col ="red", lwd=1.5)
dev.off()
```

    ## png 
    ##   2

``` r
#lm plot BMI vs pfos
w = 300
h = 200
res = 300

jpeg(file="test.5.jpg", width = w, height = h, units='mm', res = res)
plot(D$BMI ~ log10(D$PFOS),col="#636363", ylab= "BMI", xlab="Log_PFOS ng/ml" , cex.lab=1.4,cex.axis=1.2)
dev.off()
```

    ## png 
    ##   2

``` r
#residual plots for BMI vs pfos
w = 300
h = 200
res = 300

jpeg(file="test.6.jpg", width = w, height = h, units='mm', res = res)
lm.bmi.vs.pfos = lm(D$BMI ~ log10(D$PFOS))
residuals = resid(lm.bmi.vs.pfos)
plot(fitted(lm.bmi.vs.pfos),residuals, col="#636363", pch =21, xlab= "Predicted BMI values", ylab="Residuals",cex.lab=1.4,cex.axis=1.2, )
abline(0,0, col ="red", lwd=1.5)
dev.off()
```

    ## png 
    ##   2

``` r
#hist of residuals
w = 300
h = 200
res = 300

jpeg(file="test.7.jpg", width = w, height = h, units='mm', res = res)
hist(residuals, main = "", col="#636363", xlab= "Residuals", cex.lab=1.4,cex.axis=1.2)
dev.off()
```

    ## png 
    ##   2

``` r
#diet ordering
diet_ordered = ordered(D$Diet, levels=c("Excellent", "VeryGood", "Good", "Fair", "Poor"))

#boxplot comparing cholestrol across all groups
w = 300
h = 200
res = 300

#overall boxplot cholestrol vs all variables
jpeg(file="test.4.jpg", width = w, height = h, units='mm', res = res)
boxplot(D$Cholestrol~D$Gender, at = c(1,2), xlim = c(0,18), xlab = "", ylab = "Cholestrol (mg/dL)", col= "#feb24c", cex.axis = 0.75)
boxplot(D$Cholestrol~D$Race, at = c(4:9), xlab = "", ylab = "Cholestrol (mg/dL)", col= "#99d8c9", add = TRUE, cex.axis = 0.75, names = c("Asian", "Black", "\nMex\nAmer", "Other", "\nOther\nHisp", "White"))
boxplot(D$Cholestrol~diet_ordered, at = c(11:15), xlab = "", ylab = "Cholestrol (mg/dL)", col= "#bcbddc", add = TRUE, cex.axis = 0.75, names = c("Excel", "V.Good", "Good", "Fair", "Poor"))
boxplot(D$Cholestrol~D$Smoking, at = c(17,18), xlim = c(0,20), xlab = "", ylab = "Cholestrol (mg/dL)", col= "#fa9fb5", cex.axis = 0.75, add = TRUE)
abline(h=median(D$Cholestrol, na.rm = TRUE), col='red')
dev.off()
```

    ## png 
    ##   2

``` r
#overall boxplot BMI vs all variables
jpeg(file="test.9.jpg", width = w, height = h, units='mm', res = res)
boxplot(D$BMI~D$Gender, at = c(1,2), xlim = c(0,18), xlab = "", ylab = "BMI ", col= "#feb24c", cex.axis = 0.75)
boxplot(D$BMI~D$Race, at = c(4:9), xlab = "", ylab = "BMI ", col= "#99d8c9", add = TRUE, cex.axis = 0.75, names = c("Asian", "Black", "\nMex\nAmer", "Other", "\nOther\nHisp", "White"))
boxplot(D$BMI~diet_ordered, at = c(11:15), xlab = "", ylab = "BMI ", col= "#bcbddc", add = TRUE, cex.axis = 0.75, names = c("Excel", "V.Good", "Good", "Fair", "Poor"))
boxplot(D$BMI~D$Smoking, at = c(17,18), xlim = c(0,20), xlab = "", ylab = "BMI ", col= "#fa9fb5", cex.axis = 0.75, add = TRUE)
abline(h=median(D$BMI, na.rm = TRUE), col='red')
dev.off()
```

    ## png 
    ##   2

``` r
#overall boxplot Glycohemoglobin vs all variables
jpeg(file="test.10.jpg", width = w, height = h, units='mm', res = res)
boxplot(D$Glycohemoglobin~D$Gender, at = c(1,2), xlim = c(0,18), xlab = "", ylab = "Glycohemoglobin (%)", col= "#feb24c", cex.axis = 0.75)
boxplot(D$Glycohemoglobin~D$Race, at = c(4:9), xlab = "", ylab = "Glycohemoglobin (%)", col= "#99d8c9", add = TRUE, cex.axis = 0.75, names = c("Asian", "Black", "\nMex\nAmer", "Other", "\nOther\nHisp", "White"))
boxplot(D$Glycohemoglobin~diet_ordered, at = c(11:15), xlab = "", ylab = "Glycohemoglobin (%)", col= "#bcbddc", add = TRUE, cex.axis = 0.75, names = c("Excel", "V.Good", "Good", "Fair", "Poor"))
boxplot(D$Glycohemoglobin~D$Smoking, at = c(17,18), xlim = c(0,20), xlab = "", ylab = "Glycohemoglobin (%)", col= "#fa9fb5", cex.axis = 0.75, add = TRUE)
abline(h=median(D$Glycohemoglobin, na.rm = TRUE), col='red')
dev.off()
```

    ## png 
    ##   2

``` r
plot(D$PFOS ~ D$Age, na.rm=TRUE)
```

    ## Warning in plot.window(...): "na.rm" is not a graphical parameter

    ## Warning in plot.xy(xy, type, ...): "na.rm" is not a graphical parameter

    ## Warning in axis(side = side, at = at, labels = labels, ...): "na.rm" is not a
    ## graphical parameter

    ## Warning in axis(side = side, at = at, labels = labels, ...): "na.rm" is not a
    ## graphical parameter

    ## Warning in box(...): "na.rm" is not a graphical parameter

    ## Warning in title(...): "na.rm" is not a graphical parameter

![](Code_Datawrangling_and_EDA_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
cor(D$Cholestrol, D$Age, use="complete.obs")
```

    ## [1] 0.2409191

``` r
#QQplot for residuls
jpeg(file="test.20.jpg", width = w, height = h, units='mm', res = res)
qqnorm(residuals, main = "", col="#636363", xlab = "Normal Quantiles", ylab ="Observed Quantiles (Standardized Residuals)" , cex.lab=1.4,cex.axis=1.2, pch=21, ylim =c(-100,200))
qqline(residuals, col = "red", lwd=1.5)
dev.off()
```

    ## png 
    ##   2

Building multiple regression models

``` r
mult_reg_model =lm(D$Cholestrol ~ log10(D$PFOS)+ D$Gender + D$Age + D$Race + D$Diet  + D$Smoking + D$Sedentary)
summary(mult_reg_model)
```

    ## 
    ## Call:
    ## lm(formula = D$Cholestrol ~ log10(D$PFOS) + D$Gender + D$Age + 
    ##     D$Race + D$Diet + D$Smoking + D$Sedentary)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -119.63  -27.33   -3.55   22.58  363.99 
    ## 
    ## Coefficients:
    ##                         Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)           172.850900   5.759603  30.011  < 2e-16 ***
    ## log10(D$PFOS)           9.292175   2.871596   3.236  0.00124 ** 
    ## D$GenderMale           -9.785389   2.146702  -4.558 5.54e-06 ***
    ## D$Age                   0.274080   0.058272   4.703 2.77e-06 ***
    ## D$RaceBlack            -8.578298   3.868617  -2.217  0.02673 *  
    ## D$RaceMexicanAmerican  -4.583929   4.054171  -1.131  0.25836    
    ## D$RaceOther             3.346629   5.938275   0.564  0.57312    
    ## D$RaceOtherHispanic    -0.760154   4.326903  -0.176  0.86057    
    ## D$RaceWhite            -3.710200   3.684000  -1.007  0.31403    
    ## D$DietFair              4.985686   4.156157   1.200  0.23047    
    ## D$DietGood              3.891707   3.949832   0.985  0.32463    
    ## D$DietPoor              7.285045   5.205357   1.400  0.16184    
    ## D$DietVeryGood          3.362855   4.289600   0.784  0.43318    
    ## D$SmokingYes            7.619978   2.538144   3.002  0.00272 ** 
    ## D$Sedentary            -0.000146   0.005087  -0.029  0.97710    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 40.67 on 1638 degrees of freedom
    ##   (340 observations deleted due to missingness)
    ## Multiple R-squared:  0.04874,    Adjusted R-squared:  0.04061 
    ## F-statistic: 5.995 on 14 and 1638 DF,  p-value: 1.128e-11

``` r
lm(D$Cholestrol ~ log10(D$PFOS))
```

    ## 
    ## Call:
    ## lm(formula = D$Cholestrol ~ log10(D$PFOS))
    ## 
    ## Coefficients:
    ##   (Intercept)  log10(D$PFOS)  
    ##         177.5           13.1

``` r
summary(lm(D$Cholestrol ~ log10(D$PFOS)))
```

    ## 
    ## Call:
    ## lm(formula = D$Cholestrol ~ log10(D$PFOS))
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -102.06  -29.54   -3.56   24.67  360.88 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)    177.496      1.496 118.617  < 2e-16 ***
    ## log10(D$PFOS)   13.104      2.288   5.727 1.18e-08 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 41.61 on 1990 degrees of freedom
    ##   (1 observation deleted due to missingness)
    ## Multiple R-squared:  0.01621,    Adjusted R-squared:  0.01572 
    ## F-statistic: 32.79 on 1 and 1990 DF,  p-value: 1.181e-08

``` r
mult_reg_model.2 =lm(D$Cholestrol ~ log10(D$PFOA)+ D$Gender + D$Age + D$Race + D$Diet  + D$Smoking + D$Sedentary)
summary(mult_reg_model.2)
```

    ## 
    ## Call:
    ## lm(formula = D$Cholestrol ~ log10(D$PFOA) + D$Gender + D$Age + 
    ##     D$Race + D$Diet + D$Smoking + D$Sedentary)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -121.40  -27.20   -4.00   22.29  368.56 
    ## 
    ## Coefficients:
    ##                         Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)            1.754e+02  5.703e+00  30.749  < 2e-16 ***
    ## log10(D$PFOA)          1.644e+01  3.238e+00   5.077 4.28e-07 ***
    ## D$GenderMale          -9.986e+00  2.085e+00  -4.789 1.83e-06 ***
    ## D$Age                  2.803e-01  5.464e-02   5.130 3.24e-07 ***
    ## D$RaceBlack           -7.940e+00  3.854e+00  -2.060  0.03953 *  
    ## D$RaceMexicanAmerican -4.409e+00  4.000e+00  -1.102  0.27054    
    ## D$RaceOther            3.007e+00  5.896e+00   0.510  0.61014    
    ## D$RaceOtherHispanic   -1.590e+00  4.289e+00  -0.371  0.71087    
    ## D$RaceWhite           -4.809e+00  3.637e+00  -1.322  0.18630    
    ## D$DietFair             5.157e+00  4.137e+00   1.247  0.21274    
    ## D$DietGood             3.826e+00  3.932e+00   0.973  0.33063    
    ## D$DietPoor             6.952e+00  5.181e+00   1.342  0.17985    
    ## D$DietVeryGood         2.864e+00  4.268e+00   0.671  0.50234    
    ## D$SmokingYes           7.771e+00  2.526e+00   3.076  0.00213 ** 
    ## D$Sedentary            2.895e-04  5.064e-03   0.057  0.95442    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 40.48 on 1638 degrees of freedom
    ##   (340 observations deleted due to missingness)
    ## Multiple R-squared:  0.05749,    Adjusted R-squared:  0.04943 
    ## F-statistic: 7.137 on 14 and 1638 DF,  p-value: 1.532e-14

``` r
lm(D$Cholestrol ~ log10(D$PFOA))
```

    ## 
    ## Call:
    ## lm(formula = D$Cholestrol ~ log10(D$PFOA))
    ## 
    ## Coefficients:
    ##   (Intercept)  log10(D$PFOA)  
    ##        181.64          17.97
