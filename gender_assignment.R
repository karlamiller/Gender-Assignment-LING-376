rm(list = ls(all = TRUE))


# DATA PREPARATION ######
# Load data
data <- read.csv("ga.csv", header = TRUE, sep = ",")

# Remove unwanted rows and columns
df <- data[ , -(0:18)]  
df <- df[-c(1, 2), ]

# Extract metadata columns 
metadata <- data[-c(1, 2), 19:23]  # adjust 1:5 if your metadata includes more columns

# Repeat metadata rows to match the stacked format (4 times for .1, .2, .3, .3)
metadata_long <- metadata[rep(1:nrow(metadata), each = 4), ]

# Combine the columns row-wise for each group
MM <- unlist(df[, c("MM", "MM.1", "MM.2", "MM.3")], use.names = FALSE)
FF <- unlist(df[, c("FF", "FF.1", "FF.2", "FF.3")], use.names = FALSE)
MF <- unlist(df[, c("MF", "MF.1", "MF.2", "MF.3")], use.names = FALSE)
FM <- unlist(df[, c("FM", "FM.1", "FM.2", "FM.3")], use.names = FALSE)


# Combine all into one dataframe
ga <- cbind(metadata_long, MM, FF, MF, FM)
summary(ga)

library(dplyr)
#renaming the columns
ga <- ga %>%
  rename(
    languages = languages..no.duplicates,
    other_lang = languages..no.duplicates_4_TEXT
  )


#Categorical variables as factors
ga$age <- as.factor(ga$age)
ga$gender <- as.factor(ga$gender)
ga$education <- as.factor(ga$education)
ga$languages <- as.factor(ga$languages)
ga$other_lang <- as.factor(ga$other_lang)
ga$MM <- as.factor(ga$MM)
ga$FF <- as.factor(ga$FF)
ga$MF <- as.factor(ga$MF)
ga$FM <- as.factor(ga$FM)
summary(ga)


# DATA VISUALIZATION ######
# Visualization of the DV and predictors
# mosaicplot(MM ~ languages, data=ga, 
#            color = c("#420039", "#284B63"))
# mosaicplot(FF ~ languages, data=ga, 
#            color = c("#420039", "#284B63"))
mosaicplot(MF ~ languages, data=ga, 
           color = c("#420039", "#284B63"))
mosaicplot(FM ~ languages, data=ga, 
           color = c("#420039", "#284B63"))


plot (ga$MM, main ="GA of MM", col = "#420039")
plot (ga$FM, main ="GA of FM", col = "darkblue")
plot (ga$MF, main ="GA of MF", col = "darkolivegreen")
plot (ga$FF, main ="GA of FF", col = "#284B63") 
        
        



# library(nnet)
# model1 <- multinom(MM ~ age + languages + education, data = ga) #nothing significant?
# summary(model1)
# library(car)
# Anova(model1, type = "II") 
# z <- summary(model1)$coefficients / summary(model1)$standard.errors
# p <- 2 * (1 - pnorm(abs(z)))
# print(p)
# levels(ga$MM)
# levels(ga$age)
# levels(ga$languages)
# levels(ga$education)
# library(effects)
# plot(allEffects(model1))


model2 <- multinom(FM ~ age + languages + education + gender, data = ga)#languages  24.7305 10    0.00588 **
summary(model2)
Anova(model2, type = "II") 
z <- summary(model2)$coefficients / summary(model2)$standard.errors
p <- 2 * (1 - pnorm(abs(z)))
print(p)
plot(allEffects(model2))


model3 <- multinom(MF ~ age + languages + education +gender, data = ga)#languages  18.4912 10    0.04722 *
summary(model3)
Anova(model3, type = "II") 
z <- summary(model3)$coefficients / summary(model3)$standard.errors
p <- 2 * (1 - pnorm(abs(z)))
print(p)
plot(allEffects(model3))



# model4 <- multinom(FF ~ age + languages + education + gender, data = ga)#languages  18.6714 10    0.04464 *
# summary(model4)
# Anova(model4, type = "II") 
# z <- summary(model4)$coefficients / summary(model3)$standard.errors
# p <- 2 * (1 - pnorm(abs(z)))
# print(p)
# plot(allEffects(model4))

