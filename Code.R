library(ggplot2)
library(plyr)

library(corrgram)

install.packages("corrgram")


# Preparing the data:
setwd("/Users/Kaveh/Desktop/")
admit_data = read.csv(file = "Admission_Predict_Ver1.1.csv",header = TRUE, sep = ",")

# Gaining further knowledge the data:
names(admit_data)
summary(admit_data)




# Analysis of the data:

# GRE Scores
GRE = qplot(GRE.Score,
        Chance.of.Admit,
        data=admit_data,
        xlab="GRE Score",
        ylab="Chance of Admit")

(GRE + geom_smooth(model = lm))


# TOEFL.Score
TOEFL = qplot(TOEFL.Score,
              Chance.of.Admit,
              data=admit_data,
              xlab="TOEFL Score",
              ylab="Chance of Admit")

(TOEFL + geom_smooth(model = lm))


# University Rating
Rating = qplot(University.Rating,
              Chance.of.Admit,
              data=admit_data,
              xlab="University Rating",
              ylab="Chance of Admit")

(Rating + geom_smooth(model = lm))


# SOP
SOP = qplot(SOP,
            Chance.of.Admit,
            data=admit_data,
            xlab="SOP",
            ylab="Chance of Admit")

(SOP + geom_smooth(model = lm))

# LOR
LOR = qplot(LOR,
            Chance.of.Admit,
            data=admit_data,
            xlab="LOR",
            ylab="Chance of Admit")

(LOR + geom_smooth(model = lm))


# CGPA
CGPA = qplot(CGPA,
             Chance.of.Admit,
             data=admit_data,
             xlab="CGPA",
             ylab="Chance of Admit")

(CGPA + geom_smooth(model = lm))


# Research
qplot(Research,
      Chance.of.Admit,
      data=admit_data,
      xlab="Research",
      ylab="Chance of Admit")

qplot(admit_data$Research,
      xlab="Research",
      ylab="count")

count(admit_data,c("Research"))  # -> Fairly Similar Count of Research being present or not




# Analyzing the correlation between the parameters using a heat map


cor(admit_data$GRE.Score,admit_data$Chance.of.Admit) # -> moderately high positive correlation

# linear regression:
mod <- lm(admit_data$Chance.of.Admit ~ admit_data$GRE.Score)

summary(mod)

plot(admit_data$GRE.Score,
     admit_data$Chance.of.Admit
     )
abline(mod,col="red",lwd="3")





# Analyzing correlation WITHIN each university:

rating_1 = admit_data[admit_data$University.Rating==1,]
rating_2 = admit_data[admit_data$University.Rating==2,]
rating_3 = admit_data[admit_data$University.Rating==3,]
rating_4 = admit_data[admit_data$University.Rating==4,]
rating_5 = admit_data[admit_data$University.Rating==5,]

par(mfrow=c(1,2))

qplot(GRE.Score,
      Chance.of.Admit,
      data=admit_data[admit_data$University.Rating==1,],
      xlab="GRE",
      ylab="Chance of Admit")

qplot(GRE.Score,
      Chance.of.Admit,
      data=admit_data[admit_data$University.Rating==2,],
      xlab="GRE",
      ylab="Chance of Admit")




ggplot(admit_data, aes(x=admit_data$GRE.Score, y=admit_data$Chance.of.Admit)) +
       geom_point() +
       geom_smooth(model = lm, col= "red", lwd=1) +
       facet_grid(admit_data$University.Rating~.) +
       ylab("Chance of Admit") +
       xlab("GRE Score")



ggplot(admit_data, aes(x=admit_data$GRE.Score, y=admit_data$Chance.of.Admit)) +
  geom_point(alpha = 0.5) +
  facet_grid(admit_data$University.Rating~.) +
  ylab("Chance of Admit") +
  xlab("GRE Score")



ggplot(admit_data,
       aes(x=GRE.Score,y=Chance.of.Admit),
            xlab="GRE Score",
            ylab="Chance of Admit") + 
  geom_point(alpha = 0.1)
  

(GRE + geom_smooth(model = lm,col= "red"))


corrgram(mtcars, order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Car Milage Data in PC2/PC1 Order")

corrgram(admit_data,
         order=TRUE,
         lower.panel = panel.shade,
         upper.panel = panel.cor,
         text.panel=panel.txt,
         main="Correlation Between Parameters")

install.packages("corrplot")

library(corrplot)

admit_cor = cor(admit_data)

corrplot(admit_cor, method="color",type="upper")



rating_1 = admit_data[admit_data$University.Rating==1,]
rating_2 = admit_data[admit_data$University.Rating==2,]
rating_3 = admit_data[admit_data$University.Rating==3,]
rating_4 = admit_data[admit_data$University.Rating==4,]
rating_5 = admit_data[admit_data$University.Rating==5,]


library(rpart)
library(rpart.plot)
library(rattle)



prediction_1 <- rpart(Chance.of.Admit ~  
                       GRE.Score  + 
                       TOEFL.Score + 
                       SOP + 
                       LOR + 
                       CGPA + 
                       Research,
                      data = admit_data[admit_data$University.Rating==1,],
                      method="anova")

rpart.plot(prediction_1,box.palette = "auto",main="rating 1")




