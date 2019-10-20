library(lme4)
library(lmerTest)
library(gridExtra)

# Read in the data
beauty <- read.table ("Beauty.txt", header=T, sep=" ")

# data summary
str(beauty)

ggplot(data=beauty, aes(x=beauty, y=eval)) + geom_point()
ggplot(data=beauty, aes(x=beauty, y=eval)) + geom_point() + facet_wrap(~as.factor(courseID))


ggplot(beauty, aes(x=beauty, y=eval)) +
  geom_point() +
  facet_wrap(~profnumber)

beauty$tenured <- factor(beauty$tenured)
beauty$minority <- factor(beauty$minority)
beauty$female <- factor(beauty$female)
beauty$formal <- factor(beauty$formal)
beauty$lower <- factor(beauty$lower)
beauty$multipleclass <- factor(beauty$multipleclass)
beauty$nonenglish <- factor(beauty$nonenglish)
beauty$onecredit <- factor(beauty$onecredit)
beauty$tenuretrack <- factor(beauty$tenuretrack)


ggplot(beauty, aes(y=eval, x=tenured)) + geom_boxplot()
summary(aov(eval ~ tenured, beauty))

ggplot(beauty, aes(y=eval, x=minority)) + geom_boxplot()
summary(aov(eval ~ minority, beauty))

# females have lower evaluation on average
ggplot(beauty, aes(y=eval, x=female)) + geom_boxplot()
summary(aov(eval ~ female, beauty))

ggplot(beauty, aes(y=eval, x=beauty)) + geom_point()

ggplot(beauty, aes(y=eval, x=age)) + geom_point()

# when the number of students who complete evaluation is high, rating tends to be high.
ggplot(beauty, aes(y=eval, x=didevaluation)) + geom_point()


ggplot(beauty, aes(y=eval, x=formal)) + geom_boxplot()
summary(aov(eval ~ formal, beauty))


ggplot(beauty, aes(y=eval, x=lower)) + geom_boxplot()
summary(aov(eval ~ lower, beauty))


ggplot(beauty, aes(y=eval, x=multipleclass)) + geom_boxplot()
summary(aov(eval ~ multipleclass, beauty))

# professor with undergrad in non-English speaking country tend to have lower evaluation score
ggplot(beauty, aes(y=eval, x=nonenglish)) + geom_boxplot()
summary(aov(eval ~ nonenglish, beauty))


ggplot(beauty, aes(y=eval, x=onecredit)) + geom_boxplot()
summary(aov(eval ~ onecredit, beauty))


# one credit courses have significantly higher evaluation score
ggplot(beauty, aes(y=eval, x=female)) + geom_boxplot()
summary(aov(eval ~ female, beauty))

# tenured professors get lower evaluation score on average
ggplot(beauty, aes(y=eval, x=tenuretrack)) + geom_boxplot()
summary(aov(eval ~ tenuretrack, beauty))


ggplot(beauty, aes(y=eval, x=profevaluation)) + geom_point()


ggplot(beauty, aes(y=eval, x=students)) + geom_point()


ggplot(beauty, aes(y=eval, x=percentevaluating)) + geom_point()



# 5: varying intercept model
intercept.model <- lmer(eval ~ (1 | profnumber) + beauty, data = beauty) 
summary(intercept.model)

coef(intercept.model)
confint(intercept.model)
fixef(intercept.model)
ranef(intercept.model)


# plot results
min_b <- min(beauty$beauty)
max_b<- max(beauty$beauty)
bucket <- (max(beauty$beauty)-min(beauty$beauty))/4
  
beauty.vector <- seq(min_b, max_b, by=bucket)

newdata <- data.frame(profnumber=rep(unique(beauty$profnumber), each=5),
                      beauty=rep(beauty.vector, 94))

newdata$profnumber <- as.factor(newdata$profnumber)

newdata$pred <- predict(intercept.model, newdata=newdata, type="response")

ggplot(newdata, aes(x = beauty, y = pred, colour = profnumber)) +
  geom_line(size = 0.7) +
  labs(x = "Beauty", y = "Predicted Course Rating") +
  theme(legend.position="none") +
  geom_text(data = newdata, aes(label = profnumber), hjust = 0.5, vjust = 1)

# 6
# female
intercept.model <- lmer(eval ~ (1 | profnumber) + female, data = beauty) 
summary(intercept.model)

# non-english
intercept.model <- lmer(eval ~ (1 | profnumber) + nonenglish, data = beauty) 
summary(intercept.model)

# onecredit
intercept.model <- lmer(eval ~ (1 | profnumber) + onecredit, data = beauty) 
summary(intercept.model)

# tenuretrack
intercept.model <- lmer(eval ~ (1 | profnumber) + tenuretrack, data = beauty) 
summary(intercept.model)

# combined model - female + nonenglish + onecredit
intercept.model <- lmer(eval ~ (1 | profnumber) + beauty + female + nonenglish + onecredit, data = beauty) 
summary(intercept.model)

coef(intercept.model)
confint(intercept.model)
fixef(intercept.model)
ranef(intercept.model)

qqnorm(residuals(intercept.model))

# 7

intercept.model.1 <- lmer(eval ~ (1 | profnumber) + beauty, data = beauty) 
summary(intercept.model.1)

# plot results
min_b <- min(beauty$beauty)
max_b<- max(beauty$beauty)
bucket <- (max(beauty$beauty)-min(beauty$beauty))/4

beauty.vector <- seq(min_b, max_b, by=bucket)

newdata <- data.frame(profnumber=rep(unique(beauty$profnumber), each=5),
                      beauty=rep(beauty.vector, 94))

newdata$profnumber <- as.factor(newdata$profnumber)

newdata$pred <- predict(intercept.model.1, newdata=newdata, type="response")

p1 <- ggplot(newdata, aes(x = beauty, y = pred, colour = profnumber)) +
  geom_line(size = 0.7) +
  labs(x = "Beauty", y = "Predicted Course Rating") +
  theme(legend.position="none") +
  geom_text(data = newdata, aes(label = profnumber), hjust = 0.5, vjust = 1)






intercept.model.2 <- lmer(eval ~ (1 | courseID) + beauty, data = beauty) 
summary(intercept.model.2)


min_b <- min(beauty$beauty)
max_b<- max(beauty$beauty)
bucket <- (max(beauty$beauty)-min(beauty$beauty))/4

beauty.vector <- seq(min_b, max_b, by=bucket)

newdata <- data.frame(courseID=rep(unique(beauty$courseID), each=5),
                      beauty=rep(beauty.vector, 31))

newdata$courseID <- as.factor(newdata$courseID)

newdata$pred <- predict(intercept.model.2, newdata=newdata, type="response")

p2 <- ggplot(newdata, aes(x = beauty, y = pred, colour = courseID)) +
  geom_line(size = 0.7) +
  labs(x = "Beauty", y = "Predicted Course Rating") +
  theme(legend.position="none") +
  geom_text(data = newdata, aes(label = courseID), hjust = 0.5, vjust = 1)


grid.arrange(p1,p2, ncol = 2)


