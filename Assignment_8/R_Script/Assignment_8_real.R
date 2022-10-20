library(tidyverse)
library(modelr)
library(broom)
library(easystats)
library(fitdistrplus)
# read in the mushroom growth 
mush<- read_csv("./Data/mushroom_growth.csv")
names(mush)
#response= growthrate
#predictors= humididty, nitrogen,light 
#first model with all the variables
mod1 <- glm(GrowthRate ~ Temperature + Humidity + Light,data = mush)
mean(mod1$residuals^2)
summary(mod1)
# humidity and light look important
mush %>%
  ggplot(aes(x=Nitrogen,y=GrowthRate, color=Species))+
  geom_smooth(se=FALSE)+
  facet_wrap(~Humidity)+
  theme_minimal()

#this plot shows me it was based on high and low humidity
ggplot(mush,aes(y=GrowthRate, x= Humidity,))+geom_point()
#THis plot facetwrapped the humidity and i can see with lots of light
#and high humidity there is more growth 
ggplot(mush,aes(y=GrowthRate, x= Light, color=Species))+geom_point() + facet_wrap(~Humidity)
# seeing what is relevant 
#Species looks like it effects

# mod with only the significant stuff from mod 1 
mod2 <- glm(GrowthRate~ Humidity + Light+ Species,data = mush)
compare_performance(mod1,mod2)
summary(mod2)
#mean square mod2
mean(mod2$residuals^2)

# mod 2 is slightly better because it has a lower AIC and slightly higher R^2

# i did this to see what lm does 
mod3 <- lm(GrowthRate~ Humidity + Light,data = mush)
#mean square is bigger than mod2 
mean(mod3$residuals^2)

compare_performance(mod1,mod2,mod3)

#species is very important 
mush %>% ggplot(aes(x=Light,y=GrowthRate,color=Humidity)) + geom_point()+ facet_wrap(~Species)
names(mush)
mod4 <- glm(GrowthRate~ Species *Light,data = mush)
summary(mod4)
tidy(mod4)
tidy(mod1)
tidy(mod2)
compare_performance(mod1,mod2,mod3,mod4)

mush %>% ggplot(aes(x=GrowthRate,y=Nitrogen,color=Species))  +geom_point()     

mod5 <- glm(GrowthRate ~ Nitrogen+ Species+ Humidity+Light,data=mush)
summary(mod5)            
step <- MASS::stepAIC(mod2)

mod6 <- glm(data=mush,formula = step$formula)
compare_performance(mod2,mod4,mod5,mod6) %>% plot
# i still think Mod 2 is the best 
glimpse(mush)
ggplot(mush,aes(x=Nitrogen,y=GrowthRate,color=Species)) + geom_point() 

mod7 <- glm(data=mush,GrowthRate~ Nitrogen * Temperature)
compare_performance(mod8,mod2)
mean(mod7$residuals^2) #this residual is way to high 
mush %>% ggplot(aes(x=Nitrogen,y=GrowthRate),color=Humidity) +facet_wrap(~Species) +geom_point()

mod8 <- glm(data=mush, formula= GrowthRate~ .^2)
step2 <- MASS::stepAIC(mod8)
step$formula

mod9 <- glm(data=mush,formula=step$formula)

#meansquare of each model 
#mod1
mean(mod1$residuals^2)
#mod2
mean(mod2$residuals^2)
#mod3
mean(mod3$residuals^2)
#mod4
mean(mod4$residuals^2)
#mod5
mean(mod5$residuals^2)
#mod6
mean(mod6$residuals^2)
#mod7
mean(mod7$residuals^2)
#mod8
mean(mod8$residuals^2)
#mod9
mean(mod9$residuals^2)

#selects best model that we tried it will rank them from best to worst
compare_performance(mod1,mod2,mod3,mod4,mod5,mod6,mod7,mod8,mod9,rank = TRUE)
#mod8 was my best 

#add predictions on my best model mod2
add_predictions(mush,mod8)

#graph
add_predictions(mush,mod8) %>%   ggplot(aes(y=GrowthRate,x=Temperature,color=Humidity)) + facet_wrap(~Species)+ geom_smooth() + 
  geom_smooth(aes(y=pred),color="black") 
#canvas stuff
summary(mod8)

#awnsering the questions
df1 <- read_csv("../Data/non_linear_relationship.csv")

df1 %>% ggplot(aes(x=predictor,y=log(response))) + geom_point() + geom_smooth()

df1 %>% ggplot(aes(x=predictor,y=(response))) + geom_point() + geom_smooth()

df1 %>% glm(formula=log(response)~ predictor,family = "")

view(df1)
df1 %>% mutate(response=case_when(response>2 ~ TRUE, TRUE~FALSE)) %>% glm(data = .,formula=response ~predictor ,
                                                                          family="binomial") %>% add_predictions(data=df1,type="response") %>% 
  ggplot(aes(x=pred,y=response))+ geom_point()
 