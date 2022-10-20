library(tidyverse)
library(modelr)
library(broom)
library(easystats)
library(fitdistrplus)
data("mtcars")
glimpse(mtcars)
names(mtcars)
#response= mpg
#first model  mpg ~ disp
mod1 <-lm(mpg~disp,data= mtcars)
#this gives me statistics
summary(mod1)
#plot for the mod1
ggplot(mtcars, aes(x=disp,y=mpg)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  theme_minimal()
#mod2 mpg~ qsec
mod2 = lm(mpg ~ qsec, data = mtcars)
ggplot(mtcars, aes(x=disp,y=qsec)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  theme_minimal()
#compare the models 
compare_performance(mod1,mod2)
#mod 2 is worse because the aic is bigger so more complex, and r^2 is smaller 
# we used mod1 selected the columns mpg and pred 
df <- mtcars %>% 
  add_predictions(mod1) 
df %>% dplyr::select("mpg","pred")
#how to make predicitons
newdf = data.frame(disp = c(500,600,700,800,900))

#predict
pred = predict(mod1, newdata = newdf)

hyp_preds <- data.frame(disp = newdf$disp,
                        pred = pred)
df$PredictionType <- "Real"
hyp_preds$PredictionType <- "Hypothetical"
fullpreds <- full_join(df,hyp_preds)
ggplot(fullpreds,aes(x=disp,y=pred,color=PredictionType)) +
  geom_point() +
  geom_point(aes(y=mpg),color="Black") +
  theme_minimal()


mod3 <- glm(data=mtcars,
            formula = mpg ~ hp + disp + factor(am) + qsec)
mods <- list(mod1=mod1,mod2=mod2,mod3=mod3)
map(mods,performance) %>% reduce(full_join)


mtcars %>% 
  gather_residuals(mod1,mod2,mod3) %>% 
  ggplot(aes(x=model,y=resid,fill=model)) +
  geom_boxplot(alpha=.5) +
  geom_point() + 
  theme_minimal()

mtcars %>% 
  gather_predictions(mod1,mod2,mod3) %>% 
  ggplot(aes(x=disp,y=mpg)) +
  geom_point(size=3) +
  geom_point(aes(y=pred,color=model)) +
  geom_smooth(aes(y=pred,color=model)) +
  theme_minimal() +
  annotate("text",x=250,y=32,label=mod1$call) +
  annotate("text",x=250,y=30,label=mod2$call) +
  annotate("text",x=250,y=28,label=mod3$call)
