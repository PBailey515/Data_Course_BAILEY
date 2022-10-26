library(tidyverse)
library(stringr)
library(dplyr)
library(modelr)
library(easystats)
library(broom)
#read in the Data 10 pts 
df <- read_csv("Data/unicef-u5mr.csv")

glimpse(df)
view(df)
#tidy data and got rid of the prefix in year column
df1 <- df %>% pivot_longer(c("U5MR.1950" ,  "U5MR.1951" ,  "U5MR.1952" ,  "U5MR.1953" ,  "U5MR.1954" ,  "U5MR.1955" ,  "U5MR.1956" , 
                       "U5MR.1957",   "U5MR.1958" ,  "U5MR.1959" ,  "U5MR.1960" ,  "U5MR.1961" ,  "U5MR.1962"  , "U5MR.1963"  , "U5MR.1964",  
                      "U5MR.1965"  , "U5MR.1966" ,  "U5MR.1967"  , "U5MR.1968" ,  "U5MR.1969" ,  "U5MR.1970" ,  "U5MR.1971" ,  "U5MR.1972" , 
                       "U5MR.1973" ,  "U5MR.1974" ,  "U5MR.1975"  , "U5MR.1976" ,  "U5MR.1977" ,  "U5MR.1978"  , "U5MR.1979" ,  "U5MR.1980",
                       "U5MR.1981",   "U5MR.1982" ,  "U5MR.1983" ,  "U5MR.1984"  , "U5MR.1985" ,  "U5MR.1986"  , "U5MR.1987" ,  "U5MR.1988",  
                      "U5MR.1989" ,  "U5MR.1990" ,  "U5MR.1991" ,  "U5MR.1992" ,  "U5MR.1993"  , "U5MR.1994" ,  "U5MR.1995" ,  "U5MR.1996" , 
                       "U5MR.1997" ,  "U5MR.1998",   "U5MR.1999" ,  "U5MR.2000"  , "U5MR.2001" ,  "U5MR.2002" ,  "U5MR.2003" ,  "U5MR.2004" , 
                       "U5MR.2005" ,  "U5MR.2006" ,  "U5MR.2007" ,  "U5MR.2008" ,  "U5MR.2009" ,  "U5MR.2010" ,  "U5MR.2011",   "U5MR.2012",  
                       "U5MR.2013",   "U5MR.2014" ,  "U5MR.2015"),names_to= "Year", values_to= "u5_mortality_rate",names_prefix = "U5MR.") 
#checked to make sure it looked good
view(df1)
names(df1)
str(df1)
as.numeric(df1$Year)
#mutate year into a numeric so i can plot it correctly, changed the labels to Year and u5 mortality rate
df2<- df1 %>% mutate(as.numeric(Year))
#graph1
plot1<-df2 %>% group_by(CountryName) %>% ggplot(aes(x=`as.numeric(Year)`,y=u5_mortality_rate))+ theme_minimal() + labs(x="Year",y="u5_mortality_rate")+geom_path() +
  facet_wrap(~Continent)
#see where i am at 
getwd()
#save in output in my exams 2 folder
ggsave("BAILEY_Plot_1.png",plot = plot1,path = "Output",device = png)
# this is where i need to start figuring out how to do the mean
view(df2)
#gets rid of NA
df3<- df2 %>% na.omit()
#THis plot looks bad and did not work so i commented it out
#df3 %>% 
  #group_by(Continent,u5_mortality_rate,`as.numeric(Year)`) %>% 
 # summarise(mean_u5mr=mean(u5_mortality_rate)) %>% 
  #unique.data.frame() %>% ggplot(aes(x=`as.numeric(Year)`,y=mean_u5mr,color=Continent)) + geom_path() 
  # another not so good graph but it was a part of the thought process
#df3 %>% 
  #group_by(`as.numeric(Year)`,u5_mortality_rate,Continent) %>% 
  #summarise(max_u5mr=max(u5_mortality_rate)) %>% 
  #ggplot(aes(x=`as.numeric(Year)`,y=max_u5mr,color=Continent)) + geom_line() 
  
  #THIS GIVES me the mean of the year 1950 in africa 
  df3 %>% filter(Continent %in% "Africa") %>% 
    filter(`as.numeric(Year)`%in% "1951") %>% 
    summarise(mmr=mean(u5_mortality_rate))
#function to do the above 
  f1 <- function(x,var1,var2){
    x %>% filter(Continent %in% var1) %>% 
      filter(`as.numeric(Year)`%in% var2) %>% 
      summarise(mean_u5=mean(u5_mortality_rate))
  }
  #using my function
  f1(df3,"Africa","1950") 
    f1(df3,"Africa","1951") %>% tidy()
 # making a df that has all the mean of aftica of every year
  df_clean <- df3
    Africa<-c(f1(df_clean,"Africa","1950"),f1(df_clean,"Africa","1951"),f1(df_clean,"Africa","1952"),f1(df_clean,"Africa","1953"),f1(df_clean,"Africa","1954"),f1(df_clean,"Africa","1955"),
              f1(df_clean,"Africa","1956"),f1(df_clean,"Africa","1957"),f1(df_clean,"Africa","1958"),f1(df_clean,"Africa","1959"),f1(df_clean,"Africa","1960"),
              f1(df_clean,"Africa","1961"),f1(df_clean,"Africa","1962"),f1(df_clean,"Africa","1963"),f1(df_clean,"Africa","1964"),f1(df_clean,"Africa","1965"),
              f1(df_clean,"Africa","1966"),f1(df_clean,"Africa","1967"),f1(df_clean,"Africa","1968"),f1(df_clean,"Africa","1969"),f1(df_clean,"Africa","1970"),
              f1(df_clean,"Africa","1971"),f1(df_clean,"Africa","1972"),f1(df_clean,"Africa","1973"),f1(df_clean,"Africa","1974"),f1(df_clean,"Africa","1975"),
              f1(df_clean,"Africa","1976"),f1(df_clean,"Africa","1977"),f1(df_clean,"Africa","1978"),f1(df_clean,"Africa","1979"),f1(df_clean,"Africa","1980"),
              f1(df_clean,"Africa","1981"),f1(df_clean,"Africa","1982"),f1(df_clean,"Africa","1983"),f1(df_clean,"Africa","1984"),f1(df_clean,"Africa","1985"),
              f1(df_clean,"Africa","1986"),f1(df_clean,"Africa","1987"),f1(df_clean,"Africa","1988"),f1(df_clean,"Africa","1989"),f1(df_clean,"Africa","1990"),
              f1(df_clean,"Africa","1991"),f1(df_clean,"Africa","1992"),f1(df_clean,"Africa","1993"),f1(df_clean,"Africa","1994"),f1(df_clean,"Africa","1995"),
              f1(df_clean,"Africa","1996"),f1(df_clean,"Africa","1997"),f1(df_clean,"Africa","1998"),f1(df_clean,"Africa","1999"),f1(df_clean,"Africa","2000"),
              f1(df_clean,"Africa","2001"),f1(df_clean,"Africa","2002"),f1(df_clean,"Africa","2003"),f1(df_clean,"Africa","2004"),f1(df_clean,"Africa","2005"),
              f1(df_clean,"Africa","2006"),f1(df_clean,"Africa","2007"),f1(df_clean,"Africa","2008"),f1(df_clean,"Africa","2009"),f1(df_clean,"Africa","2010"),
              f1(df_clean,"Africa","2011"),f1(df_clean,"Africa","2012"),f1(df_clean,"Africa","2013"),f1(df_clean,"Africa","2014"),f1(df_clean,"Africa","2015")) %>% as.data.frame()
    # clean data 
    Africa2<- Africa %>% pivot_longer(cols = starts_with("mean"),names_to = "contintent", values_to = "mean_u5_mort") 
      #add columns
      africa_name <- rep("Africa",66)
      Africa2$Continent <- africa_name
    #add year column
      africa_year<- c(1950:2015)
      Africa2$Year <- africa_year
      
    #same thing for asia
    Asia<-c(f1(df_clean,"Asia","1950"),f1(df_clean,"Asia","1951"),f1(df_clean,"Asia","1952"),f1(df_clean,"Asia","1953"),f1(df_clean,"Asia","1954"),f1(df_clean,"Asia","1955"),
            f1(df_clean,"Asia","1956"),f1(df_clean,"Asia","1957"),f1(df_clean,"Asia","1958"),f1(df_clean,"Asia","1959"),f1(df_clean,"Asia","1960"),
            f1(df_clean,"Asia","1961"),f1(df_clean,"Asia","1962"),f1(df_clean,"Asia","1963"),f1(df_clean,"Asia","1964"),f1(df_clean,"Asia","1965"),
            f1(df_clean,"Asia","1966"),f1(df_clean,"Asia","1967"),f1(df_clean,"Asia","1968"),f1(df_clean,"Asia","1969"),f1(df_clean,"Asia","1970"),
            f1(df_clean,"Asia","1971"),f1(df_clean,"Asia","1972"),f1(df_clean,"Asia","1973"),f1(df_clean,"Asia","1974"),f1(df_clean,"Asia","1975"),
            f1(df_clean,"Asia","1976"),f1(df_clean,"Asia","1977"),f1(df_clean,"Asia","1978"),f1(df_clean,"Asia","1979"),f1(df_clean,"Asia","1980"),
            f1(df_clean,"Asia","1981"),f1(df_clean,"Asia","1982"),f1(df_clean,"Asia","1983"),f1(df_clean,"Asia","1984"),f1(df_clean,"Asia","1985"),
            f1(df_clean,"Asia","1986"),f1(df_clean,"Asia","1987"),f1(df_clean,"Asia","1988"),f1(df_clean,"Asia","1989"),f1(df_clean,"Asia","1990"),
            f1(df_clean,"Asia","1991"),f1(df_clean,"Asia","1992"),f1(df_clean,"Asia","1993"),f1(df_clean,"Asia","1994"),f1(df_clean,"Asia","1995"),
            f1(df_clean,"Asia","1996"),f1(df_clean,"Asia","1997"),f1(df_clean,"Asia","1998"),f1(df_clean,"Asia","1999"),f1(df_clean,"Asia","2000"),
            f1(df_clean,"Asia","2001"),f1(df_clean,"Asia","2002"),f1(df_clean,"Asia","2003"),f1(df_clean,"Asia","2004"),f1(df_clean,"Asia","2005"),
            f1(df_clean,"Asia","2006"),f1(df_clean,"Asia","2007"),f1(df_clean,"Asia","2008"),f1(df_clean,"Asia","2009"),f1(df_clean,"Asia","2010"),
            f1(df_clean,"Asia","2011"),f1(df_clean,"Asia","2012"),f1(df_clean,"Asia","2013"),f1(df_clean,"Asia","2014"),f1(df_clean,"Asia","2015")) %>% as.data.frame()
    #cleaning data and adding columns 
    Asia2<- Asia %>% pivot_longer(cols = starts_with("mean"),names_to = "contintent", values_to = "mean_u5_mort") 
    #add columns
    asia_name <- rep("Asia",66)
    Asia2$Continent <- asia_name
    #add year column
    asia_year<- c(1950:2015)
    Asia2$Year <- asia_year
    #joining Africa and Aisa into a data frame vertically 
   Africa2_Asia2 <- full_join(y=Africa2,Asia2)
    
    #same thing for the  americas
   Americas<-c(f1(df_clean,"Americas","1950"),f1(df_clean,"Americas","1951"),f1(df_clean,"Americas","1952"),f1(df_clean,"Americas","1953"),f1(df_clean,"Americas","1954"),f1(df_clean,"Americas","1955"),
               f1(df_clean,"Americas","1956"),f1(df_clean,"Americas","1957"),f1(df_clean,"Americas","1958"),f1(df_clean,"Americas","1959"),f1(df_clean,"Americas","1960"),
               f1(df_clean,"Americas","1961"),f1(df_clean,"Americas","1962"),f1(df_clean,"Americas","1963"),f1(df_clean,"Americas","1964"),f1(df_clean,"Americas","1965"),
               f1(df_clean,"Americas","1966"),f1(df_clean,"Americas","1967"),f1(df_clean,"Americas","1968"),f1(df_clean,"Americas","1969"),f1(df_clean,"Americas","1970"),
               f1(df_clean,"Americas","1971"),f1(df_clean,"Americas","1972"),f1(df_clean,"Americas","1973"),f1(df_clean,"Americas","1974"),f1(df_clean,"Americas","1975"),
               f1(df_clean,"Americas","1976"),f1(df_clean,"Americas","1977"),f1(df_clean,"Americas","1978"),f1(df_clean,"Americas","1979"),f1(df_clean,"Americas","1980"),
               f1(df_clean,"Americas","1981"),f1(df_clean,"Americas","1982"),f1(df_clean,"Americas","1983"),f1(df_clean,"Americas","1984"),f1(df_clean,"Americas","1985"),
               f1(df_clean,"Americas","1986"),f1(df_clean,"Americas","1987"),f1(df_clean,"Americas","1988"),f1(df_clean,"Americas","1989"),f1(df_clean,"Americas","1990"),
               f1(df_clean,"Americas","1991"),f1(df_clean,"Americas","1992"),f1(df_clean,"Americas","1993"),f1(df_clean,"Americas","1994"),f1(df_clean,"Americas","1995"),
               f1(df_clean,"Americas","1996"),f1(df_clean,"Americas","1997"),f1(df_clean,"Americas","1998"),f1(df_clean,"Americas","1999"),f1(df_clean,"Americas","2000"),
               f1(df_clean,"Americas","2001"),f1(df_clean,"Americas","2002"),f1(df_clean,"Americas","2003"),f1(df_clean,"Americas","2004"),f1(df_clean,"Americas","2005"),
               f1(df_clean,"Americas","2006"),f1(df_clean,"Americas","2007"),f1(df_clean,"Americas","2008"),f1(df_clean,"Americas","2009"),f1(df_clean,"Americas","2010"),
               f1(df_clean,"Americas","2011"),f1(df_clean,"Americas","2012"),f1(df_clean,"Americas","2013"),f1(df_clean,"Americas","2014"),f1(df_clean,"Americas","2015")) %>% as.data.frame()
   # tidy and add columsn
   Americas2<- Americas %>% pivot_longer(cols = starts_with("mean"),names_to = "contintent", values_to = "mean_u5_mort") 
   #add columns
   americas_name <- rep("Americas",66)
   Americas2$Continent <- americas_name
   #add year column
   americas_year<- c(1950:2015)
   Americas2$Year <- americas_year
   #joining Africa and Aisa and americas into a data frame vertically 
   Africa2_Asia2_Americas2 <- full_join(y=Africa2_Asia2,Americas2)
  
   
    #oceanas
   Oceania<-c(f1(df_clean,"Oceania","1950"),f1(df_clean,"Oceania","1951"),f1(df_clean,"Oceania","1952"),f1(df_clean,"Oceania","1953"),f1(df_clean,"Oceania","1954"),f1(df_clean,"Oceania","1955"),
              f1(df_clean,"Oceania","1956"),f1(df_clean,"Oceania","1957"),f1(df_clean,"Oceania","1958"),f1(df_clean,"Oceania","1959"),f1(df_clean,"Oceania","1960"),
              f1(df_clean,"Oceania","1961"),f1(df_clean,"Oceania","1962"),f1(df_clean,"Oceania","1963"),f1(df_clean,"Oceania","1964"),f1(df_clean,"Oceania","1965"),
              f1(df_clean,"Oceania","1966"),f1(df_clean,"Oceania","1967"),f1(df_clean,"Oceania","1968"),f1(df_clean,"Oceania","1969"),f1(df_clean,"Oceania","1970"),
              f1(df_clean,"Oceania","1971"),f1(df_clean,"Oceania","1972"),f1(df_clean,"Oceania","1973"),f1(df_clean,"Oceania","1974"),f1(df_clean,"Oceania","1975"),
              f1(df_clean,"Oceania","1976"),f1(df_clean,"Oceania","1977"),f1(df_clean,"Oceania","1978"),f1(df_clean,"Oceania","1979"),f1(df_clean,"Oceania","1980"),
              f1(df_clean,"Oceania","1981"),f1(df_clean,"Oceania","1982"),f1(df_clean,"Oceania","1983"),f1(df_clean,"Oceania","1984"),f1(df_clean,"Oceania","1985"),
              f1(df_clean,"Oceania","1986"),f1(df_clean,"Oceania","1987"),f1(df_clean,"Oceania","1988"),f1(df_clean,"Oceania","1989"),f1(df_clean,"Oceania","1990"),
              f1(df_clean,"Oceania","1991"),f1(df_clean,"Oceania","1992"),f1(df_clean,"Oceania","1993"),f1(df_clean,"Oceania","1994"),f1(df_clean,"Oceania","1995"),
              f1(df_clean,"Oceania","1996"),f1(df_clean,"Oceania","1997"),f1(df_clean,"Oceania","1998"),f1(df_clean,"Oceania","1999"),f1(df_clean,"Oceania","2000"),
              f1(df_clean,"Oceania","2001"),f1(df_clean,"Oceania","2002"),f1(df_clean,"Oceania","2003"),f1(df_clean,"Oceania","2004"),f1(df_clean,"Oceania","2005"),
              f1(df_clean,"Oceania","2006"),f1(df_clean,"Oceania","2007"),f1(df_clean,"Oceania","2008"),f1(df_clean,"Oceania","2009"),f1(df_clean,"Oceania","2010"),
              f1(df_clean,"Oceania","2011"),f1(df_clean,"Oceania","2012"),f1(df_clean,"Oceania","2013"),f1(df_clean,"Oceania","2014"),f1(df_clean,"Oceania","2015")) %>% as.data.frame()
   # tidy and add columsn
   Oceania2<- Oceania %>% pivot_longer(cols = starts_with("mean"),names_to = "contintent", values_to = "mean_u5_mort") 
   #add columns
   oceania_name <- rep("Oceania",66)
   Oceania2$Continent <- oceania_name
   #add year column
   oceania_year<- c(1950:2015)
   Oceania2$Year <- oceania_year
   #joining Africa and Aisa and americas into a data frame vertically 
   AAAO2 <- full_join(y=Africa2_Asia2_Americas2,Oceania2)
   
   Europe<-c(f1(df_clean,"Europe","1950"),f1(df_clean,"Europe","1951"),f1(df_clean,"Europe","1952"),f1(df_clean,"Europe","1953"),f1(df_clean,"Europe","1954"),f1(df_clean,"Europe","1955"),
             f1(df_clean,"Europe","1956"),f1(df_clean,"Europe","1957"),f1(df_clean,"Europe","1958"),f1(df_clean,"Europe","1959"),f1(df_clean,"Europe","1960"),
             f1(df_clean,"Europe","1961"),f1(df_clean,"Europe","1962"),f1(df_clean,"Europe","1963"),f1(df_clean,"Europe","1964"),f1(df_clean,"Europe","1965"),
             f1(df_clean,"Europe","1966"),f1(df_clean,"Europe","1967"),f1(df_clean,"Europe","1968"),f1(df_clean,"Europe","1969"),f1(df_clean,"Europe","1970"),
             f1(df_clean,"Europe","1971"),f1(df_clean,"Europe","1972"),f1(df_clean,"Europe","1973"),f1(df_clean,"Europe","1974"),f1(df_clean,"Europe","1975"),
             f1(df_clean,"Europe","1976"),f1(df_clean,"Europe","1977"),f1(df_clean,"Europe","1978"),f1(df_clean,"Europe","1979"),f1(df_clean,"Europe","1980"),
             f1(df_clean,"Europe","1981"),f1(df_clean,"Europe","1982"),f1(df_clean,"Europe","1983"),f1(df_clean,"Europe","1984"),f1(df_clean,"Europe","1985"),
             f1(df_clean,"Europe","1986"),f1(df_clean,"Europe","1987"),f1(df_clean,"Europe","1988"),f1(df_clean,"Europe","1989"),f1(df_clean,"Europe","1990"),
             f1(df_clean,"Europe","1991"),f1(df_clean,"Europe","1992"),f1(df_clean,"Europe","1993"),f1(df_clean,"Europe","1994"),f1(df_clean,"Europe","1995"),
             f1(df_clean,"Europe","1996"),f1(df_clean,"Europe","1997"),f1(df_clean,"Europe","1998"),f1(df_clean,"Europe","1999"),f1(df_clean,"Europe","2000"),
             f1(df_clean,"Europe","2001"),f1(df_clean,"Europe","2002"),f1(df_clean,"Europe","2003"),f1(df_clean,"Europe","2004"),f1(df_clean,"Europe","2005"),
             f1(df_clean,"Europe","2006"),f1(df_clean,"Europe","2007"),f1(df_clean,"Europe","2008"),f1(df_clean,"Europe","2009"),f1(df_clean,"Europe","2010"),
             f1(df_clean,"Europe","2011"),f1(df_clean,"Europe","2012"),f1(df_clean,"Europe","2013"),f1(df_clean,"Europe","2014"),f1(df_clean,"Europe","2015")) %>% as.data.frame()
   # tidy and add columsn
   Europe2<- Europe %>% pivot_longer(cols = starts_with("mean"),names_to = "contintent", values_to = "mean_u5_mort") 
   #add columns
   europe_name <- rep("Europe",66)
   Europe2$Continent <- europe_name
   #add year column
   europe_year<- c(1950:2015)
   Europe2$Year <- europe_year
   #joining Africa and Aisa and americas into a data frame vertically 
   AAAOE2 <- full_join(y=AAAO2,Europe2)
    
   #graph 
   plot2 <- AAAOE2 %>% ggplot(aes(x=Year,y=mean_u5_mort,color=Continent)) + geom_line() 
    #save graph
   ggsave("BAILEY_Plot_2.png",plot = plot2,path = "Output",device = png)
  
   
  #mod 1 only year 
  mod1 <- glm(formula= u5_mortality_rate~Year,data=df3)
# shows me how the model is 
  summary(mod1)
  #year and continent
mod2 <- glm(formula= u5_mortality_rate~Year+Continent,data=df3)  
#mod3 year*contintent 
mod3 <- glm(formula=u5_mortality_rate~Year*Continent,data=df3)
#compare models
compare_performance(mod1,mod2,mod3,rank=TRUE)
# model 3 is the best it has the highest R^2 , it also has a performance score of 80.0%

#mod 1 graph
add_predictions(df3,mod1) %>% ggplot(aes(x=as.numeric(Year),y=pred,color=Continent)) +geom_smooth(method="lm") 

#mod2 graph
add_predictions(df3,mod2) %>% ggplot(aes(x=as.numeric(Year),y=pred,color=Continent)) +geom_smooth(method="lm") 

#mod3
add_predictions(df3,mod3) %>% ggplot(aes(x=as.numeric(Year),y=pred,color=Continent)) +geom_smooth(method = "lm")
# makes a vector that repeats 3 x times
model3 <- rep(3,10244)
predictivedf3 <- add_predictions(df3,mod3) 
# add a clomun
predictivedf3$model3<- model3
#mod2
predictivedf2 <- add_predictions(df3,mod2)

#make vectro 
model2<-rep(2,10244)
#add column
predictivedf2$model2<-model2

#mod1
predictivedf1 <-add_predictions(df3,mod1) 
model1<-rep(1,10244)
#add column
predictivedf1$model1<-model1
# combining all the model into the df vertically
predf <- full_join(y = predictivedf1,predictivedf2)

pdf <- full_join(y=predf,predictivedf3)

#clean up pdf
clean_pdf <- pdf %>% pivot_longer(c("model1","model2","model3"),names_to = "model", values_to = "mod") %>% na.omit() 
#graph of my predictions facetwrapped by model
clean_pdf %>% ggplot(aes(x=as.numeric(Year),y=pred,color=Continent)) + facet_wrap(~model)+geom_smooth(method = "lm") +
  labs(x="Year",y="Predicted_U5_MR")
#Bonus i tried for like 3 hours but i coulfnt get it right 

#View(clean_pdf)
#clean_pdf %>% group_by(CountryName== "Ecuador") %>% view()

 #predict(mod4, df4) %>% as.data.frame()
 
 #df4$Year <- as.factor(df4$Year)
 #str(df4)
 
 #mod4 <- glm(formula=u5_mortality_rate~Year*Continent,data=df4)

 #bonus_years <- c(1950:2020)
 