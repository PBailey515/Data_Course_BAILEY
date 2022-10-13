library(tidyverse)
df<-read_csv("./Data/Utah_Religions_by_County.csv")
view(df)
df %>% ggplot(aes(x=County,y=LDS,)) + geom_point() 
# Made a religion colum and put the percent of population that is that religion
df1<-df %>% pivot_longer(c("Assemblies of God", "Episcopal Church","Pentecostal Church of God", "Greek Orthodox", "LDS", "Southern Baptist Convention","United Methodist Church","Buddhism-Mahayana", "Catholic","Evangelical","Muslim", "Non Denominational", "Orthodox"),
                         names_to = "Religion", values_to = "percent_pop_religion")
#THis plot tells me nothing 

names(df1)
df1 %>%  ggplot(aes(x=County,y=percent_pop_religion)) + geom_point()

#percent of pop by county 
df1 %>% pivot_wider(names_from = County, values_from = percent_pop_religion) %>% view()

#who is the most religious county 
df1 %>% group_by(County,Religious) %>% ggplot(aes(x=County,y=Religious)) +geom_col() + ggeasy::easy_rotate_x_labels(angle= 90)


# shows diversity of the religious communities by county ie 3 counties only has lds people
#Shows of teh relgious pop the most dominant is lds
df1 %>% group_by(County,percent_pop_religion,Religion) %>% ggplot(aes(x=County,y=percent_pop_religion,fill=Religion)) +geom_col() +ggeasy::easy_rotate_x_labels(angle=90)


#give county name it retruns religion of max proportion showed this to us in class
max_religion <- function(df1,County){
  df1 %>% filter(County==County) %>% 
    filter(percent_pop_religion==max(percent_pop_religion)) %>% 
    pluck("Religion")
}
max_religion(df1,"Beaver County")             
max_religion(df1,"Sanpete County")  

max_religion(df1,"Salt Lake County")



df1 %>% ggplot(aes(x=percent_pop_religion,y=Pop_2010,color=County))+ facet_wrap(~Religion) + geom_point()

df1 %>% ggplot(aes(x=percent_pop_religion,y=`Non-Religious`,color=Religion)) + geom_point() +facet_wrap(~County)
# i tried using summarise function but i dont think i need too, also this plot is bad 
df1 %>% summarise(N=n(),Religion=Religion, County=County,percent_pop_religion=percent_pop_religion,N=percent_pop_religion/`Non-Religious`) %>% ggplot(aes(x="N",y=percent_pop_religion)) + geom_point() 
df1 %>% summarise(N=n(),N=Pop_2010/percent_pop_religion, Religion=Religion,County=County) %>% ggplot(aes(x=Religion,y=N)) +geom_point()
# population X percent pop religion but this just shwos the number of religious people are LDS
df1 %>% ggplot(aes(x=percent_pop_religion,y=Pop_2010, color= County)) + geom_point() + geom_smooth() + facet_wrap(~Religion)

# population versus percent pop of any religion, i dont see a correlation between populaiton and percent pop of religions
df1 %>% ggplot(aes(x=Pop_2010,y=percent_pop_religion,color=County)) + geom_smooth() + geom_point() +facet_wrap(~Religion) 

#percent_pop_religion vs non religious it almost looks like the more non-religious people the less LDS meaning the non religious are Ex-LDS possibly 
#which makes sense
df1 %>% ggplot(aes(x=`Non-Religious`,y=percent_pop_religion,color=County)) +facet_wrap(~Religion) +geom_point() 

#percent pop religion vs religious this shows the more religious people there are the more lds people they are
 df1 %>% ggplot(aes(x=Religious,y=percent_pop_religion,color=County)) + facet_wrap(~Religion)+ geom_point() 

#pop vs religiois 
df1 %>% ggplot(aes(x=Religious, y= Pop_2010,color=County)) + geom_point()+facet_wrap(~Religion)

