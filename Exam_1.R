library(tidyverse)
library(patchwork)
library(dplyr)
ccd<-read_csv("BIOL3100_Exams/Exam_1/cleaned_covid_data.csv") # read in
# only States with A 
A_States<-ccd[grepl("A",ccd$Province_State),]
#plot of A states Deaths vs Last update 
A_States %>% 
  ggplot(aes(x=Last_Update,y=Deaths))+geom_point()+geom_smooth(method=lm,se=FALSE)+facet_wrap(~A_States$Province_State)
view(ccd)
#Each State
State_Al<-ccd %>% filter(Province_State=="Alaska")
State_AlA<- ccd   %>% filter(Province_State=="Alabama")
State_Ar<- ccd   %>% filter(Province_State=="Arizona")
State_ARK<- ccd %>% filter(Province_State== "Arkansas")
State_Ca<- ccd   %>% filter(Province_State=="California")
State_Co<- ccd   %>% filter(Province_State=="Colorado")
State_Con<- ccd   %>% filter(Province_State=="Connecticut")
State_De<- ccd   %>% filter(Province_State=="Delaware")
State_DoC<- ccd   %>% filter(Province_State=="District of Columbia")
State_Fl<- ccd   %>% filter(Province_State=="Florida")
State_Ge<- ccd   %>% filter(Province_State=="Georgia")
State_Ha<- ccd   %>% filter(Province_State=="Hawaii")
State_Ill<- ccd   %>% filter(Province_State=="Illinois")
State_Id<- ccd   %>% filter(Province_State=="Idaho")
State_In<- ccd   %>% filter(Province_State=="Indiana")
State_Io<- ccd   %>% filter(Province_State=="Iowa")
State_Ka<- ccd   %>% filter(Province_State=="Kansas")
State_Ke<- ccd   %>% filter(Province_State=="Kentucky")
State_Lo<- ccd   %>% filter(Province_State=="Louisiana")
State_Ma<- ccd   %>% filter(Province_State=="Maine")
State_Mar<- ccd   %>% filter(Province_State=="Maryland")
State_Mas<- ccd   %>% filter(Province_State=="Massachusetts")
State_Mi<- ccd   %>% filter(Province_State=="Michigan")
State_Min<- ccd   %>% filter(Province_State=="Minnesota")
State_Mis<- ccd   %>% filter(Province_State=="Mississippi")
State_Miss<- ccd   %>% filter(Province_State=="Missouri")
State_Mon<- ccd   %>% filter(Province_State=="Montana")
State_Ne<- ccd   %>% filter(Province_State=="Nebraska")
State_Nev<- ccd   %>% filter(Province_State=="Nevada")
State_Newj<- ccd   %>% filter(Province_State=="New Jersey")
State_Newh<- ccd   %>% filter(Province_State=="New Hampshire")
State_Newy<- ccd   %>% filter(Province_State=="New York")
State_Newm<- ccd   %>% filter(Province_State=="New Mexico")
State_Nor<- ccd   %>% filter(Province_State=="North Carolina")
State_NorD<- ccd   %>% filter(Province_State=="North Dakota")
State_Oh<- ccd   %>% filter(Province_State=="Ohio")
State_Ok<- ccd   %>% filter(Province_State=="Oklahoma")
State_Or<- ccd   %>% filter(Province_State=="Oregon")
State_Penn<- ccd   %>% filter(Province_State=="Pennsylvania")
State_Rh<- ccd   %>% filter(Province_State=="Rhode Island")
State_So<- ccd   %>% filter(Province_State=="South Dakota")
State_SoC<- ccd   %>% filter(Province_State=="South Carolina")
State_Te<- ccd   %>% filter(Province_State=="Texas")
State_Ten<- ccd   %>% filter(Province_State=="Tennessee")
State_Ut<- ccd   %>% filter(Province_State=="Utah")
State_Ve<- ccd   %>% filter(Province_State=="Vermont")
State_Vi<- ccd   %>% filter(Province_State=="Virginia")
State_Wa<- ccd   %>% filter(Province_State=="Washington")
State_We<- ccd   %>% filter(Province_State=="West Virginia")
State_Wi<- ccd   %>% filter(Province_State=="Wisconsin")
State_Wy<- ccd   %>% filter(Province_State=="Wyoming")

# this is how i found the max of each data frame
  
df_list <- list(State_Al, State_AlA, State_Ar,State_ARK,State_Ca,State_Co,State_Con,State_De,State_DoC,State_Fl,State_Ge,
                State_Ha,State_Id,State_Ill,State_In,State_Io,State_Ka,State_Ke,State_Lo,State_Ma,State_Mar,State_Mas,
                State_Mi,State_Min,State_Mis,State_Miss,State_Mon,State_Ne,State_Nev,State_Newh,State_Newj,State_Newm,
                State_Newy,State_Nor,State_NorD,State_Oh,State_Ok,State_Or,State_Penn,State_Rh,State_So,State_SoC,State_Te,
                State_Ten,State_Ut,State_Ve,State_Vi,State_Wa,State_We,State_Wi,State_Wy)
Max_fatality_ratio<-c(max(df_list[[1]]$Case_Fatality_Ratio,na.rm=TRUE),
max(df_list[[2]]$Case_Fatality_Ratio,na.rm=TRUE),
max(df_list[[3]]$Case_Fatality_Ratio,na.rm=TRUE),
max(df_list[[4]]$Case_Fatality_Ratio,na.rm=TRUE),
max(df_list[[5]]$Case_Fatality_Ratio,na.rm=TRUE),
max(df_list[[6]]$Case_Fatality_Ratio,na.rm=TRUE),
max(df_list[[7]]$Case_Fatality_Ratio,na.rm=TRUE),
max(df_list[[8]]$Case_Fatality_Ratio,na.rm=TRUE),
max(df_list[[9]]$Case_Fatality_Ratio,na.rm=TRUE),
max(df_list[[10]]$Case_Fatality_Ratio,na.rm=TRUE),
max(df_list[[11]]$Case_Fatality_Ratio,na.rm=TRUE),
max(df_list[[12]]$Case_Fatality_Ratio,na.rm=TRUE),
max(df_list[[13]]$Case_Fatality_Ratio,na.rm=TRUE),
max(df_list[[14]]$Case_Fatality_Ratio,na.rm=TRUE),
max(df_list[[15]]$Case_Fatality_Ratio,na.rm=TRUE),
max(df_list[[16]]$Case_Fatality_Ratio,na.rm=TRUE),
max(df_list[[17]]$Case_Fatality_Ratio,na.rm=TRUE),
max(df_list[[18]]$Case_Fatality_Ratio,na.rm=TRUE),
max(df_list[[19]]$Case_Fatality_Ratio,na.rm=TRUE),
max(df_list[[20]]$Case_Fatality_Ratio,na.rm=TRUE),
max(df_list[[21]]$Case_Fatality_Ratio,na.rm=TRUE),
max(df_list[[22]]$Case_Fatality_Ratio,na.rm=TRUE),
max(df_list[[23]]$Case_Fatality_Ratio,na.rm=TRUE),
max(df_list[[24]]$Case_Fatality_Ratio,na.rm=TRUE),
max(df_list[[25]]$Case_Fatality_Ratio,na.rm=TRUE),
max(df_list[[26]]$Case_Fatality_Ratio,na.rm=TRUE),
max(df_list[[27]]$Case_Fatality_Ratio,na.rm=TRUE),
max(df_list[[28]]$Case_Fatality_Ratio,na.rm=TRUE),
max(df_list[[29]]$Case_Fatality_Ratio,na.rm=TRUE),
max(df_list[[30]]$Case_Fatality_Ratio,na.rm=TRUE),
max(df_list[[31]]$Case_Fatality_Ratio,na.rm=TRUE),
max(df_list[[32]]$Case_Fatality_Ratio,na.rm=TRUE),
max(df_list[[33]]$Case_Fatality_Ratio,na.rm=TRUE),
max(df_list[[34]]$Case_Fatality_Ratio,na.rm=TRUE),
max(df_list[[35]]$Case_Fatality_Ratio,na.rm=TRUE),
max(df_list[[36]]$Case_Fatality_Ratio,na.rm=TRUE),
max(df_list[[37]]$Case_Fatality_Ratio,na.rm=TRUE),
max(df_list[[38]]$Case_Fatality_Ratio,na.rm=TRUE),
max(df_list[[39]]$Case_Fatality_Ratio,na.rm=TRUE),
max(df_list[[40]]$Case_Fatality_Ratio,na.rm=TRUE),
max(df_list[[41]]$Case_Fatality_Ratio,na.rm=TRUE),
max(df_list[[42]]$Case_Fatality_Ratio,na.rm=TRUE),
max(df_list[[43]]$Case_Fatality_Ratio,na.rm=TRUE),
max(df_list[[44]]$Case_Fatality_Ratio,na.rm=TRUE),
max(df_list[[45]]$Case_Fatality_Ratio,na.rm=TRUE),
max(df_list[[47]]$Case_Fatality_Ratio,na.rm=TRUE),
max(df_list[[48]]$Case_Fatality_Ratio,na.rm=TRUE),
max(df_list[[49]]$Case_Fatality_Ratio,na.rm=TRUE),
max(df_list[[50]]$Case_Fatality_Ratio,na.rm=TRUE),
max(df_list[[51]]$Case_Fatality_Ratio,na.rm=TRUE))

State_name<-c("Alaska","Alabama","Arizona","Arkansas","California","Colorado","Connecticut","Delaware","Florida","Georgia","Hawaii","Idaho","Illinois","Indiana","Iowa","Kansas","Kentucky","Louisiana","Maine","Maryland","Massachusetts","Michigan","Minnesota","Mississippi","Missouri","Montana","Nebraska","Nevada","New Hampshire","New Jersey","New Mexico","New York","North Carolina","North Dakota","Ohio","Oklahoma","Oregon","Pennsylvania","Rhode Island","South Carolina","South Dakota","Tennessee","Texas","Utah","Vermont","Virginia","Washington","West Virginia","Wisconsin","Wyoming")
Max_fatality_ratios<- cbind(Max_fatality_ratio,State_name)
Max_fatality_ratios<- data.frame(Max_fatality_ratio,State_name)

Max_fatality_ratios %>% 
  ggplot(aes(x=State_name,y=Max_fatality_ratio))+geom_col()


# reordered the plot and turned the axis labels
Max_fatality_ratios %>% 
  ggplot(aes(x=reorder(State_name,- Max_fatality_ratio), y=Max_fatality_ratio))+geom_col()+ggeasy::easy_rotate_x_labels(angle = 90)


