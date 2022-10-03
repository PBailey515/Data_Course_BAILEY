library(tidyverse)
dat<- read_csv("/Users/Owner/Desktop/Data_Course_BAILEY/Data/BioLog_Plate_Data.csv")

dat %>% pivot_longer( c("Hr_24","Hr_48","Hr_144"), names_to ="Absorbance_Time", values_to = "Absorbance_level") %>% 
  group_by(Substrate) %>% pivot_wider(names_from = "Sample ID",values_from = "Absorbance_level") %>% group_by(Dilution==0.1) 


clean<-dat %>% pivot_longer(c("Hr_24","Hr_48","Hr_144"),names_to = "Time", values_to = "Absorbance_level") %>% 
  pivot_wider(names_from = "Sample ID",values_from = "Absorbance_level") %>% 
  pivot_longer(c("Clear_Creek","Waste_Water"),names_to = "Water_Samples",values_to = "Absorbance_level_Water") %>% 
  pivot_longer(c("Soil_1","Soil_2"),names_to = "Soil_Samples",values_to = "Absorbance_level_Soil") %>% pivot_longer(c("Water_Samples","Soil_Samples"),names_to = "Type",values_to = "Location") %>% 
  view 
clean 
str(clean)
clean %>% ggplot(aes(x=Time,revalue(Time,c("Hr_24"="24","Hr_48"="48","Hr_144"="144")),y=Absorbance_level_Water)) + geom_point()


library(plyr)


clean<-dat %>% pivot_longer(c("Hr_24","Hr_48","Hr_144"),names_to = "Time", values_to = "Absorbance_level") %>% 
  pivot_wider(names_from = "Sample ID",values_from = "Absorbance_level") %>% 
  pivot_longer(c("Clear_Creek","Waste_Water"),names_to = "Water_Samples",values_to = "Absorbance_level_Water") %>% 
  pivot_longer(c("Soil_1","Soil_2"),names_to = "Soil_Samples",values_to = "Absorbance_level_Soil") %>% pivot_longer(c("Water_Samples","Soil_Samples"),names_to = "Type",values_to = "Location") 

new_time<-clean$Time<- revalue(clean$Time,
                               c("Hr_24"="24","Hr_48"="48","Hr_144"="144")) 
clean$new_time<- new_time %>% 
  as.numeric(x = clean$Time)
clean %>% ggplot(aes(x=new_time,y=Absorbance_level_Water,color=Type))+ geom_line() + facet_wrap(~Substrate)

clean1<-clean %>% group_by(Dilution=max(Dilution))
detach(package:plyr)
clean1 %>% 
  group_by(Substrate,new_time,Type) %>%
  summarise("avgAbW"=mean(Absorbance_level_Water,na.rm=TRUE),"avgAbS"=mean(Absorbance_level_Soil,na.rm=TRUE)) %>%
  unique.data.frame() %>%
  ggplot()+
  geom_line(aes(x=new_time,y=avgAbW,color="Water"))+
  geom_line(aes(x=new_time,y=avgAbS,color="Soil"))+
  facet_wrap(~Substrate) + labs(y= "Absorbance", x="Time")

library(gganimate)
library(plyr)
library(gganimate)
view(clean)
clean_animate<- clean %>% 
  pivot_longer(c("Absorbance_level_Water","Absorbance_level_Soil"), names_to = "Absorbance_Type",values_to = "Absorbance")
detach(package:plyr)
clean_animate %>% filter(Substrate=="Itaconic Acid") %>% group_by(Location,new_time,Dilution) %>% 
  summarise("Average_Absorbance"=mean(Absorbance,na.rm=TRUE)) %>% 
  unique.data.frame() %>% 
  ggplot()+geom_line(aes(x=new_time,y=Average_Absorbance, color=Location))+ facet_wrap(~Dilution) +
  transition_reveal(new_time)+ facet_wrap(~Dilution)+ theme_minimal()+ labs(x="Time")+labs(color="Sample_ID")
