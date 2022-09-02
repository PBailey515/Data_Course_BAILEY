#List files 
list.files(recursive = TRUE,include.dirs=TRUE, full.names = TRUE)
#List all CSV files 
csv_files<-list.files(path="Data",recursive = TRUE, full.names = TRUE,pattern=".csv")
# <- saves to the object csv_files
# find out how many lenght(csv_files)
length(csv_files)
#145 files
# to open files list.files(path="Data", recursive=TRUE, full.name=TRUE, pattern= "whatever you want to open")
wing<-list.files(path="Data", recursive=TRUE,full.names = TRUE,pattern= "wingspan_vs_mass.csv")
df<-read.csv(file=wing)
#read the first 5 lines
head(df,n=5)
#find all the files that start with "b" 
#for loop
b<-list.files(path="Data",recursive = TRUE,full.names = TRUE,pattern="^b")
for (eachfile in b) {print(readLines(eachfile,n=1))
  
} 
for (eachfile in csv_files) {print(readLines(eachfile,n=1))
  
}
# this is to read the first line in all the files that
#have .csv which i stored as csv_files a while back. 