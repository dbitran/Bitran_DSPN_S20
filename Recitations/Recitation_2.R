#Setting wd
setwd('/Users/Debbie/Documents/DOCUMENTS/CLASSES/DSPAN/GitHub/Bitran_DSPN_S20')

#Loading data
d <- read_csv("YoutubeVideos.csv")

#2 data structures: vectors and frames
#Vectors: lists of numbers 
#c = concatenate
#assigning "a" as a vector of 1, 2, & 3
a <- c(1,2,3)
##assigning "b" as a vector of 4, 5, & 6
b<- c(4,5,6) 
a*b

#Data frames: 
#cbind = concatenate in columns
c <- c ("one", "two", "three")
data <- data.frame(cbind(a,b,c))

#Data indexing:
data[n,m]
data[n,]
data$a

#Deal with missing vaues >> deleting whole row if has one missing value on any variable 
d <- na.omit(d)

# out of range values

d <- d[d$disklikes >= 0,]
#OR us tiduverse command
d <- subset (d, dislikes >= 0)

#removing some variables 
d1 <- d[, -16:-12]

#The pipe operator %>%
d %>% 
  na.omit()%>%
  subset(dislikes > 0) -> d
#instead of
d <- na.omit(d)
d <- subset(d, dislikes >= 0)

#Basics of data preprocessing 
?gather
?spread
short %>%
  gather(timepoint, observation, time1:4) -> long
#timepoint = key variable/value, observation= the values you want to transpose 

Long%>%
  spread(timepoint,observation) -> short

d1%>%
  gather(measurement, values, views:comment_count) -> d2
d2%>%
  spread(measurement, values) -> d3

#########
#create new metrics using "mutate"

d_centered <- mutate (d, z = (likes - mean(likes))/sd(likes))

#OR 

d%>%
  mutate(z = (likes - mean(likes))/sd(likes)) -> d_centered

#Summarise information

d %>%
  mutate(z = (likes - mean(likes))/sd(likes)) %>%
  summarise(mean = mean(z))

#Filter and select 
#filter out data based on values
filter(d, channel_title == 'Saturday Night Live')
#select variables for a specific dataset
select(d, likes)

d%>%
  mutate(difference = (likes-dislikes)) %>%
  filter(channel_title == "Ed Sheeran") %>%
  summarise(mean = mean(difference), sd = sd(difference)) -> diff
#Groub By 


