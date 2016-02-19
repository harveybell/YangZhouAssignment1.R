##############################################################

# title: Assignment 4 
# author: "Yang Zhou"
# date: "Winter 2016"
# assignment: https://github.com/EconomiCurtis/econ294_2015/blob/master/Assignments/Econ_294_Assignment_4.pdf

###0###

print("Yang Zhou")
print(1505133)
print("yzhou79@ucsc.edu")


###Q#1###

library(foreign)
library(dplyr)
flights<- read.csv("https://raw.githubusercontent.com/EconomiCurtis/econ294_2015/master/data/flights.csv", 
                   stringsAsFactors = FALSE) %>%
  tbl_df()
planes<- read.csv("https://raw.githubusercontent.com/EconomiCurtis/econ294_2015/master/data/planes.csv", 
                  stringsAsFactors = FALSE) %>%
  tbl_df()
weather<- read.csv("https://raw.githubusercontent.com/EconomiCurtis/econ294_2015/master/data/weather.csv", 
                   stringsAsFactors = FALSE) %>%
  tbl_df()
airports<- read.csv("https://raw.githubusercontent.com/EconomiCurtis/econ294_2015/master/data/airports.csv", 
                    stringsAsFactors = FALSE) %>%
  tbl_df()


###Q#2### 

flights$Date<-as.Date(flights$date)
weather$Date<-as.Date(weather$date)


###Q#3### 

flights.2a<-subset(flights,dest=="SFO"|dest=="OAK")
nrow(flights.2a)
flights.2b<-subset(flights,arr_delay>=60|dep_delay>=60)
nrow(flights.2b)
flights.2c<-subset(flights,arr_delay>=2*dep_delay)
nrow(flights.2c)


###Q#4###

S1<-select(flights,ends_with("_delay"))
S2<-select(flights,dep_delay,arr_delay)
S3<-select(flights,dep_delay:arr_delay)


###Q#5### 
##a##

flights.5a<-flights %>% 
  arrange(desc(dep_delay)) %>% 
  select(dep_delay,flight,dest,plane,everything()) 
print(head(flights.5a,5))


##b##

flights.5b<-flights %>%
  mutate(
    change=dep_delay-arr_delay
  ) %>%
  arrange(-change) %>%
print(head(flights.5b,5))


###Q#6###

flights.6<-flights %>%
  mutate(
    speed=dist/(time/60)
  )
flights.6<-flights.6 %>%
  mutate(
    delta=dep_delay-arr_delay
  )%>%
  arrange(-delta,-speed)
print(head(flights.6,5))

##a##

flights.6a<-flights.6 %>%
  arrange(-speed)%>%
 head(5)
flights.6a

##b##

flights.6b<-flights.6 %>%
  arrange(-delta)%>%
  head(5)
flights.6b

##c##

flights.6c<-flights.6 %>%
  arrange(delta)%>%
  head(5)
flights.6c


###Q#7###

flights.7a<-flights.6 %>%
  group_by(carrier) %>%               
  summarise( 
    num_cancelled=sum(cancelled),
    total_flights =n(),
    ratio=num_cancelled/total_flights,
    delta_min = min(delta,na.rm = T), 
    delta_q25 = quantile(delta,0.25,na.rm = T),
    delta_mean = mean(delta,na.rm=T),
    delta_q75 = quantile(delta,0.75,na.rm = T),
    delta_q90=quantile(delta,0.90,na.rm=T),
    delta_max = max(delta,na.rm = T)
  ) %>%
  arrange(desc(ratio)) %>%
  print()

day_delay <- dplyr::filter( summarize(
  group_by( dplyr::filter(
    flights,
    !is.na(dep_delay) ),
    date ),
  delay = mean(dep_delay), n = n()
),
n > 10 )
print("code above filter the NA observations in dep_delay in flights and group it by date, then making a summary table of the average of dep_delay, total number of flights")

day_delay1<-flights %>%
  filter(!is.na(dep_delay)) %>%
  group_by(date) %>%
  summarize(
    delay=mean(dep_delay),
    n=n()
  ) %>%
  dplyr::filter(n>10)


###Q#8###

day_delay0<-day_delay %>%
  mutate(
    day_lag=delay-lag(delay)
  ) %>%
  arrange(desc(day_lag)) %>%
  head(5)
day_delay0


###Q#9###

dest_delay<-flights %>%
  filter(!is.na(arr_delay)) %>%
  group_by(dest) %>%
  summarize(
    delay=mean(arr_delay),
    n=n()
  )
airports1<- airports %>%
  select(iata, airport, city, state, lat, long) %>%
  rename(dest=iata) %>%
  rename(name=airport)

##a##

df.9a<-dest_delay %>%
  left_join(airports1) %>%
  arrange(desc(delay))
print(head(df.9a,5))

##b##

df.9b<-dest_delay %>%
  inner_join(airports1)
print("No, number of observation when we use inner_join do not match with lef_join.")

##c##

df.9c<-dest_delay %>%
  right_join(airports1)
nrow(df.9c)
print("Now number of observation is 3376, and NA appears in new table. For the reason that right_join merging keeps all varibles in right table, when they can not match left one, it is shown as NA.")

##d##

df.9d<-dest_delay %>%
  full_join(airports1)
nrow(df.9d)
print("Now number of observation is 3378, and NA appears in new table. For the reason that full_join merging keeps all varibles in both tables, when they can not match each other, it is shown as NA")


###Q#10###

hourly_delay<-flights %>%
  filter(!is.na(dep_delay)) %>%
  group_by(DATE,hour) %>%
  summarize(
    delay=mean(dep_delay),
    n=n()
  )
df10<-hourly_delay %>%
  left_join(weather)

condition<- df10 %>%
  filter(!is.na(delay)) %>%
  group_by(conditions)%>%
  summarize(
    delay=weighted.mean(delay,n),
    number=sum(n)
  ) %>%
  arrange(desc(delay))
print(head(condition,5))
print("The biggest delay is associated with freezing rain.")

###Q#11###
##a##

df <- data.frame(treatment = c("a", "b"), subject1 = c(3, 4), subject2 = c(5, 6))
df

df2<-df %>%
  gather(subject,value,
         ...=-treatment,
         na.rm=T) %>%
  arrange(subject,treatment)
df2

df3<-df2 %>%
  separate(subject,
           c("name","subject"),
           7
  )
df3

df4<-df3 %>%
  select(subject,treatment,value) %>%
  arrange(subject,treatment)
df4

##b##

df <- data.frame(
  subject = c(1,1,2,2),
  treatment = c("a","b","a","b"), value = c(3,4,5,6)
)
df

df1<-df %>%
  mutate(
    name="subject"
  )
df1

df2<-df1 %>%
  unite(
    col=subject,
    ...=name,subject,
    sep=""
  )
df2


df3<-df2 %>%
  spread(
    key=subject,
    value=value
  ) 
df3

##c##

df <- data.frame(
  subject = c(1,2,3,4),
  demo = c("f_15_CA","f_50_NY","m_45_HI","m_18_DC"), value = c(3,4,5,6)
)
df

df2<-df %>%
  separate(
    demo,
    c("sex","age","state"),
    "_"
  )
df2

##d##

df <- data.frame(
  subject = c(1,2,3,4),
  sex = c("f","f","m",NA),
  age = c(11,55,65,NA),
  city = c("DC","NY","WA",NA), value = c(3,4,5,6)
)
df
df1<-df %>%
  unite(
    col = demo, 
    ... = sex, age,city,
    sep = "."
  ) 
df1

df1$demo<- replace(x=df1$demo,df1$demo=="NA.NA.NA",values=NA)
df1

##############################################################







