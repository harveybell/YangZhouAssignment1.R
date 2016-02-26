###################################################################
# title: Assignment 5
# date: "Winter 2016"
###################################################################
print("Yang Zhou")
print(1505133)
print("yzhou79@ucsc.edu")
###################################################################

###1###
#a#

library(ggplot2)
library(tidyr)
diamonds <- diamonds %>% 
  mutate(
    volume=x*y*z
  ) %>%
  tbl_df()

ggplot(diamonds,
            aes(x=volume, y=price))+
  geom_point(aes(colour = clarity,
                 size=carat),
             alpha=0.3) +
  scale_x_log10() +
  scale_y_log10()

#b#

ggplot(diamonds) +
  geom_histogram(aes(x=carat,
                     y=..density..,
                     fill=clarity
  ),binwidth = 0.2) +facet_grid(cut ~ .) 

#c#

ggplot(diamonds, aes(cut, price)) +
  geom_violin()+geom_jitter(alpha=0.02) 


###3###
#a#

library(foreign)
df<-read.dta("D://各种作业//winter//lab//org_example.dta")
library(dplyr)
df <- df%>% 
  mutate(
    date = paste(year, month, "01", sep = "-"),
    date = as.Date(date, format = "%Y-%m-%d")
  ) %>%
  filter(!is.na(rw)) %>%
  tbl_df()

df1<-df %>%
  group_by(year,month) %>%
  summarize(
    qantile10=quantile(rw,0.10),
    qantile25=quantile(rw,0.25),
    qantile75=quantile(rw,0.75),
    qantile90=quantile(rw,0.90),
    Median.Rw=median(rw)
  )

df1 <- df1%>% 
  mutate(
    date = paste(year, month, "01", sep = "-"),
    date = as.Date(date, format = "%Y-%m-%d")
  ) %>%
  tbl_df()
ggplot(df1, aes(date, Median.Rw))+
  geom_line()+lims(y=c(0,50)) +
  geom_ribbon(aes(ymin=qantile10, 
                  ymax=qantile90), 
              alpha = 0.2)+
  geom_ribbon(aes(ymin=qantile25, 
                  ymax=qantile75), 
              alpha = 0.5)

#b#

df2<-df %>%
  group_by(year,month,educ) %>%
  summarize(
    Median.Rw=median(rw)
  )

df2 <- df2%>% 
  mutate(
    date = paste(year, month, "01", sep = "-"),
    date = as.Date(date, format = "%Y-%m-%d")
  ) %>%
  tbl_df()
ggplot(df2, aes(date, Median.Rw,group=educ))+
 geom_line(aes(colour=educ))

###################################################################
