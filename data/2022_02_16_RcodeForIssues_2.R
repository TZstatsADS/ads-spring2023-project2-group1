install.packages("shiny")
library(shiny)

df=read.csv("C:/Users/naira_831868r/OneDrive/Desktop/Columbia/Spring '23/Applied Data Science/Assignments/Shiny App/DOHMH_New_York_City_Restaurant_Inspection_Results.csv")
head(df)

#For wordclouds per violation code
df_violation_temp<-df |>
  distinct(df$VIOLATION.CODE)
head(df_violation_temp)

freq=table(unlist(df$VIOLATION.CODE)) #Find frequency of violation codes to know what wordclouds make sense
freq 
class(freq)

library(dplyr)
library(wordcloud)

#for (x in df_violation) {
#  df_temp<-df |>
#    group_by(VIOLATION.CODE==x)
#  wc_data = 
#    df |>
#    unnest_tokens(output = word, input = VIOLATION.DESCRIPTION)|>
#    anti_join(y = stop_words)|>
#    group_by(word)|>
#    summarize(n = n())|>
#    ungroup()|>
#    arrange(desc(n))
#  y=wordcloud(words = wc_data$word, freq = wc_data$n,scale = c(2,0.5),max.words = 200,rot.per = 0)
#  print(y)
#  }

#Trying filtering

df1<-df |>
  filter(VIOLATION.CODE=="02B")
wc_data1 = 
  df1 |>
  unnest_tokens(output = word, input = VIOLATION.DESCRIPTION, token='ngrams', n=2)|>
  anti_join(y = stop_words)|>
  group_by(word)|>
  summarize(n = n())|>
  ungroup()|>
  arrange(desc(n))
wordcloud(words = wc_data1$word, freq = wc_data1$n,scale = c(2,0.5),max.words = 200,rot.per = 0)

df2<-df |>
  filter(VIOLATION.CODE=="02G")
wc_data2 = 
  df2 |>
  unnest_tokens(output = word, input = VIOLATION.DESCRIPTION, token='ngrams', n=2)|>
  anti_join(y = stop_words)|>
  group_by(word)|>
  summarize(n = n())|>
  ungroup()|>
  arrange(desc(n))
wordcloud(words = wc_data2$word, freq = wc_data2$n,scale = c(2,0.5),max.words = 200,rot.per = 0)

df3<-df |>
  filter(VIOLATION.CODE=="04L")
wc_data3 = 
  df3 |>
  unnest_tokens(output = word, input = VIOLATION.DESCRIPTION, token='ngrams', n=2)|>
  anti_join(y = stop_words)|>
  group_by(word)|>
  summarize(n = n())|>
  ungroup()|>
  arrange(desc(n))
wordcloud(words = wc_data3$word, freq = wc_data3$n,scale = c(2,0.5),max.words = 200,rot.per = 0)

df4<-df |>
  filter(VIOLATION.CODE=="04N")
wc_data4 = 
  df4 |>
  unnest_tokens(output = word, input = VIOLATION.DESCRIPTION, token='ngrams', n=2)|>
  anti_join(y = stop_words)|>
  group_by(word)|>
  summarize(n = n())|>
  ungroup()|>
  arrange(desc(n))
wordcloud(words = wc_data4$word, freq = wc_data4$n,scale = c(2,0.5),max.words = 200,rot.per = 0)

df5<-df |>
  filter(VIOLATION.CODE=="06C")
wc_data5 = 
  df5 |>
  unnest_tokens(output = word, input = VIOLATION.DESCRIPTION, token='ngrams', n=2)|>
  anti_join(y = stop_words)|>
  group_by(word)|>
  summarize(n = n())|>
  ungroup()|>
  arrange(desc(n))
wordcloud(words = wc_data5$word, freq = wc_data5$n,scale = c(2,0.5),max.words = 200,rot.per = 0)

df6<-df |>
  filter(VIOLATION.CODE=="06D")
wc_data6 = 
  df6 |>
  unnest_tokens(output = word, input = VIOLATION.DESCRIPTION, token='ngrams', n=2)|>
  anti_join(y = stop_words)|>
  group_by(word)|>
  summarize(n = n())|>
  ungroup()|>
  arrange(desc(n))
wordcloud(words = wc_data6$word, freq = wc_data6$n,scale = c(2,0.5),max.words = 200,rot.per = 0)

df7<-df |>
  filter(VIOLATION.CODE=="08A")
wc_data7 = 
  df7 |>
  unnest_tokens(output = word, input = VIOLATION.DESCRIPTION, token='ngrams', n=2)|>
  anti_join(y = stop_words)|>
  group_by(word)|>
  summarize(n = n())|>
  ungroup()|>
  arrange(desc(n))
wordcloud(words = wc_data7$word, freq = wc_data7$n,scale = c(2,0.5),max.words = 200,rot.per = 0)

df8<-df |>
  filter(VIOLATION.CODE=="10B")
wc_data8 = 
  df8 |>
  unnest_tokens(output = word, input = VIOLATION.DESCRIPTION, token='ngrams', n=2)|>
  anti_join(y = stop_words)|>
  group_by(word)|>
  summarize(n = n())|>
  ungroup()|>
  arrange(desc(n))
wordcloud(words = wc_data8$word, freq = wc_data8$n,scale = c(2,0.5),max.words = 200,rot.per = 0)

df9<-df |>
  filter(VIOLATION.CODE=="10F")
wc_data9 = 
  df9 |>
  unnest_tokens(output = word, input = VIOLATION.DESCRIPTION, token='ngrams', n=2)|>
  anti_join(y = stop_words)|>
  group_by(word)|>
  summarize(n = n())|>
  ungroup()|>
  arrange(desc(n))
wordcloud(words = wc_data9$word, freq = wc_data9$n,scale = c(2,0.5),max.words = 200,rot.per = 0)
