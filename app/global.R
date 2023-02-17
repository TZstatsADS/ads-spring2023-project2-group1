if (!require("dplyr")) {
  install.packages("dplyr")
  library(dplyr)
}
if (!require("sqldf")) {
  install.packages("sqldf")
  library(sqldf)
}
if (!require("sqldf")) {
  install.packages("sqldf")
  library(sqldf)
}

current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))

df <- read_csv("../data/DOHMH_New_York_City_Restaurant_Inspection_Results.csv")

df <- df %>%
  clean_names()

df$inspection_date <- as.POSIXct(df$inspection_date, format = "%m/%d/%Y")
df$inspection_year <- as.numeric(format(df$inspection_date, format = "%Y"))
df <- df[!(is.na(df$latitude) | df$latitude=="" | df$latitude==0 | is.na(df$longitude) | df$longitude=="") | df$longitude==0 , ]

df = sqldf("
          select
          camis restaurant_code
          , dba restaurant_name
          , boro
          , building
          , street
          , zipcode
          , phone
          , cuisine_description
          , inspection_date
          , inspection_year
          , action
          , violation_description
          , case 
          when action = 'No violations were recorded at the time of this inspection.' then 'No Violation'
          when action = '' then 'No Violation'
          when action is null then 'No Violation'
          when critical_flag = 'Critical' then 'XX'
          when critical_flag = 'Not-Critical' then 'YY'
          else critical_flag
          end violation_type
          , critical_flag
          , score
          , case when grade is null then 'N/A' else grade end grade
          , latitude
          , longitude
          from df
          ")

df_temp = sqldf("
          select
          restaurant_code
          , restaurant_name
          , boro
          , building
          , street
          , zipcode
          , phone
          , cuisine_description
          , inspection_date
          , inspection_year
          , group_concat(violation_description) violation_description
          , group_concat(violation_type) violation_type
          , group_concat(grade) grade
          , latitude
          , longitude
          from df
          group by restaurant_code
          , restaurant_name
          , boro
          , building
          , street
          , zipcode
          , phone
          , cuisine_description
          , inspection_date
          , inspection_year 
          , latitude
          , longitude
          ")

df_clean = sqldf("
          select
          restaurant_code
          , restaurant_name
          , boro
          , building
          , street
          , zipcode
          , phone
          , cuisine_description
          , inspection_date
          , inspection_year
          , violation_description
          , case
          when violation_type like '%XX%' then 'Critical'
          when violation_type like '%YY%' then 'Not-Critical'
          when violation_type like '%No Violation%' then 'No Violation'
          else 'Not Applicable'
          end violation_type
          , case
          when grade like '%C%' then 'C'
          when grade like '%B%' then 'B'
          when grade like '%A%' then 'A'
          when grade like '%P%' then 'P'
          when grade like '%Z%' then 'Z'
          when grade like '%N%' then 'N'
          else grade end grade
          , latitude
          , longitude
          from df_temp
          ")

# filtering

# data where inspection_year=1900 means that this restaurant has not had inspections
# exclude it from analysis such as count of inspections

# df_clean contains unfiltered data (except where removed due to no longlat), however it has been grouped so that each row is one unique inspection date, rather than a citation
# violation type included in df_clean is the most severe violation type
# restaurants may appear several times as they may have gone through multiple inspections
# please use df_unique to show number of inspections per year, or number of inspections per restaurant

df_unique = sqldf("
          with rownum as (
          select *
          , row_number() over(partition by restaurant_code, inspection_year order by inspection_date desc) rank_
          from df_clean)
          
          select *
          from rownum
          where rank_=1
          ")

# df_unique contains 1 row per restaurant per year. for each year, it takes only data from the last inspection
# please use it to show comparisons of inspection grade/violations over time, or any data over time (yearly)

df_2022 = sqldf("
          with rownum as (
          select *
          , row_number() over(partition by restaurant_code order by inspection_date desc) rank_
          from df_clean
          where inspection_year in (2022,1900))
          
          select *
          from rownum
          where rank_=1
          ")

# df_2022 contains only the last inspection data from 2022 (or uninspected restaurants)
# please use it for analysis that is not over time (e.g. word cloud of violations, distribution of rating)
          

