library(tidyverse)
library(lubridate)
library(RColorBrewer)
library(sf)

#turn off scientific notation for large values
options(scipen = 999)

## First combine all  CSV files into one data frame
# Read one file to check column types
sample_df <- read_csv("2023.csv")
str(sample_df)

# all other csv have reporting_area as integer - fix 2023 to match
sample_df$REPORTING_AREA <- as.integer(sample_df$REPORTING_AREA, na.rm = TRUE)
str(sample_df)

#fix 2015-2018
older <- read.csv("2015-2018.csv")
older <- older %>%
  mutate(SHOOTING = ifelse(SHOOTING == "Y", 1, SHOOTING)) %>%
  mutate(SHOOTING = as.numeric(SHOOTING))
str(older)

# write the fix back to a csv
write.csv(sample_df, "2023a.csv", row.names = FALSE)


#combine all csv into one df
file_names <- c("2019.csv", "2020.csv", "2021.csv", "2022.csv", "2023a.csv")
df <- map_dfr(file_names, read.csv)
df <-rbind(older, df)
str(df)

# fix variable names
df <- df %>%
  rename_with(~ c("incident_number", "offense_code", "offense_code_group", "offense_description",
                 "district", "reporting_area", "shooting", "incident_date", "year", "month", "day_of_week",
                 "hour", "ucr_part", "street", "lat", "long", "location"))
df$year <- as.factor(df$year)

#write new CSV for Tableau
write.csv(df, "boston_crime_data.csv")

years <- unique(df$year)
years

#annual incidents
annual_incidents <- df %>%
  group_by(year) %>%
  summarise(count = n())


#bar chart incident by month 
ggplot(annual_incidents, aes(x = year, y = count, fill= count)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_fill_continuous(low = "lightblue", high = "darkblue", limits = c(4000, 9000))+
  labs(title = "Total Incidents by Year",
       x="Date",
       y="Incident Count", 
       fill = "Range") +
  geom_vline(xintercept = '2020/04', color = "red") +
  geom_vline(xintercept = '2021/07', color = "red") +
  annotate("text", x = '2021/04', y = 7500, label = "COVID-19 Stay at Home Order", size = 4, color = "red", hjust = 1.2)


#offense types
offenses <- df %>%
  group_by(offense_description) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# top top 
top10offenses <- subset(offenses, count >= 11555)

top10offenses <- top10offenses %>%
  mutate(offense_description = fct_reorder(offense_description, count))
top10offenses

ggplot(top10offenses, aes(x=offense_description, y=count, fill = count))+
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_fill_continuous(low = "lightblue", high = "darkblue", limits = c(10000, 45000))+
  labs(title = "Top 10 Offense Types",
       x="Incident Count",
       y="Offense Description",
       fill="Range")+
  coord_flip()


# shootings
shootings <- df %>%
  group_by(month_year) %>%
  summarise(count = sum(shooting))
shootings <- subset(shootings, month_year < '2024/07')
shootings

shootings <- df %>%
  group_by(year) %>%
  summarise(count = sum(shooting))
shootings

ggplot(shootings, aes(x=month_year, y=count, fill=count)) +
  geom_bar(stat = "identity")+ 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Shootings by Month and Year",
       xlab = "",
       ylab= "",
       fill= "Range")+
  scale_fill_continuous(low = "lightblue", high = "darkblue", limits = c(30, 150))+
  geom_vline(xintercept = '2020/04', color = "red") +
  geom_vline(xintercept = '2021/07', color = "red") +
  annotate("text", x = '2021/04', y = 150, label = "COVID-19 Stay at Home Order", size = 4, color = "red", hjust = 1.2)

#district 
dst <- df %>%
  group_by(district) %>%
  summarise(count = n())
dst <- as.data.frame(dst)
dst <- dst %>% 
  mutate(district = ifelse(district == "", "not-filled", district))
dst <- dst %>%
  mutate(district = fct_reorder(district, count))
dst

ggplot(dst, aes(x=district, y=count, fill=count)) +
  geom_bar(stat ="identity") +
  labs(title = "Incident Counts by District",
       x="",
       y="",
       fill="Range")+
  scale_fill_continuous(low = "lightblue", high = "darkblue", limits = c(0, 60000))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  

