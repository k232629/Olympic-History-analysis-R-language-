library("plotly")
library("tidyverse")
library("data.table")
library("gridExtra")
library("knitr")

data <- read_csv("/athlete_events.csv",
                 col_types = cols(
                   ID = col_character(),
                   Name = col_character(),
                   Sex = col_factor(levels = c("M","F")),
                   Age =  col_integer(),
                   Height = col_double(),
                   Weight = col_double(),
                   Team = col_character(),
                   NOC = col_character(),
                   Games = col_character(),
                   Year = col_integer(),
                   Season = col_factor(levels = c("Summer","Winter")),
                   City = col_character(),
                   Sport = col_character(),
                   Event = col_character(),
                   Medal = col_factor(levels = c("Gold","Silver","Bronze"))
                 )
)

## завдання 1 
##Кількість жінок протягом 1896 - 2016 років у порівнянні з чоловіками з України?
teamUkraine <- data %>% 
  filter(Team == "Ukraine") %>%
  select(ID, Name, Sex, Age, Team, Sport, Year, City, Event, Medal)

counts_sex <- teamUkraine %>% group_by(Year, Sex) %>%
  summarize(Athletes = length(unique(ID)))
counts_sex$Year <- as.integer(counts_sex$Year)

ggplot(counts_sex, aes(x=Year, y=Athletes, group=Sex, color=Sex)) +
  geom_point(size=2) +
  geom_line()  +
  scale_color_manual(values=c("darkblue","red")) +
  labs(title = "Number of male and female Ukraine Olympians over time") +
  theme(plot.title = element_text(hjust = 0.5))

## завдання 2
## Чи збільшилась кількість учасників 1896 по 2016 від України?
teamUkraine <- data %>% 
  filter(Team == "Ukraine") %>%
  select(ID, Name, Sex, Age, Team, Sport, Year, City, Event, Medal)

counts_ukraine_members <- teamUkraine %>% filter(Team != "Unknown") %>%
  group_by(Year) %>%
  summarize(
    Artists = length(unique(Name))
  )

ggplot(counts_ukraine_members, aes(x=Year, y=Artists)) +
  geom_point(size=2) +
  labs(title = "Number of Ukraine Olympians over time")+
  geom_line()

##завдання 4
## кількість медалей у України 
teamUkraine <- data %>% 
  filter(Team == "Ukraine") %>%
  select(ID, Name, Sex, Age, Team, Sport, Year, City, Event, Medal)

medal_counts_ukraine <- teamUkraine %>% filter(!is.na(Medal))%>%
  group_by(Year, Medal) %>%
  summarize(Count=length(Medal)) 


levs_ukraine <- medal_counts_art %>%
  group_by(Year) %>%
  summarize(Total=sum(Count)) %>%
  arrange(Total) %>%
  select(Year)
medal_counts_ukraine$Year <- factor(medal_counts_ukraine$Year, levels=levs_art$Year)

ggplot(medal_counts_ukraine, aes(x=Year, y=Count, fill=Medal)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values=c("gold1","gray70","gold4")) +
  ggtitle("Medal counts in Ukraine") +
  theme(plot.title = element_text(hjust = 0.5))

##завдання 4.1
## кількість золотих медалей у України
teamUkraine <- data %>% 
  filter(Team == "Ukraine") %>%
  select(ID, Name, Sex, Age, Team, Sport, Year, City, Event, Medal)

gold_medal_counts_ukraine <- teamUkraine %>% filter(Medal=="Gold")%>%
  group_by(Year, Medal) %>%
  summarize(Count=length(Medal)) 

ggplot(gold_medal_counts_ukraine, aes(x=Year, y=Count)) +
  geom_point(size=2) +
  geom_line()


##fit_1 <- lm(Year ~ Count, data = gold_medal_counts_ukraine)

##завдання 5 
##Чи залежить результат від пропорції вага/зріст у жінок спорсменок України у плаванні ?
data <- data %>% mutate(bmi = data$Weight/((data$Height/100)*(data$Height/100)))

teamUkraineSW <- data %>% 
  filter(Team == "Ukraine" & Sport == "Swimming" & Sex == "F") %>%
  select(ID, Name, Sex, bmi, Age, Year, City, Event, Medal)

bmi_medals <- teamUkraineSW %>% 
  group_by(bmi) %>%
  summarize(
    Medals = length(Medal)) 
  
ggplot(bmi_medals, aes(x=bmi, y=Medals)) +
  geom_point(size=2) +
  geom_line()

##завдання 3
## Зміна результатів на 10 км спрінт у біатлоні тих хто отримував золото 

data_additional <- read_csv("/olympic_biathlon.csv")
colnames(data_additional)[which(names(data_additional) == "Winner")] <- "Name"
colnames(data_additional)[which(names(data_additional) == "Host City")] <- "City"
colnames(data_additional)[which(names(data_additional) == "Gender")] <- "Sex"

data <- data %>% mutate(Name = toupper(Name))
data_additional <- data_additional %>% mutate(Name = toupper(Name))

total <- merge(data, data_additional, by=c("Name","Year"))

biathlone <- total %>% 
  filter(Discipline == "10km Sprint" & Medal.x == "Gold") %>%
  select(ID, Name, Age, Year, Event,Result)

  
ggplot(biathlone, aes(x=Year, y=Result,group=1)) +
  geom_line() +
  labs(title = "Results of 10km Sprint in Biathlone Gold Winners") +
  geom_text(label=biathlone$Name,size = 3)
