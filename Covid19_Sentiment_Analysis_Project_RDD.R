###--------------------------------------------------------------------------###
###       Capstone Project                                                   ###
###      Singapore COVID cases Sentiment Analysis                            ###
###--------------------------------------------------------------------------###


# Problem Statement ----

# Coronavirus disease(COVID19) is an infectious desease caused by a newly discovered coronavirus.
# It has spread to numerous countries across all continent since the first discovery in Wuhan, CHina
# back in Nov 2019 and was declared as pandemic by WHO on March 11.
# 
# Various countries has came out measure/restrictions to respond to COVID-19.
# Since "circuit breaker", a partial nationwide lockdown, where only essential services could allow opened,
# SG residents has started to feel a great impact on daily life
# where they are encouraged to stay home as much as possible and wearing of mask became mandatory when going out.
# SG government has constantly revising policy and social restrictions.
# Three phases of planned reopening were announced on 19 May
# namely "Safe Reopening" (Phase1) "Safer Transition" (Phase2), and finally "Safe Nation" (Phase3)
# 
# In our Capstone Project,
# we perform sentiment analysis and modeling on the tweets about COVID19
# and seek to answer the following research questions:
# 
# 1. What are the main prevalent sentiment and emotions expressed in words in Singapore tweets
# about current COVID situation?
# 
# 2. Is there any change of sentiment over a period of time
# amidst global reopening with higher vaccination rate, in contrast growing new daily cases/death locally?
install.packages("kableExtra")
getwd()
rm(list = ls())
list.files(path=getwd(), pattern = "^covid19_twitter[0-9_]*.csv", full.names=TRUE)
# Activated Packages ----

#For our data science project, we activated the following packages, 

# install.packages(tidyverse, lubridate, 
#                   broom, modelr, tidytext, wordcloud2, 
#                   textdata  
#                   jtools, huxtable, 
#                   psych, Hmisc, car, sandwich, 
#                   ggthemes, scales, ggstance, 
#                   gvlma, ggfortify,
#                   gtrendsR, rtweet,
#                   rvest, glue)
pacman::p_load(tidyverse, lubridate, # tidy data science principle
               broom, modelr, tidytext, wordcloud2, wordcloud, reshape2,
               textdata,   # Employing Lexicon
               jtools, huxtable, gridExtra,
               psych, Hmisc, car, sandwich, 
               ggthemes, scales, ggstance, 
               gvlma, ggfortify,
               gtrendsR, rtweet,
               rvest, glue)

load("Covid19_Proj.RData")

sessionInfo()

# IMPORT Data Source 1 : SG COVID DATA ----

# df_case <- read_csv("sg_number-of-community-cases-by-age.csv")
# df_death_icu <- read_csv("sg_patients-needing-oxygen-supplementation-icu-care-or-died-by-age-groups.csv")
#   
# df_sg_cases <- list.files(path = getwd(),           # Identify all csv files in folder
#                             pattern = "^sg_number-of-community-cases-by-age[0-9_]*.csv", full.names = TRUE) %>% 
#   lapply(read_csv) %>%                                            # Store all files in list
#   bind_rows                                                       # Combine data sets into one data set 
# 
# df_sg_cases <- unique(df_sg_cases)   
# plot(df_sg_cases$pr_date, df_sg_cases$count_of_case )


df_case_last1 <- read_csv("sg_number-of-community-cases-by-age_16_11.csv")
df_case_last2 <- read_csv("sg_number-of-community-cases-by-age_01_11.csv")
df_case_last <- unique(bind_rows(df_case_last2, df_case_last1))
min(df_case_last1$pr_date); max(df_case_last1$pr_date)
view(df_sg_death_icu_last)

df_case_first <- read_csv("sg_number-of-community-cases-by-age_15_10.csv")
min(df_case_last$pr_date); max(df_case_last$pr_date)

date_before_case <- min(df_case_last$pr_date)

df_case_first <- df_case_first %>% 
  filter(pr_date < date_before_case)

df_sg_cases<- unique(bind_rows(df_case_first, df_case_last))
min(df_sg_cases$pr_date); max(df_sg_cases$pr_date)

df_sg_death_icu_last1<- read_csv("sg_patients-needing-oxygen-supplementation-icu-care-or-died-by-age-groups_16_11.csv")
df_sg_death_icu_last2<- read_csv("sg_patients-needing-oxygen-supplementation-icu-care-or-died-by-age-groups_01_11.csv")
df_sg_death_icu_last <- unique(bind_rows(df_sg_death_icu_last1, df_sg_death_icu_last2))
min(df_sg_death_icu_last$day_of_as_of_date); max(df_sg_death_icu_last$day_of_as_of_date)
view(df_sg_death_icu_last)

df_sg_death_icu_first <- read_csv("sg_patients-needing-oxygen-supplementation-icu-care-or-died-by-age-groups_15_10.csv")
min(df_sg_death_icu_first$day_of_as_of_date); max(df_sg_death_icu_last$day_of_as_of_date)

df_sg_death_icu_last <- df_sg_death_icu_last %>% 
  mutate(age_groups = ifelse(age_groups == "70+ years old", "70 years old and above", age_groups),
         age_groups = ifelse(age_groups == "61 - 70 years old", "60 - 69 years old", age_groups),
         age_groups = ifelse(age_groups == "40 - 60 years old", "40 - 59 years old", age_groups),
         age_groups = ifelse(age_groups == "19 - 39 years old", "20 - 39 years old", age_groups),
         age_groups = ifelse(age_groups == "12 - 18 years old", "12 - 19 years old", age_groups),
         clinical_status = ifelse(clinical_status == "Deceased (based on date of death)", "Deceased", clinical_status),
         clinical_status = ifelse(clinical_status == "Unstable and Under Close Monitoring in ICU", "ICU", clinical_status),
         clinical_status = ifelse(clinical_status == "In Intensive Care Unit", "ICU", clinical_status),
         clinical_status = ifelse(clinical_status == "Requires Oxygen Supplementation", "Require_Oxygen", clinical_status),
         clinical_status = ifelse(clinical_status == "Requires Oxygen Supplementation in General Ward", "Require_Oxygen", clinical_status)
  ) %>% 
  filter(!clinical_status == "Critically ill and Intubated in ICU")

date_before <- min(df_sg_death_icu_last$day_of_as_of_date)

df_sg_death_icu_first <- df_sg_death_icu_first %>% 
  mutate(age_groups = ifelse(age_groups == "70+ years old", "70 years old and above", age_groups),
         age_groups = ifelse(age_groups == "61 - 70 years old", "60 - 69 years old", age_groups),
         age_groups = ifelse(age_groups == "40 - 60 years old", "40 - 59 years old", age_groups),
         age_groups = ifelse(age_groups == "19 - 39 years old", "20 - 39 years old", age_groups),
         age_groups = ifelse(age_groups == "12 - 18 years old", "12 - 19 years old", age_groups),
         clinical_status = ifelse(clinical_status == "Deceased (based on date of death)", "Deceased", clinical_status),
         clinical_status = ifelse(clinical_status == "Unstable and Under Close Monitoring in ICU", "ICU", clinical_status),
         clinical_status = ifelse(clinical_status == "In Intensive Care Unit", "ICU", clinical_status),
         clinical_status = ifelse(clinical_status == "Requires Oxygen Supplementation", "Require_Oxygen", clinical_status),
         clinical_status = ifelse(clinical_status == "Requires Oxygen Supplementation in General Ward", "Require_Oxygen", clinical_status)
         ) %>% 
  filter(!clinical_status == "Critically ill and Intubated in ICU") %>% 
  filter(day_of_as_of_date < date_before)

df_sg_death_icu <- unique(bind_rows(df_sg_death_icu_last, df_sg_death_icu_first))
min(df_sg_death_icu$day_of_as_of_date); max(df_sg_death_icu$day_of_as_of_date)


#df_oxygen <- read_csv("cases-in-icu-or-requires-oxygen-supplement.csv")
#df_home_care_treatment_hosp <- read_csv("breakdown-of-number-of-active-cases-in-hospital-covid-19-treatment-facilities-community.csv")

# Within the dataset, there are daily new cases as well as propotion of cases in serious condition like hospitalized,
# in treatment facilities and ICU.
# which will serve as overview of the current COVID situation.

df_sg_death_icu <- df_sg_death_icu %>% 
  pivot_wider(names_from = clinical_status, values_from = count_of_case) %>% 
  rename(pr_date = day_of_as_of_date,
         age_group = age_groups) %>% 
  # mutate(age_group = ifelse(age_group == '70 years old and above','70+',age_group)) %>% 
  select(1:3, ICU, Hospitalised)


df_sgcovid <- df_sg_cases %>% 
  inner_join(df_sg_death_icu, by = c("pr_date", "age_group")) %>% 
  mutate(week = cut.Date(pr_date, breaks = "1 week", labels = FALSE)) %>% 
  print(n =nrow(.))

# Replace all NA with 0
df_sgcovid[is.na(df_sgcovid)] = 0

min(df_sgcovid$pr_date); max(df_sgcovid$pr_date)

df_sg_cases %>% filter(pr_date == "2021-11-05")

write_csv(df_sg_cases, "covid19_data_gov_sg_0917_1114_.csv")

# --------------------------------------   SG dataset ready -----------------------------------------

# Plot cases trend chart
df_sgcovid %>% 
  group_by(pr_date) %>% 
  summarise(count = sum(count_of_case)) %>% 
  ggplot(aes(pr_date, count)) +
  geom_line(size = 1)
  
# Dataset with daily count
df_sgcovid_daily <- df_sgcovid %>%
  group_by(pr_date, week) %>%
  summarise(
    cases = sum(count_of_case),
    deceased = sum(Deceased, na.rm = TRUE),
    ICU = sum(ICU, na.rm = TRUE),
    hospitalised= sum(Hospitalised)
  )

# Trend chart for all categories
df_sgcovid_daily %>% 
  ggplot(aes(pr_date, deceased)) +
  geom_line(color = "red") +
  geom_line(aes(y = hospitalised), color="blue") +
  geom_line(aes(y = cases), color="purple") +
  geom_line(aes(y = ICU), color = "orange")


# IMPORT Data Source 2 : TWEETER DATA ----

# Twitter API details

twitter_token <- create_token(
  app = "Twitter_Mining_DAMI", # Type your app name
  consumer_key = "LA9W4o4IqrgnZ7gaN2XtrL1ed", # Type your consumer key
  consumer_secret = "JvHhQXpTi3gqPgHkkhjszQsI9cHkcv9qgbYUoOVKvNWtYYyp6Z", # Type your consumer secret
  set_renv = T)

twitter_token <- create_token(
  app = "Twitter_Mining_DAMI", # Type your app name
  consumer_key = "McyQen9UsP9muXW6TVal3sffS", # Type your consumer key
  consumer_secret = "KzzuaYwZzofbUdYk7THNSUMH6sv5EW8COat5U8LNoPAjIEaJz1", # Type your consumer secret
  set_renv = T)

# get_token()

# If you wish to extract 18000 tweets sent from Singapore with #covid19
covid19_twt_sg <- search_tweets(q = "#coronavirus OR #covid19 OR #COVID OR #stayhome OR #Covid-19 OR #pandemic OR #virus OR #social-distance OR #self-quarantine OR $swab-test OR #PCR OR #infection-rate", 
                                        n = 3000, 
                                        lang = "en",
                                        include_rts = F,
                                        geocode = lookup_coords("singapore")
)

min(covid19_twt_sg$created_at);max(covid19_twt_sg$created_at)
nrow(covid19_twt_sg)

dataset <- read_csv("covid19_twitter_18_10_2021.csv")
write_csv(covid19_twt_sg, "covid19_twitter_16_11_2021.csv")
# covid19_twt_sg <- read_csv("covid19_twitter_18_10_2021.csv")
list.files()

min(dataset$created_at);max(dataset$created_at)
dataset


twitter_all <- list.files(path = getwd(),           # Identify all csv files in folder
                       pattern = "^covid19_twitter[0-9_]*.csv", full.names = TRUE) %>% 
  lapply(read_csv) %>%                                            # Store all files in list
  bind_rows                                                       # Combine data sets into one data set 
Twitter_data_all                                                          # Print data to RStudio console

# dataset time range and number of rows
twitter_all <- twitter_all %>% 
  mutate(created_at = as.POSIXlt(created_at, tz="Asia/Singapore"))
min(twitter_all$created_at);max(twitter_all$created_at)

## Remove incomplete daily data.
twitter_complete_day <- twitter_all %>% 
  filter(as.Date(created_at) != min(as.Date(created_at))) %>% 
  filter(as.Date(created_at) != max(as.Date(created_at)))
min(twitter_complete_day$created_at);max(twitter_complete_day$created_at)

nrow(twitter_complete_day)

# Top 10 screen name
twitter_complete_day %>% count(screen_name) %>% arrange(desc(n))
view(twitter_complete_day)

# Number of tweet in time
twitter_complete_day %>% count(as.Date(created_at)) %>% print(n=nrow(.))

# Data cleansing

twitter_complete_day %>% 
  count(screen_name, text) %>% 
  arrange(desc(n)) %>% 
  print(n = 100)

## Remove URL links & Alias
sg_twitter_no_url <- twitter_complete_day %>% 
  mutate(text = sub("\\s+https.*","", text)) %>% 
  mutate(text = sub("@\\w+ *","", text)) 

sg_twitter_no_url %>% 
  count(screen_name, text) %>% 
  arrange(desc(n)) %>% 
  print(n = 100)

# View tweeter count over time.
sg_twitter_no_url %>% 
  count(as.Date(created_at)) %>% 
  rename(date = `as.Date(created_at)`) %>% 
  ggplot(aes(date, n)) +
  geom_line()

## To remove duplicate text on different date (case-sensitive)
sg_tweets_sorted <-  sg_twitter_no_url[order(sg_twitter_no_url$text,-as.numeric(sg_twitter_no_url$created_at)),]
sg_tweets_no_url_dupl <- sg_tweets_sorted[!duplicated(sg_tweets_sorted$text),]

# test <- sg_tweets_sorted %>% 
#   filter(str_detect(text,"^Beijing Games competitors to")) %>% 
#   select(screen_name, text, created_at)
# 
# sg_tweets_no_url_dupl %>% 
#   filter(str_detect(text,"^Beijing Games competitors to")) %>% 
#   select(screen_name, text, created_at)

## Verify if duplicated rows exist.
sg_tweets_no_url_dupl %>% 
  count(screen_name, text) %>% 
  arrange(desc(n))

# View tweeter count over time after cleaning duplicates
sg_tweets_no_url_dupl %>% 
  count(as.Date(created_at)) %>% 
  rename(date = `as.Date(created_at)`) %>% 
  ggplot(aes(date, n)) +
  geom_line()

write_csv(sg_tweets_no_url_dupl, "covid19_sg_tweeter_1010_1115.csv")

# --------------------------------------   SG covid tweets dataset ready -----------------------------------------

##################################################
# if we need to extract subset for analysis period
##################################################
test <- read_csv("covid19_sg_tweeter_1010_1115.csv")
test$text

sg_twitter_analysis_period <- sg_tweets_no_url_dupl %>% 
  filter(created_at >= as.Date('2021-10-10') & created_at <= as.Date('2021-10-31')) 

min(sg_twitter_analysis_period$created_at);max(sg_twitter_analysis_period$created_at)


check_data <- twitter_analysis_period %>% 
  filter(twitter_analysis_period$screen_name == 'SixStoneJars1') %>% 
  filter(str_detect(text,"^Chinatown Food Street closes"))



# Basic EDA on amount of tweet in time (ALL)
options(repr.plot.width=15, repr.plot.height=9)

#chart_all <- 
  sg_tweets_no_url_dupl %>% 
  select(created_at) %>% 
  mutate(date = ymd(as.Date(created_at))) %>% 
  group_by(date) %>% 
  summarise(n = n(), .groups = "drop_last") %>%
  ggplot(aes(x=date, y = n)) + 
  geom_line(size = 1, color = my_colors[1]) +
  coord_cartesian(clip = 'off') +
  geom_vline(xintercept = as.Date('2021-10-20'), linetype="dotted", size = 1.5, color = "red") +
  geom_vline(xintercept = as.Date('2021-11-08'), linetype="dotted", size = 1.5, color = "darkblue") +
  my_theme + theme(axis.title.x = element_blank()) +
  scale_x_date(date_breaks = "1 day") + 
  ggthemes::theme_fivethirtyeight() +
  theme(axis.text.x = element_text(angle = 45, size = rel(0.6), vjust = 1, hjust = 1 )) +
    # xlab("Tweets collected period") +
    # ylab("Daily Tweets") +
    # theme(axis.title = element_text()) +
  labs(title = "Number of COVID-19 Tweets shared between 10th Oct - 15th Nov", subtitle = "Number of tweets spiked on key events") +
    geom_label(aes(x=as.Date('2021-10-19'), y = 380, label = "PM Lee's address on COVID-19"), color = "red", size = 4, angle = 90, fontface = "bold") +
    geom_label(aes(x=as.Date('2021-11-07'), y = 380, label = "More Opening on COVID-19 restrictions"  ), color = "darkblue", size = 4, angle = 90, fontface = "bold") 
  

# Basic EDA on amount of tweet in time (analyzed period)
chart_analyzed <- sg_twitter_analysis_period %>% 
  select(created_at) %>% 
  mutate(date = ymd(as.Date(created_at))) %>% 
  group_by(date) %>% 
  summarise(n = n(), .groups = "drop_last") %>%
  
  ggplot(aes(x=date, y = n)) + 
  geom_line(size = 1, color = my_colors[1]) +
  coord_cartesian(clip = 'off') +
  geom_vline(xintercept = as.Date('2021-10-20'), size = 1.5) +
  my_theme + theme(axis.title.x = element_blank()) +
  labs(title = "Number of Tweets in Time", subtitle = "2021", y = "Frequency")


gridExtra::grid.arrange(chart_all, chart_analyzed, ncol=2)


#------------------------------------------------------------------------------#
#             Sentiment Analysis(1)                                            #
#             Word Cloud                                                       # 
#------------------------------------------------------------------------------#

# TIDY & TRANSFORM ####
# Step 1: Tokenization ----
# --Let's filter in those tweets that had at least 20 words within a tweet.
sg_twitter_analysis_period <- read.csv("covid19_sg_tweeter_1010_1115.csv")

sg_twitter_analysis_period <- sg_twitter_analysis_period %>% 
  mutate(created_at = as.Date(sg_twitter_analysis_period$created_at)) %>% 
  rowid_to_column("id")

tidy_tweets_token <- sg_twitter_analysis_period %>%
  select(id, created_at, status_id, text) %>% 
  filter(text != "") %>% 
  unnest_tokens(word, text, token = "tweets") #%>% # tokenization == unnesting
  #group_by(word) %>%
  #filter(n() > 20) %>% # Threshold = ML Question
  #ungroup()

data <- sg_twitter_analysis_period %>% 
  filter(created_at == as.Date('2021-11-01')) %>% 
  select(id, text)
  
view(data)

# Step 2: Cleaning ----

tidytext::stop_words

tweets_cleaned <- tidy_tweets_token %>%
  anti_join(stop_words)


# Manual cleaning of unwanted words
tweets_token_cleaned <- tweets_cleaned %>%
  filter(!word %in% c("singapore", "covid", "covid19","positive","negative") & !str_detect(word,"^#|^@") & !str_detect(word, "^[:digit:]+$"))

count(tweets_cleaned)
tweets_token_cleaned

# A Simple, yet still Useful Visualization of Tweets: Word Cloud #
## SIMPLE WORD CLOUD ----

covid_tweets_for_wc <- tweets_token_cleaned %>% 
  group_by(word) %>% 
  summarise(frequency = n()) %>% 
  arrange(desc(frequency))

covid_tweets_for_wc

covid_tweets_for_wc %>% 
  filter(frequency > 3) %>% 
  wordcloud2(backgroundColor = "black", 
             color = "random-light")

options(repr.plot.width=15, repr.plot.height=15)

# A Postive-Negative Word Cloud by using BING
## POSITVE/NEGATIVE WORD CLOUD ----
my_colors <- c("#05A4C0", "#85CEDA", "#D2A7D8", "#A67BC5", "#BB1C8B", "#8D266E", "gold4", "darkred", "deepskyblue4")

BING <- get_sentiments("bing")
BING

tweets_token_cleaned %>% 
  inner_join(BING, by="word") %>%
  count(word, sentiment, sort=T) %>% 
  acast(word ~ sentiment, value.var = "n", fill=0) %>% 
  
  # wordcloud
  comparison.cloud(colors=my_colors[c(5, 1)], max.words = 400, title.size = 2,
                   scale = c(3,.5))


#------------------------------------------------------------------------------#
#             Sentiment Analysis(2)                                            #
#             Most Postive/Negative Tweet                                      # 
#------------------------------------------------------------------------------#

AFINN <- get_sentiments("afinn")
AFINN

summary(AFINN$value)

## MOST NEGATIVE TWEET ----
# Identifying the most negative tweets #

tweets_AFINN_indexed <- tweets_token_cleaned %>% 
  inner_join(AFINN, by = "word")

tweet_level_sentiment <- tweets_AFINN_indexed %>% 
  group_by(id) %>% 
  summarise(average_sentiment = mean(value),
            n_of_words_indexed = n()
  )

tweet_level_sentiment %>% 
  arrange(average_sentiment)

sg_twitter_analysis_period %>% 
  filter(id == 2184) %>% 
  select(text) %>% 
  pull()

# #MOST POSITIVE TWEET ----
# Identifying the most positive tweet #

tweet_level_sentiment %>% 
  arrange(desc(average_sentiment))

sg_twitter_analysis_period %>% 
  filter(id == 136) %>% 
  select(text) %>% 
  pull()


tweet_level_sentiment %>% 
  arrange(average_sentiment) %>% 
  top_n(3)


#------------------------------------------------------------------------------#
#             Sentiment Analysis(3)                                            #
#             Primary Emotions Analysis                                        # 
#------------------------------------------------------------------------------#

## OVERALL EMOTION ANALYSIS ----
NRC <- get_sentiments("nrc")
NRC

table(NRC$sentiment)
NRC %>% 
  count(sentiment)


options(repr.plot.width=15, repr.plot.height=9)

my_theme <- theme(plot.background = element_rect(fill = "grey98", color = "grey20"),
                  panel.background = element_rect(fill = "grey98"),
                  panel.grid.major = element_line(colour = "grey87"),
                  text = element_text(color = "grey20"),
                  plot.title = element_text(size = 22),
                  plot.subtitle = element_text(size = 17),
                  axis.title = element_text(size = 15),
                  axis.text = element_text(size = 15),
                  legend.box.background = element_rect(color = "grey20", fill = "grey98", size = 0.1),
                  legend.box.margin = margin(t = 3, r = 3, b = 3, l = 3),
                  legend.title = element_blank(),
                  legend.text = element_text(size = 15),
                  strip.text = element_text(size=17))

# The plot:
tweets_token_cleaned %>% 
  inner_join(NRC, "word") %>%
  filter(!sentiment %in% c("positive", "negative")) %>% 
  count(sentiment, sort=T) %>% 
  ggplot(aes(x=reorder(sentiment, n), y=n)) +
  geom_bar(stat="identity", aes(fill=n), show.legend=F) +
  geom_label(aes(label=format(n, big.mark = ",")), size=5, fill="white") +
  labs(x="Sentiment", y="Frequency", title="What is the overall mood in Tweets?") +
  scale_fill_gradient(low = my_colors[3], high = my_colors[1], guide="none") +
  coord_flip() + 
  my_theme + theme(axis.text.x = element_blank())



#------------------------------------------------------------------------------#
#             Sentiment Analysis(4)                                            #
#             Sentiment split by word frequency                                 # 
#------------------------------------------------------------------------------#

## SENTIMENT SPLIT BY WORD FREQUENCY ----
options(repr.plot.width=15, repr.plot.height=9)

tweets_token_cleaned %>% 
  inner_join(NRC, "word") %>% 
  count(sentiment, word, sort=T) %>%
  group_by(sentiment) %>% 
  arrange(desc(n)) %>% 
  slice(1:7) %>% 
  
  # Plot:
  ggplot(aes(x=reorder(word, n), y=n)) +
  geom_col(aes(fill=sentiment), show.legend = F) +
  facet_wrap(~sentiment, scales = "free_y", nrow = 2, ncol = 5) +
  coord_flip() +
  my_theme + theme(axis.text.x = element_blank()) +
  labs(x="Word", y="Frequency", title="Sentiment split by most frequent words") +
  scale_fill_manual(values = c(my_colors, "#BE82AF", "#9D4387", "#DEC0D7",
                               "#40BDC8", "#80D3DB", "#BFE9ED"))


classified_sentiment <- tweets_token_cleaned %>% 
  inner_join(NRC, "word") %>% 
  group_by(sentiment, created_at) %>% 
  summarise(cnt = n())

tweets_token_cleaned %>% 
  inner_join(NRC, "word") %>% 
  group_by(sentiment, created_at) %>% 
  summarise(cnt = n()) %>% 
  filter(sentiment == 'joy' ) %>% 
  print(n = nrow(.))

tweets_token_cleaned %>% 
  inner_join(NRC, "word") %>% 
  filter(sentiment == 'joy' & created_at == '2021-10-22')
  
NRC %>%  filter(word == 'beating')
  # Plot:
classified_sentiment %>% 
  filter(!sentiment %in% c("positive", "negative")) %>% 
  ggplot(aes(x=created_at, y =cnt, color = sentiment)) +
  geom_point() +
  facet_wrap(~sentiment, scales = "free_y", nrow = 2, ncol = 4) +
  geom_vline(xintercept = as.Date('2021-10-20'), size = 1) 


# Another view in radar chart

char_sentiment <- classified_sentiment %>% 
  filter(!sentiment %in% c("positive", "negative")) %>%
  mutate (covid_address_effect = as.factor(ifelse(created_at < '2021-10-20',0,1))) %>% 
  group_by(sentiment, covid_address_effect) %>% 
  summarise(char_sentiment_count = sum(cnt)) 

# classified_sentiment %>%
#   mutate (covid_address_effect = as.factor(ifelse(created_at >= '2021-10-20',0,1))) %>%
#   filter(sentiment == "joy" & covid_address_effect == 0) %>%
#   mutate( sum_all = sum(cnt))

total_char <-   classified_sentiment %>% 
  filter(!sentiment %in% c("positive", "negative")) %>%
  mutate (covid_address_effect = as.factor(ifelse(created_at < '2021-10-20',0,1))) %>% 
  group_by(covid_address_effect) %>% 
  summarise(total = sum(cnt))

# install.packages("radarchart")
library(radarchart)
# Radar Chart:
char_sentiment %>% 
  inner_join(total_char, by="covid_address_effect") %>% 
  mutate(percent = char_sentiment_count / total * 100 ) %>% 
  select(-char_sentiment_count, -total) %>% 
  spread(covid_address_effect, percent)  %>% 
  chartJSRadar(showToolTipLabel = T, main="covid_address_effect Tweets and Emotion", maxScale=25, responsive=T,
               addDots = T, 
               colMatrix = grDevices::col2rgb(my_colors[c(2,4)]),
               lineAlpha = 0.7, polyAlpha = 0.2)

#------------------------------------------------------------------------------#
#             Sentiment Analysis(3)                                            #
#             Modelling to Compare Sentiment Changes over period               # 
#------------------------------------------------------------------------------#

# Here, we are interested in a research question:
# Does Key COVID events on Oct. 20 change the public sentiment or more trusts in term of leadership effects?
# 1 ) PM's address the nation on COVID-19 situation in Singapore:
# * the path to a "New Normal", diverted from original zero COVID approach and to live with COVID19.
# * local cases spiked sharply over the past few weeks
# * asked for unity and COVID resilience.
# 2 ) Announcement  on COVID social curbs to be extended another month to Nov21. originally slated to be in place until Oct. 24.
# * Dining out to 2 people
# * Work from home remains the default.

# We are going to use Regression Discontinuity Analysis on the causal inference and relationship.
# The date cut off will be on Oct. 20.
# We expect to observe there is a high volume on Oct. 20, jump in sentiment score/count on Oct. 19 and Oct. 21


# Overall positive correlation between sentiment and the progression date.

sentiment_daily <- tweets_AFINN_indexed %>% 
  group_by(created_at) %>% 
  summarise(average_sentiment = mean(value),
            n_of_words_indexed = n()) 

  # Plot
  sentiment_daily %>% 
  ggplot(aes(x = created_at, y = average_sentiment)) +
  geom_point() +
  geom_vline(xintercept = as.Date('2021-10-20'), size = 1) 


merged_dataset <- df_sgcovid_daily %>% 
  inner_join(sentiment_daily, by = c("pr_date" = "created_at")) %>% 
  print(width = Inf)

# add dummy variable for pre-effect = 0, and post-effect = 1
merged_dataset <- merged_dataset %>% 
  mutate (covid_address_effect = as.factor(ifelse(pr_date < '2021-10-20',0,1)))

#1 add count for JOY sentiment_class 
joy_sentiment <- classified_sentiment %>% 
  ungroup() %>% 
  filter(sentiment == 'joy') %>% 
  rename(joy_cnt = cnt) %>% 
  select(created_at, joy_cnt)
  
merged_dataset <- merged_dataset %>% 
  inner_join(joy_sentiment, by = c("pr_date" = "created_at")) 


merged_dataset %>% 
  lm(joy_cnt ~ covid_address_effect + I(pr_date - as.Date('2021-10-20')),.) %>% 
  summary()


merged_dataset %>% 
  ggplot(aes(x = pr_date, y = joy_cnt)) +
  geom_point(aes(color = covid_address_effect)) + 
  geom_smooth(method = "lm")

#2 add count for sentiment_class 
joy_sentiment <- classified_sentiment %>% 
  ungroup() %>% 
  filter(sentiment == 'joy') %>% 
  rename(joy_cnt = cnt) %>% 
  select(created_at, joy_cnt)

merged_dataset <- merged_dataset %>% 
  inner_join(joy_sentiment, by = c("pr_date" = "created_at")) 


merged_dataset %>% 
  lm(average_sentiment ~ covid_address_effect + I(pr_date - as.Date('2021-10-20')),.) %>% 
  summary()


merged_dataset %>% 
  ggplot(aes(x = pr_date, y = average_sentiment)) +
  geom_point(aes(color = covid_address_effect)) + 
  geom_smooth(method = "lm")


# Our moderator (categorial x2), address_effect, 
# is contained as follows: 1 being pre nation address; 2 being post nation address

# From the chart, showing [ causal effect or null effects ] on the regression line

RDD <- merged_dataset %>% 
  ggplot(aes(x = pr_date, y = average_sentiment, color = covid_address_effect)) +
  geom_point() + 
  geom_smooth(method = "lm")

# Difference in Means Test
RDD_box <- merged_dataset %>% 
  ggplot(aes(x = Date, y = average_sentiment, color = covid_address_effect)) +
  geom_boxplot(outlier.colour="black",
               outlier.size=2, notch=FALSE) + 
  geom_point()

gridExtra::grid.arrange(RDD, RDD_box, ncol=2)

# conduct a difference of means test, with hypothesis: H0 : mean of pre-address_effect = mean of post-address_effect
merged_dataset %>%
  t.test(joy_cnt ~ covid_address_effect, .)




# Estimation strategy: same slopes
# install.packages("rddtools")
library(rddtools)

as.numeric(format(merged_dataset$pr_date, format = "%d"))
rdd_data(merged_dataset$joy_cnt, as.numeric(format(merged_dataset$pr_date, format = "%d")), cutpoint = 20) %>% 
  rdd_reg_lm(slope = "same") %>% 
  summary()

# joy count distribution
plot(merged_dataset$pr_date, merged_dataset$joy_cnt)


# OLS Regression for Interaction Effects Model

M1_lm <- merged_dataset %>% 
  lm(joy_cnt ~ 
       deceased + cases + average_sentiment + covid_address_effect,
     .)

M2_lm <- merged_dataset %>% 
  lm(joy_cnt ~ 
       (deceased + cases + average_sentiment ) * covid_address_effect,
     .)

summary(M1_lm)
tidy(M2_lm);glance(M2_lm)

summ(M1_lm, center = T)

# Besides, Simple Regression, we perform poisson regression analysis

mean(merged_dataset$joy_cnt)
var(merged_dataset$joy_cnt)

sum(is.na(merged_dataset$joy_cnt))

M1_Pm <- merged_dataset %>% 
  glm(joy_cnt ~ covid_address_effect + average_sentiment + cases, family="poisson",
      .)

M2_Pm <- merged_dataset %>% 
  glm(joy_cnt ~ covid_address_effect + average_sentiment + cases, family = quasipoisson(link = "log"),
      .)

summ(M1_Pm)
summ(M2_Pm)

2*

# Report Regression Analysis with Multiple Models using export_summs()

export_summs(M1_lm, M2_lm,
             error_format = "[{conf.low}, {conf.high}],
                             p = {p.value}",
             model.names = c("Main Effects only",
                             "with Interactions"),
             digits = 3)

summary(merged_dataset$average_sentiment)


# MODEL ----

model1_lm <- merged_dataset %>% 
  lm(cases ~ average_sentiment,
     .)

summary(model1_lm)

## Assumption Checks ----
gvlma(model1_lm)

autoplot(gvlma(model1_lm))


# ## Interaction Model ----
# 
# model2_log <- merged_dataset %>% 
#   lm(log(cases) ~ average_sentiment * as.factor(age_group),
#      .)
# 
# gvlma(model2_log)  ## LInk funcrtion not accepted
#                    ## Look for other interaction model.
# 
# export_summs(model1_lm, model2_log,
#              model.names = c("OLS\nMain Effects only",
#                              "OLS\nwith Interactions"),
#              error_format = "[{conf.low}, {conf.high}]",
#              digits = 3
# )

# ## Check Linear regression 
# # In order to find out whether each closely monitored covid situation can affect the overall sentiment,
# # we will do linear regression analysis. 
# # We will regress log of average sentiment on the different category of severity as well as total cases.
# 
# merged_dataset_rename <- merged_dataset %>% 
#   rename( ICU = `In Intensive Care Unit`,
#           Oxy = `Requires Oxygen Supplementation`) %>% 
#    mutate_at(c(3:7), ~replace(., is.na(.), 0))
# 
# model2_lm <- merged_dataset_rename %>% 
#   lm(average_sentiment ~ cases + Deceased + ICU + Oxy + Hospitalised,
#      .)
# 
# summary(model2_lm)
# # So, it looks like cases and ICU may influence more on public sentiment. 

# Variables-level information: broom::tidy()

model2_variables_info_tbl <- tidy(model2_lm)

model2_variables_info_tbl <- model2_variables_info_tbl %>% 
  mutate(t = estimate / std.error,
         for_95_CIs = 1.96 * std.error,
         CIs_Lower_Bound = estimate - for_95_CIs,
         CIs_Upper_Bound = estimate + for_95_CIs,
         significance = ifelse(abs(estimate) > abs(for_95_CIs), 
                               "significant",
                               "null effect"), # DONE
         cross_validation = ifelse(CIs_Lower_Bound < 0 &
                                     CIs_Upper_Bound > 0,
                                   "null effect",
                                   "significant")
  )


## Check the distribution of your Y variable ----

theme_set(theme_linedraw())

Density_plt <- merged_dataset %>% 
  ggplot(aes(x = cases)
  ) + 
  geom_density(fill = "deepskyblue4",
               alpha = 0.2)

histogram_plt <- merged_dataset %>% 
  ggplot(aes(x = cases)
  ) + 
  geom_histogram(bins = 50, # NOT robust
                 alpha = .30) 

grid.arrange(Density_plt, histogram_plt, nrow=2)


#------------------------------------------------------------------------------#
#             Sentiment Analysis(4)                                            #
#             MODEL: Analysis of Variance (ANOVA)             
#             One-Way ANOVA:
#                Null hypothesis: the means of the different groups are the same
#                Alternative hypothesis: At least one sample mean is not equal to the others.
#             Tukey's HSD pairwise test
#------------------------------------------------------------------------------#

### Data Preparations for Conducting One-Way ANOVA
stats::lm(formula = y ~ x, data = data)
# y variable = dbl. = Numerical Variable (Normal distribution)
# glm(formula = y ~ x, 
#    data = data, 
#    family = binomial(log = "logit")
#    )
# x variable = dbl. = Numerical Variable

stats::aov(formula = y ~ x, data = data)
# y variable = dbl. = Numerical Variable
# x variable = fct. = Categorical Variable

# aov() is a command for Analysis of Variance. 
# It takes the form of outcome ~ predictor. 
# Please put the name of the data after the data command. 
# Please assign the results of the analysis of variance 
# into an object named obj.aov.x1.

glimpse(merged_dataset)
week_level_sentiment <- merged_dataset %>% 
  mutate(week_factor = as.factor(week)
  )

week_level_sentiment

m1_aov <- aov(formula = average_sentiment ~ week_factor, 
              data = week_level_sentiment)
summary(m1_aov)


m1_aov_tibble <- m1_aov %>% 
  tidy()

m1_aov_tibble

m1_aov_tibble[1, 4] / m1_aov_tibble[2, 4]
m1_aov_tibble[1, 5]

## Post-Hoc Test
# For the post-hoc comparison, 
# let’s use Tukey’s Honest Significant Difference (HSD) method. 
# For each option, you input the factor 
# that you wish to examine with the post-hoc comparison.

# In ANOVA test, a significant p-value indicates that some of the gourp means are different,
# but we don't know which pairs of groups are different. 
# We perform multiple pairwise-comparison to determine if the mean difference betwen 
# specific pairs of group are statistically significant

pairwise_comparisons <- TukeyHSD(m1_aov, 
                                 which = "week_factor", 
                                 ordered = F)

pairwise_comparisons_tibble <- tidy(pairwise_comparisons)

pairwise_tibble <- pairwise_comparisons_tibble %>% 
  select(c(-term, -null.value)
  ) %>% 
  rename(week_pairs = contrast,
         sample_M_diff = estimate,
         population_M_diff_lower = conf.low,
         population_M_diff_upper = conf.high,
         adjusted_p_value = adj.p.value)

pairwise_tibble
# Can be seen from the output which pair is significant with an adjusted p-value of < 0.05

week_level_sentiment %>% 
  ggplot(aes(x = week_factor,
             y = average_sentiment)
  ) +
  stat_summary(fun.data = "mean_cl_normal",
               geom = "errorbar",
               color = "deepskyblue4") +
  stat_summary(fun.data = "mean_cl_normal",
               geom = "pointrange",
               color = "red") +
  coord_cartesian(ylim = c(-1, 2)
  ) +
  ggthemes::theme_fivethirtyeight()

# It will be better if you present your results with a visual display.

plot(TukeyHSD(m1_aov, which = "week_factor"), cex.axis = 0.5)

### How To Report Results: Create a MS word document
install.packages("MBESS")
library(MBESS)

install.packages("apaTables")
library(apaTables)

install.packages("Hmisc")
library(Hmisc)

apa.aov.table(m1_aov,
              conf.level = 0.95,
              type = 3,
              table.number = 1,
              filename = "ANOVA_overall_F_test.doc")

apa.1way.table(dv = average_sentiment,
               iv = week_factor,
               data = week_level_sentiment,
               show.conf.interval = T,
               table.number = 2,
               filename = "ANOVA_pairwise_test.doc")

# VISUALIZATION of Your Modelling Results ####

## Bar plot & Error Bar ----

mean_parameters <- week_level_sentiment %>% 
  ggplot(aes(x = week_factor,
             y = average_sentiment)
  ) + 
  geom_bar(stat = "summary",
           fun = "mean",
           aes(fill = week_factor),
           width = 0.75) +
  ggthemes::scale_fill_wsj() +
  stat_summary(fun.data = "mean_cl_normal",
               geom = "errorbar",
               color = "purple",
               width = 0.25,
               size = 1.5) + 
  ggthemes::theme_fivethirtyeight() +
  coord_cartesian(ylim = c(-1, 2)
  ) + 
  theme(legend.position = "none")

mean_parameters

## Box plot ----

set.seed(090401)

median_parameters <- week_level_sentiment %>% 
  ggplot(aes(x = week_factor,
             y = average_sentiment)
  ) + 
  geom_boxplot(aes(color = week_factor),
               notch = T) + 
  geom_jitter(aes(color = week_factor),
              alpha = 0.3) +
  ggthemes::scale_color_wsj() +
  ggthemes::theme_fivethirtyeight() + 
  coord_cartesian(ylim = c(-2, 3)) +
  labs(title = "Sentiment Change over different period of time",
       subtitle = "Reopening in sight",
       caption = "Source: Twitter REST Stream API",
       y = "Sentiment Scores\n(estimated by AFINN)",
       x = "Time Frame (of COVID developement )") + 
  theme(legend.position = "none",
        axis.title = element_text()
  )

median_parameters + 
  stat_summary(fun.data = "mean_cl_normal",
               geom = "errorbar",
               color = "purple",
               size = 1.5) + 
  coord_flip()

# Interpretation of the Results ----

# From our data science project, we could find the following two findings:

# 1. 

# 2. 

# To answer the question timeseries data for a relatively long time period is needed 
# which means a very high number of tweets. If you have only a free Twitter developer account, 
# the number of tweets you can get is limited. Therefore, another data source is needed.

# We can costumised for local slang words for NRC dictionary to better tune the posivite or negative sentiment.
# Conclusion.

save.image("Covid19_Proj.RData")

sessionInfo()

# Thank You.