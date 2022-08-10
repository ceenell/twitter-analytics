library(targets)
library(tidyverse)
library(rtweet)
library(lubridate)
library(data.table)
library(stringr)
library(patchwork)

col_blue <- '#1DA1F2'

# who are our followers? --------------------------------------------------

# list of user_ids
ds_flw <- get_followers("USGS_DataSci", n = 10000)
ds_flw
ds_flw_data <- lookup_users(ds_flw$user_id)
ds_flw_data

# total number of followers
length(unique(ds_flw_data$user_id))

# followers through time
ds_flw_ct <- read_csv('in/FollowerCount.csv') %>% 
  mutate(Date = as.Date(Date, format = '%m/%d/%y')) 
view(ds_flw_ct)
range(ds_flw_ct$Date)
ds_flw_hist <- ds_flw_ct %>%
  add_row(Date = as.Date('08/01/15', format = '%m/%d/%y'), `Follower Count` = 0)

ds_flw_ct %>% 
  filter(Date >= as.Date('11/01/19', format = '%m/%d/%y')) %>% 
  ggplot(aes(Date, `Follower Count`))+
  #geom_point(aes(), color=col_blue, size = 1, shape =)+0
  geom_line(color=col_blue, linetype = "solid", size = 2)+
  geom_line(data = ds_flw_hist, color=col_blue, linetype = "dotted", size = 2)+
  #ylim(0, NA)+
  theme_classic(base_size=18)+
  scale_x_date(breaks=scales::breaks_width("1 year"),
               labels = scales::label_date_short())+
  labs(x='', y="Followers")


# followers network -------------------------------------------------------

ds_df <- read_csv('twitter_friends.csv', col_types = "cc")
unique(ds_df$ds_flw)

##  find friends of friends
user_fr <- get_friends(follow_list[i], n = 2000, retryonratelimit = TRUE)

follow_list <- setdiff(ds_flw$user_id, unique(ds_df$ds_flw))
follow_list
i<-1
for (i in 1:length(follow_list)){
  user_fr <- get_friends(follow_list[i], n = 2000, retryonratelimit = TRUE) # who that user follows
  
  # compare to all of DS followers to see if they follow any other accounts
  user_ds_friends <- intersect(user_fr$user_id, ds_flw$user_id)
  
  user_df <- data.frame(ds_flw = rep(follow_list[i], length(user_ds_friends)), 
             user_fr = user_ds_friends) %>%
    add_row(ds_flw = follow_list[i],
            user_fr = "blank")
  
  ds_df <- rbind(ds_df, user_df)%>%
    write_csv('twitter_friends.csv')
}
str(ds_df)

ds_df <- read_csv('twitter_friends.csv')
length(unique(ds_df$ds_flw)) #68

# what do we tweet' -------------------------------------------------------

# get tweets from all time
tw_all <- get_timelines("USGS_DataSci", n = 3000)
tw_all %>% str(., max.level = 2)

# impressions, engagement, and link clicks collected manually
engage <- read_csv('in/twitter_stats.csv') %>%
  mutate(status_id = gsub("https://twitter.com/USGS_DataSci/status/", "", status_link))
str(engage)

list.files()
# tweet timeline - 
ds_flw_ct %>% 
  filter(Date >= as.Date('12/07/20', format = '%m/%d/%y')) %>% 
  ggplot(aes(Date, `Follower Count`))+
  geom_line(color=col_blue, linetype = "solid", size = 8)+
  geom_point(aes(), color="white", size = 3)+
  #geom_line(data = ds_flw_hist, color=col_blue, linetype = "dotted", size = 2)+
  #ylim(0, NA)+
  theme_classic(base_size=20)

# make columns for common hashtags
tw_hashtags <- tw_all %>%
  select(status_id, hashtags) %>%
  unnest_longer(hashtags) %>%
  mutate(hashtags = tolower(hashtags),
         val = ifelse(is.na(hashtags), 0, 1)) %>%
  group_by(status_id) %>%
  distinct() %>%
  pivot_wider(names_from = hashtags, values_from = val) %>% 
  select(status_id, dataviz, rstats, jobs, d3js, scicomm)

# first tweet?
tw_all %>%
  select(status_id, hashtags) %>%
  unnest_longer(hashtags)


# likes and retweets
 
# combine data
tw_clean <- tw_all %>%
  select(user_id:text, reply_to_screen_name:reply_count, followers_count) %>%
  mutate(date_time = as.Date(word(created_at, 1, 1))) %>% 
  mutate(month = month(date_time), 
         j = yday(date_time),
         year = year(date_time)) %>%
  filter(is_retweet == FALSE) %>%
  mutate(reply = ifelse(!is.na(reply_to_screen_name), TRUE, FALSE)) %>%
  filter(reply == FALSE) %>% # only looking at first tweets
  select(-is_retweet, -followers_count, -reply_to_screen_name, -reply) %>%
  left_join(engage %>% select(-rstats)) %>%
  left_join(tw_hashtags) %>%
  mutate(engage_rate = (engagements/impressions)*100)%>% 
  filter(date_time >= as.Date("2020-12-07")) 
str(tw_clean)

range(tw_clean$engage_rate)

tw_clean %>%
  ggplot(aes(engagements, impressions))+
  geom_point(color=col_blue, position=position_jitter(.01), size = 3, shape = 21, alpha = 0.7, stroke = 1) +
  theme_classic(base_size = 18)+
  #scale_y_log10(labels=scales::label_comma(),
  #              breaks= scales::breaks_log())+
  #scale_x_log10()+
  labs(x="Retweets", y= "Impressions")+
  theme(panel.border = element_blank())

# retweets vs impressions
tw_log <- tw_clean %>%
  ggplot(aes(retweet_count, impressions))+
  geom_hex(color="white", size=1, bins=20) +
  theme_classic(base_size = 18)+
 scale_y_log10(labels=scales::label_comma(),
               breaks= scales::breaks_log())+
 scale_x_log10()+
  labs(x="Retweets", y= "Impressions")+
  theme(panel.border = element_blank(),
        legend.position = 'none')+
  scale_fill_viridis_b(option="mako", direction = -1, "Tweets")

tw_reg <- tw_clean %>%
  ggplot(aes(retweet_count, impressions))+
  geom_hex(color="white", size=1, bins=20) +
  theme_classic(base_size = 18)+
  #scale_y_log10(labels=scales::label_comma(),
  #              breaks= scales::breaks_log())+
  #scale_x_log10()+
  labs(x="Retweets", y= "Impressions")+
  theme(panel.border = element_blank(),
        legend.position = c(0.78,0.2))+
  scale_fill_viridis_b(option="mako", direction = -1, "Tweets")+
  guides(fill  =guide_colorsteps(direction = "vertical",
                                 title.position="top",
                                 keywidth=unit(50, 'pt')))

tw_zoom <- tw_clean %>%
  ggplot(aes(retweet_count, impressions))+
  geom_hex(color="white", size=1, bins=20) +
  theme_classic(base_size = 18)+
  scale_y_continuous(labels=scales::label_comma(),
                limits=c(NA, 55000),
                breaks= scales::breaks_pretty())+
  scale_x_continuous(limits=c(NA,100))+
  labs(x="Retweets", y= "Impressions")+
  theme(panel.border = element_blank(),
        legend.position = c(0.78, 0.2))+
  scale_fill_viridis_b(option="mako", direction = -1, "Tweets")+
  guides(fill  =guide_colorsteps(direction = "vertical",
                                 title.position="top",
                                 keywidth=unit(20, 'pt')))

tw_reg +tw_zoom

## both together faceted
tw_clean %>%
  mutate(box = "zoom") %>%
  filter(impressions<=55000 & retweet_count <= 100) %>%
  bind_rows(tw_clean %>% mutate(box = 'big')) %>%
  ggplot(aes(retweet_count, impressions))+
  geom_hex(color="white", size=1, bins=20) +
  theme_classic(base_size = 18)+
  scale_y_continuous(labels=scales::label_comma(),
                     #limits=c(NA, 55000),
                     breaks= scales::breaks_pretty())+
 # scale_x_continuous(limits=c(NA,100))+
  labs(x="Retweets", y= "Impressions")+
  theme(panel.border = element_blank(),
        legend.position = c(0.38, 0.25),
        strip.background = element_blank(),
        strip.text = element_blank())+
  scale_fill_viridis_b(option="mako", direction = -1, "Tweets", values=c(0,0.1, 0.25,0.5, 0.75, .9, 1))+
  guides(fill  =guide_colorsteps(direction = "vertical",
                                 title.position="top",
                                 keywidth=unit(20, 'pt')))+
  facet_wrap(~box, scales = "free")

## average tweet?
tw_clean %>%
  summarize(across(.cols = c(retweet_count, favorite_count, impressions, engagements, engage_rate), ~mean(.x, na.rm=TRUE)))
tw_clean %>%
  summarize(across(.cols = c(retweet_count, favorite_count, impressions, engagements, engage_rate), ~se(.x))) %>%
  ggplot()+
  geom_point(aes(x=1, y=me))

# do retweets or likes best predict engagement/link clicks/impressions?
tw_clean %>%filter(impressions > 50000)%>%view
## find top tweets over last year


tw_year %>%
  arrange(desc(favorite_count)) %>%
  head(., 30) %>%
  write_csv("out/datasci_tweets_top30.csv")


tw_clean %>% str



# tweet stats -------------------------------------------------------------

# engagement rate, retweets, likes, impressions, link clicks
# engagement = retweets+follows+replies+favorites+linkclicks / impressions
# engagement rate over time
# 
# rstats
tw_clean %>% 
  mutate(rstats = ifelse(is.na(rstats), '0', '1')) %>%
  mutate(links = ifelse(is.na(link_type), '0', '1')) %>%
  ggplot(aes(date_time, engage_rate))+
    geom_point(aes(shape=rstats), size = 5, alpha = 0.85, color=col_blue, stroke=1)+
  geom_point(aes(fill=links, shape = links), size = 7, alpha = 0.85, color="orangered", stroke=1)+
  labs(x='',
       y='Engagement rate')+
  theme_classic(base_size=18)+
  scale_x_date(breaks=scales::breaks_width("1 months"),
               labels=scales::label_date_short())+
  geom_hline(yintercept = median(tw_clean$engage_rate, na.rm=TRUE), linetype="dotted", size=1)+
  scale_shape_manual(values=c(16,21))+
  scale_fill_manual(values=c("orangered","transparent"))
  geom_rect(aes(x))
  
  # img
  tw_clean %>% 
    mutate(media_type = ifelse(is.na(media_type), 'no','yes')) %>%
    ggplot(aes(date_time, engage_rate))+
    geom_point(size = 5, alpha = 0.85, color=col_blue, stroke=1)+
    labs(x='',
         y='Engagement rate')+
    theme_classic(base_size=18)+
    scale_x_date(breaks=scales::breaks_width("1 months"),
                 labels=scales::label_date_short())+
    #geom_hline(yintercept = tw_media$med,
    #           linetype=c('solid',"dotted"), size=1)+
    scale_shape_manual(values=c(16,21, 17), "has image")+
    theme(legend.position = "none")
  
  tw_clean %>% 
    mutate(rstats = ifelse(is.na(rstats), 'no','yes')) %>%
    ggplot(aes(impressions, engage_rate, shape=media_type, fill=rstats))+
    geom_point(size = 5, alpha = 0.85, color=col_blue, stroke=1)+
    labs(x='Impressions',
         y='Engagement rate')+
    theme_classic(base_size=18)+
    scale_x_log10()+
   # scale_x_continuous(
      #breaks=scales::breaks_width("1 months"),
     #            labels=scales::label_pretty())+
    #geom_hline(yintercept = tw_media$med,
    #           linetype=c('solid',"dotted"), size=1)+
    scale_shape_manual(values=c(16,21, 17), "has image")+
    theme(legend.position = "none")
  
  tw_clean %>% filter(engage_rate >=7)
  
  se <- function(x) sd(x, na.rm=TRUE)/sqrt(length(x))
  tw_media <- tw_clean %>% 
    mutate(rstats = ifelse(is.na(rstats), '0','1'))%>%
    group_by(rstats) %>%
    summarize(med = mean(engage_rate, na.rm=TRUE),
              se = se(engage_rate))# %>%
    tw_media
    
    
    ## most engaging posts
    tw_clean %>% filter(engage_rate > 6.5) %>% view
    
    link_mean<-tw_clean %>%
      filter(link_type %in% c('viz','pub','job','code','blog','wss','usgs','nwis')) %>%
      mutate(link_type = ifelse(link_type %in% c('wss','usgs','nwis'), 'usgs', link_type)) %>%
      group_by(link_type)%>%
      summarize(mean = mean(link_clicks, na.rm=TRUE),
                median = median(link_clicks, na.rm=TRUE),
                se= se(link_clicks))
    link_mean
    #link clicks by type
tw_clean %>%
  filter(link_type %in% c('viz','pub','job','code','blog','wss','usgs','nwis')) %>%
  mutate(link_type = ifelse(link_type %in% c('wss','usgs','nwis'), 'usgs', link_type)) %>%
  ggplot(aes(link_type, link_clicks))+
  #ggplot(aes(link_type, link_clicks))+
  #geom_tile(width = .5, height=5, fill=col_blue, alpha=0.5)+
  geom_point(data=link_mean, aes(link_type, mean), size=5, alpha=0.6, shape =15, stroke=1.5, color="orangered")+
  geom_point(data=link_mean, aes(link_type, median), size=5, alpha=0.6, shape =15, stroke=1.5, color="gold2")+
  geom_point(color=col_blue, size=2)+
  theme_classic(base_size=18)+
  labs(y="Link clicks", x="")+
  theme(legend.position = "none")+
  coord_flip()+ylim(NA, 300)

tw_clean %>%
  filter(link_type %in% c('viz','pub','job','code','blog')) %>%
  ggplot(aes(link_type, link_clicks))+
  geom_tile(width = 0.4, height=10, fill=col_blue, alpha=0.5)+
  theme_classic(base_size=18)+
  labs(y="Link clicks / Impressions (%)", x="")+
  theme(legend.position = "none")+
  coord_flip()
    
tw_clean %>%
  filter(link_type %in% c('viz','pub','job','code','blog')) %>%
  mutate(link_rate = 100*(link_clicks/impressions))%>%
  group_by(link_type)%>%
  summarize(mean= mean(link_rate, na.rm=TRUE),
            mean_click= mean(link_clicks, na.rm=TRUE))

## the "average" tweet
tw_clean %>%
  
