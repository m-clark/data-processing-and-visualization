
# Process google apps data ------------------------------------------------

google_apps =  noiris::google_apps

google_apps = google_apps %>% 
  mutate(type = if_else(! type %in% c('Free', 'Paid'), NA_character_, type))

# can't unnest due to NA
google_apps_sent = google_apps$greviews
names(google_apps_sent) = google_apps$app 
google_apps_sent = google_apps_sent[!is.na(google_apps_sent)]
google_apps_sent = bind_rows(google_apps_sent, .id = 'app')

google_apps_sent = google_apps_sent %>% 
  group_by(app) %>% 
  summarise(avg_sentiment_polarity = mean(sentiment_polarity, na.rm = TRUE),
            avg_sentiment_subjectivity = mean(sentiment_subjectivity, na.rmt = TRUE))

google_apps = google_apps %>% 
  left_join(google_apps_sent)

save(google_apps, file = 'data/google_apps.RData')
readr::write_csv(google_apps %>% select(-greviews), 'data/google_apps.csv')

# ga_mod = lm(avg_sentiment_polarity ~ log(reviews) + size_in_MB + type, data = google_apps)
# summary(ga_mod)
# 
# plot(emmeans(ga_mod, ~ type))
# 
# ga_mod = lm(avg_sentiment_polarity ~ log(reviews) + size_in_MB + type + log(reviews):type + size_in_MB:type, data = google_apps)
# summary(ga_mod)
# 
# plot(ggeffects::ggpredict(ga_mod,terms = c('reviews [exp]', 'type')))