library(dplyr)

library(fable)

library(future)

library(leaflet)

library(prophet)

library(rtweet)

library(sentimentr)

twitterKey <- "1SaEQ0rWyE2KS3Wm06DrAlcWq"

twitterSecret <- "xzsDZl1gGPEoYyeSeCkWVdxlmYYthAuwyhLSTDYq4j9iaQyhzR"

twitterToken <- "836938535471038464-8llcNiCuFI2NcaHMqz5XSev4umbojRO"

twitterTokenSecret <- "VGXyJbzmPeEjSVcfwvJ6vRM2NqgIq20ldXn8GBx75BMRR"

rtweet::create_token(app = "sp1500CEO", consumer_key = twitterKey, consumer_secret = twitterSecret, 
                     access_token = twitterToken, access_secret = twitterTokenSecret
                      )

usaCoords <- lookup_coords("usa")

usaTweets <- stream_tweets(usaCoords, timeout = 30)

usaTweets <- flatten(usaTweets)

usaTweets <- tidyr::separate(usaTweets, geo_coords, into = c("lat", "lon"), 
                             sep = "\\s", remove = FALSE) %>% 
  mutate(across(matches("lat|lon"), ~as.numeric(.x)))

leaflet(data = usaTweets) %>% addTiles() %>%
  addMarkers(~lon, ~lat, popup = ~text)

foxTweets <- search_tweets("foxnews", n = 100, 
                    include_rts = FALSE)

ts_plot(foxTweets, by = "mins")

foxTimeline <- get_timeline(user = "foxnews", n = 100)

ts_plot(foxTimeline, by = "mins")

users_data(foxTimeline)

trumpTimeline <- get_timeline(user = "realDonaldTrump", n = 400)

trumpTimeline$rowID <- 1:nrow(trumpTimeline)

trumpTimeline$text <- tolower(trumpTimeline$text)

load(url(
  "https://raw.githubusercontent.com/saberry/courses/master/hash_sentiment_vadar.RData"))

trumpSentiment <- sentiment(trumpTimeline$text, 
                            polarity_dt = hash_sentiment_vadar)

sentimentAll <- trumpSentiment %>% 
  group_by(element_id) %>% 
  summarize(sentiment = mean(sentiment)) %>% 
  right_join(., trumpTimeline, by = c("element_id" = "rowID")) %>% 
  select(sentiment, created_at) %>% 
  mutate(ds = created_at, 
         y = sentiment) %>% 
  select(ds, y)


ggplot(sentimentAll, aes(ds, y)) + 
  geom_line() + 
  geom_point(size = .5) + 
  theme_minimal()

sentModel <- prophet(sentimentAll, daily.seasonality = TRUE)

futureForcast <- make_future_dataframe(sentModel, freq = 60, 
                                       periods = 500)

sentForcast <- predict(sentModel, futureForcast)

plot(sentModel, sentForcast) +
  theme_minimal()

future::plan("multicore")

forcastOut <- future::future({
  sentimentAll %>%
    tsibble::as_tsibble() %>% 
    tsibble::fill_gaps() %>% 
    tidyr::fill(y, .direction = "down") %>% 
    model(arima = ARIMA(y)) %>% 
    forecast(h = "3 days")
})

