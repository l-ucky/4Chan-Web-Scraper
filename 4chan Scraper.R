#Load Libraries
library("rvest")
library("tidyverse")
library("ggplot2")
library("wordcloud")
library("tidytext")

#Page 1 Scrape
pol_threads1 <- read_html("https://boards.4channel.org/pol/") %>%
   html_elements("blockquote.postMessage") %>%
   html_text()

#Page 2 Scrape
pol_threads2 <- read_html("https://boards.4channel.org/pol/2") %>%
  html_elements("blockquote.postMessage") %>%
  html_text()

#Page 3 Scrape
pol_threads3 <- read_html("https://boards.4channel.org/pol/3") %>%
  html_elements("blockquote.postMessage") %>%
  html_text()

#Page 4 Scrape
pol_threads4 <- read_html("https://boards.4channel.org/pol/4") %>%
  html_elements("blockquote.postMessage") %>%
  html_text()

#Page 5 Scrape
pol_threads5 <- read_html("https://boards.4channel.org/pol/5") %>%
  html_elements("blockquote.postMessage") %>%
  html_text()

#Page 6 Scrape
pol_threads6 <- read_html("https://boards.4channel.org/pol/6") %>%
  html_elements("blockquote.postMessage") %>%
  html_text()

#Page 7 Scrape
pol_threads7 <- read_html("https://boards.4channel.org/pol/7") %>%
  html_elements("blockquote.postMessage") %>%
  html_text()

#Page 8 Scrape
pol_threads8 <- read_html("https://boards.4channel.org/pol/8") %>%
  html_elements("blockquote.postMessage") %>%
  html_text()

#Page 9 Scrape
pol_threads9 <- read_html("https://boards.4channel.org/pol/9") %>%
  html_elements("blockquote.postMessage") %>%
  html_text()

#Page 10 Scrape
pol_threads10 <- read_html("https://boards.4channel.org/pol/10") %>%
  html_elements("blockquote.postMessage") %>%
  html_text()

# tibble makes a table out of data
df_pol <-      c(pol_threads1, 
                 pol_threads2, 
                 pol_threads3, 
                 pol_threads4, 
                 pol_threads5, 
                 pol_threads6, 
                 pol_threads7, 
                 pol_threads8, 
                 pol_threads9,
                 pol_threads10)

# Making a table with tibble out of the above concatenated data
pol_table <- tibble(txt = df_pol)

tidy_pol <- pol_table %>% 
  unnest_tokens(word, txt, format = "text")

# Removing stop words like "as, just, is, in," etc. 
# Also removing numerical "words" that come up
tidy_pol_fixed <- tidy_pol %>%
  filter(!word %in% stop_words$word 
         & !word == "fucking"
         & !word == "https"
         & !word == "shit"
         & !is.numeric(word))

tidy_pol_fixed2 <- tidy_pol_fixed %>% 
  count(word, sort = TRUE) %>% 
  print(n = 50)

# Time to Visualize with ggplot and wordcloud

tidy_pol_fixed2 %>% 
  top_n(50) %>% 
  ggplot(aes(word, n)) + 
  geom_col() + 
  xlab("Words") +
  ylab("Count") +
  coord_flip()

tidy_pol_fixed2 %>% 
  top_n(50) %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(word, n)) +
  geom_col() + 
  xlab("Words") +
  ylab("Count") +
  coord_flip()

tidy_pol_fixed2 %>% 
  with(wordcloud(word, n, max.words = 100, random.order = FALSE, rot.per = 0.0, 
                             colors = brewer.pal(8, "Dark2")))


# Save the Data you scraped
# Make sure to change the date when saving to not overwrite the old data
# Don't save "tidy_pol_fixed2" to a csv, it only contains a fraction of the "tidy_pol_fixed" data set.
write.csv(tidy_pol_fixed, "~/Documents/Stats/4Chan Scraper/Aug-22-2023.csv", row.names=FALSE)
