#-------#
# setup #
#-------#

if (!require("pacman")) install.packages("pacman")
library(pacman)

p_load(emo, igraph, lubridate, magrittr, qdapRegex, tidytext, tidyverse, wordcloud)

setwd("/path/to/whatsapp/export/")

whatsapp_group = "\"Gruppenname\""

#-------------------------------------#
#   patterns for structure detection  #
#-------------------------------------#
# ! dependend on the smartphone OS and system language

whatsapp_datetime <- "(\\[\\d{0,2}\\.\\d{2}\\.\\d{2}?\\, ([01]?[0-9]|2[0-3]):([0-5][0-9]):?([0-5][0-9])?\\])"
# detects the following pattern: [29.01.18, 14:20:18]
whatsapp_date <- "(\\d{0,2}\\.\\d{2}\\.\\d{2}?)"
# detects "29.01.18"
whatsapp_time <- "([01]?[0-9]|2[0-3]):([0-5][0-9]):?([0-5][0-9])"
# detects "14:20:18"
whatsapp_username <- "([a-zA-Z]{3,16}){1}"
# detects the first name between 3 and 16 letters
whatsapp_notice <- ".*?(hinzugefügt$|geändert$|erstellt$)"
# detects system messages ("XXX XXX hat XXX hinzugefügt" or "XXX XXX hat das Gruppenbild geändert")
whatsapp_files <- ".*?(<[^>]*angehängt>$|<[^>]*weggelassen>$|vcf $)"
# detects files (or, if not exported with files, missing files)

#----android pattern---
# whatsapp_datetime <- "\\d{0,2}\\.\\d{2}\\.\\d{2}?\\ um ([01]?[0-9]|2[0-3]):([0-5][0-9]):?([0-5][0-9])? - "
## detects "10.10.17 um 18:50 - "

#-------------------------------------------#
#   read file, clean structure reread file  #
#-------------------------------------------#

raw <- read_file("_chat.txt") 
clean <- str_replace_all(raw, "(\\n)", " ")
clean <- str_replace_all(clean, whatsapp_datetime, "\n\\1")
# we need to do this because of the structure of the export
# whatsapp exports line breaks within messages as line breaks in the export file (duh…)

whatsapp_chat <- read_lines(clean)
whatsapp_chat <- data.frame(whatsapp_chat) %>%
    slice(4:n())
# we delete the first line because it's empty, the other lines are system messages

#----------------------#
#    clean the data    #
#----------------------#

whatsapp_chat %<>%
    rename(raw = whatsapp_chat) %>%
    mutate(date = str_extract(raw, whatsapp_date)) %>%
    mutate(date = as.Date(date, format = "%d.%m.%y")) %>%
    mutate(time = str_extract(raw, whatsapp_time)) %>%
    mutate(datetime = paste0(date, " ", time)) %>%
    mutate(user = str_extract(raw, whatsapp_username)) %>%
    mutate(text = str_replace(raw, whatsapp_datetime, "")) %>%
    mutate(datetime = as.POSIXct(datetime, format = "%Y-%m-%d %H:%M:%S")) %>%
    na.omit()

# we already got most of it
# let's delete system messages and the user names in the 'text' column

whatsapp_chat %<>%
    mutate(text = str_replace(text, "^[^:]*:\\s*", "")) %>%
    # deletes everything before the first colon
    mutate(text = str_replace(text, whatsapp_files, "")) %>%
    mutate(text = str_replace(text, whatsapp_notice, ""))

# text is perfectly clean now
# just for the sake of it, we are also analysing urls and emojis
# we are going to remove them later

whatsapp_chat %<>%
    mutate(url = rm_url(text, extract=TRUE)) %>%
    mutate(emoji = emo::ji_extract_all(text)) %>%
    mutate(emoji_count = emo::ji_count(text))

# we are going to remove any non-necessary text 
# (e. g. emoticons, urls, mentions)
    
whatsapp_chat %<>%
    mutate(text = str_replace_all(text, "(@[0-9]{10,17})", "")) %>%
    # deletes mentions of cell-phone numbers (WhatsApp mentions)
    mutate(text = rm_url(text)) %>%
    mutate(text = rm_emoticon(text))

#----------------------#
#    filtering users   #
#----------------------#

whatsapp_chat %<>%
    filter(!user %in% c("Günther", "Xaver", "Friedrich", "Gönnhart"))
# those are just example names
# funny ones though
    
#---------------------------------#
#    time for beautiful graphs    #
#---------------------------------#

#------Overall statistics-------#
messages_per_weekday <- whatsapp_chat %>%
    mutate(wday = wday(datetime, label = TRUE)) %>%
    ggplot(aes(x = wday)) +
    geom_bar() +
    labs(title = "Nachrichten pro Wochentag", 
         subtitle = paste0("WhatsApp-Verlauf der Gruppe ", whatsapp_group),
         y = "Nachrichten", x = "") 
ggsave("messages_weekday.png", dpi = 300)

messages_per_hour <- whatsapp_chat %>%
    group_by(hour = hour(datetime), user) %>%
    summarise(messages = n()) %>%
    ggplot(aes(x = hour, y = messages)) +
    geom_col() +
    labs(title = "Nachrichten pro Uhrzeit", 
         subtitle = paste0("WhatsApp-Verlauf der Gruppe ", whatsapp_group),
         y = "Nachrichten", x = "") 
ggsave("messages_hour.png", dpi = 300) 

#---------User statistics---------#
messages_per_user <- whatsapp_chat %>%
    group_by(user) %>%
    summarise(messages = n()) %>%
    arrange(desc(messages)) %>%
    ggplot(aes(x = reorder(user, -messages), y = messages, fill = user)) +
    geom_col(show.legend = FALSE) +
    labs(title = "Anzahl der Nachrichten", 
         subtitle = paste0("WhatsApp-Verlauf der Gruppe ", whatsapp_group),
         y = "Nachrichten", x = "") 
ggsave("messages_user.png", dpi = 300) 

emojis_per_user <- whatsapp_chat %>%
    group_by(user) %>%
    summarise(emojis = sum(emoji_count)) %>%
    arrange(desc(emojis)) %>%
    ggplot(aes(x = reorder(user, -emojis), y = emojis, fill = user)) +
    geom_col(show.legend = FALSE) +
    labs(title = "Summe der Emojis in Nachrichten", 
         subtitle = paste0("WhatsApp-Verlauf der Gruppe ", whatsapp_group),
         y = "Emojis", x = "")
ggsave("emojis_user.png", dpi = 300) 

emojis_per_message <- whatsapp_chat %>%
    group_by(user) %>%
    summarise(messages = n(), emojis = sum(emoji_count), avg_emojis = emojis / messages) %>%
    ggplot(aes(x = reorder(user, -avg_emojis), y = avg_emojis, fill = user)) +
    geom_col(show.legend = FALSE) +
    labs(title = "Emojis pro Nachricht",
         subtitle = paste0("WhatsApp-Verlauf der Gruppe ", whatsapp_group),
         y = "Emojis pro Nachricht", x = "")
ggsave("emojis_message.png", dpi = 300) 

messages_length <- whatsapp_chat %>%
    select(user, text) %>%
    mutate(length = nchar(text)) %>%
    filter(length > 1) %>%
    ggplot(aes(x = reorder(user, -length, median), y = length, fill = user)) +
    geom_boxplot(show.legend = FALSE) + 
    labs(title = "Länge der Nachrichten", 
         subtitle = paste0("WhatsApp-Verlauf der Gruppe ", whatsapp_group),
         y = "Zeichen pro Nachricht", x = "")
ggsave("message_length.png", dpi = 300) 

#------------------------#
#    content analysis    #
#------------------------#

#---------word cloud---------#
png("wordcloud.png", width=5, height=5, units="in", res=300)
whatsapp_words <- whatsapp_chat %>%
    unnest_tokens(word, text) %>%
    anti_join(get_stopwords("de")) %>%
    count(word, sort = TRUE) %>%
    with(wordcloud(word, n, max.words = 100, random.order = FALSE, colors=brewer.pal(8, "Dark2")))
dev.off()

#-----------tf idf----------#
# most typical used word by user (NOT most frequent used word)
# compares the words of each user against the whole data set, selects word typically used by each user
# see: https://www.tidytextmining.com/tfidf.html
# could also be described as most-exclusive words per user

whatsapp_words_distinct <- whatsapp_chat %>%
    select(user, text) %>%
    unnest_tokens(word, text) %>%
    count(user, word, sort = TRUE) %>%
    bind_tf_idf(word, user, n) %>%
    arrange(desc(tf_idf)) %>%
    mutate(word = factor(word, levels = rev(unique(word)))) %>% 
    group_by(user) %>% 
    top_n(4) %>% 
    ungroup %>% 
    ggplot(aes(word, tf_idf, fill = user)) +
    geom_col(show.legend = FALSE) +
    labs(x = NULL, y = "tf-idf") +
    facet_wrap(~user, ncol = 2, scales = "free") +
    coord_flip() +
    labs(title = "\"Typische\" Wörter",
         subtitle = paste0("WhatsApp-Verlauf der Gruppe ", whatsapp_group))
ggsave("typical_words.png", dpi = 300) 

#---------------------#
#    user activity    #
#---------------------#

# lets get fancy with ggridges
# but some data manipulation first, we want to get date intervals

first_day <- whatsapp_chat[[1, "datetime"]]

activity_per_day <- whatsapp_chat %>%
  mutate(day = as.integer(difftime(datetime, first_day, units = "days")))  %>%
  group_by(user, day) %>%
  summarise(messages = n()) %>%
  ggplot(aes(x = day, y = messages, fill = user)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~user, ncol = 1, strip.position = "left") +
  labs(title = "Nutzeraktivität (Nachrichten pro Tag)", 
       subtitle = paste0("WhatsApp-Verlauf der Gruppe ", whatsapp_group),
       x = "Alter der Gruppe in Tagen", y = "") + 
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), 
        panel.grid=element_blank())
ggsave("user_activity_day.png", dpi = 300)

activity_per_hour <-  whatsapp_chat %>%
  count(hour = hour(datetime), user) %>%
  group_by(user) %>%
  mutate(freq = n / sum(n)) %>%
  ggplot(aes(x = hour, y = freq, fill = user)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~user, ncol = 1, strip.position = "left") +
  labs(title = "Nutzeraktivität pro Uhrzeit", 
       subtitle = paste0("WhatsApp-Verlauf der Gruppe ", whatsapp_group),
       x = "Uhrzeit", y = "") +
  theme(panel.grid=element_blank()) + 
  scale_y_continuous(labels = scales::percent)
ggsave("user_activity_hour.png", dpi = 300) 


#------------------------#
#    network analysis    #
#------------------------#

users <- whatsapp_chat %>%
  select(user, text) %>%
  group_by(user) %>%
  summarise(messages = n())

png("graph.png", width=5, height=5, units="in", res=300)
user_network <- whatsapp_chat %>%
  select(user, text) %>%
  rename(from = user) %>%
  mutate(to = str_extract(text, paste(users$user, collapse = "|"))) %>%
  select(from, to) %>%
  na.omit() %>%
  group_by(from, to) %>%
  summarise(weight = n()) %>%
  left_join(users, by = c("from" = "user")) %>%
  graph.data.frame()

deg <- degree(user_network, mode="all")

set.seed(666)
plot(user_network,
     layout = layout.fruchterman.reingold(user_network, weights = E(user_network)$weight),
     edge.width = E(user_network)$weight,
     vertex.label.family = "Helvetica",
     vertex.label.color = "grey20", 
     vertex.color = "lightblue", 
     vertex.size = sqrt(deg)*10)
dev.off()