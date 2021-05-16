#install.packages("rtweet")
#install.packages("lubridate")
library(rtweet)
library(tidyverse)
library(lubridate)


#?search_tweets

tw_covid19 <- search_tweets(q = "covid19",
                    n = 1000)

tw_covid19 %>% View()


### get_timeline
#?get_timeline
usuario = "RafaelNadal"
tl_nadal <- get_timeline(user = usuario, n = 100)
tl_nadal %>% View()


### get_followers
#?get_followers
usuario <- "rnadalacademy"
gl_nadalAcademy <- get_followers(user = usuario, n = 67500)


### get_friends
#?get_friends
fr_nadalAcademynadalAcademy <- get_friends(user = usuario, n = 210)
fr_nadalAcademy %>% View()


### get_favorites
usuario = "RafaelNadal"
fv_nadal <- get_favorites(user = usuario, n = 1000)
fv_nadal %>% View()


### user_data
user_data_nadal <- users_data(fv_nadal)
user_data_nadal %>% View()


### Analisis datos de covid19
#library(tidyverse)  -> flimpse()

tw_covid19 %>% glimpse()
tw_cv19_es <- tw_covid19 %>%
  filter(lang == "es")

tw_cv19_es %>% View()

tw_cv19_es %>% 
  count(screen_name)

tw_cv19_es %>% 
  group_by(screen_name) %>% 
  summarise(n = n())

tw_cv19_es %>% 
  count(screen_name) %>%
  arrange(desc(n))

tw_covid19 %>% 
  count(screen_name) %>%
  arrange(desc(n))


dat <- fv_nadal %>% 
  count(screen_name) %>%
  arrange(desc(n)) 

  
dat_top5 <- fv_nadal %>% 
  count(screen_name) %>%
  arrange(desc(n)) %>%
  head(5)


p <- ggplot(data = dat_top5, 
            mapping = aes(x = reorder(screen_name,n),
                          y = n))

p <- p + geom_point()
p <- p + geom_bar(stat = "identity",
                  fill = "red3")

p <- p + labs(title = "Top-5 usuarios favoritos de Rafa",
              y = "# de megusta",
              x = "Usuarios de twitter")

p

#dibujar - theme
p + theme_classic()


tw_por_anio_nadal <- fv_nadal %>%
  mutate(fecha = ymd_hms(created_at)) %>%
  mutate(fecha = format(fecha, "%Y")) %>%
  select(created_at, fecha) %>%
  count(fecha)

tw_por_anio_nadal %>% View()
  
p <- ggplot(data = tw_por_anio_nadal,
            mapping = aes(x = fecha,
                          y = n,
                          group=1))
  
p <- p + geom_line()

p <- p + labs(title = "Favoritos por Año",
              x = "Años",
              y = "# de Favoritos",
              subtitle = "Rafael Nadal en Twitter")

print(p)



### Hashtags más comúnes

tw_futbol <- search_tweets("futbol", n = 8000,
                           land = "es",
                           include_rts = FALSE)

tw_futbol %>% View()


#install.packages("tidyr")
#install.packages("tidytext")
library(tidyr)
library(tidytext)
library(tidyverse)


#aux <- tw_futbol %>% 
#  filter(status_id == "1393963032074723328")

#aux$hashtags

#aux %>% 
#  unnest(hashtags) %>%
#  View()
  


dat_fut <- tw_futbol %>% 
  select(hashtags) %>%
  unnest(hashtags) %>%
  mutate(hashtags = toupper(hashtags)) %>%
  filter(!is.na(hashtags)) %>%
  count(hashtags) %>%
  arrange(desc(n)) %>%
  head(20)


p <- ggplot(data = dat_fut,
            mapping = aes(x = reorder(hashtags, n),
                          y = n)) + 
  geom_bar(stat = "identity") +
  coord_flip() + 
  
  labs(title = "Hashtags más utilizadas",
           x = "hashtags",
           y = "# de menciones")
p

  

### Wordcloud
#install.packages("wordcloud")
library(wordcloud)


tw_futbol_hts_wordcloud <- tw_futbol %>% 
  select(hashtags) %>%
  unnest(hashtags) %>%
  mutate(hashtags = toupper(hashtags)) %>%
  filter(!is.na(hashtags)) %>%
  count(hashtags) %>%
  arrange(desc(n)) %>%
  head(50)


wordcloud(words = tw_futbol_hts_wordcloud$hashtags,
          freq = tw_futbol_hts_wordcloud$n,
          colors = brewer.pal(9, "RdBu")
)



### 
# Perfil de los uaurios

user_data_nadal_anio <- user_data_nadal %>%
  distinct(user_id, account_created_at) %>%
  select(account_created_at) %>%
  mutate(anio = account_created_at %>% format("%Y")) %>%
  count(anio)


p <- ggplot(data = user_data_nadal_anio,
            mapping = aes(x = anio,
                          y = n))

p <- p + geom_bar(stat = "identity") 

p <- p + labs(title = "Cuentas creadas por Año",
      x = "Año",
      y = "# de Cuentas")
                
p            



#install.packages("VennDiagram")
library(VennDiagram)


### get_favorites
usuario = "RafaelNadal"
fr_nadal <- get_friends(user = usuario, n = 10000)
fr_nadal %>% View()



### get_favorites
usuario = "rogerfederar"
fr_federer <- get_friends(user = usuario, n = 10000)
fr_federer %>% View()



venn.diagram(
  x = list(Nadal = fr_nadal$user_id,
           Federer = fr_federer$user_id),
  filename = "amigos_fed_nad.png",
  fill = c("red3", "orange"),
  alpha = 0.50,
  cex = 2.5)
  


