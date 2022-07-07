
library (tidyverse)
library(readxl)

# carga de datos ----------------------------------------------------------


Netflix_Merge_final <- read_excel("DataSet/Netflix Title Merge final.xls")
View(Netflix_Title_Merge_final)


names(Netflix_Merge_final)
glimpse(Netflix_Merge_final)
summary(Netflix_Merge_final)



## convertir a formato fecha - campo "date_added" ----

Netflix_Merge <- Netflix_Merge_final %>% 
  mutate(date_added = as.Date(Netflix_Merge_final$date_added, format = "%B%d,%Y"))


glimpse(Netflix_Merge)
summary(Netflix_Merge)


names(Netflix_Merge)

# peliculas Series por continente -----------------------------------------
# Norteamerica ------------------------------------------------------------


norteamerica  <- Netflix_Merge %>% 
  select(norteamerica) %>% 
  count(variable =(norteamerica == 1) ) %>% 
  mutate(continente = "norteamerica")



# Sudamerica --------------------------------------------------------------

sudamerica <- Netflix_Merge %>% 
  select(sudamerica) %>% 
  count(variable =(sudamerica == 1) ) %>% 
  mutate(continente = "sudamerica")




# Oceania -----------------------------------------------------------------
oceania  <- Netflix_Merge %>% 
  select(oceania) %>% 
  count(variable =(oceania == 1) ) %>% 
  mutate(continente = "oceania")



# Asia --------------------------------------------------------------------

asia  <- Netflix_Merge %>% 
  select(asia) %>% 
  count(variable =(asia == 1) ) %>% 
  mutate(continente = "asia")





# Europa ------------------------------------------------------------------

europa  <- Netflix_Merge %>% 
  select(europa) %>% 
  count(variable =(europa == 1) ) %>% 
  mutate(continente = "europa")

# Africa ------------------------------------------------------------------
africa  <- Netflix_Merge %>% 
  select(africa) %>% 
  count(variable = (africa == 1) ) %>% 
  mutate(continente = "africa")




# merge total -------------------------------------------------------------------

total <- merge(norteamerica, merge(sudamerica,oceania, all = TRUE),all = TRUE)

total <- merge(total, merge(asia,europa, all = TRUE),all = TRUE)

total <- merge(total, africa, all = TRUE)



# grafico continentes -----------------------------------------------------


t1 <- 
  total %>% 
  filter(variable == TRUE) %>% 
  mutate( continente = fct_reorder(continente, n,.desc = TRUE)) %>% 
  ggplot(aes(x = continente, y = n, fill = continente)) +
  geom_bar(aes(y = n), 
           stat = "identity", 
           position = "dodge",
           width = 0.75) + 
  scale_x_discrete() +
  geom_text(
    aes(label = n, y = n + 0.05),
    position = position_dodge(0.9),
    vjust = 1
  )+
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Peliculas y Series producidas según continente",
       #subtitle = "",
       x='Continente', 
       y='Cantidad de Peliculas/Series')

# tipo peliculas o series segun continente --------------------------------
#norteamerica tipo ------------------------------------------------------------
  
  
  norteamericaMovies <- 
  Netflix_Merge %>% 
  filter(type == "movie") %>% 
  select(norteamerica,type) %>% 
  count(variable =(norteamerica == 1) ) %>% 
  mutate(continente = "norteamerica") %>% 
  mutate(type = "movie")


norteamericaTvShow <- 
  Netflix_Merge %>% 
  filter(type == "tv show") %>% 
  select(norteamerica,type) %>% 
  count(variable =(norteamerica == 1) ) %>% 
  mutate(continente = "norteamerica") %>% 
  mutate(type = "tv Show")


# sudamerica tipo --------------------------------------------------------------

sudamericaMovies <- 
  Netflix_Merge %>% 
  filter(type == "movie") %>% 
  select(sudamerica,type) %>% 
  count(variable =(sudamerica == 1) ) %>% 
  mutate(continente = "sudamerica") %>% 
  mutate(type = "movie")


sudamericaTvShow <- 
  Netflix_Merge %>% 
  filter(type == "tv show") %>% 
  select(sudamerica,type) %>% 
  count(variable =(sudamerica == 1) ) %>% 
  mutate(continente = "sudamerica") %>% 
  mutate(type = "tv Show")
# oceania tipo -----------------------------------------------------------------
oceaniaMovies <- 
  Netflix_Merge %>% 
  filter(type == "movie") %>% 
  select(oceania,type) %>% 
  count(variable =(oceania == 1) ) %>% 
  mutate(continente = "oceania") %>% 
  mutate(type = "movie")


oceaniaTvShow <- 
  Netflix_Merge %>% 
  filter(type == "tv show") %>% 
  select(oceania,type) %>% 
  count(variable =(oceania == 1) ) %>% 
  mutate(continente = "oceania") %>% 
  mutate(type = "tv Show")

# asia tipo --------------------------------------------------------------------

asiaMovies <- 
  Netflix_Merge %>% 
  filter(type == "movie") %>% 
  select(asia,type) %>% 
  count(variable =(asia == 1) ) %>% 
  mutate(continente = "asia") %>% 
  mutate(type = "movie")


asiaTvShow <- 
  Netflix_Merge %>% 
  filter(type == "tv show") %>% 
  select(asia,type) %>% 
  count(variable =(asia == 1) ) %>% 
  mutate(continente = "asia") %>% 
  mutate(type = "tv Show")


# europa tipo------------------------------------------------------------------

europaMovies <- 
  Netflix_Merge %>% 
  filter(type == "movie") %>% 
  select(europa,type) %>% 
  count(variable =(europa == 1) ) %>% 
  mutate(continente = "europa") %>% 
  mutate(type = "movie")


europaTvShow <- 
  Netflix_Merge %>% 
  filter(type == "tv show") %>% 
  select(europa,type) %>% 
  count(variable =(europa == 1) ) %>% 
  mutate(continente = "europa") %>% 
  mutate(type = "tv Show")

# africa tipo ------------------------------------------------------------------

africaMovies <- 
  Netflix_Merge %>% 
  filter(type == "movie") %>% 
  select(africa,type) %>% 
  count(variable =(africa == 1) ) %>% 
  mutate(continente = "africa") %>% 
  mutate(type = "movie")


africaTvShow <- 
  Netflix_Merge %>% 
  filter(type == "tv show") %>% 
  select(africa,type) %>% 
  count(variable =(africa == 1) ) %>% 
  mutate(continente = "africa") %>% 
  mutate(type = "tv Show")


# merge Movie -------------------------------------------------------------------



totalMovies <- merge(norteamericaMovies, merge(sudamericaMovies,oceaniaMovies, all = TRUE),all = TRUE)



totalMovies <- merge(totalMovies, merge(asiaMovies,europaMovies, all = TRUE),all = TRUE)

totalMovies <- merge(totalMovies, africaMovies, all = TRUE)

##ordeno la variable n
totalMovies <- 
  totalMovies %>%  
  mutate( continente = fct_reorder(continente, n,.desc = TRUE))


# merge tv show -----------------------------------------------------------

totalTvShow <- merge(norteamericaTvShow, merge(sudamericaTvShow,oceaniaTvShow, all = TRUE),all = TRUE)



totalTvShow <- merge(totalTvShow, merge(asiaTvShow,europaTvShow, all = TRUE),all = TRUE)

totalTvShow <- merge(totalTvShow, africaTvShow, all = TRUE)



# grafico movie -----------------------------------------------------------------


movies1 <- 
  totalMovies %>% 
  filter(variable == TRUE) %>% 
  mutate( continente = fct_reorder(continente, n,.desc = TRUE)) %>% 
  ggplot(aes(x = continente, y = n, fill = continente)) +
  geom_bar(aes(y = n), 
           stat = "identity", 
           position = "dodge",
           width = 0.75) + 
  scale_x_discrete() +
  geom_text(
    aes(label = n, y = n + 0.05),
    position = position_dodge(0.9),
    vjust = 1
  )+
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Peliculas producidas según continente",
       subtitle = " Peliculas producidas de Netflix según continente, entre los años ",
       x='Continente', 
       y='n')



# grafico tvshow ----------------------------------------------------------

tvShow1 <- 
  totalTvShow %>% 
  filter(variable == TRUE) %>% 
  mutate( continente = fct_reorder(continente, n,.desc = TRUE)) %>% 
  ggplot(aes(x = continente, y = n, fill = continente)) +
  geom_bar(aes(y = n), 
           stat = "identity", 
           position = "dodge",
           width = 0.75) + 
  scale_x_discrete() +
  geom_text(
    aes(label = n, y = n + 0.05),
    position = position_dodge(0.9),
    vjust = 1
  )+
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Series producidas según continente",
       subtitle = " Series producidas de Netflix según continente, entre los años ",
       x='Continente', 
       y='n')

# unificacion de graficos -------------------------------------------------


library(ggpubr) 
a1 <- ggarrange(t1, movies1, tvShow1)


# peliculasSeries_Genero -------------------------------------------------
# action ------------------------------------------------------------------

action <-  
  Netflix_Merge %>% 
  select(action) %>% 
  count(variable =(action == 1) ) %>% 
  mutate(genero = "action")



# anime -------------------------------------------------------------------
anime <-  
  Netflix_Merge %>% 
  select(anime) %>% 
  count(variable =(anime == 1) ) %>% 
  mutate(genero = "anime")

# children ----------------------------------------------------------------

children <-  
  Netflix_Merge %>% 
  select(children) %>% 
  count(variable =(children == 1) ) %>% 
  mutate(genero = "children")


# classic -----------------------------------------------------------------
classic <-  
  Netflix_Merge %>% 
  select(classic) %>% 
  count(variable =(classic == 1) ) %>% 
  mutate(genero = "classic")

# comedy ------------------------------------------------------------------

comedy <-  
  Netflix_Merge %>% 
  select(comedy) %>% 
  count(variable =(comedy == 1) ) %>% 
  mutate(genero = "comedy")
# crime -------------------------------------------------------------------
crime <-  
  Netflix_Merge %>% 
  select(crime) %>% 
  count(variable =(crime == 1) ) %>% 
  mutate(genero = "crime")

# drama -------------------------------------------------------------------

drama <-  
  Netflix_Merge %>% 
  select(drama) %>% 
  count(variable =(drama == 1) ) %>% 
  mutate(genero = "drama")
# fiction -----------------------------------------------------------------

fiction <-  
  Netflix_Merge %>% 
  select(fiction) %>% 
  count(variable =(fiction == 1) ) %>% 
  mutate(genero = "fiction")
# horror ------------------------------------------------------------------
horror <-  
  Netflix_Merge %>% 
  select(horror) %>% 
  count(variable =(horror == 1) ) %>% 
  mutate(genero = "horror")

# mystery -----------------------------------------------------------------

mystery <-  
  Netflix_Merge %>% 
  select(mystery) %>% 
  count(variable =(mystery == 1) ) %>% 
  mutate(genero = "mystery")
# reality -----------------------------------------------------------------
reality <-  
  Netflix_Merge %>% 
  select(reality) %>% 
  count(variable =(reality == 1) ) %>% 
  mutate(genero = "reality")

# romantic ----------------------------------------------------------------
romantic <-  
  Netflix_Merge %>% 
  select(romantic) %>% 
  count(variable =(romantic == 1) ) %>% 
  mutate(genero = "romantic")

# sport -------------------------------------------------------------------
sport <-  
  Netflix_Merge %>% 
  select(sport) %>% 
  count(variable =(sport == 1) ) %>% 
  mutate(genero = "sport")


# thriller ----------------------------------------------------------------

thriller <-  
  Netflix_Merge %>% 
  select(thriller) %>% 
  count(variable =(thriller == 1) ) %>% 
  mutate(genero = "thriller")

# tv show -----------------------------------------------------------------

`tv show` <-  
  Netflix_Merge %>% 
  select(`tv show`) %>% 
  count(variable =(`tv show` == 1) ) %>% 
  mutate(genero = "tv show")


# educational ---------------------------------------------------------------
educational <-  
  Netflix_Merge %>% 
  select(educational) %>% 
  count(variable =(educational == 1) ) %>% 
  mutate(genero = "educational")


# merge Genero ------------------------------------------------------------


totalGenero <- merge(action, merge(anime,children, all = TRUE),all = TRUE)

totalGenero <- merge(totalGenero, merge(classic,comedy, all = TRUE),all = TRUE)
totalGenero <- merge(totalGenero, merge(crime,drama, all = TRUE),all = TRUE)
totalGenero <- merge(totalGenero, merge(fiction,horror, all = TRUE),all = TRUE)
totalGenero <- merge(totalGenero, merge(mystery,reality, all = TRUE),all = TRUE)
totalGenero <- merge(totalGenero, merge(romantic,sport, all = TRUE),all = TRUE)
totalGenero <- merge(totalGenero, merge(thriller,`tv show`, all = TRUE),all = TRUE)
totalGenero <- merge(totalGenero, educational, all = TRUE)



# grafico Genero-----------------------------------------------------------------

genero <- 
  totalGenero %>% 
  filter(variable == TRUE) %>% 
  mutate( genero = fct_reorder(genero, n,.desc = TRUE)) %>% 
  ggplot(aes(x = genero, y = n, fill = genero)) +
  geom_bar(aes(y = n), 
           stat = "identity", 
           position = "dodge",
           width = 0.75) + 
  scale_x_discrete() +
  geom_text(
    aes(label = n, y = n + 0.05),
    position = position_dodge(0.9),
    vjust = 1
  )+
  scale_fill_manual(values=c("#1BA3C6", "#2CB5C0","#30BCAD", "#33A65C",
                             "#33A65C","#A2B627","#D5BB21","#F8B620",
                             "#F89217", "#F06719","#E03426","#FC719E",
                             "#EB73B3","#CE69BE","#7873C0", "#4F7CBA"))+
  labs(title = "Peliculas y Series producidas según genero",
       x='Genero', 
       y='Cantidad de Peliculas y Series')+
  geom_label(aes(label = n, fill = genero),
             colour = "white", 
             fontface = "bold", 
             show.legend = FALSE)

# top20Directores ---------------------------------------------------------

#hace la tabla y elimina los na
director <- 
  Netflix_Merge %>% 
  select(director) %>% 
  filter(!is.na(director)) 

##ccuenta los nombres
director <- 
  director %>% 
  count(director )

#ordena por n

director <- 
  director %>% 
  arrange(-n)

##TOP 20 de directores de peliculas  grafico de barras

d1 <- 
  director %>% 
  head(director, n = 20) %>% 
  mutate( director = fct_reorder(director, n,.desc = FALSE)) %>% 
  ggplot( aes(x = director, y =n, fill = director, label = n)) +
  geom_bar( stat="identity", show.legend = FALSE) +
  coord_flip()+
  labs(title = "Top 20 - directores de peliculas y series de Netflix",
       x = "Directores", 
       y = "Cantidad de Directores") +
  geom_label(aes(fill = director),
             colour = "white", 
             fontface = "bold", 
             show.legend = FALSE)


# PeliculasOriginalesNetflix ----------------------------------------------
#solo peliculas 6126
Orig_pelicula <- 
  Netflix_Merge %>% 
  select(or_netflix) %>% 
  filter(!is.na(or_netflix)) %>% 
  count(or_netflix) 

Orig_pelicula <- 
  Orig_pelicula %>% 
  mutate(porcent = (n/sum(n)) *100)


# grafico -----------------------------------------------------------------

op1 <- 
  Orig_pelicula %>% 
  ggplot(aes(x = or_netflix, y = porcent, fill = or_netflix)) +
  geom_bar(stat="identity", show.legend = FALSE) +
  scale_y_continuous(labels = scales::percent_format(scale = 1))+
  labs(title = "Peliculas Originales de Netflix",
       x = "Originales", 
       y = "Porcentajes") +
  geom_label(aes( label = paste0(round(porcent),"%"), fill = or_netflix),
             colour = "white", 
             fontface = "bold", 
             show.legend = FALSE)

