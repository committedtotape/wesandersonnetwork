library(tidyverse) #for most things
library(rvest) #for web scraping
library(wesanderson) #for colours
library(tidygraph) #for converting to network data
library(ggraph) #for visualising network data
library(igraph) 
library(extrafont) #for fonts

font_import() # import all fonts on machine to get the futura (Wes' fave) font - think this is only needed once?
loadfonts() # load the imported fonts
fonttable() # display table of fonts to see which fonts to use

# Wes Anderson IMDB page
wesurl <- "https://www.imdb.com/name/nm0027572/"

# Read html of page
readwes <- read_html(wesurl)

# extract names of films directed
films_date <- readwes %>% 
  html_nodes("#filmo-head-director+ .filmo-category-section .filmo-row") %>% 
  html_text() %>% 
  # remove unwanted strings
  str_remove_all("\n") %>% 
  str_trim()

# film names
films <- str_sub(films_date, 5, -1)

# film years
film_year <- as.integer(str_sub(films_date, 1, 4))

# extract urls of films directed
film_urls <- readwes %>% 
  html_nodes("#filmo-head-director+ .filmo-category-section a") %>% 
  html_attr('href') %>% 
  # remove unnecessary references after final forward slash
  str_sub(1, 17)
film_urls

# combine film names and urls into dataframe
film_df <- tibble(title = films, film_url = film_urls, film_year = film_year) %>% 
  # remove the films that are Shorts - only considering feature length films
  filter(!str_detect(toupper(title), "SHORT")) %>% 
  # remove anything in brackets after film title
  # get full url address and append trail for the full cast location
  mutate(title = str_remove(title, "\\(.*\\)"),
         film_url = str_c("https://www.imdb.com", film_url),
         film_cast_url = str_c(film_url, "fullcredits?ref_=tt_cl_sm#cast"))

film_df

# get vector of urls for the cast list of each film - to iterate over
cast_urls <- film_df$film_cast_url

# create function to scrape cast lists
wes_scrape <- function(url) {
  
  Sys.sleep(3)
  
# read html of cast list
readcast <- read_html(url)

# get film title
film_title <- readcast %>% 
  html_nodes(".parent a") %>% 
  html_text()
film_title

# get full list of actors
actors <- readcast %>% 
  html_nodes(".primary_photo+ td") %>% 
  html_text() %>% 
  str_trim()
actors

# get full list of characters
role <- readcast %>% 
  html_nodes(".character") %>% 
  html_text() %>% 
  str_trim()
role

# create dataframe of film with all actors and the character they play
cast_df <- tibble(title = film_title, actor = actors, role = role) %>% 
  # remove roles that were uncredited - don't want the list of actors to get out of control!
  filter(!str_detect(role, "uncredited"))

}

# iterate over the scraping function with vector of urls
all_wes <- map_df(cast_urls, wes_scrape)

# ensure no actor is listed twice for same film
wes <- all_wes %>% 
  distinct(title, actor) %>% 
  left_join(film_df, by = "title") %>% 
  select(title, actor, film_year)

# save data so i don't have to run scrape again
write_rds(wes, "wes_anderson_films.rds")

# most used actors - actors appearing 3 or more times - to be used to highlight plot
most_used_actors <- wes %>% 
  add_count(actor) %>% 
  filter(n >= 3) %>% 
  distinct(actor) %>% 
  pull()

# film cast sizes - not used in the end
cast_size <- wes %>% 
  count(title, film_year) %>% 
  arrange(film_year)

# for each record attach the actor appearances and film cast sizes
wes_film_actor <- wes %>% 
  select(title, actor) %>% 
  add_count(actor) %>% 
  add_count(title) %>% 
  rename(actor_weight = n,
         film_weight = nn)

# wes anderson palettes
wes_palettes <- names(wesanderson::wes_palettes)

# function to extract all colours for palettes along with palette name
wes_pal_func <- function(pal) {
  col_df <- tibble(colours = wes_palette(pal), palette = pal)
}

# create dataframe of all colours and palette names
wes_colours <- map_df(wes_palettes, wes_pal_func)

# select a colour from each required palette to be used for film nodes - 
# needs to be reversed to be used later on  i.e. latest film to first film
# i've chosen colours manually to select a nice complement of colours
pal_to_use <- rev(wes_colours[c(1, 16, 23, 32, 37, 51, 65, 75, 82), ]$colours)
pal_to_use

# select a colour to be used for every actor node
actor_colour <- wes_colours[47, ]$colours

# get actor weighting (number of appearances) for actor nodes in plot
act_weight <- wes_film_actor %>% 
  distinct(actor, actor_weight) %>% 
  rename(name = actor, weight = actor_weight) %>% 
  mutate(colour = actor_colour)

# get film weighting (number of cast members) for film nodes - not used in the end
# attach the relevent colour to each film - used in plot
film_weight <- wes_film_actor %>% 
  distinct(title, film_weight) %>% 
  mutate(film_weight = film_weight/10) %>% 
  rename(name = title, weight = film_weight) %>% 
  cbind(colour = pal_to_use)

# weighting and colours for actors and films
weights <- rbind(act_weight, film_weight)

# convert dataframe to table graph object using tidygraph package
# this is made of 2 data frames: a node df and an edge df
wes_network <- wes %>% 
  select(title, actor) %>% 
  as_tbl_graph() %>% 
  # add type to indicate if node represents a film or an actor
  mutate(type = if_else(name %in% wes_film_actor$title, "Film", "Actor")) %>% 
  # add the weightings to each film and actor
  inner_join(weights, by = "name") %>% 
  # now focus on the edges data
  activate(edges) %>% 
  # add the colour attributed to the film nodes (from). N() accesses node data
  mutate(edge_col = .N()$colour[from])

wes_network

# print the seed so I could find a plot i liked and then stick with that seed
#eff_seed <- sample(1:2^15, 1)
#print(sprintf("Seed for session: %s", eff_seed))

#3506
set.seed(3506)

# visualise network with ggraph
ggraph(wes_network, layout = "fr") + 
  # colour edges based on film node
  geom_edge_link(aes(color = edge_col), 
                 width = 0.8, alpha = .4) + 
  # colour film nodes based on the colours chosen from palettes. set 1 size for all nodes
  geom_node_point(aes(filter = type == "Film", color = colour), 
                  size = 10, show.legend = FALSE) + 
  # plot all actor nodes with a low alpha, size node based on no. of appearances
  geom_node_point(aes(filter = type == "Actor", size = weight), 
                  colour = actor_colour, alpha = 0.5, show.legend = TRUE) + 
  # plot only those actors appearing 3+ times with higher alpha
  geom_node_point(aes(filter = name %in% most_used_actors, size = weight, color = colour), 
                  alpha = 0.8, show.legend = FALSE) + 
  # label the film nodes
  geom_node_label(aes(filter = type == "Film", label = name, color = colour), 
                 repel = FALSE, hjust = 0.5, vjust = 1.2, size = 3, alpha = 0.8,
                 show.legend = FALSE, fontface = "bold",
                 family = "FuturaBT-BoldItalic") +
  # label the actors appearing 3+ times
  geom_node_text(aes(filter = name %in% most_used_actors, label = name), 
                 colour = wes_palette("BottleRocket1")[7], repel = TRUE, size = 3, 
                 show.legend = FALSE, fontface = "bold",
                 family = "FuturaBT-BoldItalic") +
  # sets the node and edge colours based on the colours held in data
  scale_color_identity() +
  scale_edge_color_identity() +
  # adjust actor node sizes for legend
  scale_size_continuous(breaks = 1:8, name = "Number of Appearances", range = c(1, 8)) +
  # set theme of graph - use the futura font
  theme_graph(background = wes_palette("Chevalier1")[3], foreground = NA, base_family = "FuturaBT-BoldCondensed") +
  # set all other themes and labels like any old ggplot
  theme(legend.position = c(0.9, 0.25),
        legend.text = element_text(colour = actor_colour, face = "bold", size = 12),
        legend.title = element_text(colour = actor_colour, face = "bold", size = 12),
        legend.title.align = 1,
        legend.background = element_rect(colour = actor_colour, fill =  wes_palette("Chevalier1")[3]),
        plot.title = element_text(colour = wes_palette("GrandBudapest1")[2], size = 22, hjust = 0.5, family = "FuturaBT-ExtraBlack"),
        plot.subtitle = element_text(colour = actor_colour, size = 16, hjust = 0.5),
        plot.caption = element_text(colour =  wes_palette("GrandBudapest1")[2], size = 12, hjust = 0.5),
        plot.margin = margin(0.8, 0.1, 0.5, 0.1, "cm")) +
  labs(title = toupper("The Films of Wes Anderson | A Network Analysis"),
       subtitle = "Network showing all credited actors appearing in Wes Anderson's 9 feature-length films\nNames of actors appearing 3 or more times are shown",
       caption = "@committedtotape | Source: IMDb.com")

# save the graph
ggsave("wes anderson actor network.png", width = 14, height = 8)
