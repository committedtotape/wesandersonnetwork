library(shiny)
library(tidyverse) #for most things
library(wesanderson) #for colours
library(tidygraph) #for converting to network data
library(visNetwork) #for interactive network viz
library(extrafont) #for fonts

# coordinates for nodes in network
# These are slightly tweaked coordinates as extracted from shiny app
# using visGetPositions function.
# Manually copied from shiny app into script using datapasta package

coords <- data.frame(
  x = c(-1062L, -429L, -3L, -746L, -225L, 149L, 1042L, 746L, 621L,
        -645L, -187L, -1389L, -1837L, -1583L, -1703L, -694L, -697L,
        -1893L, -1631L, -665L, -1846L, -623L, -1971L, -1191L, -1965L,
        -1475L, -1170L, -1737L, -1884L, -1489L, -1159L, -1943L, -1085L,
        -1559L, -505L, -1667L, -1289L, -1282L, -1936L, -1797L, -1772L,
        -1386L, -1026L, 339L, 171L, -1328L, -564L, -401L, -1189L, -946L,
        -789L, -934L, -1056L, -1057L, -947L, -1383L, -552L, -1304L, -1325L,
        -249L, -818L, -964L, -729L, -259L, 87L, -1162L, -545L, -887L,
        -382L, -1423L, -418L, -814L, -1434L, -321L, -726L, -316L, -397L,
        -965L, -913L, -1232L, -712L, -1128L, -640L, -51L, -1268L, -1112L,
        -507L, -167L, 127L, -183L, 449L, 422L, -451L, 500L, 30L, -40L,
        324L, 211L, -324L, 642L, -611L, 353L, 609L, -106L, 170L, 273L,
        200L, 283L, -13L, -654L, -188L, 355L, -372L, 122L, -152L, 109L,
        -216L, -77L, -319L, 78L, 522L, 409L, -111L, 368L, 533L, 9L,
        -409L, -561L, -491L, 181L, 651L, -326L, -312L, -214L, 599L, -173L,
        261L, 25L, -1119L, 15L, 174L, -503L, -906L, -1467L, -1311L,
        -1627L, -22L, -1356L, -1447L, -1279L, -1222L, -521L, -1420L, -1505L,
        -1075L, -1648L, -1104L, -1067L, -1388L, -1145L, -1154L, -1559L,
        -1456L, -698L, -1327L, -802L, -1414L, -994L, -930L, -935L, -615L,
        -1397L, -733L, -1020L, -1302L, -1168L, -879L, -1250L, -1261L,
        -1079L, -1691L, -1676L, -984L, -1520L, -1401L, -1411L, -1657L,
        -1675L, -1338L, -909L, -1559L, -1252L, -846L, -1133L, -710L,
        -805L, -1585L, -1596L, -1525L, -1190L, -1674L, -1608L, -1240L,
        -1541L, -1520L, -1573L, -975L, -701L, 403L, 23L, 115L, -479L, 52L,
        -379L, -623L, -306L, -690L, -768L, -919L, -541L, -175L, -866L,
        -641L, 81L, -778L, 8L, -916L, -382L, 166L, -996L, -670L, -758L,
        -463L, -171L, -173L, 11L, -597L, -408L, -238L, -734L, -1070L, -80L,
        -168L, -43L, -522L, 186L, -588L, -572L, 140L, -371L, -481L,
        -215L, -1009L, 200L, -448L, -90L, -267L, -940L, -858L, -24L, -1045L,
        161L, -794L, 464L, -135L, 460L, 29L, -176L, 607L, 639L, -324L,
        450L, -290L, 283L, 397L, 246L, 235L, 522L, 635L, 715L, 328L,
        43L, -126L, 1544L, 962L, 1068L, 1117L, 1809L, 1038L, 1019L, 1287L,
        1952L, 1251L, 1360L, 1865L, 1900L, 1236L, 1372L, 1201L, 1719L,
        1127L, 1136L, 1743L, 1739L, 1713L, 1800L, 1971L, 1828L, 1419L,
        1835L, 1185L, 1457L, 1660L, 1623L, 1935L, 1551L, 1967L, 1495L,
        1066L, 1313L, 1873L, 1857L, 1589L, 1963L, 1544L, 1699L, 1709L,
        1632L, 1833L, 1908L, 1929L, 1782L, 1583L, 1964L, 1493L, 812L, 1387L,
        1520L, 1094L, 539L, 886L, 1413L, 1458L, 931L, 990L, 1580L,
        894L, 1326L, 756L, 902L, 1277L, 762L, 963L, 857L, 687L, 1616L,
        1306L, 1265L, 1622L, 1515L, 622L, 955L, 988L, 1046L, 403L, 845L,
        1106L, 1281L, 1418L, 1425L, 457L, 479L, 1129L, 1421L, 476L, 1619L,
        416L, 859L, 685L, 1474L, 1588L, 683L, 1193L, 554L, 1184L, 403L,
        1114L, 847L, 549L, 629L, 1196L, 836L, 1097L, 790L, 1170L, 1510L,
        998L, 860L, 1547L, 1359L, 652L, 1338L, 533L, 1059L, 974L, 1456L,
        1332L, 1173L, 1604L, 757L, 1013L, 1235L, 1303L, 534L, 1535L,
        888L, 603L, 521L, 735L, 932L, 1147L, 799L, 696L, 883L, 1055L,
        1290L, 1216L, 514L, 1381L, 586L, 730L, 813L, 387L, 1424L, 707L,
        1025L, 975L, 1429L, 1210L, 1338L, 1139L, 449L, 1295L, 1458L, 1024L,
        602L, 1086L, 428L, 883L),
  y = c(658L, 307L, 585L, -219L, -628L, -349L, -341L, 302L, -647L,
        851L, 103L, 1264L, 682L, 1131L, 872L, 798L, 871L, 748L, 962L,
        727L, 827L, 783L, 461L, 1312L, 569L, 1114L, 577L, 976L, 592L,
        1200L, 1369L, 509L, 1380L, 1043L, 125L, 1054L, 1249L, 1322L, 661L,
        901L, 774L, 1183L, 1409L, -123L, -82L, 532L, 275L, -13L, 700L,
        334L, 415L, 534L, 836L, 452L, 788L, 449L, 526L, 247L, 357L,
        536L, 212L, 194L, 1044L, 404L, -11L, 334L, 1001L, 951L, 491L, 276L,
        969L, 1007L, 358L, 740L, 551L, 890L, 635L, 900L, 661L, 461L,
        142L, 227L, 1067L, 72L, 621L, 760L, 1053L, -224L, 673L, 1258L,
        1297L, 967L, 1142L, 1045L, 798L, 1262L, 875L, 761L, 987L, 258L,
        310L, 42L, 1220L, 1045L, 1346L, 1250L, 1160L, 1015L, 1354L, 330L,
        793L, 1321L, 826L, 1052L, 919L, 1259L, 664L, 697L, 1097L, 1354L,
        1163L, 1220L, 1346L, 1117L, 1261L, 1152L, 1263L, 1141L, 1215L,
        883L, 1160L, 1213L, 1302L, 1332L, 1092L, 1151L, 1340L, 944L,
        -713L, 23L, -197L, -392L, -92L, -714L, -503L, -14L, -163L, -286L,
        -543L, -169L, -773L, -464L, -405L, 95L, -11L, -460L, -583L, 84L,
        -80L, -113L, -472L, -599L, -192L, -363L, -721L, -646L, 120L,
        -182L, -299L, 23L, -258L, 30L, -571L, -412L, 115L, -855L, -726L, -1L,
        -852L, -818L, -229L, -152L, -772L, -659L, -634L, -769L, -82L,
        -302L, -816L, -537L, -116L, -626L, -419L, -268L, -477L, -39L,
        -393L, -223L, -309L, 104L, -372L, -540L, -364L, -483L, -19L, 51L,
        -646L, -109L, -235L, -968L, -791L, -1291L, -1176L, -1377L,
        -1243L, -1314L, -1302L, -998L, -1002L, -1023L, -1285L, -1212L, -909L,
        -1304L, -1262L, -1351L, -870L, -1073L, -1063L, -1107L, -1102L,
        -1181L, -623L, -1383L, -550L, -721L, -666L, -741L, -1160L, -752L,
        -992L, -1371L, -956L, -1269L, -1168L, -1155L, -1334L, -783L,
        -1241L, -1217L, -1364L, -820L, -933L, -971L, -902L, -1144L,
        -1389L, -1164L, -1100L, -844L, -1055L, -881L, -857L, -471L, -735L,
        -604L, -561L, -466L, 2L, -487L, -870L, -751L, -1004L, -1069L,
        -982L, -537L, -693L, -873L, -753L, -585L, -872L, -1071L, -1048L,
        -805L, -234L, -647L, -205L, -11L, -692L, -179L, 0L, -50L, -892L,
        -566L, -115L, 82L, -8L, -921L, -812L, -343L, -20L, -642L, -84L,
        -562L, -786L, -456L, -128L, -225L, -668L, -650L, -21L, -470L,
        -845L, -263L, -433L, -581L, -202L, -905L, -196L, -735L, -578L, -338L,
        -902L, -354L, -367L, -177L, -667L, -468L, 62L, -507L, 13L,
        -729L, -707L, -273L, 565L, -210L, 530L, 341L, 499L, 471L, 182L,
        795L, 745L, 996L, 920L, 260L, -223L, 750L, 112L, 821L, 448L, 933L,
        415L, -203L, 883L, 324L, 332L, 907L, 449L, 460L, 820L, 578L, 99L,
        821L, 514L, 709L, 635L, 843L, 97L, 655L, 641L, 567L, 375L,
        398L, 277L, 389L, 353L, 79L, 710L, 149L, 573L, 476L, 944L, 764L,
        731L, 433L, 917L, 983L, 210L, 597L, 150L, 472L, 981L, 599L, 262L,
        695L, 714L, 911L, 637L, 848L, 149L, 225L, 364L, 180L, 292L, 257L,
        132L, 833L, 510L, 813L, 995L, 562L, 653L, 691L, 198L, -1240L,
        -1134L, -1213L, -1195L, -918L, -1116L, -1384L, -1402L, -1351L,
        -1291L, -1060L, -1208L, -1408L, -1098L, -1308L, -1304L, -983L,
        -1354L, -1026L, -1059L, -1031L, -1329L, -805L, -982L, -845L,
        -1254L, -1292L, -1160L, -962L, -1182L, -1409L, -899L, -1407L, -1105L)
) %>% 
mutate(x = as.numeric(x),
y = as.numeric(y))


# read in wes data from original network viz
wes <- read_rds("wes_anderson_films.rds")

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
  select(title, actor, film_year) %>% 
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

film_palette <- rev(wes_colours[c(1, 16, 23, 32, 37, 51, 65, 75, 82), ]$colours)
film_palette

# select a colour to be used for every actor node
actor_colour <- wes_colours[47, ]$colours
#actor_colour <- "#852D2C"
actor_colour

# get actor size (number of appearances) and colour for actor nodes in plot
act_aes <- wes_film_actor %>% 
  distinct(actor, actor_weight) %>% 
  rename(name = actor, weight = actor_weight) %>% 
  mutate(colour = actor_colour,
         font_colour = "#852D2C",
         film_year = "")

act_aes

# get film weighting (number of cast members) for film nodes - not used in the end
# and relevant colour for film nodes in plot
film_aes <- wes_film_actor %>% 
  distinct(title, film_weight, film_year) %>% 
  rename(name = title, weight = film_weight) %>% 
  cbind(colour = film_palette,
        font_colour = film_palette)

film_aes

# weighting and colours for actors and films
actor_film_aes <- rbind(act_aes, film_aes)

# convert dataframe to table graph object using tidygraph package
# this is made of 2 data frames: a node df and an edge df
wes_network <- wes %>% 
  arrange(film_year) %>% 
  select(title, actor) %>% 
  as_tbl_graph() %>% 
  mutate(name_wrap = str_wrap(name, 15))

wes_network

wrap_15 <- scales::wrap_format(15)

regulars <- act_aes %>% 
  filter(weight >= 4) %>% 
  pull(name)

distinct_title <- wes %>% 
  arrange(film_year) %>% 
  distinct(title) %>% 
  mutate(film_no = row_number())
  
wes_groups <- wes %>% 
  arrange(film_year) %>% 
  add_count(actor) %>% 
  inner_join(distinct_title, by = "title") %>% 
  add_count(actor, wt = film_no) %>% 
  mutate(group = case_when(n >= 4 ~ "Regulars",
                           n == 3 & nn == 6 ~ "Early Players",
                           n == 3 & nn == 24 ~ "Late Players")) %>% 
  distinct(actor, group)

wes_network_n <- wes_network %>% 
  # add type to indicate if node represents a film or an actor
  mutate(type = if_else(name %in% wes$title, "Film", "Actor")) %>% 
  # add the weightings to each film and actor
  inner_join(actor_film_aes, by = "name") %>% 
  mutate(x = coords$x, 
         y = coords$y,
         label = name_wrap,
         title = if_else(type == "Film", paste0("<p>", name,"<br>", "(", film_year, ")", "<br>", "Cast Size: ", weight, "</p>"),
                         paste0("<p>", name,"<br>", "Appearances: ", weight, "</p>")),
         font.size = if_else(type == "Film", 60, 10),
         font.color = font_colour,
         font.strokeWidth = 1,
         font.strokeColor = font_colour,
         font.face = "Futura",
         font.vadjust = if_else(type == "Film", -250, 0),
         font.bold = 1,
         color.background = colour,
         color.border = colour,
         color.highlight.background = colour,
         color.highlight.border = colour,
         color.hover.background = colour,
         color.hover.border = colour,         
         size = if_else(type == "Actor", weight*7, 70))

wes_network_n

wes_network_n_e <- wes_network_n %>% 
  # now focus on the edges data
  activate(edges) %>% 
  # add the colour attributed to the film nodes (from). N() accesses node data
  mutate(color.color = .N()$colour[from],
         color.highlight = .N()$colour[from],
         color.hover = .N()$colour[from],
         hoverWidth = 10 #,
         #selectionWidth = 10
         )

wes_network_n_e

ui <- fluidPage(
  includeCSS("styles.css"),
  h1(toupper("Wes Anderson Actor Network"), 
     align = "center",
     style = "font-family: 'Futura', cursive;
              font-weight: 800; line-height: 1.1; padding-bottom: 5px; padding-top: 5px;
              color: #852D2C;
              background-color: #EA93B8; margin-top: 0; margin-left: 0;border-radius: 6px;"), 
  fluidRow(
    column(width = 2,
           wellPanel(
           p("Network showing all credited actors appearing in Wes Anderson's 9 feature-length films."),   
           br(),
           br(),
           p("Each", strong("green dot"), "represents an", strong("actor"), "and is sized based on the number of films they've appeared in."),
           p("The", strong("lines"), "link an", strong("actor"), "to the", strong("film(s)"), "they appeared in, and are coloured based on the film."),
           p(strong("Interact"), "by hovering over and clicking on elements of the network."),
           br(),
           br(),
           p("------------", align = "center"),
           br(),
           p("Source:", a("IMDb.com", href = "https://www.imdb.com/name/nm0027572/")),
           br(),
           p("Made by committedtotape"),
           p(a("Twitter", href = "https://twitter.com/committedtotape"), "|", 
             a("Blog", href = "https://davidsmale.netlify.com/portfolio/"), "|", 
             a("GitHub", href = "https://github.com/committedtotape/wesandersonnetwork")),
           br(),
           p(""),
           style = "font-family: 'Futura', cursive; color: #852D2C; background-color: #EA93B8;
                    border-radius: 6px;"
           )),
    column(width = 8,
           visNetworkOutput("network", width = "100%", height = 700)),
    column(width = 2,
           wellPanel(
             p(strong("Explore it more!")),
             br(),
             sliderInput("apprange", "No. of Appearances :",
                         min = 1, max = 8,
                         value = c(1, 8)),
             br(),          
             selectInput(inputId = "selact", label = "Select an Actor :",
                         choices = sort(act_aes$name)),
             br(),
             selectInput(inputId = "selgroup", label = "Select a Group :",
                         choices = c("Regulars", "Early Players", "Late Players")),
             p(strong("To reset, click on empty space in the network.")),
             br(),
             p("------------", align = "center"),
             br(),
             p(em("\"I guess you’ve just gotta find something you love to do and then… do it for the rest of your life.\"")),
             p("Max Fischer, Rushmore"),
             br(),
             style = "font-family: 'Futura', cursive; color: #852D2C; background-color: #EA93B8;
                      border-radius: 6px;")
    )    
)
)

apps_selection_3_8 <- act_aes %>% 
  filter(weight >= 3 & weight <= 8) %>% 
  pull(name)

server <- function(input, output) {
  output$network <- renderVisNetwork({
    visIgraph(wes_network_n_e, 
              type = "full"
              ) %>% 
      visInteraction(hover = TRUE,
                     tooltipStyle = 'position: fixed;visibility:hidden;padding: 2px;white-space: nowrap;
 font-family: Futura;font-size:16px;color: #852D2C;background-color: #EA93B8;border-radius: 6px;') %>% 
      visOptions(
                 highlightNearest = list(enabled = TRUE, degree = list(from = 1, to = 1),
                                         algorithm = "hierarchical")) %>% 
      visEdges(arrows = list(to = FALSE, from = FALSE)) 
  })
  
  # select an actor
  observe({
    nodes_selection <- input$selact
    
    visNetworkProxy("network") %>%
      visSelectNodes(id = nodes_selection) 
  })
  
  # select a range for number of apearances of actor
  observe({
    apps_selection <- act_aes %>% 
      filter(weight >= input$apprange[1] & weight <= input$apprange[2]) %>% 
      pull(name)
    
      visNetworkProxy("network") %>%
        visSelectNodes(id = apps_selection)

  })
  
  # select group as decided by me - Regulars, Early Players, Late Players
  observe({
      group_selection <- wes_groups %>% 
        filter(group == input$selgroup) %>% 
        pull(actor)
    
    visNetworkProxy("network") %>%
      visSelectNodes(id = group_selection) 
  })

  
}


shinyApp(ui = ui, server = server)