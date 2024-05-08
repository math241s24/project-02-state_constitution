library(shiny)
library(rvest)
library(dplyr)
library(tidyverse)
library(tidytext)
library(tmap)
library(tigris)
library(tidyr)
library(RColorBrewer)

ct_url <- "https://www.cga.ct.gov/asp/Content/constitutions/1818Constitution.htm"
ct_html <- read_html(ct_url)

ct_text <- html_text(ct_html)

ct_con <- data.frame(word = unlist(strsplit(ct_text, "\\s+"))) %>%
  filter(word != "")  %>%
  mutate(word = str_replace_all(word, "[[:punct:][:space:]]+", "")) %>%
  filter(!word== "")

rows_to_exclude_ct <- c(1:7, 223:298, 6012:12230)

ct_con_tidy <- ct_con %>%
  mutate(word = tolower(word)) %>%
  filter(!(row_number() %in% rows_to_exclude_ct))

ct_word_count <- ct_con_tidy %>%
  filter(word %in%
           c("freedom", "liberty", "welfare", "consent", "prohibit", "equality", "rights")) %>%
  count(word)  %>%
  mutate(state = rep("Connecticut"),
         year = rep(1818))

data("stop_words")
ct_con_tidy_nostop <- ct_con_tidy %>%
  anti_join(stop_words) 
ct_all_words <- ct_con_tidy_nostop %>%
  count(word, sort = TRUE) %>%
  slice_max(n = 20, order_by = n) %>%
  mutate(state = rep("Connecticut"))

de_url <- "https://avalon.law.yale.edu/18th_century/de02.asp"
de_html <- read_html(de_url)

de_text <- html_text(de_html)

de_con <- data.frame(word = unlist(strsplit(de_text, "\\s+"))) %>%
  filter(word != "")  %>%
  mutate(word = str_replace_all(word, "[[:punct:][:space:]]+", "")) 

rows_to_exclude <- c(1:54, 3721:3796)

de_con_tidy <- de_con %>%
  mutate(word = tolower(word)) %>%
  filter(!(row_number() %in% rows_to_exclude))

de_word_count <- de_con_tidy %>%
  filter(word %in%
           c("freedom", "liberty", "welfare", "consent", "prohibit", "equality", "rights")) %>%
  count(word) %>%
  mutate(state = rep("Delaware"),
         year = rep(1776))


de_con_tidy_nostop <- de_con_tidy %>%
  anti_join(stop_words) 
de_all_words <- de_con_tidy_nostop %>%
  count(word, sort = TRUE) %>%
  slice_max(n = 20, order_by = n) %>%
  mutate(state = rep("Delaware"))

ga_url <- "https://avalon.law.yale.edu/18th_century/ga02.asp"
ga_html <- read_html(ga_url)

ga_text <- html_text(ga_html)

ga_con <- data.frame(word = unlist(strsplit(ga_text, "\\s+"))) %>%
  filter(word != "")  %>%
  mutate(word = str_replace_all(word, "[[:punct:][:space:]]+", "")) 

rows_to_exclude_ga <- c(1:56, 4422:4652)

ga_con_tidy <- ga_con %>%
  mutate(word = tolower(word)) %>%
  filter(!(row_number() %in% rows_to_exclude_ga))

ga_word_count <- ga_con_tidy %>%
  filter(word %in%
           c("freedom", "liberty", "welfare", "consent", "prohibit", "equality", "rights")) %>%
  count(word) %>%
  mutate(state = rep("Georgia"),
         year = rep(1777))


ga_con_tidy_nostop <- ga_con_tidy %>%
  anti_join(stop_words) 

ga_all_words <- ga_con_tidy_nostop %>%
  count(word, sort = TRUE) %>%
  slice_max(n = 20, order_by = n) %>%
  mutate(state = rep("Georgia"))

md_url <- "https://avalon.law.yale.edu/17th_century/ma02.asp"
md_html <- read_html(md_url)

md_text <- html_text(md_html)

md_con <- data.frame(word = unlist(strsplit(md_text, "\\s+"))) %>%
  filter(word != "")  %>%
  mutate(word = str_replace_all(word, "[[:punct:][:space:]]+", "")) 

rows_to_exclude_md <- c(1:94, 2623:2669, 8854:9084)

md_con_tidy <- md_con %>%
  mutate(word = tolower(word)) %>%
  filter(!(row_number() %in% rows_to_exclude_md))

md_word_count <- md_con_tidy %>%
  filter(word %in%
           c("freedom", "liberty", "welfare", "consent", "prohibit", "equality", "rights")) %>%
  count(word) %>%
  mutate(state = rep("Maryland"),
         year = rep(1776))


md_con_tidy_nostop <- md_con_tidy %>%
  anti_join(stop_words) 
md_all_words <- md_con_tidy_nostop %>%
  count(word, sort = TRUE) %>%
  slice_max(n = 20, order_by = n) %>%
  mutate(state = rep("Maryland"))

ma_url <- "http://www.nhinet.org/ccs/docs/ma-1780.htm"
ma_html <- read_html(ma_url)

ma_text <- html_text(ma_html)

ma_con <- data.frame(word = unlist(strsplit(ma_text, "\\s+"))) %>%
  filter(word != "")  %>%
  mutate(word = str_replace_all(word, "[[:punct:][:space:]]+", "")) 

rows_to_exclude_ma <- c(11433:11445)

ma_con_tidy <- ma_con %>%
  mutate(word = tolower(word)) %>%
  mutate(word=gsub("[[:space:]]", "", word)) %>%
  filter(!(row_number() %in% rows_to_exclude_ma))

ma_word_count <- ma_con_tidy %>%
  filter(word %in%
           c("freedom", "liberty", "welfare", "consent", "prohibit", "equality", "rights")) %>%
  count(word) %>%
  mutate(state = rep("Massachusetts"),
         year = rep(1780))

ma_con_tidy_nostop <- ma_con_tidy %>%
  anti_join(stop_words) 
ma_all_words <- ma_con_tidy_nostop %>%
  count(word, sort = TRUE) %>%
  slice_max(n = 20, order_by = n) %>%
  filter(!word%in%c("(art.", "")) %>%
  mutate(state = rep("Massachusetts"))

nh_url <- "https://avalon.law.yale.edu/18th_century/nh09.asp"
nh_html <- read_html(nh_url)

nh_text <- html_text(nh_html)

nh_con <- data.frame(word = unlist(strsplit(nh_text, "\\s+"))) %>%
  filter(word != "")  %>%
  mutate(word = str_replace_all(word, "[[:punct:][:space:]]+", "")) 

rows_to_exclude_nh <- c(1:63, 994:1242)

nh_con_tidy <- nh_con %>%
  mutate(word = tolower(word)) %>%
  filter(!(row_number() %in% rows_to_exclude_nh))

nh_word_count <- nh_con_tidy %>%
  filter(word %in%
           c("freedom", "liberty", "welfare", "consent", "prohibit", "equality", "rights")) %>%
  count(word) %>%
  mutate(state = rep("New Hampshire"),
         year = rep(1776))


nh_con_tidy_nostop <- nh_con_tidy %>%
  anti_join(stop_words) 

nh_all_words <- nh_con_tidy_nostop %>%
  count(word, sort = TRUE) %>%
  slice_max(n = 20, order_by = n) %>%
  filter(!word%in%c("(art.", " ")) %>%
  mutate(state = rep("New Hampshire"))

nj_url <- "https://avalon.law.yale.edu/18th_century/nj15.asp"
nj_html <- read_html(nj_url)

nj_text <- html_text(nj_html)

nj_con <- data.frame(word = unlist(strsplit(nj_text, "\\s+"))) %>%
  filter(word != "")  %>%
  mutate(word = str_replace_all(word, "[[:punct:][:space:]]+", "")) 

rows_to_exclude_nj <- c(1:55, 2524:2889)

nj_con_tidy <- nj_con %>%
  mutate(word = tolower(word)) %>%
  filter(!(row_number() %in% rows_to_exclude_nj))

nj_word_count <- nj_con_tidy %>%
  filter(word %in%
           c("freedom", "liberty", "welfare", "consent", "prohibit", "equality", "rights")) %>%
  count(word) %>%
  mutate(state = rep("New Jersey"),
         year = rep(1776))


nj_con_tidy_nostop <- nj_con_tidy %>%
  anti_join(stop_words) 
nj_all_words <- nj_con_tidy_nostop %>%
  count(word, sort = TRUE) %>%
  slice_max(n = 20, order_by = n) %>%
  filter(!word%in%c("(art.", " "))  %>%
  mutate(state = rep("New Jersey"))

ny_url <- "https://avalon.law.yale.edu/18th_century/ny01.asp"
ny_html <- read_html(ny_url)

ny_text <- html_text(ny_html)

ny_con <- data.frame(word = unlist(strsplit(ny_text, "\\s+"))) %>%
  filter(word != "")  %>%
  mutate(word = str_replace_all(word, "[[:punct:][:space:]]+", "")) 

rows_to_exclude <- c(1:68, 7813:8781)

ny_con_tidy <- ny_con %>%
  mutate(word = tolower(word)) %>%
  filter(!(row_number() %in% rows_to_exclude))
ny_word_count <- ny_con_tidy %>%
  filter(word %in%
           c("freedom", "liberty", "welfare", "consent", "prohibit", "equality", "rights")) %>%
  count(word) %>%
  mutate(state = rep("New York"),
         year = rep(1777))


ny_con_tidy_nostop <- ny_con_tidy %>%
  anti_join(stop_words) 
ny_all_words <- ny_con_tidy_nostop %>%
  count(word, sort = TRUE) %>%
  slice_max(n = 20, order_by = n) %>%
  mutate(state = rep("New York"))

nc_url <- "https://avalon.law.yale.edu/18th_century/nc07.asp"
nc_html <- read_html(nc_url)

nc_text <- html_text(nc_html)

nc_con <- data.frame(word = unlist(strsplit(nc_text, "\\s+"))) %>%
  filter(word != "")  %>%
  mutate(word = str_replace_all(word, "[[:punct:][:space:]]+", "")) 

rows_to_exclude <- c(1:58, 3991:4231)

nc_con_tidy <- nc_con %>%
  mutate(word = tolower(word)) %>%
  filter(!(row_number() %in% rows_to_exclude))
nc_word_count <- nc_con_tidy %>%
  filter(word %in%
           c("freedom", "liberty", "welfare", "consent", "prohibit", "equality", "rights")) %>%
  count(word) %>%
  mutate(state = rep("North Carolina"),
         year = rep(1776))


nc_con_tidy_nostop <- nc_con_tidy %>%
  anti_join(stop_words) 

nc_all_words <- nc_con_tidy_nostop %>%
  count(word, sort = TRUE) %>%
  slice_max(n = 20, order_by = n) %>%
  filter(!word%in%c("(--", " ")) %>%
  mutate(state = rep("North Carolina"))

pa_url <- "https://avalon.law.yale.edu/18th_century/pa08.asp"
pa_html <- read_html(pa_url)

pa_text <- html_text(pa_html)

pa_con <- data.frame(word = unlist(strsplit(pa_text, "\\s+"))) %>%
  filter(word != "")  %>%
  mutate(word = str_replace_all(word, "[[:punct:][:space:]]+", "")) 

rows_to_exclude <- c(1:57, 6196:6533)

pa_con_tidy <- pa_con %>%
  mutate(word = tolower(word)) %>%
  filter(!(row_number() %in% rows_to_exclude))

pa_word_count <- pa_con_tidy %>%
  filter(word %in%
           c("freedom", "liberty", "welfare", "consent", "prohibit", "equality", "rights")) %>%
  count(word) %>%
  mutate(state = rep("Pennsylvania"),
         year = rep(1776))


pa_con_tidy_nostop <- pa_con_tidy %>%
  anti_join(stop_words) 
pa_all_words <- pa_con_tidy_nostop %>%
  count(word, sort = TRUE) %>%
  slice_max(n = 20, order_by = n) %>%
  filter(!word%in%c("(--", " ")) %>%
  mutate(state = rep("Pennsylvania"))

pa_word_count <- pa_con_tidy %>%
  filter(word %in%
           c("freedom", "liberty", "welfare", "consent", "prohibit", "equality", "rights")) %>%
  count(word) %>%
  mutate(state = rep("Pennsylvania"))


pa_con_tidy_nostop <- pa_con_tidy %>%
  anti_join(stop_words) 
pa_all_words <- pa_con_tidy_nostop %>%
  count(word, sort = TRUE) %>%
  slice_max(n = 20, order_by = n) %>%
  filter(!word%in%c("(--", " ")) %>%
  mutate(state = rep("Pennsylvania"))

ri_url <- "https://www.wordservice.org/State%20Constitutions/usa1031.htm"
ri_html <- read_html(ri_url)

ri_text <- html_text(ri_html)

ri_con <- data.frame(word = unlist(strsplit(ri_text, "\\s+"))) %>%
  filter(word != "")  %>%
  mutate(word = str_replace_all(word, "[[:punct:][:space:]]+", "")) %>%
  filter(!word== "")


rows_to_exclude_ri <- c(1:200, 7322:7331)

ri_con_tidy <- ri_con %>%
  mutate(word = tolower(word)) %>%
  filter(!(row_number() %in% rows_to_exclude_ri))

ri_word_count <- ri_con_tidy %>%
  filter(word %in%
           c("freedom", "liberty", "welfare", "consent", "prohibit", "equality", "rights")) %>%
  count(word) %>%
  mutate(state = rep("Rhode Island"),
         year = rep(1843))

ri_con_tidy_nostop <- ri_con_tidy %>%
  anti_join(stop_words) 
ri_all_words <- ri_con_tidy_nostop %>%
  count(word, sort = TRUE) %>%
  slice_max(n = 20, order_by = n) %>%
  filter(!word%in%c("(--", " ")) %>%
  mutate(state = rep("Rhode Island"))

sc_url <- "https://avalon.law.yale.edu/18th_century/sc02.asp"
sc_html <- read_html(sc_url)

sc_text <- html_text(sc_html)

sc_con <- data.frame(word = unlist(strsplit(sc_text, "\\s+"))) %>%
  filter(word != "")  %>%
  mutate(word = str_replace_all(word, "[[:punct:][:space:]]+", "")) 

rows_to_exclude <- c(1:58, 5639:5880)

sc_con_tidy <- sc_con %>%
  mutate(word = tolower(word)) %>%
  filter(!(row_number() %in% rows_to_exclude))

sc_word_count <- sc_con_tidy %>%
  filter(word %in%
           c("freedom", "liberty", "welfare", "consent", "prohibit", "equality", "rights")) %>%
  count(word) %>%
  mutate(state = rep("South Carolina"),
         year = rep(1778))


data("stop_words")
sc_con_tidy_nostop <- sc_con_tidy %>%
  anti_join(stop_words) 
sc_all_words <- sc_con_tidy_nostop %>%
  count(word, sort = TRUE) %>%
  slice_max(n = 20, order_by = n) %>%
  filter(!word%in%c("(--", " "))  %>%
  mutate(state = rep("South Carolina"))

va_url <- "https://www.wordservice.org/State%20Constitutions/usa1025.htm"
va_html <- read_html(va_url)

va_text <- html_text(va_html)

va_con <- data.frame(word = unlist(strsplit(va_text, "\\s+"))) %>%
  filter(word != "")  %>%
  mutate(word = str_replace_all(word, "[[:punct:][:space:]]+", "")) 

rows_to_exclude_va <- c(1:201, 3680:3689)

va_con_tidy <- va_con %>%
  mutate(word = tolower(word)) %>%
  filter(!(row_number() %in% rows_to_exclude_va))

va_word_count <- va_con_tidy %>%
  filter(word %in%
           c("freedom", "liberty", "welfare", "consent", "prohibit", "equality", "rights")) %>%
  count(word) %>%
  mutate(state = rep("Virginia"),
         year = rep(1776))


va_con_tidy_nostop <- va_con_tidy %>%
  anti_join(stop_words) 

va_all_words <- va_con_tidy_nostop %>%
  count(word, sort = TRUE) %>%
  slice_max(n = 20, order_by = n) %>%
  filter(!word%in%c("(--", " ")) %>%
  mutate(state = rep("Virginia"))

thirteen_cons <- bind_rows(ct_word_count, 
                           de_word_count, 
                           ga_word_count, 
                           ma_word_count, 
                           md_word_count, 
                           nc_word_count, 
                           nh_word_count, 
                           nj_word_count, 
                           ny_word_count, 
                           pa_word_count, 
                           ri_word_count, 
                           sc_word_count, 
                           va_word_count)
states <- states()

map_data <- thirteen_cons %>% 
  left_join(states, join_by(state == NAME)) %>%
  pivot_wider(names_from = word, values_from = n)


ui <- fluidPage(
  
  
  titlePanel("State Constitution Keywords"),
  
  
  sidebarLayout(
    sidebarPanel(
      selectizeInput("word",
                     "Keyword:",
                     choices = NULL),
      submitButton("Update Results!"),
      sliderInput("year", "Filter by Year", 
                  min = 1776, max = 1843,
                  value = c(1776, 1843),
                  step = 1)
    ),
    
    mainPanel(
      plotOutput("map")
    )
  )
)

server <- function(input, output, session) {
  
  updateSelectizeInput(session, "word", choices = unique(thirteen_cons$word))
  
  output$map <- renderPlot({
    keyword <- input$word
    
    keyword_data <- thirteen_cons %>% 
      filter(word == keyword, year >= input$year[1], year <= input$year[2]) %>% 
      select(state, n)
    
    selected_states <- c("Connecticut", "Delaware", "Georgia", "Massachusetts", "Maryland", 
                         "New Jersey", "New York", "North Carolina", "Rhode Island", 
                         "South Carolina", "Virginia", "New Hampshire", "Pennsylvania")
    
    
    filtered_states <- states %>%
      filter(NAME %in% selected_states)
    
    
    keyword_map_data <- left_join(filtered_states, keyword_data, by = c("NAME" = "state"))
    
    
    no_data_color <- "gray"
    orange_palette <- brewer.pal(9, "Oranges")
    
    
    keyword_map_data$n[is.na(keyword_map_data$n)] <- 0
    
    tm_shape(keyword_map_data) +
      tm_fill(col = "n", title = keyword, palette = c(no_data_color, orange_palette), 
              style = "cat", breaks = c(-1, 0, max(keyword_map_data$n, na.rm = TRUE))) +
      tm_borders() +
      tm_layout(title = paste("Frequency of", keyword))
  })
}






shinyApp(ui = ui, server = server)