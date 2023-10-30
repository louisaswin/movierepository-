
tmdbapikey = "personal api key"
token = "personal token"


library(httr)
library(jsonlite)

base_url = "https://api.themoviedb.org/4/list/8275899"

response = VERB("GET", url, add_headers('Authorization' = token, content_type("application/octet-stream"), accept("application/json"))
response_content = content(response, "text")
parsed_data = fromJSON(response_content)
df = as.data.frame(parsed_data$results)
#View(df)
#only get first 20 titles, need to loop through



all_movies = list()

total_pages = parsed_data$total_pages

for(page in 1:total_pages) { # adjust the range depending on how many pages you want
  response = GET(
    url = paste0(base_url, "?page=", page),
    add_headers('Authorization' = token),
    content_type("application/octet-stream"),
    accept("application/json")
  )
  
  response_content = content(response, "text")
  parsed_data = fromJSON(response_content)
  
  all_movies[[page]] = parsed_data$results
}

df_movies = do.call(rbind, all_movies)
#View(df_movies)

#summary(df_movies)
#colnames(df_movies)



library(dplyr)
# clean data 
df_movies1 = df_movies %>%
  mutate(Title = coalesce(title, name)) %>%
  select(-adult, -original_language, -video, -origin_country, -original_name, 
         -original_title, -title, -name) 
#View(df_movies1)



# fetch genre id info

# url for movie genres
# https://api.themoviedb.org/3/genre/movie/list?api_key=YOUR_API_KEY&language=en-US
response3 = GET(paste0("https://api.themoviedb.org/3/genre/movie/list?api_key=", tmdbapikey, "&language=en-US"))
content3 = content(response3, "text")
parsed_data3 = fromJSON(content3)
genre_data = as.data.frame(parsed_data3$genres)


# url for tv genres
#https://api.themoviedb.org/3/genre/tv/list?api_key=YOUR_API_KEY&language=en-US
response4 = GET(paste0("https://api.themoviedb.org/3/genre/tv/list?api_key=", tmdbapikey, "&language=en-US"))
content4 = content(response4, "text")
parsed_data4 = fromJSON(content4)
genre_data2 = as.data.frame(parsed_data3$genres)

#genre_data == genre_data2
# genre id is same for movies and tv shows




# clean data more, add genres, identify top picks
library(purrr)

df_movies2 = df_movies1 %>%
  mutate(media_type = ifelse(media_type == "movie", "Movie", "TV Show"),
         release_date = coalesce(release_date, first_air_date),
         genre = map(genre_ids, ~genre_data$name[genre_data$id %in% .x]),
         # attach names to genre ids
         # map applies function to each element of vector
         # .x refers to current genre_ids being processed
         # ~ indicates that what follows is a function
         top_pick = ifelse(Title %in% c("The Wilds", "Dead to Me", "Derry Girls", "Friends", 
                                        "Good Girls", "Grace and Frankie", "Scream Queens", 
                                        "Sex Education", "Psych", "Schitt's Creek", "The Great", 
                                        "The Good Place", "The Umbrella Academy", "You", "Fleabag", 
                                        "Barbie", "Barb & Star Go to Vista Del Mar", "Molly's Game", 
                                        "Gone Girl", "Prisoners", "The Dark Knight", 
                                        "The Perks of Being a Wallflower", "The Amazing Spider-Man 2", 
                                        "Pitch Perfect", "Sister Cities"), "Yes", "No")) %>%
  select(-first_air_date) 

#View(df_movies2)

df_movies3 = df_movies2 %>%
  filter(top_pick == "Yes")
length(df_movies3$Title)



# determine which genres are included in df
possiblegenres = character(0)
for (i in 1:nrow(df_movies2)) {
  newgenre = eval(df_movies2$genre[[i]])
  possiblegenres = unique(c(possiblegenres, newgenre))
}

#faster way
unique_genres = unique(unlist(df_movies2$genre))




library(rlang)

# learning side bar 

# how sym works: rlang:sym converts a string into a symbol. 
# important bc many packages don't take strings as direct input for columns
# instead operate on symbols
#column_name_string = "mpg"
#column_name_symbol = sym(column_name_string)

#mtcars %>%
  #select(!!column_name_symbol)
# !! forces the evaluation, telling dplyr functions to sort by 
# mpg column as if you had directly typed it





# clickable shiny

library(shiny)
library(shinyjs)
library(shinythemes)
library(rsconnect)

ui = fluidPage(
  theme = shinytheme("united"),
  
  titlePanel("Louisa's Movie and Show Repository"),
  tags$h4("Filters:"),
  sidebarLayout(
    sidebarPanel(
      selectInput("type", "Type:", choices = c("Movie", "TV Show"), selected = "Movie", multiple = TRUE),
      selectInput("genres", "Genre:", choices = unique_genres, selected = NULL, multiple = TRUE),
      hr(),    # horizontal line
      selectInput("sort_by", "Sort By:", choices = c("Title", "Release Date", "Popularity"), selected = "Title"),
      selectInput("sort_dir", "Direction:", choices = c("Ascending", "Descending"), selected = "Ascending")
      
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("All",
                 tags$p("Here is a list of all of the TV shows and movies that I can recommend in good conscience :)",
                        tags$br(),
                        "Click on a poster to get more information."),
                 uiOutput("postersUI")
        ),
        
        tabPanel("Top Picks",
                 tags$p("These are my top picks in no particular order."),
                 uiOutput("top10UI"))
      )
      
    )
  )
  
)



server = function(input, output) {
  
  output$postersUI = renderUI({
    # filtering
    # if statement to have all movies/shows shown if no genre is selected
    if (length(input$genres) == 0) {
      selected_data = df_movies2 %>% filter(media_type %in% input$type)
    } else {
      selected_data = df_movies2 %>% filter(media_type %in% input$type,
                                            # genre %in% input$genres works if movies each only have one genre
                                            # function below checks whether any genre is included selected genre
                                            map_lgl(genre, ~any(.x %in% input$genres)))
    }
    
    #sorting
    # switch evaluates EXPR and accordingly chooses one of the further arguments
    column_sort = switch(input$sort_by, "Title" = "Title", 
                         "Release Date" = "release_date",
                         "Popularity" = "popularity")
    if (input$sort_dir == "Descending") {
      selected_data = selected_data %>% arrange(desc(!!rlang::sym(column_sort)))
    } else {
      selected_data = selected_data %>% arrange(!!rlang::sym(column_sort))
    }
    
    # posters
    base_url = "https://image.tmdb.org/t/p/w200"
    
    img_tags = lapply(1:nrow(selected_data), function(i) {
      movie = selected_data[i, ]
      tags$a(href = paste0("#", movie$id),  #identifier
             tags$img(src = paste0(base_url, movie$poster_path), width = 200, height = 300, 
                      style = "margin: 10px;"),
             onclick = paste0("Shiny.setInputValue('clicked_movie', '", movie$id, "');"))
    })
    
    if (nrow(selected_data) == 0) {
      return(tags$h4("No media found based on the current filters."))
    } else {
      return(do.call(tagList, img_tags)) # return the list of imgs as UI content
    }
    
  })
  
  
  output$top10UI = renderUI({
    # filtering
    if (length(input$genres) == 0) {
      selected_data = df_movies3 %>% filter(media_type %in% input$type)
    } else {
      selected_data = df_movies3 %>% filter(media_type %in% input$type,
                                            # genre %in% input$genres works if movies each only have one genre
                                            # function below checks whether any genre is included selected genre
                                            map_lgl(genre, ~any(.x %in% input$genres)))
    }
    
    #sorting
    column_sort = switch(input$sort_by, "Title" = "Title", 
                         "Release Date" = "release_date",
                         "Popularity" = "popularity")
    if (input$sort_dir == "Descending") {
      selected_data = selected_data %>% arrange(desc(!!rlang::sym(column_sort)))
    } else {
      selected_data = selected_data %>% arrange(!!rlang::sym(column_sort))
    }
    
    # posters
    base_url = "https://image.tmdb.org/t/p/w200"
    
    img_tags = lapply(1:nrow(selected_data), function(i) {
      movie = selected_data[i, ]
      tags$a(href = paste0("#", movie$id),  #identifier
             tags$img(src = paste0(base_url, movie$poster_path), width = 200, height = 300, 
                      style = "margin: 10px;"),
             onclick = paste0("Shiny.setInputValue('clicked_movie', '", movie$id, "');"))
    })
    
    if (nrow(selected_data) == 0) {
      return(tags$h4("No media found based on the current filters."))
    } else {
      return(do.call(tagList, img_tags)) # return the list of imgs as UI content
    }
    
  })
  
  # clickable poster
  observeEvent(input$clicked_movie, {
    movie_id = input$clicked_movie
    selected_movie = df_movies2[df_movies2$id == movie_id, ] #look up movie details
    
    genre_list = unlist(selected_movie$genre)
    genre_subtitle = paste(genre_list, collapse = ", ")
    
    # show details
    showModal(modalDialog(
      title = selected_movie$Title,
      footer = NULL,
      selected_movie$overview,
      # <h1> to <h6> are header tags, h1 is highest level of header, h6 lowest
      tags$h5(genre_subtitle),
      easyClose = TRUE # close by clicking outside 
    ))
  })
  
  
}


shinyApp(ui = ui, server = server)











