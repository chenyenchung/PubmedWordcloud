#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library("shiny")
library("easyPubMed")
library("magrittr")
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Plot a Word Clound from PubMed"),
   
   sidebarLayout(
      sidebarPanel(
        textInput("search4", h3("Generating Your WordCloud from:"), value = "")
      ),
      
      mainPanel(
        h3(textOutput("title")),
        plotOutput("plot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$title <- renderText({
    title <- paste0("\"",input$search4,"\"") %>% paste("About",.)
  })
  
  output$plot <- renderPlot(
    {
      temp <- get_pubmed_ids(input$search4) %>%
        fetch_pubmed_data(., format = "xml") %>%
        articles_to_list(.)
      
      text <- lapply(temp, function(x) custom_grep(x, tag = "AbstractText")) %>% unlist
      
      # Load the data as a corpus
      docs <- Corpus(VectorSource(text))
      
      toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
      docs <- tm_map(docs, toSpace, "/")
      docs <- tm_map(docs, toSpace, "@")
      docs <- tm_map(docs, toSpace, "\\|")
      
      # Convert the text to lower case
      docs <- tm_map(docs, content_transformer(tolower))
      # Remove english common stopwords
      docs <- tm_map(docs, removeWords, stopwords("english"))
      # specify your stopwords as a character vector
      docs <- tm_map(docs, removeWords, c("expression", "significantly","research","study",
                                          "patients","cells","cell","gene","genes"))
      # Remove punctuations
      docs <- tm_map(docs, removePunctuation)
      # Eliminate extra white spaces
      docs <- tm_map(docs, stripWhitespace)
      
      
      dtm <- TermDocumentMatrix(docs)
      m <- as.matrix(dtm)
      v <- sort(rowSums(m),decreasing=TRUE)
      d <- data.frame(word = names(v),freq=v)
      wordcloud(words = d$word, freq = d$freq, min.freq = 1,
                max.words=200, random.order=FALSE, rot.per=0.35, 
                colors=brewer.pal(8, "Dark2"))
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)