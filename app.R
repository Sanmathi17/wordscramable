library(shiny)
library(shinythemes)

# Word list (you can expand this)
words <- c(
  "PYTHON", "JAVASCRIPT", "PROGRAMMING", "COMPUTER", "DATABASE",
  "ALGORITHM", "FUNCTION", "VARIABLE", "DEVELOPER", "SOFTWARE",
  "CODING", "TESTING", "DEBUG", "ARRAY", "LOOP"
)

# UI definition
ui <- fluidPage(
  theme = shinytheme("superhero"),
  
  tags$head(
    tags$style(HTML("
      .game-container { text-align: center; padding: 20px; }
      .scrambled-word { font-size: 36px; letter-spacing: 3px; margin: 20px 0; }
      .timer { font-size: 24px; color: #ff9800; }
      .score { font-size: 20px; margin: 15px 0; }
      .feedback { margin: 15px 0; font-size: 18px; }
      .correct { color: #4caf50; }
      .incorrect { color: #f44336; }
    "))
  ),
  
  titlePanel("Word Scramble Challenge"),
  
  div(class = "game-container",
      # Game status
      div(class = "score",
          textOutput("score_display")),
      div(class = "timer",
          textOutput("timer")),
      
      # Game area
      div(class = "scrambled-word",
          textOutput("scrambled_word")),
      
      # Player input
      textInput("guess", "Your guess:",
                placeholder = "Type your answer here"),
      
      # Buttons
      actionButton("submit", "Submit", class = "btn-primary"),
      actionButton("next_word", "Next Word", class = "btn-success"),
      actionButton("new_game", "New Game", class = "btn-info"),
      
      # Feedback
      div(class = "feedback",
          htmlOutput("feedback")),
      
      # Game statistics
      hr(),
      h3("Game Statistics"),
      tableOutput("game_stats")
  )
)

# Server logic
server <- function(input, output, session) {
  # Reactive values
  game_state <- reactiveValues(
    current_word = sample(words, 1),
    scrambled = "",
    score = 0,
    attempts = 0,
    correct = 0,
    time_left = 30,
    timer_active = FALSE,
    game_active = TRUE,
    words_seen = character(0),
    history = data.frame(
      Word = character(0),
      Guess = character(0),
      Result = character(0),
      TimeLeft = numeric(0),
      stringsAsFactors = FALSE
    )
  )
  
  # Timer
  observe({
    invalidateLater(1000)
    if (game_state$timer_active && game_state$time_left > 0) {
      game_state$time_left <- game_state$time_left - 1
      if (game_state$time_left == 0) {
        game_state$timer_active <- FALSE
        game_state$game_active <- FALSE
      }
    }
  })
  
  # Scramble word function
  scramble_word <- function(word) {
    chars <- strsplit(word, "")[[1]]
    paste(sample(chars), collapse = "")
  }
  
  # Initialize game
  observe({
    game_state$scrambled <- scramble_word(game_state$current_word)
  })
  
  # Handle guess submission
  observeEvent(input$submit, {
    req(input$guess)
    if (!game_state$game_active) return()
    
    guess <- toupper(trimws(input$guess))
    correct <- guess == game_state$current_word
    game_state$attempts <- game_state$attempts + 1
    
    if (correct) {
      game_state$score <- game_state$score + 
        max(10, game_state$time_left * 2)
      game_state$correct <- game_state$correct + 1
    }
    
    # Update history
    game_state$history <- rbind(
      game_state$history,
      data.frame(
        Word = game_state$current_word,
        Guess = guess,
        Result = ifelse(correct, "Correct", "Incorrect"),
        TimeLeft = game_state$time_left,
        stringsAsFactors = FALSE
      )
    )
    
    # Clear input
    updateTextInput(session, "guess", value = "")
  })
  
  # Next word handler
  observeEvent(input$next_word, {
    if (!game_state$game_active) return()
    
    # Get new word that hasn't been seen
    available_words <- setdiff(words, game_state$words_seen)
    if (length(available_words) == 0) {
      game_state$game_active <- FALSE
      return()
    }
    
    game_state$current_word <- sample(available_words, 1)
    game_state$words_seen <- c(game_state$words_seen, game_state$current_word)
    game_state$scrambled <- scramble_word(game_state$current_word)
    game_state$time_left <- 30
    game_state$timer_active <- TRUE
  })
  
  # New game handler
  observeEvent(input$new_game, {
    game_state$current_word <- sample(words, 1)
    game_state$scrambled <- scramble_word(game_state$current_word)
    game_state$score <- 0
    game_state$attempts <- 0
    game_state$correct <- 0
    game_state$time_left <- 30
    game_state$timer_active <- TRUE
    game_state$game_active <- TRUE
    game_state$words_seen <- character(0)
    game_state$history <- data.frame(
      Word = character(0),
      Guess = character(0),
      Result = character(0),
      TimeLeft = numeric(0),
      stringsAsFactors = FALSE
    )
  })
  
  # Outputs
  output$scrambled_word <- renderText({
    game_state$scrambled
  })
  
  output$timer <- renderText({
    if (!game_state$game_active) {
      "Game Over!"
    } else {
      paste("Time left:", game_state$time_left, "seconds")
    }
  })
  
  output$score_display <- renderText({
    paste("Score:", game_state$score)
  })
  
  output$feedback <- renderUI({
    if (nrow(game_state$history) == 0) return(NULL)
    
    last_result <- tail(game_state$history, 1)
    if (last_result$Result == "Correct") {
      HTML(sprintf('<div class="correct">Correct! +%d points</div>',
                   max(10, last_result$TimeLeft * 2)))
    } else {
      HTML(sprintf('<div class="incorrect">Incorrect! The word was: %s</div>',
                   last_result$Word))
    }
  })
  
  output$game_stats <- renderTable({
    data.frame(
      Statistic = c("Total Attempts", "Correct Answers", "Accuracy", "Total Score"),
      Value = c(
        game_state$attempts,
        game_state$correct,
        sprintf("%.1f%%", 
                ifelse(game_state$attempts > 0,
                       game_state$correct / game_state$attempts * 100,
                       0)),
        game_state$score
      )
    )
  })
}

# Run the app
shinyApp(ui = ui, server = server)