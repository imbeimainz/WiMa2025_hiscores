library("shiny")
library("bslib")
library("dplyr")

library("reactable")

location_hiscores_file <- "round_records_test.csv"

colnames_hiscores_file <- c(
  "round_id",
  "image_id",
  "button_pressed",
  "correct_mask",
  "time_response",
  "player_id",
  "player_name"
)

fullscore_threshold <- 0.3 # two full seconds, then it decays
decay_slope <- 3
max_time <- 10 # we said 10 seconds, right?


# TODO: clarify the fact that we have only ONE player_id but different names

from_singleentries_to_scores <- function(scores_tbl) {
  # the idea: go row-wise, and then aggregate by player
  
  scores_tbl$score <- 0      # all set to null, initially
  
  for (i in 1:nrow(scores_tbl)) {
    this_row <- scores_tbl[i, ]
    if (this_row$button_pressed != this_row$correct_mask) {
      # they did not match, then score stays 0
      this_score <- 0
    } else {
      # they do match, and we need to check the time needed
      this_score <- time_to_score(this_row$time_response)
    }
    
    scores_tbl[i, "score"] <- this_score
  }
  
  scores_tbl
  # now we do have each round scored. and we proceed with aggregation
  
  scores_aggregated <- 
    scores_tbl |> 
    group_by(player_name) |> 
    summarize(
      player_score = sum(score),
      rounds_played = n())
  
  return(scores_aggregated)
}

time_to_score <- function(time_needed) {
  if (time_needed > max_time) {
    score <- 0
  }
  
  if (time_needed <= fullscore_threshold) {
    score <- 100
  } else {
    score <- 100 - decay_slope * (time_needed - fullscore_threshold)
  }
}

# as a test, plotting some values
xs <- seq(from = 0, to = 10, by = 0.1)
plot(xs, sapply(xs, time_to_score), ylim = c(0,100))

# Define UI ------------------------
wima_ui <- bslib::page_sidebar(
  title = "Maskenball Hi-Scores",
  theme = bs_theme(
    # bg = "#101010",
    # fg = "#FFF",
    # primary = "#E69F00",
    # secondary = "#0072B2",
    # success = "#009E73",
    # base_font = font_google("Inter"),
    # code_font = font_google("JetBrains Mono")
    preset = "sketchy"
  ),
  sidebar = bslib::sidebar(
    title = "These are the current rankings for the Maskenball game!",
    img(src = "IMBEI-UM_maskenball_social.png"),
    "Keep in mind the scores are calculated only upon completion of the 20 images"
  ),
  bslib::card(
    bslib::card_header("Top scorers - WiMa 2025"),
    "Yadda yadda",
    bslib::layout_column_wrap(
      width = 1/2,
      uiOutput("player_counter"),
      uiOutput("latest_player_score")
    ),
    reactable::reactableOutput("topscorers"),
    bslib::card_footer("The rankings are re-calculated automatically every couple of seconds, please hold on if your name does not show up right away...")
  )
)

# Define server logic ----
wima_server <- function(input, output, session) {
  
  hiscores_data <- reactivePoll(1000, session,
                                # This function returns the time that location_hiscores_file was last modified
                                checkFunc = function() {
                                  if (file.exists(location_hiscores_file))
                                    file.info(location_hiscores_file)$mtime[1]
                                  else
                                    ""
                                },
                                # This function returns the content of location_hiscores_file
                                valueFunc = function() {
                                  scores_tbl <- read.csv(location_hiscores_file, header = FALSE)
                                  colnames(scores_tbl) <- colnames_hiscores_file
                                  
                                  scores_tbl$player_name <- trimws(scores_tbl$player_name)
                                  
                                  return(scores_tbl)
                                }
  )
  
  
  hiscores_computed <- reactive({
    scores_tbl <- hiscores_data()
    
    ## HERE: TODO, the actual "munging" of the individual things to show the scores
    
    
    # this will also handle the "piecewise components" of how the score is computed
    
    
    # for now: A simple count on the number of entries with this and that
    # processed_results <- table(scores_tbl$player_name)
    # 
    # final_scores <- data.frame(
    #   player = names(processed_results),
    #   score = as.vector(processed_results)
    # )
    
    final_scores <- from_singleentries_to_scores(scores_tbl)
    colnames(final_scores) <- c("Player", "Score", "Rounds played")
    
    final_scores
  })
  
  output$topscorers <- reactable::renderReactable({
    computed_scores <- hiscores_computed()
    
    computed_scores <- dplyr::arrange(computed_scores, desc(Score))
    
    ## TODO if more than three rows are in, show gold silver and bronze? would be a funny touch
    computed_scores$Player[1:3] <- 
      paste0(c(emo::ji("gold"), emo::ji("silver"), emo::ji("bronze")),
             computed_scores$Player[1:3])
    
    reactable::reactable(computed_scores, searchable = TRUE)
  })
  
  output$player_counter <- renderUI({
    bslib::value_box("Players so far", 
                     value = length(unique(hiscores_data()$player_name)),
                     showcase = bsicons::bs_icon("people-fill"),
                     theme = "teal")
  })
  
  output$latest_player_score <- renderUI({
    latest_player <- tail(hiscores_data()[ , "player_name"], n = 1)
    message(latest_player)
    message(match(latest_player, hiscores_computed()$Player))
    entry_for_latest <- hiscores_computed()[match(latest_player, hiscores_computed()$Player),]
    message(entry_for_latest)
    
    bslib::value_box("Latest Player:", 
                     value = paste0(latest_player, ": ", entry_for_latest$Score, 
                                    "\n(", entry_for_latest$`Rounds played`, " rounds played)"),
                     showcase = bsicons::bs_icon("person-circle"),
                     theme = "primary")
  })
}

# Run the app ----
shinyApp(ui = wima_ui, server = wima_server)
