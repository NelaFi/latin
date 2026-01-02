# Install required packages if needed
# install.packages(c("tidyverse", "stringr", "googlesheets4", "shiny", "DT"))

library(tidyverse)
library(stringr)
library(googlesheets4)
library(shiny)
library(DT)

gs4_deauth()

# Enhanced pattern loading with error handling
load_patterns <- function() {
  tryCatch({
    patterns <- read_sheet(
      "https://docs.google.com/spreadsheets/d/1_XilNL-PhiHtVNWiQmJO2QdV9mV0wRuH7bB254PG9D0/edit?gid=0#gid=0", 
      range = "Sheet1"
    )
    patterns %>% 
      filter(!is.na(lemma), !is.na(regex)) %>%
      mutate(
        regex_length = str_length(regex),
        # Add priority based on specificity
        priority = case_when(
          str_detect(regex, "\\\\b") ~ 3,  # word boundaries = high priority
          str_length(regex) > 20 ~ 2,       # longer patterns = medium priority
          TRUE ~ 1                           # shorter patterns = low priority
        )
      ) %>%
      arrange(desc(priority), desc(regex_length))
  }, error = function(e) {
    stop("Failed to load patterns from Google Sheets: ", e$message)
  })
}

# Enhanced role detection with duplicate handling
detect_roles <- function(id, text, patterns, remove_duplicates = TRUE) {
  # Handle missing/empty text
  if (is.na(text) || !nzchar(trimws(text))) {
    return(tibble(
      id = id,
      role = NA_character_,
      lemma = NA_character_,
      matched_text = NA_character_,
      match_position = NA_integer_,
      grammatical_case = NA_character_,
      gender = NA_character_,
      assigned_to = "missing_text",
      type = NA_character_,
      category = NA_character_,
      priority = NA_integer_,
      is_duplicate = FALSE
    ))
  }
  
  # Normalize text for better matching
  text_normalized <- str_to_title(text)
  
  # Vectorized pattern detection with position tracking
  out <- patterns %>%
    mutate(
      matches = map(regex, ~ str_locate_all(text, .x)[[1]]),
      present = map_lgl(matches, ~ nrow(.x) > 0),
      matched_text = map2_chr(regex, matches, ~ {
        if (nrow(.y) == 0) return(NA_character_)
        str_sub(text, .y[1, "start"], .y[1, "end"])
      }),
      match_position = map_int(matches, ~ if(nrow(.x) > 0) .x[1, "start"] else NA_integer_)
    ) %>%
    filter(present) %>%
    select(-matches) %>%
    transmute(
      id = id,
      role,
      lemma,
      matched_text,
      match_position,
      grammatical_case,
      gender,
      assigned_to = case_when(
        grammatical_case == "dative" ~ "deceased_role",
        grammatical_case == "nominative" ~ "commemorator_role",
        grammatical_case == "any" ~ "both",
        grammatical_case == "special" ~ "special",
        TRUE ~ "unknown"
      ),
      type,
      category,
      priority
    )
  
  # Handle no matches
  if (nrow(out) == 0) {
    return(tibble(
      id = id,
      role = NA_character_,
      lemma = NA_character_,
      matched_text = NA_character_,
      match_position = NA_integer_,
      grammatical_case = NA_character_,
      gender = NA_character_,
      assigned_to = "no_match",
      type = NA_character_,
      category = NA_character_,
      priority = NA_integer_,
      is_duplicate = FALSE
    ))
  }
  
  # Remove duplicates if requested
  if (remove_duplicates) {
    out <- out %>%
      arrange(priority, desc(str_length(matched_text)), match_position) %>%
      mutate(is_duplicate = FALSE)
    
    # Mark overlapping matches as duplicates
    if (nrow(out) > 1) {
      for (i in 2:nrow(out)) {
        current_start <- out$match_position[i]
        current_end <- current_start + str_length(out$matched_text[i]) - 1
        
        for (j in 1:(i-1)) {
          if (out$is_duplicate[i]) break
          
          prev_start <- out$match_position[j]
          prev_end <- prev_start + str_length(out$matched_text[j]) - 1
          
          # Check for overlap
          if ((current_start >= prev_start && current_start <= prev_end) ||
              (current_end >= prev_start && current_end <= prev_end) ||
              (current_start <= prev_start && current_end >= prev_end)) {
            out$is_duplicate[i] <- TRUE
          }
        }
      }
      
      out <- out %>% filter(!is_duplicate)
    }
  } else {
    out <- out %>%
      mutate(is_duplicate = FALSE) %>%
      arrange(match_position)
  }
  
  out
}

# Enhanced analysis functions
analyze_relationships <- function(results) {
  results %>%
    filter(!is.na(role)) %>%
    group_by(id) %>%
    summarise(
      n_relationships = n(),
      relationship_types = paste(unique(role), collapse = "; "),
      has_commemorator = any(grammatical_case == "nominative", na.rm = TRUE),
      has_deceased = any(grammatical_case == "dative", na.rm = TRUE),
      has_both_roles = has_commemorator & has_deceased,
      categories = paste(unique(na.omit(category)), collapse = "; "),
      .groups = "drop"
    )
}

analyze_by_category <- function(results) {
  results %>%
    filter(!is.na(category)) %>%
    separate_rows(category, sep = ", ") %>%
    count(category, grammatical_case, sort = TRUE) %>%
    pivot_wider(names_from = grammatical_case, values_from = n, values_fill = 0)
}

analyze_gender_patterns <- function(results) {
  results %>%
    filter(!is.na(gender), !is.na(assigned_to)) %>%
    count(assigned_to, gender, sort = TRUE) %>%
    pivot_wider(names_from = gender, values_from = n, values_fill = 0)
}

# Sample data
inscriptions_data <- tribble(
  ~id, ~clean_text_interpretive_word,
  "HD000013", "Dis Manibus Salbiae Impestratae coniugi piae bene merenti quae vixit annos XXXXII menses dies XVIII Lollius Herodianus fecit",
  "HD000097", "Ambatae Dessicae Rufi filiae annorum LV Titus Va socerae Arceae Dessicae Paterni filiae annorum X Titus uxsori",
  "HD000209", "Manibus Pontiae Anniae vixit annos VII menses VI dies XVII Annius pater et Pontia Agrippina mater filiae pientissimae fecerunt",
  "HD000221", "Dis Manibus sacrum Cornelia Iunia annorum XXVI pia in suis hic sita est sit tibi terra levis Marcus C Cornelius uxori bene meritae",
  "HD000303", "Dis Manibus Flavia Veneta vixit annis XXX Valerius Ianuarius coniugi carissimae bene merenti posuit memoriae causa",
  "HD000397", "Dis Manibus sacrum Cezzonia Lucaniane havete Zasgia Victoria Cezzonia bene merens femina pia vixit annos XXXV menses VIII dies XVI Lucius Turius Fabius Lucanianus filius eius pius vixit annos IIII menses IIII dies XVIIII hic siti sunt titulo usi bene quiescant",
  "HD000529", "Dis Manibus Aureliae Barachae vixit annos XXXV et Aur lt e gt liae Germanillae vixit annos IIII et altera filia Aurelia Germanilla vixit annos II et Immostae matri suae vixit annos LX Germanius Valens miles cohortis Mmiliariae Hemesenorum uxori et matri et filiis posuit et sibi vivius fecit",
  "HD000748", "vixit annos LX Lucius coniugi bene merenti posuit",
  "HD000812", "Marcellina annorum V hic sita est Caius Clodius Marcellus miles legionis XV Apollinaris filiae posuit",
  "HD000822", "Dis Manibus Postumia Silana hic sita est annorum XXX pia parentibus amatrix municipi Postumia Sura mater dedit",
  "HD000824", "Ambatae Dessicae Rufi filiae annorum LV Titus Va socerae Arceae Dessicae Paterni filiae annorum X Titus uxsori",
  "HD000825", "Nunnia Musa hic iacet vixit annis II mensibus VII diebus V tutum qui legis sic fruarus te parce puellam"
)

# Shiny UI
ui <- fluidPage(
  titlePanel("Latin Inscription Family Role Classifier"),
  
  theme = bslib::bs_theme(bootswatch = "flatly"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      h4("Configuration"),
      
      checkboxInput("remove_duplicates", 
                    "Remove Overlapping Matches", 
                    value = TRUE),
      
      actionButton("process", 
                   "Process Inscriptions", 
                   class = "btn-primary btn-block",
                   icon = icon("play")),
      
      hr(),
      
      h4("Upload Custom Data"),
      
      fileInput("file_upload", 
                "Upload CSV/TSV",
                accept = c(".csv", ".tsv", ".txt"),
                placeholder = "Optional"),
      
      helpText("CSV should have columns: 'id' and 'clean_text_interpretive_word'"),
      
      hr(),
      
      downloadButton("download_results", "Download Results", class = "btn-success btn-block"),
      downloadButton("download_summary", "Download Summary", class = "btn-info btn-block")
    ),
    
    mainPanel(
      width = 9,
      
      tabsetPanel(
        id = "tabs",
        
        # Results Tab
        tabPanel(
          "Detailed Results",
          icon = icon("table"),
          br(),
          DTOutput("results_table")
        ),
        
        # Summary Tab
        tabPanel(
          "Summary Statistics",
          icon = icon("chart-bar"),
          br(),
          
          fluidRow(
            column(6, 
                   h4("Overview"),
                   verbatimTextOutput("summary_overview")
            ),
            column(6,
                   h4("Classification Counts"),
                   verbatimTextOutput("summary_classification")
            )
          ),
          
          hr(),
          
          fluidRow(
            column(6,
                   h4("Relationship Analysis"),
                   DTOutput("relationship_table")
            ),
            column(6,
                   h4("Category by Case"),
                   DTOutput("category_table")
            )
          ),
          
          hr(),
          
          fluidRow(
            column(6,
                   h4("Gender Distribution"),
                   DTOutput("gender_table")
            ),
            column(6,
                   h4("Inscription Complexity"),
                   plotOutput("complexity_plot", height = "300px")
            )
          )
        ),
        
        # Individual Inscription View
        tabPanel(
          "Inscription Viewer",
          icon = icon("search"),
          br(),
          
          selectInput("inscription_select", 
                      "Select Inscription:", 
                      choices = NULL),
          
          hr(),
          
          h4("Original Text"),
          verbatimTextOutput("inscription_text"),
          
          h4("Detected Relationships"),
          DTOutput("inscription_relationships"),
          
          h4("Text with Highlights"),
          htmlOutput("highlighted_text")
        ),
        
        # Pattern Info
        tabPanel(
          "Pattern Library",
          icon = icon("book"),
          br(),
          
          DTOutput("patterns_table")
        )
      )
    )
  )
)

# Shiny Server
server <- function(input, output, session) {
  
  # Reactive values
  rv <- reactiveValues(
    patterns = NULL,
    results = NULL,
    data = inscriptions_data
  )
  
  # Load patterns on startup
  observe({
    tryCatch({
      rv$patterns <- load_patterns()
      showNotification("Patterns loaded successfully", type = "message")
    }, error = function(e) {
      showNotification(paste("Error loading patterns:", e$message), type = "error")
    })
  })
  
  # Handle file upload
  observeEvent(input$file_upload, {
    req(input$file_upload)
    
    tryCatch({
      ext <- tools::file_ext(input$file_upload$name)
      
      data <- if (ext == "csv") {
        read_csv(input$file_upload$datapath)
      } else if (ext %in% c("tsv", "txt")) {
        read_tsv(input$file_upload$datapath)
      } else {
        stop("Unsupported file format")
      }
      
      # Validate columns
      if (!all(c("id", "clean_text_interpretive_word") %in% names(data))) {
        stop("File must contain 'id' and 'clean_text_interpretive_word' columns")
      }
      
      rv$data <- data
      showNotification(paste("Loaded", nrow(data), "inscriptions"), type = "message")
      
    }, error = function(e) {
      showNotification(paste("Error loading file:", e$message), type = "error")
    })
  })
  
  # Process inscriptions
  observeEvent(input$process, {
    req(rv$patterns, rv$data)
    
    withProgress(message = 'Processing inscriptions...', value = 0, {
      
      n <- nrow(rv$data)
      
      results <- map2_dfr(
        rv$data$id,
        rv$data$clean_text_interpretive_word,
        function(id, text) {
          incProgress(1/n, detail = paste("Processing", id))
          detect_roles(id, text, rv$patterns, input$remove_duplicates)
        }
      )
      
      rv$results <- results
      
      # Update inscription selector
      updateSelectInput(session, "inscription_select", 
                       choices = unique(rv$data$id))
    })
    
    showNotification("Processing complete!", type = "message", duration = 3)
  })
  
  # Results table
  output$results_table <- renderDT({
    req(rv$results)
    
    rv$results %>%
      select(id, role, lemma, matched_text, grammatical_case, 
             gender, assigned_to, category, type) %>%
      datatable(
        filter = "top",
        options = list(
          pageLength = 25,
          scrollX = TRUE,
          searchHighlight = TRUE
        ),
        rownames = FALSE
      ) %>%
      formatStyle(
        'assigned_to',
        backgroundColor = styleEqual(
          c('deceased_role', 'commemorator_role', 'both', 'no_match'),
          c('#ffe6e6', '#e6f3ff', '#fff3e6', '#f0f0f0')
        )
      )
  })
  
  # Summary statistics
  output$summary_overview <- renderPrint({
    req(rv$results)
    
    cat("PROCESSING SUMMARY\n")
    cat(strrep("=", 50), "\n\n")
    
    cat("Total inscriptions:", length(unique(rv$results$id)), "\n")
    cat("Total relationships detected:", 
        sum(!is.na(rv$results$role)), "\n")
    cat("Average relationships per inscription:", 
        round(sum(!is.na(rv$results$role)) / length(unique(rv$results$id)), 2), "\n\n")
    
    cat("Inscriptions with:\n")
    cat("  - No matches:", 
        sum(rv$results$assigned_to == "no_match" & !duplicated(rv$results$id)), "\n")
    cat("  - Commemorator roles:", 
        length(unique(rv$results$id[rv$results$grammatical_case == "nominative"])), "\n")
    cat("  - Deceased roles:", 
        length(unique(rv$results$id[rv$results$grammatical_case == "dative"])), "\n")
    cat("  - Both roles:", 
        length(intersect(
          unique(rv$results$id[rv$results$grammatical_case == "nominative"]),
          unique(rv$results$id[rv$results$grammatical_case == "dative"])
        )), "\n")
  })
  
  output$summary_classification <- renderPrint({
    req(rv$results)
    
    cat("ROLE DISTRIBUTION\n")
    cat(strrep("=", 50), "\n\n")
    
    role_counts <- rv$results %>%
      filter(!is.na(role)) %>%
      count(role, sort = TRUE)
    
    print(role_counts, n = 20)
  })
  
  # Relationship analysis table
  output$relationship_table <- renderDT({
    req(rv$results)
    
    analyze_relationships(rv$results) %>%
      datatable(
        options = list(pageLength = 10, scrollX = TRUE),
        rownames = FALSE
      )
  })
  
  # Category analysis table
  output$category_table <- renderDT({
    req(rv$results)
    
    analyze_by_category(rv$results) %>%
      datatable(
        options = list(pageLength = 10, scrollX = TRUE),
        rownames = FALSE
      )
  })
  
  # Gender analysis table
  output$gender_table <- renderDT({
    req(rv$results)
    
    analyze_gender_patterns(rv$results) %>%
      datatable(
        options = list(pageLength = 10),
        rownames = FALSE
      )
  })
  
  # Complexity plot
  output$complexity_plot <- renderPlot({
    req(rv$results)
    
    complexity <- rv$results %>%
      filter(!is.na(role)) %>%
      count(id) %>%
      count(n, name = "inscriptions")
    
    ggplot(complexity, aes(x = factor(n), y = inscriptions)) +
      geom_col(fill = "#3498db") +
      geom_text(aes(label = inscriptions), vjust = -0.5) +
      labs(
        title = "Inscription Complexity Distribution",
        x = "Number of Relationships",
        y = "Number of Inscriptions"
      ) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  })
  
  # Individual inscription viewer
  output$inscription_text <- renderPrint({
    req(input$inscription_select, rv$data)
    
    text <- rv$data %>%
      filter(id == input$inscription_select) %>%
      pull(clean_text_interpretive_word)
    
    cat(text)
  })
  
  output$inscription_relationships <- renderDT({
    req(input$inscription_select, rv$results)
    
    rv$results %>%
      filter(id == input$inscription_select, !is.na(role)) %>%
      select(role, lemma, matched_text, grammatical_case, gender, category) %>%
      datatable(options = list(dom = 't'), rownames = FALSE)
  })
  
  output$highlighted_text <- renderUI({
    req(input$inscription_select, rv$data, rv$results)
    
    text <- rv$data %>%
      filter(id == input$inscription_select) %>%
      pull(clean_text_interpretive_word)
    
    matches <- rv$results %>%
      filter(id == input$inscription_select, !is.na(matched_text)) %>%
      arrange(match_position)
    
    if (nrow(matches) == 0) {
      return(HTML(paste0("<p>", text, "</p>")))
    }
    
    # Color code by assigned_to
    colors <- c(
      deceased_role = "#ffcccc",
      commemorator_role = "#ccddff",
      both = "#ffffcc",
      special = "#ccffcc"
    )
    
    highlighted <- text
    offset <- 0
    
    for (i in 1:nrow(matches)) {
      match_text <- matches$matched_text[i]
      color <- colors[matches$assigned_to[i]]
      if (is.na(color)) color <- "#f0f0f0"
      
      replacement <- paste0(
        "<mark style='background-color:", color, 
        "; padding: 2px 4px; border-radius: 3px;' title='",
        matches$role[i], " (", matches$grammatical_case[i], ")'>",
        match_text, "</mark>"
      )
      
      pos <- matches$match_position[i] + offset
      highlighted <- paste0(
        str_sub(highlighted, 1, pos - 1),
        replacement,
        str_sub(highlighted, pos + str_length(match_text))
      )
      
      offset <- offset + str_length(replacement) - str_length(match_text)
    }
    
    HTML(paste0(
      "<div style='font-size: 14px; line-height: 1.8; padding: 15px; 
       background: white; border: 1px solid #ddd; border-radius: 5px;'>",
      highlighted,
      "</div>",
      "<div style='margin-top: 10px; font-size: 12px;'>",
      "<span style='background: #ffcccc; padding: 2px 8px; margin-right: 10px;'>Deceased</span>",
      "<span style='background: #ccddff; padding: 2px 8px; margin-right: 10px;'>Commemorator</span>",
      "<span style='background: #ffffcc; padding: 2px 8px; margin-right: 10px;'>Both</span>",
      "<span style='background: #ccffcc; padding: 2px 8px;'>Special</span>",
      "</div>"
    ))
  })
  
  # Pattern library table
  output$patterns_table <- renderDT({
    req(rv$patterns)
    
    rv$patterns %>%
      select(lemma, role, grammatical_case, gender, category, type, regex) %>%
      datatable(
        filter = "top",
        options = list(pageLength = 25, scrollX = TRUE),
        rownames = FALSE
      )
  })
  
  # Download handlers
  output$download_results <- downloadHandler(
    filename = function() {
      paste0("inscription_results_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write_csv(rv$results, file)
    }
  )
  
  output$download_summary <- downloadHandler(
    filename = function() {
      paste0("inscription_summary_", Sys.Date(), ".csv")
    },
    content = function(file) {
      summary <- analyze_relationships(rv$results)
      write_csv(summary, file)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)
