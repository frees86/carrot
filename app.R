# =============================================
# APPLICATION SHINY : CARROT PROTOCOL SELECTOR
# =============================================

# We document the version of the code (note: if using the logo, it needs to be updated too!):
version <- "Version: 2026-02-25"

# We define the path to the folder containing the csv or Excel files to be loaded:
folder_path <- "./source/"
# # # IF THE PREVIOUS PATH CANNOT BE FOUND, you may need to give the whole path to this folder:
# folder_path <- "C:/Users/frees/carrot/source/"

# # To install the required packages (to do only once - note that it may be important to update readxl):
# install.packages("shiny")
# install.packages("readxl")
# install.packages("openxlsx")
# install.packages("xlsx")
# install.packages("stringr")
# install.packages("dplyr")
# install.packages("shinythemes")
# install.packages("DT")
# install.packages("shinyfullscreen")

# install.packages("shinylive")
# install.packages("httpuv")

# Loading the required packages:
library(readxl)
library(openxlsx)
library(xlsx)
library(stringr)
library(dplyr)
library(shiny)
library(shinythemes)
library(DT)
library(bslib)

library(shinylive)
library(httpuv)
library(svglite)

# WATCH OUT: Because of a bug when downloading results after exporting the app online with shinylive,
# we redefine the functions downloadLink and downloadButton here (origin: https://shiny.posit.co/r/components/inputs/download-link/):

downloadLink <- function(...) {
  tag <- shiny::downloadLink(...)
  tag$attribs$download <- NULL
  tag
}
downloadButton <- function(...) {
  tag <- shiny::downloadButton(...)
  tag$attribs$download <- NULL
  tag
}

######################################################################################################
######################################################################################################

# Shiny is based on a user interface ('ui') and a server function ('server').

# --- DEFINING THE USER INTERFACE (UI) ---

# We build the user interface:
ui <- page_fillable(
  
  # General theme:
  theme = shinytheme("spacelab"),
  collapsible = TRUE,
  # Customized color code for specific text lines and options:
  tags$head(
    tags$style(HTML("
      body { background-color: white; color: black; }
      h2 { font-family: 'Calibri', sans-serif;  color: rgb(217,152,21); font-weight: bold; }
      h4 { font-family: 'Calibri', sans-serif;  color: black; font-weight: bold; }
      
      .option-recommended { color: ForestGreen; font-weight: bold; }
      .option-possible { color: YellowGreen; font-weight: bold; }
      .option-incompatible { color: rgb(220,0,0); text-decoration: line-through; }
      .option-neutral { color: black; }
      
      li { margin-left: 1em; }
}
    ")) # NOTE: Color names can be found on https://htmlcolorcodes.com/color-names/
  ),
  # Customized margins:
  style="margin:2vw; margin-top: 0vw; margin-bottom: 0vw; overflow: auto",
  
  # # We display the title:
  # h2("CARROT: Collecting and Analyzing Rhizodeposits - Reviewing and Optimizing Tool"),
  # # We display below the version of the software, defined at the beginning of the code:
  # h6(version, style = "font-weight: bold; font-style: italic"),
  
  # # We insert the logo, title and version of CARROT:
  # mainPanel(
  #   style = "height: 10vh; overflow-y: visible",
  #   layout_columns(
  #     col_widths = c(8, 4),
  #     # row_heights =c(1),
  #     # We insert the logo of CARROT:
  #     # card(img(src='CARROT_logo.png', align = "left", height="50%", width="50%")),
  #     card("Card 1"),
  #     card(
  #       # We display the title:
  #       h2("CARROT: Collecting and Analyzing Rhizodeposits - Reviewing and Optimizing Tool"),
  #       # # We display below the version of the software, defined at the beginning of the code:
  #       # h6(version, style = "font-weight: bold; font-style: italic"),
  #       ),
  #   ),
  # ),

  # We insert the logo of CARROT (containing the title and the version, to be changed each time):
  # img(src='CARROT_logo_and_text.png', align = "left", height="135px", width="675px"),
  mainPanel(
    # htmlOutput("picture"),
    uiOutput("picture"),
    ),
  
  # We set a horizontal spacing:
  p(""),

  # Defining the layout with a main panel and a side panel:
  sidebarLayout(
    
    # DISPLAYING THE MAIN PANEL ON THE LEFT:
    # --------------------------------------
    mainPanel(
      width = 6, # The width of the main panel. For fluid layouts this is out of 12 total units
      style = "height: 100vh; overflow-y: auto",
      
      # STARTING SCREEN (GROUP SELECTION)
      conditionalPanel(
        condition = "output.screen == 'start'",
        
        p("You first need to define where to start building your protocol."),
        p(""),
        wellPanel(
          h4("Please select a group of instructions:"),
          radioButtons("start_group", 
                       "",
                       choices = list(
                         "1: Scientific objectives" = 1,
                         "2: Plant growth conditions" = 2,
                         "3: Sampling strategy" = 3,
                         "4: Sample processing before analysis" = 4,
                         "5: Sample analysis" = 5
                       ),
                       selected=1),  
        ),
        actionButton("button_start", "START", class = "btn-primary btn-lg")
      ),
      
      # NEW GROUP (GROUP SELECTION)
      conditionalPanel(
        condition = "output.screen == 'move_to_next_group'",
        wellPanel(
          h4("To continue building your protocol, please select a new group of instructions:"),
          radioButtons("continue_group", 
                       "",
                       choiceNames = list("html", "text"),
                       choiceValues = c(1,2),
                       selected=1,
          ),
        ),
        actionButton("button_continue", "CONTINUE", class = "btn-primary btn-lg")
      ),
      
      # MAIN SCREEN (QUESTIONS)
      conditionalPanel(
        condition = "output.screen == 'working'",
        
        # We show the current group of instruction:
        uiOutput("group_indicator"), # Displays the current group
        hr(), # Displays an horizontal limit
        # We show a box to check if details about incompatible options are to be displayed:
        conditionalPanel(
          condition = "output.incompatible_options",
          style = "color: rgb(220,0,0); font-style: italic",
          checkboxInput("showing_incompatibility_details", "Show details about incompatible options")
        ),
        # We show the current instruction and the possible options:
        wellPanel(
          h4(textOutput("current_instruction_title")),
          br(),
          uiOutput("current_instruction_options")
        ),
        # We show the buttons at the bottom, i.e. Next, Skip and possibly Back:
        fluidRow(
          column(6,align = "left",
                 conditionalPanel(
                   condition = "output.backwards_option == 'Allowed'",
                   actionButton("button_back", "Go back", icon = icon("arrow-left"), width="150px"),
                 ),
                 actionButton("button_skip", "Skip this", icon = icon("forward"), width="150px")
          ),
          column(6, align = "right",
                 actionButton("button_next", "Confirm and go to the next instruction", class = "btn-primary")
          )
        )
      ),
      
      # END SCREEN:
      conditionalPanel(
        condition = "output.screen == 'all_groups_done'",
        h2("Congratulations!"),
        # We display the final text:
        htmlOutput("final_page_text"),
        p(""),
        p(""),
        p(""),
        # We add two buttons to RESUME or RESTART the program:
        fluidRow(
          column(6,align = "left",
                 actionButton("button_resume", "RESUME", class = "btn-primary"),
          ),
          column(6,align = "right",
                 actionButton("button_restart", "RE-START", class = "btn-primary"),
          ),
        ),
      ),
    ),
    
    # DISPLAYING A SIDEBAR ON THE RIGHT:
    #-----------------------------------
    sidebarPanel(
      
      width = 6,
      style = "height: 100vh; overflow-y: hidden;",

      tabsetPanel(
        tabPanel(
          # SHOWING A SUPPORTING TABLE:
          tags$b("Supporting Information"),  
          conditionalPanel(
            condition = "output.screen == 'working'",  
            style = "height: 80vh; overflow-y: auto; text-align: left;",
            p(""),
            # We display the text referring to the section in the article:
            uiOutput(outputId = "SI_table_reference"),
            p(""),
            # And we display the table:
            tableOutput("SI_table"),
            hr(),
          ),
        ),
        # SHOWING THE DOWNLOADING OPTIONS:
        tabPanel(
          tags$b("Updated protocol"),
          conditionalPanel(
            condition = "output.screen == 'working' & nrow(values$history) > 1",
            tabsetPanel(
              tabPanel("As table",
                       style = "height: 80vh; overflow-y: auto; text-align: left;",
                       h2(""),
                       downloadButton("download_protocol_csv", "Download the updated protocol (.csv)"),
                       downloadButton("download_protocol_excel", "Download the updated protocol (.xlsx)"),
                       h2(""),
                       DT::DTOutput("final_table"),
                       h2(""),
              ),
              tabPanel("As text",
                       style = "height: 80vh; overflow-y: auto; text-align: left;",
                       h2(""),
                       downloadButton("download_protocol_text", "Download the updated protocol (.txt)"),
                       h2(""),
                       h4("Full text:"),
                       h5(uiOutput("protocol_full_text")),
                       h2(""),
              ),
            ),
          ),
        ),
      )
    )
  )
)

######################################################################################################
######################################################################################################

# --- DEFINING THE SERVER (where calculations are done) ---

# The server function is run once each time a user visits the app.

server <- function(input, output, session) {
  
  # Input stores the current values of all of the widgets in the app. These values will be saved 
  # under the names that you gave the widgets in your ui.
  
  # Output contains variables that the UI can access. 
  # Each entry to output should contain the output of one of Shiny's render* functions. 
  # These functions capture an R expression and do some light pre-processing on the expression. 
  # Use the render* function that corrresponds to the type of reactive object you are making.
  # Each render* function takes a single argument: an R expression surrounded by braces, {}.
  # Define user specific objects inside server function, but outside of any render* calls. 
  # These would be objects that you think each user will need their own personal copy of. 
  # For example, an object that records the user's session information. This code will be run 
  # once per user. Only place code that Shiny must rerun to build an object inside 
  # of a render* function. Shiny will rerun all of the code in a render* chunk each time a user 
  # changes a widget mentioned in the chunk.
  
  # 1) DEFINITION OF REACTIVE VALUES
  ##################################
  
  # Stores the values of the current application:
  values <- reactiveValues(
    screen = "start",                # Either 'start', 'move_to_next_group", 'working', or 'finished'
    decision_tree = data.frame(),    # Dataframe representing the Decision tree
    information_tree = data.frame(), # Dataframe representing the Information tree
    current_row = 4,                 # Current row in the Decision tree
    max_row = 126,                   # Maximal row in the Decision tree
    SI_table_name = "Undocumented",  # Name of the worksheet in which the SI table should be looked at
    history = data.frame(),          # Dataframe recording the user choices
    current_group_start=4,           # The row index corresponding to the start of the current group of instructions
    current_group_end = 4,           # The row index corresponding to the end of the current group of instructions
    incompatible_pending = NULL,     # Used to define an incompatible choice
    current_instruction_options = "",# Used to store the full options to be downloaded
    incompatible_options = FALSE,    # Used to display a button for allowing the user to get more details about incompatible options
    protocol_full_text = "",         # Full text of the protocol to be displayed
    last_text_added = "",            # Text that is added to the full protocol text at every choice made
    backwards_option = "Disabled",   # Either "Allowed" or "Disabled"
    current_group_ID = 1,            # Used to position the radio button option to the last grou^p selected
    visited_groups_id = list(),      # List containing the ID number of the groups of instructions already visisted
  )
  
  # 2) INITIALIZATION OF OUTPUT VALUES
  ####################################
  
  # The screen is either displayed or not:
  output$screen <- reactive({ values$screen })
  outputOptions(output, "screen", suspendWhenHidden = FALSE) # This allow to use this as a condition in a conditionalPanel, even if the output is not shown
  # We initialize the condition allowing the button "Back" to be displayed:
  output$backwards_option = reactive({ values$backwards_option })
  outputOptions(output, "backwards_option", suspendWhenHidden = FALSE) # This allow to use this as a condition in a conditionalPanel, even if the output is not shown
  # We initialize the condition allowing the button "Back" to be displayed:
  output$incompatible_options = reactive({ values$incompatible_options })
  outputOptions(output, "incompatible_options", suspendWhenHidden = FALSE) # This allow to use this as a condition in a conditionalPanel, even if the output is not shown
  # We initialize the current group ID number:
  output$current_group_ID = reactive({ values$current_group_ID })
  outputOptions(output, "current_group_ID", suspendWhenHidden = FALSE) # This allow to use this as a condition in a conditionalPanel, even if the output is not shown
  
  output$download_protocol_text = reactive({ values$protocol_full_text })
  outputOptions(output, "download_protocol_text", suspendWhenHidden = FALSE)
  
  # --- LOADING THE DATA ---
  #-----------------------------------------------------------------------------------------------------
  # We first define a function for loading the decision tree and the information tree:
  load_data <- function(file_path, # Explicit path (ex: "D:/Documents/")
                        download_from_url=FALSE, # If TRUE, activates a way to download and convert the file into an Excel file without expliciting a recording of the dowloaded file
                        csv_file=FALSE, # If TRUE, read.csv() will be used instead of read_excel()
                        worksheet="Decision tree", # Name of the worksheet to load in the Excel file
                        replace_this_character_by_white_space="", # A character to be replaced by white space (necessary for a proper concatenation of strings)
                        header=FALSE # Whether the first line corresponds to the header or not
  ){
    
    if (download_from_url) {
      temp_file <- tempfile()
      download.file(url = file_path, destfile = temp_file, mode = "wb", quiet = TRUE)
      workbook <- loadWorkbook(temp_file)
      raw_data <- readWorkbook(workbook, sheet = worksheet, colNames=header, na.strings = "NA")
      raw_data <- as.data.frame(raw_data)
    } else {
      # We read the worksheet in the Excel file:
      if (csv_file) {
        raw_data <- read.csv(file_path, header= header, sep=",", dec=".")
      } else {
        raw_data <- read_excel(file_path, sheet = worksheet, col_names = header, na="NA", col_types="text",
                               trim_ws=FALSE)
      }
      
      raw_data <- as.data.frame(raw_data)
    }
    # We replace a special character by a white space in all columns if necessary:
    if (replace_this_character_by_white_space != "") {
      raw_data <- raw_data %>%
        select(everything()) %>%
        dplyr::mutate_if(is.character, stringr::str_replace_all,
                         pattern = replace_this_character_by_white_space, replacement = " ")
    }
    return(raw_data)
  }
  #----------------------------------------------------------------------------------------------------
  
  #----------------------------------------------------------------------------------------------------
  # We attempt to load the initial data in the decision tree and in the information tree:
  # dt_file_path = paste0(folder_path, "CARROT_decision_tree.csv")
  # it_file_path = paste0(folder_path, "CARROT_information_tree.csv")
  main_file_path = paste0(folder_path, "CARROT.xlsm")

  tryCatch({
    # decision_tree_df <- load_data(dt_file_path,
    #                               csv_file = TRUE)
    decision_tree_df <- load_data(main_file_path,
                                  worksheet="Decision tree")
    values$decision_tree <- decision_tree_df
    
    # information_tree_df <- load_data(it_file_path,
    #                                  csv_file = TRUE,
    #                                  # Because the stupid read_excel function removes cells with white spaces only,
    #                                  # we have converted all white spaces in the original file and now need to convert those back
    #                                  # to white spaces:
    #                                  replace_this_character_by_white_space="_")
    information_tree_df <- load_data(main_file_path,
                                     worksheet="Information tree",
                                     # Because the stupid read_excel function removes cells with white spaces only,
                                     # we have converted all white spaces in the original file and now need to convert those back
                                     # to white spaces:
                                     replace_this_character_by_white_space="_")
    values$information_tree <- information_tree_df
  }, error = function(e) {
    showNotification("ERROR: the decision and information trees could not be loaded properly. Please check the path!", type = "error", duration = NULL)
    showNotification(e$message, duration = NULL)
    
  })
  #--------------------------------------------------------------------------------------------------
  
  #----------------------------------------------------------------------------------------------------
  # We define the Supporting Table to be displayed for the current choice.
  SI_file_path = paste0(folder_path, "CARROT_SI.xlsx")
  # SI_file_path = getURL(paste0(folder_path, "CARROT_SI.xlsx"))
  output$SI_table <- renderTable(
    # We load the datatable with the custom-made function 'load_data':
    df <- head(load_data(SI_file_path,
                         # We target the proper worksheet:
                         worksheet = as.character(values$information_tree[[values$current_row, 3]]),
                         # We allow the first line to be considered as the column names:
                         header=TRUE),
               # And we remove the last line corresponding to the link to the article using "head":
               -1) %>%
      # We need to replace the text in the cells so that items are shown in bullet points:
      select(everything()) %>%
      # We replace the first occurence of "o " at the beginning of each cell, starting at the third column,
      # so that a first bullet point in HTML - i.e., <li> - is introduced at the beginning of such cell:
      dplyr::mutate(across(3:last_col(), stringr::str_replace,
                           pattern = "o ", replacement = "<li>")) %>%
      # We replace a line break "\n" follow by "o " by the start of a new one in HTML, i.e. "<li>":
      dplyr::mutate_if(is.character, stringr::str_replace_all,
                       pattern = "\no ", replacement = "<li>") %>%
      # # We add a last <li> at the end of each cell:
      # dplyr::mutate(across(3:last_col(), ~ paste(.x, "<li>"))) %>%
      # We replace the text in the first two columns so that it appears as bold:
      dplyr::mutate(across(1:2, function(x) {return( paste("<b>",x,"<b>") )})),
    
    colnames = TRUE,
    bordered = TRUE,
    striped = TRUE,
    hover = TRUE,
    spacing = "s", # The spacing between the rows of the table (xs stands for "extra small", s for "small", m for "medium" and l for "large")
    align = "l", # If equal to 'l', 'c' or 'r', then all columns will be, respectively, left-, center- or right-aligned.
    na = "NA",
    sanitize.text.function=identity, # Allow to use HTML format in the text within the table
  )
  
  # We also extract the reference text which is located on the last line of the previous table: 
  output$SI_table_reference <- renderText(
    # We will create a HTML text in italic:
    HTML(paste0(
      "<i>",
      # We load the datatable with the custom-made function 'load_data':
      as.character(load_data(SI_file_path,
                             # We target the proper worksheet:
                             worksheet = as.character(values$information_tree[[values$current_row, 3]]),
                             header=TRUE) %>%
                     # We select the last line only:
                     slice(n()) %>%
                     # We select the third column only:
                     select(3)
      ),
      "<i>")
    )
  )
  #----------------------------------------------------------------------------------------------------
  
  #----------------------------------------------------------------------------------------------------
  # We detect automatically the row indices corresponding to the distinct groups of instructions:
  groups_ranges <- list()
  row_index <- 4            # Initialization of the row index in the Decision Tree
  starting_index <- 4       # Initialization of the starting row index for the current group
  list_index <- 1           # Initialization of the index in the final list
  df <- decision_tree_df
  # We initialize the group instruction:
  current_group <- df[row_index,1]
  previous_group <- df[row_index,1]
  # For each row in the Decision tree:
  while (row_index <= length(df) + 1) {
    # If the new group instruction is different from the previously recorded one:
    if (current_group != previous_group) {
      # Then we define the ending index of the previous instruction:
      ending_index <- row_index - 1
      # We add a new item in the list of groups ranges:
      groups_ranges[[list_index]] <- c(starting_index, ending_index)
      list_index <- list_index + 1
      # We define the staring index of the next group, and assign the previous group to this next group:
      starting_index <- row_index
      previous_group <- current_group
    }
    # In any case, we move to the next row:
    row_index <- row_index + 1
    if (row_index <= length(df)) {
      current_group <- df[row_index,1]
    } else {
      # If the next group instruction cannot be accessed, we define it as "END":
      current_group <- "END"
    }
  }
  # Eventually, we record these ranges in the reactive values:
  values$groups_ranges <- groups_ranges
  #----------------------------------------------------------------------------------------------------
  
  # 3) CREATING INTERMEDIATE FUNCTIONS:
  #####################################
  
  #----------------------------------------------------------------------------------------------------
  # CURRENT BLOCK - We analyze the block corresponding to the current instruction:
  current_block <- reactive({
    
    # We first ensure that values are available 
    # - if not, the operation is stopped by raising a "silent" exception:
    req(values$decision_tree, values$current_row <= values$current_group_end)
    # We access the dataframe:
    df <- values$decision_tree
    # We define the current row index:
    row_idx <- values$current_row
    # We read the instruction to be displayed:
    instruction_txt <- as.character(df[[2]][row_idx])
    if(is.na(instruction_txt)) return(NULL)
    
    # Slicing up to the end of the current group of instructions:
    col_2_slice <- df[[2]][row_idx:values$current_group_end]
    # We look for the next instruction, i.e. the first line where the text differs:
    is_same <- col_2_slice == instruction_txt
    is_same[is.na(is_same)] <- FALSE
    first_diff <- which(!is_same)[1]
    # The length of the block of the current instruction is computed:
    if (is.na(first_diff)) {
      block_len <- length(col_2_slice)
    } else {
      block_len <- first_diff - 1
    }
    # The list of actual indices corresponding to the options of the current instruction are computed:
    list_of_row_indices <- row_idx:(row_idx + block_len - 1)
    # The text of the different options is defined as a vector:
    options_text <- as.character(df[[3]][list_of_row_indices])
    
    # We return a list containing:
    return(list(
      # The text corresponding to the instruction:
      instruction = instruction_txt,
      # The indices corresponding to the options of this instructiong:
      indices = list_of_row_indices,
      # The vector containing the text of each option of the instruction:
      options = options_text
    ))
  })
  #----------------------------------------------------------------------------------------------------
  
  #----------------------------------------------------------------------------------------------------
  # COMPATIBILITY -We define the compatibility of each option with previous choices:
  options_with_compatibility <- reactive({
    
    # We initialize variables:
    req(current_block())
    block <- current_block()
    hist <- values$history
    df <- values$decision_tree
    
    # We initialize two lists for informing about the compatibility:
    status_list <- character(length(block$indices))
    conflict_list <- character(length(block$indices))
    
    # If no previous choices have been defined, everything is considered neutral:
    if (nrow(hist) == 0) {
      # We return a dataframe containing:
      return(data.frame(
        # The text of the options to be displayed:
        text = block$options,
        # The indices:
        index = block$indices,
        # The compatibility status defined as neutral:
        status = "Neutral",
        # The conflict defined as empty:
        conflict = "",
        stringsAsFactors = FALSE
      ))
    }
    
    # We initialize a counter corresponding to the number of the current options that are incompatible with previous choices:
    number_of_incompatible_options <- 0
    
    # We loop over each possible option of the current instruction:
    for (i in seq_along(block$indices)) {
      
      current_row_index <- block$indices[i]
      # We initialize a vector containing the number of Recommended, Possible and Incompatible options:
      counts <- c(Rec = 0, Poss = 0, Incomp = 0)
      # We initialize a vector that will contain the incompatible previous choices:
      conflicts <- c()
      
      # We go through each previous choices made by the user, by exploring the history:
      for (h in seq_len(nrow(hist))) {
        
        prev_col <- hist$Col_Index[h]
        if (is.na(prev_col) || prev_col < 0) next # If there was no previous choice, we go to the next choice:
        
        # We read the compatibility at the row corresponding to the current option and at the column corresponding to the previous choice:
        val <- as.character(df[current_row_index, prev_col])
        if (is.na(val)) val <- "NA"
        # The counts vector is incremented according to the compatibility that has been read:
        if (val == "Fully compatible") counts["Rec"] <- counts["Rec"] + 1
        if (val == "Possible but odd") counts["Poss"] <- counts["Poss"] + 1
        if (val == "Incompatible") {
          counts["Incomp"] <- counts["Incomp"] + 1
          # In case of incompatibility, we also add the previous choice in the conflicts vector:
          conflicts <- c(conflicts, hist$Choice[h])
        }
      }
      # After covering each previous choice, we update the lists of status and conflicts:
      if (counts["Incomp"] > 0) {
        status_list[i] <- "Incompatible"
        conflict_list[i] <- paste(conflicts, collapse = "; ")
        number_of_incompatible_options <- number_of_incompatible_options + 1
      } else if (counts["Poss"] > 0) {
        status_list[i] <- "Possible"
      } else if (counts["Rec"] > 0) {
        status_list[i] <- "Recommended"
      } else {
        status_list[i] <- "Neutral"
      }
    }
    
    # In case there is at least one incompatible option to be shown:
    if (number_of_incompatible_options > 0) {
      # We record this information in the values, so that the possibility of showing more details is displayed:
      values$incompatible_options <- TRUE
    } else {
      values$incompatible_options <- FALSE
    }
    
    # If all options have been considered incompatible for the current instruction, a special event will be triggered later on:
    if (number_of_incompatible_options == length(block$indices)) {
      
      block <- current_block()
      # We define the next row index after the current block of instruction:
      next_row <- max(block$indices) + 1
      # We update the current row and verify that this index does not correspond to the end of the current group of instruction:
      check_end(next_row)
      # We document the text of the protocol:
      values$last_text_added = " [Instruction skipped automatically] "
    }
    
    # We finally return a dataframe containing:
    return(data.frame(
      # The text of the options to be displayed:
      text = block$options,
      # The indices:
      index = block$indices,
      # The compatibility status:
      status = status_list,
      # The list of previous choices in conflict:
      conflict = conflict_list,
      stringsAsFactors = FALSE
    ))
  })
  #----------------------------------------------------------------------------------------------------
  
  #---------------------------------------------------------------------------------------------------
  # PROTOCOL TEXT - We define a function for extracting the text corresponding to the full text protocol:
  get_protocol_text <- function(index) {
    # We consider the first string piece:
    element <- values$information_tree[[index, 4]]
    if (is.na(element)) {
      element <- ""
    }
    new_text <- element
    # We add the middle string piece:
    element <- values$information_tree[[index, 5]]
    if (is.na(element)) {
      element <- ""
    }
    new_text <- HTML(paste0(new_text, element))
    # We add the final string piece:
    element <- values$information_tree[[index, 6]]
    if (is.na(element)) {
      element <- ""
    }
    new_text <- paste0(new_text, element)
    # We record this new piece of text, in case we need to remove it when clicking on the "Back" button:
    values$last_text_added <- HTML(new_text)
    
  }
  #---------------------------------------------------------------------------------------------------
  
  #----------------------------------------------------------------------------------------------------
  # CHECKING - We create a function for verifying whether we arrive at the end of a group of instruction:
  check_end <- function(next_r) {
    # If the next row index is higher than the maximal row number of the group:
    if (next_r > values$current_group_end) {
      # We check whether all groups have been covered or not, using the list of group ID that has been stored:
      current_list <- values$visited_groups_id
      expected_list <- list(1,2,3,4,5)
      # We test whether the two lists contain the same items or not, regardless of the order:
      if (identical(sort(unlist(current_list)), sort(unlist(expected_list)))) {
        # Then we switch the screen in order to display the final screen:
        values$screen <- "all_groups_done"
      } else {
        # Then we switch the screen in order to display the instructions for the next group:
        values$screen <- "move_to_next_group"
      }
      # In both cases, we update the corresponding radioButton to display already-examined groups in grey.
      # We define the normal list to be displayed:
      list_of_options <- list("1: Scientific objectives",
                              "2: Plant growth conditions",
                              "3: Sampling strategy",
                              "4: Sample processing before analysis",
                              "5: Sample analysis")
      # We initialize the new list to be displayed with the HTML code:
      new_HTML_list <- list_of_options
      # We cover each group:
      for (ID in seq(1,5)) {
        # If the group corresponding to the current ID has been visited:
        if (ID %in% values$visited_groups_id) {
          special_format <- paste0("<p style='color: grey; margin-bottom:0; padding-top:0;'</p>",list_of_options[[ID]])
          new_HTML_list[[ID]] <- HTML(special_format)
        } else {
          special_format <- paste0("<p style='color: black; margin-bottom:0; padding-top:0;'</p>",list_of_options[[ID]])
          new_HTML_list[[ID]] <- HTML(special_format)
        }
      }
      updateRadioButtons(session, "continue_group",
                         # tags$style(HTML("line-height:3;")),
                         choiceNames = new_HTML_list, 
                         choiceValues = c(1,2,3,4,5),
                         selected=values$current_group_ID)
      
      # Otherwise, we continue the current group of instruction and just move to the next one
      # by assigning the current row to the next one that had been computed:
    } else {
      values$current_row <- next_r
    }
  }
  #----------------------------------------------------------------------------------------------------
  
  #----------------------------------------------------------------------------------------------------
  # SAVING - We define a function that saves the current decision and move to the next instruction:
  save_step <- function(row_idx, user_choice) {
    
    # df <- values$decision_tree
    df <- values$decision_tree
    instruction <- as.character(df[row_idx, 2])
    
    # The column containing the instructions is recorded:
    instructions <- as.character(df[2, ])
    # The column containing possible choices is recorded:
    choices <- as.character(df[3, ])
    
    # We look for the column index matching the choice of the user:
    match_col <- which(choices == user_choice & instructions == instruction)
    final_col <- if(length(match_col) > 0) match_col[1] else -1
    
    # 2. We define a new entry for the history dataframe containing the choice of the user:
    new_entry <- data.frame(
      Group_ID = values$current_group_ID,
      Group = as.character(df[row_idx, 1]),
      Instruction = instruction,
      Options = writing_options_as_text(),
      Choice = user_choice,
      Row_Index = row_idx,
      Col_Index = final_col,
      Protocol_text = values$last_text_added,
      stringsAsFactors = FALSE
    )
    # We combine the entry with the history dataframe:
    values$history <- rbind(values$history, new_entry)
    # We update the full text of the protocol by concatenating the whole corresponding column in 'history'
    # after arranging the rows according to the ascending order of Group_ID:
    arranged_history <- values$history %>% arrange(Group_ID)
    values$protocol_full_text <- HTML(paste0(arranged_history$Protocol_text))
    output$protocol_full_text <-  renderUI({values$protocol_full_text})
    # We define the next row index after the current block of instruction:
    block <- current_block()
    next_row <- max(block$indices) + 1
    # We verify that this index does not correspond to the end of the current group of instruction.
    # If so, the screen is changing. If not, the current row is updated with "next_row" by the function check_end,
    # effectively moving to the next instruction:
    check_end(next_row)
  }
  #----------------------------------------------------------------------------------------------------
  
  # 4) RENDERING OUTPUTS
  #######################
  
  #----------------------------------------------------------------------------------------------------
  # The current instruction is recorded for being displayed in the UI:
  output$current_instruction_title <- renderText({
    req(current_block())
    current_block()$instruction
  })
  #----------------------------------------------------------------------------------------------------
  
  #----------------------------------------------------------------------------------------------------
  # The options to be displayed are now defined:
  output$current_instruction_options <- renderUI({
    
    # We define the options compatibility (as described above):
    req(options_with_compatibility())
    opts <- options_with_compatibility()
    
    # 1. The indices are transformed as a character vector:
    ids <- as.character(opts$index)
    
    # 2. The labels to be displayed in HTML are now defined:
    html_labels <- lapply(seq_len(nrow(opts)), function(i) {
      # NOTE: We used lapply to treat each line without index error.
      # The text of the option is defined:
      txt <- as.character(opts$text[i])
      # The compatibility of the option is defined:
      stat <- as.character(opts$status[i])
      # The conflicts with previous choices is defined:
      conflict <- as.character(opts$conflict[i])
      
      # We choose a CSS class:
      css_class <- switch(stat,
                          "Recommended" = "option-recommended",
                          "Possible" = "option-possible",
                          "Incompatible" = "option-incompatible",
                          "option-neutral") # option_neutral is the default value
      
      # We choose a prefix to be displayed before the option itself:
      prefix <- switch(stat,
                       "Recommended" = "[RECOMMENDED] ",
                       "Possible" = "[POSSIBLE] ",
                       "Incompatible" = "[INCOMPATIBLE] ",
                       "") # "" is the default value
      
      # We build the HTML string corresponding to the option to be displayed: 
      label_str <- paste0("<span class='", css_class, "'>", i, ": ", prefix, txt, "</span>")
      
      # We add the detail of incompatible previous choices if necessary:
      if (stat == "Incompatible" & input$showing_incompatibility_details == TRUE) {
        label_str <- paste0(label_str, "<br><small style='color:rgb(220,0,0)'> (Incompatible with: ", conflict, ")</small>")
      }
      
      # We return the HTML string to be displayed for the option:
      return(HTML(label_str))
    })
    
    # 3. We create the buttons for choosing the options (NOTE: unname() is used to avoid troubles)
    radioButtons("user_selection", label = NULL, 
                 choiceNames = unname(html_labels),
                 choiceValues = unname(ids))
    
  })
  #----------------------------------------------------------------------------------------------------
  
  #----------------------------------------------------------------------------------------------------
  # We record the options for later saving them in the history table:
  writing_options_as_text <- function() {
    
    # We define the options compatibility (as described above):
    req(options_with_compatibility())
    opts <- options_with_compatibility()
    
    # We initialize the final text:
    all_options_text = ""
    
    # The text is defined for each possible option:
    for (i in seq_len(nrow(opts))) {
      
      # The text of the option is defined:
      option_text <- as.character(opts$text[i])
      # The compatibility of the option is defined:
      option_status <- as.character(opts$status[i])
      # The conflicts with previous choices is defined:
      conflict <- as.character(opts$conflict[i])
      
      # We define a class according to the status of compatibility:
      css_class <- switch(option_status,
                          "Recommended" = "option-recommended",
                          "Possible" = "option-possible",
                          "Incompatible" = "option-incompatible",
                          "option-neutral") # option_neutral is the default value
      
      # We define a prefix to display according to the status of compatibility:
      prefix <- switch(option_status,
                       "Recommended" = "[RECOMMENDED] ",
                       "Possible" = "[POSSIBLE] ",
                       "Incompatible" = "[INCOMPATIBLE] ",
                       "") # "" is the default value
      
      # We build the complete text corresponding to the option to be displayed and add it to the previous text: 
      all_options_text <- paste0(all_options_text, prefix, option_text, "\n")
      
      # We add the detail of incompatible previous choices if necessary:
      if (option_status == "Incompatible") {
        all_options_text <- paste0(all_options_text, "   > Incompatible with: ", conflict, "\n")
      }
    }
    
    return (all_options_text)
  }
  #----------------------------------------------------------------------------------------------------
  
  # 5) REACTING TO THE USER ACTION
  ################################
  
  #----------------------------------------------------------------------------------------------------
  # STARTING BY DEFINING THE FIRST GROUP OF INSTRUCTION:
  # We define what happens after clicking on Start:
  observeEvent(input$button_start, {
    
    # We get the ID number of the group of instruction chosen by the user:
    current_group_ID <- as.numeric(input$start_group)
    values$current_group_ID <- current_group_ID
    # If this ID number has not been included in the list 'visited_groups_id' so far, we add it:
    if (!(current_group_ID %in% values$visited_groups_id)) {
      values$visited_groups_id <- append(values$visited_groups_id, current_group_ID)
    }
    
    # We get the starting and ending row index for the instructions corresponding to this group:
    ranges <- values$groups_ranges[[as.character(current_group_ID)]]
    ranges <- values$groups_ranges[[current_group_ID]]
    values$current_row <- ranges[1]
    values$current_group_start <- ranges[1]
    values$current_group_end <- ranges[2]
    # We allow showing the screen:
    values$screen <- "working"
    
    # We initialize the history table, e.g. with Group, Instruction, Choice:
    values$history <- data.frame(
      Group_ID = numeric(),
      Group = character(),
      Instruction = character(),
      Options = character(),
      Choice = character(),
      Row_Index = numeric(),
      Col_Index = numeric(),
      Protocol_text = character(),
      stringsAsFactors = FALSE
    )
    
    # And we forbid using the "Back" button at the next step:
    values$backwards_option <- "Disabled"
  })
  #----------------------------------------------------------------------------------------------------
  
  #----------------------------------------------------------------------------------------------------
  # CONTINUING WITH A NEW GROUP OF INSTRUCTIONS:
  # We define what happens after clicking on Continue:
  observeEvent(input$button_continue, {
    
    # We get the ID number of the group of instruction chosen by the user:
    current_group_ID <- as.numeric(input$continue_group)
    values$current_group_ID <- current_group_ID
    # If this ID number has not been included in the list 'visited_groups_id' so far:
    if (!(current_group_ID %in% values$visited_groups_id)) {
      # We add it:
      values$visited_groups_id <- append(values$visited_groups_id, current_group_ID)
      # We get the starting and ending row index for the instructions corresponding to this group:
      ranges <- values$groups_ranges[[current_group_ID]]
      values$current_row <- ranges[1]
      values$current_group_start <- ranges[1]
      values$current_group_end <- ranges[2]
      # We now allow moving to another type of screen:
      values$screen <- "working"
      # And we forbid using the "Back" button at the next step:
      values$backwards_option <- "Disabled"
    } else {
      # We create a dialog box to confirm the choice of revisiting the group:
      showModal(modalDialog(
        title = "WATCH OUT! This group of instructions has already been covered. Revisiting it will erase the previous answers. Do you wish to proceed?",
        footer = tagList(
          modalButton("Cancel"),
          actionButton("button_confirm_revisiting", "Confirm", class = "btn-danger")
        )
      ))
    }
  })
  
  # CONFIRM REVISITING A GROUP: In case the user has decided to revisit a group previously examined:
  observeEvent(input$button_confirm_revisiting, {
    # We remove the Dialog Box:
    removeModal()
    # If the user confirms this, we erase the answers corresponding to the group that is being revisited from the history table:
    values$history <- values$history %>%
      filter(Group_ID != values$current_group_ID)
    # We update the full text of the protocol by concatenating the whole corresponding column in 'history'
    # after arranging the rows according to the ascending order of Group_ID:
    arranged_history <- values$history %>% arrange(Group_ID)
    values$protocol_full_text <- HTML(paste0(arranged_history$Protocol_text))
    output$protocol_full_text <-  renderUI({values$protocol_full_text})
    # We get the starting and ending row index for the instructions corresponding to this group:
    ranges <- values$groups_ranges[[values$current_group_ID]]
    values$current_row <- ranges[1]
    values$current_group_start <- ranges[1]
    values$current_group_end <- ranges[2]
    # We now allow moving to another type of screen:
    values$screen <- "working"
    # And we forbid using the "Back" button at the next step:
    values$backwards_option <- "Disabled"
  })
  #----------------------------------------------------------------------------------------------------

  #----------------------------------------------------------------------------------------------------
  # NEXT: We set up the action following the click on the "Next" button (i.e. Confirm):
  observeEvent(input$button_next, {
    req(input$user_selection)
    # We define the index corresponding to the user choice:
    selected_idx <- as.numeric(input$user_selection)
    # We define the compatibility of this choice with previous choices:
    opts <- options_with_compatibility()
    # We define the text of the final choice:
    selected_opt <- opts[opts$index == selected_idx, ]
    
    # If the choice of the user is incompatible with previous choices:
    if (selected_opt$status == "Incompatible") {
      values$incompatible_pending <- selected_opt
      # We create a dialog box to confirm the choice:
      showModal(modalDialog(
        title = "WATCH OUT!",
        HTML(paste0("You have selected an option that is incompatible with previous choices :<br><b>", 
                    selected_opt$conflict, "</b><br><br>Do you really want to select this option?")),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("button_confirm_incompatible", "Confirm", class = "btn-danger")
        )
      ))
      # Else, if the choice of the user is compatible with previous choices:
    } else {
      # We set the text corresponding to the protocol to be displayed 
      # (values$last_text_added is updated):
      get_protocol_text(selected_idx)
      # We save the present choice, update the items of values, and finally go to the next step:
      save_step(selected_idx, selected_opt$text)
    }
    
    # We allow to display the "Back" button at the next step:
    output$backwards_option <- reactive({"Allowed"})
    
  })
  
# CONFIRM INCOMPATIBLE: In case the user has selected an incompatible choice and confirmed his/her choice:
  observeEvent(input$button_confirm_incompatible, {
    # We remove the Dialog Box:
    removeModal()
    req(values$incompatible_pending)
    # We set the text corresponding to the protocol to be displayed 
    # (values$last_text_added is updated):
    get_protocol_text(values$incompatible_pending$index)
    # We save the present choice, update the items of values, and finally go to the next step:
    save_step(values$incompatible_pending$index, values$incompatible_pending$text)
    # We reset the incompatible pending table:
    values$incompatible_pending <- NULL
  })
  #----------------------------------------------------------------------------------------------------
  
  #----------------------------------------------------------------------------------------------------
  # SKIP: We define what happens after clicking on the "Skip" button:
  observeEvent(input$button_skip, {
    
    # We define the current block:
    block <- current_block()
    # We make a special entry in the history table for the case where the instruction has been skipped:
    new_entry <- data.frame(
      Group_ID = values$current_group_ID,
      Group = as.character(values$decision_tree[block$indices[1], 1]),
      Instruction = block$instruction,
      Options = writing_options_as_text(),
      Choice = "[Skip this instruction]",
      Row_Index = values$current_row,
      Col_Index = values$current_row,
      Protocol_text = " [Instruction skipped] ",
      stringsAsFactors = FALSE
    )
    # We combine the entry with the history dataframe:
    values$history <- rbind(values$history, new_entry)
    # We reset the protocol full text:
    values$last_text_added <- " [Instruction skipped] "
    # We update the concatenated text corresponding to the protocol:
    values$protocol_full_text <- HTML(paste0(values$history$Protocol_text))
    output$protocol_full_text <-  renderUI({values$protocol_full_text})
    # We define the next row index after the current block of instruction:
    next_row <- max(block$indices) + 1
    # We verify that this index does not correspond to the end of the current group of instruction:
    check_end(next_row)
  })
  #----------------------------------------------------------------------------------------------------
  
  #----------------------------------------------------------------------------------------------------
  # BACK: We define what happens after clicking on "Back":
  observeEvent(input$button_back, {
    
    # We need to remove the last entry in the history (if any), and to update the row index.
    if(nrow(values$history) > 0) {
      
      # We get the last entry in the history table:
      last_entry <- tail(values$history, 1)
      if (last_entry$Row_Index > 0) {
        # We need to move back to the row index of the beginning of the instruction:
        target_row <- last_entry$Row_Index
        curr_inst <- as.character(values$decision_tree[target_row, 2])
        # We move backwards until the instruction is changed
        while(target_row > 1 && as.character(values$decision_tree[target_row - 1, 2]) == curr_inst) {
          target_row <- target_row - 1
        }
        # And we finally update the row index
        values$current_row <- target_row
      } else {
        # NOTE: this case should not happen!
        showNotification("WATCH OUT: A problem was encountered when trying to move back!", type="warning")
      }
      # We update the current block:
      block <- current_block()
      
      # We update the history table by removing the last line:
      values$history <- head(values$history, -1)
      
      # We update the concatenated text corresponding to the protocol, after arranging the rows according to the ascending order of Group_ID:
      arranged_history <- values$history %>% arrange(Group_ID)
      values$protocol_full_text <- HTML(paste0(arranged_history$Protocol_text))
      output$protocol_full_text <- renderUI({values$protocol_full_text})
      # We update the last text added:
      values$last_text_added <-  ""
      
      # We verify that the new row is not equal to the first row of the current group of instruction.
      if (values$current_row == values$current_group_start) {
        # If so, we forbid using the "Back" button at the next step:
        values$backwards_option <- "Disabled"
        output$backwards_option <- reactive({"Disabled"})
      }
    }
  })
  #----------------------------------------------------------------------------------------------------
  
  #----------------------------------------------------------------------------------------------------
  # RESTART FROM THE BEGINNING:
  # We define what happens after clicking on RESTART:
  observeEvent(input$button_restart, {
    # We show a warning message, which needs to be confirmed before restarting:
    showModal(modalDialog(
      title = "WATCH OUT! You are about to reinitialize the program. Restarting will erase all previous answers. Do you wish to proceed?",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("button_confirm_restarting", "Confirm", class = "btn-danger")
      )
    ))
  })
  # CONFIRM RESTARTING FROM THE BEGINNING: In case the user has decided to restart and confirmed it:
  observeEvent(input$button_confirm_restarting, {
    # We remove the Dialog Box:
    removeModal()
    # We now allow moving to another type of screen:
    values$screen <- "start"
    # We reinitialize the list of group ID already visited:
    values$visited_groups_id <- list()
    # We reset the history and other variables:
    values$history <- data.frame()
    values$incompatible_pending <- NULL
    values$incompatible_options <- FALSE
    values$protocol_full_text <- ""
    values$last_text_added <- ""
    values$current_group_ID = 1
    # And we forbid using the "Back" button at the next step:
    values$backwards_option <- "Disabled"
  })
  #----------------------------------------------------------------------------------------------------
  
  #----------------------------------------------------------------------------------------------------
  # RESUME THE EXPLORATION:
  # We define what happens after clicking on RESUME:
  observeEvent(input$button_resume, {
    
    showModal(modalDialog(
      title = "WATCH OUT! You are about to resume the program. Resuming will allow you to reconsider any group of instructions while keeping all previous answers. Do you wish to proceed?",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("button_confirm_resuming", "Confirm", class = "btn-danger")
      )
    ))
  })
  # CONFIRM RESUMING: In case the user has decided to resume and confirmed it:
  observeEvent(input$button_confirm_resuming, {
    # We remove the Dialog Box:
    removeModal()

    # # IF WE WANT TO RESET ALL GROUPS:
    # # We reinitialize the list of group ID already visited:
    # values$visited_groups_id <- list()
    # # And we update the corresponding radioButton to display already-examined groups in grey.
    # # We define the normal list to be displayed:
    # list_of_options <- list("1: Scientific objectives",
    #                         "2: Plant growth conditions",
    #                         "3: Sampling strategy",
    #                         "4: Sample processing before analysis",
    #                         "5: Sample analysis")
    # # We initialize the new list to be displayed with the HTML code:
    # new_HTML_list <- list_of_options
    # # We cover each group:
    # for (ID in seq(1,5)) {
    #   # If the group corresponding to the current ID has been visited:
    #   if (ID %in% values$visited_groups_id) {
    #     special_format <- paste0("<p style='color: grey; margin-bottom:0; padding-top:0;'</p>",list_of_options[[ID]])
    #     new_HTML_list[[ID]] <- HTML(special_format)
    #   } else {
    #     special_format <- paste0("<p style='color: black; margin-bottom:0; padding-top:0;'</p>",list_of_options[[ID]])
    #     new_HTML_list[[ID]] <- HTML(special_format)
    #   }
    # }
    # updateRadioButtons(session, "continue_group",
    #                    # tags$style(HTML("line-height:3;")),
    #                    choiceNames = new_HTML_list, 
    #                    choiceValues = c(1,2,3,4,5),
    #                    selected=values$current_group_ID)
    
    # OTHERWISE, ONLY ONE GROUP WILL BE VISITED BEFORE DISPLAYING AGAIN THE FINAL PAGE WITH THE OPTION RESUME
    
    # We now allow moving to another type of screen:
    values$screen <- "move_to_next_group"
    # And we forbid using the "Back" button at the next step:
    values$backwards_option <- "Disabled"
  })
  #----------------------------------------------------------------------------------------------------
  
  # 6) OPTIONAL DISPLAY
  #####################
  
  # Importing an external image as the logo:
  output$picture <-
    # renderText({
    #   c(
    #     '<img src="',
    #     "https://github.com/frees86/carrot/blob/main/source/CARROT_logo_and_text.png",
    #     '">'
    #   )
    # })
    renderUI({
      # This will read the corresponding file in the folder 'www' by default:
      img(src = "CARROT_logo_and_text.png", align = "left", height="135px", width="675px")
    })
  
  # Displaying the name of the current group of instructions with a special color:
  output$group_indicator <- renderUI({
    req(current_block())
    grp <- as.character(values$decision_tree[values$current_row, 1])
    tagList(
      tags$b("Current group: "), # br(),
      span(grp, style = "color: rgb(217,152,21); font-weight: bold")
    )
  })
  
  # Creating a summarized history table with only Instruction and Choice:
  output$mini_history_table <- renderTable({
    req(values$history)
    if(nrow(values$history) == 0) return(NULL)
    tail(values$history[, c("Instruction", "Choice")], 5)
  }, colnames = FALSE
  )
  # Displaying the final table:
  output$final_table <- renderDT({
    req(values$history)
    datatable(values$history[, c("Group", "Instruction", "Choice")], 
              options = list(pageLength = 50))
  })
  
  # Creating the final text to be displayed at the end of the program:
  output$final_page_text <- renderUI({
    # We use HTML and paste with the separator <br> for line breaks:
    HTML(paste(
      "You have now covered all possible instructions.",
      "You can download the final protocol in .csv or in .txt.",
      "The first file corresponds to a table containing the instructions, and, for each one, the corresponding options along with their compatibility with previous choices, and the corresponding final choice.",
      "The second file corresponds to a synthetic, complete summarized text corresponding to your protocol.",
      "",
      "You can leave the program here by closing this window, or:",
      "   - RE-START the program, which will erase any previous answer,",
      "   - RESUME the program and continue your exploration by re-visiting a specific group of instructions - if so, only the answers corresponding to this group of instructions will be erased.",
      sep="<br/>"
    ))
  })
  
  # Downloading the final protocol as CSV file:
  output$download_protocol_csv <- downloadHandler(
    filename = function() { paste("CARROT_Protocol_", Sys.Date(), ".csv", sep="") },
    content = function(file) {
      # We select only the columns of interest in the 'history' table:
      write.csv(values$history[, c("Group", "Instruction", "Options", "Choice")],
                file, row.names = FALSE)
    }
  )
  
  # Downloading the final protocol as Excel file:
  output$download_protocol_excel <- downloadHandler(
    filename = function() { paste("CARROT_Protocol_", Sys.Date(), ".xlsx", sep="") },
    content = function(file) {
      # We select only the columns of interest in the 'history' table:
      write.xlsx(values$history[, c("Group", "Instruction", "Options", "Choice")],
                file, row.names = FALSE)
    }
  )
  
  # Downloading the final protocol as .txt file:
  output$download_protocol_text <- downloadHandler(
    filename = function() { paste("CARROT_Protocol_", Sys.Date(), ".txt", sep="") },
    content = function(file) {
      writeLines(values$protocol_full_text, file)
    }
  )
  
}

######################################################################################################
######################################################################################################

# --- LAUNCHING THE PROGRAM WITH SHINY ---
shinyApp(ui = ui, server = server)

######################################################################################################
######################################################################################################

# CREATING THE APP TO BE USED ONLINE ON GITHUB PAGES:
#####################################################

# Follow the procedure to create an app which will be displayed on GitHub Pages:
# https://hbctraining.github.io/Training-modules/RShiny/lessons/shinylive.html

# Update the "docs" folder from app.R using shinylive:
# shinylive::export(appdir = "../carrot/", destdir = "docs")
# or:
# shinylive::export(appdir = ".", destdir = "docs")

# To test whether the app online will work or not (WATCH OUT: it may be necessary to update the port number over repeated tests):
# httpuv::runStaticServer("docs/", port = 8008)

# Copy and paste the content of the updated "docs" into the "docs" of the cloned repository from GitHub
# Commit and push to origin (i.e GitHub)

# To launch the online app from GitHub Pages once everything has been done 
# (you may need to wait a few minutes before the app is updated after a new push):
# https://frees86.github.io/carrot/
