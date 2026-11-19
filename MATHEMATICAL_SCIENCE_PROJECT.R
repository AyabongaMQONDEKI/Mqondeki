
library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(plotly)
library(DT)
library(stringr)
library(scales)

# ---------- 1. CLEAN DATA ----------
Academic_Records <- read_csv("Academic_Records_Data_2025_CPUT_Students.csv")
Registration_Data <- read_csv("Registration_Data_2025_CPUT_Students.csv")
Residence_Data <- read_csv("Residence_Data_2025_CPUT_Students.csv")
Residence_Data %>%
  distinct(Identifier, .keep_all = TRUE)%>%
  select(c(Identifier,DATE_IN))-> Residence_Data

Academic_Records %>%
  distinct() %>%
  mutate(Out_comes = case_when(
    `Pass Fail` == "P" ~ "Pass",
    `Pass Fail` == "F" ~ "Fail",
    .default = "Not_Resulted"
  ),
  Level_of_study =case_when(
    `Period of Study` == 0 ~ "ECP year 0",
    `Period of Study` == 1 ~ "first year",
    `Period of Study` == 2 ~ "second year",
    `Period of Study` == 3 ~ "third year",
    `Period of Study` == 4 ~ " Advance Diploma",
    `Period of Study` == 5 ~ "Honours",
    `Period of Study` == 6 ~ "Masters",
    `Period of Study` == 7 ~ "PhD"
  ),
  Semester = case_when(
    `Subject Block` == 0 ~ "year subject",
    `Subject Block` == 1 ~ "semester 1 subject",
    `Subject Block` == 2 ~ "semester 2 subject"
  ))-> Academic_Records
Academic_Records %>%
  select(c(Identifier,Year,`Qualification Description`,
           Subject_Description,`Qualification Type Name`,
           Out_comes,Level_of_study,Semester)) -> Academic_Records

Registration_Data%>%
  distinct()%>% 
  mutate(Gender = case_when(Gender == 'F' ~ 'Female',
                            Gender == 'M' ~ 'Male',
                            Gender == 'U' ~ 'Unspecified'))%>%
  rename(FacultyName = `Faculty School Name` )%>%
  rename(DepartmentName = `Department Name`)%>%
  rename(Race = `Ethnic Group Name` ) -> Registration_Data
Registration_Data%>%
  select(c(Identifier,FacultyName,DepartmentName,Gender,
           Race,`Qualification Name`,`Qualification Type`,
           `Post Graduate`,
           `Offering Type Category`)) -> Registration_Data


Academic_Records %>%
  left_join(Registration_Data, by = "Identifier") %>%
  left_join(Residence_Data, by = "Identifier")-> Student_data
Student_data%>%
  filter(FacultyName == "APPLIED SCIENCES") -> Student_data

dat0 <- Student_data %>%
  mutate(
    Year         = as.integer(Year),
    Out_omes     = str_trim(as.character(Out_comes)),
    Out_omes     = case_when(
      str_to_lower(Out_omes) %in% c("pass", "p") ~ "Pass",
      str_to_lower(Out_omes) %in% c("fail", "f") ~ "Fail",
      TRUE                                       ~ "Not Resulted"
    ),
    Gender       = str_to_title(str_trim(Gender)),
    Race         = str_to_title(str_trim(Race)),
    DepartmentName        = str_trim(DepartmentName),
    Subject_Description   = str_trim(Subject_Description),
    `Qualification Type Name` = str_trim(`Qualification Type Name`),
    `Qualification Name`  = str_trim(`Qualification Name`),
    Level_of_study        = str_to_title(str_trim(Level_of_study))
  ) %>%
  filter(!is.na(Year))

yr_rng    <- range(dat0$Year, na.rm = TRUE)
dept_ch   <- sort(unique(dat0$DepartmentName))
level_ch  <- sort(unique(dat0$Level_of_study))
gender_ch <- sort(unique(dat0$Gender))
race_ch   <- sort(unique(dat0$Race))

PAL <- c(Pass = "#1E8449", Fail = "#C0392B", `Not Resulted` = "#95A5A6")

# helper: empty selection = keep all
filt <- function(d, col, vals) {
  if (is.null(vals) || length(vals) == 0) return(d)
  d[d[[col]] %in% vals, , drop = FALSE]
}

# ---------- 2. UI ----------
ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "flatly", primary = "#1B4F72"),
  
  titlePanel(
    tags$span(icon("graduation-cap"),
              "APPLIED SCIENCE Dashboard ",
              tags$small(class = "text-muted",
                         "— 89,463 records analysed"))
  ),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h6(icon("sliders"), " Filters", class = "fw-bold"),
      sliderInput("year", "Academic Year",
                  min = yr_rng[1], max = yr_rng[2],
                  value = yr_rng, step = 1, sep = ""),
      selectInput("dept",   "Department",     dept_ch,   dept_ch,   multiple = TRUE),
      selectInput("level",  "Level of Study", level_ch,  level_ch,  multiple = TRUE),
      hr(),
      h6(icon("users"), " Demographics", class = "fw-bold"),
      selectInput("gender", "Gender", gender_ch, gender_ch, multiple = TRUE),
      selectInput("race",   "Race",   race_ch,   race_ch,   multiple = TRUE),
      hr(),
      actionButton("reset", "Reset filters",
                   icon = icon("rotate-left"),
                   class = "btn-warning w-100")
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(
        # ---- OVERVIEW ----
        tabPanel("Overview", icon = icon("gauge-high"),
                 br(),
                 layout_column_wrap(
                   width = "230px", fill = FALSE,
                   card(card_header(icon("clipboard-check"), " Assessed"),
                        div(class = "text-center fs-2 fw-bold text-primary",
                            textOutput("kpi_assessed"))),
                   card(card_header(icon("check-circle"), " Pass Rate"),
                        div(class = "text-center fs-2 fw-bold text-success",
                            textOutput("kpi_passrate"))),
                   card(card_header(icon("times-circle"), " Fail Rate"),
                        div(class = "text-center fs-2 fw-bold text-danger",
                            textOutput("kpi_failrate"))),
                   card(card_header(icon("book-open"), " Subjects"),
                        div(class = "text-center fs-2 fw-bold text-secondary",
                            textOutput("kpi_subjects")))
                 ),
                 br(),
                 layout_column_wrap(
                   width = 1/2,
                   card(card_header(icon("chart-pie"), " Outcome Mix"),
                        plotlyOutput("p_donut", height = "290px")),
                   card(card_header(icon("tachometer-alt"), " Overall Pass Rate"),
                        plotlyOutput("p_gauge", height = "290px"))
                 )
        ),
        
        # ---- SUBJECT ANALYSIS ----
        tabPanel("Subject Analysis", icon = icon("book-open"),
                 br(),
                 card(
                   card_header(icon("ranking-star"), " Subject Leaderboard"),
                   layout_column_wrap(
                     width = 1/3, fill = FALSE,
                     sliderInput("min_n", "Min assessed per subject",
                                 min = 1, max = 100, value = 15, step = 5),
                     radioButtons("rank_dir", "Show",
                                  choices = c("Best" = "top", "Weakest" = "bottom"),
                                  inline = TRUE),
                     numericInput("top_n", "Number of subjects",
                                  value = 15, min = 5, max = 50, step = 5)
                   ),
                   plotlyOutput("p_leaderboard", height = "500px")
                 ),
                 br(),
                 card(card_header(icon("exclamation-triangle"), " Highest Failure Volume"),
                      plotlyOutput("p_failvol", height = "400px")),
                 br(),
                 card(card_header(icon("table"), " Subject Performance Table"),
                      DTOutput("t_subjects"))
        ),
        
        # ---- TRENDS ----
        tabPanel("Trends", icon = icon("chart-line"),
                 br(),
                 card(card_header(icon("arrow-trend-up"), " Pass Rate & Volume over Time"),
                      plotlyOutput("p_trend", height = "400px")),
                 br(),
                 card(card_header(icon("building"), " Pass Rate by Department over Time"),
                      plotlyOutput("p_trend_dept", height = "400px"))
        ),
        
        # ---- DEMOGRAPHICS ----
        tabPanel("Demographics", icon = icon("users"),
                 br(),
                 layout_column_wrap(
                   width = 1/2,
                   card(card_header(icon("venus-mars"), " Pass Rate — Gender × Race"),
                        plotlyOutput("p_heat", height = "380px")),
                   card(card_header(icon("balance-scale"), " Pass Rate by Gender & Level"),
                        plotlyOutput("p_gender_level", height = "380px"))
                 ),
                 br(),
                 card(card_header(icon("chart-bar"), " Outcome Composition by Race"),
                      plotlyOutput("p_race_stack", height = "400px"))
        ),
        
        # ---- DATA ----
        tabPanel("Data", icon = icon("table"),
                 br(),
                 card(
                   card_header(
                     icon("database"), " Filtered Records",
                     downloadButton("dl", "Download CSV",
                                    class = "btn-sm btn-success float-end")
                   ),
                   DTOutput("t_raw")
                 )
        )
      )
    )
  )
)

# ---------- 3. SERVER ----------
server <- function(input, output, session) {
  
  filtered_data <- reactive({
    d <- dat0 %>% filter(Year >= input$year[1], Year <= input$year[2])
    d <- filt(d, "DepartmentName", input$dept)
    d <- filt(d, "Level_of_study", input$level)
    d <- filt(d, "Gender",         input$gender)
    d <- filt(d, "Race",           input$race)
    d
  })
  
  assessed <- reactive({
    filtered_data() %>% filter(Out_omes %in% c("Pass", "Fail"))
  })
  
  observeEvent(input$reset, {
    updateSliderInput(session, "year", value = yr_rng)
    updateSelectInput(session, "dept",   selected = dept_ch)
    updateSelectInput(session, "level",  selected = level_ch)
    updateSelectInput(session, "gender", selected = gender_ch)
    updateSelectInput(session, "race",   selected = race_ch)
  })
  
  # KPI cards
  output$kpi_assessed <- renderText(format(nrow(assessed()), big.mark = ","))
  output$kpi_passrate <- renderText({
    a <- assessed(); if (nrow(a) == 0) return("—")
    paste0(round(mean(a$Out_omes == "Pass") * 100, 1), "%")
  })
  output$kpi_failrate <- renderText({
    a <- assessed(); if (nrow(a) == 0) return("—")
    paste0(round(mean(a$Out_omes == "Fail") * 100, 1), "%")
  })
  output$kpi_subjects <- renderText(
    format(n_distinct(filtered_data()$Subject_Description), big.mark = ","))
  
  # Donut
  output$p_donut <- renderPlotly({
    d <- filtered_data() %>% count(Out_omes)
    plot_ly(d, labels = ~Out_omes, values = ~n, type = "pie", hole = 0.55,
            marker = list(colors = unname(PAL[d$Out_omes]),
                          line = list(color = "#FFF", width = 2)),
            textinfo = "label+percent",
            hovertemplate = "<b>%{label}</b><br>%{value:,}<extra></extra>") %>%
      layout(showlegend = FALSE, margin = list(t = 10, b = 10, l = 10, r = 10))
  })
  
  # Gauge
  output$p_gauge <- renderPlotly({
    a  <- assessed()
    pr <- if (nrow(a) == 0) 0 else round(mean(a$Out_omes == "Pass") * 100, 1)
    bar_col <- if (pr >= 75) "#1E8449" else if (pr >= 50) "#D68910" else "#C0392B"
    plot_ly(type = "indicator", mode = "gauge+number",
            value = pr, number = list(suffix = "%", font = list(size = 36)),
            gauge = list(
              axis = list(range = list(0, 100)),
              bar  = list(color = bar_col, thickness = 0.3),
              steps = list(
                list(range = c(0, 50),   color = "#FADBD8"),
                list(range = c(50, 75),  color = "#FCF3CF"),
                list(range = c(75, 100), color = "#D4EFDF")
              ))) %>%
      layout(margin = list(t = 20, b = 10))
  })
  
  # Leaderboard
  output$p_leaderboard <- renderPlotly({
    d <- assessed() %>%
      group_by(Subject_Description) %>%
      summarise(assessed = n(), pr = mean(Out_omes == "Pass"), .groups = "drop") %>%
      filter(assessed >= input$min_n)
    validate(need(nrow(d) > 0,
                  "No subject meets the minimum-assessed threshold. Lower the slider."))
    d <- if (input$rank_dir == "top") {
      d %>% arrange(desc(pr)) %>% slice_head(n = input$top_n)
    } else {
      d %>% arrange(pr) %>% slice_head(n = input$top_n)
    }
    plot_ly(d, x = ~pr,
            y = ~reorder(str_trunc(Subject_Description, 34), pr),
            type = "bar", orientation = "h",
            marker = list(color = ~pr, colorscale = "RdYlGn",
                          cmin = 0, cmax = 1, showscale = FALSE),
            text = ~paste0(round(pr * 100, 1), "%  (n=", assessed, ")"),
            textposition = "outside",
            hovertemplate = "<b>%{y}</b><br>Pass rate: %{x:.1%}<br>%{text}<extra></extra>") %>%
      layout(xaxis = list(title = "Pass rate", tickformat = ".0%", range = c(0, 1.15)),
             yaxis = list(title = ""), showlegend = FALSE,
             margin = list(l = 10, r = 30, t = 10, b = 40))
  })
  
  # Highest failure volume
  output$p_failvol <- renderPlotly({
    d <- assessed() %>%
      group_by(Subject_Description) %>%
      summarise(fails = sum(Out_omes == "Fail"), assessed = n(),
                pr = mean(Out_omes == "Pass"), .groups = "drop") %>%
      arrange(desc(fails)) %>% slice_head(n = 15)
    validate(need(nrow(d) > 0, "No data for the current filters"))
    plot_ly(d, x = ~fails,
            y = ~reorder(str_trunc(Subject_Description, 34), fails),
            type = "bar", orientation = "h",
            marker = list(color = "#C0392B"),
            text = ~paste0(fails, " fails  |  PR ", round(pr * 100, 1), "%"),
            textposition = "outside",
            hovertemplate = "<b>%{y}</b><br>Failures: %{x:,}<br>%{text}<extra></extra>") %>%
      layout(xaxis = list(title = "Number of failed results"),
             yaxis = list(title = ""), showlegend = FALSE,
             margin = list(l = 10, r = 30, t = 10, b = 40))
  })
  
  # Subject table
  output$t_subjects <- renderDT({
    t <- assessed() %>%
      group_by(Subject_Description, DepartmentName) %>%
      summarise(Assessed = n(),
                Pass = sum(Out_omes == "Pass"),
                Fail = sum(Out_omes == "Fail"),
                .groups = "drop") %>%
      mutate(`Pass Rate` = Pass / Assessed,
             `Fail Rate` = Fail / Assessed) %>%
      arrange(desc(Assessed))
    datatable(t, rownames = FALSE, filter = "top",
              options = list(pageLength = 10, scrollX = TRUE,
                             order = list(list(2, "desc")), dom = "ftip"),
              class = "compact stripe hover") %>%
      formatPercentage(c("Pass Rate", "Fail Rate"), 1) %>%
      formatStyle("Pass Rate",
                  background = styleColorBar(c(0, 1), "#ABEBC6"),
                  backgroundSize = "100% 85%", backgroundRepeat = "no-repeat",
                  backgroundPosition = "center") %>%
      formatStyle("Fail Rate",
                  background = styleColorBar(c(0, 1), "#F5B7B1"),
                  backgroundSize = "100% 85%", backgroundRepeat = "no-repeat",
                  backgroundPosition = "center")
  })
  
  # Trend
  output$p_trend <- renderPlotly({
    d <- assessed() %>%
      group_by(Year) %>%
      summarise(assessed = n(), pass_pct = mean(Out_omes == "Pass") * 100,
                .groups = "drop")
    validate(need(nrow(d) > 0, "No data for the current filters"))
    plot_ly(d) %>%
      add_bars(x = ~Year, y = ~assessed, name = "Assessed", yaxis = "y2",
               marker = list(color = "#AED6F1"),
               hovertemplate = "%{x}<br>Assessed: %{y:,}<extra></extra>") %>%
      add_lines(x = ~Year, y = ~pass_pct, name = "Pass rate %", yaxis = "y",
                line = list(color = "#1B4F72", width = 3),
                marker = list(size = 7, color = "#1B4F72"),
                hovertemplate = "%{x}<br>Pass rate: %{y:.1f}%<extra></extra>") %>%
      layout(xaxis = list(title = "Year", dtick = 1),
             yaxis = list(title = "Pass rate (%)", range = c(0, 100)),
             yaxis2 = list(title = "Assessed", overlaying = "y",
                           side = "right", showgrid = FALSE),
             legend = list(orientation = "h", x = 0, y = 1.12),
             margin = list(t = 30, b = 50, r = 60),
             hovermode = "x unified", bargap = 0.4)
  })
  
  output$p_trend_dept <- renderPlotly({
    d <- assessed() %>%
      group_by(Year, DepartmentName) %>%
      summarise(assessed = n(), pr = mean(Out_omes == "Pass"), .groups = "drop") %>%
      filter(assessed >= 10)
    validate(need(nrow(d) > 0, "Not enough data for the current filters"))
    plot_ly(d, x = ~Year, y = ~pr, color = ~str_trunc(DepartmentName, 20),
            type = "scatter", mode = "lines+markers",
            hovertemplate = "%{x}<br>Pass rate: %{y:.1%}<extra>%{fullData.name}</extra>") %>%
      layout(xaxis = list(title = "Year", dtick = 1),
             yaxis = list(title = "Pass rate", tickformat = ".0%", range = c(0, 1)),
             legend = list(font = list(size = 9), orientation = "h", y = -0.3),
             margin = list(t = 10, b = 80))
  })
  
  # Heatmap
  output$p_heat <- renderPlotly({
    d <- assessed() %>%
      group_by(Gender, Race) %>%
      summarise(assessed = n(), pr = mean(Out_omes == "Pass") * 100, .groups = "drop")
    validate(need(nrow(d) > 0, "No data for the current filters"))
    plot_ly(d, x = ~Race, y = ~Gender, z = ~pr, type = "heatmap",
            colorscale = "RdYlGn", cmin = 0, cmax = 100,
            text = ~paste0(round(pr, 1), "%<br>n=", assessed),
            hovertemplate = "<b>%{y} / %{x}</b><br>%{text}<extra></extra>",
            colorbar = list(title = "Pass<br>%", thickness = 12)) %>%
      layout(xaxis = list(title = ""), yaxis = list(title = ""),
             margin = list(t = 10, b = 40))
  })
  
  # Gender × level
  output$p_gender_level <- renderPlotly({
    d <- assessed() %>%
      group_by(Gender, Level_of_study) %>%
      summarise(assessed = n(), pr = mean(Out_omes == "Pass"), .groups = "drop") %>%
      filter(assessed >= 5)
    validate(need(nrow(d) > 0, "No data for the current filters"))
    plot_ly(d, x = ~Level_of_study, y = ~pr, color = ~Gender,
            type = "bar", barmode = "group",
            text = ~paste0(round(pr * 100, 1), "%"), textposition = "outside",
            customdata = ~assessed,
            hovertemplate = "<b>%{x}</b><br>%{data.name}: %{y:.1%}<br>n=%{customdata:,}<extra></extra>") %>%
      layout(xaxis = list(title = "", tickangle = -20),
             yaxis = list(title = "Pass rate", tickformat = ".0%",
                          range = c(0, min(1.25, max(d$pr) + 0.25))),
             legend = list(orientation = "h", y = 1.12),
             margin = list(t = 30, b = 90))
  })
  
  # Race composition
  output$p_race_stack <- renderPlotly({
    d <- filtered_data() %>% count(Race, Out_omes)
    validate(need(nrow(d) > 0, "No data for the current filters"))
    plot_ly(d, x = ~n, y = ~Race, color = ~Out_omes, colors = PAL,
            type = "bar", orientation = "h", barmode = "stack",
            hovertemplate = "<b>%{y}</b><br>%{data.name}: %{x:,}<extra></extra>") %>%
      layout(xaxis = list(title = "Number of records"),
             yaxis = list(title = "", categoryorder = "total ascending"),
             legend = list(orientation = "h", y = 1.12),
             margin = list(t = 30, l = 10, b = 50), bargap = 0.35)
  })
  
  # Data explorer
  output$t_raw <- renderDT({
    filtered_data() %>%
      select(Identifier, Year, FacultyName, DepartmentName,
             `Qualification Type Name`, `Qualification Name`, Level_of_study,
             Subject_Description, Out_omes, Gender, Race) %>%
      datatable(rownames = FALSE, filter = "top", server = TRUE,
                options = list(pageLength = 15, scrollX = TRUE,
                               lengthMenu = c(10, 25, 50, 100), dom = "lftip"),
                class = "compact stripe hover") %>%
      formatStyle("Out_omes",
                  backgroundColor = styleEqual(
                    c("Pass", "Fail", "Not Resulted"),
                    c("#D4EFDF", "#FADBD8", "#EAECEE")))
  })
  
  output$dl <- downloadHandler(
    filename = function() paste0("subject_outcomes_", Sys.Date(), ".csv"),
    content  = function(file) write.csv(filtered_data(), file, row.names = FALSE)
  )
}

shinyApp(ui, server)