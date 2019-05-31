# global --------------
library(shiny)
library(shinydashboard)
library(shinycustomloader)

library(dplyr)
library(tidyr)
library(DT)
library(ggplot2)
library(plotly)

options(scipen = 100)

## new equations ----
steer <- function(등지방, 등심단면적, 도체중) {
  eq <- (11.06398 - 1.25149 * 등지방 + 0.28293 * 등심단면적 + (0.56781 * 도체중)) / 도체중 * 100
  eq <- round(eq, 2)
  return(eq)
}

cow <- function(등지방, 등심단면적, 도체중) {
  eq <- (6.90137 - 0.9446 * 등지방 + 0.31805 * 등심단면적 + (0.54952 * 도체중)) / 도체중 * 100
  eq <- round(eq, 2)
  return(eq)
}

bull <- function(등지방, 등심단면적, 도체중) {
  eq <- (0.20103 - 2.19525 * 등지방 + 0.29275 * 등심단면적 + (0.64099 * 도체중)) / 도체중 * 100
  eq <- round(eq, 2)
  return(eq)
}

# header --------------

header <- dashboardHeader(
  
  title = "새로운 육량등급 시뮬레이터",
  titleWidth = 300,

  tags$li(
    class = "dropdown",
    tags$a(icon("code"),
      href = "https://adatalab.github.io",
      title = "adatalab"
    )
  )
)

# sidebar --------------

sidebar <- dashboardSidebar(
  width = 300,
  sidebarMenu(

    ## file input ----
    fileInput(
      inputId = "selFile",
      label = "파일을 선택해 주세요"
    ),

    selectInput(
      "sex",
      "성별",
      choices = c("모두", "거세", "수", "암", "미암")
    ),

    menuItem("Summary",
      tabName = "summary",
      icon = icon("chart-pie")
    ),

    ## menu item ----
    menuItem("Table",
      tabName = "table",
      icon = icon("table")
    ),
    
    ## Comments ----
    menuItem(
      "Comments",
      tabName = "comments",
      icon = icon("comment")
    ),

    ## About ----
    menuItem("About",
      tabName = "about",
      icon = icon("info-circle")
    )
  )
)

# body --------------

body <- dashboardBody(
  tabItems(
    ## main ----
    tabItem(
      tabName = "summary",

      fluidRow(
        box(
          
          # tags$head(includeScript("gtag.js")), ## Google analytics
          
          title = "About",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          "2019년 12월 1일부터 새롭게 변화하는 쇠고기 등급판정 세부기준 개정에 따라 바뀐 새로운 육량등급을 적용해 볼 수 있는 프로그램입니다.",
          br(),
          "예제파일 양식에 맞춰 파일을 업로드하면 기존 데이터를 기반으로 새로운 육량등급체계에 맞춰 산출된 새로운 육량등급을 보실 수 있습니다.",
          tags$strong("노란색 셀은 필수 입력사항입니다")
        ),

        box(
          title = "예제파일 다운로드",
          status = "danger",
          solidHeader = TRUE,
          collapsible = TRUE,
          tags$a(
            href = "https://github.com/YoungjunNa/adatalab/raw/master/%E1%84%92%E1%85%A1%E1%86%AB%E1%84%8B%E1%85%AE%E1%84%83%E1%85%B3%E1%86%BC%E1%84%80%E1%85%B3%E1%86%B8%E1%84%8B%E1%85%A8%E1%84%8C%E1%85%A6.xlsx",
            "농장데이터예제.xlsx 다운로드",
            target = "_blank"
          ),
          br(),
          "주의: 5MB 미만의 파일만 업로드 가능합니다.",
          br(),
          "업로드하신 파일은 개발자가 수집하거나 사용할수 ",
          tags$strong("없습니다.")
        )
      ),


      fluidRow(
        box(
          title = "문의 & 개선제안",
          status = "info",
          solidHeader = TRUE,
          collapsible = TRUE,
          "Email: ruminoreticulum@gmail.com"
        ),

        box(
          title = "바로가기",
          status = "info",
          solidHeader = TRUE,
          collapsible = TRUE,
          tags$a(
            href = "http://www.law.go.kr/%ED%96%89%EC%A0%95%EA%B7%9C%EC%B9%99/%EC%B6%95%EC%82%B0%EB%AC%BC%20%EB%93%B1%EA%B8%89%ED%8C%90%EC%A0%95%20%EC%84%B8%EB%B6%80%EA%B8%B0%EC%A4%80",
            "축산물 등급판정 세부기준(개정)",
            target = "_blank"
          )
        )
      ),

      fluidPage(
        column(
          12,
          withLoader(plotlyOutput("compare1"), type = "html", loader = "pacman")
        ),

        column(
          12,
          withLoader(dataTableOutput("compare2"), type = "html", loader = "pacman")
        )
      )
    ),

    ## table ----
    tabItem(
      tabName = "table",
      withLoader(dataTableOutput("result1"), type = "html", loader = "pacman")
    ),
    
    
    ## comments ----
    tabItem(
      tabName = "comments",
      div(id = "disques_thread",
          includeHTML("www/disqus.html"))
    ),

    ## About ----
    tabItem(
      tabName = "about",
      fluidRow(
        box(
          title = "project adatalab",
          width = 6,
          status = "success",
          solidHeader = TRUE,
          tags$img(
            src = "https://github.com/adatalab/logo/blob/master/logo.png?raw=true",
            width = 100,
            height = 100
          ),
          br()
          ,
          tags$a(
            href = "https://adatalab.github.io/",
            '"animal science with data science. animal datalab."',
            target = "_blank"
          ),
          br(),
          "문의 & 개선제안: ruminoreticulum@gmail.com"
        ),

        box(
          title = "R packages",
          width = 6,
          status = "primary",
          solidHeader = TRUE,
          tags$a(
            href = "https://github.com/adatalab/feed",
            "feed",
            target = "_blank"
          ),
          br(),
          tags$a(
            href = "https://github.com/adatalab/hanwoo",
            "hanwoo",
            target = "_blank"
          ),
          br(),
          tags$a(
            href = "https://github.com/adatalab/dairyCattle",
            "dairyCattle",
            target = "_blank"
          ),
          br(),
          tags$a(
            href = "https://github.com/adatalab/CH4goat",
            "CH4goat",
            target = "_blank"
          )
        ),

        box(
          title = "Applications",
          width = 6,
          status = "info",
          solidHeader = TRUE,
          tags$a(
            href = "https://youngjunna.shinyapps.io/Hanwoolab-example/",
            "HanwooLab",
            target = "_blank"
          ),
          br(),
          tags$a(
            href = "https://youngjunna.shinyapps.io/dairylab-pro/",
            "DairyLab",
            target = "_blank"
          )
        )
      )
    )
  )
)

# ui --------------

ui <- dashboardPage(header, sidebar, body, skin = "green")


# server --------------
server <- function(input, output, session) {

  ## reactive ----
  df <- reactive({
    req(input$selFile$datapath)
    df1 <- readxl::read_excel(input$selFile$datapath)

    df1[c("등지방", "등심단면적", "도체중", "육량지수", "근내지방")] <- sapply(df1[c("등지방", "등심단면적", "도체중", "육량지수", "근내지방")], as.numeric)

    ### steer ----
    steer1 <- filter(df1, 성별 == "거세")
    steer1$new_육량지수 <- mapply(steer, steer1$등지방, steer1$등심단면적, steer1$도체중)
    steer1 <- mutate(
      steer1,
      new_육량등급 = ifelse(new_육량지수 >= 62.52, "A", ifelse(new_육량지수 < 60.40, "C", "B"))
    )

    ### cow ----
    cow1 <- filter(df1, 성별 == "미암" | 성별 == "암")
    cow1$new_육량지수 <- mapply(cow, cow1$등지방, cow1$등심단면적, cow1$도체중)
    cow1 <- mutate(
      cow1,
      new_육량등급 = ifelse(new_육량지수 >= 61.83, "A", ifelse(new_육량지수 < 59.7, "C", "B"))
    )

    ### bull ----
    bull1 <- filter(df1, 성별 == "수")
    bull1$new_육량지수 <- mapply(bull, bull1$등지방, bull1$등심단면적, bull1$도체중)
    bull1 <- mutate(
      bull1,
      new_육량등급 = ifelse(new_육량지수 >= 68.45, "A", ifelse(new_육량지수 < 66.32, "C", "B"))
    )

    df1 <- rbind(steer1, cow1, bull1)
    df1 <- df1[, c(5:10, 23, 11:12, 24, 13:22)]

    if (input$sex == "거세") {
      df1 <- df1 %>%
        filter(성별 == "거세")
    }

    if (input$sex == "암") {
      df1 <- df1 %>%
        filter(성별 == "암")
    }

    if (input$sex == "미암") {
      df1 <- df1 %>%
        filter(성별 == "미암")
    }

    if (input$sex == "수") {
      df1 <- df1 %>%
        filter(성별 == "수")
    }

    if (input$sex == "모두") {
      df1
    } else {
      df1
    }

    return(df1)
  })

  ## output ----
  output$result1 <- renderDataTable({
    req(input$selFile$datapath)
    datatable(
      df(),
      rownames = FALSE,
      extensions = "Buttons",
      options = list(
        dom = "Bfrtip",
        buttons = c("copy", "csv", "excel", "print"),
        autoWidth = TRUE,
        columnDefs = list(list(width = "100px", targets = "_all", className = "dt-center")),
        pageLength = 10000,
        lengthMenu = c(50, 100, 1000, 10000, 100000),
        deferRender = TRUE,
        scrollX = TRUE,
        scrollY = 800
      )
    ) %>%
      formatStyle("등지방",
        background = styleColorBar(c(0, max(df()$등지방, na.rm = TRUE)), "lightblue"),
        backgroundSize = "98% 88%",
        backgroundRepeat = "no-repeat",
        backgroundPosition = "center"
      ) %>%
      formatStyle("등심단면적",
        background = styleColorBar(c(0, max(df()$등심단면적, na.rm = TRUE)), "green"),
        backgroundSize = "98% 88%",
        backgroundRepeat = "no-repeat",
        backgroundPosition = "center"
      ) %>%
      formatStyle("도체중",
        background = styleColorBar(c(0, max(df()$도체중, na.rm = TRUE)), "pink"),
        backgroundSize = "98% 88%",
        backgroundRepeat = "no-repeat",
        backgroundPosition = "center"
      ) %>%
      formatStyle("육량지수",
        background = styleColorBar(c(0, 100), "teal"),
        backgroundSize = "98% 88%",
        backgroundRepeat = "no-repeat",
        backgroundPosition = "center"
      ) %>%
      formatStyle("new_육량지수",
        background = styleColorBar(c(0, 100), "teal"),
        backgroundSize = "98% 88%",
        backgroundRepeat = "no-repeat",
        backgroundPosition = "center"
      ) %>%
      formatStyle("근내지방",
        background = styleColorBar(c(0, max(df()$근내지방, na.rm = TRUE)), "lime"),
        backgroundSize = "98% 88%",
        backgroundRepeat = "no-repeat",
        backgroundPosition = "center"
      ) %>%
      formatStyle("육량등급",
        backgroundColor = styleEqual(c("A", "B", "C"), c("lime", "yellow", "red"))
      ) %>%
      formatStyle("육질등급",
        backgroundColor = styleEqual(c("1++", "1+", "1", "2", "3"), c("#DAA520", "#FFD700", "#FFA500", "#FF8C00", "gray"))
      ) %>%
      formatStyle("new_육량등급",
        backgroundColor = styleEqual(c("A", "B", "C"), c("lime", "yellow", "red"))
      )
  })

  output$compare1 <- renderPlotly({
    req(input$selFile$datapath)

    df <- gather(df(), version, result, 육량등급, new_육량등급)

    df <- filter(df, result == "A" | result == "B" | result == "C")
    df$result <- factor(df$result, levels = c("A", "B", "C"))

    p <- df %>%
      ggplot(aes(result, fill = version)) +
      scale_x_discrete(limits = c("C", "B", "A")) +
      geom_bar(width = 0.5, position = "dodge") +
      # geom_bar(aes(y = round((..count..) / sum(..count..) * 100, 1)), width = 0.5, position = "dodge") +
      scale_fill_brewer(palette = "Set1") +
      ylab("두수") +
      xlab("") +
      coord_flip() +
      guides(fill = FALSE) +
      theme_classic() +
      ggtitle("육량등급비교")
    ggplotly(p)
  })

  output$compare2 <- renderDataTable({
    df <- filter(df(), 육량등급 == "A" | 육량등급 == "B" | 육량등급 == "C")
    df$육량등급 <- factor(df$육량등급, levels = c("A", "B", "C"))
    df$new_육량등급 <- factor(df$new_육량등급, levels = c("A", "B", "C"))

    a <- as.data.frame(table(df$육량등급))
    b <- as.data.frame(table(df$new_육량등급))

    dd <- data.frame(등급 = c("A", "B", "C"), 기존 = a$Freq, 새로운 = b$Freq)
    dd <- mutate(dd, 차이 = 새로운 - 기존)
    dd <- mutate(dd, 기존대비_퍼센트 = round(차이 / 기존 * 100, 1))

    dd
  })
}

# runApp --------------
shinyApp(ui, server)
