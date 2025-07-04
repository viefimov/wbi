library(cluster)
library(rsconnect)
library(shiny)
library(shinyjs)
library(readr)
library(dplyr)
library(caret)
library(tidymodels)
library(shinyWidgets)
wb <- read_csv('./data/Wellbeing_and_lifestyle_data_Kaggle.csv')
wb <- wb[-10005, ]
wb <- wb %>% 
  select(-c(Timestamp, AGE, GENDER))
wb$DAILY_STRESS <- as.integer(wb$DAILY_STRESS)

set.seed(46)
split <- initial_split(wb, prop = 0.8)
wb_train <- training(split)
wb_test <- testing(split)

linmodel <- lm(WORK_LIFE_BALANCE_SCORE ~ ., data = wb_train)
wb <- wb %>% 
  select(-c(WORK_LIFE_BALANCE_SCORE))

happiest <- list(FRUITS_VEGGIES = 5, DAILY_STRESS = 0, PLACES_VISITED = 10,
                 CORE_CIRCLE = 10, SUPPORTING_OTHERS = 10, SOCIAL_NETWORK = 10, ACHIEVEMENT = 10,
                 DONATION = 5, BMI_RANGE = 1, TODO_COMPLETED = 10, FLOW = 10, DAILY_STEPS = 10,
                 LIVE_VISION = 10, SLEEP_HOURS = 10, LOST_VACATION = 0, DAILY_SHOUTING = 0,
                 SUFFICIENT_INCOME = 2, PERSONAL_AWARDS = 10, TIME_FOR_PASSION = 10,
                 WEEKLY_MEDITATION = 10)
unhappiest <- list(FRUITS_VEGGIES = 0, DAILY_STRESS = 5, PLACES_VISITED = 0,
                   CORE_CIRCLE = 0, SUPPORTING_OTHERS = 0, SOCIAL_NETWORK = 0, ACHIEVEMENT = 0,
                   DONATION = 0, BMI_RANGE = 2, TODO_COMPLETED = 0, FLOW = 0, DAILY_STEPS = 1,
                   LIVE_VISION = 0, SLEEP_HOURS = 1, LOST_VACATION = 10, DAILY_SHOUTING = 10,
                   SUFFICIENT_INCOME = 1, PERSONAL_AWARDS = 0, TIME_FOR_PASSION = 0,
                   WEEKLY_MEDITATION = 0)
wb_dummies=na.omit(wb)

wb <- rbind(wb, happiest)
wb <- rbind(wb, unhappiest)

wb_scores=predict(linmodel, wb)

scores_vector=as.vector(wb_scores)

df=data.frame(scores_vector=scores_vector)


wb=cbind(wb, df)
wb$wb_scores_scaled <- 100*((df$scores_vector - 480) / (820.2 - 480))

lm <- lm(wb_scores_scaled ~ FRUITS_VEGGIES + DAILY_STRESS + PLACES_VISITED + CORE_CIRCLE + SUPPORTING_OTHERS + SOCIAL_NETWORK + ACHIEVEMENT + DONATION + BMI_RANGE + TODO_COMPLETED + FLOW + DAILY_STEPS + LIVE_VISION + SLEEP_HOURS + LOST_VACATION + DAILY_SHOUTING + SUFFICIENT_INCOME + PERSONAL_AWARDS + TIME_FOR_PASSION + WEEKLY_MEDITATION, data = wb)



ui <- tags$div(
  style = "display: flex; flex-direction: column; justify: space-between; height: 100vh; width: 100%;",  # Use 'height' to fill the viewport
  fluidPage(
    setBackgroundImage(src = "https://images.unsplash.com/photo-1516541196182-6bdb0516ed27?q=80&w=2574&auto=format&fit=crop&ixlib=rb-4.0.3&ixid=M3wxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHx8fA%3D%3D"),
    tags$head(
      tags$style(HTML("
        #main-content {  display: flex; flex-direction: column; }
        #content-body { overflow-y: auto; }  # Allows content to scroll
        body {margin: 0; width: 100%; padding: 0; opacity:0.5;} 
        .container-fluid {margin:0; width:100%; min-height:100vh; padding: 0;}
        #summary {width:100%; font-size:60px; text-align:center;}
        .recommendation-text {
      white-space: pre-wrap;
      word-wrap: break-word;
      width: 100%;
      display: flex; flex-direction: column;
      gap: 0;}
      
    .form-group {
      width: 100% !important;
    }
    .shiny-input-container .irs {
      width: 100%  !important;
    }
    .shiny-slider-input .irs {
      width: 100% !important;
    }
    .shiny-input-radiogroup {
      width: 100%;
    }
    .equal-height-columns {
          display: flex;
          justify-content: space-between;

        }
        .equal-height-columns > div {
          height:100%;
          flex: 1;
          display: flex;
          flex-direction: column;
          border:none;
        }
        .equal-height-columns > div > div {
          padding: 7px;
          border: 2px solid #94B1FF;
          border-radius: 5px;
          background-color:rgba(148,177,255,0.08);
          
        }
        @media(max-width:35em){
        h1 {
          font-size: 15px;
        
        }
        a {
          font-size: 8px;
        }
        }
        @media (max-width: 600px) {
          .equal-height-columns {
            flex-direction: column;
          }}

      "))
    ),
    div(
      id = "main-content",
      class = 'bg-img',
      useShinyjs(),
      tags$header(
        style = "display: flex;  align-items: center; justify-content: center;margin-bottom: 40px; background-color: #B4CFFA; width: 100%; padding-left: 10px; padding-right: 10px;border-radius: 72% 28% 72% 28% / 23% 77% 23% 77%; border: 7px solid #B4CFFA; box-shadow: 0px 0px 12px 5px #B4CFFA",
        div(
          style = "display: flex; align-items: center;",  # Container for centering elements
          tags$h1("Индекс благополучия", style = "max-width: fit-content; "),
          actionLink("surveyLink", "Основано на результатах опроса Authentic Happiness Project", onclick = "window.open('https://www.kaggle.com/datasets/ydalat/lifestyle-and-wellbeing-data');", style = " ")
        )
      ),
      
      
      div(
        id = "content-body",
        
        div(id = "introText",
            h3("Узнайте свой уровень благополучия"),
            p("Нажмите кнопку ниже чтобы начать опрос."),
            actionButton("startSurvey", "Начать опрос", class = "btn-primary btn-lg"),
            style = "text-align: center; padding: 20px; width:100%;"
        ),
        conditionalPanel(
          condition = "input.startSurvey > 0",
          div(id = "formDiv",
              
              style = 'padding:10px; flex-grow:0;',
              div(class = "equal-height-columns",
                column(4, 
                       style = "padding: 5px; margin-bottom: 10px; height:100%;",
                       sliderInput("fruits", "1) Какое количество фруктов/овощей Вы потребляете каждый день?", min = 0, max = 5, value = 0),
                       sliderInput("stress", "2) Какой уровень стресса вы обычно испытываете каждый день?", min = 0, max = 5, value = 0),
                       sliderInput("newPlaces", "3) Как много новых мест вы посетили за последний год?", min = 0, max = 10, value = 0),
                       sliderInput("closePeople", "4) Сколько людей входит в ваш близкий круг общения?
", min = 0, max = 10, value = 0),
                       sliderInput("helpOthers", "5) Скольким людям вы помогли улучшить жизнь за последний год?", min = 0, max = 10, value = 0),
                       sliderInput("interactPeople", "6) С каким кол-вом людей вы обычно контактируете в течение дня?", min = 0, max = 10, value = 0),
                       sliderInput("achievements", "7) Сколькими своими значительными достижениями за последний год Вы гордитесь?
", min = 0, max = 10, value = 0)),
                column(4, 
                       style = "padding: 5px; margin-bottom: 10px; height:100%;",
                       sliderInput("donate", "8) Сколько раз за последний год вы жертвовали деньгами или своим временем на благотворительность?", min = 0, max = 5, value = 0),
                       sliderInput("shout", "16) Сколько раз за неделю обычно вы кричите или обижаетесь на кого-то?", min = 0, max = 10, value = 0),
                       sliderInput("tasks", "10) Насколько хорошо вы справляетесь с выполнением своего списка дел на неделю?", min = 0, max = 10, value = 0),
                       sliderInput("flowHours", "11) Сколько часов в день обычно вы находитесь в состоянии потока, т.е. полностью погружены в какую-то деятельность?", min = 0, max = 10, value = 0),
                       sliderInput("walkSteps", "12) Сколько тысяч шагов вы обычно проходите каждый день?", min = 1, max = 10, value = 1),
                       sliderInput("visionYears", "13) На сколько лет вперед у вас есть четкое представление о своей жизни?", min = 0, max = 10, value = 0),
                       sliderInput("sleep", "14) Сколько часов в день Вы обычно спите?
", min = 1, max = 10, value = 1)),
                column(4, 
                       style = "padding: 5px; margin-bottom: 10px; height:100%;",
                       sliderInput("vacationLoss", "15) Сколько дней отпуска вы обычно теряете каждый год?", min = 0, max = 10, value = 0),
                       radioButtons(
                         "bmi", 
                         "9) Какой у Вас индекс массы тела (ИМТ)? (ИМТ = вес/(рост^2))
", 
                         choices = list(
                           "меньше 25" = 1, 
                           "25 и больше" = 2
                         ),selected = NA),
                       radioButtons(
                         "incomeSufficiency", # The input ID
                         "17) Вам хватает дохода, чтобы обеспечить свои базовые потребности?", # The question
                         choices = list(
                           "не хватает" = 1, 
                           "хватает" = 2     
                         ),selected = NA),
                       sliderInput("recognitions", "18) Сколько наград вы получили в своей жизни?", min = 0, max = 10, value = 0),
                       sliderInput("passionHours", "19) Сколько часов в день вы проводите, занимаясь тем, что вам действительно нравится?", min = 0, max = 10, value = 0),
                       sliderInput("selfThink", "20) Сколько раз в неделю у вас есть возможность подумать о себе?", min = 0, max = 10, value = 0),
                       actionButton("submit", "Отправить", class = "btn-primary btn-lg"))
              )
          )
        ),
        conditionalPanel(
          condition = "input.submit > 0 && output.validInputs",
          div(id = "resultDiv",
              style = 'padding-left:10px;padding-right:10px; width:100%;overflow-x: hidden; display: flex; flex-direction:column;',
              div(style='display:flex; justify-content:space-between;',
                  div(style = 'max-width:60%; font-size:15px; text-shadow: 1px 1px 0px rgba(138,163,206,0.54); background-color:rgba(189, 223, 255, 0.3); padding: 10px; border-radius:15px;',
                      h4("Небольшая статистическая справка:"),
                      textOutput("loadedText")),
                  div(style='margin-right: 10px; align-self: center; max-width:fit-content; text-align:center; font-size:30px; flex-grow:1; block-size: fit-content; padding-left:20px;padding-right:20px; border-radius: 25px; background-color: rgba(148,177,255,0.38);box-shadow: 0px 0px 20px 3px rgba(148,177,255,0.5);',
                      div('Результат:'),
                      textOutput("summary"))
              ) ,
              
              
              
              div(class = "recommendation-text",id='recom-text',
                  style='text-shadow: 1px 1px 0px rgba(138,163,206,0.54);',
                h4("Наши рекоммендации"),
                uiOutput("recom")
              )
              
          )
        )
      )
    )
  )
)






server <- function(input, output, session) {
  validInputs <- reactive({
    !is.null(input$incomeSufficiency) && !is.na(input$incomeSufficiency) &&
      !is.null(input$bmi) && !is.na(input$bmi)
  })
  output$validInputs <- reactive({
    validInputs()
  })
  outputOptions(output, "validInputs", suspendWhenHidden = FALSE)
  observeEvent(input$submit, {
    if (!validInputs()) {
      showModal(modalDialog(
        title = "Неправильно заполена форма",
        "Для отправки нужно заполнить все поля. Ответьте на пропущенные вопросы и отправьте форму заново.",
        easyClose = TRUE
      ))
      
    } else {
    new_data <- data.frame(
      FRUITS_VEGGIES = as.integer(input$fruits),
      DAILY_STRESS = as.integer(input$stress),
      PLACES_VISITED = as.integer(input$newPlaces),
      CORE_CIRCLE = as.integer(input$closePeople),
      SUPPORTING_OTHERS = as.integer(input$helpOthers),
      SOCIAL_NETWORK = as.integer(input$interactPeople),
      ACHIEVEMENT = as.integer(input$achievements),
      DONATION = as.integer(input$donate),
      BMI_RANGE = as.integer(input$bmi),
      TODO_COMPLETED = as.integer(input$tasks),
      FLOW = as.integer(input$flowHours),
      DAILY_STEPS = as.integer(input$walkSteps),
      LIVE_VISION = as.integer(input$visionYears),
      SLEEP_HOURS = as.integer(input$sleep),
      LOST_VACATION = as.integer(input$vacationLoss),
      DAILY_SHOUTING = as.integer(input$shout),
      SUFFICIENT_INCOME = as.integer(input$incomeSufficiency),
      PERSONAL_AWARDS = as.integer(input$recognitions),
      TIME_FOR_PASSION = as.integer(input$passionHours),
      WEEKLY_MEDITATION = as.integer(input$selfThink)
    )
    print("New Data for Prediction:")  
    print(new_data)
    print(colnames(wb_dummies))
    print(colnames(new_data))
    
    print(ncol(wb_dummies))
    print(ncol(new_data))
    
    result <- predict(lm, newdata = new_data)
    updated_data <- rbind(wb_dummies, new_data)
    
    kmeans_model <- kmeans(updated_data, centers = 4, nstart = 10)
    cluster_labels <- kmeans_model$cluster

    updated_data$cluster_labels <- cluster_labels
    user_cluster <- updated_data$cluster_labels[nrow(updated_data)]
    
   
    print(user_cluster)
    print(class(result))
    recommendations <- switch(as.character(user_cluster),
                              "1" = "Представители данного кластера по сравнению с другими реже жертвуют свое время или деньги на благие цели. Также им реже хватает дохода для удовлетворения своих базовых потребностей. Более того, они не так часто помогают людям в своем окружении и едят меньше фруктов и овощей, чем представители других групп.",
                              "2" = "Среди особенностей этого кластера можно выделить: наименьшее кол-во пропущенных отпусков, самый низкий ИМТ. Также у этой группы пусть и не самые, но довольно высокие показатели удовлетворенности доходом, кол-ва новых посещенных мест, потребляемых фруктов и овощей, медитаций и т.д. Однако в среднем они меньше времени по сравнению с остальными ежедневно уделяют своим увлечениям, гордятся меньшим количеством собственных достижений, реже поддерживают близких и видят свое будущее на меньшее число лет вперед.",
                              "3" = "У этого кластера в среднем самый низкий уровень стресса, негативных эмоций, большее кол-во часов сна, количество пройденных шагов, медитаций, кол-во сделанных дел, социальных взаимодействий, поддержки окружающих пожертвований и т.д.",
                              "4" = "Данному кластеру чаще остальных надо следить за уровнем ежедневного стресса, посещать больше новых мест. Также представители чаще сталкиваются с проблемой нехватки денег и чаще теряют дни отпуска.",
                              "Ошибка в кластеризации")
    if (result <= 40) {
      recommendation <- paste("У вас очень низкий уровень благополучия. Чтобы улучшить свой результат, вы можете обратить внимание на критерии, к которым чаще остальных уязвимы представители той же группы:\n", recommendations)
      recommendation_bg <- "rgba(255, 141, 141, 0.3)"
      } else if (result > 40 && result <= 60) {
      recommendation <- paste("У вас средний уровень благополучия. Есть куда расти! Чтобы улучшить свой результат, вы можете обратить внимание на критерии, к которым чаще остальных уязвимы представители той же группы:\n", recommendations)
      recommendation_bg <- "rgba(176, 211, 255, 0.3)"
      } else if (result > 60 && result <= 80) {
      recommendation <- paste("Прекрасный результат, не забывайте его поддерживать. Чтобы улучшить свой результат, вы можете обратить внимание на критерии, к которым чаще остальных уязвимы представители той же группы:\n", recommendations)
      recommendation_bg <- "rgba(193, 254, 197, 0.3)"
      } else if (result > 80) {
      recommendation <- "Вау, поздравляем с таким высоким результатом! Продолжайте в том же духе"
      recommendation_bg <- "rgba(193, 254, 197, 0.35)"
      } else {
      recommendation <- "Ошибка в выводе данных"
      recommendation_bg <- "rgba(200, 15, 15, 0.4)"
    }
    
    
    print("Recommendations:")
    print(recommendation)
    
    shinyjs::hide("formDiv")
    
    output$summary <- renderText({
      paste(as.integer(result))
    })
    output$recom <- renderUI({
      tags$div(
        style = paste("background-color:", recommendation_bg, "; padding:10px; border-radius: 15px;"),
        
        recommendation
      )
    })
    output$loadedText <- renderText({
      '
Респонденты до 20 лет реже поддерживают других и занимаются благотворительностью, но больше времени уделяют увлечениям. Группа 21-35 лет имеет самый узкий круг общения, в среднем 5 человек. Наименее уверены в будущем респонденты 21-35 и 36-50 лет, тогда как у респондентов старше 51 года уверенность выше. Респонденты старше 51 года также едят больше фруктов и овощей. Существенных различий между гендерами нет, однако женщины более уверены в будущем и продуктивны, но гордятся меньшим количеством событий в своей жизни.'
    })
  }})
  observeEvent(input$startSurvey, {
    hide("introText") 
  })

}



shinyApp(ui = ui, server = server)

