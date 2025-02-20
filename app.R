# from https://hbctraining.github.io/Training-modules/RShiny/lessons/shinylive.html#:~:text=In%20order%20for%20GitHub%20Pages,the%20files%20output%20from%20Shinylive.

library(shiny)
library(ggplot2)
library(DT)

ui <- fluidPage(
  plotOutput("plot", brush = "plot_brush"),
  DTOutput("table")
)

server <- function(input, output) {
  output$plot <- renderPlot(
    ggplot(mtcars) +
      geom_point(aes(x = mpg, y = disp))
  )
  output$table <- renderDT({
    brushedPoints(mtcars, input$plot_brush)
  })
}

shinyApp(ui = ui, server = server)

# shinylive::export(appdir = "../shinylive_app/", destdir = "docs")
# httpuv::runStaticServer("docs/", port = 8008)
