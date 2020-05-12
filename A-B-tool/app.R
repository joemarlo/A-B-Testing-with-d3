library(shiny)
library(shinyWidgets)
library(tidyverse)


# data load and themes ----------------------------------------------------

summarized_results <- read_csv("prob_effect.csv")

theme_custom <- function() {
    theme_gray() +
        theme(
            panel.grid.minor.y = element_line(color = NA),
            panel.grid.major.y = element_line(color = "gray95"),
            panel.grid.minor.x = element_line(color = NA),
            panel.grid.major.x = element_line(color = "gray95"),
            panel.background = element_rect(fill = NA),
            plot.background = element_rect(
                fill = NA,
                color = "gray95",
                size = 10
            ),
            plot.margin = unit(c(1, 1, 1, 1), "cm"),
            axis.title = element_text(color = "gray30"),
            axis.ticks = element_line(color = NA),
            strip.background = element_rect(fill = "gray95"),
            strip.text = element_text(
                color = "gray30",
                size = 11,
                face = "bold"
            ),
            plot.title = element_text(color = "gray30",
                                      face = "bold"),
            plot.subtitle = element_text(size = 10,
                                         color = "gray30"),
            text = element_text(family = "Helvetica"),
            plot.caption = element_text(face = "italic",
                                        size = 6,
                                        color = 'grey50'),
            legend.title = element_blank(),
            legend.position = 'bottom',
            legend.key = element_rect(fill = NA)
        )
}

theme_set(theme_custom())

# wrap ggplot() with a new default color palette
ggplot <- function(...){ ggplot2::ggplot(...) + 
        scale_color_brewer(palette = "Dark2") +
        scale_fill_brewer(palette = "Set1")
}



# UI ----------------------------------------------------------------------


# Define UI for application that draws a histogram
ui <- fluidPage(theme = "my-shiny.css",
                
    # download roboto font
    HTML('<link rel="stylesheet" href="//fonts.googleapis.com/css?family=Roboto:400,300,700,400italic">'),
                
    # choose default slider skin
    chooseSliderSkin(skin = "Flat",
                     color = "#374f48"),
    
    htmlOutput("probability_results"),

    sidebarLayout(
        
        sidebarPanel(
        
            sliderTextInput(
                "n_stops",
                "Number of stops",
                choices = c(1, 2, 3, 5, 10, 20),
                selected = 1
            ),
            sliderTextInput(
                "n_comparisons",
                "Number of comparisons",
                choices = c(1, 2, 3, 5, 10, 20),
                selected = 1
            ),
            sliderTextInput(
                "effect_size",
                "Effect size (as proportion of mean)",
                choices = c(0, 0.01, 0.02, 0.05, 0.10, 0.20),
                selected = 0
            ),
            sliderTextInput(
                "sample_size",
                "Sample size",
                choices = c(100, 1000, 5000, 10000),
                selected = 1000
            ),
            sliderTextInput(
                "std_dev",
                "Spread (standard deviation as proportion of mean)",
                choices = c(2.25, 4.50, 9.00),
                selected = 4.50
            )
    ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot")
        )
    ),
    
    HTML('<div class="belowplot" >
         <p>Probability estimates based on 5,000 simulations</p>
         <a href="https://www.marlo.works/posts/a-b-testing/">marlo.works/A-B-Testing</a>
         </div>'),
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$probability_results <- renderText({
        
        probs <- summarized_results %>% 
            filter(n_checks == input$n_stops,
                   n_comparisons == input$n_comparisons,
                   effect_size == input$effect_size,
                   sample_size == input$sample_size,
                   std_dev == input$std_dev) %>% 
            pull(Probability_of_finding_an_effect)
        
        paste0('<h2>Pulling it all together<br> <span class="subheader">Probabililty of finding at least one effect: &nbsp</span> ~ ', 
               round(min(0.99, max(0.05, probs)), 2),
               '</h2>')
    })
    
    output$distPlot <- renderPlot({
        
        # simulate random distributions
        dat <-
            tibble(
                As = as.vector(replicate(input$n_comparisons, rnorm(
                    n = input$sample_size, 
                    mean = 45, 
                    sd = input$std_dev
                ))),
                Bs = as.vector(replicate(input$n_comparisons, rnorm(
                    n = input$sample_size, 
                    mean = 45 * (1 + input$effect_size), 
                    sd = input$std_dev
                ))),
                ID = rep(1:input$n_comparisons, each = input$sample_size)
            )
        
        # calculate means for each distribution
        means <- dat %>% group_by(ID) %>% summarize(meanA = mean(As),
                                                     meanB = mean(Bs))
        
        # draw the plot
        # dat %>% 
        #     ggplot() +
        #     geom_density(aes(x = As), fill = "grey20", alpha = 0.6) +
        #     geom_density(aes(x = Bs), fill = "#4e917e", alpha = 0.6) +
        #     geom_vline(data = means, aes(xintercept = meanA), color = 'black') +
        #     geom_vline(data = means, aes(xintercept = meanB), color = '#6fd9bb') +
        #     scale_x_continuous(labels = NULL, lim = c(20, 70)) +
        #     scale_y_continuous(labels = NULL) +
        #     facet_wrap(~ID) +
        #     labs(x = "Minutes spent on site",
        #          y = NULL) +
        #     theme(legend.position = "top")

        fill_colors <- c("A" = "grey20", "B" = "#4e917e")
        dat %>% 
            ggplot() +
            geom_density(aes(x = As, fill = "A"), alpha = 0.6) +
            geom_density(aes(x = Bs, fill = "B"), alpha = 0.6) +
            geom_vline(data = means, aes(xintercept = meanA), color = 'black') +
            geom_vline(data = means, aes(xintercept = meanB), color = '#6fd9bb') +
            scale_x_continuous(labels = NULL, lim = c(20, 70)) +
            scale_y_continuous(labels = NULL) +
            scale_fill_manual(values = fill_colors) +
            facet_wrap(~ID) +
            labs(title = "Distributions of A and B with respective means",
                 x = NULL,
                 y = NULL) +
            theme(legend.position = "bottom")
        
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
