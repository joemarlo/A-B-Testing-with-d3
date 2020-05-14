library(shiny)
library(shinyWidgets)
library(tidyverse)
set.seed(44)

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
                size = 12
            ),
            plot.title = element_text(color = "gray30",
                                      size = 16),
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

# slider text vectors
effect_size_values <- c(0, 0.01, 0.02, 0.05, 0.10, 0.20)
sample_size_values <- c(100, 1000, 5000, 10000)
sample_size_text <- c("100", "1,000", "5,000", "10,000")
spread_values <- c(0.01, 0.05, 0.10, 0.20)


# UI ----------------------------------------------------------------------


# Define UI for application that draws a histogram
ui <- fluidPage(theme = "my-shiny.css",
                
    # download roboto and inconsolata font
    HTML('<link rel="stylesheet" href="//fonts.googleapis.com/css?family=Roboto:400,300,700,400italic">'),
    HTML('<link rel="stylesheet" href="https://fonts.googleapis.com/css?family=Inconsolata">'),
                
    # choose default slider skin
    chooseSliderSkin(skin = "Flat",
                     color = "#374f48"),
    
    htmlOutput("probability_results"),

    sidebarLayout(
        
        sidebarPanel(
            
            sliderTextInput(
                "effect_size",
                "The true effect size between A and B (as proportion of mean)",
                choices = effect_size_values,
                selected = 0
            ),
            sliderTextInput(
                "n_stops",
                "How many times you stop and check the data",
                choices = c(1, 2, 3, 5, 10, 20),
                selected = 1
            ),
            sliderTextInput(
                "n_comparisons",
                "How many comparisons are conducted",
                choices = c(1, 2, 3, 5, 10, 20),
                selected = 1
            ),
            sliderTextInput(
                "sample_size",
                "Sample size",
                choices = sample_size_text,
                selected = "1,000"
            ),
            sliderTextInput(
                "spread",
                "Spread (standard deviation as proportion of mean)",
                choices = spread_values,
                selected = 0.1
            )
    ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot")
        )
    ),
    
    HTML('<div class="belowplot" >
         <p>Probability estimates based on 5,000 simulations @ alpha = 0.05</p>
         <p>See 
         <a href="https://www.marlo.works/posts/a-b-testing/" target="_blank">marlo.works/A-B-Testing</a>
          for more info</p>
         </div>')
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    # look up variables from the slider text to the actuql value
    sample_size <- reactive({sample_size_values[input$sample_size == sample_size_text]})
    
    # filter dataset to inputs and pull out probability
    output$probability_results <- renderText({
        
        probs <- summarized_results %>%
            filter(n_checks == input$n_stops,
                   n_comparisons == input$n_comparisons,
                   effect_size == input$effect_size,
                   sample_size == sample_size(),
                   std_dev == input$spread) %>%
            pull(Probability_of_finding_an_effect)

        paste0('<h2>A/B testing: pulling it all together<br>
               <span class="subheader">Probability of finding at least one effect: &nbsp </span>',
               '<span class="emphasis"> ~',
               round(min(0.99, max(0.05, probs)), 2),
               '</span></h2>')
    })
    
    output$distPlot <- renderPlot({

        # simulate random distributions
        dat <-
            tibble(
                As = as.vector(replicate(input$n_comparisons, rnorm(
                    n = sample_size(),
                    mean = 45,
                    sd = 45 * input$spread
                ))),
                Bs = as.vector(replicate(input$n_comparisons, rnorm(
                    n = sample_size(),
                    mean = 45 * (1 + input$effect_size),
                    sd = 45 * input$spread
                )))
            )
        
        # dynamically adjust facet labels so its readable on mobile
        if (input$n_comparisons == 1){
            dat$ID <- "A single comparison between A and B"
        } else if (input$n_comparisons <= 5){
            dat$ID <- factor(
                rep(paste0("Comparison ", 1:input$n_comparisons), each = sample_size()),
                levels = paste0("Comparison ", 1:input$n_comparisons))
        } else {
            dat$ID <- factor(
                rep(c("Comp. 1", 2:input$n_comparisons), each = sample_size()),
                levels =  c("Comp. 1", 2:input$n_comparisons))
        }

        # calculate means for each distribution
        means <- dat %>% 
            group_by(ID) %>% 
            summarize(meanA = mean(As),
                      meanB = mean(Bs))
        
        # facet font size; responsive to number of facets so its readable on mobile
        facet_font <- if_else(input$n_comparisons > 5, 10, 12)
        
        # draw the plot
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
            labs(title = "Distributions of A and B",
                 x = NULL,
                 y = NULL) +
            theme(strip.text = element_text(size = facet_font))

    })
}

# Run the application 
shinyApp(ui = ui, server = server)
