source("scripts/helper_functions.R")
library(broom)
library(ggrepel)

# Generate Distributions
A <- tibble(A = rnorm(250, 45, 15))

B <- tibble(B = c(rnorm(100, 30, 20), rnorm(150, 50,18)))

# create variable to represent medium of site engagement
Medium <- c(rep("Computer", 100), rep("Mobile", 150))

# combine into a single df
study <- cbind(A, B, Medium) %>%
  as_tibble() %>%
  pivot_longer(cols = 1:2) %>%
  filter(value >= 0) %>%
  mutate(conditional = str_c(name, Medium, sep =  " "))

# pull out A and B results for naive A/B test
A <- study %>% filter(name == "A") %>% select(value) %>% as_vector()
B <- study %>% filter(name == "B") %>% select(value) %>% as_vector()

# implement test
ttest_results <- t.test(A, B)


# plot data 
ggplot(study, aes(value, fill = name)) + 
  geom_density(alpha = .7) + 
  geom_label(x = 3, y = 0.022, fill = 'white', label.size = NA, hjust = 0, color = 'grey30',
           label = paste0("A = ", round(mean(A), 1), " minutes\nB = ", round(mean(B), 1), " minutes\np-value = ", round(ttest_results$p.value, 2))) +
  labs(title = "A/B test results",
       x = "Minutes spent on site", 
       y = NULL) + 
  theme(legend.title = element_blank(),
        legend.position = c(0.8, 0.8),
        legend.box.background = element_rect(color = 'white'), 
        axis.text.y = element_blank())

save_plot(name = 'no_interaction')


# conduct a test to specify engagement types
reg.output <- tidy(summary(lm(value~ name*Medium, study)))
reg.output[2:4,1] <- c("B", "Mobile", "B x Mobile Interaction")

# text for facet annotations
ann_text <- data.frame(value = c(20, 20), 
                       lab = c(
                         "A = 43.5  B = 32.4\np-value < 0.01",
                         "A = 45.3  B = 51.16\np-value < 0.01"), 
                       name = NA,
                       Medium = c('Computer', 'Mobile'))

# create seperate facets by medium type
ggplot(study, aes(value, fill = name)) + 
  geom_density(alpha = .7) + 
  geom_label(data = ann_text, aes(label = lab), y = -0.0135,
             fill = 'white', label.size = NA, hjust = 0, color = 'grey30') +
  labs(title = "A/B test results with interaction",
       x = "Minutes spent on site", 
       y = NULL) + 
  facet_wrap(~Medium) + 
  coord_cartesian(ylim = c(0, 0.03), # This focuses the x-axis on the range of interest
                  clip = 'off') +
  theme(legend.title = element_blank(),
        legend.position = c(0.5, 0.7),
        legend.box.background = element_rect(color = 'white'), 
        axis.text.y = element_blank(),
        plot.margin = unit(c(1, 1, 2, 1), "cm"))

save_plot(name = 'interaction')
