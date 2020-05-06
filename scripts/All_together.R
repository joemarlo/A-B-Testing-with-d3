source("scripts/helper_functions.R")
library(broom)
library(parallel)

# set number of cores available for parallel processing
cpu_cores <- detectCores()

# set number of simulations to 10,000
simulations <- 100

# set alpha of a/b checks to 0.05
alpha <- 0.05

# set the maximum number of checks
checks <- 20

# difference in effect size translated to mu
effect_size <- seq(0, 0.05, by = 0.01)
base_mean <- 120
mus <- base_mean * (1 + effect_size)

# create grid of all input combinations
input_grid <- crossing(
  n_checks = seq(0, 20, by = 5),
  n_comparisons = seq(0, 20, by = 5),
  effect_size = mus,
  sample_size = seq(1000, 5000, by = 2000)
)

# run the simulation
results <- mclapply(1:simulations, mc.cores = cpu_cores, FUN = function(i){
  
  single.sim <- pmap_dfr(input_grid, function(n_checks, n_comparisons, effect_size, sample_size){
    
    # n_checks = 5
    # n_comparisons = 5
    # effect_size = 125
    # sample_size = 100
    
    # set distributions to conduct t-test on
    A <- rnorm(sample_size, base_mean, 15)
    
    # list of rnorms for for multiple comparisons
    B <- list(replicate(n = n_comparisons, 
                        rnorm(sample_size, effect_size, 15), 
                        simplify = FALSE))
    
    # set sample sizes to conduct checks at (i.e. the intervals)
    looks <- seq(from = 0,
                 to = sample_size,
                 length.out = n_checks) %>%
      ceiling() %>% 
      .[-1]
    
    # create combinations of looks and B vectors
    looks_checks_combinations <- crossing(looks = looks, 
             Bs = B) %>% 
      unnest(Bs)
    
    # 'look' at the data at each interval and every multiple comparison, and run a t-test
    p.vals <- map2_dbl(looks_checks_combinations$looks, looks_checks_combinations$Bs, function(look, B) {
      t.test(A[1:look],
             B[1:look],
             var.equal = TRUE)$p.value
    })
    
    # check to see if any of the pvalues are below alpha
    false_positive <- any(p.vals < alpha)
    
    return(tibble(sim = i, n_checks = n_checks, n_comparisons = n_comparisons ,
                  effect_size = effect_size, sample_size = sample_size, 
                  false_positive = false_positive))
  })
  
  return(single.sim)
}) %>% bind_rows()


# add column identifying what the correct outcome of the test should be
results$true_result <- results$effect_size > base_mean

# plot the accuracy
results %>% 
  group_by(effect_size, sample_size, n_checks, n_comparisons) %>%
  summarize(`Accuracy` = mean(false_positive == true_result)) %>% 
  ggplot(aes(x = n_checks, y = `Accuracy`)) +
  geom_point() + 
  geom_line() +
  scale_x_continuous(breaks = c(1, 5, 10, 15, 20)) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
  labs(title = "Frequent stoppage increases false positives",
       subtitle = paste0(scales::comma(simulations), " simulations"),
       x = "Number of stops") +
  facet_grid(effect_size ~ sample_size)



# Generate base vector ----------------------------------------------------

base_vector <- rnorm(10000, 50, 20)

write_csv(x = ., path = "d3/base_vector.csv")


# https://www.lexjansen.com/nesug/nesug10/hl/hl07.pdf




data <- list(
  id = c("John", "Jane"),
  greeting = c("Hello.", "Bonjour."),
  sep = c("! ", "... ")
)

data %>%
  cross_df() %>%
  map(lift(paste))
