source("scripts/helper_functions.R")
library(broom)
library(parallel)

# set number of cores available for parallel processing
cpu_cores <- detectCores()

# set number of simulations
simulations <- 1000

# set alpha
alpha <- 0.05

# difference in effect size translated to mu
effect_sizes <- seq(0, 0.1, by = 0.01)
base_mean <- 120
std_dev <- 15
mus <- base_mean * (1 + effect_sizes)

# create grid of all input combinations
input_grid <- crossing(
  n_checks = c(1, 5, 10, 20),
  n_comparisons = c(1, 5, 10, 20),
  effect_size = mus,
  sample_size = c(100, 1000, 5000)
)

# run the simulation
results <- mclapply(1:simulations, mc.cores = cpu_cores, FUN = function(i){
  
  single.sim <- pmap_dfr(input_grid, function(n_checks, n_comparisons, effect_size, sample_size){
    
    # set the base distributions to conduct t-test on
    A <- rnorm(sample_size, base_mean, std_dev)
    
    # create a list of distributions to compare the base distribution against (multiple comparisons)
    B <- list(replicate(n = n_comparisons, 
                        rnorm(sample_size, effect_size, std_dev), 
                        simplify = FALSE))
    
    # set sample sizes to conduct checks at (i.e. the intervals)
    looks <- seq(from = sample_size / n_checks,
                 to = sample_size,
                 by = sample_size / n_checks) %>%
      ceiling()
    
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
    at_least_one_effect_detected <- any(p.vals < alpha)
    
    return(tibble(sim = i, n_checks = n_checks, n_comparisons = n_comparisons ,
                  effect_size = effect_size, sample_size = sample_size, 
                  effect_detected = at_least_one_effect_detected))
  })
  
  return(single.sim)
}) %>% bind_rows()

# visualize results
# too many dimensions so need to filter
results %>% 
  filter(n_comparisons == 10) %>% 
  group_by(effect_size, sample_size, n_checks) %>%
  summarize(Probability_of_finding_an_effect = mean(effect_detected)) %>% 
  ggplot(aes(x = n_checks, y = Probability_of_finding_an_effect)) +
  geom_point() + 
  geom_line() +
  scale_x_continuous(breaks = c(1, 5, 10, 15, 20)) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
  labs(title = "TITLE",
       subtitle = paste0(scales::comma(simulations), " simulations"),
       x = "Number of stops") +
  facet_grid(effect_size ~ sample_size)

# probability of finding an effect
summarized_results <- results %>% 
  group_by(n_checks, n_comparisons, effect_size, sample_size) %>%
  summarize(Probability_of_finding_an_effect = mean(effect_detected)) %>% 
  ungroup()

# write out dataframe to be used in d3
summarized_results %>% 
  rowwise() %>% 
  # replace effect size with original relative value
  mutate(effect_size = effect_sizes[effect_size == mus]) %>% 
  ungroup() %>% 
  write_csv(path = "d3/prob_effect.csv")


# Generate base vector ----------------------------------------------------

base_vector <- rnorm(10000, 50, 20)

write_csv(x = ., path = "d3/base_vector.csv")


# https://www.lexjansen.com/nesug/nesug10/hl/hl07.pdf

