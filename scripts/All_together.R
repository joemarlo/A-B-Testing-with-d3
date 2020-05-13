source("scripts/helper_functions.R")
library(broom)
library(parallel)

# set number of cores available for parallel processing
cpu_cores <- detectCores()

# set number of simulations
simulations <- 5000

# set alpha
alpha <- 0.05

# difference in effect size and spread
effect_sizes <- c(0, 0.01, 0.02, 0.05, 0.10, 0.20)
base_mean <- 45
mus <- base_mean * (1 + effect_sizes)
sds_offset <- c(0.01, 0.05, 0.1, 0.20)
sds <- base_mean * sds_offset

# create grid of all input combinations
input_grid <- crossing(
  n_checks = c(1, 2, 3, 5, 10, 20),
  n_comparisons = c(1, 2, 3, 5, 10, 20),
  effect_size = mus,
  std_dev = sds,
  sample_size = c(100, 1000, 5000, 10000),
)

# run the simulation
results <- mclapply(1:simulations, mc.cores = cpu_cores, FUN = function(i){
  
  single.sim <- pmap_dfr(input_grid, function(n_checks, n_comparisons, effect_size, std_dev, sample_size){
    
    # create a list of base distributions to conduct t-test on
    As <- replicate(n = n_comparisons,
                    rnorm(sample_size, base_mean, std_dev),
                    simplify = FALSE)
    
    # create a list of distributions to compare the base distribution against (multiple comparisons)
    Bs <- replicate(n = n_comparisons,
                    rnorm(sample_size, effect_size, std_dev),
                    simplify = FALSE)
    
    # set sample sizes to conduct checks at (i.e. the intervals)
    looks <- seq(from = sample_size / n_checks,
                 to = sample_size,
                 by = sample_size / n_checks) %>%
      ceiling()
    
    # create combinations of As, Bs, and looks
    looks_checks_combinations <- tibble(As = As,
                                        Bs = Bs) %>%
      crossing(looks)
    
    # 'look' at the data at each interval and every multiple comparison, then run a t-test
    p.vals <- pmap_dbl(list(
      looks_checks_combinations$As,
      looks_checks_combinations$Bs,
      looks_checks_combinations$looks
    ),
    function(A, B, look) {
      t.test(A[1:look],
             B[1:look],
             var.equal = TRUE)$p.value
    })
    
    # check to see if any of the pvalues are below alpha
    at_least_one_effect_detected <- any(p.vals < alpha)
    
    return(tibble(sim = i, n_checks = n_checks, n_comparisons = n_comparisons ,
                  effect_size = effect_size, std_dev = std_dev, sample_size = sample_size, 
                  effect_detected = at_least_one_effect_detected))
  })
  
  return(single.sim)
}) %>% bind_rows()

# visualize results
# too many dimensions so need to filter
results %>% 
  filter(n_checks == 1) %>% 
  group_by(effect_size, sample_size, n_comparisons) %>%
  summarize(Probability_of_finding_an_effect = mean(effect_detected)) %>% 
  ggplot(aes(x = n_comparisons, y = Probability_of_finding_an_effect)) +
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
  group_by(n_checks, n_comparisons, effect_size, sample_size, std_dev) %>%
  summarize(Probability_of_finding_an_effect = mean(effect_detected)) %>% 
  ungroup()

# write out dataframe to be used in d3
summarized_results %>% 
  rowwise() %>% 
  # replace effect size with original relative value
  # replace sd size with original relative value
  mutate(effect_size = effect_sizes[effect_size == mus],
         std_dev = sds_offset[std_dev == sds]) %>% 
  ungroup() %>% 
  write_csv(path = "d3/prob_effect.csv")


# examine results ---------------------------------------------------------
summarized_results <- read_csv("d3/prob_effect.csv")
# 
# expanded_grid <- crossing(
#   n_checks = 1:10,
#   n_comparisons = 1:10,
#   effect_size = mus,
#   sample_size = seq(1000, 5000, 2000)
# )


# Generate base vector ----------------------------------------------------

# base_vector <- rnorm(10000, 50, 20)

# write_csv(x = ., path = "d3/base_vector.csv")


# https://www.lexjansen.com/nesug/nesug10/hl/hl07.pdf

