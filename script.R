# carrega os pacotes
if (!require('pacman')) install.packages('pacman')
pacman::p_load(dplyr, purrr, furrr, readxl, tictoc)



# A
stats <- read_excel('data.xlsx')
stats %>% head()

stats_mean <- stats %>% 
  mutate(GF_mean = GF/Games,
         GA_mean = GA/Games) %>%
  select(Country, GF_mean, GA_mean)
stats_mean %>% head()

extract <- function(country='Brazil', G='GF_mean', data=stats_mean) {
  stats_mean %>%
    filter(Country == country) %>%
    select(any_of(G)) %>%
    pull()
}

lambda_ij <- (extract() + extract('Serbia', 'GA_mean'))/2
lambda_ji <- (extract('Serbia') + extract(G='GA_mean'))/2

placar <- c(5, 0)
lambdas <- c(lambda_ij, lambda_ji)
(prob <- placar %>% dpois(lambdas) %>% prod())

nsim <- 1e6
goals <- rpois(2*nsim, rep(lambdas, each=nsim))
scores <- tibble(Brazil=goals[1:nsim], Serbia=goals[(nsim+1):(2*nsim)])

(prob_mc <- scores %>% filter(Brazil == 5 & Serbia == 0) %>% nrow() / nrow(scores))



# B
countries <- c('Brazil', 'Serbia', 'Switzerland', 'Cameroon')
games <- combn(x=countries,  m=2) %>%
  t() %>%
  as_tibble() %>%
  setNames(c('home', 'visitor'))
lambdas <- games %>%
  left_join(stats_mean, by=join_by(home == Country)) %>%
  left_join(stats_mean, by=join_by(visitor == Country), suffix = c('_i', '_j')) %>%
  mutate(lambda_ij = (GF_mean_i + GA_mean_j)/2,
         lambda_ji = (GF_mean_j + GA_mean_i)/2)

simulation <- function(i){
  board <- lambdas
  board$GF_i <- rpois(n=nrow(lambdas), lambda=lambdas$lambda_ij)
  board$GF_j <- rpois(n=nrow(lambdas), lambda=lambdas$lambda_ji)
  board <- board %>%
    mutate(result_i = case_when(GF_i > GF_j ~ '+', 
                                GF_i < GF_j ~ '-',
                                TRUE ~ '='),
           result_j = case_when(GF_j > GF_i ~ '+', 
                                GF_j < GF_i ~ '-',
                                TRUE ~ '='))
  outcome_i <- board %>% mutate(GA_i = GF_j) %>% select(home, GF_i, GA_i, result_i) 
  outcome_j <- board %>% mutate(GA_j = GF_i) %>% select(visitor, GF_j, GA_j, result_j)
  colnames(outcome_j) <- colnames(outcome_i)
  outcome <- outcome_i %>% bind_rows(outcome_j)
  outcome$points <- ifelse(outcome$result_i == '+', 3, ifelse(outcome$result_i == '=', 1, 0))
  outcome$cards <- rpois(n=12, lambda=2)
  class <- outcome %>%
    group_by(home) %>%
    summarise(points=sum(points), 
              GF=sum(GF_i),
              GA=sum(GA_i),
              DIFF=GF-GA,
              CARDS=sum(cards)) %>%
    arrange(desc(points), desc(DIFF), desc(GF), CARDS) %>%
    mutate(pos = 1:n())
  output <- board %>%
    slice(1:3) %>%
    select(result_i) %>%
    t() %>%
    cbind(class %>% filter(home == 'Brazil') %>% select(pos) %>% pull() <= 2) %>%
    as_tibble() %>%
    setNames(c('serbia', 'switzerland', 'cameroon', 'classified'))
  output$classified <- as.logical(output$classified)
  row.names(output) <- i
  return(output)
}

tic()
plan(multisession, workers=4)
result <- future_map(1:1e4, ~ simulation(.x))
toc()

final <- do.call(bind_rows, result)
final %>% filter(serbia == '+') %>% summarise(mean(classified))
final %>% filter(switzerland == '+') %>% summarise(mean(classified))
final %>% filter(cameroon == '+') %>% summarise(mean(classified))