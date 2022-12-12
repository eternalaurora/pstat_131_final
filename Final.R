library(tidyverse)
library(tidymodels)
library(corrplot)
library(corrr)
library(glmnet)
library(ranger)
library(xgboost)
library(kernlab)
tidymodels_prefer()
load("football.Rdata")
football_data = football_data %>% mutate(position = as.factor(position)) %>%
  select(-contains(c("minus", "plus", "per", "rate"))) %>%
  select(-c(corner_kicks, blocks, aerial_duels_lost)) %>%
  drop_na()
set.seed(1)
split = initial_split(football_data, strata = rating)
training_set = training(split)
testing_set = testing(split)
folds = vfold_cv(training_set, strata = rating)
football_recipe =
  recipe(rating ~ ., data = football_data %>% select(goals:rating)) %>%
  step_dummy(position) %>%
  step_pca(
    non_penalty_goals,
    shots,
    shots_on_target,
    non_penalty_expected_goals,
    prefix = "shot",
    num_comp = 6
  ) %>%
  step_pca(goals,
           expected_goals,
           prefix = "with_penalty_goal",
           num_comp = 1) %>%
  step_pca(passes_attempted,
           passes_completed,
           prefix = "pass",
           num_comp = 1) %>%
  step_pca(
    short_passes_completed,
    short_passes_attempted,
    prefix = "short_pass",
    num_comp = 1
  ) %>%
  step_pca(
    medium_passes_completed,
    medium_passes_attempted,
    prefix = "medium_pass",
    num_comp = 1
  ) %>%
  step_pca(
    long_passes_completed,
    long_passes_attempted,
    prefix = "long_pass",
    num_comp = 1
  ) %>%
  step_pca(dribblers_tackled,
           dribblers_confronted,
           prefix = "tackling_dribblers",
           num_comp = 1) %>%
  step_pca(dribbles_completed,
           dribbles_attempted,
           prefix = "dribbling",
           num_comp = 1) %>%
  step_pca(pass_targets,
           passes_received,
           prefix = "pass_receiving",
           num_comp = 1) %>%
  step_pca(aerial_duels_won,
           aerial_duels_engaged,
           prefix = "aerial_duel",
           num_comp = 1) %>%
  step_pca(pressures,
           pressure_successes,
           prefix = "pressure",
           num_comp = 1) %>%
  step_pca(tackles_attempted,
           tackles_won,
           prefix = "tackle",
           num_comp = 1) %>%
  step_normalize(all_predictors()) %>%
  step_nzv(all_predictors())
lasso_spec = linear_reg(
  mode = "regression",
  engine = "glmnet",
  penalty = tune(),
  mixture = 1
)
lasso_workflow = workflow() %>% add_model(lasso_spec) %>%
  add_recipe(football_recipe)
lasso_grid = grid_regular(penalty(), levels = 10)
lasso_tune =
  tune_grid(
    lasso_workflow,
    resamples = folds,
    grid = lasso_grid,
    metrics = metric_set(rsq, rmse, mae)
  )
lasso_interaction_workflow = workflow() %>% add_model(lasso_spec) %>%
  add_recipe(football_recipe %>%
               step_interact(terms = ~ starts_with("position"):.))
lasso_interaction_tune =
  tune_grid(
    lasso_interaction_workflow,
    resamples = folds,
    grid = lasso_grid,
    metrics = metric_set(rsq, rmse, mae)
  )
elastic_net_spec = linear_reg(
  mode = "regression",
  engine = "glmnet",
  penalty = tune(),
  mixture = tune()
)
elastic_net_workflow = workflow() %>% add_model(elastic_net_spec) %>%
  add_recipe(football_recipe)
elastic_net_grid = grid_regular(penalty(), mixture(), levels = 10)
elastic_net_tune =
  tune_grid(
    elastic_net_workflow,
    resamples = folds,
    grid = elastic_net_grid,
    metrics = metric_set(rsq, rmse, mae)
  )
random_forest_spec = rand_forest("regression",
                                 mtry = tune(),
                                 trees = tune(),
                                 min_n = tune()) %>%
  set_engine("ranger", importance = "impurity")
random_forest_workflow = workflow() %>% add_model(random_forest_spec) %>%
  add_recipe(football_recipe)
random_forest_grid = grid_regular(mtry(range = c(1, 10)),
                                  trees(range = c(1, 1000)),
                                  min_n(), levels = 10)
random_forest_tune = tune_grid(
  random_forest_workflow,
  resamples = folds,
  grid = random_forest_grid,
  metrics = metric_set(rsq, rmse, mae)
)
boosted_tree_spec = boost_tree("regression", trees = tune())
boosted_tree_workflow = workflow() %>% add_model(boosted_tree_spec) %>%
  add_recipe(football_recipe)
boosted_tree_grid = grid_regular(trees(), levels = 10)
boosted_tree_tune = tune_grid(
  boosted_tree_workflow,
  resamples = folds,
  grid = boosted_tree_grid,
  metrics = metric_set(rsq, rmse, mae)
)
support_vector_machine_spec =
  svm_linear("regression", cost = tune()) %>%
  set_engine("kernlab", scaled = FALSE)
support_vector_machine_workflow = workflow() %>%
  add_model(support_vector_machine_spec) %>%
  add_recipe(football_recipe)
support_vector_machine_grid = grid_regular(cost(), levels = 10)
support_vector_machine_tune = tune_grid(
  support_vector_machine_workflow,
  resamples = folds,
  grid = support_vector_machine_grid,
  metrics = metric_set(rsq, rmse, mae)
)
linear_workflow = workflow() %>% add_model(linear_reg()) %>%
  add_recipe(football_recipe)
linear_fit = linear_workflow %>%
  fit_resamples(resamples = folds, metrics = metric_set(rsq, rmse, mae))
linear_interaction_workflow = workflow() %>% add_model(linear_reg()) %>%
  add_recipe(football_recipe %>%
               step_interact(terms = ~starts_with("position"):.))
linear_interaction_fit = linear_interaction_workflow %>%
  fit_resamples(resamples = folds, metrics = metric_set(rsq, rmse, mae))
save(
  lasso_tune,
  lasso_interaction_tune,
  elastic_net_tune,
  random_forest_tune,
  boosted_tree_tune,
  support_vector_machine_tune,
  linear_fit,
  linear_interaction_fit,
  file = "Final.Rdata"
)
