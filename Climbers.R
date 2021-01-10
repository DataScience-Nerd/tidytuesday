library(tidyverse)
library(tidymodels)
# Read ----------------------------------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load('2020-09-22')
members <- tuesdata$members

# EDA -----------------------------------------------------------------------------------------

skimr::skim(members)

members %>%
        group_by(year = 10 *(year %/% 10)) %>% 
        summarise(died =mean(died),
                  success= mean(success)) %>% 
        pivot_longer(died:success, names_to = "outcome", values_to = "percent") %>%
        ggplot(aes(year,percent,color = outcome))+
        geom_line(alpha=0.7,size =1.50)+
        scale_y_continuous(labels = scales::percent_format())

members %>%
        group_by(age = 10 *(age %/% 10)) %>% 
        summarise(died =mean(died),
                  success= mean(success)) %>% 
        pivot_longer(died:success, names_to = "outcome", values_to = "percent") %>%
        ggplot(aes(age,percent,color = outcome))+
        geom_line(alpha=0.7,size =1.50)+
        scale_y_continuous(labels = scales::percent_format())

members %>% 
        count(success,died) %>% 
        group_by(success) %>% 
        mutate(percent= n/sum(n))

members %>% count(peak_name,sort = T)

members %>% 
        filter(season != "Unknown") %>% 
        count(season,died) %>% 
        group_by(season) %>% 
        mutate(percent=n/sum(n),
               died =case_when(died~"Died",
                               TRUE~"Did Not Die")) %>% 
        ggplot(aes(season,percent,fill=season))+
        geom_col(show.legend = F,position = "dodge",alpha=0.8)+
        facet_wrap(~died,scale="free")+
        scale_y_continuous(labels = scales::parse_format())

members_df<- members %>% 
        filter(season != "Unknown") %>% 
        select(peak_id,year,season,sex,age, citizenship,hired,success,died) %>% 
        filter(!is.na(sex),!is.na(citizenship)) %>% 
        mutate(died =case_when(died~"Died",
                               TRUE~"Survived")) %>% 
        mutate_if(is.character,factor) %>% 
        mutate_if(is.logical,as.integer)


# Modeling ------------------------------------------------------------------------------------

library(tidymodels)
library(themis)

set.seed(123)
members_split <- initial_split(members_df, strata = died)
members_train <- training(members_split)
members_test <- testing(members_split)

set.seed(123)
members_folds <- vfold_cv(members_train, strata = died)
members_folds

members_rec <- recipe(died ~ ., data = members_train) %>%
        step_medianimpute(age) %>%
        step_other(peak_id, citizenship) %>%
        step_dummy(all_nominal(), -died) %>%
        step_smote(died)

members_rec

glm_spec <- logistic_reg() %>%
        set_engine("glm")

glm_spec

rf_spec <- rand_forest(trees = 1000) %>%
        set_mode("classification") %>%
        set_engine("ranger")

rf_spec

# Workflow

members_wf <- workflow() %>%
        add_recipe(members_rec)

members_wf

members_metrics <- metric_set(roc_auc, accuracy, sensitivity, specificity)

doParallel::registerDoParallel()
glm_rs <- members_wf %>%
        add_model(glm_spec) %>%
        fit_resamples(
                resamples = members_folds,
                metrics = members_metrics,
                control = control_resamples(save_pred = TRUE)
        )

glm_rs

rf_rs <- members_wf %>%
        add_model(rf_spec) %>%
        fit_resamples(
                resamples = members_folds,
                metrics = members_metrics,
                control = control_resamples(save_pred = TRUE)
        )

rf_rs

# Evaluate Models -----------------------------------------------------------------------------

collect_metrics(glm_rs)
collect_metrics(rf_rs)

glm_rs %>%
        conf_mat_resampled()

rf_rs %>%
        conf_mat_resampled()

glm_rs %>%
        collect_predictions() %>%
        group_by(id) %>%
        roc_curve(died, .pred_died) %>%
        ggplot(aes(1 - specificity, sensitivity, color = id)) +
        geom_abline(lty = 2, color = "gray80", size = 1.5) +
        geom_path(show.legend = FALSE, alpha = 0.6, size = 1.2) +
        coord_equal()

members_final <- members_wf %>%
        add_model(glm_spec) %>%
        last_fit(members_split)

members_final

collect_metrics(members_final)

collect_predictions(members_final) %>%
        conf_mat(died, .pred_class)

members_final %>%
        pull(.workflow) %>%
        pluck(1) %>%
        tidy(exponentiate = TRUE) %>%
        arrange(estimate) %>%
        kable(digits = 3)

members_final %>%
        pull(.workflow) %>%
        pluck(1) %>%
        tidy() %>%
        filter(term != "(Intercept)") %>%
        ggplot(aes(estimate, fct_reorder(term, estimate))) +
        geom_vline(xintercept = 0, color = "gray50", lty = 2, size = 1.2) +
        geom_errorbar(aes(
                xmin = estimate - std.error,
                xmax = estimate + std.error
        ),
        width = .2, color = "gray50", alpha = 0.7
        ) +
        geom_point(size = 2, color = "#85144B") +
        labs(y = NULL, x = "Coefficent from logistic regression")
