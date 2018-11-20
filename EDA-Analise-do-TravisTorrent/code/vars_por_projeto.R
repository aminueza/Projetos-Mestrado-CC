library(tidyverse)
library(lubridate)
library(plotly)
library(here)

travis <- read_csv("data/travistorrent_8_2_2017.csv")

# Remove jobs idênticos no mesmo build
travis = travis %>% 
    select(-tr_job_id) %>% 
    group_by(gh_project_name) %>% 
    distinct() %>% 
    ungroup()

projetos = travis %>%
    mutate(commit_date = ymd_hms(gh_first_commit_created_at)) %>%
    group_by(gh_project_name) %>%
    arrange(gh_project_name) %>%
    filter(build_successful == "FALSE") %>%
    summarize( team_first = first(gh_team_size),
        team_last = last(gh_team_size),
        new_members = team_last - team_first,
        median_team = median(gh_team_size),
        lang = last(gh_lang),
        merge = sum(gh_is_pr, na.rm=TRUE),
        num_commits = sum(as.numeric(gh_num_commits_in_push), na.rm = TRUE),
        commit_team_prop = num_commits / median_team,
        build_failed_prop = sum(!build_successful)) %>% drop_na(build_failed_prop)

projetos[, sapply(projetos, FUN = function(x) !any(x < 0))]
projetos %>% 
    #filter(num_commits > 0) %>% 
 #   filter(tests_successful != "NaN") %>%
 #   filter(lang != "javascript") %>%
    write_csv("projetos.csv")

tempo = travis %>%
    mutate(commit_date = ymd_hms(gh_first_commit_created_at)) %>%
    group_by(gh_project_name) %>%
    arrange(gh_project_name) %>%
    drop_na(tr_log_buildduration) %>%
    summarize( team = median(gh_team_size),
               lang = last(gh_lang),
               build = median(as.numeric(tr_log_buildduration),
               num_commits = sum(as.numeric(gh_num_commits_in_push), na.rm = TRUE) ))

