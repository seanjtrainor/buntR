#READ IN NEEDED PACKAGES

usePackage <- function(p) {
  if(!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}


listPackages <- c("baseballr", "dplyr", "RcppRoll", "zoo", "lubridate", "tidyverse",
                  "tree", "caret", "randomForest", "rvest", "ggalt", "remotes", "gbm", "glmnet")

sapply(listPackages, usePackage)
remotes::install_github("seanjtrainor/shiftr")

#Set variables for different out situations to create IP field

two_out_ie_events <- c('caught_stealing_2b', 'caught_stealing_3b', 'field_out', 'fielders_choice', 'fielders_choice_out',
                       'force_out', 'strikeout', 'other_out')

single_out_events <- c('caught_stealing_2b', 'caught_stealing_3b', 'field_out', 'fielders_choice', 'fielders_choice_out',
                       'force_out', 'strikeout', 'sac_bunt', 'sac_fly')

double_out_events <- c('double_play', 'grounded_into_double_play', 'strikeout_double_play')

three_out_events <- c('triple_play')

#Bring in Chadwick

chadwick_player_lu_table <- subset(chadwick_player_lu(), !is.na(key_mlbam)) %>%
  dplyr::select(key_mlbam,key_fangraphs, name_last, name_first) %>%
  dplyr::mutate(hitter_name = paste(name_first, name_last))

#bring in sprint speeds

sprint.speed18 <- read.csv("~/Desktop/baseball/shiftr/sprint_speed (4).csv") %>%
  dplyr::select(player_id, sprint_speed) %>% 
  dplyr::mutate(game_year = "2018")

sprint.speed19 <- read.csv("~/Desktop/baseball/shiftr/sprint_speed (1).csv") %>%
  dplyr::select(player_id, sprint_speed) %>% 
  dplyr::mutate(game_year = "2019")

sprint.speed20 <- read.csv("~/Desktop/baseball/shiftr/sprint_speed (2).csv") %>%
  dplyr::select(player_id, sprint_speed) %>% 
  dplyr::mutate(game_year = "2020")

sprint.speed21 <- read.csv("~/Desktop/baseball/shiftr/sprint_speed (3).csv") %>%
  dplyr::select(player_id, sprint_speed) %>% 
  dplyr::mutate(game_year = "2021")

sprint.speed22 <- read.csv("~/Desktop/baseball/shiftr/sprint_speed (6).csv") %>%
  dplyr::select(player_id, sprint_speed) %>% 
  dplyr::mutate(game_year = "2022")

sprint.speed <- rbind(sprint.speed18, sprint.speed19, sprint.speed20, sprint.speed21, sprint.speed22)

sprint_max <- sprint.speed %>%
  group_by(player_id) %>%
  filter(game_year == max(game_year)) %>%
  select(-c(game_year))

rm(sprint.speed18, sprint.speed19, sprint.speed20, sprint.speed21, sprint.speed22)

#BRING IN DATA

pbp <- rbind(season_pbp(2022),season_pbp(2021), season_pbp(2020), season_pbp(2019), season_pbp(2018))

#Create innings pitched field

innings <- pbp %>% group_by(game_date,game_year,pitcher,p_throws, home_team,away_team,p_team, p_league, inning_topbot) %>%
  arrange(game_date,pitcher, at_bat_number, pitch_number) %>%
  summarise(innings = last(inning),
            last_events = last(events),
            outs = last(outs_when_up))%>%
  mutate(IP = ifelse(outs==2 && last_events %in% two_out_ie_events, as.numeric(innings), 
                     ifelse(outs==1 && last_events %in% double_out_events, as.numeric(innings),
                            ifelse(outs==0 && last_events == 'triple_play', as.numeric(innings),
                                   ifelse(last_events %in% single_out_events,(as.numeric(innings)-1)+((as.numeric(outs)+1)/3),
                                          ifelse(last_events %in% double_out_events,(as.numeric(innings)-1)+((as.numeric(outs)+2)/3),
                                                 (as.numeric(innings)-1)+((as.numeric(outs)/3)))))))) %>%
  select(game_date,game_year, pitcher, p_throws, home_team, away_team, p_team, p_league, inning_topbot, IP)

HIP <- filter(pbp21, grepl("bunt", des) == TRUE)

###################################################
#Pitch summary
################################################

pitch_sum <- pbp %>% group_by(game_date,game_year, pitcher, p_throws, home_team,league, away_team, p_team, p_league, inning_topbot,stand) %>%
  summarise(BF = n_distinct(at_bat_number),
            uBB = sum(events == "walk", na.rm = TRUE),
            HBP = sum(events == "hit_by_pitch", na.rm = TRUE),
            X1B = sum(events == "single", na.rm = TRUE),
            X2B = sum(events == "double", na.rm = TRUE),
            X3B = sum(events == "triple", na.rm = TRUE),
            HR = sum(events == "home_run", na.rm = TRUE),
            SH = sum(substr(events,1,3) == "sac", na.rm = TRUE),
            SO = sum(substr(events,1,9) == "strikeout", na.rm = TRUE),
            FB = sum(bb_type == "fly_ball", na.rm = TRUE),
            GB = sum(bb_type == "ground_ball", na.rm = TRUE),
            FB_HR = sum(events == "home_run" & bb_type == "fly_ball", na.rm = TRUE),
            PF = mean(PF)) %>%
  mutate(AB = BF - (uBB + HBP + SH)
         #     ,HR_FB_rate = FB_HR/FB
  ) %>%
  left_join(innings, by = c("game_date","game_year", "pitcher", "p_throws", "home_team", "away_team", "p_team", "p_league", "inning_topbot")) %>%
  arrange(pitcher, stand, game_date)

pitch_cum_r <- roll_fip(pitch_sum) %>%
  filter(stand == "R") %>%
  arrange(pitcher, stand, game_date) %>%
  select("game_date","game_year", "pitcher", "AB", "uBB", "SO", "FIP", "stand") %>%
    mutate(cum_p_k_rate_r = SO/AB,
           cum_p_bb_rate_r = uBB/AB) %>%
    rename(FIP_cum_r = FIP) %>%
  ungroup() %>%
  select(-c("AB", "uBB", "SO", "stand"))
  
pitch_cum_l <- roll_fip(pitch_sum) %>%
  filter(stand == "L") %>%
  arrange(pitcher, stand, game_date) %>%
  select("game_date","game_year", "pitcher", "AB", "uBB", "SO", "FIP", "stand") %>%
    mutate(cum_p_k_rate_l = SO/AB,
           cum_p_bb_rate_l = uBB/AB) %>%
    rename(FIP_cum_l = FIP) %>%
  ungroup() %>%
  select(-c("AB", "uBB", "SO", "stand"))


pitch_cum_tot <- roll_fip(pitch_sum, rl_excl = "Y") %>%
  arrange(pitcher, game_date) %>%
  select("game_date", "game_year", "pitcher", "AB", "uBB", "SO", "FIP") %>%
  mutate(cum_p_k_rate = SO/AB,
         cum_p_bb_rate = uBB/AB) %>%
  rename(FIP_cum_tot = FIP) %>%
  ungroup() %>%
  select(-c("AB", "uBB", "SO"))

pitch_rol_r <- roll_fip(pitch_sum, 5) %>%
  filter(stand == "R") %>%
  arrange(pitcher, stand, game_date) %>%
  select("game_date", "game_year", "pitcher", "AB", "uBB", "SO", "FIP", "stand") %>%
  mutate(rol_p_k_rate_r = SO/AB,
         rol_p_bb_rate_r = uBB/AB) %>%
  rename(FIP_rol_r = FIP) %>%
  ungroup() %>%
  select(-c("AB", "uBB", "SO", "stand"))

pitch_rol_l <- roll_fip(pitch_sum, 5) %>%
  filter(stand == "L") %>%
  arrange(pitcher, stand, game_date) %>%
  select("game_date","game_year", "pitcher", "AB", "uBB", "SO", "FIP", "stand") %>%
  mutate(rol_p_k_rate_l = SO/AB,
         rol_p_bb_rate_l = uBB/AB) %>%
  rename(FIP_rol_l = FIP) %>%
  ungroup() %>%
  select(-c("AB", "uBB", "SO", "stand"))


pitch_rol_tot <- roll_fip(pitch_sum, 5, rl_excl = "Y") %>%
  arrange(pitcher, game_date) %>%
  select("game_date","game_year", "pitcher", "AB", "uBB", "SO", "FIP") %>%
  mutate(rol_p_k_rate = SO/AB,
         rol_p_bb_rate = uBB/AB) %>%
  rename(FIP_rol_tot = FIP) %>%
  ungroup() %>%
  select(-c("AB", "uBB", "SO"))

pitch_all <- pitch_cum_tot %>%
  left_join(pitch_cum_l, by = c("game_date", "pitcher", "game_year")) %>%
  left_join(pitch_cum_r, by = c("game_date", "pitcher", "game_year")) %>%
  left_join(pitch_rol_tot, by = c("game_date", "pitcher", "game_year")) %>%
  left_join(pitch_rol_l, by = c("game_date", "pitcher", "game_year")) %>%
  left_join(pitch_rol_r, by = c("game_date", "pitcher", "game_year")) %>%
  distinct() %>%
  group_by(pitcher, game_year) %>%
  mutate(real_dt = lag(game_date))

pitch_max <- pitch_all %>%
  group_by(pitcher) %>%
  filter(game_date == max(game_date)) %>%
  select(-c(game_year, game_date, real_dt))

p_throw <- pbp %>% group_by(pitcher, p_throws) %>%
  summarise(cnt = n_distinct(at_bat_number)) %>%
  filter(cnt == max(cnt)) %>%
  select(-cnt)

rm(pitch_cum_tot, pitch_cum_l, pitch_cum_r, pitch_rol_tot, pitch_rol_l, pitch_rol_r)

dup_check <- pitch_all[which(duplicated(paste(pitch_all$pitcher, pitch_all$game_date))),]



###################################################
#Batting summary
################################################

bat_sum <- pbp %>% group_by(game_date,game_year, batter, p_throws) %>%
  summarise(BF = n_distinct(at_bat_number),
            uBB = sum(events == "walk", na.rm = TRUE),
            HBP = sum(events == "hit_by_pitch", na.rm = TRUE),
            X1B = sum(events == "single", na.rm = TRUE),
            X2B = sum(events == "double", na.rm = TRUE),
            X3B = sum(events == "triple", na.rm = TRUE),
            HR = sum(events == "home_run", na.rm = TRUE),
            SH = sum(substr(events,1,3) == "sac", na.rm = TRUE),
            SO = sum(substr(events,1,9) == "strikeout", na.rm = TRUE),
            FB = sum(bb_type == "fly_ball", na.rm = TRUE),
            GB = sum(bb_type == "ground_ball", na.rm = TRUE),
            FB_HR = sum(events == "home_run" & bb_type == "fly_ball", na.rm = TRUE),
            PF = mean(PF)) %>%
  mutate(AB = BF - (uBB + HBP + SH)
         #     ,HR_FB_rate = FB_HR/FB
  ) %>%
#  left_join(innings, by = c("game_date", "pitcher", "p_throws", "home_team", "away_team", "p_team", "p_league", "inning_topbot")) %>%
  arrange(batter, p_throws, game_date)

bat_cum_r <- roll_woba(bat_sum) %>%
  filter(p_throws == "R") %>%
  arrange(batter, p_throws, game_date) %>%
  select("game_date","game_year", "batter","BF","PF", "AB", "uBB", "SO", "wOBA", "p_throws") %>%
  mutate(cum_b_k_rate_r = SO/AB,
         cum_b_bb_rate_r = uBB/AB) %>%
  rename(wOBA_cum_r = wOBA,
         BF_cum_r = BF,
         PF_cum_r = PF) %>%
  ungroup() %>%
  select(-c("AB", "uBB", "SO", "p_throws"))

bat_cum_l <- roll_woba(bat_sum) %>%
  filter(p_throws == "L") %>%
  arrange(batter, p_throws, game_date) %>%
  select("game_date","game_year", "batter","BF","PF", "AB", "uBB", "SO", "wOBA", "p_throws") %>%
  mutate(cum_b_k_rate_l = SO/AB,
         cum_b_bb_rate_l = uBB/AB) %>%
  rename(wOBA_cum_l = wOBA,
         BF_cum_l = BF,
         PF_cum_l = PF) %>%
  ungroup() %>%
  select(-c("AB", "uBB", "SO", "p_throws"))


bat_cum_tot <- roll_woba(bat_sum, rl_excl = "Y") %>%
  arrange(batter, game_date) %>%
  select("game_date", "game_year", "batter","BF", "PF", "AB", "uBB", "SO", "wOBA") %>%
  mutate(cum_b_k_rate = SO/AB,
         cum_b_bb_rate = uBB/AB) %>%
  rename(wOBA_cum_tot = wOBA,
         BF_cum_tot = BF,
         PF_cum_tot = PF) %>%
  select(-c("AB", "uBB", "SO"))

bat_rol_r <- roll_woba(bat_sum, 15) %>%
  filter(p_throws == "R") %>%
  arrange(batter, p_throws, game_date) %>%
  select("game_date","game_year", "batter", "BF", "PF", "AB", "uBB", "SO", "wOBA", "p_throws") %>%
  mutate(rol_b_k_rate_r = SO/AB,
         rol_b_bb_rate_r = uBB/AB) %>%
  rename(wOBA_rol_r = wOBA,
         BF_rol_r = BF,
         PF_rol_r = PF) %>%
  ungroup() %>%
  select(-c("AB", "uBB", "SO", "p_throws"))

bat_rol_l <- roll_woba(bat_sum, 15) %>%
  filter(p_throws == "L") %>%
  arrange(batter, p_throws, game_date) %>%
  select("game_date", "game_year", "batter", "BF", "PF", "AB", "uBB", "SO", "wOBA", "p_throws") %>%
  mutate(rol_b_k_rate_l = SO/AB,
         rol_b_bb_rate_l = uBB/AB) %>%
  rename(wOBA_rol_l = wOBA,
         BF_rol_l = BF,
         PF_rol_l = PF) %>%
  ungroup() %>%
  select(-c("AB", "uBB", "SO", "p_throws"))


bat_rol_tot <- roll_woba(bat_sum, 15, rl_excl = "Y") %>%
  arrange(batter, game_date) %>%
  select("game_date", "game_year", "batter", "BF", "PF", "AB", "uBB", "SO", "wOBA") %>%
  mutate(rol_b_k_rate = SO/AB,
         rol_b_bb_rate = uBB/AB) %>%
  rename(wOBA_rol_tot = wOBA,
         BF_rol_tot = BF,
         PF_rol_tot = PF) %>%
  select(-c("AB", "uBB", "SO"))

bat_all <- bat_cum_tot %>%
  left_join(bat_cum_l, by = c("game_date", "batter", "game_year")) %>%
  left_join(bat_cum_r, by = c("game_date", "batter", "game_year")) %>%
  left_join(bat_rol_tot, by = c("game_date", "batter", "game_year")) %>%
  left_join(bat_rol_l, by = c("game_date", "batter", "game_year")) %>%
  left_join(bat_rol_r, by = c("game_date", "batter", "game_year")) %>%
  left_join(bat_all_lg_fin, by = c("game_date", "game_year")) %>%
  distinct() %>%
  group_by(batter, game_year) %>%
  mutate(real_dt = lag(game_date),
         wRAA_cum_tot = ((wOBA_cum_tot - wOBA_cum_tot_lg)/woba_scale)*BF_cum_tot,
         wRAA_cum_l = ((wOBA_cum_l - wOBA_cum_l_lg)/woba_scale)*BF_cum_l,
         wRAA_cum_r = ((wOBA_cum_r - wOBA_cum_r_lg)/woba_scale)*BF_cum_r,
         wRAA_rol_tot = ((wOBA_rol_tot - wOBA_rol_tot_lg)/woba_scale)*BF_rol_tot,
         wRAA_rol_l = ((wOBA_rol_l - wOBA_rol_l_lg)/woba_scale)*BF_rol_l,
         wRAA_rol_r = ((wOBA_rol_r - wOBA_rol_r_lg)/woba_scale)*BF_rol_r,
         wRAA_cum_tot_pa = wRAA_cum_tot/BF_cum_tot,
         wRC_cum_tot = (((wRAA_cum_tot/BF_cum_tot + (cum_score/BF_cum_tot_lg))+((cum_score/BF_cum_tot_lg)-((PF_cum_tot/100)*(cum_score/BF_cum_tot_lg))))/(wRC_cum_tot_lg/BF_cum_tot_lg))*100,
         wRC_cum_l = (((wRAA_cum_l/BF_cum_l + (cum_score/BF_cum_tot_lg))+((cum_score/BF_cum_tot_lg)-((PF_cum_l/100)*(cum_score/BF_cum_tot_lg))))/(wRC_cum_l_lg/BF_cum_l_lg))*100,
         wRC_cum_r = (((wRAA_cum_r/BF_cum_r + (cum_score/BF_cum_tot_lg))+((cum_score/BF_cum_tot_lg)-((PF_cum_r/100)*(cum_score/BF_cum_tot_lg))))/(wRC_cum_r_lg/BF_cum_r_lg))*100,
         wRC_rol_tot = (((wRAA_rol_tot/BF_rol_tot + (rol_score/BF_rol_tot_lg))+((rol_score/BF_rol_tot_lg)-((PF_rol_tot/100)*(rol_score/BF_rol_tot_lg))))/(wRC_rol_tot_lg/BF_rol_tot_lg))*100,
         wRC_rol_l = (((wRAA_rol_l/BF_rol_l + (rol_score/BF_rol_tot_lg))+((rol_score/BF_rol_tot_lg)-((PF_rol_l/100)*(rol_score/BF_rol_tot_lg))))/(wRC_rol_l_lg/BF_rol_l_lg))*100,
         wRC_rol_r = (((wRAA_rol_r/BF_rol_r + (rol_score/BF_rol_tot_lg))+((rol_score/BF_rol_tot_lg)-((PF_rol_r/100)*(rol_score/BF_rol_tot_lg))))/(wRC_rol_r_lg/BF_rol_r_lg))*100) %>%
  select(-c(PF_cum_tot, wOBA_cum_tot, PF_cum_l, wOBA_cum_l, PF_cum_r, wOBA_cum_r, PF_rol_tot,
            wOBA_rol_tot, PF_rol_l, wOBA_rol_l, PF_rol_r, wOBA_rol_r, BF_cum_tot_lg, wOBA_cum_tot_lg,
            woba_scale, lg_woba, BF_cum_l_lg, wOBA_cum_l_lg, BF_cum_r_lg, wOBA_cum_r_lg, BF_rol_tot_lg,
            wOBA_rol_tot_lg, BF_rol_l_lg, wOBA_rol_l_lg, BF_rol_r_lg, wOBA_rol_r_lg, rol_score, cum_score,
            wRC_cum_tot_lg, wRC_cum_l_lg, wRC_cum_r_lg, wRC_rol_tot_lg, wRC_rol_l_lg, wRC_rol_r_lg,
            wRAA_cum_tot, wRAA_cum_l, wRAA_cum_r, wRAA_rol_tot, wRAA_rol_l, wRAA_rol_r, wRAA_cum_tot_pa))

bat_max <- bat_all %>%
  group_by(batter) %>%
  filter(game_date == max(game_date)) %>%
  select(-c(game_date, game_year, real_dt))

stand_bat <- pbp %>% group_by(batter, p_throws, stand) %>%
  summarise(cnt = n_distinct(at_bat_number)) %>%
  filter(cnt == max(cnt)) %>%
  select(-cnt)

rm(bat_cum_tot, bat_cum_l, bat_cum_r, bat_rol_tot, bat_rol_l, bat_rol_r)

dup_check <- bat_all[which(duplicated(paste(bat_all$batter, bat_all$game_date))),]

ex <- filter(bat_sum, batter == 594807)


#HIP$bunt <- grepl("bunt", HIP$des, ignore.case =  T)

HIP <- HIP %>%
  mutate(on1 = ifelse(is.na(on_1b),0,1),
         on2 = ifelse(is.na(on_2b),0,1),
         on3 = ifelse(is.na(on_3b),0,1)) %>%
  mutate(comb = paste(outs_when_up, on1, on2, on3))

###########################################################
#Final score of every half inning
###########################################################

fin_score <- pbp %>% select(game_date,game_year, game_pk, inning, inning_topbot, bat_score) %>%
  group_by(game_date,game_year, game_pk, inning, inning_topbot) %>%
  summarise(end_score = max(bat_score))

###############################################
# Bring in on deck
###############################################

b_order <- pbp %>%
  select(game_pk, game_date,game_year, batter,inning, inning_topbot, at_bat_number) %>%
  arrange(game_pk, inning_topbot, at_bat_number) %>%
  distinct() %>%
  mutate(on_deck = lead(batter))

###########################################################
#Situation 1
###########################################################

sit1_beg <- pbp %>% select(game_date,game_year,game_pk,at_bat_number, batter, pitcher, inning, inning_topbot, stand, p_throws, on_3b, on_2b, on_1b, outs_when_up,bat_score) %>%
  filter(outs_when_up == 1 & !is.na(on_1b) & is.na(on_2b) & is.na(on_3b)) %>%
  distinct() %>%
  left_join(fin_score, by = c("game_date","game_year", "game_pk", "inning", "inning_topbot")) %>%
  mutate(runs_created = end_score - bat_score) %>%
  left_join(b_order, by = c("game_pk", "game_date", "game_year", "batter", "inning", "inning_topbot", "at_bat_number")) %>%
  left_join(bat_all[2:28], by = c("batter" = "batter", "game_date" = "real_dt")) %>%
  left_join(pitch_all[2:22], by = c("pitcher" = "pitcher", "game_date" = "real_dt")) %>%
  left_join(sprint.speed, by=c("batter" = "player_id", "game_year" = "game_year")) %>%
  left_join(bat_all[2:28], by = c("on_deck" = "batter", "game_date" = "real_dt")) %>%
  left_join(sprint.speed, by=c("on_1b" = "player_id", "game_year.y" = "game_year")) %>%
  select(-c("game_year.y", "game_year.x.x", "game_year.y.y"))

input_df <- function(df){
  
    df1 <- df %>%
      filter(stand == "R" & p_throws == "R") %>%
      select(-c("wRC_cum_l.x", "cum_b_k_rate_l.x", "cum_b_bb_rate_l.x", "wRC_cum_l.y", "cum_b_k_rate_l.y", "cum_b_bb_rate_l.y",
                "wRC_rol_l.x", "rol_b_k_rate_l.x", "rol_b_bb_rate_l.x", "wRC_rol_l.y", "rol_b_k_rate_l.y", "rol_b_bb_rate_l.y",
                "FIP_rol_l", "rol_p_k_rate_l", "rol_p_bb_rate_l", "FIP_cum_l", "cum_p_k_rate_l", "cum_p_bb_rate_l", "BF_rol_l.x",
                "BF_rol_l.y", "BF_cum_l.x", "BF_cum_l.y")) %>%
      rename(wRC_cum_hand = wRC_cum_r.x,
             cum_b_k_rate_hand = cum_b_k_rate_r.x,
             cum_b_bb_rate_hand = cum_b_bb_rate_r.x,
             wRC_cum_hand_od = wRC_cum_r.y,
             cum_b_k_rate_hand_od = cum_b_k_rate_r.y,
             cum_b_bb_rate_hand_od = cum_b_bb_rate_r.y,
             wRC_rol_hand = wRC_rol_r.x,
             rol_b_k_rate_hand = rol_b_k_rate_r.x,
             rol_b_bb_rate_hand = rol_b_bb_rate_r.x,
             wRC_rol_hand_od = wRC_rol_r.y,
             rol_b_k_rate_hand_od = rol_b_k_rate_r.y,
             rol_b_bb_rate_hand_od = rol_b_bb_rate_r.y,
             FIP_rol_hand = FIP_rol_r,
             rol_p_k_rate_hand = rol_p_k_rate_r,
             rol_p_bb_rate_hand = rol_p_bb_rate_r,
             FIP_cum_hand = FIP_cum_r,
             cum_p_k_rate_hand = cum_p_k_rate_r,
             cum_p_bb_rate_hand = cum_p_bb_rate_r,
             BF_cum_hand = BF_cum_r.x,
             BF_rol_hand = BF_rol_r.x,
             BF_cum_hand_od = BF_cum_r.y,
             BF_rol_hand_od = BF_rol_r.y)
    
    df2 <- df %>%
      filter(stand == "L" & p_throws == "L") %>%
      select(-c("wRC_cum_r.x", "cum_b_k_rate_r.x", "cum_b_bb_rate_r.x", "wRC_cum_r.y", "cum_b_k_rate_r.y", "cum_b_bb_rate_r.y",
                "wRC_rol_r.x", "rol_b_k_rate_r.x", "rol_b_bb_rate_r.x", "wRC_rol_r.y", "rol_b_k_rate_r.y", "rol_b_bb_rate_r.y",
                "FIP_rol_r", "rol_p_k_rate_r", "rol_p_bb_rate_r", "FIP_cum_r", "cum_p_k_rate_r", "cum_p_bb_rate_r", "BF_rol_r.x",
                "BF_rol_r.y", "BF_cum_r.x", "BF_cum_r.y")) %>%
      rename(wRC_cum_hand = wRC_cum_l.x,
             cum_b_k_rate_hand = cum_b_k_rate_l.x,
             cum_b_bb_rate_hand = cum_b_bb_rate_l.x,
             wRC_cum_hand_od = wRC_cum_l.y,
             cum_b_k_rate_hand_od = cum_b_k_rate_l.y,
             cum_b_bb_rate_hand_od = cum_b_bb_rate_l.y,
             wRC_rol_hand = wRC_rol_l.x,
             rol_b_k_rate_hand = rol_b_k_rate_l.x,
             rol_b_bb_rate_hand = rol_b_bb_rate_l.x,
             wRC_rol_hand_od = wRC_rol_l.y,
             rol_b_k_rate_hand_od = rol_b_k_rate_l.y,
             rol_b_bb_rate_hand_od = rol_b_bb_rate_l.y,
             FIP_rol_hand = FIP_rol_l,
             rol_p_k_rate_hand = rol_p_k_rate_l,
             rol_p_bb_rate_hand = rol_p_bb_rate_l,
             FIP_cum_hand = FIP_cum_l,
             cum_p_k_rate_hand = cum_p_k_rate_l,
             cum_p_bb_rate_hand = cum_p_bb_rate_l,
             BF_cum_hand = BF_cum_l.x,
             BF_rol_hand = BF_rol_l.x,
             BF_cum_hand_od = BF_cum_l.y,
             BF_rol_hand_od = BF_rol_l.y)
    
    df3 <- df %>%
      filter(stand == "R" & p_throws == "L") %>%
      select(-c("wRC_cum_r.x", "cum_b_k_rate_r.x", "cum_b_bb_rate_r.x", "wRC_cum_r.y", "cum_b_k_rate_r.y", "cum_b_bb_rate_r.y",
                "wRC_rol_r.x", "rol_b_k_rate_r.x", "rol_b_bb_rate_r.x", "wRC_rol_r.y", "rol_b_k_rate_r.y", "rol_b_bb_rate_r.y",
                "FIP_rol_l", "rol_p_k_rate_l", "rol_p_bb_rate_l", "FIP_cum_l", "cum_p_k_rate_l", "cum_p_bb_rate_l", "BF_rol_r.x",
                "BF_rol_r.y", "BF_cum_r.x", "BF_cum_r.y")) %>%
      rename(wRC_cum_hand = wRC_cum_l.x,
             cum_b_k_rate_hand = cum_b_k_rate_l.x,
             cum_b_bb_rate_hand = cum_b_bb_rate_l.x,
             wRC_cum_hand_od = wRC_cum_l.y,
             cum_b_k_rate_hand_od = cum_b_k_rate_l.y,
             cum_b_bb_rate_hand_od = cum_b_bb_rate_l.y,
             wRC_rol_hand = wRC_rol_l.x,
             rol_b_k_rate_hand = rol_b_k_rate_l.x,
             rol_b_bb_rate_hand = rol_b_bb_rate_l.x,
             wRC_rol_hand_od = wRC_rol_l.y,
             rol_b_k_rate_hand_od = rol_b_k_rate_l.y,
             rol_b_bb_rate_hand_od = rol_b_bb_rate_l.y,
             FIP_rol_hand = FIP_rol_r,
             rol_p_k_rate_hand = rol_p_k_rate_r,
             rol_p_bb_rate_hand = rol_p_bb_rate_r,
             FIP_cum_hand = FIP_cum_r,
             cum_p_k_rate_hand = cum_p_k_rate_r,
             cum_p_bb_rate_hand = cum_p_bb_rate_r,
             BF_cum_hand = BF_cum_l.x,
             BF_rol_hand = BF_rol_l.x,
             BF_cum_hand_od = BF_cum_l.y,
             BF_rol_hand_od = BF_rol_l.y)
    
    df4 <- df %>%
      filter(stand == "L" & p_throws == "R") %>%
      select(-c("wRC_cum_l.x", "cum_b_k_rate_l.x", "cum_b_bb_rate_l.x", "wRC_cum_l.y", "cum_b_k_rate_l.y", "cum_b_bb_rate_l.y",
                "wRC_rol_l.x", "rol_b_k_rate_l.x", "rol_b_bb_rate_l.x", "wRC_rol_l.y", "rol_b_k_rate_l.y", "rol_b_bb_rate_l.y",
                "FIP_rol_r", "rol_p_k_rate_r", "rol_p_bb_rate_r", "FIP_cum_r", "cum_p_k_rate_r", "cum_p_bb_rate_r", "BF_rol_l.x",
                "BF_rol_l.y", "BF_cum_l.x", "BF_cum_l.y")) %>%
      rename(wRC_cum_hand = wRC_cum_r.x,
             cum_b_k_rate_hand = cum_b_k_rate_r.x,
             cum_b_bb_rate_hand = cum_b_bb_rate_r.x,
             wRC_cum_hand_od = wRC_cum_r.y,
             cum_b_k_rate_hand_od = cum_b_k_rate_r.y,
             cum_b_bb_rate_hand_od = cum_b_bb_rate_r.y,
             wRC_rol_hand = wRC_rol_r.x,
             rol_b_k_rate_hand = rol_b_k_rate_r.x,
             rol_b_bb_rate_hand = rol_b_bb_rate_r.x,
             wRC_rol_hand_od = wRC_rol_r.y,
             rol_b_k_rate_hand_od = rol_b_k_rate_r.y,
             rol_b_bb_rate_hand_od = rol_b_bb_rate_r.y,
             FIP_rol_hand = FIP_rol_l,
             rol_p_k_rate_hand = rol_p_k_rate_l,
             rol_p_bb_rate_hand = rol_p_bb_rate_l,
             FIP_cum_hand = FIP_cum_l,
             cum_p_k_rate_hand = cum_p_k_rate_l,
             cum_p_bb_rate_hand = cum_p_bb_rate_l,
             BF_cum_hand = BF_cum_r.x,
             BF_rol_hand = BF_rol_r.x,
             BF_cum_hand_od = BF_cum_r.y,
             BF_rol_hand_od = BF_rol_r.y)
    
    df_all <- rbind(df1, df2, df3, df4)
    
    return(df_all)
}

sit1_input <- input_df(sit1_beg)
#sit1_input$sprint_speed.x[is.na(sit1_input$sprint_speed.x)] <- mean(sit1_input$sprint_speed.x, na.rm=TRUE)
#sit1_input$sprint_speed.y[is.na(sit1_input$sprint_speed.y)] <- mean(sit1_input$sprint_speed.y, na.rm=TRUE)



sit1_end <- pbp %>% select(game_date,game_year,game_pk,at_bat_number, batter, pitcher, inning, inning_topbot, stand, p_throws, on_3b, on_2b, on_1b, outs_when_up,bat_score) %>%
  filter(outs_when_up == 2 & is.na(on_1b) & !is.na(on_2b) & is.na(on_3b)) %>%
  distinct() %>%
  left_join(fin_score, by = c("game_date","game_year", "game_pk", "inning", "inning_topbot")) %>%
  mutate(runs_created = end_score - bat_score) %>%
  left_join(b_order, by = c("game_pk", "game_date", "game_year", "batter", "inning", "inning_topbot", "at_bat_number")) %>%
  left_join(bat_all[2:28], by = c("batter" = "batter", "game_date" = "real_dt")) %>%
  left_join(pitch_all[2:22], by = c("pitcher" = "pitcher", "game_date" = "real_dt")) %>%
  left_join(sprint.speed, by=c("batter" = "player_id", "game_year" = "game_year")) %>%
  left_join(bat_all[2:28], by = c("on_deck" = "batter", "game_date" = "real_dt")) %>%
  left_join(sprint.speed, by=c("on_2b" = "player_id", "game_year.y" = "game_year")) %>%
  select(-c("game_year.y", "game_year.x.x", "game_year.y.y"))

sit1_output <- input_df(sit1_end)

rm(b_order, bat_all, bat_sum, fin_score, pitch_all, pitch_sum, pitch_cum_rl)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#CREATE DIFFERENT INPUT DATASETS
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

sit1_input_fin <- sit1_input[,c(2,17,19:64)]

#Remove all NA

sit1_input_fin_comp <- sit1_input_fin[complete.cases(sit1_input_fin),]
sit1_input_fin_comp_scale <- sit1_input_fin_comp
sit1_input_fin_comp_scale[3:48] <- scale(sit1_input_fin_comp_scale[3:48])

###############################################
# Fit a GLM - Poisson model for runs generated
################################################

rc_pois <- glm(runs_created ~ wOBA_cum_hand + wOBA_rol_tot.x + wOBA_rol_hand + FIP_rol_tot +
                 rol_p_k_rate + rol_p_bb_rate + FIP_rol_hand + rol_p_k_rate_hand + rol_p_bb_rate_hand +
                 sprint_speed.x + wOBA_cum_hand_od + wOBA_rol_tot.y + wOBA_rol_hand_od + sprint_speed.y
               , data=sit1_input[,c(2,17, 19:64)], family = poisson)
summary(rc_pois)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# LASSO REGRESSION - Find important variables for Poisson Model
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#sit1_input_fin <- sit1_input[,c(2,17,19:64)]
#sit1_input_fin <- sit1_input_fin[complete.cases(sit1_input_fin),]

x<- model.matrix(runs_created ~., sit1_input_fin_comp_scale)[,-1]
y <- sit1_input_fin_comp_scale$runs_created

set.seed(3453)
train <- sample(1:nrow(x), nrow(x)*.75)
test <- (-train)
y.test <- y[test]

grid <- 10^seq(10,-2,length = 100)

lasso.mod <- glmnet(x[train,], y[train], alpha = 1, family = "poisson", lambda = grid)
#plot(lasso.mod)

set.seed(4324)
cv.out <- cv.glmnet(x[train,], y[train], alpha = 1, family = "poisson")
#plot(cv.out)
bestlam <- cv.out$lambda.min
lasso.pred <- predict(lasso.mod, s = bestlam, newx = x[test,])

out <- glmnet(x,y, alpha = 1, lambda = grid, family = "poisson")
lasso.coef <- predict(out, type = "coefficients", s = bestlam)
lasso.coef

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#Random Forest
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

bag.bunt <- randomForest(runs_created ~., data = sit1_input_fin_comp,
                         subset = train, mtry = 47, importance = TRUE)

yhat.bag <- predict(bag.bunt, newdata = sit1_input_fin_comp[-train,], type= "response")
mean((yhat.bag - y.test)^2)

ntree = 500
test.error.bunt <- rep(0, ntree)

for (i in 1:ntree){
  bunt.rf2 <- randomForest(runs_created ~., data = sit1_input_fin,
                           subset = train,ntree = i ,importance = TRUE)
  
  
  yhat.test.bunt <- predict(bunt.rf2, newdata = sit1_input_fin[-train,]) 
  test.error.bunt[i] <- mean((yhat.test.bunt - y.test)^2)
}



rf.bunt <- randomForest(runs_created ~., data = sit1_input_fin_comp,
                         subset = train, importance = TRUE)
yhat.rf <- predict(rf.bunt, newdata = sit1_input_fin_comp[-train,])
mean((yhat.rf - y.test)^2)
importance(rf.bunt)

library(gbm)

boost.bunt.sit1.inp <- gbm(runs_created ~., data = sit1_input_fin_comp[train,],
                  distribution = "poisson", n.trees = 500,
                  interaction.depth = 4)
summary(boost.bunt.sit1.inp)
yhat.boost.sit1.inp <- predict(boost.bunt.sit1.inp, newdata = sit1_input_fin_comp[-train,], type = "response", n.trees = 500)
mean((yhat.boost.sit1.inp - y.test)^2)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Create output datasets. We are going with the boosted model since it was the 
# best fit for the input
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

sit1_output_fin <- sit1_output[,c(2,17,19:64)]

#Remove all NA

sit1_output_fin_comp <- sit1_output_fin[complete.cases(sit1_output_fin),]
sit1_output_fin_comp_scale <- sit1_output_fin_comp
sit1_output_fin_comp_scale[3:48] <- scale(sit1_output_fin_comp_scale[3:48])

x.op.sit1<- model.matrix(runs_created ~., sit1_output_fin_comp_scale)[,-1]
y.op.sit1 <- sit1_output_fin_comp_scale$runs_created

set.seed(3453)
train.op.sit1 <- sample(1:nrow(x.op.sit1), nrow(x.op.sit1)*.75)
test.op.sit1 <- (-train.op.sit1)
y.test.op.sit1 <- y.op.sit1[test.op.sit1]

boost.bunt.sit1.op <- gbm(runs_created ~., data = sit1_output_fin_comp[train.op.sit1,],
                           distribution = "poisson", n.trees = 500,
                           interaction.depth = 4)
summary(boost.bunt.sit1.op)
yhat.boost.sit1.op <- predict(boost.bunt.sit1.op, newdata = sit1_output_fin_comp[-train.op.sit1,], type = "response", n.trees = 500)
mean((yhat.boost.sit1.op - y.test.op.sit1)^2)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Situation 2
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

sit2_beg <- pbp %>% select(game_date,game_year,game_pk,at_bat_number, batter, pitcher, inning, inning_topbot, stand, p_throws, on_3b, on_2b, on_1b, outs_when_up,bat_score) %>%
  filter(outs_when_up == 0 & !is.na(on_1b) & is.na(on_2b) & is.na(on_3b)) %>%
  distinct() %>%
  left_join(fin_score, by = c("game_date","game_year", "game_pk", "inning", "inning_topbot")) %>%
  mutate(runs_created = end_score - bat_score) %>%
  left_join(b_order, by = c("game_pk", "game_date", "game_year", "batter", "inning", "inning_topbot", "at_bat_number")) %>%
  left_join(bat_all[2:28], by = c("batter" = "batter", "game_date" = "real_dt")) %>%
  left_join(pitch_all[2:22], by = c("pitcher" = "pitcher", "game_date" = "real_dt")) %>%
  left_join(sprint.speed, by=c("batter" = "player_id", "game_year" = "game_year")) %>%
  left_join(bat_all[2:28], by = c("on_deck" = "batter", "game_date" = "real_dt")) %>%
  left_join(sprint.speed, by=c("on_1b" = "player_id", "game_year.y" = "game_year")) %>%
  select(-c("game_year.y", "game_year.x.x", "game_year.y.y"))

sit2_input <- input_df(sit2_beg)

sit2_input_fin <- sit2_input[,c(2,17,19:64)]

#Remove all NA

sit2_input_fin_comp <- sit2_input_fin[complete.cases(sit2_input_fin),]
sit2_input_fin_comp_scale <- sit2_input_fin_comp
sit2_input_fin_comp_scale[3:48] <- scale(sit2_input_fin_comp_scale[3:48])

x.ip.sit2<- model.matrix(runs_created ~., sit2_input_fin_comp_scale)[,-1]
y.ip.sit2 <- sit2_input_fin_comp_scale$runs_created

train.ip.sit2 <- sample(1:nrow(x.ip.sit2), nrow(x.ip.sit2)*.75)
test.ip.sit2 <- (-train.ip.sit2)
y.test.ip.sit2 <- y.ip.sit2[test.ip.sit2]

boost.bunt.sit2.ip <- gbm(runs_created ~., data = sit2_input_fin_comp[train.ip.sit2,],
                          distribution = "poisson", n.trees = 2000,
                          interaction.depth = 4)
summary(boost.bunt.sit2.ip)
yhat.boost.sit2.ip <- predict(boost.bunt.sit2.ip, newdata = sit2_input_fin_comp[-train.ip.sit2,], type = "response", n.trees = 500)
mean((yhat.boost.sit2.ip - y.test.ip.sit2)^2)

sit2_end <- pbp %>% select(game_date,game_year,game_pk,at_bat_number, batter, pitcher, inning, inning_topbot, stand, p_throws, on_3b, on_2b, on_1b, outs_when_up,bat_score) %>%
  filter(outs_when_up == 1 & is.na(on_1b) & !is.na(on_2b) & is.na(on_3b)) %>%
  distinct() %>%
  left_join(fin_score, by = c("game_date","game_year", "game_pk", "inning", "inning_topbot")) %>%
  mutate(runs_created = end_score - bat_score) %>%
  left_join(b_order, by = c("game_pk", "game_date", "game_year", "batter", "inning", "inning_topbot", "at_bat_number")) %>%
  left_join(bat_all[2:28], by = c("batter" = "batter", "game_date" = "real_dt")) %>%
  left_join(pitch_all[2:22], by = c("pitcher" = "pitcher", "game_date" = "real_dt")) %>%
  left_join(sprint.speed, by=c("batter" = "player_id", "game_year" = "game_year")) %>%
  left_join(bat_all[2:28], by = c("on_deck" = "batter", "game_date" = "real_dt")) %>%
  left_join(sprint.speed, by=c("on_2b" = "player_id", "game_year.y" = "game_year")) %>%
  select(-c("game_year.y", "game_year.x.x", "game_year.y.y"))

sit2_output <- input_df(sit2_end)

sit2_output_fin <- sit2_output[,c(2,17,19:64)]

#Remove all NA

sit2_output_fin_comp <- sit2_output_fin[complete.cases(sit2_output_fin),]
sit2_output_fin_comp_scale <- sit2_output_fin_comp
sit2_output_fin_comp_scale[3:48] <- scale(sit2_output_fin_comp_scale[3:48])

x.op.sit2<- model.matrix(runs_created ~., sit2_output_fin_comp_scale)[,-1]
y.op.sit2 <- sit2_output_fin_comp_scale$runs_created

train.op.sit2 <- sample(1:nrow(x.op.sit2), nrow(x.op.sit2)*.75)
test.op.sit2 <- (-train.op.sit2)
y.test.op.sit2 <- y.op.sit2[test.op.sit2]

boost.bunt.sit2.op <- gbm(runs_created ~., data = sit2_output_fin_comp[train.op.sit2,],
                          distribution = "poisson", n.trees = 500,
                          interaction.depth = 4)
summary(boost.bunt.sit2.op)
yhat.boost.sit2.op <- predict(boost.bunt.sit2.op, newdata = sit2_output_fin_comp[-train.op.sit2,], type = "response", n.trees = 2000)
mean((yhat.boost.sit2.op - y.test.op.sit2)^2)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Situation 3
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

sit3_beg <- pbp %>% select(game_date,game_year,game_pk,at_bat_number, batter, pitcher, inning, inning_topbot, stand, p_throws, on_3b, on_2b, on_1b, outs_when_up,bat_score) %>%
  filter(outs_when_up == 0 & is.na(on_1b) & !is.na(on_2b) & is.na(on_3b)) %>%
  distinct() %>%
  left_join(fin_score, by = c("game_date","game_year", "game_pk", "inning", "inning_topbot")) %>%
  mutate(runs_created = end_score - bat_score) %>%
  left_join(b_order, by = c("game_pk", "game_date", "game_year", "batter", "inning", "inning_topbot", "at_bat_number")) %>%
  left_join(bat_all[2:28], by = c("batter" = "batter", "game_date" = "real_dt")) %>%
  left_join(pitch_all[2:22], by = c("pitcher" = "pitcher", "game_date" = "real_dt")) %>%
  left_join(sprint.speed, by=c("batter" = "player_id", "game_year" = "game_year")) %>%
  left_join(bat_all[2:28], by = c("on_deck" = "batter", "game_date" = "real_dt")) %>%
  left_join(sprint.speed, by=c("on_2b" = "player_id", "game_year.y" = "game_year")) %>%
  select(-c("game_year.y", "game_year.x.x", "game_year.y.y"))

sit3_input <- input_df(sit3_beg)

sit3_input_fin <- sit3_input[,c(2,17,19:64)]

#Remove all NA

sit3_input_fin_comp <- sit3_input_fin[complete.cases(sit3_input_fin),]
sit3_input_fin_comp_scale <- sit3_input_fin_comp
sit3_input_fin_comp_scale[3:48] <- scale(sit3_input_fin_comp_scale[3:48])

x.ip.sit3<- model.matrix(runs_created ~., sit3_input_fin_comp)[,-1]
y.ip.sit3 <- sit3_input_fin_comp$runs_created

train.ip.sit3 <- sample(1:nrow(x.ip.sit3), nrow(x.ip.sit3)*.75)
test.ip.sit3 <- (-train.ip.sit3)
y.test.ip.sit3 <- y.ip.sit2[test.ip.sit3]

boost.bunt.sit3.ip <- gbm(runs_created ~., data = sit3_input_fin_comp[train.ip.sit3,],
                          distribution = "poisson", n.trees = 2000,
                          interaction.depth = 4)
summary(boost.bunt.sit3.ip)
yhat.boost.sit3.ip <- predict(boost.bunt.sit3.ip, newdata = sit3_input_fin_comp[-train.ip.sit3,], type = "response", n.trees = 500)
mean((yhat.boost.sit3.ip - y.test.ip.sit3)^2)

sit3_end <- pbp %>% select(game_date,game_year,game_pk,at_bat_number, batter, pitcher, inning, inning_topbot, stand, p_throws, on_3b, on_2b, on_1b, outs_when_up,bat_score) %>%
  filter(outs_when_up == 1 & is.na(on_1b) & is.na(on_2b) & !is.na(on_3b)) %>%
  distinct() %>%
  left_join(fin_score, by = c("game_date","game_year", "game_pk", "inning", "inning_topbot")) %>%
  mutate(runs_created = end_score - bat_score) %>%
  left_join(b_order, by = c("game_pk", "game_date", "game_year", "batter", "inning", "inning_topbot", "at_bat_number")) %>%
  left_join(bat_all[2:28], by = c("batter" = "batter", "game_date" = "real_dt")) %>%
  left_join(pitch_all[2:22], by = c("pitcher" = "pitcher", "game_date" = "real_dt")) %>%
  left_join(sprint.speed, by=c("batter" = "player_id", "game_year" = "game_year")) %>%
  left_join(bat_all[2:28], by = c("on_deck" = "batter", "game_date" = "real_dt")) %>%
  left_join(sprint.speed, by=c("on_3b" = "player_id", "game_year.y" = "game_year")) %>%
  select(-c("game_year.y", "game_year.x.x", "game_year.y.y"))

sit3_output <- input_df(sit3_end)

sit3_output_fin <- sit3_output[,c(2,17,19:64)]

#Remove all NA

sit3_output_fin_comp <- sit3_output_fin[complete.cases(sit3_output_fin),]
sit3_output_fin_comp_scale <- sit3_output_fin_comp
sit3_output_fin_comp_scale[3:48] <- scale(sit3_output_fin_comp_scale[3:48])

x.op.sit3<- model.matrix(runs_created ~., sit3_output_fin_comp_scale)[,-1]
y.op.sit3 <- sit3_output_fin_comp_scale$runs_created

train.op.sit3 <- sample(1:nrow(x.op.sit3), nrow(x.op.sit3)*.75)
test.op.sit3 <- (-train.op.sit3)
y.test.op.sit3 <- y.op.sit3[test.op.sit3]

boost.bunt.sit3.op <- gbm(runs_created ~., data = sit3_output_fin_comp[train.op.sit3,],
                          distribution = "poisson", n.trees = 500,
                          interaction.depth = 4)
summary(boost.bunt.sit3.op)
yhat.boost.sit3.op <- predict(boost.bunt.sit3.op, newdata = sit3_output_fin_comp[-train.op.sit3,], type = "response", n.trees = 2000)
mean((yhat.boost.sit3.op - y.test.op.sit3)^2)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Situation 4
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

sit4_beg <- pbp %>% select(game_date,game_year,game_pk,at_bat_number, batter, pitcher, inning, inning_topbot, stand, p_throws, on_3b, on_2b, on_1b, outs_when_up,bat_score) %>%
  filter(outs_when_up == 0 & !is.na(on_1b) & !is.na(on_2b) & is.na(on_3b)) %>%
  distinct() %>%
  left_join(fin_score, by = c("game_date","game_year", "game_pk", "inning", "inning_topbot")) %>%
  mutate(runs_created = end_score - bat_score) %>%
  left_join(b_order, by = c("game_pk", "game_date", "game_year", "batter", "inning", "inning_topbot", "at_bat_number")) %>%
  left_join(bat_all[2:28], by = c("batter" = "batter", "game_date" = "real_dt")) %>%
  left_join(pitch_all[2:22], by = c("pitcher" = "pitcher", "game_date" = "real_dt")) %>%
  left_join(sprint.speed, by=c("batter" = "player_id", "game_year" = "game_year")) %>%
  left_join(bat_all[2:28], by = c("on_deck" = "batter", "game_date" = "real_dt")) %>%
  left_join(sprint.speed, by=c("on_2b" = "player_id", "game_year.y" = "game_year")) %>%
  select(-c("game_year.y", "game_year.x.x", "game_year.y.y"))

sit4_input <- input_df(sit4_beg)

sit4_input_fin <- sit4_input[,c(2,17,19:64)]

#Remove all NA

sit4_input_fin_comp <- sit4_input_fin[complete.cases(sit4_input_fin),]
sit4_input_fin_comp_scale <- sit4_input_fin_comp
sit4_input_fin_comp_scale[3:48] <- scale(sit4_input_fin_comp_scale[3:48])

x.ip.sit4<- model.matrix(runs_created ~., sit4_input_fin_comp_scale)[,-1]
y.ip.sit4 <- sit4_input_fin_comp_scale$runs_created

train.ip.sit4 <- sample(1:nrow(x.ip.sit4), nrow(x.ip.sit4)*.75)
test.ip.sit4 <- (-train.ip.sit4)
y.test.ip.sit4 <- y.ip.sit4[test.ip.sit4]

boost.bunt.sit4.ip <- gbm(runs_created ~., data = sit4_input_fin_comp[train.ip.sit4,],
                          distribution = "poisson", n.trees = 2000,
                          interaction.depth = 4)
summary(boost.bunt.sit4.ip)
yhat.boost.sit4.ip <- predict(boost.bunt.sit4.ip, newdata = sit4_input_fin_comp[-train.ip.sit4,], type = "response", n.trees = 500)
mean((yhat.boost.sit4.ip - y.test.ip.sit4)^2)

sit4_end <- pbp %>% select(game_date,game_year,game_pk,at_bat_number, batter, pitcher, inning, inning_topbot, stand, p_throws, on_3b, on_2b, on_1b, outs_when_up,bat_score) %>%
  filter(outs_when_up == 1 & is.na(on_1b) & !is.na(on_2b) & !is.na(on_3b)) %>%
  distinct() %>%
  left_join(fin_score, by = c("game_date","game_year", "game_pk", "inning", "inning_topbot")) %>%
  mutate(runs_created = end_score - bat_score) %>%
  left_join(b_order, by = c("game_pk", "game_date", "game_year", "batter", "inning", "inning_topbot", "at_bat_number")) %>%
  left_join(bat_all[2:28], by = c("batter" = "batter", "game_date" = "real_dt")) %>%
  left_join(pitch_all[2:22], by = c("pitcher" = "pitcher", "game_date" = "real_dt")) %>%
  left_join(sprint.speed, by=c("batter" = "player_id", "game_year" = "game_year")) %>%
  left_join(bat_all[2:28], by = c("on_deck" = "batter", "game_date" = "real_dt")) %>%
  left_join(sprint.speed, by=c("on_3b" = "player_id", "game_year.y" = "game_year")) %>%
  select(-c("game_year.y", "game_year.x.x", "game_year.y.y"))

sit4_output <- input_df(sit4_end)

sit4_output_fin <- sit4_output[,c(2,17,19:64)]

#Remove all NA

sit4_output_fin_comp <- sit4_output_fin[complete.cases(sit4_output_fin),]
sit4_output_fin_comp_scale <- sit4_output_fin_comp
sit4_output_fin_comp_scale[3:48] <- scale(sit4_output_fin_comp_scale[3:48])

x.op.sit4<- model.matrix(runs_created ~., sit4_output_fin_comp_scale)[,-1]
y.op.sit4 <- sit4_output_fin_comp_scale$runs_created

train.op.sit4 <- sample(1:nrow(x.op.sit4), nrow(x.op.sit4)*.75)
test.op.sit4 <- (-train.op.sit4)
y.test.op.sit4 <- y.op.sit4[test.op.sit4]

boost.bunt.sit4.op <- gbm(runs_created ~., data = sit4_output_fin_comp[train.op.sit4,],
                          distribution = "poisson", n.trees = 500,
                          interaction.depth = 4)
summary(boost.bunt.sit4.op)
yhat.boost.sit4.op <- predict(boost.bunt.sit4.op, newdata = sit4_output_fin_comp[-train.op.sit4,], type = "response", n.trees = 2000)
mean((yhat.boost.sit4.op - y.test.op.sit4)^2)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Make the model
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

should_u_bunt <- function(outs, on1b=NULL, on2b=NULL, on3b=NULL, batter, pitcher, deck, hole){
  
  df1 <- pitch_max %>% filter(pitcher == pitcher) %>%
    left_join(p_throw, by = "pitcher") %>%
    cbind(filter(bat_max, batter == batter)) %>%
    left_join(stand_bat, by = c("batter", "p_throws")) %>%
    left_join(sprint_max, by = c("batter" = "player_id")) %>%
    mutate(filter(bat_max, batter == deck) %>%
             rename_with( ~ paste0(.x, ".y")))
  
  df2 <- pitch_max %>% filter(pitcher == pitcher) %>%
    left_join(p_throw, by = "pitcher") %>%
    cbind(filter(bat_max, batter == deck)) %>%
    left_join(stand_bat, by = c("batter", "p_throws")) %>%
    left_join(sprint_max, by = c("batter" = "player_id")) %>%
    mutate(filter(bat_max, batter == hole) %>%
             rename_with( ~ paste0(.x, ".y")))
  
  if(!is.null(on2b)){
    df1 <- df1 %>% mutate(filter(sprint_max, player_id == on2b) %>%
                            select(sprint_speed) %>%
                            rename(sprint_speed.y = sprint_speed))
  }else if(!is.null(on1b)){
    df1 <- df1 %>% mutate(filter(sprint_max, player_id == on1b) %>%
                            select(sprint_speed) %>%
                            rename(sprint_speed.y = sprint_speed))
  }
  
}

df1 <- pitch_max %>% filter(pitcher == 408314) %>%
  left_join(p_throw, by = "pitcher") %>%
  cbind(filter(bat_max, batter == 405395)) %>%
  left_join(stand_bat, by = c("batter", "p_throws")) %>%
  left_join(sprint_max, by = c("batter" = "player_id")) %>%
  mutate(filter(bat_max, batter == 425877) %>%
          rename_with( ~ paste0(.x, ".y")))











