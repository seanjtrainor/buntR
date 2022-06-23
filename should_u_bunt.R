#READ IN NEEDED PACKAGES

usePackage <- function(p) {
  if(!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}


listPackages <- c("baseballr", "dplyr", "RcppRoll", "zoo", "lubridate", "tidyverse",
                  "tree", "caret", "randomForest", "rvest", "ggalt", "remotes")

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

sprint.speed19 <- read.csv("~/Desktop/baseball/shiftr/sprint_speed (1).csv") %>%
  dplyr::select(player_id, sprint_speed) %>% 
  dplyr::mutate(game_year = "2019")

sprint.speed20 <- read.csv("~/Desktop/baseball/shiftr/sprint_speed (2).csv") %>%
  dplyr::select(player_id, sprint_speed) %>% 
  dplyr::mutate(game_year = "2020")

sprint.speed21 <- read.csv("~/Desktop/baseball/shiftr/sprint_speed (3).csv") %>%
  dplyr::select(player_id, sprint_speed) %>% 
  dplyr::mutate(game_year = "2021")

sprint.speed <- rbind(sprint.speed19, sprint.speed20, sprint.speed21)

rm(sprint.speed19, sprint.speed20, sprint.speed21)

#BRING IN DATA
rm(pbp21)
pbp <- rbind(season_pbp(2021), season_pbp(2020), season_pbp(2019))

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
  select("game_date","game_year", "batter", "AB", "uBB", "SO", "wOBA", "p_throws") %>%
  mutate(cum_b_k_rate_r = SO/AB,
         cum_b_bb_rate_r = uBB/AB) %>%
  rename(wOBA_cum_r = wOBA) %>%
  ungroup() %>%
  select(-c("AB", "uBB", "SO", "p_throws"))

bat_cum_l <- roll_woba(bat_sum) %>%
  filter(p_throws == "L") %>%
  arrange(batter, p_throws, game_date) %>%
  select("game_date","game_year", "batter", "AB", "uBB", "SO", "wOBA", "p_throws") %>%
  mutate(cum_b_k_rate_l = SO/AB,
         cum_b_bb_rate_l = uBB/AB) %>%
  rename(wOBA_cum_l = wOBA) %>%
  ungroup() %>%
  select(-c("AB", "uBB", "SO", "p_throws"))


bat_cum_tot <- roll_woba(bat_sum, rl_excl = "Y") %>%
  arrange(batter, game_date) %>%
  select("game_date", "game_year", "batter", "AB", "uBB", "SO", "wOBA") %>%
  mutate(cum_b_k_rate = SO/AB,
         cum_b_bb_rate = uBB/AB) %>%
  rename(wOBA_cum_tot = wOBA) %>%
  select(-c("AB", "uBB", "SO"))

bat_rol_r <- roll_woba(bat_sum, 15) %>%
  filter(p_throws == "R") %>%
  arrange(batter, p_throws, game_date) %>%
  select("game_date","game_year", "batter", "AB", "uBB", "SO", "wOBA", "p_throws") %>%
  mutate(rol_b_k_rate_r = SO/AB,
         rol_b_bb_rate_r = uBB/AB) %>%
  rename(wOBA_rol_r = wOBA) %>%
  ungroup() %>%
  select(-c("AB", "uBB", "SO", "p_throws"))

bat_rol_l <- roll_woba(bat_sum, 15) %>%
  filter(p_throws == "L") %>%
  arrange(batter, p_throws, game_date) %>%
  select("game_date", "game_year", "batter", "AB", "uBB", "SO", "wOBA", "p_throws") %>%
  mutate(rol_b_k_rate_l = SO/AB,
         rol_b_bb_rate_l = uBB/AB) %>%
  rename(wOBA_rol_l = wOBA) %>%
  ungroup() %>%
  select(-c("AB", "uBB", "SO", "p_throws"))


bat_rol_tot <- roll_woba(bat_sum, 15, rl_excl = "Y") %>%
  arrange(batter, game_date) %>%
  select("game_date", "game_year", "batter", "AB", "uBB", "SO", "wOBA") %>%
  mutate(rol_b_k_rate = SO/AB,
         rol_b_bb_rate = uBB/AB) %>%
  rename(wOBA_rol_tot = wOBA) %>%
  select(-c("AB", "uBB", "SO"))

bat_all <- bat_cum_tot %>%
  left_join(bat_cum_l, by = c("game_date", "batter", "game_year")) %>%
  left_join(bat_cum_r, by = c("game_date", "batter", "game_year")) %>%
  left_join(bat_rol_tot, by = c("game_date", "batter", "game_year")) %>%
  left_join(bat_rol_l, by = c("game_date", "batter", "game_year")) %>%
  left_join(bat_rol_r, by = c("game_date", "batter", "game_year")) %>%
  distinct() %>%
  group_by(batter, game_year) %>%
  mutate(real_dt = lag(game_date))

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
  left_join(bat_all[2:22], by = c("batter" = "batter", "game_date" = "real_dt")) %>%
  left_join(pitch_all[2:22], by = c("pitcher" = "pitcher", "game_date" = "real_dt")) %>%
  left_join(sprint.speed, by=c("batter" = "player_id", "game_year" = "game_year")) %>%
  left_join(bat_all[2:22], by = c("on_deck" = "batter", "game_date" = "real_dt")) %>%
  left_join(sprint.speed, by=c("on_1b" = "player_id", "game_year.y" = "game_year")) %>%
  select(-c("game_year.y", "game_year.x.x", "game_year.y.y"))

input_df <- function(df){
  
    df1 <- df %>%
      filter(stand == "R" & p_throws == "R") %>%
      select(-c("wOBA_cum_l.x", "cum_b_k_rate_l.x", "cum_b_bb_rate_l.x", "wOBA_cum_l.y", "cum_b_k_rate_l.y", "cum_b_bb_rate_l.y",
                "wOBA_rol_l.x", "rol_b_k_rate_l.x", "rol_b_bb_rate_l.x", "wOBA_rol_l.y", "rol_b_k_rate_l.y", "rol_b_bb_rate_l.y",
                "FIP_rol_l", "rol_p_k_rate_l", "rol_p_bb_rate_l", "FIP_cum_l", "cum_p_k_rate_l", "cum_p_bb_rate_l")) %>%
      rename(wOBA_cum_hand = wOBA_cum_r.x,
             cum_b_k_rate_hand = cum_b_k_rate_r.x,
             cum_b_bb_rate_hand = cum_b_bb_rate_r.x,
             wOBA_cum_hand_od = wOBA_cum_r.y,
             cum_b_k_rate_hand_od = cum_b_k_rate_r.y,
             cum_b_bb_rate_hand_od = cum_b_bb_rate_r.y,
             wOBA_rol_hand = wOBA_rol_r.x,
             rol_b_k_rate_hand = rol_b_k_rate_r.x,
             rol_b_bb_rate_hand = rol_b_bb_rate_r.x,
             wOBA_rol_hand_od = wOBA_rol_r.y,
             rol_b_k_rate_hand_od = rol_b_k_rate_r.y,
             rol_b_bb_rate_hand_od = rol_b_bb_rate_r.y,
             FIP_rol_hand = FIP_rol_r,
             rol_p_k_rate_hand = rol_p_k_rate_r,
             rol_p_bb_rate_hand = rol_p_bb_rate_r,
             FIP_cum_hand = FIP_cum_r,
             cum_p_k_rate_hand = cum_p_k_rate_r,
             cum_p_bb_rate_hand = cum_p_bb_rate_r)
    
    df2 <- df %>%
      filter(stand == "L" & p_throws == "L") %>%
      select(-c("wOBA_cum_r.x", "cum_b_k_rate_r.x", "cum_b_bb_rate_r.x", "wOBA_cum_r.y", "cum_b_k_rate_r.y", "cum_b_bb_rate_r.y",
                "wOBA_rol_r.x", "rol_b_k_rate_r.x", "rol_b_bb_rate_r.x", "wOBA_rol_r.y", "rol_b_k_rate_r.y", "rol_b_bb_rate_r.y",
                "FIP_rol_r", "rol_p_k_rate_r", "rol_p_bb_rate_r", "FIP_cum_r", "cum_p_k_rate_r", "cum_p_bb_rate_r")) %>%
      rename(wOBA_cum_hand = wOBA_cum_l.x,
             cum_b_k_rate_hand = cum_b_k_rate_l.x,
             cum_b_bb_rate_hand = cum_b_bb_rate_l.x,
             wOBA_cum_hand_od = wOBA_cum_l.y,
             cum_b_k_rate_hand_od = cum_b_k_rate_l.y,
             cum_b_bb_rate_hand_od = cum_b_bb_rate_l.y,
             wOBA_rol_hand = wOBA_rol_l.x,
             rol_b_k_rate_hand = rol_b_k_rate_l.x,
             rol_b_bb_rate_hand = rol_b_bb_rate_l.x,
             wOBA_rol_hand_od = wOBA_rol_l.y,
             rol_b_k_rate_hand_od = rol_b_k_rate_l.y,
             rol_b_bb_rate_hand_od = rol_b_bb_rate_l.y,
             FIP_rol_hand = FIP_rol_l,
             rol_p_k_rate_hand = rol_p_k_rate_l,
             rol_p_bb_rate_hand = rol_p_bb_rate_l,
             FIP_cum_hand = FIP_cum_l,
             cum_p_k_rate_hand = cum_p_k_rate_l,
             cum_p_bb_rate_hand = cum_p_bb_rate_l)
    
    df3 <- df %>%
      filter(stand == "R" & p_throws == "L") %>%
      select(-c("wOBA_cum_r.x", "cum_b_k_rate_r.x", "cum_b_bb_rate_r.x", "wOBA_cum_r.y", "cum_b_k_rate_r.y", "cum_b_bb_rate_r.y",
                "wOBA_rol_r.x", "rol_b_k_rate_r.x", "rol_b_bb_rate_r.x", "wOBA_rol_r.y", "rol_b_k_rate_r.y", "rol_b_bb_rate_r.y",
                "FIP_rol_l", "rol_p_k_rate_l", "rol_p_bb_rate_l", "FIP_cum_l", "cum_p_k_rate_l", "cum_p_bb_rate_l")) %>%
      rename(wOBA_cum_hand = wOBA_cum_l.x,
             cum_b_k_rate_hand = cum_b_k_rate_l.x,
             cum_b_bb_rate_hand = cum_b_bb_rate_l.x,
             wOBA_cum_hand_od = wOBA_cum_l.y,
             cum_b_k_rate_hand_od = cum_b_k_rate_l.y,
             cum_b_bb_rate_hand_od = cum_b_bb_rate_l.y,
             wOBA_rol_hand = wOBA_rol_l.x,
             rol_b_k_rate_hand = rol_b_k_rate_l.x,
             rol_b_bb_rate_hand = rol_b_bb_rate_l.x,
             wOBA_rol_hand_od = wOBA_rol_l.y,
             rol_b_k_rate_hand_od = rol_b_k_rate_l.y,
             rol_b_bb_rate_hand_od = rol_b_bb_rate_l.y,
             FIP_rol_hand = FIP_rol_r,
             rol_p_k_rate_hand = rol_p_k_rate_r,
             rol_p_bb_rate_hand = rol_p_bb_rate_r,
             FIP_cum_hand = FIP_cum_r,
             cum_p_k_rate_hand = cum_p_k_rate_r,
             cum_p_bb_rate_hand = cum_p_bb_rate_r)
    
    df4 <- df %>%
      filter(stand == "L" & p_throws == "R") %>%
      select(-c("wOBA_cum_l.x", "cum_b_k_rate_l.x", "cum_b_bb_rate_l.x", "wOBA_cum_l.y", "cum_b_k_rate_l.y", "cum_b_bb_rate_l.y",
                "wOBA_rol_l.x", "rol_b_k_rate_l.x", "rol_b_bb_rate_l.x", "wOBA_rol_l.y", "rol_b_k_rate_l.y", "rol_b_bb_rate_l.y",
                "FIP_rol_r", "rol_p_k_rate_r", "rol_p_bb_rate_r", "FIP_cum_r", "cum_p_k_rate_r", "cum_p_bb_rate_r")) %>%
      rename(wOBA_cum_hand = wOBA_cum_r.x,
             cum_b_k_rate_hand = cum_b_k_rate_r.x,
             cum_b_bb_rate_hand = cum_b_bb_rate_r.x,
             wOBA_cum_hand_od = wOBA_cum_r.y,
             cum_b_k_rate_hand_od = cum_b_k_rate_r.y,
             cum_b_bb_rate_hand_od = cum_b_bb_rate_r.y,
             wOBA_rol_hand = wOBA_rol_r.x,
             rol_b_k_rate_hand = rol_b_k_rate_r.x,
             rol_b_bb_rate_hand = rol_b_bb_rate_r.x,
             wOBA_rol_hand_od = wOBA_rol_r.y,
             rol_b_k_rate_hand_od = rol_b_k_rate_r.y,
             rol_b_bb_rate_hand_od = rol_b_bb_rate_r.y,
             FIP_rol_hand = FIP_rol_l,
             rol_p_k_rate_hand = rol_p_k_rate_l,
             rol_p_bb_rate_hand = rol_p_bb_rate_l,
             FIP_cum_hand = FIP_cum_l,
             cum_p_k_rate_hand = cum_p_k_rate_l,
             cum_p_bb_rate_hand = cum_p_bb_rate_l)
    
    df_all <- rbind(df1, df2, df3, df4)
    
    return(df_all)
}

sit1_input <- input_df(sit1_beg)



sit1_end <- pbp %>% select(game_date,game_year,game_pk,at_bat_number, batter, pitcher, inning, inning_topbot, stand, p_throws, on_3b, on_2b, on_1b, outs_when_up,bat_score) %>%
  filter(outs_when_up == 2 & is.na(on_1b) & !is.na(on_2b) & is.na(on_3b)) %>%
  distinct() %>%
  left_join(fin_score, by = c("game_date","game_year", "game_pk", "inning", "inning_topbot")) %>%
  mutate(runs_created = end_score - bat_score) %>%
  left_join(b_order, by = c("game_pk", "game_date", "game_year", "batter", "inning", "inning_topbot", "at_bat_number")) %>%
  left_join(bat_all[2:22], by = c("batter" = "batter", "game_date" = "real_dt")) %>%
  left_join(pitch_all[2:22], by = c("pitcher" = "pitcher", "game_date" = "real_dt")) %>%
  left_join(sprint.speed, by=c("batter" = "player_id", "game_year" = "game_year")) %>%
  left_join(bat_all[2:22], by = c("on_deck" = "batter", "game_date" = "real_dt")) %>%
  left_join(sprint.speed, by=c("on_2b" = "player_id", "game_year.y" = "game_year")) %>%
  select(-c("game_year.y", "game_year.x.x", "game_year.y.y"))


rm(b_order, bat_all, bat_sum, fin_score, pitch_all, pitch_sum, pitch_cum_rl)


###############################################
# Fit a GLM - Poisson model for runs generated
################################################

rc_pois <- glm(runs_created ~ wOBA_cum_hand + wOBA_rol_tot.x + wOBA_rol_hand + FIP_rol_tot +
                 rol_p_k_rate + rol_p_bb_rate + FIP_rol_hand + rol_p_k_rate_hand + rol_p_bb_rate_hand +
                 sprint_speed.x + wOBA_cum_hand_od + wOBA_rol_tot.y + wOBA_rol_hand_od + sprint_speed.y
               , data=sit1_input[,c(2,17:56)], family = poisson)
summary(rc_pois)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# LASSO REGRESSION - Find important variables for Poisson Model
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
library(glmnet)
install.packages("glmnet")

sit1_input_fin <- sit1_input[,c(2,17,19:56)]
sit1_input_fin <- sit1_input_fin[complete.cases(sit1_input_fin),]

x<- model.matrix(runs_created ~., sit1_input_fin)[,-1]
y <- sit1_input_fin$runs_created

set.seed(3453)
train <- sample(1:nrow(x), nrow(x)*.75)
test <- (-train)
y.test <- y[test]

grid <- 10^seq(10,-2,length = 100)

lasso.mod <- glmnet(x[train,], y[train], alpha = 1, family = "poisson", lambda = grid)
plot(lasso.mod)

set.seed(4324)
cv.out <- cv.glmnet(x[train,], y[train], alpha = 1, family = "poisson")
plot(cv.out)
bestlam <- cv.out$lambda.min
lasso.pred <- predict(lasso.mod, s = bestlam, newx = x[test,])

out <- glmnet(x,y, alpha = 1, lambda = grid, family = "poisson")
lasso.coef <- predict(out, type = "coefficients", s = bestlam)
as.data.frame(lasso.coef)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#Random Forest
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

bag.bunt <- randomForest(runs_created ~., data = sit1_input_fin,
                         subset = train, mtry = 39, importance = TRUE)

yhat.bag <- predict(bag.bunt, newdata = sit1_input_fin[-train,])
mean((yhat.bag - y.test)^2)

rf.bunt <- randomForest(runs_created ~., data = sit1_input_fin,
                         subset = train, mtry = 13, importance = TRUE)
yhat.rf <- predict(rf.bunt, newdata = sit1_input_fin[-train,])
mean((yhat.rf - y.test)^2)
importance(rf.bunt)

library(gbm)

boost.bunt <- gbm(runs_created ~., data = sit1_input_fin[train,],
                  distribution = "poisson", n.trees = 2000,
                  interaction.depth = 4)
summary(boost.bunt)
yhat.boost <- predict(boost.bunt, newdata = sit1_input_fin[-train,], type = "response", n.trees = 500)
mean((yhat.boost - y.test)^2)





