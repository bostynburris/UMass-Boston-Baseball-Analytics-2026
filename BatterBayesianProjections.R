library(tidyverse)
library(baseballr)
library(readxl)

HitterAvgThruFeb9 <- read_excel("HitterAvgThruFeb9.xlsx")
View(HitterAvgThruFeb9)

View(D3teamtotals)

teamtotalba <- D3teamtotals %>%
  select(H.x, AB, BA)
View(teamtotalba)

mu <- sum(teamtotalba$H.x) / sum(teamtotalba$AB)
mu

var <- var(teamtotalba$BA)  
var

var_noise <- mean(teamtotalba$BA * (1 - teamtotalba$BA) / teamtotalba$AB)
#### var_talent <- var - var_noise
#### var_talent <- max(var_talent, 1e-6)
#### var_talent = 450.793
#### since team total data adds way too much shrinkage when looking at individual data,
#### i need to set nu closer to 150 so there is more variance and way less shrinkage
#### at a more reasonable level

nu <- 150
alpha <- mu * nu
beta  <- (1 - mu) * nu

project_ba <- function(H, AB, alpha, beta) {
  (alpha + H) / (alpha + beta + AB)
}

HitterAvgThruFeb9$proj_BA <- project_ba(HitterAvgThruFeb9$H,
                              HitterAvgThruFeb9$AB,
                              alpha,
                              beta)

HitterAvgThruFeb9 <- HitterAvgThruFeb9 %>%
  mutate(
    proj_BA = round(proj_BA, 3),
    Avg = round(Avg, 3)
  )
###################################################################

#### Projection tool worked effectively. now blanketing across all hitter stats
#### that can prove to be useful for lineup construction

###################################################################

careerstats <- read.csv("careerstatsUMB2026.csv", header = TRUE)
careerstats

careerstats <- careerstats %>%
  select(-Pos_Yr, -B_T)
careerstats

returning_names <- c('Gouveia Justin',
                     'Miles Elliot',
                     'Parker Breon',
                     'Farnacci Nick',
                     'Gaer Brandon',
                     'Garner Max',
                     'Larkins P.',
                     'Slack Ryan',
                     'Piscionie N.',
                     'LaFleur Jayden',
                     'Bissaillon Chris',
                     'Carlson Andrew')

returnerscareer <- careerstats[careerstats$Name %in% returning_names, ]
returnerscareer

shift_rows <- is.na(returnerscareer$GP)

returnerscareer$GP[shift_rows] <- returnerscareer$CS[shift_rows]

returnerscareer$CS[shift_rows] <- 0
returnerscareer

returnerscareer <- returnerscareer %>%
  mutate(
    PA = AB + BB + HBP + SF + SH,
    k_pct = round(K/AB, 3),
    bb_pct = round(BB/PA, 3),
    hbp_pct = round(HBP/PA, 3),
    K_bb_hbp = round(K/(BB+HBP), 3),
    cs_pct = round(CS/(CS+SB), 3),
    power_profile = round((X2B + X3B + HR)/H, 3),
    rbi_profile = round(RBI/PA, 3),
    bip_pct = round((AB - K)/AB, 3),
    babip = round(H/(AB - K), 3),
    sb_pct = 1 - cs_pct
  )
View(returnerscareer)

returnerscareer <- returnerscareer %>%
  clean_names()

D3teamtotals <- D3teamtotals %>%
  mutate(
    k_pct = round(SO.x/AB, 3),
    bb_pct = round(BB.x/PA, 3),
    hbp_pct = round(HBP.x/PA, 3),
    k_bb_hbp = round(SO.x/(BB.x+HBP.x), 3),
    cs_pct = 1 - SB_pct,
    power_profile = round((X2B + X3B + HR.x)/H.x, 3),
    rbi_profile = round(RBI/PA, 3),
    bip_pct = round((AB - SO.x)/AB, 3),
    babip = round(H.x/(AB - SO.x), 3),
    sb_pct = SB_pct
  )
View(D3teamtotals)

hitterprediction <- D3teamtotals %>%
  select(Tm, k_pct, bb_pct, hbp_pct, k_bb_hbp, cs_pct, power_profile,
         rbi_profile, bip_pct, babip, sb_pct)

UMBhitterprediction <- returnerscareer %>%
  select(name, k_pct, bb_pct, hbp_pct, k_bb_hbp, cs_pct, power_profile,
         rbi_profile, bip_pct, babip, sb_pct)
UMBhitterprediction
hitterprediction

###################################################################

#### adding all 2026 hitter fall stats to model to combine and give approximations for all players on upcoming roster

fallstats2026 <- read_csv("fallstats2026.csv")
View(fallstats2026)

fallstats2026 <- fallstats2026 %>%
  clean_names()

fallstats2026 <- fallstats2026 %>%
  rename(
    sb = st,
  )

fallstats2026 <- fallstats2026 %>%
  rename(
    k_bb_hbp = K_bb_hbp
  )

fallstats2026 <- fallstats2026 %>%
  select(-x1b, -bk)

returnerscareer <- returnerscareer %>%
  select(-gp)

returnerscareer <- returnerscareer %>%
  rename(
    player = name
  )

fallstats2026 <- fallstats2026 %>%
  mutate(
    tb = (h - x2b - x3b - hr) + (2 * x2b) + (3 * x3b) + (4 * hr),
    k_pct = round(k/ab, 3),
    bb_pct = round(bb/pa, 3),
    hbp_pct = round(hbp/pa, 3),
    K_bb_hbp = round(k/(bb+hbp), 3),
    cs_pct = round(cs/(cs+sb), 3),
    power_profile = round((x2b + x3b + hr)/h, 3),
    rbi_profile = round(rbi/pa, 3),
    bip_pct = round((ab - k)/ab, 3),
    babip = round(h/(ab - k), 3),
    sb_pct = 1 - cs_pct,
    avg = h / ab,
    slg = tb / ab,
    obp = (hbp + bb + h) / (ab + bb + hbp + sac),
    ops = obp + slg,
    iso = slg - avg
  )

returnerscareer <- returnerscareer %>%
  mutate(
    sac = sf + sh
    )

returnerscareer <- returnerscareer %>%
  select(-sf,-sh)

fallstats2026 <- fallstats2026 %>%
  select(-K_bb_hbp)

returnerscareer <- returnerscareer %>%
  mutate(
    player_clean = str_trim(player),
    last_name = word(player_clean, -1)
  )

fallstats2026 <- fallstats2026 %>%
  mutate(
    player_clean = str_trim(player),
    last_name = word(player_clean, -1)
  )

returnerscareer <- returnerscareer %>%
  mutate(
    player = str_trim(player),
    player = ifelse(
      str_count(player, " ") == 1,
      paste(word(player, 2), word(player, 1)),
      player
    )
  )

preseasonstatshitters <- bind_rows(returnerscareer, fallstats2026)
preseasonstatshitters

unique(preseasonstatshitters$player)

preseasonstatshitters <- preseasonstatshitters %>%
  mutate(
    player = ifelse(player == "jayden LaFleur",
                    "Jayden LaFleur",
                    player)
  )

preseasonstatshitters <- preseasonstatshitters %>%
  mutate(
    player = ifelse(player == "N. Piscionie",
                    "N. Piscionere",
                    player)
  )

preseasonstatshitters <- preseasonstatshitters %>%
  filter(!player %in% c("Derek Hawley",
                        "Connor Seeley"
                      ))

unique(preseasonstatshitters$player)

preseasonstatshitters <- preseasonstatshitters %>%
  group_by(player) %>%
  summarise(
    ab  = sum(ab,  na.rm = TRUE),
    pa  = sum(pa,  na.rm = TRUE),
    h   = sum(h,   na.rm = TRUE),
    x2b = sum(x2b, na.rm = TRUE),
    x3b = sum(x3b, na.rm = TRUE),
    hr  = sum(hr,  na.rm = TRUE),
    r   = sum(r,   na.rm = TRUE),
    rbi = sum(rbi, na.rm = TRUE),
    tb  = sum(tb,  na.rm = TRUE),
    k   = sum(k,   na.rm = TRUE),
    bb  = sum(bb,  na.rm = TRUE),
    hbp = sum(hbp, na.rm = TRUE),
    sb  = sum(sb,  na.rm = TRUE),
    cs  = sum(cs,  na.rm = TRUE),
    sac = sum(sac, na.rm = TRUE),
    .groups = "drop"
  ) %>% mutate(
  avg = round(h / ab, 3),
  obp = round((h + bb + hbp) / (ab + bb + hbp + sac), 3),
  slg = round(tb / ab, 3),
  ops = obp + slg,
  iso = slg - avg,
  k_pct   = round(k / pa, 3),
  bb_pct  = round(bb / pa, 3),
  hbp_pct = round(hbp / pa, 3),
  k_bb_hbp = round((k / bb + hbp), 3),
  bip_pct = round((pa - k - bb - hbp - hr) / pa, 3),
  babip   = round((h - hr) / (ab - k - hr + sac), 3),
  sb_pct = round(sb / (sb + cs), 3),
  cs_pct = round(cs / (sb + cs), 3),
  power_profile = round((x2b + x3b + hr)/h, 3),
  rbi_profile   = round(rbi / pa, 3)
)

View(preseasonstatshitters)

###################################################################

league_means <- D3teamtotals %>%
  summarise(
    mu_k   = sum(SO.x)   / sum(AB),
    mu_bb  = sum(BB.x)  / sum(PA),
    mu_hbp = sum(HBP.x) / sum(PA),
    mu_pp  = mean(power_profile),
    mu_babip = (sum(H.x) - sum(HR.x)) /
      (sum(AB) - sum(SO.x) - sum(HR.x) + sum(SF) + sum(SH))
  )

var_k <- var(D3teamtotals$k_pct)
mu_k  <- mean(D3teamtotals$k_pct)
nu_k <- (mu_k * (1 - mu_k) / var_k) - 1
nu_k

var_bb <- var(D3teamtotals$bb_pct)
mu_bb <- mean(D3teamtotals$bb_pct)
nu_bb <- (mu_bb * (1 - mu_bb) / var_bb) - 1
nu_bb

var_hbp <- var(D3teamtotals$hbp_pct)
mu_hbp <- mean(D3teamtotals$hbp_pct)
nu_hbp <- (mu_hbp * (1 - mu_hbp) / var_hbp) - 1
nu_hbp

var_pp <- var(D3teamtotals$power_profile)
mu_pp <- mean(D3teamtotals$power_profile)
nu_pp <- (mu_pp * (1 - mu_pp) / var_pp) - 1
nu_pp

var_babip <- var(D3teamtotals$babip)
mu_babip <- mean(D3teamtotals$babip)
nu_babip <- (mu_babip * (1 - mu_babip) / var_babip) - 1
nu_babip


alpha_k  <- mu_k  * nu_k
beta_k   <- (1 - mu_k)  * nu_k

alpha_bb <- mu_bb * nu_bb
beta_bb  <- (1 - mu_bb) * nu_bb

alpha_hbp <- mu_hbp * nu_hbp
beta_hbp  <- (1 - mu_hbp) * nu_hbp

alpha_pp <- mu_pp * nu_pp
beta_pp  <- (1 - mu_pp) * nu_pp

alpha_babip <- mu_babip * nu_babip
beta_babip  <- (1 - mu_babip) * nu_babip

preseasonstatshitters <- preseasonstatshitters %>%
  mutate(
    proj_k_pct =
      (alpha_k + k) /
      (alpha_k + beta_k + pa)
  )

preseasonstatshitters <- preseasonstatshitters %>%
  mutate(
    proj_bb_pct =
      (alpha_bb + bb) /
      (alpha_bb + beta_bb + pa)
  )

preseasonstatshitters <- preseasonstatshitters %>%
  mutate(
    proj_hbp_pct =
      (alpha_hbp + hbp) /
      (alpha_hbp + beta_hbp + pa)
  )

preseasonstatshitters <- preseasonstatshitters %>%
  mutate(
    bip_events = ab - k + sac,
    proj_babip =
      (alpha_babip + h) /
      (alpha_babip + beta_babip + bip_events),
    proj_hits = proj_babip * bip_events
  )

preseasonstatshitters <- preseasonstatshitters %>%
  mutate(
    proj_power =
      (alpha_pp + x2b + x3b + hr) /
      (alpha_pp + beta_pp + h)
  )

preseasonstatshitters <- preseasonstatshitters %>%
  mutate(
    proj_bip_pct = round(1 - proj_k_pct - proj_bb_pct - proj_hbp_pct, 3),
    proj_avg = round(proj_hits / ab, 3),
    proj_obp = round((proj_hits + (proj_bb_pct * pa) + (proj_hbp_pct * pa)) /
                       (ab + (proj_bb_pct * pa) + (proj_hbp_pct * pa) + sac), 3),
    proj_slg = round(proj_avg + (proj_hits * proj_power / ab), 3),
    proj_ops = round(proj_obp + proj_slg, 3)
  )

preseasonhitterproj <- preseasonstatshitters %>%
  select(player, proj_avg, proj_obp, proj_slg, proj_ops, proj_k_pct, 
         proj_bb_pct, proj_hbp_pct, proj_babip,
          proj_bip_pct, proj_power)
View(preseasonhitterproj)

write.csv(preseasonhitterproj, "PreseasonHitterProjections.csv", row.names = FALSE)

###################################################################

