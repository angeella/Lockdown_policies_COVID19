
require(COVID19) # for COVID data
require(dplyr) # for easiness of data manipulation
require(ggplot2) # for fancy plots
require(plotly) # for interactive plots
require(rnaturalearth) # for earth maps
require(rnaturalearthdata) # cont'ed
require(tidyr) # for easiness of coding
require(sf) # handles maps in the compact "sf" format
require(Hmisc) # just for computing lagged variables in the future
require(lme4) # handles mixed and random effects models
require(glmmTMB) # cont'ed
require(ciTools) # computes prediction intervals from RE models
source("lib/maps.R") # makes maps
source("lib/phases.R") # determines Phases Zero and One of each territory supplied

# World Bank data linkage, the name of final variable is on the left
wb <- c(
  "pop_0_14"="SP.POP.0014.TO.ZS",
  "pop_15_64"="SP.POP.1564.TO.ZS",
  "pop_65_up"="SP.POP.65UP.TO.ZS",
  "pop_density"="EN.POP.DNST",
  "pop_death_rate"="SP.DYN.CDRT.IN",
  "gdp"="NY.GDP.PCAP.CD",
  "health_exp"="SP.DYN.LE00.IN",
  "hosp_beds"="SH.MED.BEDS.ZS"
)

# loads COVID data
dat <- bind_rows(
  covid19(
    country = c(
      "Portugal", "Spain", "France", "Switzerland", "Ireland", "Austria", "United Kingdom", "Belgium", "Netherlands", "Germany", "Denmark", "Sweden", "Norway", "Korea, South", "Singapore"
    ), level = 1, wb=wb),
  covid19(country = "Italy", level = 2, wb=wb)
) %>%
  rename(
    # just to have shorter names
    country=administrative_area_level_1,
    region=administrative_area_level_2
  ) %>%
  arrange(country, region, date) %>%
  mutate(
    active=pmax(0, confirmed-deaths-recovered),
    date = date %>% as.Date(),
    region = region %>% replace_na("country")
  ) %>%
  group_by(country, region) %>%
  mutate( # this operation is performed safely within each group
    active.future=Hmisc::Lag(active,-14),
    tests.future=Hmisc::Lag(tests,-14), # negative lags not allowed by dplyr::lag()
    tests.past=Hmisc::Lag(tests,14)
  ) %>%
  as.data.frame() %>%
  calc.phases() %>%
  filter(confirmed > 30) # trimming first observations

map <- get.maps(dat) # loads maps

save(map, file="out/maps.RData") # saves maps for markdown

dat <- dat %>%
  filter(country=="Italy" & region!="country") # considering just Italian regions here

ggplotly(
dat %>%
  ggplot() +
  geom_line(aes(x=confirmed, y=tests, group=id, color=region))
)

dat <- dat %>%
  mutate(
    # that +0.5 approximates Firth's bias correction for binomial model
    denominator=log(0.5 + active),
    log_past_tests=log(tests - tests.past + 0.5), # same here, though just to avoid zeroes
    log_future_tests=log(tests.future - tests + 0.5), # cont'ed
    log_pop=log(population), # log variables fit in elasticity modelling approach
    log_dens=log(pop_density), # this way also region's size is accounted for
    overdispersion=1:nrow(dat) # day-and-region intercept
  )

# trimming data before early retail reopenings affects outcomes
dat <- dat %>%
  filter(date <= as.Date("2020-05-14")+14)

f.def <- active.future ~
  offset(denominator) +
  phase +
  log_past_tests +
  # log_future_tests +
  (phase+1|region) +
  (1|date) +
  (1|overdispersion) # +
  # log_pop #+
  # log_dens

model <- glmer(
  f.def,
  data = dat,
  family = poisson()
)


dat <- dat %>% right_join(model@frame %>% dplyr::select(region, date), by=c("region", "date"))
dat <- ciTools::add_pi(dat, model)
dat <- dat %>%
  rename(
    predlower=`LPB0.025`,
    predpoint=pred,
    predupper=`UPB0.975`
    )
dat <- dat %>%
  mutate(denominator=exp(denominator)) %>%
  mutate(
    ratelower=predlower/denominator,
    ratepoint=predpoint/denominator,
    rateupper=predupper/denominator
  )

save(dat, model, file="out/regions.RData")
