############################################
##                SCRIPT 01               ##
##   LOADING DATASET and PRE-PROCESSING   ##
############################################


# Source setup scripts:
source(here::here("scr","scr_00.R"))

# Importing the datasets
lobby_qca <- read_csv("data/lobby_QCA.csv")
# dataset with trust
oecd_trust <- read_csv("data/oecd_trust.csv")
# dataset with CPI:
cpi_12_19 <- read_delim("data/cpi_12_19.csv", 
                               ";", escape_double = FALSE, trim_ws = TRUE)

# extracting the mean of trust values for the years 17-18-19:
mean_trust <- oecd_trust %>% 
  filter (LOCATION != "CZE") %>% 
  filter (TIME != 2016 & TIME != 2020) %>% 
  group_by(LOCATION) %>% 
  summarise(MEAN_TRUST = mean (Value)) 

# extracting the value of trust for the year 2019:
trust <- oecd_trust %>% 
  filter (LOCATION != "CZE") %>% 
  filter (TIME == 2019) %>% 
  select(LOCATION, Value) %>% 
  rename (TRUST = Value)

# extracting the mean of cpi for the years 17-18-19:
target <- c("AUT", "BEL", "CHE", "DEU", "DNK", "ESP", "EST", "FIN", "FRA", "GBR", "GRC", "HUN", "IRL", "ITA", "LTU", "LUX", "LVA",
            "NLD", "NOR", "POL", "PRT","SVK", "SVN", "SWE")

mean_cpi <- cpi_12_19 %>% 
  filter (ISO3 %in% target) %>% 
  select (ISO3, `CPI score 2019`| `CPI score 2018`| `CPI score 2017`) %>% 
  group_by(ISO3) %>% 
  rowwise() %>% 
  summarise(MEAN_CPI = mean (c(`CPI score 2019`, `CPI score 2018`, `CPI score 2017`))) 

# extracting the value of cpi for the year 2019:
cpi <- cpi_12_19 %>% 
  filter (ISO3 %in% target) %>% 
  select (ISO3, `CPI score 2019`) %>% 
  group_by(ISO3) %>% 
  rowwise()  %>% 
  rename (CPI = `CPI score 2019`)


# merging datasets: 
lobby_qca <- merge(lobby_qca, trust, by.x="Country code", by.y="LOCATION")
lobby_qca <- merge(lobby_qca, mean_trust, by.x="Country code", by.y="LOCATION")
lobby_qca <- merge(lobby_qca, cpi, by.x="Country code", by.y="ISO3")
lobby_qca <- merge(lobby_qca, mean_cpi, by.x="Country code", by.y="ISO3")

# Renaming variables
dat <- lobby_qca %>% 
  rename(
    COUNTRY = `Country`,
    CNTRY = `Country code` ,
    LAW = `Law on lobbying (national)`,
    REGISTER = `Register`,
    REG_OVER = `Register Oversight`,
    CODE = `Code of conduct`,
    DEF_LOB = `Definition of lobbyist`,
    DEF_LOB_ACT = `Definition of lobby activity`,
    TARGET = `State actor target`,
    COOLING = `Cooling-off`,
    DECL = `Periodical declarations`,
    MEETING = `Transparency on meetings`,
    SANCTION_LIM = `Sanctions limiting lobbying activity`,
    SANCTION_TRI = `Tribunal sanctions`,
    DISTANCE2019 = `Distance Year / 2019`,
    judicial_ind = `Judicial Independence (IPI)`,
    adm_burden = `Administrative Burden (IPI)`,
    trade_ope = `Trade Openness (IPI)`,
    budget_trans = `Budget Transparency (IPI)`,
    e_cit = `E-Citizenship (IPI)`,
    free_press = `Freedom of the Press (IPI)`,
    MEAN_CPI = MEAN_CPI,
    MEAN_TRUST = MEAN_TRUST, 
    TRUST = TRUST, 
    CPI = CPI)

# Saving dataset: 
save(dat,file="data/lobby_dat.Rda")


