############################################
##                SCRIPT 04               ##
##           DATA VISUALISATIONS          ##
############################################

# Source setup scripts:
source(here::here("Desktop", "TGL", "research", "scr","scr_00.R"))
load("data/lobby.Rda")

# dataset with trust
oecd_trust <- read_csv("data/oecd_trust.csv")
# extracting the value of trust for the year 2019:
trust <- oecd_trust %>% 
  filter (LOCATION != "CZE") %>% 
  filter (TIME == 2019) %>% 
  select(LOCATION, Value) %>% 
  rename (TRUST = Value)

# dataset with CPI:
cpi_12_19 <- read_delim("data/cpi_12_19.csv", 
                        ";", escape_double = FALSE, trim_ws = TRUE)
# extracting the value of cpi for the year 2019:
target <- c("AUT", "BEL", "CHE", "DEU", "DNK", "ESP", "EST", "FIN", "FRA", "GBR", "GRC", "HUN", "IRL", "ITA", "LTU", "LUX", "LVA",
            "NLD", "NOR", "POL", "PRT","SVK", "SVN", "SWE")
cpi <- cpi_12_19 %>% 
  filter (ISO3 %in% target) %>% 
  select (ISO3, `CPI score 2019`) %>% 
  group_by(ISO3) %>% 
  rowwise()  %>% 
  rename (CPI = `CPI score 2019`)

# merging datasets: 
outcomes <- merge(cpi, trust, by.x="ISO3", by.y="LOCATION")


# bar plot CPI: ------------

# plot
ggplot(cpi, aes(x = reorder(ISO3, + CPI), y = CPI)) +
  geom_bar(stat="identity") +
  xlab("European countries") + ylab("Corruption Perceptions Index") +
  geom_text(aes(label=CPI), position=position_dodge(width=0.9), vjust=-0.25, size=3.5) +
  theme_minimal() 

# bar plot TRUST: ------------

# plot
ggplot(trust, aes(x = reorder(LOCATION, + TRUST), y = TRUST)) +
  geom_bar(stat="identity") +
  xlab("European countries") + ylab("Trust in Government") +
  geom_text(aes(label=round(TRUST, digits = 2)), position=position_dodge(width=0.9), vjust=-0.25, size=3.5) +
  theme_minimal() 



