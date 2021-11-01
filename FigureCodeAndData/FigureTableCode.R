# Code for reproducing manuscript figures and tables

# Set working directory
setwd("~/dropbox/research/mystudies/ltwstudy_2017/es_specialissue/managementcostpaper/Github/LakeTahoeWest_ManagementCost/FigureCodeandData/") # Set the working directory to 

# Import and process management cost results data
management_results_r1 <- read.csv("emds_summaries.csv") # round 1
management_results_r2 <- read.csv("emds_summaries_130reps.csv") # round 2

management_results_r1 <- management_results_r1 %>% 
  mutate(round = "1") %>%   
  mutate(decade_num = 10*rep(1:10, 4)) %>% 
  mutate(climate.gcm = "CanESM",
         rcp= 4.5) %>% 
  mutate(biomass.cut.mtons_mean = NA, 
         biomass.cut.mtons_sd = NA) %>% 
  dplyr::select(X, round, scenario, climate.gcm, rcp, decade_num, decade:timber.cuft.annualavg_sd, biomass.cut.mtons_mean, biomass.cut.mtons_sd, cumulative.sequestration.allpools.mtons.carbon.decadeavg_mean, cumulative.sequestration.allpools.mtons.carbon.decadeavg_sd)

management_results_r2 <- management_results_r2 %>% 
  mutate(round = ifelse(scenario ==5, "1", "2")) %>%  # scenario 5 belongs in round 1 
  mutate(decade_num = 10*rep(1:10, 41)) %>% 
  dplyr::select(X, round, scenario, climate.gcm, rcp, decade_num, decade:cumulative.sequestration.allpools.mtons.carbon.decadeavg_sd)

management_results <- rbind(management_results_r1, management_results_r2)

## Filter to data frame for management cost analysis 
management_results <- management_results %>% 
  filter(round == 1)

# Figure 4
Fig4Data <- read.csv("/FigureCodeandData/Figure4Data.csv")

Fig4 <- Fig4Data %>% 
  ggplot() +
  geom_line(aes(x=DBH, HarvestPct)) +
  xlab("Diameter at breast height (inches)") + ylab("Percent Biomass Harvested") +
  ylim(0,100) +
  theme_bw()

# Figure 5 Fire Results
fire_ltw <- read.csv("~/dropbox/research/mystudies/ltwstudy_2017/es_specialissue/managementcostpaper/LTW_fire_byinten_allclim_allscen_scenv3.csv")
fire_ltw <- fire_ltw %>% 
  filter(Climate == "CanESM_4.5")

fire_ltw %>% 
  filter(!(Severity %in% c("Rx", "All_fire", "Total_wildfire"))) %>% 
  group_by(Scenario, Severity) %>% 
  summarize(Mean_acre = sum(Mean, na.rm=TRUE)) %>% 
  ggplot() +
  geom_bar(aes(x=Scenario, y=Mean_acre, fill=factor(Severity, levels=c("High","Medium","Low"))), stat="identity") + 
  xlab("") + ylab("Area Burned (acres)") +
  scale_fill_manual(name = "Fire Intensity",
                    values=c( "#56B4E9", "#009E73","#D55E00")) + 
  scale_x_discrete(name="", labels=c("Scenario1" = "Scenario 1", "Scenario2" = "Scenario 2", "Scenario3" = "Scenario 3", "Scenario4" = "Scenario 4", "Scenario5" = "Scenario 5")) +
  theme(legend.position = "") +
  theme_classic() 

# Figure 6
Fig6Input <- management_results %>% 
  filter(decade_num <= 30) %>% 
  group_by(scenario) %>% 
  summarize(TotalTreatmentCost = mean(manage.cost.rxfire.timberrev.transport.net_mean)/1000,
            TotalManagementCost = mean(all.costs.manage.timber.transport.rx.wildfire_mean)/1000,
            WildfireSuppressionCost = mean(wildfire.cost_mean)/1000,
            NetRevenue = mean(timber.biomass.rev.post.trans.net_mean)/1000) %>%
  mutate(TotalTreatmentCost = TotalTreatmentCost + NetRevenue) %>% 
  mutate(RxCost = ifelse(scenario==4, 550*750/1000,
                         ifelse(scenario==5, 2600*750/1000, 0))) %>% 
  mutate(ThinningCost = TotalTreatmentCost - RxCost) %>% 
  mutate(Scenario = ifelse(scenario == 1, "S1",
                           ifelse(scenario == 2, "S2",
                                  ifelse(scenario == 3, "S3",
                                         ifelse(scenario == 4, "S4",
                                                ifelse(scenario == 5, "S5",NA)))))) %>% 
  mutate(NetRevenue = -NetRevenue) %>% 
  dplyr::select(-scenario)

ResultsFig6 <- Fig6Input %>% 
  dplyr::select(-TotalTreatmentCost) %>%
  gather(key=CostCategory, value=Cost, -Scenario) %>% 
  mutate(CostCategory = factor(CostCategory, levels=c("ThinningCost", "NetRevenue", "RxCost", "WildfireSuppressionCost", "TotalManagementCost"))) %>% 
  ggplot() +
  geom_bar(aes(CostCategory, Cost, fill=CostCategory), stat="identity", position=position_dodge()) +
  facet_wrap(~Scenario, nrow=1) +
  scale_fill_manual(name = "Category", labels=c("Thinning", "Timber/Biomass Sale Subsidy", "Rx Fire", "Wildfire Suppression", "Total"),
                    values=c("#999999", "#E69F00", "#56B4E9", "#009E73","#D55E00")) + 
  ylab("Average Annual Cost (1,000$)") + xlab("") + 
  theme_classic() +
  theme(axis.text.x=element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_rect(color = "grey", fill = NA, size = 0.5))

# Figure 7
AcresTreated <- data.frame(Scenario = c("S1", "S2", "S3", "S4", "S5"),
                           AcresTreated = c(0, 952, 3614, 1398, 2801))
Fig7Input <- Fig6Input %>% 
  left_join(AcresTreated, by="Scenario") %>% 
  mutate(TotalCostPerAcre = 1000*TotalManagementCost / AcresTreated,
         TreatmentCostPerAcre = 1000*TotalTreatmentCost / AcresTreated)
  
Fig7Input  %>% 
  select(Scenario, TotalCostPerAcre, TreatmentCostPerAcre) %>% 
  filter(Scenario != "S1") %>% 
  gather(key="CostCategory", value="CostPerAcre", TotalCostPerAcre:TreatmentCostPerAcre) %>% 
  ggplot() +
  geom_bar(aes(x=Scenario, y=CostPerAcre, fill=CostCategory), position="dodge", stat="identity") +
  xlab("") + ylab("$/treated acre") +
  scale_x_discrete(labels=c("S1" = "Scenario 1", "S2" = "Scenario 2", "S3" = "Scenario 3", "S4" = "Scenario 4", "S5" = "Scenario 5")) +
  scale_fill_manual(name = "Cost Type",
                    labels = c("Treatment + Suppression", "Treatment only"),
                    values=c( "#56B4E9", "#D55E00")) + 
  theme_classic() +
  theme(legend.position = c(.7,.8)) 

ggsave("Figure7.png")

# Table 4
# First 10 years
management_results %>% 
  filter(decade_num == 10) %>% 
  dplyr::select(wildfire.cost_mean, manage.cost.rxfire.timberrev.transport.net_mean, timber.biomass.rev.post.trans.net_mean)

# First 30 years
management_results %>% 
  filter(decade_num <= 30) %>%
  group_by(scenario) %>% 
  summarize(wildfire.cost_mean = mean(wildfire.cost_mean),
            manage.cost.rxfire.timberrev.transport.net_mean = mean(manage.cost.rxfire.timberrev.transport.net_mean),
            timber.biomass.rev.post.trans.net_mean = mean(timber.biomass.rev.post.trans.net_mean)) %>% 
  dplyr::select(scenario, wildfire.cost_mean, manage.cost.rxfire.timberrev.transport.net_mean, timber.biomass.rev.post.trans.net_mean)

1653+2344-130
# over 100 years
management_results %>% 
  group_by(scenario) %>% 
  summarize(wildfire.cost_mean = mean(wildfire.cost_mean),
            manage.cost.rxfire.timberrev.transport.net_mean = mean(manage.cost.rxfire.timberrev.transport.net_mean),
            timber.biomass.rev.post.trans.net_mean = mean(timber.biomass.rev.post.trans.net_mean)) %>% 
  select(scenario, wildfire.cost_mean, manage.cost.rxfire.timberrev.transport.net_mean, timber.biomass.rev.post.trans.net_mean)

