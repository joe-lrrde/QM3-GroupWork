#install.packages("stargazer")
#install.packages("performance")
#install.packages("insight")
#install.packages("see")
#install.packages("readxl")
library(stargazer)
library(performance)
library(insight)
library(see)
library(readxl)
# ==========================================
# 1. DATA LOADING AND DESCRIPTIVE STATISTICS
# ==========================================
#We have to import the data 
Renewable_energies_dataset <- read_excel("Renewable_energies_dataset.xls")
GDP_per_capita_dataset <- read_excel("GDP per capita dataset.xls")
Gross_Capital_Formation_percentage_of_GDP_ <- read_excel("Gross Capital Formation (percentage of GDP).xls")
Urban_population_percentage_of_total_ <- read_excel("Urban population (percentage of total).xls")
Total_natural_resources_rents_of_GDP_ <- read_excel("Total natural resources rents (% of GDP).xls")
Trade_of_GDP_ <- read_excel("Trade (% of GDP).xls")
Foreign_direct_investement<- read_excel("Foreign direct investment, net inflows (% of GDP).xls")
C02_emissions_per_capita <- read_excel("C02 emissions per capita.xls")
#We have multiple years but we choose 2021 because it is the most recent one with good data
recentdata<-data.frame(Country = Renewable_energies_dataset$`Country Name`, 
                       Renewable_energy_consumption = Renewable_energies_dataset$`2021`)
recentdata <- within(recentdata, {
              GDP_per_capita = GDP_per_capita_dataset$`2021`
              Gross_Capital  = Gross_Capital_Formation_percentage_of_GDP_$`2021`
              Urban_pop      = Urban_population_percentage_of_total_$`2021`
              Total_natural  = Total_natural_resources_rents_of_GDP_$`2021`
              Trade_GDP      = Trade_of_GDP_$`2021`
              Foreign_direct_investement= Foreign_direct_investement$`2021`
              C02_emissions  = C02_emissions_per_capita$`2021`
              })
recentdata <-na.omit(recentdata)
#We create a table of descriptive statistics or "Table 1"
stargazer(recentdata, type="text", digits=1,
          title ="Table 1: Summary Statistics of the Variables (2021)")
# ==========================================
# 2. BIVARIATE AND LOGARITHMIC OLS MODELS
# ==========================================
#We create our first OLS of renewable energy consumption and GDP per capita
modA1 <- lm(Renewable_energy_consumption ~ GDP_per_capita, data = recentdata)
stargazer(modA1, type = "text", 
          title ="Table 2: OLS results for Renewable Energy Consumption and GDP per capita")
#Sigificant relation between the variables (comment R squared, F statistics...)
plot(recentdata$GDP_per_capita, recentdata$Renewable_energy_consumption,
     xlab = "GDP per Capita", 
     ylab = "Renewable Energy Consumption",
     main = "Relation between GDP per Capita and Renewable Consumption",
     col="blue")
abline(modA1, col="red", lwd=2)
#Now we try using logarithms in the GDP per capita to increase the precision of the model
modA2 <- lm(Renewable_energy_consumption ~ log(GDP_per_capita), data =recentdata)
stargazer(modA2, type = "text", 
          title ="Table 3: OLS results for log(Renewable Energy Consumption) and log(GDP per capita)")
#It is more significant!
plot(log(recentdata$GDP_per_capita), recentdata$Renewable_energy_consumption,
     xlab = "log(GDP per Capita)", 
     ylab = "Renewable Energy Consumption",
     main = "Relation between log(GDP per Capita) and Energy Consumption",
     col="blue")
abline(modA2, col="red", lwd=2)
#We are going to check the relationship with other variables
modA3 <- lm(Renewable_energy_consumption ~ Gross_Capital, data = recentdata)
stargazer(modA3, type = "text", 
          title ="Table 4: OLS results for Renewable Energy Consumption and Gross Capital Formation")
#No significant relationship between Renewable Energy Consumption and Gross Capital Formation
plot(recentdata$Gross_Capital, recentdata$Renewable_energy_consumption,
     xlab = "Gross Capital Formation (% of GDP)", 
     ylab = "Renewable Energy Consumption",
     main = "Relation between Gross Capital Formation and Renewable Consumption",
     col="blue")
abline(modA3, col="red", lwd=2)
#Now we check with urban population
modA4 <- lm(Renewable_energy_consumption ~ Urban_pop, data = recentdata)
stargazer(modA4, type = "text", 
          title ="Table 5: OLS results for Renewable Energy Consumption and Urban Population")
#Really good relation between the variables, the best so far
plot(recentdata$Urban_pop, recentdata$Renewable_energy_consumption,
     xlab = "Urban Population (% of total)", 
     ylab = "Renewable Energy Consumption",
     main = "Relation between Urban Population and Renewable Consumption",
     col="blue")
abline(modA4, col="red", lwd=2)
#We try with a logarithmic model to see if we can increase the precision
modA5<-lm(Renewable_energy_consumption ~ log(Urban_pop), data =recentdata)
stargazer(modA5, type = "text", 
          title ="Table 6: OLS results for Renewable Energy Consumption and log(Urban Population)")
#It does not improve using logarithms
plot(log(recentdata$Urban_pop), recentdata$Renewable_energy_consumption,
     xlab = "log(Urban Population as % of total)", 
     ylab = "Renewable Energy Consumption",
     main = "Relation between log(Urban Population) and Renewable Consumption",
     col="blue")
abline(modA5, col="red", lwd=2)
#We try new variables
modA6 <- lm(Renewable_energy_consumption ~ Total_natural, data = recentdata)
stargazer(modA6, type = "text", 
          title ="Table 7: OLS results for Renewable Energy Consumption and Total Natural Resources")
#There is no significant relationship between those two variables
plot(recentdata$Total_natural, recentdata$Renewable_energy_consumption,
     xlab = "Total Natural Resources (% of GDP)", 
     ylab = "Renewable Energy Consumption",
     main = "Relation between Total Natural Resources and Renewable Consumption",
     col="blue")
abline(modA6, col="red", lwd=2)
#We try with the last variable
modA7 <- lm(Renewable_energy_consumption ~ Trade_GDP, data = recentdata)
stargazer(modA7, type = "text", 
          title ="Table 8: OLS results for Renewable Energy Consumption and Trade as % of GDP")
#Good relationship between the variables 
plot(recentdata$Trade_GDP, recentdata$Renewable_energy_consumption,
     xlab = "Total Trade (% of GDP)", 
     ylab = "Renewable Energy Consumption",
     main = "Relation between Total Trade and Renewable Consumption",
     col="blue")
abline(modA7, col="red", lwd=2)
#We try using logarithms in this case
modA8 <-lm(Renewable_energy_consumption ~ log(Trade_GDP), data =recentdata)
stargazer(modA8, type = "text", 
          title ="Table 9: OLS results for Renewable Energy Consumption and log(Trade as % of GDP)")
#It slightly improves using logarithms
plot(log(recentdata$Trade_GDP), recentdata$Renewable_energy_consumption,
     xlab = "log(Total Trade as % of GDP)", 
     ylab = "Renewable Energy Consumption",
     main = "Relation between log(Total Trade) and Renewable Consumption",
     col="blue")
abline(modA8, col="red", lwd=2)
#We check the Direct Foreign Investment
modA9 <- lm(Renewable_energy_consumption ~ Foreign_direct_investement, data = recentdata)
summary(modA9)
stargazer(modA9, type = "text", 
          title ="Table 10: OLS results for Renewable Energy Consumption and Foreign direct investment")
#There is no significant relationship between those two variables
plot(recentdata$Foreign_direct_investement, recentdata$Renewable_energy_consumption,
     xlab = "Foreign direct investment", 
     ylab = "Renewable Energy Consumption",
     main = "Relation between Foreign direct investment and Renewable Consumption",
     col="blue")
abline(modA9, col="red", lwd=2)
# ===========================================
# 3. MULTIVARIABLE MODEL AND MODEL SELECTION
# ===========================================
#Finally, we are going to do a multivariate OLS
mod_multiple <-lm(Renewable_energy_consumption ~ 
               log(GDP_per_capita) +
               Urban_pop + 
               Gross_Capital + 
               log(Trade_GDP) + 
               Total_natural +
               Foreign_direct_investement , 
               data = recentdata)
stargazer(mod_multiple, type = "text", 
          title ="Table 11: Multivariable OLS results for Renewable Energy Consumption and the other variables")
par(mfrow = c(2, 2))
plot(mod_multiple)

# Really good performing model, and log(GDP_per_capita) remains highly significant, 
# but Trade_GDP and Urban_pop lose significance..
#We are going to compare the models we have
model_comparison <- compare_performance(modA1, modA2, modA3, modA4, modA5, modA6,
                                        modA7, modA8, modA9, mod_multiple, rank = TRUE)
print_md(model_comparison)
par(mfrow = c(1, 1))
plot(model_comparison)
#The best model is modA2 
#Using that command it should display the comparison correctly in R markdown
# ==========================================
# 4. ENVIRONMENTAL IMPACT ANALYSIS (CO2)
# ==========================================
#As a conclusion we are going to check if there is a relation between Renewable 
#Energy Consumption and CO2 emissions per capita
modA10 <- lm(Renewable_energy_consumption ~ C02_emissions, data = recentdata)
stargazer(modA10, type = "text", 
          title ="Table 12: OLS results for Renewable Energy Consumption and C02 emissions per capita")
#Strong relation
plot(recentdata$C02_emissions, recentdata$Renewable_energy_consumption,
     xlab = "C02 emissions per capita", 
     ylab = "Renewable Energy Consumption",
     main = "Relation between C02 emissions and Renewable Consumption",
     col="blue")
abline(modA10, col="red", lwd=2)
#We try with a logarithmic model
modA11 <- lm(Renewable_energy_consumption ~ log(C02_emissions), data = recentdata)
stargazer(modA11, type = "text", 
          title ="Table 13: OLS results for Renewable Energy Consumption and log(C02 emissions per capita)")
#Much better using logarithms!
plot(log(recentdata$C02_emissions), recentdata$Renewable_energy_consumption,
     xlab = "log(C02 emissions per capita)", 
     ylab = "Renewable Energy Consumption",
     main = "Relation between log(C02 emissions) and Renewable Consumption",
     col="blue")
abline(modA11, col="red", lwd=2)