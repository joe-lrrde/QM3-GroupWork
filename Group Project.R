#install.packages("stargazer")
#install.packages("performance")
#install.packages("insight")
#install.packages("see")
#install.packages("readxl")
#install.packages("gvlma")
#install.packages("patchwork")
#install.packages("qqplotr")
library(stargazer)
library(performance)
library(insight)
library(see)
library(readxl)
library(gvlma)
library(patchwork)
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
Foreign_direct_investment_inflow<- read_excel("Foreign direct investment, net inflows (% of GDP).xls")
#We have multiple years but we choose 2021 because it is the most recent one with good data
recentdata<-data.frame(Country = Renewable_energies_dataset$`Country Name`, 
                       Renewable_energy_consumption = Renewable_energies_dataset$`2021`)
recentdata <- within(recentdata, {
              GDP_per_capita = GDP_per_capita_dataset$`2021`
              Gross_Capital  = Gross_Capital_Formation_percentage_of_GDP_$`2021`
              Urban_pop      = Urban_population_percentage_of_total_$`2021`
              Total_natural  = Total_natural_resources_rents_of_GDP_$`2021`
              Trade_GDP      = Trade_of_GDP_$`2021`
              Foreign_direct_investment= Foreign_direct_investment_inflow$`2021`
              })
recentdata <-na.omit(recentdata)
#We see the start of recentdata
head(recentdata)
#We create a table of descriptive statistics or "Table 1"
stargazer(recentdata, type="html", digits=1,
          title ="Table 1: Summary Statistics of the Variables (2021)")
#We do a boxplot of each variable
boxplot(recentdata$Renewable_energy_consumption, main="Renewable Energy Consumption", ylab="Percentage of GDP", col="blue")
boxplot(recentdata$GDP_per_capita, main="GDP per Capita", ylab="2015 Dollars", col="green")
boxplot(recentdata$Gross_Capital, main="Gross Capital Formation", ylab="Percentage of GDP", col="violet")
boxplot(recentdata$Urban_pop, main="Urban population", ylab="Percentage of Total", col="red")
boxplot(recentdata$Total_natural, main="Total natural resources rents", ylab="Percentage of GDP", col="orange")
boxplot(recentdata$Trade_GDP, main="Trade", ylab="Percentage of GDP", col="maroon")
boxplot(recentdata$Foreign_direct_investment, main="Foreign direct investment, net inflows", ylab="Percentage of GDP", col="purple")
# ==========================================
# 2. BIVARIATE AND LOGARITHMIC OLS MODELS
# ==========================================
#We create our first OLS of renewable energy consumption and GDP per capita
mod_gdp <- lm(Renewable_energy_consumption ~ GDP_per_capita, data = recentdata)
stargazer(mod_gdp, type="html", 
          title ="Table 2: OLS results for Renewable Energy Consumption and GDP per capita")
#Sigificant relation between the variables (comment R squared, F statistics...)
plot(recentdata$GDP_per_capita, recentdata$Renewable_energy_consumption,
     xlab = "GDP per Capita", 
     ylab = "Renewable Energy Consumption",
     main = "Relation between GDP per Capita and Renewable Consumption",
     col="blue")
abline(mod_gdp, col="red", lwd=2)
#We check the validation of the linear model assumptions for this case
gvlma(mod_gdp)
plot(check_model(mod_gdp, panel = FALSE))
#Now we try using logarithms in the GDP per capita to increase the precision of the model
mod_log_gdp <- lm(Renewable_energy_consumption ~ log(GDP_per_capita), data =recentdata)
stargazer(mod_log_gdp, type="html", 
          title ="Table 3: OLS results for Renewable Energy Consumption and log(GDP per capita)")
#It is more significant!
plot(log(recentdata$GDP_per_capita), recentdata$Renewable_energy_consumption,
     xlab = "log(GDP per Capita)", 
     ylab = "Renewable Energy Consumption",
     main = "Relation between log(GDP per Capita) and Energy Consumption",
     col="blue")
abline(mod_log_gdp, col="red", lwd=2)
#We check the validation of the linear model assumptions for this case
gvlma(mod_log_gdp)
plot(check_model(mod_log_gdp, panel = FALSE))
#We are going to check the relationship with other variables
mod_gross_capital <- lm(Renewable_energy_consumption ~ Gross_Capital, data = recentdata)
stargazer(mod_gross_capital, type="html", 
          title ="Table 4: OLS results for Renewable Energy Consumption and Gross Capital Formation")
#No significant relationship between Renewable Energy Consumption and Gross Capital Formation
plot(recentdata$Gross_Capital, recentdata$Renewable_energy_consumption,
     xlab = "Gross Capital Formation (% of GDP)", 
     ylab = "Renewable Energy Consumption",
     main = "Relation between Gross Capital Formation and Renewable Consumption",
     col="blue")
abline(mod_gross_capital, col="red", lwd=2)
#We check the validation of the linear model assumptions for this case
gvlma(mod_gross_capital)
plot(check_model(mod_gross_capital, panel = FALSE))
#Now we check with urban population
mod_urban_pop <- lm(Renewable_energy_consumption ~ Urban_pop, data = recentdata)
stargazer(mod_urban_pop, type="html", 
          title ="Table 5: OLS results for Renewable Energy Consumption and Urban Population")
#Really good relation between the variables, the best so far
plot(recentdata$Urban_pop, recentdata$Renewable_energy_consumption,
     xlab = "Urban Population (% of total)", 
     ylab = "Renewable Energy Consumption",
     main = "Relation between Urban Population and Renewable Consumption",
     col="blue")
abline(mod_urban_pop, col="red", lwd=2)
#We check the validation of the linear model assumptions for this case
gvlma(mod_urban_pop)
plot(check_model(mod_urban_pop, panel = FALSE))
#We try with a logarithmic model to see if we can increase the precision
mod_log_urban_pop<-lm(Renewable_energy_consumption ~ log(Urban_pop), data =recentdata)
stargazer(mod_log_urban_pop, type="html", 
          title ="Table 6: OLS results for Renewable Energy Consumption and log(Urban Population)")
#It does not improve using logarithms
plot(log(recentdata$Urban_pop), recentdata$Renewable_energy_consumption,
     xlab = "log(Urban Population as % of total)", 
     ylab = "Renewable Energy Consumption",
     main = "Relation between log(Urban Population) and Renewable Consumption",
     col="blue")
abline(mod_log_urban_pop, col="red", lwd=2)
#We check the validation of the linear model assumptions for this case
gvlma(mod_log_urban_pop)
plot(check_model(mod_log_urban_pop, panel = FALSE))
#We try new variables
mod_nat_resources <- lm(Renewable_energy_consumption ~ Total_natural, data = recentdata)
stargazer(mod_nat_resources, type="html", 
          title ="Table 7: OLS results for Renewable Energy Consumption and Total Natural Resources")
#There is no significant relationship between those two variables
plot(recentdata$Total_natural, recentdata$Renewable_energy_consumption,
     xlab = "Total Natural Resources (% of GDP)", 
     ylab = "Renewable Energy Consumption",
     main = "Relation between Total Natural Resources and Renewable Consumption",
     col="blue")
abline(mod_nat_resources, col="red", lwd=2)
#We check the validation of the linear model assumptions for this case
gvlma(mod_nat_resources)
plot(check_model(mod_nat_resources, panel = FALSE))
#We try with the last variable
mod_trade <- lm(Renewable_energy_consumption ~ Trade_GDP, data = recentdata)
stargazer(mod_trade, type="html", 
          title ="Table 8: OLS results for Renewable Energy Consumption and Trade as % of GDP")
#Good relationship between the variables 
plot(recentdata$Trade_GDP, recentdata$Renewable_energy_consumption,
     xlab = "Total Trade (% of GDP)", 
     ylab = "Renewable Energy Consumption",
     main = "Relation between Total Trade and Renewable Consumption",
     col="blue")
abline(mod_trade, col="red", lwd=2)
#We check the validation of the linear model assumptions for this case
gvlma(mod_trade)
plot(check_model(mod_trade, panel = FALSE))
#We try using logarithms in this case
mod_log_trade <-lm(Renewable_energy_consumption ~ log(Trade_GDP), data =recentdata)
stargazer(mod_log_trade, type="html", 
          title ="Table 9: OLS results for Renewable Energy Consumption and log(Trade as % of GDP)")
#It slightly improves using logarithms
plot(log(recentdata$Trade_GDP), recentdata$Renewable_energy_consumption,
     xlab = "log(Total Trade as % of GDP)", 
     ylab = "Renewable Energy Consumption",
     main = "Relation between log(Total Trade) and Renewable Consumption",
     col="blue")
abline(mod_log_trade, col="red", lwd=2)
#We check the validation of the linear model assumptions for this case
gvlma(mod_log_trade)
plot(check_model(mod_log_trade, panel = FALSE))
#We check the Direct Foreign Investment
mod_foreign_investment <- lm(Renewable_energy_consumption ~ Foreign_direct_investment, data = recentdata)
summary(mod_foreign_investment)
stargazer(mod_foreign_investment, type="html", 
          title ="Table 10: OLS results for Renewable Energy Consumption and Foreign direct investment")
#There is no significant relationship between those two variables
plot(recentdata$Foreign_direct_investment, recentdata$Renewable_energy_consumption,
     xlab = "Foreign direct investment", 
     ylab = "Renewable Energy Consumption",
     main = "Relation between Foreign direct investment and Renewable Consumption",
     col="blue")
abline(mod_foreign_investment, col="red", lwd=2)
#We check the validation of the linear model assumptions for this case
gvlma(mod_foreign_investment)
plot(check_model(mod_foreign_investment, panel = FALSE))
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
               Foreign_direct_investment , 
               data = recentdata)
stargazer(mod_multiple, type="html", 
          title ="Table 11: Multivariable OLS results for Renewable Energy Consumption and the other variables")
# Really good performing model, and log(GDP_per_capita) remains highly significant, 
# but Trade_GDP and Urban_pop lose significance..
#We check the validation of the linear model assumptions for this case
gvlma(mod_multiple)
plot(check_model(mod_multiple, panel = FALSE))
#We are going to compare the models we have
model_comparison <- compare_performance(mod_gdp, mod_log_gdp, mod_gross_capital, mod_urban_pop, mod_log_urban_pop, mod_nat_resources,
                                        mod_trade, mod_log_trade, mod_foreign_investment, mod_multiple, rank = TRUE)
print_md(model_comparison)
par(mfrow = c(1, 1))
plot(model_comparison)
#The best model is mod_log_gdp 
#Using that command it should display the comparison correctly in R markdown