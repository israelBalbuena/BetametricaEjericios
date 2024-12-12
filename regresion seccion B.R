# --------------------------------PACKAGES -------------------------------------#

library(pacman)

p_load(tidyverse, siebanxicor,magrittr, lubridate,car, 
       lmtest, hrbrthemes, GGally, moments, nortest, sandwich,
       strucchange, broom, caret, janitor, patchwork,forecast, 
       dynlm, nlme, strucchange)


options(scipen = 999)


#------------------------------ DATA CLEANING ---------------------------------#


token_banxico <- "2f80e6c992be4007948965a6da5957e50ca5884e660815517addefdcf5e7ef7e"

siebanxicor::setToken(token_banxico)

# ingresos <-  getSeriesData("SG9" ) %>% as.data.frame() #19977-01-01, 2024-10-01, millones de pesos pesos
# 
# igae <- getSeriesData("SR17692") %>% as.data.frame() # 1993-01-01, 2024-09-01
# 
# remesas <- getSeriesData("SE27803") %>% as.data.frame() #1995-01-01 2024-10-01, millones de dolares
# 
# salario <- getSeriesData("SL11298") %>% as.data.frame() #1964-01-01 2024-10-01, pesos por día  Salarios Mínimos General Pesos por día


base <- getSeriesData(c("SG9", "SR17692", "SE27803", "SL11298"), 
                          startDate = "1995-01-01",endDate = "2024-09-01") %>% 
            as.data.frame() %>% 
            select(-contains(".date")) %>% 
            rename(
                  "Ingresos" = SG9.value,
                  "Igae" = SR17692.value, 
                  "remesas" = SE27803.value,
                  "salario" = SL11298.value) %>% 
            mutate(
                  "fecha" =  seq(as_date("1995-01-01"),as_date("2024-09-01"), by ="month")
            )

# ----------------------- MODELO ----------------------------------------------#

modelo1 <-   base %$%
                lm( log(Ingresos)  ~ Igae)

summary(modelo1)             


# --------------------- SUPUESTOS ---------------------------------------------#


# normalidad : rechamos normalidad 


qqnorm(modelo1$residuals)
qqline(modelo1$residuals, col="red")

jarque.test(modelo1$residuals) # hay normalidad

shapiro.test(modelo1$residuals) # no hay normalidad

ad.test(modelo2$residuals) # no normalidad

# homocedasticidad  # aceptamos homocedasticidad 


bptest(modelo1)

augment(modelo1) %>% 
  ggplot(mapping = aes(.fitted, .resid))+
  geom_point() +
  geom_smooth(method = "lm")


# colinealidad 

vif(modelo1)

#autocorrelacion 

bgtest(modelo1) # autocorrelacion


dwtest(modelo1)

# estabilidad en los parametros 


sctest(modelo1, type="OLS-MUSUM") # los parametros son estables

plot(
  
  efp(modelo1, data = base, type = "OLS-CUSUM")
  
)

# que variable cause heterocedasticidad


lm(modelo1$residuals ~ Igae, data=base ) %>% summary()

example("corMatrix")

cor(base[1:4])


modelo2 <- base %>% 
              filter(Ingresos<3500000) %$% 
                    lm(log(Ingresos) ~ Igae )

summary(modelo2)


cor(base[2:4])














       
       
      








  