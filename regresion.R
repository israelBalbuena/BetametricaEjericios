# -------------------------------PACKAGES -------------------------------------#

library(pacman)

p_load(tidyverse, siebanxicor,magrittr, lubridate,car, 
       lmtest, hrbrthemes, GGally, moments, nortest, sandwich,
       strucchange, broom, caret, janitor, patchwork,forecast, 
       dynlm, nlme)


options(scipen = 999)


#------------------------------ DATA CLEANING ---------------------------------#


token_banxico <- "2f80e6c992be4007948965a6da5957e50ca5884e660815517addefdcf5e7ef7e"

siebanxicor::setToken(token_banxico)

base <-  getSeriesData(c("SF1", "SE29146","SG1", "SF18561","SP1"), 
                        startDate = "1993-01-01", 
                        endDate = "2024-09-01") %>% 
          as.data.frame() %>% 
          select(SG1.date, SF1.value, 
                 SE29146.value,SG1.value, SF18561.value, SP1.value ) %>% 
          rename(
                  "fecha" =SG1.date, 
                  "Oferta_monetaria" = SF1.value, 
                  "Exportaciones" = SE29146.value, 
                  "Gasto_presupuestal" = SG1.value,
                  "Tipo_cambio" = 	SF18561.value, 
                  "INPC" = SP1.value
                  ) %>% 
          mutate(
            
                  fecha = ymd(fecha), 
                  Oferta_monetaria = (Oferta_monetaria * 1000)* (136.080/INPC), 
                  Exportaciones = ((Exportaciones * Tipo_cambio)*1000)* (136.080/INPC), 
                  Gasto_presupuestal = (Gasto_presupuestal *1000000)* (136.080/INPC) 
        
          )


            
# SF1 = OFERTA MONETARIA MILES DE PESOS

# SE29146 = EXPORTACION TOTAL(INCLUYE MAQUILA) TOTAL,  MILES DE DÓLARES

# SG1 = Gastos Presupuestales del Sector Público Clasificación Económica Gasto presupuestario ( millones de pesos)

#	SF18561 = tipo de cambio peso dolar, fecha de publicacion del DOF


#------------------------------- ANALISIS GRAFICO -----------------------------#

graficar_vcontinuas <- function(datos, variable_x, variable_y){
  
                 ggplot(datos, mapping = aes({{variable_x}}, {{variable_y}})) +
                            geom_point(color = "red", size=2) +
                            geom_smooth(method = "lm", color ="black")+
                            geom_line(color="blue")+
                            labs(
                              title = paste(" Relacion entre", as_label(enquo(variable_x)), "y", as_label(enquo(variable_y)),sep = " ") ,
                              x = as_label(enquo(variable_x)),
                              y = as_label(enquo(variable_y)) )+
                            theme_bw() +
                            theme(
                              axis.text = element_text(face = "bold" ) ) +
                                scale_x_continuous(
                                  breaks = function(x) seq(0, max(x), length.out=5),
                                  labels = function(x) round(x / 1e9, 0)) +
                                scale_y_continuous(  
                                  breaks = function(y) seq(0, max(y), length.out=5), 
                                  labels = function(y) round( y/1e9,0) ) 
                                
                              
}


oferta_exportaciones <- graficar_vcontinuas(base,Exportaciones,Oferta_monetaria)
      
      
oferta_gasto <-  graficar_vcontinuas(base,Gasto_presupuestal, Oferta_monetaria)


gasto_exportaciones <- graficar_vcontinuas(base, Exportaciones, Gasto_presupuestal)


oferta_exportaciones + oferta_gasto + gasto_exportaciones



graficar_densidades <- function(datos, variable_x, color, fill){
  
        ggplot(datos, mapping = aes({{variable_x}}))+
          geom_density(color = color, fill= fill) +
        theme_bw()+
        theme(
          axis.text = element_blank()
          
        )
            
  
}



densidad_oferta <- graficar_densidades(base,Oferta_monetaria, "blue","#BFE8B2")

densidad_exportaciones <-  graficar_densidades(base,Exportaciones,"red","#B3DFD6")

densidad_gasto <-  graficar_densidades(base,sqrt(Gasto_presupuestal),"black","#FFE5C9")


densidad_oferta + densidad_exportaciones + densidad_gasto


densidad_gasto_normal <-  graficar_densidades(base,Oferta_monetaria,"black","#FFE5C9")


densidad_gasto_log <-  graficar_densidades(base,log(Oferta_monetaria),"black","#FFE5C9")


densidad_box<-  graficar_densidades(base, BoxCox(Oferta_monetaria, lambda = BoxCox.lambda(Oferta_monetaria)) ,"black","#FFE5C9")


densidad_raiz<-  graficar_densidades(base,sqrt(Oferta_monetaria) ,"black","#FFE5C9")


densidad_gasto_normal + densidad_gasto_log + densidad_raiz + densidad_box


boxcox(base$Oferta_monetaria, lambda = seq(-2,2,by=0.1) )



lambda <-   BoxCox.lambda(base$Oferta_monetaria)


ver <-   BoxCox(base$Oferta_monetaria, lambda = lambda)




# ---------------------------- MODEL ------------------------------------------#            


modelo1 <-     base %$% 
                  lm( log(Oferta_monetaria) ~ 
                        log(Exportaciones)  + log(Gasto_presupuestal))


summary(modelo1)


augment(modelo1)


# ------------------------ SUPUESTOS ------------------------------------------#

# NORMALIDAD GRAFICAMENTE 


modelo1 %$% hist(residuals) 

qqnorm(modelo1$residuals)  
qqline(modelo1$residuals, col= "red" )


# NORMALIDAD CON PRUEBAS: LOS RESIDUOS NO SIGUEN UNA DISTRIBUCION NORMAL

jarque.test(modelo1$residuals) # evalua la normalidad de los residuos a traves de evaluar su asimetria y su curtosis

shapiro.test(modelo1$residuals)

ad.test(modelo1$residuals)


#  MULTICOLINEALIDAD: NO HAY COLINEALIDAD 

vif(modelo1)


# HOMOCEDASTICIDAD: EL MODELO TIENE HETEROCEDASTICIDAD 


bptest(modelo1) # la variaza de los residuos no es constante(heterocedasticidad)

ncvTest(modelo1) # rechazamos homocedasticidad

#  AUTOCORRELACION



              augment(modelo1) %>%
                mutate(residuals_lag = lag(.resid, n=1L)) %>% 
                ggplot(mapping = aes(x=residuals_lag, y=.resid))+
                  geom_point(color="red")+
                  geom_smooth(method = "lm", color="blue") +
                  theme_bw()
            


dwtest(modelo1)  # el modelo tiene autocorrelacion 

bgtest(modelo1, order = 1)

bgtest(modelo1, order = 2)

bgtest(modelo1, order = 3)

bgtest(modelo1, order = 4)

bgtest(modelo1, order = 10)

bgtest(modelo1, order = 350)

# ESTABILIDAD EN LOS PARAMETROS

sctest(modelo1, type="OLS-MUSUM")  # LOS PARAMETROS NO SON ESTABLES


plot(
  
  efp(modelo1, data = base, type = "OLS-CUSUM")
  
)


# LINEALIDAD O ESPECIFICACION DEL MODLEO 

resettest(modelo1) # el modelo no esta bien especificado


# --------------------  MODELO CON PESOS----------------------------------------#




modelo2 <- base %$%
              lm(log(Oferta_monetaria) ~ 
                            log(Exportaciones) + log(Gasto_presupuestal),
                  weights = 1/log(Exportaciones))

# Normalidad: RECHAZAMOS NORMALIDAD

shapiro.test(modelo2$residuals)

jarque.test(modelo2$residuals)

ad.test(modelo2$residuals)

# Multicolinealidad: ACEPTAMOS NO COLINEALIDAD

vif(modelo2)

# HOOCEDASTICIDAD: RECHAZAMOS NO HOMOCEDASTICIDAD 

bptest(modelo2) 

ncvTest(modelo2)

# AUTOCORRELACION: RECHAZAMOS NO AUTOCORRELACION

dwtest(modelo2)

bgtest(modelo2) 

# ESTABILIDAD EN LOS PARAMETROS RECHAZAMOS ESTABILIDAD EN LOS PARAMETROS

sctest(modelo1, type="OLS-MUSUM") 

# LINEALIDAD O ESPECIFICIDAD 


resettest(modelo2)


#---------------------------- MODELO CON VARIANZA MODELADA ---------------------#

varianza_modelada <- lm(
                        modelo1$residuals^2 ~ log(base$Exportaciones +
                                              log(base$Gasto_presupuestal)))


varianza_predicha <- predict(varianza_modelada)

modelo3 <-  base %$% lm(
                      log(Oferta_monetaria) ~ log(Exportaciones) + log(Gasto_presupuestal),
                      weights = 1/(varianza_predicha^2) ) 


# Normalidad: RECHAZAMOS  NORMALIDAD 

shapiro.test(modelo3$residuals)

jarque.test(modelo3$residuals)

ad.test(modelo3$residuals)

#  Multicolinealidad: ACEPTAMOS NO MULTICOLINEALIDAD

vif(modelo3)

# Homocedasticidad: rechazamos homocedasticidad 

bptest(modelo3)

ncvTest(modelo3)

# Autocorrelacion: rechamzamos no autocorrelacion 

bgtest(modelo3)  

# Especificidad o linealidad: aceptamos estabilidad en los parametros 

sctest(modelo3, type="OLS-MUSUM") 

plot(
  
  efp(modelo3, data = base, type = "OLS-CUSUM")
  
)



#  LINEALIDAD O ESPECIFICIDAD: NO ACEPTAMOS LINEALIDAD 

resettest(modelo3)


# ----------------- CON TRANSFORMACIONES BOXCOX  ------------------------------#

        
modelo4 <- base %>% 
          mutate(
            oferta_box = BoxCox(Oferta_monetaria, lambda= BoxCox.lambda(Oferta_monetaria)),
            exportaciones_box = BoxCox(Exportaciones, lambda= BoxCox.lambda(Exportaciones)),
           ) %$%
          lm(oferta_box ~ exportaciones_box + log(Gasto_presupuestal))


summary(modelo4)

# NORMALIDAD 

shapiro.test(modelo4$residuals)

jarque.test(as.vector(modelo4$residuals))

# Colinealidad 

vif(modelo4)


# homocedasticidad

bptest(modelo4)

ncvTest(modelo4)

# autocorrelacion

dwtest(modelo4)

bgtest(modelo4)

# estabilidad de los parametros

sctest(modelo4, type="OLS-MUSUM")

# Especificidad linealidad

resettest(modelo4)

# -----------------  MODELO GLS -----------------------------------------------$


modelo_gls <-  gls(model = M1 ~ GP + TI + IPC, 
                   correlation = corAR1(),
                   method= "ML")

modelo5 <-  base %$%
                gls( log(Oferta_monetaria) ~ log(Exportaciones) + log(Gasto_presupuestal), 
                     correlation = corAR1(), 
                     method="ML")

summary(modelo5)


# -------------------------------


coeftest(modelo3, vcov=NeweyWest(modelo3))

summary(modelo3)

modelo_neweywst <- coeftest(modelo3, vcov=NeweyWest(modelo3))


summary(modelo_neweywst)

coefficients <- coef(model)
vcov_neweywest <- NeweyWest(model)

new_data <- data.frame(x = c(1, 2, 3))

# Matriz de diseño (X)
X <- model.matrix(~ x, data = new_data)

# Predicciones y sus errores estándar
pred_mean <- X %*% coefficients
pred_se <- sqrt(diag(X %*% vcov_neweywest %*% t(X)))

# Intervalos de confianza
conf_lower <- pred_mean - 1.96 * pred_se
conf_upper <- pred_mean + 1.96 * pred_se

# Resultado
pred_df <- data.frame(
  Predicted = pred_mean,
  Lower = conf_lower,
  Upper = conf_upper
)
print(pred_df)

summary(modelo3)




sctest(modelo3, type="OLS-MUSUM" )












set.seed(123)


setcontrol <- trainControl(
  method = "cv",
  number = 10,
  verboseIter = TRUE)


modelo2 <- train( log(Oferta_monetaria) ~ log(Exportaciones) + log(Gasto_presupuestal),
                    base, 
                    method="lm",
                    trControl = setcontrol, 
                    metric ="RMSE")


summary(modelo1)

summary(modelo2)



?train()


modelo2































#  QUE VARIABLE ES LA QUE CAUSA LA HETEROGENEIDAD


summary(lm(abs(modelo1$residuals) ~ log(base$Exportaciones)))

summary(lm(log(base$Gasto_presupuestal) ~ log(base$Exportaciones)))


gqtest(modelo1, order.by =  ~log(base$Exportaciones), data = base )

gqtest(modelo1, order.by =  ~log(base$Gasto_presupuestal), data = base )

varianza_estimada <-  abs(modelo1$residuals) ^ 2

pesos <-  1/varianza_estimada

modelo_ponderado <- base %$%
                        lm(log(Oferta_monetaria) ~ 
                             log(Exportaciones) + Tipo_cambio, 
                            weights = pesos)


summary(modelo_ponderado)

bptest(modelo_ponderado)


ncvTest(modelo_ponderado)

qqnorm(modelo_ponderado$residuals)
qqline(modelo_ponderado$residuals, col="red")


?write.csv

write.csv(base, file = "base.csv")


coeftest(modelo1, vcov = vcovHC(modelo1, type = "HC1"))



lambda <- 0.5  # Ajustar según la sensibilidad deseada

pesos <- exp(-lambda * abs(modelo1$residuals))



modelo_raiz_cuadrada  <- base %$%
                            lm(sqrt(Oferta_monetaria) ~ 
                                 log(Exportaciones) + log(Gasto_presupuestal), 
                               weights = pesos)




summary(modelo_raiz_cuadrada)


bptest(modelo_raiz_cuadrada)

ncvTest(modelo_raiz_cuadrada)

# ----------------------


residuos <- residuals(modelo1)


modelo_varianza <- lm(residuos^2 ~ log(base$Exportaciones) +
                                       Tipo_cambio, data=base)


varianza_estimada <- predict(modelo_varianza)


pesos <- 1/abs(varianza_estimada)^2


modelo_vmodelada <- base %$%
                        lm(log(Oferta_monetaria) ~ log(Exportaciones) +
                                                    Tipo_cambio,
                           weights = pesos)




bptest(modelo_vmodelada)


ncvTest(modelo_vmodelada)

qqnorm(modelo_vmodelada$residuals)
qqline(modelo_vmodelada$residuals, col="red")























