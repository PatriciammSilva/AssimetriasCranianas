## Independência

## Packages 
library(readxl)
library(forcats)
library(corrplot)
library(corrgram)
library(GGally)
library(nnet)

## Ler Dataset 
df = read_excel("Desktop/Assimetrias Cranianas/Dataset.xlsx")
str(df)


## Alterações Variáveis
varfac = c("Clínica", "SexoCuidador", "Escolaridade", "GestAnteriores", 
           "GestGemelar", "ComplicaçõesGest", "TipoParto", "ComplicaçõesParto",
           "SexoBebé", "InternNeonatologia", "LuxaçãoCongénita", "Babywearing",
           "TummyTime", "OutraTerapia", "Encaminhamento", "OutrasPatologias", 
           "LateralPlagiocefalia", "TratPretendido", "TerminoTrat", 
           "ClasAssimetriaInicial", "ClasAssimetriaFinal", 
           "ContribuiçãoDesenvolMotor", "ContribuiçãoMelhoriaSono", 
           "RespNecessidades", "Ajuda", "LidarProblemas", "Voltaria", 
           "Recomendação", "SatisfaçãoGeralTrat", "QualificaçãoTrat")
df[varfac] = lapply(df[varfac], as.factor)  
str(df)


## Acrescentar Níveis
ClasAssimetriaInicial = fct_expand(ClasAssimetriaInicial,"Sem Assimetria")
ClasAssimetriaInicial = factor(ClasAssimetriaInicial, 
         levels = c("Grave", "Moderada", "Leve", "Sem Assimetria"))
ClasAssimetriaFinal = fct_expand(ClasAssimetriaFinal, "Grave")
ClasAssimetriaFinal = factor(ClasAssimetriaFinal, 
         levels = c("Grave", "Moderada", "Leve", "Sem Assimetria"))
SatisfaçãoGeralTrat = fct_expand(SatisfaçãoGeralTrat, "Muito Insatisfeito", 
         after=0)


## Guardar Variáveis
attach(df)


## Testes de independência
   # TESTE 1 :
   # Teste Qui-quadrado 
   # Testar se duas variáveis categóricas são independentes
   # H0 : variáveis são independentes vs H1 : variáveis não são independentes
   # p-valor	Interpretação
   # < 0.05	→ Rejeita se H0   → H1
   # ≥ 0.05	→ Não rejeita H0  → H0
   # TESTE 2 : 
   # Teste Fisher
   # Verificar se existe associação entre duas variáveis categóricas
   # H0 : variáveis são independentes vs H1 : variáveis não são independentes
   # p-valor	Interpretação
   # < 0.05	→ Rejeita se H0   → H1
   # ≥ 0.05	→ Não rejeita H0  → H0

   # Classificação Assimetria Inicial 
t01 = table(ClasAssimetriaInicial, GestAnteriores)        
chisq.test(t01)           ; fisher.test(t01)
t02 = table(ClasAssimetriaInicial, GestGemelar)          
chisq.test(t02)           ; fisher.test(t02)
t03 = table(ClasAssimetriaInicial, ComplicaçõesGest)     
chisq.test(t03)           ; fisher.test(t03)
t04 = table(ClasAssimetriaInicial, ComplicaçõesParto)     
chisq.test(t04)           ; fisher.test(t04)
t05 = table(ClasAssimetriaInicial, SexoBebé)             
chisq.test(t05)           ; fisher.test(t05)
t06 = table(ClasAssimetriaInicial, InternNeonatologia)   
chisq.test(t06)           ; fisher.test(t06)
t07 = table(ClasAssimetriaInicial, LuxaçãoCongénita)     
chisq.test(t07)           ; fisher.test(t07)
t08 = table(ClasAssimetriaInicial, OutrasPatologias)     
chisq.test(t08)           ; fisher.test(t08)
t09 = table(ClasAssimetriaInicial, LateralPlagiocefalia) 
chisq.test(t09)           ; fisher.test(t09)
t10 = table(ClasAssimetriaInicial, ClasAssimetriaFinal)  
chisq.test(t10)           ; fisher.test(t10)

   # Classificação Assimetria Final 
t11 = table(ClasAssimetriaFinal, Babywearing)        
chisq.test(t11)           ; fisher.test(t11)
t12 = table(ClasAssimetriaFinal, TummyTime)          
chisq.test(t12)           ; fisher.test(t12)
t13 = table(ClasAssimetriaFinal, OutraTerapia)     
chisq.test(t13)           ; fisher.test(t13)
t14 = table(ClasAssimetriaFinal, OutrasPatologias)     
chisq.test(t14)           ; fisher.test(t14)
t15 = table(ClasAssimetriaFinal, ClasAssimetriaInicial)             
chisq.test(t15)           ; fisher.test(t15)
t16 = table(ClasAssimetriaFinal, ContribuiçãoDesenvolMotor)   
chisq.test(t16)           ; fisher.test(t16)
t17 = table(ClasAssimetriaFinal, ContribuiçãoMelhoriaSono)     
chisq.test(t17)           ; fisher.test(t17)
t18 = table(ClasAssimetriaFinal, RespNecessidades)     
chisq.test(t18)           ; fisher.test(t18)
t19 = table(ClasAssimetriaFinal, Ajuda) 
chisq.test(t19)           ; fisher.test(t19)
t20 = table(ClasAssimetriaFinal, LidarProblemas)  
chisq.test(t20)           ; fisher.test(t20)
t21 = table(ClasAssimetriaFinal, Voltaria)  
chisq.test(t21)           ; fisher.test(t21)
t22 = table(ClasAssimetriaFinal, Recomendação)  
chisq.test(t22)           ; fisher.test(t22)
t23 = table(ClasAssimetriaFinal, SatisfaçãoGeralTrat)  
chisq.test(t23)           ; fisher.test(t23)
t24 = table(ClasAssimetriaFinal, QualificaçãoTrat)  
chisq.test(t24)           ; fisher.test(t24)


   # Satisfação 
t25 = table(SatisfaçãoGeralTrat, ClasAssimetriaFinal)             
chisq.test(t25)           ; fisher.test(t25)
t26 = table(SatisfaçãoGeralTrat, ContribuiçãoDesenvolMotor)   
chisq.test(t26)           ; fisher.test(t26)
t27 = table(SatisfaçãoGeralTrat, ContribuiçãoMelhoriaSono)     
chisq.test(t27)           ; fisher.test(t27)
t28 = table(SatisfaçãoGeralTrat, RespNecessidades)     
chisq.test(t28)           ; fisher.test(t28)
t29 = table(SatisfaçãoGeralTrat, Ajuda) 
chisq.test(t29)           ; fisher.test(t29)
t30 = table(SatisfaçãoGeralTrat, LidarProblemas)  
chisq.test(t30)           ; fisher.test(t30)
t31 = table(SatisfaçãoGeralTrat, Voltaria)  
chisq.test(t31)           ; fisher.test(t31)
t32 = table(ClasAssimetriaFinal, Recomendação)  
chisq.test(t32)           ; fisher.test(t32)
t33 = table(ClasAssimetriaFinal, QualificaçãoTrat)  
chisq.test(t33)           ; fisher.test(t33)

# Qualificação 
t34 = table(QualificaçãoTrat, ClasAssimetriaFinal)             
chisq.test(t34)           ; fisher.test(t34)
t35 = table(QualificaçãoTrat, ContribuiçãoDesenvolMotor)   
chisq.test(t35)           ; fisher.test(t35)
t36 = table(QualificaçãoTrat, ContribuiçãoMelhoriaSono)     
chisq.test(t36)           ; fisher.test(t36)
t37 = table(QualificaçãoTrat, RespNecessidades)     
chisq.test(t37)           ; fisher.test(t37)
t38 = table(QualificaçãoTrat, Ajuda) 
chisq.test(t38)           ; fisher.test(t38)
t39 = table(QualificaçãoTrat, LidarProblemas)  
chisq.test(t39)           ; fisher.test(t39)
t40 = table(QualificaçãoTrat, Voltaria)  
chisq.test(t40)           ; fisher.test(t40)
t41 = table(QualificaçãoTrat, Recomendação)  
chisq.test(t41)           ; fisher.test(t41)
t42 = table(QualificaçãoTrat, SatisfaçãoGeralTrat)  
chisq.test(t42)           ; fisher.test(t42)

