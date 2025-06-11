## Análise AC

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


## Estatísticas Descritivas
summary(df)
dfnum = df[, c("IdadeCuidador", "QuantasGestAnteriores", "DuraçãoGestaMeses",
               "IdadeBebéMeses", "PesoNasKg", "AlturaNasCm", 
               "PerimCefálicoNasCm", "TempoInternadoDias", "ÍndiceAPGAR", 
               "NúmTrat", "DiasTrat", "DifDiagonaisIniciaisMm", 
               "DifDiagonaisFinaisMm", "PontuaçãoCSQ8")]
summary(dfnum)

              
## Variância e desvio padrão
var(dfnum) 
var(IdadeCuidador)              ;   sd(IdadeCuidador)         
var(QuantasGestAnteriores)      ;   sd(QuantasGestAnteriores) 
var(DuraçãoGestaMeses)          ;   sd(DuraçãoGestaMeses)     
var(IdadeBebéMeses)             ;   sd(IdadeBebéMeses)  
var(PesoNasKg)                  ;   sd(PesoNasKg)  
var(AlturaNasCm)                ;   sd(AlturaNasCm)
var(PerimCefálicoNasCm)         ;   sd(PerimCefálicoNasCm)
var(TempoInternadoDias)         ;   sd(TempoInternadoDias)  
var(ÍndiceAPGAR)                ;   sd(ÍndiceAPGAR) 
var(NúmTrat)                    ;   sd(NúmTrat)               
var(DiasTrat)                   ;   sd(DiasTrat)       
var(DifDiagonaisIniciaisMm)     ;   sd(DifDiagonaisIniciaisMm)  
var(DifDiagonaisFinaisMm)       ;   sd(DifDiagonaisFinaisMm)
var(PontuaçãoCSQ8)              ;   sd(PontuaçãoCSQ8)


## Correlações
cor(dfnum[,c(1:13)])
   # Gráfico 1 
pairs(dfnum[,c(1:13)], main = "Matriz de Correlações")
   # Gráfico 2
corrplot(cor(dfnum[,c(1:13)], use="complete.obs"), method = "shade", type = "lower", 
         tl.col = "black")
   # Gráfico 3
corrgram(dfnum[,c(1:13)], lower.panel = panel.cor, upper.panel = panel.density,
         main= "Matriz de Correlações")


## Variáveis Numéricas
   # Idade Cuidador
boxplot(IdadeCuidador, main = "Idade do Cuidador", horizontal = TRUE, 
       col= "#BAE1FF")
hist(IdadeCuidador, main = "Idade do Cuidador", col= "#BAE1FF")

   # Número de Gestações anteriores
boxplot(QuantasGestAnteriores, main = "Número de Gestações Anteriores", 
        horizontal = TRUE, col= "#BAE1FF")
barplot(table(QuantasGestAnteriores), main = "Número de Gestações Anteriores", 
        col= "#BAE1FF", ylim = c(0,30))

   # Duração Gestação
boxplot(DuraçãoGestaMeses, main = "Duração da Gestação em meses", 
        horizontal = TRUE, col= "#BAE1FF")
hist(DuraçãoGestaMeses, main = "Duração da Gestação em meses", col= "#BAE1FF")

   # Idade do Bebé
boxplot(IdadeBebéMeses, main = "Idade do Bebé em Meses", horizontal = TRUE, 
        col= "#BAE1FF")
barplot(table(IdadeBebéMeses), main = "Idade do Bebé em Meses", 
        col= "#BAE1FF", ylim = c(0,20))

   # Peso no Nascimento
boxplot(PesoNasKg, main = "Peso no Nascimento em Quilos", 
        horizontal = TRUE, col= "#BAE1FF")

   # Altura no Nascimento
boxplot(AlturaNasCm, main = "Altura no Nascimento em Centímetros", 
        horizontal = TRUE, col= "#BAE1FF")
hist(AlturaNasCm, main = "Altura no Nascimento em Centímetros", col= "#BAE1FF")

   # Perímetro Cefálico
boxplot(PerimCefálicoNasCm, main = "Perímetro Cefálico em centímetros", 
        horizontal = TRUE, col= "#BAE1FF")
hist(PerimCefálicoNasCm, main = "Perímetro Cefálico em centímetros", 
     breaks = 10, col= "#BAE1FF")

   # Tempo de Internamento 
barplot(table(TempoInternadoDias), main = "Tempo de Internamento em Dias", 
     col= "#BAE1FF", ylim = c(0,50))

   # Índice APGAR
barplot(table(ÍndiceAPGAR), main = "Índice APGAR", col= "#BAE1FF", 
        ylim = c(0,50))

   # Número de Tratamentos
boxplot(NúmTrat, main = "Número de Tratamentos", horizontal = TRUE, 
        col= "#BAE1FF")
barplot(table(NúmTrat), main = "Número de Tratamentos", col= "#BAE1FF", 
        ylim = c(0,20))

   # Tempo de Tratamento em dias
boxplot(DiasTrat, main = "Tempo de Tratamento em dias", horizontal = TRUE, 
        col= "#BAE1FF")
barplot(table(DiasTrat), main = "Número de Tratamentos", col= "#BAE1FF", 
        ylim = c(0,12))

   # Diferenças Diagonais no Início
boxplot(DifDiagonaisIniciaisMm, main = "Diferenças Diagonais no Início", 
        horizontal = TRUE, col= "#BAE1FF")
hist(DifDiagonaisIniciaisMm, main = "Diferenças Diagonais no Início", 
    col= "#BAE1FF", ylim = c(0,20))
barplot(table(DifDiagonaisIniciaisMm), main = "Diferenças Diagonais no Início", 
        col= "#BAE1FF", ylim = c(0,12))

   # Diferenças Diagonais no Final
boxplot(DifDiagonaisFinaisMm, main = "Diferenças Diagonais no Final", 
        horizontal = TRUE, col= "#BAE1FF")
hist(DifDiagonaisFinaisMm, main = "Diferenças Diagonais no Final", 
    col= "#BAE1FF", ylim = c(0,20))
barplot(table(DifDiagonaisFinaisMm), main = "Diferenças Diagonais no Final", 
        col= "#BAE1FF", ylim = c(0,20))

   # Pontuação
boxplot(PontuaçãoCSQ8, main = "Pontuação CSQ-8", horizontal = TRUE, 
        col= "#BAE1FF")
hist(PontuaçãoCSQ8, main = "Pontuação CSQ-8", col= "#BAE1FF", ylim = c(0,20))


## Variáveis Categóricas
   # Clínica
table(Clínica)/ 42 * 100
barplot(table(Clínica), main = "Clínica", col = c("#FFD1DC","#D5FFBA"),
        ylim = c(0,40))

   # Sexo Cuidador
table(SexoCuidador) / 42 * 100
barplot(table(SexoCuidador), main = "Sexo do Cuidador",
        col = c("#FFD1DC","#D5FFBA"), ylim = c(0,50))

   # Escolaridade
table(Escolaridade) / 42 * 100
barplot(table(Escolaridade), main = "Escolaridade", 
        col = c("#BAE1FF","#FFD1DC","#D5FFBA"), ylim = c(0,50))

   # Gestações Anteriores
table(GestAnteriores) / 42 * 100
barplot(table(GestAnteriores), main = "Gestações Anteriores", 
        col = c("#FFD1DC","#D5FFBA"), ylim = c(0,40))

   # Gestação Gemelar
table(GestGemelar) / 42 * 100
barplot(table(GestGemelar), main = "Gestação Gemelar", 
        col = c("#FFD1DC","#D5FFBA"), ylim = c(0,50))

    # Complicações Gestação
table(ComplicaçõesGest) / 42 * 100
barplot(table(ComplicaçõesGest), main = "Complicações na Gestação", 
        col = c("#FFD1DC","#D5FFBA"), ylim = c(0,50))

   # Tipo Parto
table(TipoParto) / 42 * 100
barplot(table(TipoParto), main = "Tipo de Parto", 
        col = c("#BAE1FF","#FFD1DC","#D5FFBA","#FFFFBA"), ylim = c(0,30))

   # Complicações Parto
table(ComplicaçõesParto) / 42 * 100
barplot(table(ComplicaçõesParto), main = "Complicações no Parto", 
        col = c("#FFD1DC","#D5FFBA"), ylim = c(0,55))

   # Sexo Bebé
table(SexoBebé) / 42 * 100
barplot(table(SexoBebé), main = "Sexo do Bebé", 
        col = c("#FFD1DC","#D5FFBA"), ylim = c(0,55))

   # Internamento Neonatologia
table(InternNeonatologia) / 42 * 100
barplot(table(InternNeonatologia), main = "Internamento Neonatologia", 
        col = c("#FFD1DC","#D5FFBA"), ylim = c(0,50))

   # Luxação Congénita
table(LuxaçãoCongénita) / 42 * 100
barplot(table(LuxaçãoCongénita), main = "Luxação Congénita", 
        col = c("#FFD1DC","#D5FFBA"), ylim = c(0,40))

   # Babywearing
table(Babywearing) / 42 * 100
barplot(table(Babywearing), main = "Babywearing", col = c("#FFD1DC","#D5FFBA"), 
        ylim = c(0,50))

    # Tummy Time
table(TummyTime) / 42 * 100
barplot(table(TummyTime), main = "Tummy Time", col = c("#FFD1DC","#D5FFBA"), 
         ylim = c(0,55))

   # Outra Terapia
table(OutraTerapia) / 42 * 100
barplot(table(OutraTerapia), main = "Outra Terapia", 
        col = c("#FFD1DC","#D5FFBA"),ylim = c(0,60))

   # Encaminhamento Osteopatia Pediátrica
table(Encaminhamento) / 42 * 100
barplot(table(Encaminhamento), 
        main = "Encaminhamento Osteopatia Pediátrica", 
        col = c("#BAE1FF","#FFD1DC","#D5FFBA"), ylim = c(0,50)) 

   # Outras Patologias
table(OutrasPatologias) / 42 * 100
barplot(table(OutrasPatologias), main = "Outras Patologias", 
        col = c("#FFD1DC","#D5FFBA"), ylim = c(0,60))

   # Lateralidade Plagiocefalia
table(LateralPlagiocefalia) / 42 * 100
barplot(table(LateralPlagiocefalia), main = "Lateralidade Plagiocefalia", 
        col = c("#FFD1DC","#D5FFBA"), ylim = c(0,45))

   # Tratamento Pretendido 
table(TratPretendido) / 42 * 100
barplot(table(TratPretendido), main = "Tratamento Pretendido", 
        col = c("#BAE1FF","#FFD1DC","#D5FFBA"), ylim = c(0,45))

    # Termino Tratamento
table(TerminoTrat) / 42 * 100
barplot(table(TerminoTrat), main = "Conclusão do Tratamento", 
        col = c("#FFD1DC","#D5FFBA"), ylim = c(0,50))

   # Classificação Assimetria Inicial
table(ClasAssimetriaInicial) / 42 * 100
barplot(table(ClasAssimetriaInicial), main = "Classificação Assimetria Inicial", 
        col = c("#BAE1FF","#FFD1DC","#D5FFBA","#FFFFBA"), ylim = c(0,40))

   # Classificação Assimetria Final
table(ClasAssimetriaFinal) / 42 * 100
barplot(table(ClasAssimetriaFinal), main = "Classificação Assimetria Final", 
        col=c("#BAE1FF","#FFD1DC","#D5FFBA","#FFFFBA"), ylim = c(0,50))

   # Contribuição Desenvolvimento Motor
table(ContribuiçãoDesenvolMotor) / 42 * 100
barplot(table(ContribuiçãoDesenvolMotor), 
        main = "Contribuição Desenvolvimento Motor", 
        col = c("#FFD1DC","#D5FFBA"),  ylim = c(0,55))

    # Contribuição Melhoria Sono
table(ContribuiçãoMelhoriaSono) / 42 * 100
barplot(table(ContribuiçãoMelhoriaSono), 
        main = "Contribuição na Melhoria do sono", col = c("#FFD1DC","#D5FFBA"), 
        ylim = c(0,30))

   # Resposta Necessidade
table(RespNecessidades) / 42 * 100
barplot(table(RespNecessidades), main = "Resposta Necessidades", 
        col = c("#BAE1FF","#FFD1DC","#D5FFBA"), ylim = c(0,50))

   # Quantidade_Ajuda
table(Ajuda) / 42 * 100
barplot(table(Ajuda), main = "Ajuda", 
        col = c("#BAE1FF","#FFD1DC","#D5FFBA"), ylim = c(0,50))  

   # Lidar Problemas
table(LidarProblemas) / 42 * 100
barplot(table(LidarProblemas), main = "Ajudou a Lidar com Problemas", 
        col = c("#FFD1DC","#D5FFBA"), ylim = c(0,40))

   # Voltaria
table(Voltaria) / 42 * 100
barplot(table(Voltaria), main = "Voltaria", 
        col = c("#BAE1FF","#FFD1DC","#D5FFBA","#FFFFBA"), ylim = c(0,50))

   # Recomendação
table(Recomendação) / 42 * 100
barplot(table(Recomendação), main = "Recomendação", 
        col = c("#BAE1FF","#FFD1DC","#D5FFBA"), ylim = c(0,50))

   # Satisfação Geral Tratamento
table(SatisfaçãoGeralTrat) / 42 * 100
barplot(table(SatisfaçãoGeralTrat), main = "Satisfação Geral Tratamento", 
        col = c("#BAE1FF","#FFD1DC","#D5FFBA","#FFFFBA"), ylim = c(0,50))

   # Qualificação Tratamento
table(QualificaçãoTrat) / 42 * 100
barplot(table(QualificaçãoTrat), main = "Qualificação Tratamento", 
        col = c("#BAE1FF","#FFD1DC","#D5FFBA"), ylim = c(0,50))


## Relações
    # Correlação
ggpairs(df[, c("PesoNasKg", "AlturaNasCm")])
ggpairs(df[, c("DifDiagonaisIniciaisMm", "DifDiagonaisFinaisMm")])
ggpairs(df[, c("DifDiagonaisIniciaisMm", "DifDiagonaisFinaisMm", 
               "IdadeBebéMeses")])
ggpairs(df[, c("DifDiagonaisIniciaisMm", "DifDiagonaisFinaisMm", "NúmTrat")])
ggpairs(df[, c("DifDiagonaisIniciaisMm", "DifDiagonaisFinaisMm", 
               "PontuaçãoCSQ8")])
ggpairs(df[, c("DifDiagonaisIniciaisMm", "DifDiagonaisFinaisMm", "DiasTrat")])
ggpairs(df[, c("ClasAssimetriaInicial", "ClasAssimetriaFinal")])
ggpairs(df[, c("ClasAssimetriaInicial", "ClasAssimetriaFinal", 
               "QualificaçãoTrat")])

   # Relação :
boxplot(df[,c(31,33)], main = "Diferenças Diagonais", horizontal = TRUE, 
        col= c("#FFD1DC","#D5FFBA"))
par(mfrow = c(1, 2))
hist(DifDiagonaisIniciaisMm, main = "Diferenças Diagonais no Início", 
     horizontal = TRUE, col= "#FFD1DC")
hist(DifDiagonaisFinaisMm, main = "Diferenças Diagonais no Final", 
     horizontal = TRUE, col= "#D5FFBA")
boxplot(df[,c(31,29,33)], main = "Diferenças Diagonais", horizontal = TRUE, 
        col= c("#FFD1DC","#D5FFBA"))

   # Relação  : barplot das classificações
tipo = factor(c(rep("Inicial", 42), rep("Final", 42)), 
              levels = c("Inicial", "Final"))
resultado = c(ClasAssimetriaInicial, ClasAssimetriaFinal)
tabela = table(tipo, resultado)
barplot(tabela, beside = TRUE, col = c("#FFD1DC", "#D5FFBA"),
        legend.text = TRUE, args.legend = list(title = "Tipo", x = "topright"),
        xlab = "Classificação da Assimetria", ylab = "Número de bebés",
        main = "Distribuição das Classificações por Grau")


## Normalidade
   # testar se os dados seguem a normalidade
   # H0 : seguem distribuição normal vs H1 : não seguem distribuição normal
   # p-valor :
   # < 0.05	→ Rejeita se H0     → H1
   # ≥ 0.05	→ Não se rejeita H0 → H0
shapiro.test(IdadeCuidador) 
shapiro.test(QuantasGestAnteriores)
shapiro.test(DuraçãoGestaMeses)
shapiro.test(IdadeBebéMeses)
shapiro.test(PesoNasKg) 
shapiro.test(AlturaNasCm) 
shapiro.test(PerimCefálicoNasCm)
shapiro.test(TempoInternadoDias)
shapiro.test(ÍndiceAPGAR)
shapiro.test(NúmTrat)
shapiro.test(DiasTrat)
shapiro.test(DifDiagonaisIniciaisMm)
shapiro.test(DifDiagonaisFinaisMm)
shapiro.test(PontuaçãoCSQ8)


## Gráficos - seguem normalidade
   # Idade_anos
hist(IdadeCuidador, main = "Idade do Cuidador", probability = TRUE, 
     col= "#BAE1FF", ylim = c(0, 0.13), xlab = "Idade em Anos", 
     ylab = "Probabilidade")
x1 = seq(min(IdadeCuidador), max(IdadeCuidador), length = 50) 
y1 = dnorm(x1, mean(IdadeCuidador), sd(IdadeCuidador)) 
lines(x1, y1, col = "black", lwd = 2)
   # Peso_Nascimento_Kg
hist(PesoNasKg, main = "Peso no Nascimento", probability = TRUE,
     col= "#BAE1FF", ylim = c(0, 0.9), xlab = "Peso em Quilos", 
     ylab = "Probabilidade")
x2 = seq(min(PesoNasKg), max(PesoNasKg), length = 50)  
y2 = dnorm(x2, mean(PesoNasKg), sd(PesoNasKg))  
lines(x2, y2, col = "black", lwd = 2) 
   # Altura_Nascimento_Cm
hist(AlturaNasCm, main = "Altura no Nascimento", 
     probability = TRUE, col= "#BAE1FF", ylim = c(0, 0.2),
     xlab = "Altura em Centímetros", ylab = "Probabilidade")
x3 = seq(min(AlturaNasCm), max(AlturaNasCm), length = 50)
y3 = dnorm(x3, mean(AlturaNasCm), sd(AlturaNasCm))  
lines(x3, y3, col = "black", lwd = 2)


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


## Modelos
   # Modelo 1 : Idade Bebé ~ Peso Nascimento + Altura Nascimento
mod1 = lm(IdadeBebéMeses ~ PesoNasKg + AlturaNasCm, data = df)
summary(mod1)

   # Modelo 1a : Idade Bebé ~ Peso Nascimento
mod1a = lm(IdadeBebéMeses ~ PesoNasKg, data = df)
summary(mod1a)

   # Modelo 2 : Complicações Parto ~ Idade Cuidados + Duração Gestação
mod2 = glm(ComplicaçõesParto ~ IdadeCuidador + DuraçãoGestaMeses, 
           data = df, family = "binomial")
summary(mod2)

   # Modelo 2a : Complicações_Parto ~ Duração_Gestação_Meses
mod2a = glm(ComplicaçõesParto ~ DuraçãoGestaMeses, data = df,
            family = "binomial")
summary(mod2a)

   # Modelo 3 : Diferença Diagonais Finais ~ Diferença Diagonais Iniciais + 
   #           Dias Tratamento + Número Tratamentos
mod3 = lm(DifDiagonaisFinaisMm ~ DifDiagonaisIniciaisMm + DiasTrat + NúmTrat, 
          data = df)
summary(mod3)

   # Modelo 3a : Diferença Diagonais Finais ~ Diferença Diagonais Iniciais +
   #            Dias Tratamento
mod3a = lm(DifDiagonaisFinaisMm ~ DifDiagonaisIniciaisMm + DiasTrat, data = df)
summary(mod3a)

   # Modelo 4 : Dias Internamento ~ Diferença Diagonais Finais + 
#           Dias Tratamento
mod4 = lm(TempoInternadoDias ~ DifDiagonaisFinaisMm + DiasTrat, data = df)
summary(mod4)


   # Modelo 4a : Dias Internamento ~ Dias Tratamento
mod4a = lm(TempoInternadoDias ~ DiasTrat, data = df)
summary(mod4a)

   # Modelo 5 : Diferença Diagonais Finais ~ Tummy Time + Babywearing
mod5 = lm(DifDiagonaisFinaisMm ~ TummyTime + Babywearing, data = df)
summary(mod5)

   # Modelo 5a : Diferença Diagonais Finais ~ Tummy Time 
mod5a = lm(DifDiagonaisFinaisMm ~ TummyTime, data = df)
summary(mod5a)

   # Modelo 6 : Diferença Diagonais Finais ~ Idade Bebé + Sexo Bebé + 
   #         Outras Terapias
mod6 = lm(DifDiagonaisFinaisMm ~ IdadeBebéMeses + SexoBebé + OutraTerapia, 
          data = df)
summary(mod6)

   # Modelo 6a : Diferença Diagonais Finais ~ Idade Bebé + Outras Terapias 
mod6a = lm(DifDiagonaisFinaisMm ~ IdadeBebéMeses + OutraTerapia, data = df)
summary(mod6a)

   # Modelo 6b : Diferença Diagonais Finais ~ Idade Bebé 
mod6b = lm(DifDiagonaisFinaisMm ~ IdadeBebéMeses, data = df)
summary(mod6b)

   # Modelo 7 : Diferença Diagonais Iniciais ~ Gestação Gemelar + 
   #           Perimetro Cefálico
mod7 = lm(DifDiagonaisIniciaisMm ~ GestGemelar + PerimCefálicoNasCm, data = df)
summary(mod7)

   # Modelo 8 : Dias Tratamento ~ Outras Patologias + Outro Tratamento + 
   #          Classificação Inicial
mod8 = lm(DiasTrat ~ OutrasPatologias + OutraTerapia + ClasAssimetriaInicial, 
          data=df)
summary(mod8)

   # Modelo 8a : Dias Tratamento ~ Outras Patologias +  Classificação Inicial
mod8a = lm(DiasTrat ~ OutrasPatologias + ClasAssimetriaInicial, data=df)
summary(mod8a)

   # Modelo 8b : Dias Tratamento ~   Classificação Inicial
mod8b = lm(DiasTrat ~ ClasAssimetriaInicial, data=df)
summary(mod8b)

   # Modelo 9: Número Tratamentos ~ Outras Patologias + Outro Tratamento + 
   #          Classificação Inicial
mod9 = lm(NúmTrat ~ OutrasPatologias + OutraTerapia + ClasAssimetriaInicial, 
          data = df)
summary(mod9)

   # Modelo 9a: Número Tratamentos ~ Outro Tratamento + Classificação Inicial
mod9a = lm(NúmTrat ~ OutraTerapia + ClasAssimetriaInicial, data = df)
summary(mod9a)

   # Modelo 9b : Número Tratamentos ~ Classificação Inicial
mod9b = lm(NúmTrat ~ ClasAssimetriaInicial, data = df)
summary(mod9b)

   # Modelo 10 : Classificação Final ~ Difernça Final
mod10 = multinom(ClasAssimetriaFinal ~ DifDiagonaisFinaisMm + DiasTrat + 
                   NúmTrat, data = df)
summary(mod10)

   # Modelo 11 : Satisfação ~ Classificação Final + Diferença Final
mod11 = multinom(SatisfaçãoGeralTrat ~ ClasAssimetriaFinal + 
                   DifDiagonaisFinaisMm)
summary(mod11)

   # Modelo 12 : Recomendação ~ Satisfação + Classificação Final + 
   #          Diferença Final
mod12 = multinom(Recomendação ~ SatisfaçãoGeralTrat + ClasAssimetriaFinal + 
                   DifDiagonaisFinaisMm)
summary(mod12)

   # Modelo 13 : Classificação Final ~ TummyTime + Babywearing 
mod13 = multinom(ClasAssimetriaFinal ~ TummyTime + Babywearing)
summary(mod13)

   # Modelo 14
mod14 = multinom(ClasAssimetriaFinal ~ TummyTime + Recomendação + 
                   SatisfaçãoGeralTrat)
summary(mod14)

   # Modelo 15
mod15 = multinom(SatisfaçãoGeralTrat ~ QualificaçãoTrat+ Voltaria + 
                   RespNecessidades)
summary(mod15)


