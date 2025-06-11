## Visualização Gráfica - Individual

## Packages 
library(readxl)
library(forcats)


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




