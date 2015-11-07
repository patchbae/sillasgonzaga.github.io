---
layout: post
title: Correlação entre o dólar e ações de empresas do Índice Bovespa
---


Introdução
----------

Este projeto surgiu de uma situação que eu estou lidando no momento: digamos que você deseja investir dinheiro para fazer uma viagem internacional durante suas férias daqui a 5 anos. Como os gastos da viagem serão indexados pelo dólar, não basta apenas escolher um bom investimento como títulos do tesouro ou outras modalidades de renda fixa. Por mais que os rendimentos deles possam apresentar enormes vantagens para seus investidores, eles precisam ser maiores que a valorização do dólar, caso contrário você correrá o risco de chegar no fim desse período de 5 anos sem o dinheiro necessário para realizar seu sonho. Se já é muito difícil prever quanto custará o dólar na próxima semana, prever seu valor em 5 anos é impossível.

Independentemente se você deseja realizar uma viagem internacional ou não, muitas pessoas certamente já desejaram saber como o desempenho do dólar afeta o rendimento de ações na bolsa. É disto que o post inaugural deste blog se trata: talvez seja uma boa decisão investir em ações de empresas que acompanhem o comportamento do dólar, isto é, que aumentem quando o dólar aumenta. Em estatística, isso é chamado de correlação:

> Diz-se que existe correlação entre duas ou mais variáveis quando as alterações sofridas por uma delas são acompanhadas por modificações nas outras. Ou seja, no caso de duas variáveis x e y os aumentos (ou diminuições) em x correspondem a aumentos (ou diminuições) em y. Assim, a correlação revela se existe uma relação funcional entre uma variável e as restantes.

Programação e resultados

Todo o processo de análise foi feito no R, usando dados de cotações obtidos no Yahoo Finance. Segue abaixo o código

``` r
#1 Baixar série historica do dolar de 2 anos
library(quantmod)
```

    ## Loading required package: xts
    ## Loading required package: zoo
    ## 
    ## Attaching package: 'zoo'
    ## 
    ## The following objects are masked from 'package:base':
    ## 
    ##     as.Date, as.Date.numeric
    ## 
    ## Loading required package: TTR
    ## Version 0.4-0 included new data defaults. See ?getSymbols.

``` r
library(plyr)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'
    ## 
    ## The following objects are masked from 'package:plyr':
    ## 
    ##     arrange, count, desc, failwith, id, mutate, rename, summarise,
    ##     summarize
    ## 
    ## The following objects are masked from 'package:xts':
    ## 
    ##     first, last
    ## 
    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag
    ## 
    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(corrplot)
library(ggplot2)
library(reshape2)
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'
    ## 
    ## The following object is masked from 'package:plyr':
    ## 
    ##     here

Após carregar as bibliotecas que serão usadas no código, é usado o pacote quantmod para baixar e plotar uma série histórica do dólar. Por tentativa e erro, defini que o período de análise seria entre 25 de agosto de 2013 até 01 de agosto de 2015.

``` r
datainicial = "2013-08-25"
datafinal = "2015-08-01"
cotacoes = getFX("USD/BRL", from=datainicial, to=datafinal, env=NULL)
lineChart(cotacoes)
```

![](postBovespaBlog_files/figure-markdown_github/unnamed-chunk-2-1.png)

Um pouco de transformação de dados (o famoso Data Wrangling) necessária para transformar a variável cotacoes de classe xts em dataframes:

``` r
cotacoes = as.data.frame(cotacoes)
cotacoes$Data = rownames(cotacoes)
rownames(cotacoes) = NULL
```

Agora já podemos baixar as cotações das empresas. Já que existem centenas de empresas listadas , eu escolhi apenas as que compõem o Índice Bovespa. Para isso, eu salvei a lista das empresas em um arquivo csv e a carreguei no R.

``` r
ibovespa <- read.csv2("https://raw.githubusercontent.com/sillasgonzaga/sillasgonzaga.github.io/master/data/ibovespa.csv")
suffix = as.data.frame(replicate(nrow(ibovespa), ".SA")) 

#Um pouco de Data Wrangling para concatenar os códigos das empresas com o sufixo.

temp = cbind(ibovespa, suffix)
ibovespa = apply(temp, 1,
               function(x) paste0(toString(x[1]), toString(x[2]))
               )
```

Depois de carregar os códigos das empresas e adicionar o sufixo .SA (sufixo que identifica as ações da Bovespa), hora de baixar as séries históricas de cotações de cada empresa.

``` r
#Baixar cotacões
getSymbols(ibovespa[1:36], auto.assign = TRUE, from=datainicial, to=datafinal) #O 37(IBOV.SA) dá problema
```

    ##     As of 0.4-0, 'getSymbols' uses env=parent.frame() and
    ##  auto.assign=TRUE by default.
    ## 
    ##  This  behavior  will be  phased out in 0.5-0  when the call  will
    ##  default to use auto.assign=FALSE. getOption("getSymbols.env") and 
    ##  getOptions("getSymbols.auto.assign") are now checked for alternate defaults
    ## 
    ##  This message is shown once per session and may be disabled by setting 
    ##  options("getSymbols.warning4.0"=FALSE). See ?getSymbols for more details.

    ## Warning in download.file(paste(yahoo.URL, "s=", Symbols.name, "&a=",
    ## from.m, : downloaded length 27947 != reported length 200

    ## Warning in download.file(paste(yahoo.URL, "s=", Symbols.name, "&a=",
    ## from.m, : downloaded length 25950 != reported length 200

    ## Warning in download.file(paste(yahoo.URL, "s=", Symbols.name, "&a=",
    ## from.m, : downloaded length 29038 != reported length 200

    ## Warning in download.file(paste(yahoo.URL, "s=", Symbols.name, "&a=",
    ## from.m, : downloaded length 24632 != reported length 200

    ## Warning in download.file(paste(yahoo.URL, "s=", Symbols.name, "&a=",
    ## from.m, : downloaded length 24698 != reported length 200

    ## pausing 1 second between requests for more than 5 symbols

    ## Warning in download.file(paste(yahoo.URL, "s=", Symbols.name, "&a=",
    ## from.m, : downloaded length 23860 != reported length 200

    ## pausing 1 second between requests for more than 5 symbols

    ## Warning in download.file(paste(yahoo.URL, "s=", Symbols.name, "&a=",
    ## from.m, : downloaded length 26062 != reported length 200

    ## pausing 1 second between requests for more than 5 symbols

    ## Warning in download.file(paste(yahoo.URL, "s=", Symbols.name, "&a=",
    ## from.m, : downloaded length 24725 != reported length 200

    ## pausing 1 second between requests for more than 5 symbols

    ## Warning in download.file(paste(yahoo.URL, "s=", Symbols.name, "&a=",
    ## from.m, : downloaded length 25071 != reported length 200

    ## pausing 1 second between requests for more than 5 symbols

    ## Warning in download.file(paste(yahoo.URL, "s=", Symbols.name, "&a=",
    ## from.m, : downloaded length 24412 != reported length 200

    ## pausing 1 second between requests for more than 5 symbols

    ## Warning in download.file(paste(yahoo.URL, "s=", Symbols.name, "&a=",
    ## from.m, : downloaded length 24346 != reported length 200

    ## pausing 1 second between requests for more than 5 symbols

    ## Warning in download.file(paste(yahoo.URL, "s=", Symbols.name, "&a=",
    ## from.m, : downloaded length 25931 != reported length 200

    ## pausing 1 second between requests for more than 5 symbols

    ## Warning in download.file(paste(yahoo.URL, "s=", Symbols.name, "&a=",
    ## from.m, : downloaded length 23352 != reported length 200

    ## pausing 1 second between requests for more than 5 symbols

    ## Warning in download.file(paste(yahoo.URL, "s=", Symbols.name, "&a=",
    ## from.m, : downloaded length 29571 != reported length 200

    ## pausing 1 second between requests for more than 5 symbols

    ## Warning in download.file(paste(yahoo.URL, "s=", Symbols.name, "&a=",
    ## from.m, : downloaded length 24573 != reported length 200

    ## pausing 1 second between requests for more than 5 symbols

    ## Warning in download.file(paste(yahoo.URL, "s=", Symbols.name, "&a=",
    ## from.m, : downloaded length 30601 != reported length 200

    ## pausing 1 second between requests for more than 5 symbols

    ## Warning in download.file(paste(yahoo.URL, "s=", Symbols.name, "&a=",
    ## from.m, : downloaded length 23213 != reported length 200

    ## pausing 1 second between requests for more than 5 symbols

    ## Warning in download.file(paste(yahoo.URL, "s=", Symbols.name, "&a=",
    ## from.m, : downloaded length 24895 != reported length 200

    ## pausing 1 second between requests for more than 5 symbols

    ## Warning in download.file(paste(yahoo.URL, "s=", Symbols.name, "&a=",
    ## from.m, : downloaded length 28230 != reported length 200

    ## pausing 1 second between requests for more than 5 symbols

    ## Warning in download.file(paste(yahoo.URL, "s=", Symbols.name, "&a=",
    ## from.m, : downloaded length 24064 != reported length 200

    ## pausing 1 second between requests for more than 5 symbols

    ## Warning in download.file(paste(yahoo.URL, "s=", Symbols.name, "&a=",
    ## from.m, : downloaded length 29136 != reported length 200

    ## pausing 1 second between requests for more than 5 symbols

    ## Warning in download.file(paste(yahoo.URL, "s=", Symbols.name, "&a=",
    ## from.m, : downloaded length 24912 != reported length 200

    ## pausing 1 second between requests for more than 5 symbols

    ## Warning in download.file(paste(yahoo.URL, "s=", Symbols.name, "&a=",
    ## from.m, : downloaded length 25057 != reported length 200

    ## pausing 1 second between requests for more than 5 symbols

    ## Warning in download.file(paste(yahoo.URL, "s=", Symbols.name, "&a=",
    ## from.m, : downloaded length 23877 != reported length 200

    ## pausing 1 second between requests for more than 5 symbols

    ## Warning in download.file(paste(yahoo.URL, "s=", Symbols.name, "&a=",
    ## from.m, : downloaded length 22661 != reported length 200

    ## pausing 1 second between requests for more than 5 symbols

    ## Warning in download.file(paste(yahoo.URL, "s=", Symbols.name, "&a=",
    ## from.m, : downloaded length 22898 != reported length 200

    ## pausing 1 second between requests for more than 5 symbols

    ## Warning in download.file(paste(yahoo.URL, "s=", Symbols.name, "&a=",
    ## from.m, : downloaded length 25927 != reported length 200

    ## pausing 1 second between requests for more than 5 symbols

    ## Warning in download.file(paste(yahoo.URL, "s=", Symbols.name, "&a=",
    ## from.m, : downloaded length 24793 != reported length 200

    ## pausing 1 second between requests for more than 5 symbols

    ## Warning in download.file(paste(yahoo.URL, "s=", Symbols.name, "&a=",
    ## from.m, : downloaded length 25937 != reported length 200

    ## pausing 1 second between requests for more than 5 symbols

    ## Warning in download.file(paste(yahoo.URL, "s=", Symbols.name, "&a=",
    ## from.m, : downloaded length 24554 != reported length 200

    ## pausing 1 second between requests for more than 5 symbols

    ## Warning in download.file(paste(yahoo.URL, "s=", Symbols.name, "&a=",
    ## from.m, : downloaded length 22381 != reported length 200

    ## pausing 1 second between requests for more than 5 symbols

    ## Warning in download.file(paste(yahoo.URL, "s=", Symbols.name, "&a=",
    ## from.m, : downloaded length 24497 != reported length 200

    ## pausing 1 second between requests for more than 5 symbols

    ## Warning in download.file(paste(yahoo.URL, "s=", Symbols.name, "&a=",
    ## from.m, : downloaded length 23515 != reported length 200

    ## pausing 1 second between requests for more than 5 symbols

    ## Warning in download.file(paste(yahoo.URL, "s=", Symbols.name, "&a=",
    ## from.m, : downloaded length 22751 != reported length 200

    ## pausing 1 second between requests for more than 5 symbols

    ## Warning in download.file(paste(yahoo.URL, "s=", Symbols.name, "&a=",
    ## from.m, : downloaded length 24510 != reported length 200

    ## pausing 1 second between requests for more than 5 symbols

    ## Warning in download.file(paste(yahoo.URL, "s=", Symbols.name, "&a=",
    ## from.m, : downloaded length 24671 != reported length 200

    ## pausing 1 second between requests for more than 5 symbols

    ##  [1] "ABEV3.SA" "BBAS3.SA" "BBDC3.SA" "BBDC4.SA" "BBSE3.SA" "BRAP4.SA"
    ##  [7] "BRFS3.SA" "BRKM5.SA" "BRML3.SA" "BRPR3.SA" "BVMF3.SA" "CCRO3.SA"
    ## [13] "CESP6.SA" "CIEL3.SA" "CMIG4.SA" "CPFE3.SA" "CPLE6.SA" "CRUZ3.SA"
    ## [19] "CSAN3.SA" "CSNA3.SA" "CTIP3.SA" "CYRE3.SA" "DTEX3.SA" "ECOR3.SA"
    ## [25] "ELET3.SA" "ELET6.SA" "EMBR3.SA" "ENBR3.SA" "ESTC3.SA" "FIBR3.SA"
    ## [31] "GFSA3.SA" "GGBR4.SA" "GOAU4.SA" "GOLL4.SA" "HGTX3.SA" "HYPE3.SA"

``` r
getSymbols(ibovespa[38:40], auto.assign = TRUE, from=datainicial, to=datafinal) #O 41(IBOV.SA) dá problema
```

    ## Warning in download.file(paste(yahoo.URL, "s=", Symbols.name, "&a=",
    ## from.m, : downloaded length 27533 != reported length 200

    ## Warning in download.file(paste(yahoo.URL, "s=", Symbols.name, "&a=",
    ## from.m, : downloaded length 27460 != reported length 200

    ## Warning in download.file(paste(yahoo.URL, "s=", Symbols.name, "&a=",
    ## from.m, : downloaded length 23250 != reported length 200

    ## [1] "ITSA4.SA" "ITUB4.SA" "JBSS3.SA"

``` r
getSymbols(ibovespa[42:56], auto.assign = TRUE, from=datainicial, to=datafinal) #O 57(IBOV.SA) dá problema
```

    ## Warning in download.file(paste(yahoo.URL, "s=", Symbols.name, "&a=",
    ## from.m, : downloaded length 27969 != reported length 200

    ## Warning in download.file(paste(yahoo.URL, "s=", Symbols.name, "&a=",
    ## from.m, : downloaded length 24738 != reported length 200

    ## Warning in download.file(paste(yahoo.URL, "s=", Symbols.name, "&a=",
    ## from.m, : downloaded length 25446 != reported length 200

    ## Warning in download.file(paste(yahoo.URL, "s=", Symbols.name, "&a=",
    ## from.m, : downloaded length 22148 != reported length 200

    ## Warning in download.file(paste(yahoo.URL, "s=", Symbols.name, "&a=",
    ## from.m, : downloaded length 23469 != reported length 200

    ## pausing 1 second between requests for more than 5 symbols

    ## Warning in download.file(paste(yahoo.URL, "s=", Symbols.name, "&a=",
    ## from.m, : downloaded length 25545 != reported length 200

    ## pausing 1 second between requests for more than 5 symbols

    ## Warning in download.file(paste(yahoo.URL, "s=", Symbols.name, "&a=",
    ## from.m, : downloaded length 24807 != reported length 200

    ## pausing 1 second between requests for more than 5 symbols

    ## Warning in download.file(paste(yahoo.URL, "s=", Symbols.name, "&a=",
    ## from.m, : downloaded length 21555 != reported length 200

    ## pausing 1 second between requests for more than 5 symbols

    ## Warning in download.file(paste(yahoo.URL, "s=", Symbols.name, "&a=",
    ## from.m, : downloaded length 25453 != reported length 200

    ## pausing 1 second between requests for more than 5 symbols

    ## Warning in download.file(paste(yahoo.URL, "s=", Symbols.name, "&a=",
    ## from.m, : downloaded length 25089 != reported length 200

    ## pausing 1 second between requests for more than 5 symbols

    ## Warning in download.file(paste(yahoo.URL, "s=", Symbols.name, "&a=",
    ## from.m, : downloaded length 24258 != reported length 200

    ## pausing 1 second between requests for more than 5 symbols

    ## Warning in download.file(paste(yahoo.URL, "s=", Symbols.name, "&a=",
    ## from.m, : downloaded length 22166 != reported length 200

    ## pausing 1 second between requests for more than 5 symbols

    ## Warning in download.file(paste(yahoo.URL, "s=", Symbols.name, "&a=",
    ## from.m, : downloaded length 24618 != reported length 200

    ## pausing 1 second between requests for more than 5 symbols

    ## Warning in download.file(paste(yahoo.URL, "s=", Symbols.name, "&a=",
    ## from.m, : downloaded length 25663 != reported length 200

    ## pausing 1 second between requests for more than 5 symbols

    ## Warning in download.file(paste(yahoo.URL, "s=", Symbols.name, "&a=",
    ## from.m, : downloaded length 3922 != reported length 200

    ## pausing 1 second between requests for more than 5 symbols

    ##  [1] "KROT3.SA" "LAME4.SA" "LREN3.SA" "MRFG3.SA" "MRVE3.SA" "MULT3.SA"
    ##  [7] "NATU3.SA" "OIBR4.SA" "PCAR4.SA" "PETR3.SA" "PETR4.SA" "POMO4.SA"
    ## [13] "QUAL3.SA" "RENT3.SA" "RUMO3.SA"

``` r
getSymbols(ibovespa[58:length(ibovespa)], auto.assign = TRUE, from=datainicial, to=datafinal)
```

    ## Warning in download.file(paste(yahoo.URL, "s=", Symbols.name, "&a=",
    ## from.m, : downloaded length 25758 != reported length 200

    ## Warning in download.file(paste(yahoo.URL, "s=", Symbols.name, "&a=",
    ## from.m, : downloaded length 24285 != reported length 200

    ## Warning in download.file(paste(yahoo.URL, "s=", Symbols.name, "&a=",
    ## from.m, : downloaded length 23404 != reported length 200

    ## Warning in download.file(paste(yahoo.URL, "s=", Symbols.name, "&a=",
    ## from.m, : downloaded length 24285 != reported length 200

    ## Warning in download.file(paste(yahoo.URL, "s=", Symbols.name, "&a=",
    ## from.m, : downloaded length 24335 != reported length 200

    ## pausing 1 second between requests for more than 5 symbols

    ## Warning in download.file(paste(yahoo.URL, "s=", Symbols.name, "&a=",
    ## from.m, : downloaded length 25286 != reported length 200

    ## pausing 1 second between requests for more than 5 symbols

    ## Warning in download.file(paste(yahoo.URL, "s=", Symbols.name, "&a=",
    ## from.m, : downloaded length 23177 != reported length 200

    ## pausing 1 second between requests for more than 5 symbols

    ## Warning in download.file(paste(yahoo.URL, "s=", Symbols.name, "&a=",
    ## from.m, : downloaded length 26166 != reported length 200

    ## pausing 1 second between requests for more than 5 symbols

    ## Warning in download.file(paste(yahoo.URL, "s=", Symbols.name, "&a=",
    ## from.m, : downloaded length 25239 != reported length 200

    ## pausing 1 second between requests for more than 5 symbols

    ## Warning in download.file(paste(yahoo.URL, "s=", Symbols.name, "&a=",
    ## from.m, : downloaded length 27244 != reported length 200

    ## pausing 1 second between requests for more than 5 symbols

    ##  [1] "SBSP3.SA" "SMLE3.SA" "SUZB5.SA" "TBLE3.SA" "TIMP3.SA" "UGPA3.SA"
    ##  [7] "USIM5.SA" "VALE3.SA" "VALE5.SA" "VIVT4.SA"

Como exemplo, esta é o arquivo de cotações da Petrobras baixado, seguido por um gráfico:

``` r
head(PETR3.SA)
```

    ##            PETR3.SA.Open PETR3.SA.High PETR3.SA.Low PETR3.SA.Close
    ## 2013-08-26         17.50         17.69        17.14          17.14
    ## 2013-08-27         16.98         17.12        16.53          16.57
    ## 2013-08-28         16.60         16.75        16.10          16.17
    ## 2013-08-29         16.26         16.49        15.96          15.97
    ## 2013-08-30         16.26         16.49        15.71          16.05
    ## 2013-09-02         16.38         16.43        16.05          16.15
    ##            PETR3.SA.Volume PETR3.SA.Adjusted
    ## 2013-08-26         6789100          16.53003
    ## 2013-08-27        10819800          15.98032
    ## 2013-08-28         7769900          15.59455
    ## 2013-08-29         6036800          15.40167
    ## 2013-08-30        13563200          15.47882
    ## 2013-09-02         6689200          15.57526

``` r
lineChart(PETR3.SA)
```

![](postBovespaBlog_files/figure-markdown_github/unnamed-chunk-6-1.png)

As variáveis criadas pelo quantmod com as cotações baixadas do Yahoo Finance são da classe xts. Além disso, é apenas na última coluna (Adjusted) que estamos interessados, além, claro, da coluna referente às datas. Portanto, foi criada uma função que transforma a variável em dataframe e extrai apenas a coluna referente ao Adjusted:

``` r
f = function(x) {
  temp = as.data.frame(x)
  result = as.data.frame(temp[,6])
  names(result) = names(temp)[6]
  result$Data = rownames(temp)
  return(result)
}
```

Criada a função, basta aplicá-la em cada uma das variáveis. Infelizmente, tive de fazer isso uma por uma, manualmente, porque não consiga pensar em um for loop que iterasse em todos os arquivos da classe xts. Sugestões são muito bem vindas.

``` r
ABEV3.SA = f(ABEV3.SA)
BBAS3.SA = f(BBAS3.SA)
BBDC3.SA = f(BBDC3.SA)
BBDC4.SA = f(BBDC4.SA)
BBSE3.SA = f(BBSE3.SA)
BRAP4.SA = f(BRAP4.SA)
BRFS3.SA = f(BRFS3.SA) 
BRKM5.SA = f(BRKM5.SA)
BRML3.SA = f(BRML3.SA)
BRPR3.SA = f(BRPR3.SA)
BVMF3.SA = f(BVMF3.SA)
CCRO3.SA = f(CCRO3.SA)
CESP6.SA = f(CESP6.SA)
CIEL3.SA = f(CIEL3.SA)
CMIG4.SA = f(CMIG4.SA)
CPFE3.SA = f(CPFE3.SA)
CPLE6.SA = f(CPLE6.SA)
CRUZ3.SA = f(CRUZ3.SA)
CSAN3.SA = f(CSAN3.SA)
CSNA3.SA = f(CSNA3.SA)
CTIP3.SA = f(CTIP3.SA)
CYRE3.SA = f(CYRE3.SA)
DTEX3.SA = f(DTEX3.SA)
ECOR3.SA = f(ECOR3.SA)
ELET3.SA = f(ELET3.SA)
ELET6.SA = f(ELET6.SA)
EMBR3.SA = f(EMBR3.SA)
ENBR3.SA = f(ENBR3.SA)
ESTC3.SA = f(ESTC3.SA)
FIBR3.SA = f(FIBR3.SA)
GFSA3.SA = f(GFSA3.SA)
GGBR4.SA = f(GGBR4.SA)
GOAU4.SA = f(GOAU4.SA)
GOLL4.SA = f(GOLL4.SA)
HGTX3.SA = f(HGTX3.SA)
HYPE3.SA = f(HYPE3.SA)
ITSA4.SA = f(ITSA4.SA)
ITUB4.SA = f(ITUB4.SA)
JBSS3.SA = f(JBSS3.SA)
KROT3.SA = f(KROT3.SA)
LAME4.SA = f(LAME4.SA)
LREN3.SA = f(LREN3.SA)
MRFG3.SA = f(MRFG3.SA)
MRVE3.SA = f(MRVE3.SA)
MULT3.SA = f(MULT3.SA)
NATU3.SA = f(NATU3.SA)
OIBR4.SA = f(OIBR4.SA)
PCAR4.SA = f(PCAR4.SA)
PETR3.SA = f(PETR3.SA)
PETR4.SA = f(PETR4.SA)
POMO4.SA = f(POMO4.SA)
QUAL3.SA = f(QUAL3.SA)
RENT3.SA = f(RENT3.SA)
RUMO3.SA = f(RUMO3.SA)
SBSP3.SA = f(SBSP3.SA)
SMLE3.SA = f(SMLE3.SA)
SUZB5.SA = f(SUZB5.SA)
TBLE3.SA = f(TBLE3.SA)
TIMP3.SA = f(TIMP3.SA)
UGPA3.SA = f(UGPA3.SA)
USIM5.SA = f(USIM5.SA)
VALE3.SA = f(VALE3.SA)
VALE5.SA = f(VALE5.SA)
VIVT4.SA = f(VIVT4.SA)
```

O último passo da etapa de transformação de dados consiste em juntar todas as ações das empresas, além das cotações do dólar, em uma variável só, chamada de df\_final. Nesta etapa, tive um problema semelhante à anterior: não sabia muito bem como criar um for loop para não ter de escrever as variáveis manualmente. Ainda bem que o iBovespa só é composto por 67 empresas.

``` r
df_final = join_all(dfs=list(cotacoes,
                              ABEV3.SA,BBAS3.SA,BBDC3.SA,BBDC4.SA,BBSE3.SA,BRAP4.SA,BRFS3.SA,BRKM5.SA,BRML3.SA,
                              BRPR3.SA,BVMF3.SA,CCRO3.SA,CESP6.SA,CIEL3.SA,CMIG4.SA,CPFE3.SA,CPLE6.SA,CRUZ3.SA,
                              CSAN3.SA,CSNA3.SA,CTIP3.SA,CYRE3.SA,DTEX3.SA,ECOR3.SA,ELET3.SA,ELET6.SA,EMBR3.SA,
                              ENBR3.SA,ESTC3.SA,FIBR3.SA,GFSA3.SA,GGBR4.SA,GOAU4.SA,GOLL4.SA,HGTX3.SA,HYPE3.SA,
                              ITSA4.SA,ITUB4.SA,JBSS3.SA,KROT3.SA,LAME4.SA,LREN3.SA,MRFG3.SA,MRVE3.SA,MULT3.SA,
                              NATU3.SA,OIBR4.SA,PCAR4.SA,PETR3.SA,PETR4.SA,POMO4.SA,QUAL3.SA,RENT3.SA,RUMO3.SA,
                              SBSP3.SA,SMLE3.SA,SUZB5.SA,TBLE3.SA,TIMP3.SA,UGPA3.SA,USIM5.SA,VALE3.SA,VALE5.SA,
                              VIVT4.SA), by="Data")
```

Agora, um novo dataframe sem a coluna de data é criado. Isso é necessário para criar a matriz de correlação, que só aceita como argumento um dataframe 100% composto de elementos numéricos.

``` r
df_final2 = na.omit(df_final)
df_final2 = select(df_final2, -Data)
```

Finalmente, podemos prosseguir com a criação da matriz de correlação usando a função base cor():

``` r
m = cor(df_final2)
m
```

    ##                        USD.BRL ABEV3.SA.Adjusted BBAS3.SA.Adjusted
    ## USD.BRL            1.000000000      0.0065237154       -0.78346582
    ## ABEV3.SA.Adjusted  0.006523715      1.0000000000        0.26066766
    ## BBAS3.SA.Adjusted -0.783465819      0.2606676596        1.00000000
    ## BBDC3.SA.Adjusted -0.559099285      0.3383855428        0.70494887
    ## BBDC4.SA.Adjusted -0.772544701      0.2368133922        0.90154315
    ## BBSE3.SA.Adjusted -0.812723431      0.1570762312        0.87987611
    ## BRAP4.SA.Adjusted -0.634233415     -0.2212636432        0.63506917
    ## BRFS3.SA.Adjusted  0.682417067      0.2754835032       -0.64024892
    ## BRKM5.SA.Adjusted -0.300356979      0.0582865554        0.28817515
    ## BRML3.SA.Adjusted -0.741005535     -0.0279387667        0.69946183
    ## BRPR3.SA.Adjusted -0.045352850      0.2124285015        0.10657611
    ## BVMF3.SA.Adjusted -0.873495673      0.1500872481        0.82294183
    ## CCRO3.SA.Adjusted -0.511495825      0.0984825037        0.53388900
    ## CESP6.SA.Adjusted -0.639790114      0.1078537728        0.59156295
    ## CIEL3.SA.Adjusted  0.314515856      0.2402205888       -0.24520247
    ## CMIG4.SA.Adjusted -0.790029738     -0.2186449574        0.72173340
    ## CPFE3.SA.Adjusted -0.519303811      0.2863927890        0.63347231
    ## CPLE6.SA.Adjusted -0.026570726      0.7273127319        0.27785027
    ## CRUZ3.SA.Adjusted -0.659017854      0.1682178415        0.72831185
    ## CSAN3.SA.Adjusted -0.836329728     -0.1887013285        0.73108426
    ## CSNA3.SA.Adjusted -0.769779132     -0.1213969058        0.82274443
    ## CTIP3.SA.Adjusted  0.336590131      0.3961284247       -0.23775963
    ## CYRE3.SA.Adjusted -0.597389108     -0.0502203088        0.60029772
    ## DTEX3.SA.Adjusted -0.772026500     -0.0037847006        0.79441449
    ## ECOR3.SA.Adjusted -0.798257744     -0.0900746032        0.72949828
    ## ELET3.SA.Adjusted -0.687807023     -0.0186551954        0.78366586
    ## ELET6.SA.Adjusted -0.191068779     -0.1532332029        0.20061928
    ## EMBR3.SA.Adjusted  0.273655451     -0.0437537398       -0.36699292
    ## ENBR3.SA.Adjusted  0.539702192      0.4910371636       -0.25526060
    ## ESTC3.SA.Adjusted -0.504366750     -0.1602003979        0.13462669
    ## FIBR3.SA.Adjusted  0.246033794     -0.2362261555       -0.11046946
    ## GFSA3.SA.Adjusted -0.331735691      0.3784222221        0.56006067
    ## GGBR4.SA.Adjusted -0.797173692     -0.1758640437        0.78347102
    ## GOAU4.SA.Adjusted -0.776681945     -0.1551230789        0.74946601
    ## GOLL4.SA.Adjusted -0.806915122     -0.2606618918        0.64248981
    ## HGTX3.SA.Adjusted -0.617874928      0.0194065513        0.69670845
    ## HYPE3.SA.Adjusted -0.088649050     -0.0146042313       -0.12536572
    ## ITSA4.SA.Adjusted -0.558717505      0.3625893769        0.66969466
    ## ITUB4.SA.Adjusted -0.857298804     -0.0007558543        0.88998417
    ## JBSS3.SA.Adjusted -0.342140079     -0.0097778957        0.20964147
    ## KROT3.SA.Adjusted -0.392245524     -0.1894596630        0.05398823
    ## LAME4.SA.Adjusted  0.002772544      0.7213596301        0.13104881
    ## LREN3.SA.Adjusted  0.035992221     -0.0691455791       -0.19093575
    ## MRFG3.SA.Adjusted  0.517055297      0.4906429404       -0.36647961
    ## MRVE3.SA.Adjusted -0.482412517      0.1342876324        0.37253875
    ## MULT3.SA.Adjusted -0.678287407      0.0367812596        0.70585204
    ## NATU3.SA.Adjusted -0.415362052      0.0366199531        0.46927372
    ## OIBR4.SA.Adjusted -0.566406717     -0.4750117152        0.29268316
    ## PCAR4.SA.Adjusted -0.709017078     -0.0723561792        0.73417184
    ## PETR3.SA.Adjusted -0.614373251     -0.1026435724        0.52306485
    ## PETR4.SA.Adjusted -0.760015843     -0.1357083219        0.65270822
    ## POMO4.SA.Adjusted -0.659647754     -0.2262486305        0.58672170
    ## QUAL3.SA.Adjusted -0.648398080      0.1994957934        0.76340979
    ## RENT3.SA.Adjusted -0.717689346     -0.0588653388        0.64433311
    ## RUMO3.SA.Adjusted -0.185407925      0.0783300737        0.30918975
    ## SBSP3.SA.Adjusted -0.187708352      0.1880265201        0.33624783
    ## SMLE3.SA.Adjusted  0.308193179      0.5433649892       -0.19129844
    ## SUZB5.SA.Adjusted  0.366263762     -0.3474168279       -0.35703308
    ## TBLE3.SA.Adjusted -0.121755545      0.6703648117        0.42812560
    ## TIMP3.SA.Adjusted -0.127444976     -0.2173456351       -0.11680890
    ## UGPA3.SA.Adjusted -0.275872844     -0.0636423037        0.32501823
    ## USIM5.SA.Adjusted -0.743681463     -0.1150177771        0.82837489
    ## VALE3.SA.Adjusted -0.580478341     -0.1377850756        0.65258922
    ## VALE5.SA.Adjusted -0.608094501     -0.2419958212        0.60621095
    ## VIVT4.SA.Adjusted -0.652512277      0.0922362561        0.80026959
    ##                   BBDC3.SA.Adjusted BBDC4.SA.Adjusted BBSE3.SA.Adjusted
    ## USD.BRL                -0.559099285       -0.77254470       -0.81272343
    ## ABEV3.SA.Adjusted       0.338385543        0.23681339        0.15707623
    ## BBAS3.SA.Adjusted       0.704948870        0.90154315        0.87987611
    ## BBDC3.SA.Adjusted       1.000000000        0.88573231        0.62829991
    ## BBDC4.SA.Adjusted       0.885732308        1.00000000        0.80729590
    ## BBSE3.SA.Adjusted       0.628299909        0.80729590        1.00000000
    ## BRAP4.SA.Adjusted       0.166497612        0.50616662        0.60813992
    ## BRFS3.SA.Adjusted      -0.617922415       -0.73257421       -0.64399762
    ## BRKM5.SA.Adjusted      -0.303220718        0.03584852        0.34772177
    ## BRML3.SA.Adjusted       0.833913784        0.84637550        0.76393492
    ## BRPR3.SA.Adjusted       0.693885648        0.40254113        0.12432347
    ## BVMF3.SA.Adjusted       0.553504204        0.78079923        0.81486674
    ## CCRO3.SA.Adjusted       0.695886587        0.71160484        0.55448134
    ## CESP6.SA.Adjusted       0.778680010        0.71493411        0.68804655
    ## CIEL3.SA.Adjusted      -0.554872662       -0.42949012       -0.32573913
    ## CMIG4.SA.Adjusted       0.400560621        0.68533018        0.80085891
    ## CPFE3.SA.Adjusted       0.449376230        0.64601698        0.62352551
    ## CPLE6.SA.Adjusted       0.398048367        0.27971097        0.18734663
    ## CRUZ3.SA.Adjusted       0.804647084        0.84423233        0.64966017
    ## CSAN3.SA.Adjusted       0.695901143        0.81812279        0.79661043
    ## CSNA3.SA.Adjusted       0.410580594        0.74973043        0.75676372
    ## CTIP3.SA.Adjusted      -0.317631450       -0.31383874       -0.35701443
    ## CYRE3.SA.Adjusted       0.856828353        0.79907791        0.64260411
    ## DTEX3.SA.Adjusted       0.724422602        0.86825679        0.79054389
    ## ECOR3.SA.Adjusted       0.705791348        0.80732053        0.81415392
    ## ELET3.SA.Adjusted       0.297123417        0.65175488        0.67869976
    ## ELET6.SA.Adjusted      -0.448893522       -0.04990403        0.19801395
    ## EMBR3.SA.Adjusted      -0.182260733       -0.28294110       -0.24890710
    ## ENBR3.SA.Adjusted      -0.181431344       -0.28668984       -0.47998558
    ## ESTC3.SA.Adjusted       0.219375876        0.21244376        0.25607271
    ## FIBR3.SA.Adjusted       0.114244961        0.00628701        0.01007470
    ## GFSA3.SA.Adjusted       0.347874268        0.55292791        0.38073361
    ## GGBR4.SA.Adjusted       0.637381122        0.82930854        0.80833390
    ## GOAU4.SA.Adjusted       0.716478376        0.83140329        0.80074854
    ## GOLL4.SA.Adjusted       0.515910162        0.69464343        0.76690738
    ## HGTX3.SA.Adjusted       0.807029311        0.83434636        0.59318597
    ## HYPE3.SA.Adjusted      -0.318705952       -0.28551435        0.12636521
    ## ITSA4.SA.Adjusted       0.588848173        0.66108761        0.72690420
    ## ITUB4.SA.Adjusted       0.721041954        0.89306003        0.87383172
    ## JBSS3.SA.Adjusted      -0.142892180        0.03351416        0.38035263
    ## KROT3.SA.Adjusted      -0.184234100       -0.04990236        0.24360066
    ## LAME4.SA.Adjusted       0.112120988        0.08340342        0.05366386
    ## LREN3.SA.Adjusted      -0.327308459       -0.37734976       -0.11530788
    ## MRFG3.SA.Adjusted      -0.280464850       -0.47716750       -0.44263919
    ## MRVE3.SA.Adjusted       0.651586554        0.57356195        0.49368939
    ## MULT3.SA.Adjusted       0.890787300        0.87691107        0.74106449
    ## NATU3.SA.Adjusted       0.003161942        0.30252342        0.52799374
    ## OIBR4.SA.Adjusted       0.173652043        0.31831880        0.56543067
    ## PCAR4.SA.Adjusted       0.774932524        0.87782708        0.69816149
    ## PETR3.SA.Adjusted      -0.077981442        0.28942817        0.56336818
    ## PETR4.SA.Adjusted       0.168068707        0.50172787        0.73945486
    ## POMO4.SA.Adjusted       0.142141132        0.52961581        0.60555114
    ## QUAL3.SA.Adjusted       0.636846333        0.80805955        0.66582456
    ## RENT3.SA.Adjusted       0.818981940        0.81213146        0.72033340
    ## RUMO3.SA.Adjusted       0.491346433        0.48915088        0.29516038
    ## SBSP3.SA.Adjusted       0.222769327        0.40608231        0.27557817
    ## SMLE3.SA.Adjusted       0.047804213       -0.11123607       -0.31692690
    ## SUZB5.SA.Adjusted      -0.743009519       -0.61216405       -0.24521061
    ## TBLE3.SA.Adjusted       0.460918049        0.48499278        0.27320901
    ## TIMP3.SA.Adjusted       0.056544053       -0.10807272        0.06797638
    ## UGPA3.SA.Adjusted      -0.103025143        0.23499133        0.32756204
    ## USIM5.SA.Adjusted       0.513001124        0.79848056        0.73403465
    ## VALE3.SA.Adjusted       0.159821554        0.52857589        0.53010602
    ## VALE5.SA.Adjusted       0.068454518        0.45287106        0.55417508
    ## VIVT4.SA.Adjusted       0.815145076        0.89680199        0.69176793
    ##                   BRAP4.SA.Adjusted BRFS3.SA.Adjusted BRKM5.SA.Adjusted
    ## USD.BRL                -0.634233415        0.68241707       -0.30035698
    ## ABEV3.SA.Adjusted      -0.221263643        0.27548350        0.05828656
    ## BBAS3.SA.Adjusted       0.635069166       -0.64024892        0.28817515
    ## BBDC3.SA.Adjusted       0.166497612       -0.61792241       -0.30322072
    ## BBDC4.SA.Adjusted       0.506166616       -0.73257421        0.03584852
    ## BBSE3.SA.Adjusted       0.608139923       -0.64399762        0.34772177
    ## BRAP4.SA.Adjusted       1.000000000       -0.47064123        0.50905403
    ## BRFS3.SA.Adjusted      -0.470641233        1.00000000        0.09696694
    ## BRKM5.SA.Adjusted       0.509054028        0.09696694        1.00000000
    ## BRML3.SA.Adjusted       0.404065694       -0.80502006       -0.10744562
    ## BRPR3.SA.Adjusted      -0.275015687       -0.32590959       -0.60013855
    ## BVMF3.SA.Adjusted       0.638974401       -0.58739850        0.37778983
    ## CCRO3.SA.Adjusted       0.312934610       -0.61289593       -0.20234872
    ## CESP6.SA.Adjusted       0.294472491       -0.65558258       -0.16073504
    ## CIEL3.SA.Adjusted      -0.230055497        0.50818430        0.31174191
    ## CMIG4.SA.Adjusted       0.791484600       -0.70313462        0.43914767
    ## CPFE3.SA.Adjusted       0.548300626       -0.31253677        0.37597501
    ## CPLE6.SA.Adjusted      -0.279849625        0.10331979       -0.07897263
    ## CRUZ3.SA.Adjusted       0.504912123       -0.65658169       -0.06153129
    ## CSAN3.SA.Adjusted       0.631991318       -0.80868980        0.09285943
    ## CSNA3.SA.Adjusted       0.861971006       -0.67549625        0.46375479
    ## CTIP3.SA.Adjusted      -0.537207504        0.44327484        0.10869525
    ## CYRE3.SA.Adjusted       0.312441422       -0.79729748       -0.29944460
    ## DTEX3.SA.Adjusted       0.651214136       -0.77569296        0.13651000
    ## ECOR3.SA.Adjusted       0.623729335       -0.75850789        0.08753283
    ## ELET3.SA.Adjusted       0.880761770       -0.54085853        0.48901363
    ## ELET6.SA.Adjusted       0.640845525        0.05433998        0.77163206
    ## EMBR3.SA.Adjusted      -0.205295776        0.26540427       -0.01137018
    ## ENBR3.SA.Adjusted      -0.456390260        0.50472434       -0.22815736
    ## ESTC3.SA.Adjusted       0.313384748       -0.19745652        0.07781708
    ## FIBR3.SA.Adjusted      -0.055193944       -0.19202934       -0.27571880
    ## GFSA3.SA.Adjusted       0.276807142       -0.30163427        0.19709165
    ## GGBR4.SA.Adjusted       0.741799926       -0.81040259        0.20907605
    ## GOAU4.SA.Adjusted       0.651156887       -0.83136505        0.06821733
    ## GOLL4.SA.Adjusted       0.714697549       -0.74217960        0.22664949
    ## HGTX3.SA.Adjusted       0.466998372       -0.74121568       -0.24927035
    ## HYPE3.SA.Adjusted       0.110663173        0.24597779        0.44275187
    ## ITSA4.SA.Adjusted       0.399486725       -0.39818952        0.29456681
    ## ITUB4.SA.Adjusted       0.656725275       -0.76701088        0.20552324
    ## JBSS3.SA.Adjusted       0.321704688        0.01200038        0.67252041
    ## KROT3.SA.Adjusted       0.271817523        0.03712342        0.47741193
    ## LAME4.SA.Adjusted      -0.358292848        0.26727920        0.17658428
    ## LREN3.SA.Adjusted      -0.020844345        0.29974708        0.17077540
    ## MRFG3.SA.Adjusted      -0.565090890        0.66270181       -0.16584978
    ## MRVE3.SA.Adjusted       0.034183236       -0.45595562       -0.17664933
    ## MULT3.SA.Adjusted       0.352972708       -0.80729441       -0.16943453
    ## NATU3.SA.Adjusted       0.681282485       -0.21204276        0.65325156
    ## OIBR4.SA.Adjusted       0.542189258       -0.54285915        0.25701808
    ## PCAR4.SA.Adjusted       0.600724045       -0.80722330       -0.04637346
    ## PETR3.SA.Adjusted       0.749734564       -0.20567733        0.74885837
    ## PETR4.SA.Adjusted       0.802891349       -0.44227593        0.66043303
    ## POMO4.SA.Adjusted       0.739114203       -0.51881748        0.57945214
    ## QUAL3.SA.Adjusted       0.367206746       -0.61570438        0.18479367
    ## RENT3.SA.Adjusted       0.440926848       -0.77869557       -0.11217652
    ## RUMO3.SA.Adjusted      -0.007168534       -0.43368381       -0.23696850
    ## SBSP3.SA.Adjusted       0.209828777       -0.28963707        0.24875275
    ## SMLE3.SA.Adjusted      -0.532694037        0.33973880       -0.30422449
    ## SUZB5.SA.Adjusted       0.073766381        0.39442237        0.39346499
    ## TBLE3.SA.Adjusted      -0.032704289       -0.05475126        0.04280692
    ## TIMP3.SA.Adjusted      -0.036758556       -0.04362381       -0.28359898
    ## UGPA3.SA.Adjusted       0.511562145       -0.11970930        0.65757222
    ## USIM5.SA.Adjusted       0.810746667       -0.74290242        0.28430059
    ## VALE3.SA.Adjusted       0.951671219       -0.44944691        0.50321784
    ## VALE5.SA.Adjusted       0.977348760       -0.43265680        0.58425659
    ## VIVT4.SA.Adjusted       0.415899307       -0.72797120       -0.08301361
    ##                   BRML3.SA.Adjusted BRPR3.SA.Adjusted BVMF3.SA.Adjusted
    ## USD.BRL                 -0.74100553     -0.0453528500       -0.87349567
    ## ABEV3.SA.Adjusted       -0.02793877      0.2124285015        0.15008725
    ## BBAS3.SA.Adjusted        0.69946183      0.1065761115        0.82294183
    ## BBDC3.SA.Adjusted        0.83391378      0.6938856475        0.55350420
    ## BBDC4.SA.Adjusted        0.84637550      0.4025411272        0.78079923
    ## BBSE3.SA.Adjusted        0.76393492      0.1243234687        0.81486674
    ## BRAP4.SA.Adjusted        0.40406569     -0.2750156873        0.63897440
    ## BRFS3.SA.Adjusted       -0.80502006     -0.3259095911       -0.58739850
    ## BRKM5.SA.Adjusted       -0.10744562     -0.6001385509        0.37778983
    ## BRML3.SA.Adjusted        1.00000000      0.5362242319        0.72154686
    ## BRPR3.SA.Adjusted        0.53622423      1.0000000000        0.01725110
    ## BVMF3.SA.Adjusted        0.72154686      0.0172510959        1.00000000
    ## CCRO3.SA.Adjusted        0.77764921      0.4737898811        0.63240934
    ## CESP6.SA.Adjusted        0.87943447      0.5805331707        0.61682198
    ## CIEL3.SA.Adjusted       -0.72241193     -0.5640662983       -0.23384516
    ## CMIG4.SA.Adjusted        0.72931028     -0.0101987616        0.76940034
    ## CPFE3.SA.Adjusted        0.54449063      0.1877620064        0.64847514
    ## CPLE6.SA.Adjusted        0.09963863      0.3080126105        0.23518311
    ## CRUZ3.SA.Adjusted        0.79695273      0.4209256480        0.69917243
    ## CSAN3.SA.Adjusted        0.92249081      0.2760792641        0.76522837
    ## CSNA3.SA.Adjusted        0.61728907     -0.0937069889        0.77487134
    ## CTIP3.SA.Adjusted       -0.53604624     -0.2049319736       -0.26004328
    ## CYRE3.SA.Adjusted        0.95613858      0.6426160741        0.55839527
    ## DTEX3.SA.Adjusted        0.89603569      0.3633569369        0.79323075
    ## ECOR3.SA.Adjusted        0.90068387      0.3034651153        0.76980379
    ## ELET3.SA.Adjusted        0.46365680     -0.1693945661        0.72940900
    ## ELET6.SA.Adjusted       -0.20860795     -0.6399296558        0.26744102
    ## EMBR3.SA.Adjusted       -0.06524479      0.0760864015       -0.17596157
    ## ENBR3.SA.Adjusted       -0.61334563      0.0126472696       -0.46046673
    ## ESTC3.SA.Adjusted        0.42907189      0.0761843651        0.43027801
    ## FIBR3.SA.Adjusted        0.12066438      0.2702041177       -0.34815164
    ## GFSA3.SA.Adjusted        0.22674482      0.1244939084        0.46505018
    ## GGBR4.SA.Adjusted        0.87773972      0.2290067195        0.77341682
    ## GOAU4.SA.Adjusted        0.93261448      0.3296405906        0.74288935
    ## GOLL4.SA.Adjusted        0.80985599      0.1407298695        0.74259168
    ## HGTX3.SA.Adjusted        0.83966385      0.4633593801        0.67380138
    ## HYPE3.SA.Adjusted       -0.13625514     -0.3679011714       -0.01929366
    ## ITSA4.SA.Adjusted        0.65111630      0.2537812014        0.61161955
    ## ITUB4.SA.Adjusted        0.86496533      0.1613338734        0.82596542
    ## JBSS3.SA.Adjusted        0.12396966     -0.3791819543        0.33472452
    ## KROT3.SA.Adjusted        0.11079661     -0.3809007056        0.29758143
    ## LAME4.SA.Adjusted       -0.14691194      0.0210391567        0.13087509
    ## LREN3.SA.Adjusted       -0.33923763     -0.4825416357       -0.15924461
    ## MRFG3.SA.Adjusted       -0.61380824     -0.1776093789       -0.44265194
    ## MRVE3.SA.Adjusted        0.73361731      0.5501650835        0.57225209
    ## MULT3.SA.Adjusted        0.97100872      0.6077033659        0.63745405
    ## NATU3.SA.Adjusted        0.26537199     -0.3228506996        0.54775101
    ## OIBR4.SA.Adjusted        0.61401110     -0.0003408243        0.49209455
    ## PCAR4.SA.Adjusted        0.88023130      0.4386592636        0.66974352
    ## PETR3.SA.Adjusted        0.14141600     -0.5892010352        0.62601545
    ## PETR4.SA.Adjusted        0.42514946     -0.3559269990        0.74330752
    ## POMO4.SA.Adjusted        0.43234741     -0.1746921792        0.64995702
    ## QUAL3.SA.Adjusted        0.68560706      0.2944242102        0.71752102
    ## RENT3.SA.Adjusted        0.95999943      0.5291223314        0.66311693
    ## RUMO3.SA.Adjusted        0.57037165      0.5342148345        0.33842048
    ## SBSP3.SA.Adjusted        0.28571290      0.2722983484        0.29745152
    ## SMLE3.SA.Adjusted       -0.26001796      0.2596684541       -0.12871280
    ## SUZB5.SA.Adjusted       -0.58467231     -0.7534878988       -0.38583629
    ## TBLE3.SA.Adjusted        0.25339502      0.3947941424        0.35548504
    ## TIMP3.SA.Adjusted        0.16371388     -0.0207376531        0.06017962
    ## UGPA3.SA.Adjusted        0.05845545     -0.2381219642        0.28523111
    ## USIM5.SA.Adjusted        0.68980561      0.0639161305        0.77909403
    ## VALE3.SA.Adjusted        0.33236075     -0.2859805427        0.62677106
    ## VALE5.SA.Adjusted        0.31326483     -0.3588229823        0.60889533
    ## VIVT4.SA.Adjusted        0.79045906      0.4025257078        0.65074739
    ##                   CCRO3.SA.Adjusted CESP6.SA.Adjusted CIEL3.SA.Adjusted
    ## USD.BRL                -0.511495825       -0.63979011       0.314515856
    ## ABEV3.SA.Adjusted       0.098482504        0.10785377       0.240220589
    ## BBAS3.SA.Adjusted       0.533888999        0.59156295      -0.245202472
    ## BBDC3.SA.Adjusted       0.695886587        0.77868001      -0.554872662
    ## BBDC4.SA.Adjusted       0.711604839        0.71493411      -0.429490120
    ## BBSE3.SA.Adjusted       0.554481341        0.68804655      -0.325739129
    ## BRAP4.SA.Adjusted       0.312934610        0.29447249      -0.230055497
    ## BRFS3.SA.Adjusted      -0.612895930       -0.65558258       0.508184302
    ## BRKM5.SA.Adjusted      -0.202348716       -0.16073504       0.311741907
    ## BRML3.SA.Adjusted       0.777649214        0.87943447      -0.722411930
    ## BRPR3.SA.Adjusted       0.473789881        0.58053317      -0.564066298
    ## BVMF3.SA.Adjusted       0.632409342        0.61682198      -0.233845165
    ## CCRO3.SA.Adjusted       1.000000000        0.65846613      -0.507484615
    ## CESP6.SA.Adjusted       0.658466134        1.00000000      -0.644501466
    ## CIEL3.SA.Adjusted      -0.507484615       -0.64450147       1.000000000
    ## CMIG4.SA.Adjusted       0.500874447        0.59874824      -0.449382234
    ## CPFE3.SA.Adjusted       0.532607118        0.40715030      -0.341303019
    ## CPLE6.SA.Adjusted       0.197067956        0.16933180       0.198796452
    ## CRUZ3.SA.Adjusted       0.800755387        0.70504111      -0.499808890
    ## CSAN3.SA.Adjusted       0.669790352        0.75805256      -0.658721357
    ## CSNA3.SA.Adjusted       0.475526725        0.43231851      -0.271468885
    ## CTIP3.SA.Adjusted      -0.345772237       -0.52701071       0.734021262
    ## CYRE3.SA.Adjusted       0.783269081        0.85430973      -0.798080492
    ## DTEX3.SA.Adjusted       0.746719351        0.77703964      -0.611153422
    ## ECOR3.SA.Adjusted       0.729419710        0.78829265      -0.627336382
    ## ELET3.SA.Adjusted       0.413525403        0.36934234      -0.134823954
    ## ELET6.SA.Adjusted      -0.181878592       -0.24546005       0.318652050
    ## EMBR3.SA.Adjusted      -0.109976208       -0.14798780      -0.160046332
    ## ENBR3.SA.Adjusted      -0.344239188       -0.49302565       0.630646615
    ## ESTC3.SA.Adjusted       0.205323342        0.44968182      -0.442012200
    ## FIBR3.SA.Adjusted       0.028079633        0.11836132      -0.343821646
    ## GFSA3.SA.Adjusted       0.398525081        0.11662483       0.257952990
    ## GGBR4.SA.Adjusted       0.663190630        0.71364391      -0.618979719
    ## GOAU4.SA.Adjusted       0.714344020        0.80013035      -0.700751662
    ## GOLL4.SA.Adjusted       0.548111722        0.70518162      -0.586057908
    ## HGTX3.SA.Adjusted       0.841697403        0.71547164      -0.571149614
    ## HYPE3.SA.Adjusted      -0.405108614       -0.05899255       0.005076577
    ## ITSA4.SA.Adjusted       0.422698746        0.60674082      -0.471230755
    ## ITUB4.SA.Adjusted       0.617686106        0.71246161      -0.502369127
    ## JBSS3.SA.Adjusted      -0.151664987        0.10471217      -0.064901466
    ## KROT3.SA.Adjusted      -0.182040942        0.04785505      -0.142347505
    ## LAME4.SA.Adjusted      -0.103282094       -0.07713275       0.504068655
    ## LREN3.SA.Adjusted      -0.508207296       -0.23701899       0.232519256
    ## MRFG3.SA.Adjusted      -0.492378855       -0.47142175       0.523368327
    ## MRVE3.SA.Adjusted       0.679108522        0.68445512      -0.478594207
    ## MULT3.SA.Adjusted       0.765299889        0.85661286      -0.696398037
    ## NATU3.SA.Adjusted       0.222517821        0.16403861      -0.150316258
    ## OIBR4.SA.Adjusted       0.362327610        0.53228322      -0.558705335
    ## PCAR4.SA.Adjusted       0.751268595        0.72812912      -0.644997875
    ## PETR3.SA.Adjusted       0.007110187        0.06717428       0.233490026
    ## PETR4.SA.Adjusted       0.225000937        0.33904458      -0.029704583
    ## POMO4.SA.Adjusted       0.301894327        0.28728333      -0.095190773
    ## QUAL3.SA.Adjusted       0.633931455        0.52034514      -0.241539430
    ## RENT3.SA.Adjusted       0.755429669        0.86974869      -0.746627083
    ## RUMO3.SA.Adjusted       0.676072114        0.47057370      -0.385697432
    ## SBSP3.SA.Adjusted       0.384490645        0.18915241      -0.075539111
    ## SMLE3.SA.Adjusted       0.032250351       -0.09387496       0.404369875
    ## SUZB5.SA.Adjusted      -0.603110882       -0.54617473       0.400616656
    ## TBLE3.SA.Adjusted       0.436926805        0.23796602       0.068422678
    ## TIMP3.SA.Adjusted       0.086889771        0.21945498      -0.249948509
    ## UGPA3.SA.Adjusted      -0.017497410       -0.09231221       0.109473968
    ## USIM5.SA.Adjusted       0.589685091        0.52251152      -0.327653330
    ## VALE3.SA.Adjusted       0.306826600        0.18368339      -0.089729494
    ## VALE5.SA.Adjusted       0.221275817        0.18099651      -0.106465839
    ## VIVT4.SA.Adjusted       0.734675670        0.59986848      -0.462667309
    ##                   CMIG4.SA.Adjusted CPFE3.SA.Adjusted CPLE6.SA.Adjusted
    ## USD.BRL                 -0.79002974       -0.51930381      -0.026570726
    ## ABEV3.SA.Adjusted       -0.21864496        0.28639279       0.727312732
    ## BBAS3.SA.Adjusted        0.72173340        0.63347231       0.277850275
    ## BBDC3.SA.Adjusted        0.40056062        0.44937623       0.398048367
    ## BBDC4.SA.Adjusted        0.68533018        0.64601698       0.279710965
    ## BBSE3.SA.Adjusted        0.80085891        0.62352551       0.187346631
    ## BRAP4.SA.Adjusted        0.79148460        0.54830063      -0.279849625
    ## BRFS3.SA.Adjusted       -0.70313462       -0.31253677       0.103319785
    ## BRKM5.SA.Adjusted        0.43914767        0.37597501      -0.078972625
    ## BRML3.SA.Adjusted        0.72931028        0.54449063       0.099638628
    ## BRPR3.SA.Adjusted       -0.01019876        0.18776201       0.308012610
    ## BVMF3.SA.Adjusted        0.76940034        0.64847514       0.235183111
    ## CCRO3.SA.Adjusted        0.50087445        0.53260712       0.197067956
    ## CESP6.SA.Adjusted        0.59874824        0.40715030       0.169331796
    ## CIEL3.SA.Adjusted       -0.44938223       -0.34130302       0.198796452
    ## CMIG4.SA.Adjusted        1.00000000        0.68297161      -0.179624582
    ## CPFE3.SA.Adjusted        0.68297161        1.00000000       0.251778181
    ## CPLE6.SA.Adjusted       -0.17962458        0.25177818       1.000000000
    ## CRUZ3.SA.Adjusted        0.59354579        0.53203276       0.162171327
    ## CSAN3.SA.Adjusted        0.85269478        0.53634828      -0.088863573
    ## CSNA3.SA.Adjusted        0.91163628        0.68962363      -0.115506401
    ## CTIP3.SA.Adjusted       -0.51606613       -0.20510593       0.399347221
    ## CYRE3.SA.Adjusted        0.61309395        0.44676168       0.041004712
    ## DTEX3.SA.Adjusted        0.85080460        0.67922372       0.006178751
    ## ECOR3.SA.Adjusted        0.81408005        0.57555673      -0.046001123
    ## ELET3.SA.Adjusted        0.82516191        0.67237893      -0.053444136
    ## ELET6.SA.Adjusted        0.46256977        0.37515823      -0.212117370
    ## EMBR3.SA.Adjusted       -0.09482600        0.05521899       0.077588184
    ## ENBR3.SA.Adjusted       -0.71013631       -0.25956643       0.432791626
    ## ESTC3.SA.Adjusted        0.37582721        0.14902816      -0.174152214
    ## FIBR3.SA.Adjusted        0.04170722       -0.10285190      -0.205501123
    ## GFSA3.SA.Adjusted        0.30776258        0.48669460       0.314308577
    ## GGBR4.SA.Adjusted        0.92891191        0.66343111      -0.136102431
    ## GOAU4.SA.Adjusted        0.86915764        0.58666619      -0.100548779
    ## GOLL4.SA.Adjusted        0.87631989        0.51505051      -0.178641935
    ## HGTX3.SA.Adjusted        0.58682828        0.48371263       0.138690418
    ## HYPE3.SA.Adjusted        0.10979954       -0.01608316      -0.100392875
    ## ITSA4.SA.Adjusted        0.64970112        0.70662768       0.318696928
    ## ITUB4.SA.Adjusted        0.84290112        0.60892915       0.054960946
    ## JBSS3.SA.Adjusted        0.49456937        0.34272741      -0.049384742
    ## KROT3.SA.Adjusted        0.32385344        0.12870060      -0.158531940
    ## LAME4.SA.Adjusted       -0.25713404        0.13266723       0.674462450
    ## LREN3.SA.Adjusted       -0.23787305       -0.40491415      -0.097657957
    ## MRFG3.SA.Adjusted       -0.78308138       -0.43478416       0.459450439
    ## MRVE3.SA.Adjusted        0.34562210        0.38045956       0.303396979
    ## MULT3.SA.Adjusted        0.67664061        0.51657477       0.121307173
    ## NATU3.SA.Adjusted        0.71682203        0.67692669      -0.092486767
    ## OIBR4.SA.Adjusted        0.75983099        0.31184657      -0.349014704
    ## PCAR4.SA.Adjusted        0.78452397        0.59700482      -0.033418206
    ## PETR3.SA.Adjusted        0.60483050        0.33032603      -0.120842461
    ## PETR4.SA.Adjusted        0.79786521        0.45533122      -0.142405642
    ## POMO4.SA.Adjusted        0.84043462        0.61077959      -0.213267195
    ## QUAL3.SA.Adjusted        0.65713099        0.67817380       0.237739315
    ## RENT3.SA.Adjusted        0.71475986        0.49234567      -0.016745715
    ## RUMO3.SA.Adjusted        0.33492675        0.42236682       0.132264010
    ## SBSP3.SA.Adjusted        0.43079613        0.62449437       0.105846599
    ## SMLE3.SA.Adjusted       -0.57557462       -0.16368390       0.548245731
    ## SUZB5.SA.Adjusted       -0.13875702       -0.35542031      -0.353616213
    ## TBLE3.SA.Adjusted        0.11753790        0.58736429       0.636144650
    ## TIMP3.SA.Adjusted       -0.11127129       -0.32177447      -0.017006048
    ## UGPA3.SA.Adjusted        0.56463807        0.57685311      -0.137533016
    ## USIM5.SA.Adjusted        0.87025364        0.66122431      -0.056357349
    ## VALE3.SA.Adjusted        0.72752604        0.57215871      -0.195190690
    ## VALE5.SA.Adjusted        0.76819886        0.53693372      -0.288360273
    ## VIVT4.SA.Adjusted        0.60683723        0.55716766       0.195707895
    ##                   CRUZ3.SA.Adjusted CSAN3.SA.Adjusted CSNA3.SA.Adjusted
    ## USD.BRL                -0.659017854       -0.83632973       -0.76977913
    ## ABEV3.SA.Adjusted       0.168217841       -0.18870133       -0.12139691
    ## BBAS3.SA.Adjusted       0.728311852        0.73108426        0.82274443
    ## BBDC3.SA.Adjusted       0.804647084        0.69590114        0.41058059
    ## BBDC4.SA.Adjusted       0.844232328        0.81812279        0.74973043
    ## BBSE3.SA.Adjusted       0.649660174        0.79661043        0.75676372
    ## BRAP4.SA.Adjusted       0.504912123        0.63199132        0.86197101
    ## BRFS3.SA.Adjusted      -0.656581686       -0.80868980       -0.67549625
    ## BRKM5.SA.Adjusted      -0.061531289        0.09285943        0.46375479
    ## BRML3.SA.Adjusted       0.796952728        0.92249081        0.61728907
    ## BRPR3.SA.Adjusted       0.420925648        0.27607926       -0.09370699
    ## BVMF3.SA.Adjusted       0.699172427        0.76522837        0.77487134
    ## CCRO3.SA.Adjusted       0.800755387        0.66979035        0.47552673
    ## CESP6.SA.Adjusted       0.705041106        0.75805256        0.43231851
    ## CIEL3.SA.Adjusted      -0.499808890       -0.65872136       -0.27146889
    ## CMIG4.SA.Adjusted       0.593545788        0.85269478        0.91163628
    ## CPFE3.SA.Adjusted       0.532032762        0.53634828        0.68962363
    ## CPLE6.SA.Adjusted       0.162171327       -0.08886357       -0.11550640
    ## CRUZ3.SA.Adjusted       1.000000000        0.78082398        0.64543235
    ## CSAN3.SA.Adjusted       0.780823976        1.00000000        0.77162429
    ## CSNA3.SA.Adjusted       0.645432347        0.77162429        1.00000000
    ## CTIP3.SA.Adjusted      -0.488827114       -0.64723967       -0.37213753
    ## CYRE3.SA.Adjusted       0.798517644        0.85717329        0.51127917
    ## DTEX3.SA.Adjusted       0.860424615        0.89916961        0.81561498
    ## ECOR3.SA.Adjusted       0.827111058        0.94617212        0.73650151
    ## ELET3.SA.Adjusted       0.576527574        0.61064508        0.94124298
    ## ELET6.SA.Adjusted      -0.132746824        0.01318182        0.52849644
    ## EMBR3.SA.Adjusted      -0.264260425       -0.11008816       -0.24254441
    ## ENBR3.SA.Adjusted      -0.341353691       -0.72291073       -0.45495850
    ## ESTC3.SA.Adjusted       0.309474699        0.51629173        0.18228231
    ## FIBR3.SA.Adjusted      -0.006093549        0.11232723       -0.04616102
    ## GFSA3.SA.Adjusted       0.426659342        0.19897003        0.52751803
    ## GGBR4.SA.Adjusted       0.775436018        0.94551326        0.88674937
    ## GOAU4.SA.Adjusted       0.812924432        0.96549835        0.79106502
    ## GOLL4.SA.Adjusted       0.642387171        0.92050543        0.75378859
    ## HGTX3.SA.Adjusted       0.901776708        0.79666704        0.63536131
    ## HYPE3.SA.Adjusted      -0.334355069       -0.01771153       -0.09913873
    ## ITSA4.SA.Adjusted       0.497483761        0.59660931        0.54250667
    ## ITUB4.SA.Adjusted       0.767120091        0.90754637        0.83163752
    ## JBSS3.SA.Adjusted      -0.098549365        0.24247430        0.31632595
    ## KROT3.SA.Adjusted      -0.154871049        0.26696326        0.14543218
    ## LAME4.SA.Adjusted      -0.109613488       -0.29070597       -0.16827196
    ## LREN3.SA.Adjusted      -0.360150503       -0.21664906       -0.28208124
    ## MRFG3.SA.Adjusted      -0.504162261       -0.70328653       -0.68939041
    ## MRVE3.SA.Adjusted       0.504677230        0.57551143        0.21466362
    ## MULT3.SA.Adjusted       0.822601863        0.88649974        0.59656897
    ## NATU3.SA.Adjusted       0.292463570        0.41004720        0.67556374
    ## OIBR4.SA.Adjusted       0.307112088        0.72548649        0.51306721
    ## PCAR4.SA.Adjusted       0.877135462        0.90355421        0.78634704
    ## PETR3.SA.Adjusted       0.191153289        0.39330950        0.66338942
    ## PETR4.SA.Adjusted       0.410827910        0.64649336        0.79465063
    ## POMO4.SA.Adjusted       0.366330579        0.58558927        0.86692739
    ## QUAL3.SA.Adjusted       0.647870092        0.62392093        0.72746126
    ## RENT3.SA.Adjusted       0.829733992        0.92290166        0.59810607
    ## RUMO3.SA.Adjusted       0.492868204        0.38096590        0.30356485
    ## SBSP3.SA.Adjusted       0.292247819        0.20351988        0.47688224
    ## SMLE3.SA.Adjusted      -0.095159525       -0.47689928       -0.46019322
    ## SUZB5.SA.Adjusted      -0.605343724       -0.39159835       -0.16775399
    ## TBLE3.SA.Adjusted       0.368075235        0.06551147        0.24007869
    ## TIMP3.SA.Adjusted       0.029252529        0.16710312       -0.26417465
    ## UGPA3.SA.Adjusted       0.055910361        0.20877895        0.63324960
    ## USIM5.SA.Adjusted       0.735607046        0.79096405        0.96515298
    ## VALE3.SA.Adjusted       0.504133475        0.53925509        0.88610487
    ## VALE5.SA.Adjusted       0.413670204        0.55330074        0.86975881
    ## VIVT4.SA.Adjusted       0.808182529        0.74661300        0.69269303
    ##                   CTIP3.SA.Adjusted CYRE3.SA.Adjusted DTEX3.SA.Adjusted
    ## USD.BRL                  0.33659013       -0.59738911      -0.772026500
    ## ABEV3.SA.Adjusted        0.39612842       -0.05022031      -0.003784701
    ## BBAS3.SA.Adjusted       -0.23775963        0.60029772       0.794414486
    ## BBDC3.SA.Adjusted       -0.31763145        0.85682835       0.724422602
    ## BBDC4.SA.Adjusted       -0.31383874        0.79907791       0.868256794
    ## BBSE3.SA.Adjusted       -0.35701443        0.64260411       0.790543889
    ## BRAP4.SA.Adjusted       -0.53720750        0.31244142       0.651214136
    ## BRFS3.SA.Adjusted        0.44327484       -0.79729748      -0.775692963
    ## BRKM5.SA.Adjusted        0.10869525       -0.29944460       0.136509995
    ## BRML3.SA.Adjusted       -0.53604624        0.95613858       0.896035689
    ## BRPR3.SA.Adjusted       -0.20493197        0.64261607       0.363356937
    ## BVMF3.SA.Adjusted       -0.26004328        0.55839527       0.793230753
    ## CCRO3.SA.Adjusted       -0.34577224        0.78326908       0.746719351
    ## CESP6.SA.Adjusted       -0.52701071        0.85430973       0.777039637
    ## CIEL3.SA.Adjusted        0.73402126       -0.79808049      -0.611153422
    ## CMIG4.SA.Adjusted       -0.51606613        0.61309395       0.850804601
    ## CPFE3.SA.Adjusted       -0.20510593        0.44676168       0.679223723
    ## CPLE6.SA.Adjusted        0.39934722        0.04100471       0.006178751
    ## CRUZ3.SA.Adjusted       -0.48882711        0.79851764       0.860424615
    ## CSAN3.SA.Adjusted       -0.64723967        0.85717329       0.899169609
    ## CSNA3.SA.Adjusted       -0.37213753        0.51127917       0.815614980
    ## CTIP3.SA.Adjusted        1.00000000       -0.59011635      -0.548805374
    ## CYRE3.SA.Adjusted       -0.59011635        1.00000000       0.851410245
    ## DTEX3.SA.Adjusted       -0.54880537        0.85141025       1.000000000
    ## ECOR3.SA.Adjusted       -0.64394535        0.84550320       0.901320045
    ## ELET3.SA.Adjusted       -0.29064311        0.35810003       0.725990317
    ## ELET6.SA.Adjusted        0.04681054       -0.35869391       0.091677856
    ## EMBR3.SA.Adjusted       -0.01805089       -0.10732473      -0.184479591
    ## ENBR3.SA.Adjusted        0.68063598       -0.54796915      -0.580671068
    ## ESTC3.SA.Adjusted       -0.57495015        0.33954976       0.382065461
    ## FIBR3.SA.Adjusted       -0.30976838        0.27137199       0.027114199
    ## GFSA3.SA.Adjusted        0.31886048        0.17576809       0.413000197
    ## GGBR4.SA.Adjusted       -0.60467580        0.81655596       0.952766376
    ## GOAU4.SA.Adjusted       -0.67508146        0.89740502       0.949341785
    ## GOLL4.SA.Adjusted       -0.68573931        0.72220268       0.815923288
    ## HGTX3.SA.Adjusted       -0.50919493        0.85558328       0.833627976
    ## HYPE3.SA.Adjusted       -0.13593230       -0.26308853      -0.193102951
    ## ITSA4.SA.Adjusted       -0.26549855        0.55407607       0.667402202
    ## ITUB4.SA.Adjusted       -0.46442345        0.77465346       0.882008096
    ## JBSS3.SA.Adjusted       -0.04835388       -0.05133060       0.146863671
    ## KROT3.SA.Adjusted       -0.18832891       -0.06808916       0.050792330
    ## LAME4.SA.Adjusted        0.70615997       -0.26001950      -0.186075518
    ## LREN3.SA.Adjusted       -0.02700435       -0.40820332      -0.424563092
    ## MRFG3.SA.Adjusted        0.52488548       -0.61099260      -0.745993004
    ## MRVE3.SA.Adjusted       -0.17452866        0.70241952       0.546674731
    ## MULT3.SA.Adjusted       -0.49668593        0.96591411       0.882667881
    ## NATU3.SA.Adjusted       -0.28503813        0.14845072       0.520139651
    ## OIBR4.SA.Adjusted       -0.66109408        0.52406575       0.556410152
    ## PCAR4.SA.Adjusted       -0.58246570        0.88442154       0.934460946
    ## PETR3.SA.Adjusted       -0.15245180       -0.06168459       0.317526885
    ## PETR4.SA.Adjusted       -0.35627564        0.24488303       0.568578789
    ## POMO4.SA.Adjusted       -0.16480074        0.29660726       0.632146813
    ## QUAL3.SA.Adjusted        0.02180183        0.60437963       0.768074005
    ## RENT3.SA.Adjusted       -0.63602711        0.95181346       0.899776153
    ## RUMO3.SA.Adjusted       -0.06646466        0.61767917       0.559953005
    ## SBSP3.SA.Adjusted        0.15352546        0.26565751       0.470414165
    ## SMLE3.SA.Adjusted        0.56899237       -0.22767483      -0.318773114
    ## SUZB5.SA.Adjusted        0.07304083       -0.62022379      -0.529705002
    ## TBLE3.SA.Adjusted        0.37361617        0.21380464       0.334130841
    ## TIMP3.SA.Adjusted       -0.36832542        0.15595752      -0.083594232
    ## UGPA3.SA.Adjusted        0.10239282       -0.05281390       0.295917563
    ## USIM5.SA.Adjusted       -0.41032195        0.61059392       0.862215237
    ## VALE3.SA.Adjusted       -0.34243073        0.24098961       0.619323402
    ## VALE5.SA.Adjusted       -0.39720806        0.20064932       0.585558271
    ## VIVT4.SA.Adjusted       -0.28377804        0.79070183       0.793483959
    ##                   ECOR3.SA.Adjusted ELET3.SA.Adjusted ELET6.SA.Adjusted
    ## USD.BRL                 -0.79825774       -0.68780702       -0.19106878
    ## ABEV3.SA.Adjusted       -0.09007460       -0.01865520       -0.15323320
    ## BBAS3.SA.Adjusted        0.72949828        0.78366586        0.20061928
    ## BBDC3.SA.Adjusted        0.70579135        0.29712342       -0.44889352
    ## BBDC4.SA.Adjusted        0.80732053        0.65175488       -0.04990403
    ## BBSE3.SA.Adjusted        0.81415392        0.67869976        0.19801395
    ## BRAP4.SA.Adjusted        0.62372933        0.88076177        0.64084553
    ## BRFS3.SA.Adjusted       -0.75850789       -0.54085853        0.05433998
    ## BRKM5.SA.Adjusted        0.08753283        0.48901363        0.77163206
    ## BRML3.SA.Adjusted        0.90068387        0.46365680       -0.20860795
    ## BRPR3.SA.Adjusted        0.30346512       -0.16939457       -0.63992966
    ## BVMF3.SA.Adjusted        0.76980379        0.72940900        0.26744102
    ## CCRO3.SA.Adjusted        0.72941971        0.41352540       -0.18187859
    ## CESP6.SA.Adjusted        0.78829265        0.36934234       -0.24546005
    ## CIEL3.SA.Adjusted       -0.62733638       -0.13482395        0.31865205
    ## CMIG4.SA.Adjusted        0.81408005        0.82516191        0.46256977
    ## CPFE3.SA.Adjusted        0.57555673        0.67237893        0.37515823
    ## CPLE6.SA.Adjusted       -0.04600112       -0.05344414       -0.21211737
    ## CRUZ3.SA.Adjusted        0.82711106        0.57652757       -0.13274682
    ## CSAN3.SA.Adjusted        0.94617212        0.61064508        0.01318182
    ## CSNA3.SA.Adjusted        0.73650151        0.94124298        0.52849644
    ## CTIP3.SA.Adjusted       -0.64394535       -0.29064311        0.04681054
    ## CYRE3.SA.Adjusted        0.84550320        0.35810003       -0.35869391
    ## DTEX3.SA.Adjusted        0.90132004        0.72599032        0.09167786
    ## ECOR3.SA.Adjusted        1.00000000        0.61926114        0.01162841
    ## ELET3.SA.Adjusted        0.61926114        1.00000000        0.65759052
    ## ELET6.SA.Adjusted        0.01162841        0.65759052        1.00000000
    ## EMBR3.SA.Adjusted       -0.16534030       -0.30802690       -0.03309110
    ## ENBR3.SA.Adjusted       -0.67941426       -0.29361915       -0.10638776
    ## ESTC3.SA.Adjusted        0.49818047        0.11254966       -0.04284901
    ## FIBR3.SA.Adjusted        0.06617449       -0.14640802       -0.22408598
    ## GFSA3.SA.Adjusted        0.22754103        0.55852362        0.25611847
    ## GGBR4.SA.Adjusted        0.91384661        0.76510261        0.19092536
    ## GOAU4.SA.Adjusted        0.94520389        0.65765875        0.02215633
    ## GOLL4.SA.Adjusted        0.88400597        0.62558087        0.19465734
    ## HGTX3.SA.Adjusted        0.81199608        0.56212555       -0.17249451
    ## HYPE3.SA.Adjusted       -0.05841629       -0.10625212        0.27872436
    ## ITSA4.SA.Adjusted        0.60614904        0.50014096        0.16039834
    ## ITUB4.SA.Adjusted        0.88025370        0.71734460        0.10458581
    ## JBSS3.SA.Adjusted        0.18514306        0.27319685        0.50047553
    ## KROT3.SA.Adjusted        0.19697880        0.09464489        0.32777147
    ## LAME4.SA.Adjusted       -0.22629799       -0.11537475       -0.03601591
    ## LREN3.SA.Adjusted       -0.20296482       -0.25173084        0.03840140
    ## MRFG3.SA.Adjusted       -0.67642641       -0.57456703       -0.24309488
    ## MRVE3.SA.Adjusted        0.58357553        0.09437536       -0.35031726
    ## MULT3.SA.Adjusted        0.86300376        0.43715195       -0.27500074
    ## NATU3.SA.Adjusted        0.44525483        0.69930901        0.70516736
    ## OIBR4.SA.Adjusted        0.65725629        0.37959374        0.24198215
    ## PCAR4.SA.Adjusted        0.87708201        0.66785416       -0.01398455
    ## PETR3.SA.Adjusted        0.37955767        0.67844767        0.70515288
    ## PETR4.SA.Adjusted        0.63143725        0.74706775        0.58010519
    ## POMO4.SA.Adjusted        0.52766378        0.82019291        0.66441372
    ## QUAL3.SA.Adjusted        0.63278884        0.64675381        0.13150964
    ## RENT3.SA.Adjusted        0.92096592        0.44436595       -0.23067130
    ## RUMO3.SA.Adjusted        0.40078666        0.23154754       -0.19096214
    ## SBSP3.SA.Adjusted        0.21077600        0.47041898        0.30868455
    ## SMLE3.SA.Adjusted       -0.38506519       -0.33727536       -0.31641397
    ## SUZB5.SA.Adjusted       -0.42029783       -0.12143935        0.45296063
    ## TBLE3.SA.Adjusted        0.12578209        0.29503812        0.02642924
    ## TIMP3.SA.Adjusted        0.18671664       -0.30386292       -0.35794101
    ## UGPA3.SA.Adjusted        0.16144355        0.62615150        0.74970707
    ## USIM5.SA.Adjusted        0.76219235        0.91386137        0.40304597
    ## VALE3.SA.Adjusted        0.52776040        0.91820700        0.68136521
    ## VALE5.SA.Adjusted        0.53462120        0.89238366        0.73455777
    ## VIVT4.SA.Adjusted        0.74200282        0.58503001       -0.11607385
    ##                   EMBR3.SA.Adjusted ENBR3.SA.Adjusted ESTC3.SA.Adjusted
    ## USD.BRL                0.2736554508       0.539702192      -0.504366750
    ## ABEV3.SA.Adjusted     -0.0437537398       0.491037164      -0.160200398
    ## BBAS3.SA.Adjusted     -0.3669929243      -0.255260600       0.134626689
    ## BBDC3.SA.Adjusted     -0.1822607326      -0.181431344       0.219375876
    ## BBDC4.SA.Adjusted     -0.2829410974      -0.286689840       0.212443761
    ## BBSE3.SA.Adjusted     -0.2489071016      -0.479985581       0.256072706
    ## BRAP4.SA.Adjusted     -0.2052957756      -0.456390260       0.313384748
    ## BRFS3.SA.Adjusted      0.2654042692       0.504724340      -0.197456523
    ## BRKM5.SA.Adjusted     -0.0113701804      -0.228157362       0.077817079
    ## BRML3.SA.Adjusted     -0.0652447893      -0.613345631       0.429071891
    ## BRPR3.SA.Adjusted      0.0760864015       0.012647270       0.076184365
    ## BVMF3.SA.Adjusted     -0.1759615681      -0.460466729       0.430278008
    ## CCRO3.SA.Adjusted     -0.1099762083      -0.344239188       0.205323342
    ## CESP6.SA.Adjusted     -0.1479878041      -0.493025653       0.449681819
    ## CIEL3.SA.Adjusted     -0.1600463321       0.630646615      -0.442012200
    ## CMIG4.SA.Adjusted     -0.0948260044      -0.710136310       0.375827211
    ## CPFE3.SA.Adjusted      0.0552189883      -0.259566425       0.149028161
    ## CPLE6.SA.Adjusted      0.0775881836       0.432791626      -0.174152214
    ## CRUZ3.SA.Adjusted     -0.2642604254      -0.341353691       0.309474699
    ## CSAN3.SA.Adjusted     -0.1100881557      -0.722910734       0.516291729
    ## CSNA3.SA.Adjusted     -0.2425444108      -0.454958503       0.182282315
    ## CTIP3.SA.Adjusted     -0.0180508898       0.680635978      -0.574950148
    ## CYRE3.SA.Adjusted     -0.1073247252      -0.547969149       0.339549756
    ## DTEX3.SA.Adjusted     -0.1844795907      -0.580671068       0.382065461
    ## ECOR3.SA.Adjusted     -0.1653403005      -0.679414263       0.498180469
    ## ELET3.SA.Adjusted     -0.3080269037      -0.293619155       0.112549659
    ## ELET6.SA.Adjusted     -0.0330910963      -0.106387763      -0.042849008
    ## EMBR3.SA.Adjusted      1.0000000000      -0.149888331       0.252503107
    ## ENBR3.SA.Adjusted     -0.1498883307       1.000000000      -0.643917622
    ## ESTC3.SA.Adjusted      0.2525031072      -0.643917622       1.000000000
    ## FIBR3.SA.Adjusted     -0.0005136407      -0.092367627      -0.202028788
    ## GFSA3.SA.Adjusted     -0.2932871408       0.293791282      -0.294279581
    ## GGBR4.SA.Adjusted     -0.1458598314      -0.672677849       0.380497469
    ## GOAU4.SA.Adjusted     -0.1270797223      -0.696562699       0.440941821
    ## GOLL4.SA.Adjusted     -0.0676851130      -0.779087867       0.593640894
    ## HGTX3.SA.Adjusted     -0.2017821674      -0.348825024       0.271384176
    ## HYPE3.SA.Adjusted      0.3490846249      -0.327503048       0.369243807
    ## ITSA4.SA.Adjusted      0.1401622294      -0.414629235       0.266141253
    ## ITUB4.SA.Adjusted     -0.2329664920      -0.563518620       0.334579833
    ## JBSS3.SA.Adjusted      0.2848500257      -0.455116601       0.303074374
    ## KROT3.SA.Adjusted      0.2965998909      -0.563496278       0.613772190
    ## LAME4.SA.Adjusted      0.0322476430       0.479567343      -0.194948464
    ## LREN3.SA.Adjusted      0.0609827285      -0.020086682       0.238008127
    ## MRFG3.SA.Adjusted      0.0961716307       0.691077286      -0.300164043
    ## MRVE3.SA.Adjusted      0.0034298181      -0.346985179       0.318234695
    ## MULT3.SA.Adjusted     -0.1162198236      -0.510506439       0.315993797
    ## NATU3.SA.Adjusted      0.0489636009      -0.463162914       0.223933982
    ## OIBR4.SA.Adjusted      0.1756068015      -0.882107986       0.565110928
    ## PCAR4.SA.Adjusted     -0.1602829300      -0.506384851       0.324555566
    ## PETR3.SA.Adjusted     -0.2047241349      -0.329740471       0.293400292
    ## PETR4.SA.Adjusted     -0.1911331850      -0.529105773       0.402938710
    ## POMO4.SA.Adjusted     -0.1710861063      -0.418157394       0.133025996
    ## QUAL3.SA.Adjusted     -0.2010371636      -0.215912041       0.002967298
    ## RENT3.SA.Adjusted     -0.0941009136      -0.642003342       0.516682281
    ## RUMO3.SA.Adjusted     -0.0670469182      -0.173849200      -0.109073261
    ## SBSP3.SA.Adjusted     -0.0356220892      -0.003664360      -0.235976079
    ## SMLE3.SA.Adjusted     -0.1128294277       0.650806015      -0.304846721
    ## SUZB5.SA.Adjusted      0.0188397240      -0.002097329      -0.227324488
    ## TBLE3.SA.Adjusted     -0.0669257927       0.355787126      -0.275984516
    ## TIMP3.SA.Adjusted      0.0453324514      -0.311989462       0.417839038
    ## UGPA3.SA.Adjusted      0.0400137876      -0.123274294      -0.129331843
    ## USIM5.SA.Adjusted     -0.2712665787      -0.404628492       0.183043862
    ## VALE3.SA.Adjusted     -0.2586899599      -0.273549577       0.144310384
    ## VALE5.SA.Adjusted     -0.2230859601      -0.389918915       0.227994191
    ## VIVT4.SA.Adjusted     -0.2945322704      -0.262154284       0.015951154
    ##                   FIBR3.SA.Adjusted GFSA3.SA.Adjusted GGBR4.SA.Adjusted
    ## USD.BRL                0.2460337939        -0.3317357       -0.79717369
    ## ABEV3.SA.Adjusted     -0.2362261555         0.3784222       -0.17586404
    ## BBAS3.SA.Adjusted     -0.1104694590         0.5600607        0.78347102
    ## BBDC3.SA.Adjusted      0.1142449611         0.3478743        0.63738112
    ## BBDC4.SA.Adjusted      0.0062870104         0.5529279        0.82930854
    ## BBSE3.SA.Adjusted      0.0100746979         0.3807336        0.80833390
    ## BRAP4.SA.Adjusted     -0.0551939444         0.2768071        0.74179993
    ## BRFS3.SA.Adjusted     -0.1920293378        -0.3016343       -0.81040259
    ## BRKM5.SA.Adjusted     -0.2757188040         0.1970917        0.20907605
    ## BRML3.SA.Adjusted      0.1206643769         0.2267448        0.87773972
    ## BRPR3.SA.Adjusted      0.2702041177         0.1244939        0.22900672
    ## BVMF3.SA.Adjusted     -0.3481516428         0.4650502        0.77341682
    ## CCRO3.SA.Adjusted      0.0280796327         0.3985251        0.66319063
    ## CESP6.SA.Adjusted      0.1183613193         0.1166248        0.71364391
    ## CIEL3.SA.Adjusted     -0.3438216457         0.2579530       -0.61897972
    ## CMIG4.SA.Adjusted      0.0417072197         0.3077626        0.92891191
    ## CPFE3.SA.Adjusted     -0.1028519020         0.4866946        0.66343111
    ## CPLE6.SA.Adjusted     -0.2055011231         0.3143086       -0.13610243
    ## CRUZ3.SA.Adjusted     -0.0060935486         0.4266593        0.77543602
    ## CSAN3.SA.Adjusted      0.1123272250         0.1989700        0.94551326
    ## CSNA3.SA.Adjusted     -0.0461610229         0.5275180        0.88674937
    ## CTIP3.SA.Adjusted     -0.3097683845         0.3188605       -0.60467580
    ## CYRE3.SA.Adjusted      0.2713719894         0.1757681        0.81655596
    ## DTEX3.SA.Adjusted      0.0271141992         0.4130002        0.95276638
    ## ECOR3.SA.Adjusted      0.0661744907         0.2275410        0.91384661
    ## ELET3.SA.Adjusted     -0.1464080245         0.5585236        0.76510261
    ## ELET6.SA.Adjusted     -0.2240859850         0.2561185        0.19092536
    ## EMBR3.SA.Adjusted     -0.0005136407        -0.2932871       -0.14585983
    ## ENBR3.SA.Adjusted     -0.0923676265         0.2937913       -0.67267785
    ## ESTC3.SA.Adjusted     -0.2020287883        -0.2942796        0.38049747
    ## FIBR3.SA.Adjusted      1.0000000000        -0.1667353        0.10528397
    ## GFSA3.SA.Adjusted     -0.1667352692         1.0000000        0.33011553
    ## GGBR4.SA.Adjusted      0.1052839712         0.3301155        1.00000000
    ## GOAU4.SA.Adjusted      0.1567595341         0.2326102        0.97741583
    ## GOLL4.SA.Adjusted      0.0878302037         0.1081695        0.89223558
    ## HGTX3.SA.Adjusted      0.0304998989         0.4070822        0.78581152
    ## HYPE3.SA.Adjusted     -0.0711114834        -0.5180190       -0.09341730
    ## ITSA4.SA.Adjusted     -0.0359121709         0.1902535        0.64189254
    ## ITUB4.SA.Adjusted     -0.0082044703         0.3146938        0.91531665
    ## JBSS3.SA.Adjusted     -0.0599248586        -0.1202671        0.25749247
    ## KROT3.SA.Adjusted     -0.2418289706        -0.4007381        0.16126032
    ## LAME4.SA.Adjusted     -0.4267040437         0.3478367       -0.29888173
    ## LREN3.SA.Adjusted     -0.1846295672        -0.6041573       -0.32794231
    ## MRFG3.SA.Adjusted     -0.2109658405        -0.2320130       -0.79249310
    ## MRVE3.SA.Adjusted      0.0009262426         0.2021589        0.47858177
    ## MULT3.SA.Adjusted      0.2134007889         0.3051869        0.85139129
    ## NATU3.SA.Adjusted     -0.1300354281         0.2521721        0.54884223
    ## OIBR4.SA.Adjusted      0.1924026168        -0.2053269        0.68133541
    ## PCAR4.SA.Adjusted      0.1783031527         0.4172463        0.92815364
    ## PETR3.SA.Adjusted     -0.2754443212         0.2552905        0.43467250
    ## PETR4.SA.Adjusted     -0.1381589578         0.2782789        0.67742560
    ## POMO4.SA.Adjusted     -0.0643288118         0.5493737        0.71652459
    ## QUAL3.SA.Adjusted     -0.1149689230         0.7037301        0.71367829
    ## RENT3.SA.Adjusted      0.1837640743         0.1735962        0.87717525
    ## RUMO3.SA.Adjusted      0.1266593010         0.4794676        0.45714121
    ## SBSP3.SA.Adjusted      0.0430732421         0.7053228        0.39818263
    ## SMLE3.SA.Adjusted     -0.2884019954         0.2589613       -0.49272396
    ## SUZB5.SA.Adjusted      0.2204927428        -0.4170785       -0.36321084
    ## TBLE3.SA.Adjusted     -0.1593146277         0.7388661        0.17529788
    ## TIMP3.SA.Adjusted      0.0014353292        -0.6253515       -0.04457275
    ## UGPA3.SA.Adjusted     -0.0085249228         0.5082563        0.38926824
    ## USIM5.SA.Adjusted     -0.0176528598         0.5796047        0.89785643
    ## VALE3.SA.Adjusted     -0.1247205785         0.4702349        0.68589084
    ## VALE5.SA.Adjusted     -0.1160106417         0.3422583        0.68371420
    ## VIVT4.SA.Adjusted      0.0801706992         0.5039153        0.77330872
    ##                   GOAU4.SA.Adjusted GOLL4.SA.Adjusted HGTX3.SA.Adjusted
    ## USD.BRL                 -0.77668194       -0.80691512      -0.617874928
    ## ABEV3.SA.Adjusted       -0.15512308       -0.26066189       0.019406551
    ## BBAS3.SA.Adjusted        0.74946601        0.64248981       0.696708448
    ## BBDC3.SA.Adjusted        0.71647838        0.51591016       0.807029311
    ## BBDC4.SA.Adjusted        0.83140329        0.69464343       0.834346362
    ## BBSE3.SA.Adjusted        0.80074854        0.76690738       0.593185974
    ## BRAP4.SA.Adjusted        0.65115689        0.71469755       0.466998372
    ## BRFS3.SA.Adjusted       -0.83136505       -0.74217960      -0.741215684
    ## BRKM5.SA.Adjusted        0.06821733        0.22664949      -0.249270352
    ## BRML3.SA.Adjusted        0.93261448        0.80985599       0.839663855
    ## BRPR3.SA.Adjusted        0.32964059        0.14072987       0.463359380
    ## BVMF3.SA.Adjusted        0.74288935        0.74259168       0.673801378
    ## CCRO3.SA.Adjusted        0.71434402        0.54811172       0.841697403
    ## CESP6.SA.Adjusted        0.80013035        0.70518162       0.715471645
    ## CIEL3.SA.Adjusted       -0.70075166       -0.58605791      -0.571149614
    ## CMIG4.SA.Adjusted        0.86915764        0.87631989       0.586828283
    ## CPFE3.SA.Adjusted        0.58666619        0.51505051       0.483712630
    ## CPLE6.SA.Adjusted       -0.10054878       -0.17864194       0.138690418
    ## CRUZ3.SA.Adjusted        0.81292443        0.64238717       0.901776708
    ## CSAN3.SA.Adjusted        0.96549835        0.92050543       0.796667041
    ## CSNA3.SA.Adjusted        0.79106502        0.75378859       0.635361310
    ## CTIP3.SA.Adjusted       -0.67508146       -0.68573931      -0.509194928
    ## CYRE3.SA.Adjusted        0.89740502        0.72220268       0.855583279
    ## DTEX3.SA.Adjusted        0.94934178        0.81592329       0.833627976
    ## ECOR3.SA.Adjusted        0.94520389        0.88400597       0.811996078
    ## ELET3.SA.Adjusted        0.65765875        0.62558087       0.562125553
    ## ELET6.SA.Adjusted        0.02215633        0.19465734      -0.172494507
    ## EMBR3.SA.Adjusted       -0.12707972       -0.06768511      -0.201782167
    ## ENBR3.SA.Adjusted       -0.69656270       -0.77908787      -0.348825024
    ## ESTC3.SA.Adjusted        0.44094182        0.59364089       0.271384176
    ## FIBR3.SA.Adjusted        0.15675953        0.08783020       0.030499899
    ## GFSA3.SA.Adjusted        0.23261019        0.10816955       0.407082221
    ## GGBR4.SA.Adjusted        0.97741583        0.89223558       0.785811517
    ## GOAU4.SA.Adjusted        1.00000000        0.89465980       0.832042483
    ## GOLL4.SA.Adjusted        0.89465980        1.00000000       0.646274159
    ## HGTX3.SA.Adjusted        0.83204248        0.64627416       1.000000000
    ## HYPE3.SA.Adjusted       -0.09498498        0.16460656      -0.467881502
    ## ITSA4.SA.Adjusted        0.63722979        0.57829380       0.437799212
    ## ITUB4.SA.Adjusted        0.91170522        0.81479494       0.758776782
    ## JBSS3.SA.Adjusted        0.19609804        0.34378549      -0.204270250
    ## KROT3.SA.Adjusted        0.13221980        0.40173233      -0.201391793
    ## LAME4.SA.Adjusted       -0.32455918       -0.34208794      -0.195979962
    ## LREN3.SA.Adjusted       -0.28700567       -0.10316353      -0.432036004
    ## MRFG3.SA.Adjusted       -0.74912461       -0.69856187      -0.532845012
    ## MRVE3.SA.Adjusted        0.54562763        0.50027649       0.568507957
    ## MULT3.SA.Adjusted        0.90775138        0.74141523       0.845447064
    ## NATU3.SA.Adjusted        0.45106081        0.49263387       0.216816202
    ## OIBR4.SA.Adjusted        0.69226727        0.84627233       0.340078359
    ## PCAR4.SA.Adjusted        0.93753064        0.79544456       0.898110831
    ## PETR3.SA.Adjusted        0.32821069        0.52747389       0.109446146
    ## PETR4.SA.Adjusted        0.60146046        0.74902369       0.335358066
    ## POMO4.SA.Adjusted        0.57927115        0.65428529       0.363051247
    ## QUAL3.SA.Adjusted        0.65679498        0.47965672       0.645611600
    ## RENT3.SA.Adjusted        0.94357434        0.82309275       0.824082961
    ## RUMO3.SA.Adjusted        0.46596472        0.23223982       0.581086918
    ## SBSP3.SA.Adjusted        0.29843965        0.16911298       0.284149649
    ## SMLE3.SA.Adjusted       -0.46395470       -0.50609665      -0.099017460
    ## SUZB5.SA.Adjusted       -0.41990710       -0.23741163      -0.628511843
    ## TBLE3.SA.Adjusted        0.12796012       -0.05435979       0.344470645
    ## TIMP3.SA.Adjusted        0.08495837        0.20275422       0.057188444
    ## UGPA3.SA.Adjusted        0.21750569        0.27430983       0.006281369
    ## USIM5.SA.Adjusted        0.82601742        0.74374667       0.766561706
    ## VALE3.SA.Adjusted        0.56350140        0.58792227       0.476733997
    ## VALE5.SA.Adjusted        0.56099780        0.64228614       0.376309664
    ## VIVT4.SA.Adjusted        0.78172036        0.56898989       0.840861705
    ##                   HYPE3.SA.Adjusted ITSA4.SA.Adjusted ITUB4.SA.Adjusted
    ## USD.BRL                -0.088649050       -0.55871750     -0.8572988038
    ## ABEV3.SA.Adjusted      -0.014604231        0.36258938     -0.0007558543
    ## BBAS3.SA.Adjusted      -0.125365719        0.66969466      0.8899841686
    ## BBDC3.SA.Adjusted      -0.318705952        0.58884817      0.7210419539
    ## BBDC4.SA.Adjusted      -0.285514349        0.66108761      0.8930600342
    ## BBSE3.SA.Adjusted       0.126365212        0.72690420      0.8738317189
    ## BRAP4.SA.Adjusted       0.110663173        0.39948672      0.6567252750
    ## BRFS3.SA.Adjusted       0.245977786       -0.39818952     -0.7670108825
    ## BRKM5.SA.Adjusted       0.442751873        0.29456681      0.2055232361
    ## BRML3.SA.Adjusted      -0.136255139        0.65111630      0.8649653285
    ## BRPR3.SA.Adjusted      -0.367901171        0.25378120      0.1613338734
    ## BVMF3.SA.Adjusted      -0.019293662        0.61161955      0.8259654208
    ## CCRO3.SA.Adjusted      -0.405108614        0.42269875      0.6176861056
    ## CESP6.SA.Adjusted      -0.058992551        0.60674082      0.7124616117
    ## CIEL3.SA.Adjusted       0.005076577       -0.47123075     -0.5023691268
    ## CMIG4.SA.Adjusted       0.109799540        0.64970112      0.8429011179
    ## CPFE3.SA.Adjusted      -0.016083159        0.70662768      0.6089291499
    ## CPLE6.SA.Adjusted      -0.100392875        0.31869693      0.0549609456
    ## CRUZ3.SA.Adjusted      -0.334355069        0.49748376      0.7671200907
    ## CSAN3.SA.Adjusted      -0.017711532        0.59660931      0.9075463666
    ## CSNA3.SA.Adjusted      -0.099138728        0.54250667      0.8316375208
    ## CTIP3.SA.Adjusted      -0.135932298       -0.26549855     -0.4644234509
    ## CYRE3.SA.Adjusted      -0.263088525        0.55407607      0.7746534598
    ## DTEX3.SA.Adjusted      -0.193102951        0.66740220      0.8820080960
    ## ECOR3.SA.Adjusted      -0.058416292        0.60614904      0.8802537027
    ## ELET3.SA.Adjusted      -0.106252120        0.50014096      0.7173446004
    ## ELET6.SA.Adjusted       0.278724363        0.16039834      0.1045858126
    ## EMBR3.SA.Adjusted       0.349084625        0.14016223     -0.2329664920
    ## ENBR3.SA.Adjusted      -0.327503048       -0.41462923     -0.5635186205
    ## ESTC3.SA.Adjusted       0.369243807        0.26614125      0.3345798326
    ## FIBR3.SA.Adjusted      -0.071111483       -0.03591217     -0.0082044703
    ## GFSA3.SA.Adjusted      -0.518018963        0.19025353      0.3146938411
    ## GGBR4.SA.Adjusted      -0.093417297        0.64189254      0.9153166462
    ## GOAU4.SA.Adjusted      -0.094984982        0.63722979      0.9117052159
    ## GOLL4.SA.Adjusted       0.164606564        0.57829380      0.8147949365
    ## HGTX3.SA.Adjusted      -0.467881502        0.43779921      0.7587767820
    ## HYPE3.SA.Adjusted       1.000000000        0.30711865     -0.0206958906
    ## ITSA4.SA.Adjusted       0.307118651        1.00000000      0.7097787353
    ## ITUB4.SA.Adjusted      -0.020695891        0.70977874      1.0000000000
    ## JBSS3.SA.Adjusted       0.693822952        0.49877042      0.3000625136
    ## KROT3.SA.Adjusted       0.747917912        0.33179684      0.2233334270
    ## LAME4.SA.Adjusted       0.030531525        0.19782658     -0.0840838886
    ## LREN3.SA.Adjusted       0.635871511       -0.12695603     -0.1339643322
    ## MRFG3.SA.Adjusted       0.236137441       -0.26844434     -0.5569821487
    ## MRVE3.SA.Adjusted      -0.153408890        0.43496102      0.4957105623
    ## MULT3.SA.Adjusted      -0.224207435        0.63067473      0.8410341387
    ## NATU3.SA.Adjusted       0.290164622        0.59165466      0.4516705480
    ## OIBR4.SA.Adjusted       0.413605922        0.45934146      0.5690729520
    ## PCAR4.SA.Adjusted      -0.298868303        0.56367515      0.8384764476
    ## PETR3.SA.Adjusted       0.342725245        0.25257973      0.4805285448
    ## PETR4.SA.Adjusted       0.285917042        0.42234238      0.6795879426
    ## POMO4.SA.Adjusted      -0.026428578        0.39560328      0.6023157423
    ## QUAL3.SA.Adjusted      -0.391665872        0.55968625      0.7219328861
    ## RENT3.SA.Adjusted      -0.123978979        0.59277950      0.8296673065
    ## RUMO3.SA.Adjusted      -0.596687784        0.26716571      0.3298588667
    ## SBSP3.SA.Adjusted      -0.362345969        0.34306154      0.2314690315
    ## SMLE3.SA.Adjusted      -0.397604637       -0.24374879     -0.4012400561
    ## SUZB5.SA.Adjusted       0.471504894       -0.32406263     -0.3435299621
    ## TBLE3.SA.Adjusted      -0.425851455        0.40270930      0.1923821111
    ## TIMP3.SA.Adjusted       0.355648109       -0.05859071      0.0720839072
    ## UGPA3.SA.Adjusted       0.059454848        0.31452759      0.2544087714
    ## USIM5.SA.Adjusted      -0.266670467        0.48721991      0.8241806427
    ## VALE3.SA.Adjusted      -0.065363212        0.34473397      0.6114439717
    ## VALE5.SA.Adjusted       0.090915874        0.34164197      0.6023941340
    ## VIVT4.SA.Adjusted      -0.448404732        0.51349790      0.8260971073
    ##                   JBSS3.SA.Adjusted KROT3.SA.Adjusted LAME4.SA.Adjusted
    ## USD.BRL                -0.342140079       -0.39224552       0.002772544
    ## ABEV3.SA.Adjusted      -0.009777896       -0.18945966       0.721359630
    ## BBAS3.SA.Adjusted       0.209641473        0.05398823       0.131048808
    ## BBDC3.SA.Adjusted      -0.142892180       -0.18423410       0.112120988
    ## BBDC4.SA.Adjusted       0.033514164       -0.04990236       0.083403423
    ## BBSE3.SA.Adjusted       0.380352630        0.24360066       0.053663864
    ## BRAP4.SA.Adjusted       0.321704688        0.27181752      -0.358292848
    ## BRFS3.SA.Adjusted       0.012000382        0.03712342       0.267279198
    ## BRKM5.SA.Adjusted       0.672520411        0.47741193       0.176584283
    ## BRML3.SA.Adjusted       0.123969661        0.11079661      -0.146911941
    ## BRPR3.SA.Adjusted      -0.379181954       -0.38090071       0.021039157
    ## BVMF3.SA.Adjusted       0.334724523        0.29758143       0.130875094
    ## CCRO3.SA.Adjusted      -0.151664987       -0.18204094      -0.103282094
    ## CESP6.SA.Adjusted       0.104712175        0.04785505      -0.077132748
    ## CIEL3.SA.Adjusted      -0.064901466       -0.14234750       0.504068655
    ## CMIG4.SA.Adjusted       0.494569373        0.32385344      -0.257134036
    ## CPFE3.SA.Adjusted       0.342727411        0.12870060       0.132667227
    ## CPLE6.SA.Adjusted      -0.049384742       -0.15853194       0.674462450
    ## CRUZ3.SA.Adjusted      -0.098549365       -0.15487105      -0.109613488
    ## CSAN3.SA.Adjusted       0.242474305        0.26696326      -0.290705974
    ## CSNA3.SA.Adjusted       0.316325946        0.14543218      -0.168271956
    ## CTIP3.SA.Adjusted      -0.048353882       -0.18832891       0.706159971
    ## CYRE3.SA.Adjusted      -0.051330597       -0.06808916      -0.260019500
    ## DTEX3.SA.Adjusted       0.146863671        0.05079233      -0.186075518
    ## ECOR3.SA.Adjusted       0.185143061        0.19697880      -0.226297992
    ## ELET3.SA.Adjusted       0.273196853        0.09464489      -0.115374750
    ## ELET6.SA.Adjusted       0.500475534        0.32777147      -0.036015914
    ## EMBR3.SA.Adjusted       0.284850026        0.29659989       0.032247643
    ## ENBR3.SA.Adjusted      -0.455116601       -0.56349628       0.479567343
    ## ESTC3.SA.Adjusted       0.303074374        0.61377219      -0.194948464
    ## FIBR3.SA.Adjusted      -0.059924859       -0.24182897      -0.426704044
    ## GFSA3.SA.Adjusted      -0.120267136       -0.40073808       0.347836743
    ## GGBR4.SA.Adjusted       0.257492470        0.16126032      -0.298881733
    ## GOAU4.SA.Adjusted       0.196098044        0.13221980      -0.324559179
    ## GOLL4.SA.Adjusted       0.343785486        0.40173233      -0.342087937
    ## HGTX3.SA.Adjusted      -0.204270250       -0.20139179      -0.195979962
    ## HYPE3.SA.Adjusted       0.693822952        0.74791791       0.030531525
    ## ITSA4.SA.Adjusted       0.498770420        0.33179684       0.197826578
    ## ITUB4.SA.Adjusted       0.300062514        0.22333343      -0.084083889
    ## JBSS3.SA.Adjusted       1.000000000        0.68805457       0.078171495
    ## KROT3.SA.Adjusted       0.688054574        1.00000000      -0.006681700
    ## LAME4.SA.Adjusted       0.078171495       -0.00668170       1.000000000
    ## LREN3.SA.Adjusted       0.294191104        0.47681895       0.029476536
    ## MRFG3.SA.Adjusted      -0.135831983       -0.05081812       0.493575716
    ## MRVE3.SA.Adjusted       0.025616472        0.10363014       0.114353911
    ## MULT3.SA.Adjusted       0.042351481       -0.03208144      -0.124327250
    ## NATU3.SA.Adjusted       0.554587619        0.34030058      -0.049869129
    ## OIBR4.SA.Adjusted       0.512157590        0.54731135      -0.464561733
    ## PCAR4.SA.Adjusted       0.018259671       -0.06533101      -0.290886411
    ## PETR3.SA.Adjusted       0.542233435        0.49434422       0.013957392
    ## PETR4.SA.Adjusted       0.540804386        0.44999042      -0.087936236
    ## POMO4.SA.Adjusted       0.407240938        0.23041783      -0.103225363
    ## QUAL3.SA.Adjusted       0.127710228       -0.09263726       0.233772606
    ## RENT3.SA.Adjusted       0.088353525        0.08272582      -0.229949275
    ## RUMO3.SA.Adjusted      -0.220133359       -0.38404058      -0.001567558
    ## SBSP3.SA.Adjusted       0.037606347       -0.29271059       0.142184423
    ## SMLE3.SA.Adjusted      -0.455151273       -0.39457108       0.569618723
    ## SUZB5.SA.Adjusted       0.357852237        0.27628978      -0.218670592
    ## TBLE3.SA.Adjusted      -0.110841317       -0.38100587       0.570495863
    ## TIMP3.SA.Adjusted       0.047465612        0.37073940      -0.227769906
    ## UGPA3.SA.Adjusted       0.456265000        0.15490199       0.062732422
    ## USIM5.SA.Adjusted       0.148731019       -0.00759822      -0.194211323
    ## VALE3.SA.Adjusted       0.225082505        0.12909295      -0.223733276
    ## VALE5.SA.Adjusted       0.342858455        0.28257422      -0.283286669
    ## VIVT4.SA.Adjusted      -0.063317260       -0.18363860      -0.064743427
    ##                   LREN3.SA.Adjusted MRFG3.SA.Adjusted MRVE3.SA.Adjusted
    ## USD.BRL                  0.03599222       0.517055297     -0.4824125170
    ## ABEV3.SA.Adjusted       -0.06914558       0.490642940      0.1342876324
    ## BBAS3.SA.Adjusted       -0.19093575      -0.366479614      0.3725387503
    ## BBDC3.SA.Adjusted       -0.32730846      -0.280464850      0.6515865538
    ## BBDC4.SA.Adjusted       -0.37734976      -0.477167499      0.5735619470
    ## BBSE3.SA.Adjusted       -0.11530788      -0.442639190      0.4936893913
    ## BRAP4.SA.Adjusted       -0.02084435      -0.565090890      0.0341832359
    ## BRFS3.SA.Adjusted        0.29974708       0.662701806     -0.4559556187
    ## BRKM5.SA.Adjusted        0.17077540      -0.165849777     -0.1766493309
    ## BRML3.SA.Adjusted       -0.33923763      -0.613808243      0.7336173063
    ## BRPR3.SA.Adjusted       -0.48254164      -0.177609379      0.5501650835
    ## BVMF3.SA.Adjusted       -0.15924461      -0.442651935      0.5722520944
    ## CCRO3.SA.Adjusted       -0.50820730      -0.492378855      0.6791085219
    ## CESP6.SA.Adjusted       -0.23701899      -0.471421749      0.6844551180
    ## CIEL3.SA.Adjusted        0.23251926       0.523368327     -0.4785942071
    ## CMIG4.SA.Adjusted       -0.23787305      -0.783081383      0.3456220978
    ## CPFE3.SA.Adjusted       -0.40491415      -0.434784163      0.3804595639
    ## CPLE6.SA.Adjusted       -0.09765796       0.459450439      0.3033969790
    ## CRUZ3.SA.Adjusted       -0.36015050      -0.504162261      0.5046772295
    ## CSAN3.SA.Adjusted       -0.21664906      -0.703286533      0.5755114264
    ## CSNA3.SA.Adjusted       -0.28208124      -0.689390413      0.2146636248
    ## CTIP3.SA.Adjusted       -0.02700435       0.524885479     -0.1745286589
    ## CYRE3.SA.Adjusted       -0.40820332      -0.610992595      0.7024195166
    ## DTEX3.SA.Adjusted       -0.42456309      -0.745993004      0.5466747306
    ## ECOR3.SA.Adjusted       -0.20296482      -0.676426408      0.5835755350
    ## ELET3.SA.Adjusted       -0.25173084      -0.574567034      0.0943753574
    ## ELET6.SA.Adjusted        0.03840140      -0.243094879     -0.3503172589
    ## EMBR3.SA.Adjusted        0.06098273       0.096171631      0.0034298181
    ## ENBR3.SA.Adjusted       -0.02008668       0.691077286     -0.3469851790
    ## ESTC3.SA.Adjusted        0.23800813      -0.300164043      0.3182346946
    ## FIBR3.SA.Adjusted       -0.18462957      -0.210965840      0.0009262426
    ## GFSA3.SA.Adjusted       -0.60415727      -0.232012964      0.2021588662
    ## GGBR4.SA.Adjusted       -0.32794231      -0.792493097      0.4785817684
    ## GOAU4.SA.Adjusted       -0.28700567      -0.749124605      0.5456276342
    ## GOLL4.SA.Adjusted       -0.10316353      -0.698561874      0.5002764875
    ## HGTX3.SA.Adjusted       -0.43203600      -0.532845012      0.5685079572
    ## HYPE3.SA.Adjusted        0.63587151       0.236137441     -0.1534088898
    ## ITSA4.SA.Adjusted       -0.12695603      -0.268444339      0.4349610228
    ## ITUB4.SA.Adjusted       -0.13396433      -0.556982149      0.4957105623
    ## JBSS3.SA.Adjusted        0.29419110      -0.135831983      0.0256164717
    ## KROT3.SA.Adjusted        0.47681895      -0.050818118      0.1036301358
    ## LAME4.SA.Adjusted        0.02947654       0.493575716      0.1143539106
    ## LREN3.SA.Adjusted        1.00000000       0.490301004     -0.3448351647
    ## MRFG3.SA.Adjusted        0.49030100       1.000000000     -0.3026046628
    ## MRVE3.SA.Adjusted       -0.34483516      -0.302604663      1.0000000000
    ## MULT3.SA.Adjusted       -0.40673710      -0.596068440      0.6838560179
    ## NATU3.SA.Adjusted       -0.12943278      -0.442035999      0.0181367335
    ## OIBR4.SA.Adjusted        0.02381793      -0.636410761      0.3948938885
    ## PCAR4.SA.Adjusted       -0.46881212      -0.743820276      0.5048123361
    ## PETR3.SA.Adjusted        0.27277559      -0.237553098     -0.0527643241
    ## PETR4.SA.Adjusted        0.12143943      -0.469495699      0.1392365752
    ## POMO4.SA.Adjusted       -0.36248065      -0.707515317      0.2210294613
    ## QUAL3.SA.Adjusted       -0.55606569      -0.543208206      0.4910534852
    ## RENT3.SA.Adjusted       -0.29413462      -0.664754603      0.6693080230
    ## RUMO3.SA.Adjusted       -0.84891906      -0.515233223      0.5620456518
    ## SBSP3.SA.Adjusted       -0.78145598      -0.510997070      0.2394126822
    ## SMLE3.SA.Adjusted       -0.20145632       0.491300314      0.1465285904
    ## SUZB5.SA.Adjusted        0.53528173       0.238454245     -0.5831162327
    ## TBLE3.SA.Adjusted       -0.62887675      -0.006551816      0.3542067444
    ## TIMP3.SA.Adjusted        0.55311510       0.220557893      0.2514161806
    ## UGPA3.SA.Adjusted       -0.34114530      -0.479279363     -0.0913173904
    ## USIM5.SA.Adjusted       -0.39079208      -0.701576186      0.2956007423
    ## VALE3.SA.Adjusted       -0.14701633      -0.521168532     -0.0098301540
    ## VALE5.SA.Adjusted       -0.03601091      -0.551853150     -0.0390943698
    ## VIVT4.SA.Adjusted       -0.41415299      -0.506575568      0.5080763418
    ##                   MULT3.SA.Adjusted NATU3.SA.Adjusted OIBR4.SA.Adjusted
    ## USD.BRL                 -0.67828741      -0.415362052     -0.5664067174
    ## ABEV3.SA.Adjusted        0.03678126       0.036619953     -0.4750117152
    ## BBAS3.SA.Adjusted        0.70585204       0.469273724      0.2926831565
    ## BBDC3.SA.Adjusted        0.89078730       0.003161942      0.1736520428
    ## BBDC4.SA.Adjusted        0.87691107       0.302523420      0.3183187972
    ## BBSE3.SA.Adjusted        0.74106449       0.527993738      0.5654306731
    ## BRAP4.SA.Adjusted        0.35297271       0.681282485      0.5421892582
    ## BRFS3.SA.Adjusted       -0.80729441      -0.212042760     -0.5428591481
    ## BRKM5.SA.Adjusted       -0.16943453       0.653251556      0.2570180807
    ## BRML3.SA.Adjusted        0.97100872       0.265371992      0.6140110998
    ## BRPR3.SA.Adjusted        0.60770337      -0.322850700     -0.0003408243
    ## BVMF3.SA.Adjusted        0.63745405       0.547751013      0.4920945477
    ## CCRO3.SA.Adjusted        0.76529989       0.222517821      0.3623276104
    ## CESP6.SA.Adjusted        0.85661286       0.164038609      0.5322832231
    ## CIEL3.SA.Adjusted       -0.69639804      -0.150316258     -0.5587053348
    ## CMIG4.SA.Adjusted        0.67664061       0.716822030      0.7598309918
    ## CPFE3.SA.Adjusted        0.51657477       0.676926688      0.3118465733
    ## CPLE6.SA.Adjusted        0.12130717      -0.092486767     -0.3490147042
    ## CRUZ3.SA.Adjusted        0.82260186       0.292463570      0.3071120884
    ## CSAN3.SA.Adjusted        0.88649974       0.410047195      0.7254864863
    ## CSNA3.SA.Adjusted        0.59656897       0.675563743      0.5130672091
    ## CTIP3.SA.Adjusted       -0.49668593      -0.285038128     -0.6610940813
    ## CYRE3.SA.Adjusted        0.96591411       0.148450719      0.5240657479
    ## DTEX3.SA.Adjusted        0.88266788       0.520139651      0.5564101524
    ## ECOR3.SA.Adjusted        0.86300376       0.445254827      0.6572562927
    ## ELET3.SA.Adjusted        0.43715195       0.699309013      0.3795937408
    ## ELET6.SA.Adjusted       -0.27500074       0.705167360      0.2419821487
    ## EMBR3.SA.Adjusted       -0.11621982       0.048963601      0.1756068015
    ## ENBR3.SA.Adjusted       -0.51050644      -0.463162914     -0.8821079863
    ## ESTC3.SA.Adjusted        0.31599380       0.223933982      0.5651109276
    ## FIBR3.SA.Adjusted        0.21340079      -0.130035428      0.1924026168
    ## GFSA3.SA.Adjusted        0.30518692       0.252172133     -0.2053268518
    ## GGBR4.SA.Adjusted        0.85139129       0.548842231      0.6813354050
    ## GOAU4.SA.Adjusted        0.90775138       0.451060807      0.6922672732
    ## GOLL4.SA.Adjusted        0.74141523       0.492633875      0.8462723349
    ## HGTX3.SA.Adjusted        0.84544706       0.216816202      0.3400783588
    ## HYPE3.SA.Adjusted       -0.22420743       0.290164622      0.4136059216
    ## ITSA4.SA.Adjusted        0.63067473       0.591654660      0.4593414632
    ## ITUB4.SA.Adjusted        0.84103414       0.451670548      0.5690729520
    ## JBSS3.SA.Adjusted        0.04235148       0.554587619      0.5121575901
    ## KROT3.SA.Adjusted       -0.03208144       0.340300575      0.5473113540
    ## LAME4.SA.Adjusted       -0.12432725      -0.049869129     -0.4645617332
    ## LREN3.SA.Adjusted       -0.40673710      -0.129432780      0.0238179259
    ## MRFG3.SA.Adjusted       -0.59606844      -0.442035999     -0.6364107607
    ## MRVE3.SA.Adjusted        0.68385602       0.018136734      0.3948938885
    ## MULT3.SA.Adjusted        1.00000000       0.198789562      0.5087364379
    ## NATU3.SA.Adjusted        0.19878956       1.000000000      0.4662998050
    ## OIBR4.SA.Adjusted        0.50873644       0.466299805      1.0000000000
    ## PCAR4.SA.Adjusted        0.90676373       0.368563752      0.5159654563
    ## PETR3.SA.Adjusted        0.06074368       0.586568670      0.4333373135
    ## PETR4.SA.Adjusted        0.35681500       0.635056796      0.6206055388
    ## POMO4.SA.Adjusted        0.39893483       0.636017120      0.5369410592
    ## QUAL3.SA.Adjusted        0.70137342       0.397276796      0.1923262745
    ## RENT3.SA.Adjusted        0.95160250       0.259114892      0.6173203682
    ## RUMO3.SA.Adjusted        0.59449297       0.141642130      0.1222691867
    ## SBSP3.SA.Adjusted        0.32763064       0.426021513      0.0436264805
    ## SMLE3.SA.Adjusted       -0.22520202      -0.476614935     -0.6255002914
    ## SUZB5.SA.Adjusted       -0.61885871       0.132216479      0.0690558477
    ## TBLE3.SA.Adjusted        0.30808668       0.208393261     -0.3143792250
    ## TIMP3.SA.Adjusted        0.06012779      -0.245515561      0.3361464796
    ## UGPA3.SA.Adjusted        0.07131638       0.622565024      0.2466806556
    ## USIM5.SA.Adjusted        0.67848725       0.579523479      0.4718015771
    ## VALE3.SA.Adjusted        0.30563917       0.662898071      0.3563701238
    ## VALE5.SA.Adjusted        0.26396281       0.690011271      0.4737935652
    ## VIVT4.SA.Adjusted        0.83350079       0.197182550      0.2597820142
    ##                   PCAR4.SA.Adjusted PETR3.SA.Adjusted PETR4.SA.Adjusted
    ## USD.BRL                 -0.70901708     -0.6143732512       -0.76001584
    ## ABEV3.SA.Adjusted       -0.07235618     -0.1026435724       -0.13570832
    ## BBAS3.SA.Adjusted        0.73417184      0.5230648546        0.65270822
    ## BBDC3.SA.Adjusted        0.77493252     -0.0779814421        0.16806871
    ## BBDC4.SA.Adjusted        0.87782708      0.2894281703        0.50172787
    ## BBSE3.SA.Adjusted        0.69816149      0.5633681770        0.73945486
    ## BRAP4.SA.Adjusted        0.60072404      0.7497345638        0.80289135
    ## BRFS3.SA.Adjusted       -0.80722330     -0.2056773258       -0.44227593
    ## BRKM5.SA.Adjusted       -0.04637346      0.7488583681        0.66043303
    ## BRML3.SA.Adjusted        0.88023130      0.1414159979        0.42514946
    ## BRPR3.SA.Adjusted        0.43865926     -0.5892010352       -0.35592700
    ## BVMF3.SA.Adjusted        0.66974352      0.6260154508        0.74330752
    ## CCRO3.SA.Adjusted        0.75126859      0.0071101869        0.22500094
    ## CESP6.SA.Adjusted        0.72812912      0.0671742761        0.33904458
    ## CIEL3.SA.Adjusted       -0.64499788      0.2334900265       -0.02970458
    ## CMIG4.SA.Adjusted        0.78452397      0.6048305039        0.79786521
    ## CPFE3.SA.Adjusted        0.59700482      0.3303260293        0.45533122
    ## CPLE6.SA.Adjusted       -0.03341821     -0.1208424612       -0.14240564
    ## CRUZ3.SA.Adjusted        0.87713546      0.1911532894        0.41082791
    ## CSAN3.SA.Adjusted        0.90355421      0.3933095030        0.64649336
    ## CSNA3.SA.Adjusted        0.78634704      0.6633894173        0.79465063
    ## CTIP3.SA.Adjusted       -0.58246570     -0.1524518002       -0.35627564
    ## CYRE3.SA.Adjusted        0.88442154     -0.0616845869        0.24488303
    ## DTEX3.SA.Adjusted        0.93446095      0.3175268846        0.56857879
    ## ECOR3.SA.Adjusted        0.87708201      0.3795576714        0.63143725
    ## ELET3.SA.Adjusted        0.66785416      0.6784476692        0.74706775
    ## ELET6.SA.Adjusted       -0.01398455      0.7051528752        0.58010519
    ## EMBR3.SA.Adjusted       -0.16028293     -0.2047241349       -0.19113319
    ## ENBR3.SA.Adjusted       -0.50638485     -0.3297404709       -0.52910577
    ## ESTC3.SA.Adjusted        0.32455557      0.2934002922        0.40293871
    ## FIBR3.SA.Adjusted        0.17830315     -0.2754443212       -0.13815896
    ## GFSA3.SA.Adjusted        0.41724628      0.2552905296        0.27827885
    ## GGBR4.SA.Adjusted        0.92815364      0.4346725034        0.67742560
    ## GOAU4.SA.Adjusted        0.93753064      0.3282106946        0.60146046
    ## GOLL4.SA.Adjusted        0.79544456      0.5274738906        0.74902369
    ## HGTX3.SA.Adjusted        0.89811083      0.1094461456        0.33535807
    ## HYPE3.SA.Adjusted       -0.29886830      0.3427252447        0.28591704
    ## ITSA4.SA.Adjusted        0.56367515      0.2525797276        0.42234238
    ## ITUB4.SA.Adjusted        0.83847645      0.4805285448        0.67958794
    ## JBSS3.SA.Adjusted        0.01825967      0.5422334346        0.54080439
    ## KROT3.SA.Adjusted       -0.06533101      0.4943442243        0.44999042
    ## LAME4.SA.Adjusted       -0.29088641      0.0139573921       -0.08793624
    ## LREN3.SA.Adjusted       -0.46881212      0.2727755890        0.12143943
    ## MRFG3.SA.Adjusted       -0.74382028     -0.2375530981       -0.46949570
    ## MRVE3.SA.Adjusted        0.50481234     -0.0527643241        0.13923658
    ## MULT3.SA.Adjusted        0.90676373      0.0607436755        0.35681500
    ## NATU3.SA.Adjusted        0.36856375      0.5865686704        0.63505680
    ## OIBR4.SA.Adjusted        0.51596546      0.4333373135        0.62060554
    ## PCAR4.SA.Adjusted        1.00000000      0.2149816127        0.48600966
    ## PETR3.SA.Adjusted        0.21498161      1.0000000000        0.93762116
    ## PETR4.SA.Adjusted        0.48600966      0.9376211568        1.00000000
    ## POMO4.SA.Adjusted        0.57466998      0.6743327217        0.75721142
    ## QUAL3.SA.Adjusted        0.71055412      0.2383541881        0.40589691
    ## RENT3.SA.Adjusted        0.90154589      0.1358020710        0.43494691
    ## RUMO3.SA.Adjusted        0.55004601     -0.2695780611       -0.06519444
    ## SBSP3.SA.Adjusted        0.42461246     -0.0008222318        0.11729523
    ## SMLE3.SA.Adjusted       -0.33435738     -0.3773290240       -0.48080815
    ## SUZB5.SA.Adjusted       -0.55661419      0.3166015665        0.13069286
    ## TBLE3.SA.Adjusted        0.28211325     -0.0950847885       -0.04481326
    ## TIMP3.SA.Adjusted       -0.10275533      0.0405263014        0.04252174
    ## UGPA3.SA.Adjusted        0.28046955      0.5007877099        0.50801190
    ## USIM5.SA.Adjusted        0.85158485      0.5520117899        0.70679203
    ## VALE3.SA.Adjusted        0.58316169      0.7205945916        0.73844455
    ## VALE5.SA.Adjusted        0.52539816      0.7881877425        0.80335432
    ## VIVT4.SA.Adjusted        0.84217001      0.1566520366        0.36505931
    ##                   POMO4.SA.Adjusted QUAL3.SA.Adjusted RENT3.SA.Adjusted
    ## USD.BRL                 -0.65964775      -0.648398080       -0.71768935
    ## ABEV3.SA.Adjusted       -0.22624863       0.199495793       -0.05886534
    ## BBAS3.SA.Adjusted        0.58672170       0.763409791        0.64433311
    ## BBDC3.SA.Adjusted        0.14214113       0.636846333        0.81898194
    ## BBDC4.SA.Adjusted        0.52961581       0.808059553        0.81213146
    ## BBSE3.SA.Adjusted        0.60555114       0.665824561        0.72033340
    ## BRAP4.SA.Adjusted        0.73911420       0.367206746        0.44092685
    ## BRFS3.SA.Adjusted       -0.51881748      -0.615704379       -0.77869557
    ## BRKM5.SA.Adjusted        0.57945214       0.184793674       -0.11217652
    ## BRML3.SA.Adjusted        0.43234741       0.685607056        0.95999943
    ## BRPR3.SA.Adjusted       -0.17469218       0.294424210        0.52912233
    ## BVMF3.SA.Adjusted        0.64995702       0.717521025        0.66311693
    ## CCRO3.SA.Adjusted        0.30189433       0.633931455        0.75542967
    ## CESP6.SA.Adjusted        0.28728333       0.520345141        0.86974869
    ## CIEL3.SA.Adjusted       -0.09519077      -0.241539430       -0.74662708
    ## CMIG4.SA.Adjusted        0.84043462       0.657130991        0.71475986
    ## CPFE3.SA.Adjusted        0.61077959       0.678173799        0.49234567
    ## CPLE6.SA.Adjusted       -0.21326719       0.237739315       -0.01674571
    ## CRUZ3.SA.Adjusted        0.36633058       0.647870092        0.82973399
    ## CSAN3.SA.Adjusted        0.58558927       0.623920930        0.92290166
    ## CSNA3.SA.Adjusted        0.86692739       0.727461259        0.59810607
    ## CTIP3.SA.Adjusted       -0.16480074       0.021801826       -0.63602711
    ## CYRE3.SA.Adjusted        0.29660726       0.604379625        0.95181346
    ## DTEX3.SA.Adjusted        0.63214681       0.768074005        0.89977615
    ## ECOR3.SA.Adjusted        0.52766378       0.632788840        0.92096592
    ## ELET3.SA.Adjusted        0.82019291       0.646753813        0.44436595
    ## ELET6.SA.Adjusted        0.66441372       0.131509639       -0.23067130
    ## EMBR3.SA.Adjusted       -0.17108611      -0.201037164       -0.09410091
    ## ENBR3.SA.Adjusted       -0.41815739      -0.215912041       -0.64200334
    ## ESTC3.SA.Adjusted        0.13302600       0.002967298        0.51668228
    ## FIBR3.SA.Adjusted       -0.06432881      -0.114968923        0.18376407
    ## GFSA3.SA.Adjusted        0.54937367       0.703730112        0.17359619
    ## GGBR4.SA.Adjusted        0.71652459       0.713678290        0.87717525
    ## GOAU4.SA.Adjusted        0.57927115       0.656794979        0.94357434
    ## GOLL4.SA.Adjusted        0.65428529       0.479656723        0.82309275
    ## HGTX3.SA.Adjusted        0.36305125       0.645611600        0.82408296
    ## HYPE3.SA.Adjusted       -0.02642858      -0.391665872       -0.12397898
    ## ITSA4.SA.Adjusted        0.39560328       0.559686254        0.59277950
    ## ITUB4.SA.Adjusted        0.60231574       0.721932886        0.82966731
    ## JBSS3.SA.Adjusted        0.40724094       0.127710228        0.08835352
    ## KROT3.SA.Adjusted        0.23041783      -0.092637263        0.08272582
    ## LAME4.SA.Adjusted       -0.10322536       0.233772606       -0.22994928
    ## LREN3.SA.Adjusted       -0.36248065      -0.556065695       -0.29413462
    ## MRFG3.SA.Adjusted       -0.70751532      -0.543208206       -0.66475460
    ## MRVE3.SA.Adjusted        0.22102946       0.491053485        0.66930802
    ## MULT3.SA.Adjusted        0.39893483       0.701373416        0.95160250
    ## NATU3.SA.Adjusted        0.63601712       0.397276796        0.25911489
    ## OIBR4.SA.Adjusted        0.53694106       0.192326275        0.61732037
    ## PCAR4.SA.Adjusted        0.57466998       0.710554119        0.90154589
    ## PETR3.SA.Adjusted        0.67433272       0.238354188        0.13580207
    ## PETR4.SA.Adjusted        0.75721142       0.405896908        0.43494691
    ## POMO4.SA.Adjusted        1.00000000       0.640968333        0.39630944
    ## QUAL3.SA.Adjusted        0.64096833       1.000000000        0.60954187
    ## RENT3.SA.Adjusted        0.39630944       0.609541873        1.00000000
    ## RUMO3.SA.Adjusted        0.29614701       0.664027878        0.49993018
    ## SBSP3.SA.Adjusted        0.59383201       0.665262740        0.23516226
    ## SMLE3.SA.Adjusted       -0.38403977      -0.022098999       -0.31232966
    ## SUZB5.SA.Adjusted       -0.05550076      -0.558234761       -0.57666857
    ## TBLE3.SA.Adjusted        0.21934018       0.645011701        0.15845293
    ## TIMP3.SA.Adjusted       -0.37969108      -0.401203639        0.16383694
    ## UGPA3.SA.Adjusted        0.80590871       0.457662613        0.03461501
    ## USIM5.SA.Adjusted        0.81181398       0.754366101        0.66484317
    ## VALE3.SA.Adjusted        0.77356121       0.471799534        0.34359573
    ## VALE5.SA.Adjusted        0.79780854       0.398713746        0.33103343
    ## VIVT4.SA.Adjusted        0.44545995       0.780061189        0.74190911
    ##                   RUMO3.SA.Adjusted SBSP3.SA.Adjusted SMLE3.SA.Adjusted
    ## USD.BRL                -0.185407925     -0.1877083523        0.30819318
    ## ABEV3.SA.Adjusted       0.078330074      0.1880265201        0.54336499
    ## BBAS3.SA.Adjusted       0.309189751      0.3362478291       -0.19129844
    ## BBDC3.SA.Adjusted       0.491346433      0.2227693275        0.04780421
    ## BBDC4.SA.Adjusted       0.489150875      0.4060823069       -0.11123607
    ## BBSE3.SA.Adjusted       0.295160384      0.2755781703       -0.31692690
    ## BRAP4.SA.Adjusted      -0.007168534      0.2098287767       -0.53269404
    ## BRFS3.SA.Adjusted      -0.433683813     -0.2896370731        0.33973880
    ## BRKM5.SA.Adjusted      -0.236968499      0.2487527493       -0.30422449
    ## BRML3.SA.Adjusted       0.570371647      0.2857128985       -0.26001796
    ## BRPR3.SA.Adjusted       0.534214834      0.2722983484        0.25966845
    ## BVMF3.SA.Adjusted       0.338420484      0.2974515248       -0.12871280
    ## CCRO3.SA.Adjusted       0.676072114      0.3844906446        0.03225035
    ## CESP6.SA.Adjusted       0.470573700      0.1891524129       -0.09387496
    ## CIEL3.SA.Adjusted      -0.385697432     -0.0755391107        0.40436987
    ## CMIG4.SA.Adjusted       0.334926750      0.4307961320       -0.57557462
    ## CPFE3.SA.Adjusted       0.422366816      0.6244943651       -0.16368390
    ## CPLE6.SA.Adjusted       0.132264010      0.1058465992        0.54824573
    ## CRUZ3.SA.Adjusted       0.492868204      0.2922478191       -0.09515952
    ## CSAN3.SA.Adjusted       0.380965903      0.2035198811       -0.47689928
    ## CSNA3.SA.Adjusted       0.303564851      0.4768822383       -0.46019322
    ## CTIP3.SA.Adjusted      -0.066464662      0.1535254635        0.56899237
    ## CYRE3.SA.Adjusted       0.617679169      0.2656575146       -0.22767483
    ## DTEX3.SA.Adjusted       0.559953005      0.4704141651       -0.31877311
    ## ECOR3.SA.Adjusted       0.400786665      0.2107759984       -0.38506519
    ## ELET3.SA.Adjusted       0.231547541      0.4704189799       -0.33727536
    ## ELET6.SA.Adjusted      -0.190962140      0.3086845475       -0.31641397
    ## EMBR3.SA.Adjusted      -0.067046918     -0.0356220892       -0.11282943
    ## ENBR3.SA.Adjusted      -0.173849200     -0.0036643602        0.65080602
    ## ESTC3.SA.Adjusted      -0.109073261     -0.2359760786       -0.30484672
    ## FIBR3.SA.Adjusted       0.126659301      0.0430732421       -0.28840200
    ## GFSA3.SA.Adjusted       0.479467607      0.7053228133        0.25896128
    ## GGBR4.SA.Adjusted       0.457141213      0.3981826314       -0.49272396
    ## GOAU4.SA.Adjusted       0.465964718      0.2984396467       -0.46395470
    ## GOLL4.SA.Adjusted       0.232239820      0.1691129810       -0.50609665
    ## HGTX3.SA.Adjusted       0.581086918      0.2841496495       -0.09901746
    ## HYPE3.SA.Adjusted      -0.596687784     -0.3623459690       -0.39760464
    ## ITSA4.SA.Adjusted       0.267165715      0.3430615355       -0.24374879
    ## ITUB4.SA.Adjusted       0.329858867      0.2314690315       -0.40124006
    ## JBSS3.SA.Adjusted      -0.220133359      0.0376063466       -0.45515127
    ## KROT3.SA.Adjusted      -0.384040578     -0.2927105886       -0.39457108
    ## LAME4.SA.Adjusted      -0.001567558      0.1421844235        0.56961872
    ## LREN3.SA.Adjusted      -0.848919061     -0.7814559827       -0.20145632
    ## MRFG3.SA.Adjusted      -0.515233223     -0.5109970701        0.49130031
    ## MRVE3.SA.Adjusted       0.562045652      0.2394126822        0.14652859
    ## MULT3.SA.Adjusted       0.594492966      0.3276306400       -0.22520202
    ## NATU3.SA.Adjusted       0.141642130      0.4260215127       -0.47661494
    ## OIBR4.SA.Adjusted       0.122269187      0.0436264805       -0.62550029
    ## PCAR4.SA.Adjusted       0.550046015      0.4246124598       -0.33435738
    ## PETR3.SA.Adjusted      -0.269578061     -0.0008222318       -0.37732902
    ## PETR4.SA.Adjusted      -0.065194442      0.1172952319       -0.48080815
    ## POMO4.SA.Adjusted       0.296147014      0.5938320148       -0.38403977
    ## QUAL3.SA.Adjusted       0.664027878      0.6652627402       -0.02209900
    ## RENT3.SA.Adjusted       0.499930184      0.2351622584       -0.31232966
    ## RUMO3.SA.Adjusted       1.000000000      0.6679637477        0.18554171
    ## SBSP3.SA.Adjusted       0.667963748      1.0000000000        0.05345941
    ## SMLE3.SA.Adjusted       0.185541714      0.0534594072        1.00000000
    ## SUZB5.SA.Adjusted      -0.601040703     -0.3889543229       -0.36171731
    ## TBLE3.SA.Adjusted       0.600439291      0.6792102281        0.46638541
    ## TIMP3.SA.Adjusted      -0.299938593     -0.7554066326       -0.08900662
    ## UGPA3.SA.Adjusted       0.145536813      0.6375234013       -0.32630309
    ## USIM5.SA.Adjusted       0.427692806      0.4974999690       -0.35644291
    ## VALE3.SA.Adjusted       0.076114513      0.3402090775       -0.39397189
    ## VALE5.SA.Adjusted      -0.022896339      0.2651299177       -0.50063567
    ## VIVT4.SA.Adjusted       0.558437504      0.3807752674       -0.10720517
    ##                   SUZB5.SA.Adjusted TBLE3.SA.Adjusted TIMP3.SA.Adjusted
    ## USD.BRL                 0.366263762     -0.1217555449      -0.127444976
    ## ABEV3.SA.Adjusted      -0.347416828      0.6703648117      -0.217345635
    ## BBAS3.SA.Adjusted      -0.357033084      0.4281255982      -0.116808897
    ## BBDC3.SA.Adjusted      -0.743009519      0.4609180485       0.056544053
    ## BBDC4.SA.Adjusted      -0.612164053      0.4849927751      -0.108072721
    ## BBSE3.SA.Adjusted      -0.245210608      0.2732090137       0.067976379
    ## BRAP4.SA.Adjusted       0.073766381     -0.0327042889      -0.036758556
    ## BRFS3.SA.Adjusted       0.394422369     -0.0547512592      -0.043623811
    ## BRKM5.SA.Adjusted       0.393464991      0.0428069229      -0.283598981
    ## BRML3.SA.Adjusted      -0.584672308      0.2533950209       0.163713879
    ## BRPR3.SA.Adjusted      -0.753487899      0.3947941424      -0.020737653
    ## BVMF3.SA.Adjusted      -0.385836291      0.3554850382       0.060179617
    ## CCRO3.SA.Adjusted      -0.603110882      0.4369268048       0.086889771
    ## CESP6.SA.Adjusted      -0.546174728      0.2379660200       0.219454980
    ## CIEL3.SA.Adjusted       0.400616656      0.0684226785      -0.249948509
    ## CMIG4.SA.Adjusted      -0.138757016      0.1175379027      -0.111271286
    ## CPFE3.SA.Adjusted      -0.355420310      0.5873642855      -0.321774467
    ## CPLE6.SA.Adjusted      -0.353616213      0.6361446503      -0.017006048
    ## CRUZ3.SA.Adjusted      -0.605343724      0.3680752353       0.029252529
    ## CSAN3.SA.Adjusted      -0.391598354      0.0655114683       0.167103119
    ## CSNA3.SA.Adjusted      -0.167753989      0.2400786940      -0.264174648
    ## CTIP3.SA.Adjusted       0.073040835      0.3736161695      -0.368325418
    ## CYRE3.SA.Adjusted      -0.620223789      0.2138046355       0.155957525
    ## DTEX3.SA.Adjusted      -0.529705002      0.3341308410      -0.083594232
    ## ECOR3.SA.Adjusted      -0.420297834      0.1257820858       0.186716643
    ## ELET3.SA.Adjusted      -0.121439351      0.2950381214      -0.303862918
    ## ELET6.SA.Adjusted       0.452960634      0.0264292431      -0.357941011
    ## EMBR3.SA.Adjusted       0.018839724     -0.0669257927       0.045332451
    ## ENBR3.SA.Adjusted      -0.002097329      0.3557871259      -0.311989462
    ## ESTC3.SA.Adjusted      -0.227324488     -0.2759845165       0.417839038
    ## FIBR3.SA.Adjusted       0.220492743     -0.1593146277       0.001435329
    ## GFSA3.SA.Adjusted      -0.417078474      0.7388660812      -0.625351467
    ## GGBR4.SA.Adjusted      -0.363210844      0.1752978794      -0.044572754
    ## GOAU4.SA.Adjusted      -0.419907097      0.1279601183       0.084958366
    ## GOLL4.SA.Adjusted      -0.237411626     -0.0543597858       0.202754221
    ## HGTX3.SA.Adjusted      -0.628511843      0.3444706453       0.057188444
    ## HYPE3.SA.Adjusted       0.471504894     -0.4258514555       0.355648109
    ## ITSA4.SA.Adjusted      -0.324062633      0.4027092983      -0.058590711
    ## ITUB4.SA.Adjusted      -0.343529962      0.1923821111       0.072083907
    ## JBSS3.SA.Adjusted       0.357852237     -0.1108413168       0.047465612
    ## KROT3.SA.Adjusted       0.276289781     -0.3810058712       0.370739395
    ## LAME4.SA.Adjusted      -0.218670592      0.5704958625      -0.227769906
    ## LREN3.SA.Adjusted       0.535281734     -0.6288767521       0.553115098
    ## MRFG3.SA.Adjusted       0.238454245     -0.0065518160       0.220557893
    ## MRVE3.SA.Adjusted      -0.583116233      0.3542067444       0.251416181
    ## MULT3.SA.Adjusted      -0.618858711      0.3080866777       0.060127790
    ## NATU3.SA.Adjusted       0.132216479      0.2083932615      -0.245515561
    ## OIBR4.SA.Adjusted       0.069055848     -0.3143792250       0.336146480
    ## PCAR4.SA.Adjusted      -0.556614194      0.2821132467      -0.102755327
    ## PETR3.SA.Adjusted       0.316601567     -0.0950847885       0.040526301
    ## PETR4.SA.Adjusted       0.130692863     -0.0448132641       0.042521740
    ## POMO4.SA.Adjusted      -0.055500755      0.2193401794      -0.379691080
    ## QUAL3.SA.Adjusted      -0.558234761      0.6450117015      -0.401203639
    ## RENT3.SA.Adjusted      -0.576668574      0.1584529303       0.163836935
    ## RUMO3.SA.Adjusted      -0.601040703      0.6004392910      -0.299938593
    ## SBSP3.SA.Adjusted      -0.388954323      0.6792102281      -0.755406633
    ## SMLE3.SA.Adjusted      -0.361717314      0.4663854067      -0.089006617
    ## SUZB5.SA.Adjusted       1.000000000     -0.5501724013       0.138243598
    ## TBLE3.SA.Adjusted      -0.550172401      1.0000000000      -0.535292127
    ## TIMP3.SA.Adjusted       0.138243598     -0.5352921266       1.000000000
    ## UGPA3.SA.Adjusted       0.113318290      0.3062288638      -0.639170478
    ## USIM5.SA.Adjusted      -0.298591785      0.3107653979      -0.240181953
    ## VALE3.SA.Adjusted       0.018858313      0.1428804888      -0.219973844
    ## VALE5.SA.Adjusted       0.126819754     -0.0009715515      -0.142245010
    ## VIVT4.SA.Adjusted      -0.542617783      0.4048083489      -0.087925793
    ##                   UGPA3.SA.Adjusted USIM5.SA.Adjusted VALE3.SA.Adjusted
    ## USD.BRL                -0.275872844       -0.74368146      -0.580478341
    ## ABEV3.SA.Adjusted      -0.063642304       -0.11501778      -0.137785076
    ## BBAS3.SA.Adjusted       0.325018228        0.82837489       0.652589216
    ## BBDC3.SA.Adjusted      -0.103025143        0.51300112       0.159821554
    ## BBDC4.SA.Adjusted       0.234991330        0.79848056       0.528575894
    ## BBSE3.SA.Adjusted       0.327562035        0.73403465       0.530106023
    ## BRAP4.SA.Adjusted       0.511562145        0.81074667       0.951671219
    ## BRFS3.SA.Adjusted      -0.119709300       -0.74290242      -0.449446907
    ## BRKM5.SA.Adjusted       0.657572220        0.28430059       0.503217837
    ## BRML3.SA.Adjusted       0.058455446        0.68980561       0.332360747
    ## BRPR3.SA.Adjusted      -0.238121964        0.06391613      -0.285980543
    ## BVMF3.SA.Adjusted       0.285231108        0.77909403       0.626771057
    ## CCRO3.SA.Adjusted      -0.017497410        0.58968509       0.306826600
    ## CESP6.SA.Adjusted      -0.092312214        0.52251152       0.183683393
    ## CIEL3.SA.Adjusted       0.109473968       -0.32765333      -0.089729494
    ## CMIG4.SA.Adjusted       0.564638066        0.87025364       0.727526045
    ## CPFE3.SA.Adjusted       0.576853113        0.66122431       0.572158713
    ## CPLE6.SA.Adjusted      -0.137533016       -0.05635735      -0.195190690
    ## CRUZ3.SA.Adjusted       0.055910361        0.73560705       0.504133475
    ## CSAN3.SA.Adjusted       0.208778953        0.79096405       0.539255087
    ## CSNA3.SA.Adjusted       0.633249600        0.96515298       0.886104870
    ## CTIP3.SA.Adjusted       0.102392820       -0.41032195      -0.342430731
    ## CYRE3.SA.Adjusted      -0.052813900        0.61059392       0.240989610
    ## DTEX3.SA.Adjusted       0.295917563        0.86221524       0.619323402
    ## ECOR3.SA.Adjusted       0.161443554        0.76219235       0.527760397
    ## ELET3.SA.Adjusted       0.626151503        0.91386137       0.918206997
    ## ELET6.SA.Adjusted       0.749707071        0.40304597       0.681365207
    ## EMBR3.SA.Adjusted       0.040013788       -0.27126658      -0.258689960
    ## ENBR3.SA.Adjusted      -0.123274294       -0.40462849      -0.273549577
    ## ESTC3.SA.Adjusted      -0.129331843        0.18304386       0.144310384
    ## FIBR3.SA.Adjusted      -0.008524923       -0.01765286      -0.124720578
    ## GFSA3.SA.Adjusted       0.508256266        0.57960472       0.470234897
    ## GGBR4.SA.Adjusted       0.389268241        0.89785643       0.685890837
    ## GOAU4.SA.Adjusted       0.217505686        0.82601742       0.563501401
    ## GOLL4.SA.Adjusted       0.274309834        0.74374667       0.587922269
    ## HGTX3.SA.Adjusted       0.006281369        0.76656171       0.476733997
    ## HYPE3.SA.Adjusted       0.059454848       -0.26667047      -0.065363212
    ## ITSA4.SA.Adjusted       0.314527588        0.48721991       0.344733974
    ## ITUB4.SA.Adjusted       0.254408771        0.82418064       0.611443972
    ## JBSS3.SA.Adjusted       0.456265000        0.14873102       0.225082505
    ## KROT3.SA.Adjusted       0.154901988       -0.00759822       0.129092953
    ## LAME4.SA.Adjusted       0.062732422       -0.19421132      -0.223733276
    ## LREN3.SA.Adjusted      -0.341145297       -0.39079208      -0.147016331
    ## MRFG3.SA.Adjusted      -0.479279363       -0.70157619      -0.521168532
    ## MRVE3.SA.Adjusted      -0.091317390        0.29560074      -0.009830154
    ## MULT3.SA.Adjusted       0.071316377        0.67848725       0.305639165
    ## NATU3.SA.Adjusted       0.622565024        0.57952348       0.662898071
    ## OIBR4.SA.Adjusted       0.246680656        0.47180158       0.356370124
    ## PCAR4.SA.Adjusted       0.280469554        0.85158485       0.583161690
    ## PETR3.SA.Adjusted       0.500787710        0.55201179       0.720594592
    ## PETR4.SA.Adjusted       0.508011898        0.70679203       0.738444547
    ## POMO4.SA.Adjusted       0.805908708        0.81181398       0.773561206
    ## QUAL3.SA.Adjusted       0.457662613        0.75436610       0.471799534
    ## RENT3.SA.Adjusted       0.034615012        0.66484317       0.343595727
    ## RUMO3.SA.Adjusted       0.145536813        0.42769281       0.076114513
    ## SBSP3.SA.Adjusted       0.637523401        0.49749997       0.340209077
    ## SMLE3.SA.Adjusted      -0.326303088       -0.35644291      -0.393971889
    ## SUZB5.SA.Adjusted       0.113318290       -0.29859178       0.018858313
    ## TBLE3.SA.Adjusted       0.306228864        0.31076540       0.142880489
    ## TIMP3.SA.Adjusted      -0.639170478       -0.24018195      -0.219973844
    ## UGPA3.SA.Adjusted       1.000000000        0.52446704       0.593439199
    ## USIM5.SA.Adjusted       0.524467040        1.00000000       0.848370811
    ## VALE3.SA.Adjusted       0.593439199        0.84837081       1.000000000
    ## VALE5.SA.Adjusted       0.610387750        0.80197300       0.973041668
    ## VIVT4.SA.Adjusted       0.188127038        0.75496967       0.453099343
    ##                   VALE5.SA.Adjusted VIVT4.SA.Adjusted
    ## USD.BRL               -0.6080945014       -0.65251228
    ## ABEV3.SA.Adjusted     -0.2419958212        0.09223626
    ## BBAS3.SA.Adjusted      0.6062109494        0.80026959
    ## BBDC3.SA.Adjusted      0.0684545176        0.81514508
    ## BBDC4.SA.Adjusted      0.4528710569        0.89680199
    ## BBSE3.SA.Adjusted      0.5541750762        0.69176793
    ## BRAP4.SA.Adjusted      0.9773487597        0.41589931
    ## BRFS3.SA.Adjusted     -0.4326567964       -0.72797120
    ## BRKM5.SA.Adjusted      0.5842565857       -0.08301361
    ## BRML3.SA.Adjusted      0.3132648340        0.79045906
    ## BRPR3.SA.Adjusted     -0.3588229823        0.40252571
    ## BVMF3.SA.Adjusted      0.6088953250        0.65074739
    ## CCRO3.SA.Adjusted      0.2212758174        0.73467567
    ## CESP6.SA.Adjusted      0.1809965139        0.59986848
    ## CIEL3.SA.Adjusted     -0.1064658387       -0.46266731
    ## CMIG4.SA.Adjusted      0.7681988594        0.60683723
    ## CPFE3.SA.Adjusted      0.5369337217        0.55716766
    ## CPLE6.SA.Adjusted     -0.2883602726        0.19570790
    ## CRUZ3.SA.Adjusted      0.4136702036        0.80818253
    ## CSAN3.SA.Adjusted      0.5533007447        0.74661300
    ## CSNA3.SA.Adjusted      0.8697588137        0.69269303
    ## CTIP3.SA.Adjusted     -0.3972080577       -0.28377804
    ## CYRE3.SA.Adjusted      0.2006493246        0.79070183
    ## DTEX3.SA.Adjusted      0.5855582705        0.79348396
    ## ECOR3.SA.Adjusted      0.5346211978        0.74200282
    ## ELET3.SA.Adjusted      0.8923836586        0.58503001
    ## ELET6.SA.Adjusted      0.7345577676       -0.11607385
    ## EMBR3.SA.Adjusted     -0.2230859601       -0.29453227
    ## ENBR3.SA.Adjusted     -0.3899189148       -0.26215428
    ## ESTC3.SA.Adjusted      0.2279941910        0.01595115
    ## FIBR3.SA.Adjusted     -0.1160106417        0.08017070
    ## GFSA3.SA.Adjusted      0.3422582571        0.50391532
    ## GGBR4.SA.Adjusted      0.6837141972        0.77330872
    ## GOAU4.SA.Adjusted      0.5609977970        0.78172036
    ## GOLL4.SA.Adjusted      0.6422861425        0.56898989
    ## HGTX3.SA.Adjusted      0.3763096638        0.84086170
    ## HYPE3.SA.Adjusted      0.0909158736       -0.44840473
    ## ITSA4.SA.Adjusted      0.3416419730        0.51349790
    ## ITUB4.SA.Adjusted      0.6023941340        0.82609711
    ## JBSS3.SA.Adjusted      0.3428584550       -0.06331726
    ## KROT3.SA.Adjusted      0.2825742211       -0.18363860
    ## LAME4.SA.Adjusted     -0.2832866694       -0.06474343
    ## LREN3.SA.Adjusted     -0.0360109146       -0.41415299
    ## MRFG3.SA.Adjusted     -0.5518531505       -0.50657557
    ## MRVE3.SA.Adjusted     -0.0390943698        0.50807634
    ## MULT3.SA.Adjusted      0.2639628117        0.83350079
    ## NATU3.SA.Adjusted      0.6900112715        0.19718255
    ## OIBR4.SA.Adjusted      0.4737935652        0.25978201
    ## PCAR4.SA.Adjusted      0.5253981592        0.84217001
    ## PETR3.SA.Adjusted      0.7881877425        0.15665204
    ## PETR4.SA.Adjusted      0.8033543226        0.36505931
    ## POMO4.SA.Adjusted      0.7978085350        0.44545995
    ## QUAL3.SA.Adjusted      0.3987137459        0.78006119
    ## RENT3.SA.Adjusted      0.3310334264        0.74190911
    ## RUMO3.SA.Adjusted     -0.0228963389        0.55843750
    ## SBSP3.SA.Adjusted      0.2651299177        0.38077527
    ## SMLE3.SA.Adjusted     -0.5006356723       -0.10720517
    ## SUZB5.SA.Adjusted      0.1268197539       -0.54261778
    ## TBLE3.SA.Adjusted     -0.0009715515        0.40480835
    ## TIMP3.SA.Adjusted     -0.1422450098       -0.08792579
    ## UGPA3.SA.Adjusted      0.6103877500        0.18812704
    ## USIM5.SA.Adjusted      0.8019729994        0.75496967
    ## VALE3.SA.Adjusted      0.9730416680        0.45309934
    ## VALE5.SA.Adjusted      1.0000000000        0.37040303
    ## VIVT4.SA.Adjusted      0.3704030266        1.00000000

Como a matriz resultante m é muito grande (64 linhas e 64 colunas), vou mostrar apenas uma parte dela:

``` r
m[1:5,1:5]
```

    ##                        USD.BRL ABEV3.SA.Adjusted BBAS3.SA.Adjusted
    ## USD.BRL            1.000000000       0.006523715        -0.7834658
    ## ABEV3.SA.Adjusted  0.006523715       1.000000000         0.2606677
    ## BBAS3.SA.Adjusted -0.783465819       0.260667660         1.0000000
    ## BBDC3.SA.Adjusted -0.559099285       0.338385543         0.7049489
    ## BBDC4.SA.Adjusted -0.772544701       0.236813392         0.9015431
    ##                   BBDC3.SA.Adjusted BBDC4.SA.Adjusted
    ## USD.BRL                  -0.5590993        -0.7725447
    ## ABEV3.SA.Adjusted         0.3383855         0.2368134
    ## BBAS3.SA.Adjusted         0.7049489         0.9015431
    ## BBDC3.SA.Adjusted         1.0000000         0.8857323
    ## BBDC4.SA.Adjusted         0.8857323         1.0000000

Os rótulos das linhas e colunas são referentes aos, digamos, indicadores (USD.BRL é a cotação do dólar). Os números dentro da matriz correspondem a correlação entre a variável da linha e a da coluna. Apenas com esta pequena fração da matriz, já é possível observar algumas correlações muito interessantes: - O dólar apresenta correlações negativas muito fortes (-0,77 e -0,75) com as ações do Banco do Brasil ON (BBAS3) e Bradesco (BBDC4), respectivamente. Ou seja, na maioria das vezes quando o dólar sobe, as ações desses dois bancos caem e vice-versa. Será uma tendência com todos os bancos listados na bolsa?
- Existe uma correlação positiva muito forte (0,90) entre as ações do Bradesco e do Banco do Brasil.

Além disso, é possível saber facilmente qual a correlação negativa mais forte dentre as cotações analisadas:

``` r
min(m)
```

    ## [1] -0.882108

``` r
which(m == min(m), arr.ind=TRUE)
```

    ##                   row col
    ## OIBR4.SA.Adjusted  48  29
    ## ENBR3.SA.Adjusted  29  48

A correlação negativa mais forte (-0,88), sabe-se lá o porquê, é entre a Oi e a EDP Energias do Brasil. Para comprovar este fato, veja este gráfico gerado com o ggplot2:

``` r
temp = select(df_final, Data, OIBR4.SA.Adjusted, ENBR3.SA.Adjusted)
temp = na.omit(temp)
temp = melt(temp, "Data")
levels(temp$variable) = c("Oi", "EDP Energias\n do Brasil")
ggplot(temp, aes(x=ymd(Data), value)) +  geom_line() +
  facet_grid(variable~., scales="free") +
  labs(title="Comparacão entre açoes da Oi e da EDP", x="Data", y="Valor")
```

![](postBovespaBlog_files/figure-markdown_github/unnamed-chunk-14-1.png)

Olhando o gráfico, pode-se inferir que a existência de outliers (comportamento anômalo) de subida e descida bruscas que as ações da Oi tiveram no fim de 2014 pode ter influenciado no cálculo da correlação. A título de curiosidade, vamos fazer a mesma análise com a segunda correlação negativa mais forte:

``` r
which(m == min( m[m!=min(m)] ), arr.ind=TRUE)
```

    ##                   row col
    ## BVMF3.SA.Adjusted  12   1
    ## USD.BRL             1  12

``` r
temp = select(df_final, Data, USD.BRL, BVMF3.SA.Adjusted)
temp = na.omit(temp)
temp = melt(temp, "Data")
levels(temp$variable) = c("Dólar", "BM&F Bovespa")
ggplot(temp, aes(x=ymd(Data), value)) +  geom_line() +
  facet_grid(variable~., scales="free") +
  labs(title="Comparacão entre dólar e açoes da BM&F Bovespa", x="Data", y="Valor")
```

![](postBovespaBlog_files/figure-markdown_github/unnamed-chunk-16-1.png)

Agora sim fica fácil percer a correlação negativa entre as variáveis: nos períodos quando o dólar sobe, as ações da BMF Bovespa caem (com algumas exceções) e vice-versa.

Existe um método ainda mais fácil de observar correlações entre um maior número de variáveis, como o do nosso exemplo(64): o gráfico de correlação, também chamado de correlograma.

Na verdade, 64 variáveis ainda é um número alto para se plotar em um gráfico. Por tentativa e erro, definir que o correlograma irá exibir apenas 15 indicadores, sendo eles o dólar e outros 14 selecionados aleatoriamente.

``` r
m2 = (df_final2[, c(1, runif(9,min=2, max=65))])
m2 = cor(m2)
corrplot(m2, method="color", tl.cex = 1, type="full", addCoef.col = "white")
```

![](postBovespaBlog_files/figure-markdown_github/unnamed-chunk-17-1.png)

Aliás, é bom que se diga que eu só selecionei a seed 123 (que fixa a escolha de números aleatórios pelo R), ou seja, se você tentar esse mesmo código, provavelmente sua matriz não terá os mesmos indicadores da mostrada aqui.

Dois pontos interessantes:

-   Existe uma correlação positiva altísssima (0,96, sendo que o máximo é 1) entre o dólar e as ações da Suzano Papel e Celulose(SUZB5), o que pode ser interpretado como natural dado a importância da exportação para a empresa.
-   A maior correlação negativa entre o dólar e um dos indicadores dessa fração da matriz original - no caso, a BRMalls (BRML3) é de -0,71.

Essas duas observações são investigadas no gráfico abaixo.

``` r
temp <- df_final %>%
  select(Data, USD.BRL, BRML3.SA.Adjusted, SUZB5.SA.Adjusted) %>%
  na.omit() %>%
  melt("Data")

levels(temp$variable) = c("Dolar", "BRMalls", "Suzano Papel\n e Celulose")
ggplot(temp, aes(x=ymd(Data), value)) + geom_line() + facet_grid(variable~., scales="free")
```

![](postBovespaBlog_files/figure-markdown_github/unnamed-chunk-18-1.png)
