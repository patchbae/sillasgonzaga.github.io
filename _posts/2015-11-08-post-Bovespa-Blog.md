---
layout: post
title: Correla��o entre o d�lar e a��es de empresas do �ndice Bovespa
output: html_document
---



## Introdu��o

Este projeto surgiu de uma situa��o que eu estou lidando no momento: digamos que voc� deseja investir dinheiro para fazer uma viagem internacional durante suas f�rias daqui a 5 anos. Como os gastos da viagem ser�o indexados pelo d�lar, n�o basta apenas escolher um bom investimento como t�tulos do tesouro ou outras modalidades de renda fixa. Por mais que os rendimentos deles possam apresentar enormes vantagens para seus investidores, eles precisam ser maiores que a valoriza��o do d�lar, caso contr�rio voc� correr� o risco de chegar no fim desse per�odo de 5 anos sem o dinheiro necess�rio para realizar seu sonho. Se j� � muito dif�cil prever quanto custar� o d�lar na pr�xima semana, prever seu valor em 5 anos � imposs�vel.  

Independentemente se voc� deseja realizar uma viagem internacional ou n�o, muitas pessoas certamente j� desejaram saber como o desempenho do d�lar afeta o rendimento de a��es na bolsa. � disto que o post inaugural deste blog se trata: talvez seja uma boa decis�o investir em a��es de empresas que acompanhem o comportamento do d�lar, isto �, que aumentem quando o d�lar aumenta. Em estat�stica, isso � chamado de correla��o:

> Diz-se que existe correla��o entre duas ou mais vari�veis quando as altera��es sofridas por uma delas s�o acompanhadas por modifica��es nas outras. Ou seja, no caso de duas vari�veis x e y os aumentos (ou diminui��es) em x correspondem a aumentos (ou diminui��es) em y. Assim, a correla��o revela se existe uma rela��o funcional entre uma vari�vel e as restantes.  

Programa��o e resultados

Todo o processo de an�lise foi feito no R, usando dados de cota��es obtidos no Yahoo Finance. Segue abaixo o c�digo


{% highlight r %}
#1 Baixar s�rie historica do dolar de 2 anos
library(quantmod)
library(plyr)
library(dplyr)
library(corrplot)
library(ggplot2)
library(reshape2)
library(lubridate)
{% endhighlight %}

Ap�s carregar as bibliotecas que ser�o usadas no c�digo, � usado o pacote quantmod para baixar e plotar uma s�rie hist�rica do d�lar. Por tentativa e erro, defini que o per�odo de an�lise seria entre 25 de agosto de 2013 at� 01 de agosto de 2015.


{% highlight r %}
datainicial = "2013-08-25"
datafinal = "2015-08-01"
cotacoes = getFX("USD/BRL", from=datainicial, to=datafinal, env=NULL)
lineChart(cotacoes)
{% endhighlight %}

![center](/figs/postBovespaBlog/unnamed-chunk-2-1.png) 

Um pouco de transforma��o de dados (o famoso Data Wrangling) necess�ria para transformar a vari�vel cotacoes de classe xts em dataframes:


{% highlight r %}
cotacoes = as.data.frame(cotacoes)
cotacoes$Data = rownames(cotacoes)
rownames(cotacoes) = NULL
{% endhighlight %}

Agora j� podemos baixar as cota��es das empresas. J� que existem centenas de empresas listadas , eu escolhi apenas as que comp�em o �ndice Bovespa. Para isso, eu salvei a lista das empresas em um arquivo csv e a carreguei no R.


{% highlight r %}
ibovespa <- read.csv2("https://raw.githubusercontent.com/sillasgonzaga/sillasgonzaga.github.io/master/data/ibovespa.csv")
suffix = as.data.frame(replicate(nrow(ibovespa), ".SA")) 

#Um pouco de Data Wrangling para concatenar os c�digos das empresas com o sufixo.

temp = cbind(ibovespa, suffix)
ibovespa = apply(temp, 1,
               function(x) paste0(toString(x[1]), toString(x[2]))
               )
{% endhighlight %}

Depois de carregar os c�digos das empresas e adicionar o sufixo .SA (sufixo que identifica as a��es da Bovespa), hora de baixar as s�ries hist�ricas de cota��es de cada empresa.


{% highlight r %}
#Baixar cotac�es
getSymbols(ibovespa[1:36], auto.assign = TRUE, from=datainicial, to=datafinal) #O 37(IBOV.SA) d� problema
{% endhighlight %}



{% highlight text %}
##  [1] "ABEV3.SA" "BBAS3.SA" "BBDC3.SA" "BBDC4.SA" "BBSE3.SA" "BRAP4.SA"
##  [7] "BRFS3.SA" "BRKM5.SA" "BRML3.SA" "BRPR3.SA" "BVMF3.SA" "CCRO3.SA"
## [13] "CESP6.SA" "CIEL3.SA" "CMIG4.SA" "CPFE3.SA" "CPLE6.SA" "CRUZ3.SA"
## [19] "CSAN3.SA" "CSNA3.SA" "CTIP3.SA" "CYRE3.SA" "DTEX3.SA" "ECOR3.SA"
## [25] "ELET3.SA" "ELET6.SA" "EMBR3.SA" "ENBR3.SA" "ESTC3.SA" "FIBR3.SA"
## [31] "GFSA3.SA" "GGBR4.SA" "GOAU4.SA" "GOLL4.SA" "HGTX3.SA" "HYPE3.SA"
{% endhighlight %}



{% highlight r %}
getSymbols(ibovespa[38:40], auto.assign = TRUE, from=datainicial, to=datafinal) #O 41(IBOV.SA) d� problema
{% endhighlight %}



{% highlight text %}
## [1] "ITSA4.SA" "ITUB4.SA" "JBSS3.SA"
{% endhighlight %}



{% highlight r %}
getSymbols(ibovespa[42:56], auto.assign = TRUE, from=datainicial, to=datafinal) #O 57(IBOV.SA) d� problema
{% endhighlight %}



{% highlight text %}
##  [1] "KROT3.SA" "LAME4.SA" "LREN3.SA" "MRFG3.SA" "MRVE3.SA" "MULT3.SA"
##  [7] "NATU3.SA" "OIBR4.SA" "PCAR4.SA" "PETR3.SA" "PETR4.SA" "POMO4.SA"
## [13] "QUAL3.SA" "RENT3.SA" "RUMO3.SA"
{% endhighlight %}



{% highlight r %}
getSymbols(ibovespa[58:length(ibovespa)], auto.assign = TRUE, from=datainicial, to=datafinal)
{% endhighlight %}



{% highlight text %}
##  [1] "SBSP3.SA" "SMLE3.SA" "SUZB5.SA" "TBLE3.SA" "TIMP3.SA" "UGPA3.SA"
##  [7] "USIM5.SA" "VALE3.SA" "VALE5.SA" "VIVT4.SA"
{% endhighlight %}

Como exemplo, esta � o arquivo de cota��es da Petrobras baixado, seguido por um gr�fico:


{% highlight r %}
head(PETR3.SA)
{% endhighlight %}



{% highlight text %}
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
{% endhighlight %}



{% highlight r %}
lineChart(PETR3.SA)
{% endhighlight %}

![center](/figs/postBovespaBlog/unnamed-chunk-6-1.png) 

As vari�veis criadas pelo quantmod com as cota��es baixadas do Yahoo Finance s�o da classe xts. Al�m disso, � apenas na �ltima coluna (Adjusted) que estamos interessados, al�m, claro, da coluna referente �s datas. Portanto, foi criada uma fun��o que transforma a vari�vel em dataframe e extrai apenas a coluna referente ao Adjusted:


{% highlight r %}
f = function(x) {
  temp = as.data.frame(x)
  result = as.data.frame(temp[,6])
  names(result) = names(temp)[6]
  result$Data = rownames(temp)
  return(result)
}
{% endhighlight %}

Criada a fun��o, basta aplic�-la em cada uma das vari�veis. Infelizmente, tive de fazer isso uma por uma, manualmente, porque n�o consiga pensar em um for loop que iterasse em todos os arquivos da classe xts. Sugest�es s�o muito bem vindas.


{% highlight r %}
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
{% endhighlight %}

O �ltimo passo da etapa de transforma��o de dados consiste em juntar todas as a��es das empresas, al�m das cota��es do d�lar, em uma vari�vel s�, chamada de df_final. Nesta etapa, tive um problema semelhante � anterior: n�o sabia muito bem como criar um for loop para n�o ter de escrever as vari�veis manualmente. Ainda bem que o iBovespa s� � composto por 67 empresas.


{% highlight r %}
df_final = join_all(dfs=list(cotacoes,
                              ABEV3.SA,BBAS3.SA,BBDC3.SA,BBDC4.SA,BBSE3.SA,BRAP4.SA,BRFS3.SA,BRKM5.SA,BRML3.SA,
                              BRPR3.SA,BVMF3.SA,CCRO3.SA,CESP6.SA,CIEL3.SA,CMIG4.SA,CPFE3.SA,CPLE6.SA,CRUZ3.SA,
                              CSAN3.SA,CSNA3.SA,CTIP3.SA,CYRE3.SA,DTEX3.SA,ECOR3.SA,ELET3.SA,ELET6.SA,EMBR3.SA,
                              ENBR3.SA,ESTC3.SA,FIBR3.SA,GFSA3.SA,GGBR4.SA,GOAU4.SA,GOLL4.SA,HGTX3.SA,HYPE3.SA,
                              ITSA4.SA,ITUB4.SA,JBSS3.SA,KROT3.SA,LAME4.SA,LREN3.SA,MRFG3.SA,MRVE3.SA,MULT3.SA,
                              NATU3.SA,OIBR4.SA,PCAR4.SA,PETR3.SA,PETR4.SA,POMO4.SA,QUAL3.SA,RENT3.SA,RUMO3.SA,
                              SBSP3.SA,SMLE3.SA,SUZB5.SA,TBLE3.SA,TIMP3.SA,UGPA3.SA,USIM5.SA,VALE3.SA,VALE5.SA,
                              VIVT4.SA), by="Data")
{% endhighlight %}


Agora, um novo dataframe sem a coluna de data � criado. Isso � necess�rio para criar a matriz de correla��o, que s� aceita como argumento um dataframe 100% composto de elementos num�ricos.


{% highlight r %}
df_final2 = na.omit(df_final)
df_final2 = select(df_final2, -Data)
{% endhighlight %}


Finalmente, podemos prosseguir com a cria��o da matriz de correla��o usando a fun��o base cor():

{% highlight r %}
m = cor(df_final2)
{% endhighlight %}


Como a matriz resultante m � muito grande (64 linhas e 64 colunas), vou mostrar apenas uma parte dela:


{% highlight r %}
m[1:5,1:5]
{% endhighlight %}



{% highlight text %}
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
{% endhighlight %}

Os r�tulos das linhas e colunas s�o referentes aos, digamos, indicadores (USD.BRL � a cota��o do d�lar). Os n�meros dentro da matriz correspondem a correla��o entre a vari�vel da linha e a da coluna. Apenas com esta pequena fra��o da matriz, j� � poss�vel observar algumas correla��es muito interessantes:
- O d�lar apresenta correla��es negativas muito fortes (-0,77 e -0,75) com as a��es do Banco do Brasil ON (BBAS3) e Bradesco (BBDC4), respectivamente. Ou seja, na maioria das vezes quando o d�lar sobe, as a��es desses dois bancos caem e vice-versa. Ser� uma tend�ncia com todos os bancos listados na bolsa?  
- Existe uma correla��o positiva muito forte (0,90) entre as a��es do Bradesco e do Banco do Brasil.  

Al�m disso, � poss�vel saber facilmente qual a correla��o negativa mais forte dentre as cota��es analisadas:


{% highlight r %}
min(m)
{% endhighlight %}



{% highlight text %}
## [1] -0.882108
{% endhighlight %}



{% highlight r %}
which(m == min(m), arr.ind=TRUE)
{% endhighlight %}



{% highlight text %}
##                   row col
## OIBR4.SA.Adjusted  48  29
## ENBR3.SA.Adjusted  29  48
{% endhighlight %}

A correla��o negativa mais forte (-0,88), sabe-se l� o porqu�, � entre a Oi e a EDP Energias do Brasil. Para comprovar este fato, veja este gr�fico gerado com o ggplot2:


{% highlight r %}
temp = select(df_final, Data, OIBR4.SA.Adjusted, ENBR3.SA.Adjusted)
temp = na.omit(temp)
temp = melt(temp, "Data")
levels(temp$variable) = c("Oi", "EDP Energias\n do Brasil")
ggplot(temp, aes(x=ymd(Data), value)) +  geom_line() +
  facet_grid(variable~., scales="free") +
  labs(title="Comparac�o entre a�oes da Oi e da EDP", x="Data", y="Valor")
{% endhighlight %}

![center](/figs/postBovespaBlog/unnamed-chunk-14-1.png) 

Olhando o gr�fico, pode-se inferir que a exist�ncia de outliers (comportamento an�malo) de subida e descida bruscas que as a��es da Oi tiveram no fim de 2014 pode ter influenciado no c�lculo da correla��o. A t�tulo de curiosidade, vamos fazer a mesma an�lise com a segunda correla��o negativa mais forte:


{% highlight r %}
which(m == min( m[m!=min(m)] ), arr.ind=TRUE)
{% endhighlight %}



{% highlight text %}
##                   row col
## BVMF3.SA.Adjusted  12   1
## USD.BRL             1  12
{% endhighlight %}


{% highlight r %}
temp = select(df_final, Data, USD.BRL, BVMF3.SA.Adjusted)
temp = na.omit(temp)
temp = melt(temp, "Data")
levels(temp$variable) = c("D�lar", "BM&F Bovespa")
ggplot(temp, aes(x=ymd(Data), value)) +  geom_line() +
  facet_grid(variable~., scales="free") +
  labs(title="Comparac�o entre d�lar e a�oes da BM&F Bovespa", x="Data", y="Valor")
{% endhighlight %}

![center](/figs/postBovespaBlog/unnamed-chunk-16-1.png) 

Agora sim fica f�cil percer a correla��o negativa entre as vari�veis: nos per�odos quando o d�lar sobe, as a��es da BMF Bovespa caem (com algumas exce��es) e vice-versa.  

Existe um m�todo ainda mais f�cil de observar correla��es entre um maior n�mero de vari�veis, como o do nosso exemplo(64): o gr�fico de correla��o, tamb�m chamado de correlograma.  

Na verdade, 64 vari�veis ainda � um n�mero alto para se plotar em um gr�fico. Por tentativa e erro, definir que o correlograma ir� exibir apenas 15 indicadores, sendo eles o d�lar e outros 14 selecionados aleatoriamente.  


{% highlight r %}
m2 = (df_final2[, c(1, runif(9,min=2, max=65))])
m2 = cor(m2)
corrplot(m2, method="color", tl.cex = 1, type="full", addCoef.col = "white")
{% endhighlight %}

![center](/figs/postBovespaBlog/unnamed-chunk-17-1.png) 


Ali�s, � bom que se diga que eu s� selecionei a seed 123 (que fixa a escolha de n�meros aleat�rios pelo R), ou seja, se voc� tentar esse mesmo c�digo, provavelmente sua matriz n�o ter� os mesmos indicadores da mostrada aqui.

Dois pontos interessantes:

- Existe uma correla��o positiva alt�sssima (0,96, sendo que o m�ximo � 1) entre o d�lar e as a��es da Suzano Papel e Celulose(SUZB5), o que pode ser interpretado como natural dado a import�ncia da exporta��o para a empresa.
- A maior correla��o negativa entre o d�lar e um dos indicadores dessa fra��o da matriz original - no caso, a BRMalls (BRML3) � de -0,71.

Essas duas observa��es s�o investigadas no gr�fico abaixo.


{% highlight r %}
temp <- df_final %>%
  select(Data, USD.BRL, BRML3.SA.Adjusted, SUZB5.SA.Adjusted) %>%
  na.omit() %>%
  melt("Data")

levels(temp$variable) = c("Dolar", "BRMalls", "Suzano Papel\n e Celulose")
ggplot(temp, aes(x=ymd(Data), value)) + geom_line() + facet_grid(variable~., scales="free")
{% endhighlight %}

![center](/figs/postBovespaBlog/unnamed-chunk-18-1.png) 
