---
title: "Transparência (3): Em quais estados os salários são mais mal distribuídos?"
author: "Sillas Teixeira Gonzaga"
date: "Sunday, January 11, 2016"
layout: post
output:
  md_document:
    variant: markdown_phpextra+backtick_code_blocks
---




{% highlight r %}
library(ggplot)
{% endhighlight %}



{% highlight text %}
## Error in library(ggplot): there is no package called 'ggplot'
{% endhighlight %}



{% highlight r %}
library(ggrepel)
library(ggthemes)
library(dplyr)
{% endhighlight %}


##### Aviso

Este post funciona como um adendo ao anterior, portanto recomendo o ler antes de prosseguir com a leitura.

Assim que eu publiquei o último post, percebi que perdi a oportunidade de analisar o quão diferente são as distribuições dos salários nos estados brasileiros e não só nas regiões. Voltando ao nosso dataset, que dessa vez carrego apenas as colunas de salários e UFs:



{% highlight r %}
df <- read.csv2("/home/sillas/R/data/transparenciaComSalarios.csv", stringsAsFactors = FALSE) %>%
  select(uf = UF_EXERCICIO, salario = SALARIO)
{% endhighlight %}

Quais são, então, os estados com as maiores assimetria e curtoses em sua distribuição de salário?


{% highlight r %}
temp <- df %>%
  group_by(uf) %>%
  summarise(assimetria = skewness(salario),
            curtose = kurtosis(salario))

ggplot(temp, aes(x = assimetria, y = curtose)) +
  geom_point() +
  geom_text_repel(aes(label = uf)) +
  theme_few()
{% endhighlight %}

![center](/figs/transparenciaParte3/unnamed-chunk-3-1.png) 

Do gráfico de cima tiramos duas conclusões:  
* A disparidade do Amapá e, principalmente, de Roraima em relação aos outros estados é colossal.
* Existe uma correlação linear entre assimetria e curtose, algo que eu não esperava muito. Podemos checar este dado:


{% highlight r %}
cor(temp$assimetria, temp$curtose)
{% endhighlight %}



{% highlight text %}
## [1] 0.9850373
{% endhighlight %}

Realmente, a correlação é muito alta.

Voltando aos estados, nada melhor do que plotar uma comparação entre os estados mais díspares e os que a distribuição mais se aproxima do normal (SP e DF):


{% highlight r %}
temp <- filter(df, uf %in% c("RR", "AP", "DF", "SP"))

# Necessário para mudar a ordem dos estados no gráfico
temp$uf <- factor(temp$uf, levels = c("RR", "AP", "DF", "SP"))  


ggplot(temp, aes(salario)) +
  geom_histogram() +
  facet_grid(uf ~., scales = "free") +
  scale_x_continuous(breaks=seq(0, 50000, by=5000)) +
  theme_few() +
  labs(title = "Distribuição do salário dos servidores em certas UFs", x = "Faixa salarial", y = "Frequência")
{% endhighlight %}

![center](/figs/transparenciaParte3/unnamed-chunk-5-1.png) 

Agora fica tudo muito claro: Existe uma concentração estranhamente grande de pessoas que ganham cerca de R$5000,00 mensais em comparação com o resto dos servidores do estado.

A presença de outliers que ganha mais de 25000 reais distorce o gráfico, então vale a pena olhar para a mesma distribuição sem eles:


{% highlight r %}
ggplot(subset(temp, salario <= 25000), aes(salario)) +
  geom_histogram(binwidth = 1000) +
  facet_grid(uf ~., scales = "free") +
  scale_x_continuous(breaks=seq(0, 50000, by=5000))  +
  theme_few() +
  labs(title = "Distribuição do salário dos servidores em certas UFs", x = "Faixa salarial", y = "Frequência")
{% endhighlight %}

![center](/figs/transparenciaParte3/unnamed-chunk-6-1.png) 

Temos agora ainda mais evidência de um fenômeno muito interessante: os salários em RR e AP são muito mais distribuídos. Na verdade, o que acontece é que a grande maioria dos servidores roraimenses e amapaenses ganham até R$5000,00 e muito poucos ganham mais de R$15000,00.

