---
title: "Blog Post"
author: "Sillas Teixeira Gonzaga"
date: "Monday, October 05, 2015"
output:
  md_document:
    variant: markdown_phpextra+backtick_code_blocks
---

# 1 - Introdu��o

Alguns dos datasets brasileiros mais interessantes podem ser encontrados no __[Portal da Transpar�ncia](http://transparencia.gov.br/)__, no qual � poss�vel obter dados sobre:

* gastos diretos do Governo Federal (desde 2004 - exceto Cart�o de Pagamentos - desde 2002)
* transfer�ncias de recursos a Estados e Munic�pios (desde 2004)
* conv�nios com pessoas f�sicas, jur�dicas ou entes governamentais (desde 1996)
* previs�o e arrecada��o de receitas (desde 2009)
* __servidores do Governo Federal.__

� por esse �ltimo item que mais me interessei no momento e sobre o qual publicarei uma s�rie de posts nos pr�ximos dias.  

Os dados foram baixados [deste endere�o](http://transparencia.gov.br/downloads/servidores.asp) e correspondem ao m�s de Agosto, que era a op��o mais recente dispon�vel at� ent�o.  

A pasta zipada baixada cont�m cinco arquivos, dentre os quais s� usaremos dois: *20150831-Cadastro.csv*  e *20150831-Remuneracao.csv*. Ambos contem 44 vari�veis e cerca de 700 mil linhas, mais a maioria delas n�o s�o muito importantes neste contexto.

# 2. Importa��o e limpeza dos dados



Ap�s carregar as bibliotecas que ser�o usadas, hora de carregar os dados. Essa foi a primeira vez que eu trabalhei com um dataset t�o grande no R. O arquivo pesa mais de 370 MB e demorou mais de um minuto para ser carregado. Imagina se fosse no Excel..

Ao notar que o carregamento dos dados demorava muito, usei uma solu��o que aprendi em f�runs sobre o R: importar apenas as colunas necess�rias usando dplyr. Menos da metade das colunas presentes no arquivo csv original ser�o usadas na an�lise e o ato de filtr�-las fora agiliza em muito a importa��o para o R. Confira a compara��o:


{% highlight r %}
# Teste 1: Importar tudo
system.time(df <- read.csv("C:/R/data/201508_Servidores/20150831_Cadastro.csv", sep="\t", stringsAsFactors = FALSE))
{% endhighlight %}



{% highlight text %}
## Warning in read.table(file = file, header = header, sep = sep, quote =
## quote, : line 2 appears to contain embedded nulls
{% endhighlight %}



{% highlight text %}
## Warning in read.table(file = file, header = header, sep = sep, quote =
## quote, : line 3 appears to contain embedded nulls
{% endhighlight %}



{% highlight text %}
## Warning in read.table(file = file, header = header, sep = sep, quote =
## quote, : line 4 appears to contain embedded nulls
{% endhighlight %}



{% highlight text %}
## Warning in read.table(file = file, header = header, sep = sep, quote =
## quote, : line 5 appears to contain embedded nulls
{% endhighlight %}



{% highlight text %}
## Warning in scan(file = file, what = what, sep = sep, quote = quote, dec =
## dec, : embedded nul(s) found in input
{% endhighlight %}



{% highlight text %}
##    user  system elapsed 
##   42.68    0.70   43.93
{% endhighlight %}



{% highlight r %}
# Teste 2: importar apenas colunas importantes
system.time(df <- read.csv("C:/R/data/201508_Servidores/20150831_Cadastro.csv",
                           sep="\t", stringsAsFactors = FALSE) %>%
              select(-DIPLOMA_INGRESSO_CARGOFUNCAO, -DATA_NOMEACAO_CARGOFUNCAO,
             -REFERENCIA_CARGO, -COD_AFASTAMENTO, -COD_GRUPO_AFASTAMENTO,
             -NIVEL_CARGO, -COD_UORG_EXERCICIO, -COD_UORG_LOTACAO, -OPCAO_PARCIAL,
             -DIPLOMA_INGRESSO_SERVICOPUBLICO, -DIPLOMA_INGRESSO_ORGAO,
             -DOCUMENTO_INGRESSO_SERVICOPUBLICO, -DATA_INICIO_AFASTAMENTO,
             -DATA_TERMINO_AFASTAMENTO, -TIPO_VINCULO, -COD_ORGSUP_EXERCICIO,
             -COD_ORG_EXERCICIO, -COD_ORGSUP_LOTACAO, -COD_ORG_LOTACAO,
             -CPF, -MATRICULA, -FUNCAO, -CLASSE_CARGO, -PADRAO_CARGO,
             -SIGLA_FUNCAO, -NIVEL_FUNCAO, -CODIGO_ATIVIDADE)
)
{% endhighlight %}



{% highlight text %}
## Warning in read.table(file = file, header = header, sep = sep, quote =
## quote, : line 2 appears to contain embedded nulls
{% endhighlight %}



{% highlight text %}
## Warning in read.table(file = file, header = header, sep = sep, quote =
## quote, : line 3 appears to contain embedded nulls
{% endhighlight %}



{% highlight text %}
## Warning in read.table(file = file, header = header, sep = sep, quote =
## quote, : line 4 appears to contain embedded nulls
{% endhighlight %}



{% highlight text %}
## Warning in read.table(file = file, header = header, sep = sep, quote =
## quote, : line 5 appears to contain embedded nulls
{% endhighlight %}



{% highlight text %}
## Warning in scan(file = file, what = what, sep = sep, quote = quote, dec =
## dec, : embedded nul(s) found in input
{% endhighlight %}



{% highlight text %}
##    user  system elapsed 
##   37.34    0.56   38.31
{% endhighlight %}

Cada linha do df corresponde a um servidor e cada uma das vari�veis corresponde a um atributo do mesmo.

Primeiro ponto a ser analisado: qual a qualidade dos dados? Quantas vari�veis tem muitos valores vazios ou nulos?


{% highlight text %}
##                   Id_SERVIDOR_PORTAL                                 NOME 
##                                    0                                    0 
##                        ORG_EXERCICIO                     ORGSUP_EXERCICIO 
##                                    4                                    4 
##                     SITUACAO_VINCULO                      REGIME_JURIDICO 
##                                    4                                    4 
##                  JORNADA_DE_TRABALHO DATA_DIPLOMA_INGRESSO_SERVICOPUBLICO 
##                                    4                                    4 
##                          ORG_LOTACAO                       ORGSUP_LOTACAO 
##                                   14                                   14 
##                  DATA_INGRESSO_ORGAO            DATA_INGRESSO_CARGOFUNCAO 
##                                 2799                                47192 
##                         UORG_LOTACAO                      DESCRICAO_CARGO 
##                               120819                               154017 
##                       UORG_EXERCICIO                         UF_EXERCICIO 
##                               165589                               189437 
##                            ATIVIDADE 
##                               688290
{% endhighlight %}

Visto que � poss�vel que um mesmo servidor tenha mais de um cargo p�blico (por exemplo, uma pessoa pode ser professora de universidade federal e chefe de seu departamento), � necess�rio excluir os servidores repetidos.


{% highlight r %}
length(df$Id_SERVIDOR_PORTAL) #Quantidade de IDs de servidores no arquivo
{% endhighlight %}



{% highlight text %}
## [1] 795107
{% endhighlight %}



{% highlight r %}
length(unique(df$Id_SERVIDOR_PORTAL)) #Quantidade de IDs �nicas
{% endhighlight %}



{% highlight text %}
## [1] 681266
{% endhighlight %}



{% highlight r %}
100 * length(unique(df$Id_SERVIDOR_PORTAL)) / 605670 # Porcentual de IDs �nicas
{% endhighlight %}



{% highlight text %}
## [1] 112.4814
{% endhighlight %}



{% highlight r %}
df <- df[!duplicated(df$Id_SERVIDOR_PORTAL), ]
{% endhighlight %}


Uma informa��o n�o presente no relat�rio � a regi�o do Servidor. Isso � facilmente inserido manualmente pelo R (ali�s, um bom exerc�cio seria a cria��o de uma library com datasets brasileiros).


{% highlight r %}
UF_EXERCICIO = sort(unique(na.omit(df$UF_EXERCICIO)))
br <- data.frame(UF_EXERCICIO)
br$REGIAO <- c('Norte', 'Nordeste', 'Norte', 'Norte', 'Nordeste', 'Nordeste', 'Centro-Oeste', 'Sudeste', 'Centro-Oeste', 'Nordeste', 'Sudeste', 'Centro-Oeste', 'Centro-Oeste', 'Norte', 'Nordeste', 'Nordeste', 'Nordeste', 'Sul', 'Sudeste', 'Nordeste', 'Norte', 'Norte', 'Sul', 'Sul', 'Nordeste', 'Sudeste', 'Nordeste')

df <- merge(df, br, by="UF_EXERCICIO")
{% endhighlight %}

__Observa��o:__ notei um comportamento estranho do R. Ao fazer o merge(), ele automaticamente deleta todas as linhas onde o valor da vari�vel UF_EXERC�CIO � nulo. Como isso n�o implica um grande preju�zo para a an�lise, iremos proseguir mesmo assim.  


Vamos agora � explora��o b�sica de dados. Primeira pergunta: qual estado tem o maior n�mero de servidores p�blicos?


{% highlight r %}
temp <- df %>%
    select(UF_EXERCICIO, REGIAO) %>%
    mutate(Estado = UF_EXERCICIO) %>%
    group_by(Estado, REGIAO) %>%
    summarise(numero.servidores = n())

    ggplot(data=temp, aes(x=reorder(Estado, numero.servidores), y=numero.servidores, fill=REGIAO)) +
        geom_bar(stat="identity") + coord_flip() +
        labs(title="N�mero de servidores por Estado", x="", y="N�mero de servidores") +
        theme_economist() +
        scale_fill_economist()
{% endhighlight %}

![center](/figs/transparencia/unnamed-chunk-4-1.png) 

{% highlight r %}
#Gr�fico por regi�o
df %>%
    select(REGIAO) %>%
    group_by(REGIAO) %>%
    summarise(numero.servidores = n()) %>%
     ggplot(aes(x=reorder(REGIAO, numero.servidores) , y=numero.servidores)) +
        geom_bar(stat="identity") + coord_flip() +
        labs(title="N�mero de servidores por Estado", x="", y="N�mero de servidores") +
        theme_economist()
{% endhighlight %}

![center](/figs/transparencia/unnamed-chunk-4-2.png) 

� claro que a popula��o de cada estado tem uma grande influ�ncia no resultado anterior... ser�?
Para tirar a d�vida, aqui vai um gr�fico de propor��o de servidores em cada estado. A tabela com a popula��o de cada estado foi extra�da manualmente da Wikipedia.



{% highlight r %}
pop <- read.csv2("C:/R/data/201508_Servidores/populacao.csv", stringsAsFactors = FALSE)
names(pop) <- c("Estado", "Popula��o")
temp <- merge(temp, pop, by="Estado")
temp$Proporcao = round(1000*(temp$numero.servidores/temp$Popula��o),2)

    ggplot(data=temp, aes(x=reorder(Estado, temp$Proporcao), y=temp$Proporcao, fill=REGIAO)) +
        geom_bar(stat="identity") + coord_flip() +
        labs(title="Propor��o de servidores por Estado", x="Estado", y="Propor��o da popula��o \n que � funcion�rio p�blico") +
        theme_economist()
{% endhighlight %}

![center](/figs/transparencia/unnamed-chunk-5-1.png) 

Os resultados s�o muito interessantes. Mais de um quarto dos habitantes do Distrito Federal s�o funcion�rios p�blicos. Roraima, Amap� e Rio de Janeiro tamb�m parecem ter m�quinas p�blicas inchadas.

Para finalizar, vou salvar o data frame criado para posteriores an�lises.


{% highlight r %}
df %>% as.data.frame() %>% write.csv2(file = "C:/R/data/201508_Servidores/transparencia.csv", row.names = FALSE)
{% endhighlight %}

O novo arquivo tem 191 MB, 48% a menos que o original.