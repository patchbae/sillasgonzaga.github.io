---
title: "Meta-análise R (1): Quais são os pacotes mais baixados?"
author: "Sillas Teixeira Gonzaga"
date: "May 25, 2016"
layout: post
comments: true
output:
  md_document:
    variant: markdown_phpextra+backtick_code_blocks
---



Este é o primeiro post de uma nova série sobre meta-análises de pacotes R. Com o pacote `miniCRAN` é possível baixar logs de downloads de pacotes de R por meio do [espelho (mirror) do RStudio do CRAN](https://cran.rstudio.com/). Cada linha nesses logs representa um download de um pacote por um usuário.

O objetivo desta série é analisar os dados gerados por esses logs.

Para este primeiro post, é mostrado:  
- Como baixar os logs de downloads de pacotes do CRAN do RStudio de forma automatizada com o pacote `miniCRAN`;  
- Como selecionar os pacotes R mais populares pelo [Princípio de Pareto](https://pt.wikipedia.org/wiki/Princ%C3%ADpio_de_Pareto);  
- Os 20 pacotes mais populares;  
- Um grafo de redes criado a partir dos pacotes mais populares filtrados e suas dependências.

Os pacotes usados neste post são:

{% highlight r %}
#library(installr)
suppressMessages(library(dplyr)) # Usado para agregar os dados em pacotes
suppressMessages(library(igraph)) # Usado para plotar o grafo criado pelo miniCRAN
library(miniCRAN)
library(ggplot2) 
library(ggthemes)
library(scales)
library(feather) # Usado para carregar arquivos
{% endhighlight %}

As linhas abaixo mostram como eu baixei os logs da mirror do RStudio do CRAN para o período entre 24 de Abril de 2016 a 24 de Maio a 2016. Os logs de cada dia desse período são salvos na pasta indicada no argumento `log_folder`, totalizando cerca de 250 MB. O dataframe gerado com o código gerado é enorme, por isso é recomendável removê-lo da memória após realizar os filtros desejados a partir dele.


{% highlight r %}
temp_dir <- download_RStudio_CRAN_data(START = '2016-04-24',END = '2016-05-24', log_folder="/home/sillas/R/data")
df_cran <- read_RStudio_CRAN_data("/home/sillas/R/data")
#save(df_cran, file = "/home/sillas/R/data/df_cran.Rdata")
#load("/home/sillas/R/data/df_cran.Rdata")

# Agregar logs por pacote:

# df_pkgs <- df_cran %>%
#   group_by(package) %>%
#   summarise(downloads = n()) %>%
#   arrange(desc(downloads)) %>%
#   mutate(downloads_acum = cumsum(downloads))

#rm(df_cran)
{% endhighlight %}

Para não ter de carregar o objeto `df_cran` toda vez que eu renderizo o arquivo markdown deste post, salvei uma cópia em disco do dataframe `df_pkgs`. Para isso, usei o pacote `feather`, que torna os processos de escrita e leitura de arquivos no R [muito rápidas](https://blog.rstudio.org/2016/03/29/feather/).


{% highlight r %}
#write_feather(df_pkgs, "df_pkgs.feather")
df_pkgs <- read_feather("/home/sillas/R/Projetos/PaixaoPorDados/sillasgonzaga.github.io/data/df_pkgs.feather")
print(df_pkgs, 10)
{% endhighlight %}



{% highlight text %}
## Source: local data frame [9,236 x 3]
## 
##          package downloads downloads_acum
##            <chr>     <int>          <int>
## 1  RcppArmadillo    425443         425443
## 2           Rcpp    285869         711312
## 3        ggplot2    246536         957848
## 4         digest    210749        1168597
## 5        stringr    207837        1376434
## 6           plyr    203498        1579932
## 7        stringi    202125        1782057
## 8       magrittr    195198        1977255
## 9         scales    194719        2171974
## 10      reshape2    182363        2354337
## ..           ...       ...            ...
{% endhighlight %}



{% highlight r %}
dim(df_pkgs)
{% endhighlight %}



{% highlight text %}
## [1] 9236    3
{% endhighlight %}

Temos que 9236 diferentes pacotes foram baixados no período analisado.  

Para determinar a quantidade de pacotes a serem analisados como membros de uma rede, usei o Princípio de Pareto:


{% highlight r %}
(total_downloads <- sum(df_pkgs$downloads))
{% endhighlight %}



{% highlight text %}
## [1] 17491320
{% endhighlight %}



{% highlight r %}
(limite80 <- total_downloads * 0.80)
{% endhighlight %}



{% highlight text %}
## [1] 13993056
{% endhighlight %}



{% highlight r %}
df_pkgs_pareto <- filter(df_pkgs, downloads_acum <= limite80)

nrow(df_pkgs_pareto)
{% endhighlight %}



{% highlight text %}
## [1] 335
{% endhighlight %}



{% highlight r %}
100*nrow(df_pkgs_pareto)/nrow(df_pkgs)
{% endhighlight %}



{% highlight text %}
## [1] 3.627111
{% endhighlight %}

Temos que 335 pacotes, cerca de 3,6% do total, equivalem a 80% de todos os downloads de pacotes nos últimos 30 dias, o que  mostra que a regra de Pareto é aplicável aqui e que, apesar de haver milhares de pacotes disponíveis no CRAN, a grande maioria deles não são baixados muitas vezes, como mostram as seguintes estatísticas:


{% highlight r %}
summary(df_pkgs$downloads)
{% endhighlight %}



{% highlight text %}
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##       1     137     182    1894     315  425400
{% endhighlight %}

O número mediano de downloads por usuário é de 182, muito distante dos 10 mais populares mostrados acima.

Os vinte pacotes mais baixados são:


{% highlight r %}
df_pkgs_pareto %>%
  top_n(20, wt = downloads) %>%
  ggplot(aes(x = reorder(package, downloads), y = downloads)) +
    geom_bar(stat = "identity", fill = "#014d64") +
    labs(x = "", y = "Quantidade de downloads\n entre Abril e Maio/2016") +
    scale_y_continuous(labels = comma) +
    coord_flip() +
    theme_economist() 
{% endhighlight %}

![center](/figs/metaanalise_1/unnamed-chunk-6-1.png)

Pessoalmente, não fiquei surpreso ao ver que, dos 6 pacotes mais baixados, 3 (ggplot2, stringr e plyr) fazem parte do Hadleyverse, ou seja, foram criados pelo gênio Hadley Wickham, que revolucionou o modo como o R é usado e é ídolo para muitos usuários da linguagem, como eu :).  

Após filtrar os pacotes que entrarão na análise, o pacote miniCRAN é usado para extrair as dependências de cada um e formar uma rede deles. A função `makeDepGraph` extrai as dependências dos pacotes indicados na função e cria um grafo. Por exemplo:


{% highlight r %}
pkgDep("ggplot2")
{% endhighlight %}



{% highlight text %}
##  [1] "ggplot2"       "digest"        "gtable"        "MASS"         
##  [5] "plyr"          "reshape2"      "scales"        "Rcpp"         
##  [9] "stringr"       "RColorBrewer"  "dichromat"     "munsell"      
## [13] "labeling"      "colorspace"    "stringi"       "magrittr"     
## [17] "jsonlite"      "rex"           "httr"          "crayon"       
## [21] "withr"         "memoise"       "mime"          "curl"         
## [25] "openssl"       "R6"            "lazyeval"      "lattice"      
## [29] "survival"      "Formula"       "latticeExtra"  "cluster"      
## [33] "rpart"         "nnet"          "acepack"       "foreign"      
## [37] "gridExtra"     "data.table"    "chron"         "Matrix"       
## [41] "maps"          "sp"            "nlme"          "mvtnorm"      
## [45] "TH.data"       "sandwich"      "codetools"     "zoo"          
## [49] "praise"        "SparseM"       "MatrixModels"  "evaluate"     
## [53] "formatR"       "highr"         "markdown"      "yaml"         
## [57] "knitr"         "htmltools"     "caTools"       "bitops"       
## [61] "gdtools"       "BH"            "covr"          "ggplot2movies"
## [65] "hexbin"        "Hmisc"         "mapproj"       "maptools"     
## [69] "mgcv"          "multcomp"      "testthat"      "quantreg"     
## [73] "rmarkdown"     "svglite"
{% endhighlight %}



{% highlight r %}
makeDepGraph("ggplot2")
{% endhighlight %}



{% highlight text %}
## IGRAPH DN-- 74 123 -- 
## + attr: name (v/c), type (e/c)
## + edges (vertex names):
##  [1] magrittr    ->rex          lazyeval    ->rex         
##  [3] jsonlite    ->httr         mime        ->httr        
##  [5] curl        ->httr         openssl     ->httr        
##  [7] R6          ->httr         memoise     ->crayon      
##  [9] digest      ->memoise      lattice     ->latticeExtra
## [11] RColorBrewer->latticeExtra gtable      ->gridExtra   
## [13] chron       ->data.table   Matrix      ->survival    
## [15] digest      ->ggplot2      gtable      ->ggplot2     
## + ... omitted several edges
{% endhighlight %}

Assim, dos 335 pacotes mais populares, são gerados dois grafos: o da esquerda, com o método `plot` com as modificações nativas realizadas pelo `miniCRAN` e o da direita, feita pelo pacote `igraph`.


{% highlight r %}
set.seed(123)
list_pkgs <- df_pkgs_pareto$package
g <- makeDepGraph(list_pkgs)

par(mfrow=(c(1,2)))
plot(g, vertex.size=10, cex=0.7, main = "") # método plot.pkgDepGraph

plot.igraph(g)
{% endhighlight %}

![center](/figs/metaanalise_1/unnamed-chunk-8-1.png)

Como pode-se ver, ambos os gráficos acima são visualmente poluídos e não dá para aprender muita coisa a partir deles. Além disso, a fim de analisar a centralidade de um pacote em um grafo, é importante saber o que o argumento `suggests` da função `makeDepGraph` significa. Segundo [Hadley Wickham](http://r-pkgs.had.co.nz/description.html), quando o pacote A sugere um outro pacote B, significa que o A *pode* usar o pacote B, mas ele não é requerido. Este pode ser usado para rodar testes, montar vignettes (tutoriais de pacotes), etc. 

Vamos, então, como fica o grafo dos 20 pacotes mais populares, com e sem `suggests`, com o pacote `ggplot2` destacado:


{% highlight r %}
list_pkgs <- df_pkgs$package[1:20]

par(mfrow=(c(1,2)))
g <- makeDepGraph(list_pkgs, enhances = FALSE, suggests = FALSE)
set.seed(123)
plot(g, pkgsToHighlight = "ggplot2",vertex.size=20, cex=1, main = "Argumento suggests = FALSE", legendPosition = NULL)

g <- makeDepGraph(list_pkgs, enhances = FALSE, suggests = TRUE)
set.seed(123)
plot(g, pkgsToHighlight = "ggplot2", vertex.size=20, cex=1, main = "Argumento suggests = TRUE", legendPosition = c(-1, -1))
{% endhighlight %}

![center](/figs/metaanalise_1/unnamed-chunk-9-1.png)

Agora sim já é possível aprender algumas coisas a partir do grafo. O sentido da linha vermelha indica que, por exemplo, o `ggplot2` importa vários pacotes(`digest`, `gtable`, `MASS`, `reshape2`, `plyr` e `scales`), mas não é importado por nenhum outro. Já o grafo da direita mostra que o `ggplot2` sugere muitos outros, o que aumenta sua centralidade na rede.

Conclusão: para realizar análises de centralidade de pacotes R, é necessário deixar o argumento `suggests` como `FALSE`.
