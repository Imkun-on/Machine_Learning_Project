<h1>Life Expectancy 2000-2015</h1>
  <img src="https://guardian.ng/wp-content/uploads/2018/06/Lifespan.-Photo-Wasteless-Future-e1530292285147.jpg" alt="Lifespan Photo Wasteless Future">
    <h2>Informazione sul dataset</h2>
    <ul>
    <li><strong>Stato:</strong> 119 nomi di nazioni.</li>
    <li><strong>Anno:</strong> l’anno, dal 2000 al 2015 (inclusi).</li>
    <li><strong>Continente:</strong> nomi dei differenti continenti.</li>
    <li><strong>Meno Sviluppato:</strong> se il valore è 'TRUE', la nazione è classificata come sottosviluppata, se il valore è 'FALSE', la nazione non è classificata come sottosviluppata.</li>
    <li><strong>Aspettativa di vita:</strong> valore dell'aspettativa di vita.</li>
    <li><strong>Popolazione:</strong> il numero totale di abitanti per stato.</li>
    <li><strong>Emissioni CO2:</strong> il numero totale di emissioni di CO2 per stato.</li>
    <li><strong>Spese Sanitarie:</strong> l'importo sanitario speso per assistenza sanitaria da uno stato.</li>
    <li><strong>Consumo energia elettrica:</strong> la quantità totale di energia elettrica consumata da uno stato.</li>
    <li><strong>Area Forestale:</strong> la superficie totale delle foreste in uno stato.</li>
    <li><strong>PIL Pro Capitale:</strong> il prodotto interno lordo per capita in uno stato.</li>
    <li><strong>Individui che usano Internet:</strong> il numero totale di persone che utilizzano Internet in uno stato.</li>
    <li><strong>Spese Militari:</strong> la quantità di denaro speso per i militari in uno stato.</li>
    <li><strong>Persone che usano servizi di acqua potabile:</strong> il numero totale di persone che utilizzano servizi di acqua potabile in uno stato.</li>
    <li><strong>Obesità sugli adulti:</strong> la percentuale di adulti obesi in uno stato.</li>
    <li><strong>Consumo Birra PRO Capitale:</strong> la quantità di birra consumata dalle persone in uno stato.</li>
    </ul>
    <h1> Obiettivo principale </h1>
    <p> Prevedere la variabile "Aspettativa di vita" </p>
    # Importazione Librerie
    if (!require("corrplot")) install.packages("corrplot")
  if (!require("ggplot2")) install.packages("ggplot2")
  if (!require("plotly")) install.packages("plotly")
  if (!require("dplyr")) install.packages("dplyr")
  if (!require("countrycode")) install.packages("countrycode")
  if (!require("reshape2")) install.packages("reshape2")
  if (!require("gridExtra")) install.packages("gridExtra")
  if (!require("caret")) install.packages("caret")
  library(corrplot)
  library(ggplot2)
  library(plotly)
  library(dplyr)
  library(countrycode)
  library(reshape2)
  library(caret)
  # Load data from a CSV file
  Life_Expectancy_00_15 <- read.csv("/kaggle/input/life-expectancy-2000-2015/Life_Expectancy_00_15.csv", sep = ";")
  
  # Display the first few rows of the dataframe
  head(Life_Expectancy_00_15)
  str(Life_Expectancy_00_15)
  summary(Life_Expectancy_00_15)
  
  sum(is.na(Life_Expectancy_00_15))
  sum(duplicated(Life_Expectancy_00_15))
  data_num <- Life_Expectancy_00_15[sapply(Life_Expectancy_00_15, is.numeric)]
  corr_matrix <- cor(data_num, use = "complete.obs")
  
  # Trasformo la matrice di correlazione in un formato adatto per ggplot2
  melted_corr_matrix <- melt(corr_matrix)
  
  # Creo il heatmap con ggplot2
  p <- ggplot(data = melted_corr_matrix, aes(x=Var1, y=Var2, fill=value)) +
    geom_tile(color = "white") +  # Aggiunge le linee bianche tra i tiles
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                         midpoint = 0, limit = c(-1,1), space = "Lab",
                         name="Correlation") +
    theme_minimal() +  # Tema minimale per un aspetto pulito
    theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1),  # Ruota le etichette x per migliorare la leggibilità
          axis.text.y = element_text(size = 12)) +
    labs(x='', y='', title="Correlation Matrix")  # Imposta i titoli e le etichette
  
  # Visualizzo il grafico
  print(p)
  
  
  # Assumiamo che Life_Expectancy_00_15 sia un data frame già esistente che contiene i dati necessari
  # Calcolo la media dell'aspettativa di vita per anno e continente
  avg_life_expectancy <- Life_Expectancy_00_15 %>%
    group_by(Year, Continent) %>%
    summarise(avg_life_expectancy = mean(Life.Expectancy, na.rm = TRUE), .groups = 'drop')
  
  # Creo un grafico a linee interattivo con plotly
  fig <- plot_ly(data = avg_life_expectancy, x = ~Year, y = ~avg_life_expectancy, color = ~Continent, type = 'scatter', mode = 'lines+markers') %>%
    layout(title = 'Tendenza temporale dell\'aspettativa di vita per continente',
           xaxis = list(title = 'Anno'),
           yaxis = list(title = 'Aspettativa di vita media'))
  
  # Visualizzo il grafico
  fig
  
  
  # Carico la libreria necessaria
  library(plotly)
  
  # Assumiamo che Life_Expectancy_00_15 sia un data frame già esistente che contiene i dati necessari
  # Creo uno scatterplot
  fig <- plot_ly(data = Life_Expectancy_00_15, x = ~`Life.Expectancy`, y = ~`CO2.emissions`,
                 type = 'scatter', mode = 'markers',
                 color = ~Continent, colors = RColorBrewer::brewer.pal(8, "Pastel1"),
                 marker = list(size = 7),
                 height = 600, width = 1000) %>%  # Sposto qui la definizione delle dimensioni
    layout(title = list(text = 'Aspettativa di vita Vs Emissione CO2', x = 0.5),
           xaxis = list(title = 'Aspettativa di vita'),
           yaxis = list(title = 'Emissione CO2'),
           legend = list(title = 'Continente'))
  
  # Visualizzo il grafico
  fig
  
  # Carico la libreria necessaria
  library(plotly)
  
  # Assumiamo che Life_Expectancy_00_15 sia un data frame già esistente che contiene i dati necessari
  # Creo uno scatterplot
  fig <- plot_ly(data = Life_Expectancy_00_15, x = ~`Life.Expectancy`, y = ~`Health.expenditure`,
                 type = 'scatter', mode = 'markers',
                 color = ~Continent, colors = RColorBrewer::brewer.pal(8, "Pastel1"),
                 marker = list(size = 7),
                 height = 600, width = 1000) %>%  # Definisco le dimensioni direttamente qui
    layout(title = list(text = 'Aspettativa di vita Vs Spese sanitarie', x = 0.5),
           xaxis = list(title = 'Aspettativa di vita'),
           yaxis = list(title = 'Spese sanitarie'),
           legend = list(title = 'Continente'))
  
  # Visualizzo il grafico
  fig
  
  # Carico la libreria necessaria
  library(plotly)
  
  # Assumiamo che Life_Expectancy_00_15 sia un data frame già esistente che contiene i dati necessari
  # Creo uno scatterplot
  fig <- plot_ly(data = Life_Expectancy_00_15, x = ~`Life.Expectancy`, y = ~`Electric.power.consumption`,
                 type = 'scatter', mode = 'markers',
                 color = ~Continent, colors = RColorBrewer::brewer.pal(8, "Pastel1"),
                 marker = list(size = 7),
                 height = 600, width = 1000) %>%  # Definisco le dimensioni direttamente qui
    layout(title = list(text = 'Aspettativa di vita Vs Consumo di energia elettrica', x = 0.5),
           xaxis = list(title = 'Aspettativa di vita'),
           yaxis = list(title = 'Consumo di energia elettrica'),
           legend = list(title = 'Continente'))
  
  # Visualizzo il grafico
  fig
  
  # Carico la libreria necessaria
  library(plotly)
  
  # Assumiamo che Life_Expectancy_00_15 sia un data frame già esistente che contiene i dati necessari
  # Creo uno scatterplot
  fig <- plot_ly(data = Life_Expectancy_00_15, x = ~`Life.Expectancy`, y = ~`GDP.per.capita`,
                 type = 'scatter', mode = 'markers',
                 color = ~Continent, colors = RColorBrewer::brewer.pal(8, "Pastel1"),
                 marker = list(size = 7),
                 height = 600, width = 1000) %>%  # Definisco le dimensioni direttamente qui
    layout(title = list(text = 'Aspettativa di vita Vs PIL Pro Capitale', x = 0.5),
           xaxis = list(title = 'Aspettativa di vita'),
           yaxis = list(title = 'PIL Pro Capitale'),
           legend = list(title = 'Continente'))
  
  # Visualizzo il grafico
  fig
  
  # Carico la libreria necessaria
  library(plotly)
  
  # Assumiamo che Life_Expectancy_00_15 sia un data frame già esistente che contiene i dati necessari
  # Creo uno scatterplot
  fig <- plot_ly(data = Life_Expectancy_00_15, x = ~`Life.Expectancy`, y = ~`Individuals.using.the.Internet`,
                 type = 'scatter', mode = 'markers',
                 color = ~Continent, colors = RColorBrewer::brewer.pal(8, "Pastel1"),
                 marker = list(size = 7),
                 height = 600, width = 1000) %>%  # Definisco le dimensioni direttamente qui
    layout(title = list(text = 'Aspettativa di vita Vs Individui che usano Internet', x = 0.5),
           xaxis = list(title = 'Aspettativa di vita'),
           yaxis = list(title = 'Individui che usano Internet'),
           legend = list(title = 'Continente'))
  
  # Visualizzo il grafico
  fig
  
  # Carico la libreria necessaria
  library(plotly)
  
  # Assumiamo che Life_Expectancy_00_15 sia un data frame già esistente che contiene i dati necessari
  # Creo uno scatterplot
  fig <- plot_ly(data = Life_Expectancy_00_15, 
                 x = ~`Life.Expectancy`, 
                 y = ~`People.using.at.least.basic.drinking.water.services`,
                 type = 'scatter', 
                 mode = 'markers',
                 color = ~Continent, 
                 colors = RColorBrewer::brewer.pal(8, "Pastel1"),
                 marker = list(size = 7),
                 height = 600, 
                 width = 1000) %>%  
    layout(title = list(text = 'Aspettativa di vita Vs Persone che usano servizi idrici', x = 0.5),
           xaxis = list(title = 'Aspettativa di vita'),
           yaxis = list(title = 'Persone che usano servizi idrici'),
           legend = list(title = 'Continente'))
  
  # Visualizzo il grafico
  fig
  
  # Carico la libreria necessaria
  library(plotly)
  
  # Assumiamo che Life_Expectancy_00_15 sia un data frame già esistente che contiene i dati necessari
  # Creo uno scatterplot
  fig <- plot_ly(data = Life_Expectancy_00_15, 
                 x = ~`Life.Expectancy`, 
                 y = ~`Obesity.among.adults`,
                 type = 'scatter', 
                 mode = 'markers',
                 color = ~Continent, 
                 colors = RColorBrewer::brewer.pal(8, "Pastel1"),
                 marker = list(size = 7),
                 height = 600, 
                 width = 1000) %>%  
    layout(title = list(text = 'Aspettativa di vita Vs Obesità sugli adulti', x = 0.5),
           xaxis = list(title = 'Aspettativa di vita'),
           yaxis = list(title = 'Obesità sugli adulti'),
           legend = list(title = 'Continente'))
  
  # Visualizzo il grafico
  fig
  
  if (!require("RColorBrewer")) install.packages("RColorBrewer")
  if (!require("gridExtra")) install.packages("gridExtra")
  # Carico la libreria necessaria
  library(plotly)
  library(RColorBrewer)  # Assicurati di avere questa libreria installata
  
  # Assumiamo che Life_Expectancy_00_15 sia un data frame già esistente che contiene i dati necessari
  # Creo uno scatterplot, assicurati che i nomi delle colonne corrispondano ai nomi reali nel tuo dataframe
  fig <- plot_ly(data = Life_Expectancy_00_15, 
                 x = ~`Life.Expectancy`,  # Sostituisci con il nome reale della colonna, se diverso
                 y = ~`Beer.consumption.per.capita`,  # Sostituisci con il nome reale della colonna, se diverso
                 type = 'scatter', 
                 mode = 'markers',
                 color = ~Continent, 
                 colors = RColorBrewer::brewer.pal(8, "Pastel1"),
                 marker = list(size = 7),
                 height = 600, 
                 width = 1000) %>%
    layout(title = list(text = 'Aspettativa di vita Vs Consumo Birra pro capite', x = 0.5),
           xaxis = list(title = 'Aspettativa di vita'),
           yaxis = list(title = 'Consumo birra pro capite'),
           legend = list(title = 'Continente'))
  
  # Visualizzo il grafico
  fig
  
  <h1>Conclusioni sugli sctterplot</h1>
    <p>Le persone che vivono in paesi a basse emissioni di carbonio possono raggiungere un'aspettativa di vita ragionevolmente elevata, ma non possono raggiungere alti livelli di reddito.</p>
<p>Fino a poco tempo fa, si riteneva vere le seguenti correlazioni: lo sviluppo umano dipende dalla crescita economica, la crescita economica richiede energia aggiuntiva e quindi porta a un aumento delle emissioni di gas serra.</p>
<p>Di conseguenza, l'umanità non può evolversi senza sfruttare ulteriormente le risorse della Terra.</p>
    
    </p>Tuttavia, queste correlazioni non sono né così forti, né universalmente valide come si pensava inizialmente.</p>
    
    </p>Queste dipendenze reciproche sono oggetto di numerose indagini.</p>
    
    </p>Julia K. Steinberger (University of Leeds & Alpen-Adria-Universität), J. Timmons Roberts, Glen P. Peters e Giovanni Baiocchi hanno pubblicato congiuntamente uno studio su Nature Climate Change, in cui esplorano i legami tra le emissioni di carbonio e lo sviluppo umano.</p>
    
    <p>Durante le loro indagini, l'aspettativa di vita, il reddito e le emissioni di carbonio sono stati visti in termini di come si relazionano tra loro.</p>

<p>Inoltre, le emissioni sono state classificate come emissioni territoriali (ad esempio dovute alla produzione industriale nel rispettivo paese) o basate sul consumo (i valori netti sono derivati sommando i valori delle emissioni importate e sottraendo i valori delle emissioni esportate in base al carbonio incorporato nei beni e nei servizi). Nel complesso, è possibile dimostrare – in accordo con quest'ultimo approccio – che la maggior parte dei paesi esportatori di carbonio, come quelli dell'ex Unione Sovietica, dell'Europa orientale, del Medio Oriente o del Sud Africa, si trovano nella fascia media, sia in termini di aspettativa di vita che in termini di reddito.</p>
    
    <p>I paesi importatori di carbonio, tuttavia, rappresentano un gruppo eccezionalmente eterogeneo, costituito da due estremi: da un lato, questi sono i paesi più poveri, costretti a importare costosi combustibili fossili e beni prodotti ad alta intensità di carbonio.</p>
    
    <p>D'altra parte, questo gruppo comprende quei paesi con lo status socio-economico più sviluppato, la più alta aspettativa di vita e un reddito medio pro capite più elevato.</p>

<p>La ricerca ha rivelato che, sebbene le emissioni di carbonio sia territoriali che basate sul consumo siano altamente correlate con lo sviluppo umano, la forma e la forza della relazione tra emissioni di carbonio e reddito è completamente diversa dalla relazione tra emissioni di carbonio e aspettativa di vita.</p>

<p>Il confronto tra paesi mostra che, sebbene sia possibile ottenere contemporaneamente basse emissioni di carbonio e un'elevata aspettativa di vita, ciò vale solo quando il reddito della popolazione è moderato. Gli obiettivi economici e ambientali sembrano contraddirsi a vicenda; questo sembra certamente essere il caso dei valori più elevati del PIL pro capite. Inoltre, i ricercatori sono stati in grado di dimostrare che in questi paesi esiste un'ampia varietà di opportunità di sviluppo, che non seguono necessariamente le tendenze globali. </p>
<h1> Gestione outliers </h1>
<p> Prima di procedere con la previsione dell'aspettativa di vita, esaminerò le variabili correlate con l'aspettativa di vita per identificare la presenza di valori anomali (outliers). In presenza di outliers, applicherò il metodo di winsorizzazione, che modifica i valori estremi al di fuori del primo e terzo quantile per adattarli più strettamente alla distribuzione centrale.</p>
<p>Questo processo mira a minimizzare l'impatto degli outliers sui risultati finali ottenuti dai modelli di regressione.</p>
    col_dict <- list('Year' = 1, 'Life.Expectancy' = 4, 'Population' = 5, 'CO2.emissions' = 6, 'Health.expenditure' = 7, 'Electric.power.consumption' = 8, 'Forest.area' = 9, 'GDP.per.capita' = 10, 'Individuals.using.the.Internet' = 11, 'Military.expenditure' = 12, 'People.practicing.open.defecation' = 13, 'People.using.at.least.basic.drinking.water.services' = 14, 'Obesity.among.adults' = 15, 'Beer.consumption.per.capita' = 16)
  
  for (variable in names(col_dict)) {
    q <- quantile(Life_Expectancy_00_15[[variable]], c(0.25, 0.75))
    iqr <- diff(q)
    
    min_val <- q[1] - (iqr * 1.5)
    max_val <- q[2] + (iqr * 1.5)
    
    outliers <- Life_Expectancy_00_15[[variable]][Life_Expectancy_00_15[[variable]] < min_val | Life_Expectancy_00_15[[variable]] > max_val]
    outliers_count <- length(outliers)
    outliers_percentage <- outliers_count * 100 / 1987
    
    print(paste("Number of outliers and percentage of it in", variable, ":", outliers_count, "and", outliers_percentage, "%"))
  }
  
  winsorize <- function(data, percentile_lower=0.05, percentile_upper=0.95) {
    # Winsorizes numerical columns in a DataFrame.
    
    # Arguments:
    #   data (data.frame): The DataFrame to winsorize.
    #   percentile_lower (numeric, optional): The percentile threshold for lower winsorization. Defaults to 0.05.
    #   percentile_upper (numeric, optional): The percentile threshold for upper winsorization. Defaults to 0.95.
    
    numerical_columns <- data[, sapply(data, is.numeric)]
    
    for (col in names(numerical_columns)) {
      lower_bound <- quantile(data[[col]], percentile_lower)
      upper_bound <- quantile(data[[col]], percentile_upper)
      
      data[[col]][data[[col]] < lower_bound] <- lower_bound
      data[[col]][data[[col]] > upper_bound] <- upper_bound
    }
    
    return(data)
  }
  
  Life_Expectancy_00_15_winsorized <- winsorize(Life_Expectancy_00_15)
  
  par(mfrow=c(5,4), mar=c(4, 4, 2, 1), oma=c(0, 0, 2, 0))
  
  for (variable in names(col_dict)) {
    i <- col_dict[[variable]]
    boxplot(Life_Expectancy_00_15_winsorized[[variable]], main=variable, outline=FALSE, whisklty=1.5)
  }
  
  title("Boxplot per le variabili", outer=TRUE)
  
  # Seleziono solo le colonne numeriche nel DataFrame winsorizzato
  data_num_W <- Life_Expectancy_00_15_winsorized[, sapply(Life_Expectancy_00_15_winsorized, is.numeric)]
  head(data_num_W)
  <h1>Codifica dei dati</h1>
    <p>Il dataset contiene diverse variabili categoriche che necessitano di essere trasformate per le analisi successive. Il procedimento che adotteremo è la codifica numerica: questa operazione consiste nell'assegnare a ciascuna categoria un corrispondente valore numerico unico, garantendo così che ogni categoria sia rappresentata da un numero specifico.</p>
# Trasformo le colonne "Country", "Least Developed" e "Continent" in numeri interi
Life_Expectancy_00_15_winsorized$Country <- as.integer(as.factor(Life_Expectancy_00_15_winsorized$Country))
Life_Expectancy_00_15_winsorized$Least.Developed <- as.integer(as.factor(Life_Expectancy_00_15_winsorized$Least.Developed))
Life_Expectancy_00_15_winsorized$Continent <- as.integer(as.factor(Life_Expectancy_00_15_winsorized$Continent))
str(Life_Expectancy_00_15_winsorized)
<h1>Suddivisione dei dati</h1>
# Definisco una lista delle colonne da escludere in X
colonne_da_escludere <- c('Life.Expectancy', 'Population', 'Military.expenditure', 'People.practicing.open.defecation', 'Forest.area')
# Seleziono solo le colonne da escludere in X
X <- Life_Expectancy_00_15_winsorized[, !names(Life_Expectancy_00_15_winsorized) %in% colonne_da_escludere]
# Definisco la variabile y contenente solo la colonna "Life_Expectancy" del DataFrame
y <- Life_Expectancy_00_15_winsorized$Life.Expectancy
head(X)
head(y)
# Divido il dataset in training set e test set
set.seed(1) # Imposto il seed per la riproducibilità dei risultati
train_indices <- sample(1:nrow(X), 0.8*nrow(X)) # 80% dei dati per il training set
X_train <- X[train_indices, ]
X_test <- X[-train_indices, ]
y_train <- y[train_indices]
y_test <- y[-train_indices]
cat("Dimensioni del training set:", dim(X_train), dim(y_train), "\n")
cat("Dimensioni del test set:", dim(X_test), dim(y_test), "\n")
<h1>Normalizzazione dei Dati</h1>
<p>Prima di procedere con la creazione dei modelli di regressione, è essenziale normalizzare i dati. La necessità di questo passaggio deriva dall'esistenza di variabili nel dataset che sono espresse in unità di misura diverse. Per garantire che ogni variabile contribuisca equamente al modello, procederemo con la standardizzazione dell'intero dataset, portando ogni variabile ad avere una media di 0 e una deviazione standard di 1. Questo approccio facilita l'applicazione di tecniche di regressione, migliorando l'efficacia e l'accuratezza dei modelli predittivi.</p>
    
    # Calcolo delle medie e deviazioni standard per la standardizzazione
    preProcessParams <- preProcess(X_train, method = c("center", "scale"))
  
  # Applica la standardizzazione ai dati di addestramento
  X_train <- predict(preProcessParams, X_train)
  
  # Applica la stessa standardizzazione ai dati di test
  X_test <- predict(preProcessParams, X_test)
  # Creazione di un data frame pandas da X_train standardizzato
  df_train <- as.data.frame(X_train)
  head(df_train)
  # Creazione di un data frame pandas da X_test standardizzato
  df_test <- as.data.frame(X_test)
  head(df_test)
  <h1>Modelli di Machine Learning per la Previsione dell'Aspettativa di Vita</h1>
<p>Per prevedere la variabile "Aspettativa di vita", impiego diversi modelli di regressione. Analizzo, per ciascun modello, un confronto tra i valori osservati e quelli previsti, esaminando le metriche di performance sia sulla fase di addestramento (training) che di validazione (test). Infine, confronto i modelli per determinare quale offre le migliori previsioni, basandomi sul coefficiente R<sup>2</sup> e assicurandomi l'assenza di overfitting.</p>
    <p>I modelli di regressione esaminati includono:</p>
    <ul>
    <li>Regressione Lineare</li>
    <li>Regressione Lasso</li>
    <li>Regressione Ridge</li>
    <li>Regressione AdaBoost</li>
    <li>Regressione Gradiente</li>
    <li>Foresta Casuale</li>
    <li>GradientBoosting</li>
    <li>Albero di Decisione</li>
    <li>XgBoost</li>
    <li>Elastic Net</li>
    <li>Lasso</li>
    <li>AdaBoost</li>
    <li>SGD</li>
    <li>HistGradientBoosting</li>
    <li>KNN</li>
    <li>Extratree</li>
    <li>SVM</li>    
    </ul>
    # Gradient Boosting Regressor
    library(gbm)
  # Inizializzo il modello Gradient Boosting Regressor
  model <- gbm(y_train ~ ., data = as.data.frame(cbind(y_train, X_train)), distribution = "gaussian",
               n.trees = 100, shrinkage = 0.1, interaction.depth = 3, n.minobsinnode = 10, verbose = FALSE)
  
  # Previsioni sui set di addestramento e di test
  ytrain_pred <- predict(model, newdata = as.data.frame(X_train), n.trees = 100)
  ytest_pred <- predict(model, newdata = as.data.frame(X_test), n.trees = 100)
  
  # Calcolo e stampo le metriche per il training set
  cat("Metriche per il training set:\n")
  cat("R^2:", cor(y_train, ytrain_pred)^2, "\n")
  cat("MAE:", mean(abs(y_train - ytrain_pred)), "\n")
  cat("MSE:", mean((y_train - ytrain_pred)^2), "\n")
  cat("RMSE:", sqrt(mean((y_train - ytrain_pred)^2)), "\n")
  cat("EVS:", cor(y_train, ytrain_pred, method = "spearman")^2, "\n\n")
  
  # Calcolo e stampo le metriche per il test set
  cat("Metriche per il test set:\n")
  cat("R^2:", cor(y_test, ytest_pred)^2, "\n")
  cat("MAE:", mean(abs(y_test - ytest_pred)), "\n")
  cat("MSE:", mean((y_test - ytest_pred)^2), "\n")
  cat("RMSE:", sqrt(mean((y_test - ytest_pred)^2)), "\n")
  cat("EVS:", cor(y_test, ytest_pred, method = "spearman")^2, "\n\n")
  
  cat("Confronto tra valori osservati e previsti per i primi 10 elementi del test set:\n")
  for (i in 1:10) {
    cat("Osservato:", y_test[i], ", Previsto:", ytest_pred[i], "\n")
  }
  
  # SVR Regressor
  # Carico il pacchetto per il modello di regressione SVR
  library(e1071)
  
  # Inizializzo il modello SVR
  model_svr <- svm(y_train ~ ., data = as.data.frame(cbind(y_train, X_train)), kernel = "radial", cost = 100, gamma = 0.1, epsilon = 0.1)
  
  # Previsioni sui set di addestramento e di test
  ytrain_pred_svr <- predict(model_svr, newdata = as.data.frame(X_train))
  ytest_pred_svr <- predict(model_svr, newdata = as.data.frame(X_test))
  
  # Calcolo e stampo le metriche per il training set
  cat("Metriche per il training set:\n")
  cat("R^2:", cor(y_train, ytrain_pred_svr)^2, "\n")
  cat("MAE:", mean(abs(y_train - ytrain_pred_svr)), "\n")
  cat("MSE:", mean((y_train - ytrain_pred_svr)^2), "\n")
  cat("RMSE:", sqrt(mean((y_train - ytrain_pred_svr)^2)), "\n")
  cat("EVS:", cor(y_train, ytrain_pred_svr, method = "spearman")^2, "\n\n")
  
  # Calcolo e stampo le metriche per il test set
  cat("Metriche per il test set:\n")
  cat("R^2:", cor(y_test, ytest_pred_svr)^2, "\n")
  cat("MAE:", mean(abs(y_test - ytest_pred_svr)), "\n")
  cat("MSE:", mean((y_test - ytest_pred_svr)^2), "\n")
  cat("RMSE:", sqrt(mean((y_test - ytest_pred_svr)^2)), "\n")
  cat("EVS:", cor(y_test, ytest_pred_svr, method = "spearman")^2, "\n\n")
  
  cat("Confronto tra valori osservati e previsti per i primi 10 elementi del test set:\n")
  for (i in 1:10) {
    cat("Osservato:", y_test[i], ", Previsto:", ytest_pred_svr[i], "\n")
  }
  
  # KNN Regressor
  # Carico il pacchetto per il modello di regressione KNN
  library(caret)
  
  # Inizializzo il modello KNeighborsRegressor
  model_knn <- train(X_train, y_train, method = "knn", trControl = trainControl(method = "cv"))
  
  # Previsioni sui set di addestramento e di test
  ytrain_pred_knn <- predict(model_knn, newdata = as.data.frame(X_train))
  ytest_pred_knn <- predict(model_knn, newdata = as.data.frame(X_test))
  
  # Calcolo e stampo le metriche per il training set
  cat("Metriche per il training set:\n")
  cat("R^2:", cor(y_train, ytrain_pred_knn)^2, "\n")
  cat("MAE:", mean(abs(y_train - ytrain_pred_knn)), "\n")
  cat("MSE:", mean((y_train - ytrain_pred_knn)^2), "\n")
  cat("RMSE:", sqrt(mean((y_train - ytrain_pred_knn)^2)), "\n")
  cat("EVS:", cor(y_train, ytrain_pred_knn, method = "spearman")^2, "\n\n")
  
  # Calcolo e stampo le metriche per il test set
  cat("Metriche per il test set:\n")
  cat("R^2:", cor(y_test, ytest_pred_knn)^2, "\n")
  cat("MAE:", mean(abs(y_test - ytest_pred_knn)), "\n")
  cat("MSE:", mean((y_test - ytest_pred_knn)^2), "\n")
  cat("RMSE:", sqrt(mean((y_test - ytest_pred_knn)^2)), "\n")
  cat("EVS:", cor(y_test, ytest_pred_knn, method = "spearman")^2, "\n\n")
  
  cat("Confronto tra valori osservati e previsti per i primi 10 elementi del test set:\n")
  for (i in 1:10) {
    cat("Osservato:", y_test[i], ", Previsto:", ytest_pred_knn[i], "\n")
  }
  
  # XGB Regressor
  # Carico il pacchetto per il modello XGBoost
  library(xgboost)
  
  # Inizializzo il modello XGBoost Regressor
  model_xgb <- xgboost(data = as.matrix(X_train), label = y_train, nrounds = 100, eta = 0.1, verbose = FALSE)
  
  # Previsioni sui set di addestramento e di test
  ytrain_pred_xgb <- predict(model_xgb, as.matrix(X_train))
  ytest_pred_xgb <- predict(model_xgb, as.matrix(X_test))
  
  # Calcolo e stampo le metriche per il training set
  cat("Metriche per il training set (XGBoost):\n")
  cat("R^2:", cor(y_train, ytrain_pred_xgb)^2, "\n")
  cat("MAE:", mean(abs(y_train - ytrain_pred_xgb)), "\n")
  cat("MSE:", mean((y_train - ytrain_pred_xgb)^2), "\n")
  cat("RMSE:", sqrt(mean((y_train - ytrain_pred_xgb)^2)), "\n")
  cat("EVS:", cor(y_train, ytrain_pred_xgb, method = "spearman")^2, "\n\n")
  
  # Calcolo e stampo le metriche per il test set
  cat("Metriche per il test set (XGBoost):\n")
  cat("R^2:", cor(y_test, ytest_pred_xgb)^2, "\n")
  cat("MAE:", mean(abs(y_test - ytest_pred_xgb)), "\n")
  cat("MSE:", mean((y_test - ytest_pred_xgb)^2), "\n")
  cat("RMSE:", sqrt(mean((y_test - ytest_pred_xgb)^2)), "\n")
  cat("EVS:", cor(y_test, ytest_pred_xgb, method = "spearman")^2, "\n\n")
  
  # Stampo il confronto tra valori osservati e previsti per i primi 10 elementi del test set
  cat("Confronto tra valori osservati e previsti per i primi 10 elementi del test set (XGBoost):\n")
  for (i in 1:10) {
    cat("Osservato:", y_test[i], ", Previsto:", ytest_pred_xgb[i], "\n")
  }
  
  # RandomForestRegressor
  # Carico il pacchetto per il modello Random Forest
  library(randomForest)
  
  # Inizializzo il modello RandomForestRegressor
  model_rf <- randomForest(X_train, y_train, ntree = 100, importance = TRUE)
  
  # Previsioni sui set di addestramento e di test
  ytrain_pred_rf <- predict(model_rf, newdata = X_train)
  ytest_pred_rf <- predict(model_rf, newdata = X_test)
  
  # Calcolo e stampo le metriche per il training set
  cat("Metriche per il training set (Random Forest):\n")
  cat("R^2:", cor(y_train, ytrain_pred_rf)^2, "\n")
  cat("MAE:", mean(abs(y_train - ytrain_pred_rf)), "\n")
  cat("MSE:", mean((y_train - ytrain_pred_rf)^2), "\n")
  cat("RMSE:", sqrt(mean((y_train - ytrain_pred_rf)^2)), "\n")
  cat("EVS:", cor(y_train, ytrain_pred_rf, method = "spearman")^2, "\n\n")
  
  # Calcolo e stampo le metriche per il test set
  cat("Metriche per il test set (Random Forest):\n")
  cat("R^2:", cor(y_test, ytest_pred_rf)^2, "\n")
  cat("MAE:", mean(abs(y_test - ytest_pred_rf)), "\n")
  cat("MSE:", mean((y_test - ytest_pred_rf)^2), "\n")
  cat("RMSE:", sqrt(mean((y_test - ytest_pred_rf)^2)), "\n")
  cat("EVS:", cor(y_test, ytest_pred_rf, method = "spearman")^2, "\n\n")
  
  # Stampo il confronto tra valori osservati e previsti per i primi 10 elementi del test set
  cat("Confronto tra valori osservati e previsti per i primi 10 elementi del test set (Random Forest):\n")
  for (i in 1:10) {
    cat("Osservato:", y_test[i], ", Previsto:", ytest_pred_rf[i], "\n")
  }
  
  # DecisionTreeRegressor
  # Carico il pacchetto per il modello Decision Tree
  library(rpart)
  
  # Inizializzo il modello DecisionTreeRegressor
  model_dt <- rpart(y_train ~ ., data = as.data.frame(cbind(y_train, X_train)), method = "anova", control = rpart.control(minsplit = 2, minbucket = 1, cp = 0))
  
  # Previsioni sui set di addestramento e di test
  ytrain_pred_dt <- predict(model_dt, newdata = as.data.frame(X_train))
  ytest_pred_dt <- predict(model_dt, newdata = as.data.frame(X_test))
  
  # Calcolo e stampo le metriche per il training set
  cat("Metriche per il training set (Decision Tree):\n")
  cat("R^2:", cor(y_train, ytrain_pred_dt)^2, "\n")
  cat("MAE:", mean(abs(y_train - ytrain_pred_dt)), "\n")
  cat("MSE:", mean((y_train - ytrain_pred_dt)^2), "\n")
  cat("RMSE:", sqrt(mean((y_train - ytrain_pred_dt)^2)), "\n")
  cat("EVS:", cor(y_train, ytrain_pred_dt, method = "spearman")^2, "\n\n")
  
  # Calcolo e stampo le metriche per il test set
  cat("Metriche per il test set (Decision Tree):\n")
  cat("R^2:", cor(y_test, ytest_pred_dt)^2, "\n")
  cat("MAE:", mean(abs(y_test - ytest_pred_dt)), "\n")
  cat("MSE:", mean((y_test - ytest_pred_dt)^2), "\n")
  cat("RMSE:", sqrt(mean((y_test - ytest_pred_dt)^2)), "\n")
  cat("EVS:", cor(y_test, ytest_pred_dt, method = "spearman")^2, "\n\n")
  
  # Stampo il confronto tra valori osservati e previsti per i primi 10 elementi del test set
  cat("Confronto tra valori osservati e previsti per i primi 10 elementi del test set (Decision Tree):\n")
  for (i in 1:10) {
    cat("Osservato:", y_test[i], ", Previsto:", ytest_pred_dt[i], "\n")
  }
  
  # AdaBoostRegressor
  library(gbm)
  
  # Configurazione del modello GBM per la regressione
  set.seed(30)  # Per la riproducibilità
  model_gbm <- gbm(y_train ~ ., 
                   data = as.data.frame(cbind(y_train, X_train)), 
                   distribution = "gaussian", 
                   n.trees = 100,
                   interaction.depth = 3,
                   shrinkage = 0.1,
                   cv.folds = 5,
                   n.minobsinnode = 10,
                   verbose = FALSE)
  
  # Previsioni
  ytrain_pred_gbm <- predict(model_gbm, newdata = X_train, n.trees = 100)
  ytest_pred_gbm <- predict(model_gbm, newdata = X_test, n.trees = 100)
  
  # Calcolo e stampa delle metriche
  calculate_metrics <- function(actual, predicted, data_type = "Training") {
    cat(sprintf("Metriche per il %s set (GBM):\n", data_type))
    cat(sprintf("R^2: %.3f\n", cor(actual, predicted)^2))
    cat(sprintf("MAE: %.3f\n", mean(abs(actual - predicted))))
    cat(sprintf("MSE: %.3f\n", mean((actual - predicted)^2)))
    cat(sprintf("RMSE: %.3f\n", sqrt(mean((actual - predicted)^2))))
    cat(sprintf("EVS: %.3f\n\n", cor(actual, predicted, method = "spearman")^2))
  }
  
  calculate_metrics(y_train, ytrain_pred_gbm, "Training")
  calculate_metrics(y_test, ytest_pred_gbm, "Test")
  
  # Stampa del confronto tra valori osservati e previsti per i primi 10 elementi del test set
  cat("Confronto tra valori osservati e previsti per i primi 10 elementi del test set (GBM):\n")
  for (i in 1:min(10, length(y_test))) {
    cat(sprintf("Osservato: %.3f, Previsto: %.3f\n", y_test[i], ytest_pred_gbm[i]))
  }
  
  # Linear Regressor
  # Inizializzo il modello di Regressione Lineare
  model_lr <- lm(y_train ~ ., data = as.data.frame(cbind(y_train, X_train)))
  
  # Previsioni sui set di addestramento e di test
  ytrain_pred_lr <- predict(model_lr, newdata = as.data.frame(X_train))
  ytest_pred_lr <- predict(model_lr, newdata = as.data.frame(X_test))
  
  # Calcolo e stampo le metriche per il training set
  cat("Metriche per il training set (Regressione Lineare):\n")
  cat("R^2:", summary(model_lr)$r.squared, "\n")
  cat("MAE:", mean(abs(y_train - ytrain_pred_lr)), "\n")
  cat("MSE:", mean((y_train - ytrain_pred_lr)^2), "\n")
  cat("RMSE:", sqrt(mean((y_train - ytrain_pred_lr)^2)), "\n")
  cat("EVS:", cor(y_train, ytrain_pred_lr, method = "spearman")^2, "\n\n")
  
  # Calcolo e stampo le metriche per il test set
  cat("Metriche per il test set (Regressione Lineare):\n")
  cat("R^2:", cor(y_test, ytest_pred_lr)^2, "\n")
  cat("MAE:", mean(abs(y_test - ytest_pred_lr)), "\n")
  cat("MSE:", mean((y_test - ytest_pred_lr)^2), "\n")
  cat("RMSE:", sqrt(mean((y_test - ytest_pred_lr)^2)), "\n")
  cat("EVS:", cor(y_test, ytest_pred_lr, method = "spearman")^2, "\n\n")
  
  # Stampo il confronto tra valori osservati e previsti per i primi 10 elementi del test set
  cat("Confronto tra valori osservati e previsti per i primi 10 elementi del test set (Regressione Lineare):\n")
  for (i in 1:10) {
    cat("Osservato:", y_test[i], ", Previsto:", ytest_pred_lr[i], "\n")
  }
  
  # Polinomial Regressor
  library(caret)
  library(dplyr)
  
  # Filtrare le colonne prima di applicare poly()
  X_train_poly <- data.frame(lapply(X_train, function(x) {
    if (length(unique(x)) > 2) {  # Solo se ci sono più di due valori unici
      return(poly(x, degree = 2))
    } else {
      return(x)  # Restituisci la colonna inalterata se non sufficiente per poly()
    }
  }))
  
  X_test_poly <- data.frame(lapply(X_test, function(x) {
    if (length(unique(x)) > 2) {
      return(poly(x, degree = 2))
    } else {
      return(x)
    }
  }))
  
  # Procedi con la costruzione del modello
  model_poly <- lm(y_train ~ ., data = X_train_poly)
  
  # Esegue predizioni
  ytrain_pred_poly <- predict(model_poly, newdata = X_train_poly)
  ytest_pred_poly <- predict(model_poly, newdata = X_test_poly)
  
  # Calcola metriche e stampa risultati come prima
  # Stampa delle metriche
  print_metrics <- function(actual, predicted, dataset_name) {
    if(length(actual) == length(predicted)) {
      cat(sprintf("\nMetriche per il %s set (Regressione Polinomiale):\n", dataset_name))
      cat(sprintf("R^2: %f\n", cor(actual, predicted)^2))
      cat(sprintf("MAE: %f\n", mean(abs(actual - predicted))))
      cat(sprintf("MSE: %f\n", mean((actual - predicted)^2)))
      cat(sprintf("RMSE: %f\n", sqrt(mean((actual - predicted)^2))))
      cat(sprintf("EVS: %f\n", cor(actual, predicted, method = "spearman")^2))
    } else {
      cat(sprintf("\nErrore: Dimensioni incompatibili tra valori osservati e previsti nel %s set.\n", dataset_name))
    }
  }
  
  # Stampa delle metriche
  print_metrics(y_train, ytrain_pred_poly, "training")
  print_metrics(y_test, ytest_pred_poly, "test")
  
  # Stampa dei confronti per i primi 10 elementi del test set
  cat("\nConfronto tra valori osservati e previsti per i primi 10 elementi del test set (Regressione Polinomiale):\n")
  for (i in 1:min(10, length(y_test))) {
    cat(sprintf("Osservato: %f, Previsto: %f\n", y_test[i], ytest_pred_poly[i]))
  }
  
  # Ridge
  # Carico il pacchetto necessario per la regressione ridge
  library(glmnet)
  
  # Inizializzo il modello Ridge Regression
  model_ridge <- glmnet(as.matrix(X_train), y_train, alpha = 0, lambda = 1, standardize = FALSE)  
  
  # Previsioni sui set di addestramento e di test
  ytrain_pred_ridge <- predict(model_ridge, s = 1, newx = as.matrix(X_train))
  ytest_pred_ridge <- predict(model_ridge, s = 1, newx = as.matrix(X_test))
  
  # Calcolo e stampo le metriche per il training set
  cat("Metriche per il training set (Regressione Ridge):\n")
  cat("R^2:", cor(y_train, ytrain_pred_ridge)^2, "\n")
  cat("MAE:", mean(abs(y_train - ytrain_pred_ridge)), "\n")
  cat("MSE:", mean((y_train - ytrain_pred_ridge)^2), "\n")
  cat("RMSE:", sqrt(mean((y_train - ytrain_pred_ridge)^2)), "\n")
  cat("EVS:", cor(y_train, ytrain_pred_ridge, method = "spearman")^2, "\n\n")
  
  # Calcolo e stampo le metriche per il test set
  cat("Metriche per il test set (Regressione Ridge):\n")
  cat("R^2:", cor(y_test, ytest_pred_ridge)^2, "\n")
  cat("MAE:", mean(abs(y_test - ytest_pred_ridge)), "\n")
  cat("MSE:", mean((y_test - ytest_pred_ridge)^2), "\n")
  cat("RMSE:", sqrt(mean((y_test - ytest_pred_ridge)^2)), "\n")
  cat("EVS:", cor(y_test, ytest_pred_ridge, method = "spearman")^2, "\n\n")
  
  # Stampo confronto tra valori osservati e previsti per i primi 10 elementi del test set
  cat("Confronto tra valori osservati e previsti per i primi 10 elementi del test set (Regressione Ridge):\n")
  for (i in 1:10) {
    cat("Osservato:", y_test[i], ", Previsto:", ytest_pred_ridge[i], "\n")
  }
  
  # Lasso
  # Carico il pacchetto necessario per la regressione lasso
  library(glmnet)
  
  # Inizializzo il modello Lasso Regression
  model_lasso <- glmnet(as.matrix(X_train), y_train, alpha = 1, lambda = 1, standardize = FALSE)  
  
  # Previsioni sui set di addestramento e di test
  ytrain_pred_lasso <- predict(model_lasso, s = 1, newx = as.matrix(X_train))
  ytest_pred_lasso <- predict(model_lasso, s = 1, newx = as.matrix(X_test))
  
  # Calcolo e stampo le metriche per il training set
  cat("Metriche per il training set (Regressione Lasso):\n")
  cat("R^2:", cor(y_train, ytrain_pred_lasso)^2, "\n")
  cat("MAE:", mean(abs(y_train - ytrain_pred_lasso)), "\n")
  cat("MSE:", mean((y_train - ytrain_pred_lasso)^2), "\n")
  cat("RMSE:", sqrt(mean((y_train - ytrain_pred_lasso)^2)), "\n")
  cat("EVS:", cor(y_train, ytrain_pred_lasso, method = "spearman")^2, "\n\n")
  
  # Calcolo e stampo le metriche per il test set
  cat("Metriche per il test set (Regressione Lasso):\n")
  cat("R^2:", cor(y_test, ytest_pred_lasso)^2, "\n")
  cat("MAE:", mean(abs(y_test - ytest_pred_lasso)), "\n")
  cat("MSE:", mean((y_test - ytest_pred_lasso)^2), "\n")
  cat("RMSE:", sqrt(mean((y_test - ytest_pred_lasso)^2)), "\n")
  cat("EVS:", cor(y_test, ytest_pred_lasso, method = "spearman")^2, "\n\n")
  
  # Stampo confronto tra valori osservati e previsti per i primi 10 elementi del test set
  cat("Confronto tra valori osservati e previsti per i primi 10 elementi del test set (Regressione Lasso):\n")
  for (i in 1:10) {
    cat("Osservato:", y_test[i], ", Previsto:", ytest_pred_lasso[i], "\n")
  }
  
  # ExtraTreeRegressor
  # Carico il pacchetto necessario per la regressione Extra Trees
  library(randomForest)
  
  # Inizializzo il modello Extra Trees Regression
  model_et <- randomForest(X_train, y_train, ntree = 100, mtry = ncol(X_train), importance = TRUE)
  
  # Previsioni sui set di addestramento e di test
  ytrain_pred_et <- predict(model_et, X_train)
  ytest_pred_et <- predict(model_et, X_test)
  
  # Calcolo e stampo le metriche per il training set
  cat("Metriche per il training set (Extra Trees):\n")
  cat("R^2:", cor(y_train, ytrain_pred_et)^2, "\n")
  cat("MAE:", mean(abs(y_train - ytrain_pred_et)), "\n")
  cat("MSE:", mean((y_train - ytrain_pred_et)^2), "\n")
  cat("RMSE:", sqrt(mean((y_train - ytrain_pred_et)^2)), "\n")
  cat("EVS:", cor(y_train, ytrain_pred_et, method = "spearman")^2, "\n\n")
  
  # Calcolo e stampo le metriche per il test set
  cat("Metriche per il test set (Extra Trees):\n")
  cat("R^2:", cor(y_test, ytest_pred_et)^2, "\n")
  cat("MAE:", mean(abs(y_test - ytest_pred_et)), "\n")
  cat("MSE:", mean((y_test - ytest_pred_et)^2), "\n")
  cat("RMSE:", sqrt(mean((y_test - ytest_pred_et)^2)), "\n")
  cat("EVS:", cor(y_test, ytest_pred_et, method = "spearman")^2, "\n\n")
  
  # Stampo confronto tra valori osservati e previsti per i primi 10 elementi del test set
  cat("Confronto tra valori osservati e previsti per i primi 10 elementi del test set (Extra Trees):\n")
  for (i in 1:10) {
    cat("Osservato:", y_test[i], ", Previsto:", ytest_pred_et[i], "\n")
  }
  
  # HistGradientBoosting
  library(xgboost)
  
  # Preparazione dei dati in formato xgb.DMatrix
  dtrain <- xgb.DMatrix(data = as.matrix(X_train), label = y_train)
  dtest <- xgb.DMatrix(data = as.matrix(X_test), label = y_test)
  
  # Specifica dei parametri per il modello
  params <- list(
    booster = "gbtree",
    objective = "reg:squarederror",
    eta = 0.1,
    max_depth = 3,
    colsample_bytree = 1,
    subsample = 1,
    eval_metric = "rmse"
  )
  
  # Training del modello
  model_hgb <- xgb.train(
    params = params,
    data = dtrain,          # Qui 'data' è cambiato in 'dtrain' e 'nrounds' in 'nrounds'
    nrounds = 100,
    watchlist = list(train = dtrain, eval = dtrain),
    verbose = 0,
    seed = 30
  )
  
  # Previsioni sui set di addestramento e di test
  ytrain_pred_hgb <- predict(model_hgb, dtrain)
  ytest_pred_hgb <- predict(model_hgb, dtest)
  
  # Calcolo e stampa delle metriche
  print_metrics <- function(actual, predicted, data_type) {
    cat(sprintf("\nMetriche per il %s set (HistGradientBoosting):\n", data_type))
    cat(sprintf("R^2: %.3f\n", cor(actual, predicted)^2))
    cat(sprintf("MAE: %.3f\n", mean(abs(actual - predicted))))
    cat(sprintf("MSE: %.3f\n", mean((actual - predicted)^2)))
    cat(sprintf("RMSE: %.3f\n", sqrt(mean((actual - predicted)^2))))
    cat(sprintf("EVS: %.3f\n", cor(actual, predicted, method = "spearman")^2))
  }
  
  # Metriche per il training e il test set
  print_metrics(y_train, ytrain_pred_hgb, "Training")
  print_metrics(y_test, ytest_pred_hgb, "Test")
  
  # Stampa del confronto tra valori osservati e previsti per i primi 10 elementi del test set
  cat("\nConfronto tra valori osservati e previsti per i primi 10 elementi del test set (HistGradientBoosting):\n")
  for (i in 1:min(10, length(y_test))) {
    cat(sprintf("Osservato: %.3f, Previsto: %.3f\n", y_test[i], ytest_pred_hgb[i]))
  }
  
  # SGD Regressor
  # Carico il pacchetto necessario per SGDRegressor
  library(glmnet)
  
  # Trasformo i dati in matrici
  X_train_matrix <- as.matrix(X_train)
  X_test_matrix <- as.matrix(X_test)
  
  # Inizializzo il modello SGDRegressor
  model_sgd <- cv.glmnet(x = X_train_matrix, y = y_train, alpha = 1, nfolds = 5, type.measure = "mse")
  
  # Previsioni sui set di addestramento e di test
  ytrain_pred_sgd <- predict(model_sgd, s = "lambda.min", newx = X_train_matrix)
  ytest_pred_sgd <- predict(model_sgd, s = "lambda.min", newx = X_test_matrix)
  
  # Calcolo e stampo le metriche per il training set
  cat("Metriche per il training set (SGDRegressor):\n")
  cat("R^2:", cor(y_train, ytrain_pred_sgd)^2, "\n")
  cat("MAE:", mean(abs(y_train - ytrain_pred_sgd)), "\n")
  cat("MSE:", mean((y_train - ytrain_pred_sgd)^2), "\n")
  cat("RMSE:", sqrt(mean((y_train - ytrain_pred_sgd)^2)), "\n")
  cat("EVS:", cor(y_train, ytrain_pred_sgd, method = "spearman")^2, "\n\n")
  
  # Calcolo e stampo le metriche per il test set
  cat("Metriche per il test set (SGDRegressor):\n")
  cat("R^2:", cor(y_test, ytest_pred_sgd)^2, "\n")
  cat("MAE:", mean(abs(y_test - ytest_pred_sgd)), "\n")
  cat("MSE:", mean((y_test - ytest_pred_sgd)^2), "\n")
  cat("RMSE:", sqrt(mean((y_test - ytest_pred_sgd)^2)), "\n")
  cat("EVS:", cor(y_test, ytest_pred_sgd, method = "spearman")^2, "\n\n")
  
  # Stampo confronto tra valori osservati e previsti per i primi 10 elementi del test set
  cat("Confronto tra valori osservati e previsti per i primi 10 elementi del test set (SGDRegressor):\n")
  for (i in 1:10) {
    cat("Osservato:", y_test[i], ", Previsto:", ytest_pred_sgd[i], "\n")
  }
  
  # Elastic Net
  # Carico il pacchetto necessario per Elastic Net
  library(glmnet)
  
  # Trasformo i dati in matrici
  X_train_matrix <- as.matrix(X_train)
  X_test_matrix <- as.matrix(X_test)
  
  # Inizializzo il modello Elastic Net
  model_en <- cv.glmnet(x = X_train_matrix, y = y_train, alpha = 0.5, nfolds = 5, type.measure = "mse")
  
  # Previsioni sui set di addestramento e di test
  ytrain_pred_en <- predict(model_en, s = "lambda.min", newx = X_train_matrix)
  ytest_pred_en <- predict(model_en, s = "lambda.min", newx = X_test_matrix)
  
  # Calcolo e stampo le metriche per il training set
  cat("Metriche per il training set (Elastic Net):\n")
  cat("R^2:", cor(y_train, ytrain_pred_en)^2, "\n")
  cat("MAE:", mean(abs(y_train - ytrain_pred_en)), "\n")
  cat("MSE:", mean((y_train - ytrain_pred_en)^2), "\n")
  cat("RMSE:", sqrt(mean((y_train - ytrain_pred_en)^2)), "\n")
  cat("EVS:", cor(y_train, ytrain_pred_en, method = "spearman")^2, "\n\n")
  
  # Calcolo e stampo le metriche per il test set
  cat("Metriche per il test set (Elastic Net):\n")
  cat("R^2:", cor(y_test, ytest_pred_en)^2, "\n")
  cat("MAE:", mean(abs(y_test - ytest_pred_en)), "\n")
  cat("MSE:", mean((y_test - ytest_pred_en)^2), "\n")
  cat("RMSE:", sqrt(mean((y_test - ytest_pred_en)^2)), "\n")
  cat("EVS:", cor(y_test, ytest_pred_en, method = "spearman")^2, "\n\n")
  
  # Stampo confronto tra valori osservati e previsti per i primi 10 elementi del test set
  cat("Confronto tra valori osservati e previsti per i primi 10 elementi del test set (Elastic Net):\n")
  for (i in 1:10) {
    cat("Osservato:", y_test[i], ", Previsto:", ytest_pred_en[i], "\n")
  }
  
  # Model Comparising
  library(dplyr)
  
  # Definizione dei dati
  model_name <- c('Linear','Polinomial',"Ridge", "Lasso", 'Decision Tree', 'Random Forest','Extratree','Gradient Boosting','XGBoost','AdaBoost','HistGradientBoosting', 'SGD Regressor', 'Elastic Net', 'SVM', 'KNN')
  R2 <- c(0.86, 0.97, 0.86, 0.80, 0.96, 0.99, 1.00, 0.97, 0.99, 0.85, 0.99, 0.85, 0.78, 1.00, 0.99)
  MAE <- c(2.50, 1.24, 2.50, 2.86, 0.77, 0.51, 0.27, 1.06, 0.51, 2.62, 0.49, 2.51, 2.94, 0.23, 0.49)
  MSE <- c(10.12, 2.41, 10.12, 14.21, 2.71, 0.59, 0.17, 1.89, 0.66, 10.65, 0.54, 10.15, 15.45, 0.15, 0.59)
  RMSE <- c(3.18, 1.55, 3.18, 3.77, 1.65, 0.77, 0.41, 1.37, 0.81, 3.26, 0.73, 3.19, 3.93, 0.38, 0.76)
  EVS <- c(0.86, 0.97, 0.86, 0.80, 0.96, 0.99, 1.00, 0.97, 0.99, 0.85, 0.99, 0.85, 0.78, 1.00, 0.99)
  
  # Creazione di un DataFrame
  models_df <- data.frame(Model_Name = model_name, R2 = R2, MAE = MAE, MSE = MSE, RMSE = RMSE, EVS = EVS)
  
  # Ordinamento del DataFrame in base al valore di R2
  models_df_sorted <- models_df %>%
    arrange(R2) %>%
    select(Model_Name, everything())
  
  # Stampa del DataFrame ordinato
  print(models_df_sorted)
  
  <h1>Build ANN Model </h1>
    # Carico il pacchetto keras
    library(keras)
  # Converti X_train e X_test in matrici numeriche
  X_train_matrix <- as.matrix(X_train)
  X_test_matrix <- as.matrix(X_test)
  
  # Assicurati che y_train e y_test siano vettori numerici
  y_train_vector <- as.vector(y_train)
  y_test_vector <- as.vector(y_test)
  
  library(keras)
  
  # Definizione del modello
  model <- keras_model_sequential() %>%
    layer_dense(units = 64, activation = "relu", input_shape = c(ncol(X_train_matrix))) %>%
    layer_dense(units = 64, activation = "relu") %>%
    layer_dense(units = 1)
  
  # Compilazione del modello
  model %>% compile(
    optimizer = "adam",
    loss = "mean_squared_error",
    metrics = c("mean_absolute_error", "mean_squared_error")
  )
  
  # Addestramento del modello
  history <- model %>% fit(
    x = X_train_matrix,
    y = y_train_vector,
    epochs = 100,
    batch_size = 32,
    validation_split = 0.2
  )
  
  # Visualizzazione delle metriche di training
  plot(history)
  
  # Previsioni sui set di addestramento e di test
  ytrain_pred <- model %>% predict(X_train_matrix)
  ytest_pred <- model %>% predict(X_test_matrix)
  
  # Appiattisci (flatten) ytest_pred se necessario (dipende dalla struttura dell'output del modello)
  ytest_pred <- as.vector(ytest_pred)
  
  # Stampa i primi 10 valori previsti e i valori reali
  cat("Osservato vs Previsto:\n")
  for (i in 1:min(10, length(y_test_vector))) {
    cat(sprintf("Osservato: %.2f, Previsto: %.2f\n", y_test_vector[i], ytest_pred[i]))
  }
  
  # Calcolo delle metriche
  calculate_metrics <- function(actual, predicted, data_type = "Training") {
    cat(sprintf("\nMetriche per il %s set:\n", data_type))
    cat(sprintf("R^2: %.3f\n", cor(actual, predicted)^2))
    cat(sprintf("MAE: %.3f\n", mean(abs(actual - predicted))))
    cat(sprintf("MSE: %.3f\n", mean((actual - predicted)^2)))
    cat(sprintf("RMSE: %.3f\n", sqrt(mean((actual - predicted)^2))))
  }
  calculate_metrics(y_train_vector, ytrain_pred, "Training")
  calculate_metrics(y_test_vector, ytest_pred, "Testing")
  
  
  library(ggplot2)
  
  # Assumi che y_test_vector e ytest_pred siano già definiti come sopra
  
  # Creazione di un dataframe per ggplot
  data <- data.frame(Osservato = y_test_vector, Previsto = ytest_pred)
  
  # Grafico di dispersione con la linea di identità
  ggplot(data, aes(x = Osservato, y = Previsto)) +
    geom_point() +  # aggiunge i punti di dispersione
    geom_abline(intercept = 0, slope = 1, col = "red") +  # aggiunge la linea y=x
    labs(x = "Valori Osservati", y = "Valori Previsti", title = "Confronto tra Valori Osservati e Previsti") +
    theme_minimal()  # usa un tema minimale per il grafico
  
  