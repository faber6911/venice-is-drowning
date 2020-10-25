<div align = "center"><p align="center">
  <a href="http://datascience.disco.unimib.it/it/"><img src = "https://raw.githubusercontent.com/malborroni/Foundations_of_Computer-Science/master/images/DSunimib.png" width = "100%"></a>
</p>
</div>

<h1 align = "center">Venice is drowning <img src="https://raw.githubusercontent.com/faber6911/venice-is-drowning/master/pictures_waves.png" width="2%"/></h1>
<h6 align = "center">a Streaming Data Management and Time Series Analysis project</h6>

<p align="center">
  <a href="#overview"> Overview &nbsp;</a>|
  <a href="#data sources">Data sources &nbsp;</a>|
  <a href="#obiettivi">Objectives &nbsp;</a>|
  <a href="#aboutus">&nbsp; About us &nbsp;</a>
</p>


<a name="overview"></a>
## &#9741; &nbsp; Overview
An attempt to model the tidal phenomenon in the Venice lagoon using both linear/statistical and machine learning approaches.

<a name="data sources"></a>
## &#9741; &nbsp; Data sources
<ul>
  <li><a href = "https://www.kaggle.com/lbronchal/venezia">kaggle</a> dataset</li>
  <li><a href = "https://github.com/lbcommer/venezia-high-waters">github repo</a> inspiration</li>
  <li><a href = "https://www.comune.venezia.it/node/6214">tides data</a> for 2018</li>
  <li><a href = "https://www.arpa.veneto.it/">ARPA Veneto</a> for meteorological data</li>
  <li><a href = "https://rhodesmill.org/pyephem/">PyEphem</a> API for lunar motion</li>
  <li><a href = "https://cran.r-project.org/web/packages/oce/index.html">oce</a> package for Analysis of Oceanographic Data</li>
</ul>

<a name="obiettivi"></a>
## &#9741; &nbsp; Objectives
<!--
<li>Realizzare previsioni su serie storica maree laguna di Venezia 1983-2018 (analisi univariata)</li>
<li>Realizzare previsioni su serie storica maree laguna di Venezia 1983-2018 dopo l'integrazione di dati meteo (analisi multivariata)</li>
-->
The main objective of the project is to analyze the data of the tide detections regarding the area of the Venice lagoon, producing predictive models whose performances are evaluated on a time horizon ranging from one hour up to a week of forecast.

For this purpose, three models, both linear and machine-learning based, are tested:

* ARIMA (AutoRegressive Integrated Moving Average);
* UCM (Unobserved Component Models);
* LSTM (Long Short-Term Memory).

The final report is available <a href="https://faber6911.github.io/venice-is-drowning/report/report.html">here</a> while <a href="https://github.com/faber6911/venice-is-drowning/blob/master/slides/venice_is_drowning_slides.pdf">here</a> the slides.

<a name="aboutus"></a>
## &#9741; &nbsp; About us

&#8860; &nbsp; **Dario Bertazioli**

- *Current Studies*: Data Science Master Student at Università degli Studi di Milano-Bicocca;
- *Past Studies*: Bachelor's degree in Physics at Università degli Studi di Milano.
<br>

<p align = "center">
  <a href = "https://www.linkedin.com/in/dario-bertazioli-961ab4180/"><img src="https://raw.githubusercontent.com/DBertazioli/Interact/master/img/iconfinder_Popular_Social_Media-22_2329259.png" width = "3%"></a>
  <a href = "https://github.com/DBertazioli/"><img src="https://raw.githubusercontent.com/malborroni/Foundations_of_Computer-Science/master/images/GitHub.png" width = "3%"></a>
</p>

&#8860; &nbsp; **Fabrizio D'Intinosante**

- *Current Studies*: Data Science Master Student at Università degli Studi di Milano-Bicocca;
- *Past Studies*: Bachelor's Degree in Economics e Statistics at Università degli Studi di Torino.
<br>

<p align = "center">
  <a href = "https://www.linkedin.com/in/fabrizio-d-intinosante-125042180/"><img src="https://raw.githubusercontent.com/DBertazioli/Interact/master/img/iconfinder_Popular_Social_Media-22_2329259.png" width = "3%"></a>
  <a href = "https://faber6911.github.io/"><img src="https://raw.githubusercontent.com/malborroni/Foundations_of_Computer-Science/master/images/GitHub.png" width = "3%"></a>
</p>
