<p align="center"><a href="http://193.146.75.235/sample-apps/final_apps/layout/"><img src="https://github.com/ordanovich/images/blob/master/portada_small.png?raw=true"></a></p>

## Interactive application for programmatic data retrieval from [EuroStat](https://ec.europa.eu/eurostat/data/database)

This appication is based on the [**eurostat** package](http://ropengov.github.io/eurostat/index.html). To learn more about the functionality of the package please refer to the [tutorial](http://ropengov.github.io/eurostat/articles/eurostat_tutorial.html).

*Leo Lahti, Przemyslaw Biecek, Markus Kainu and Janne Huovari. **Retrieval and analysis of Eurostat open data with the eurostat package.** R Journal 9(1):385-392, 2017). 
R package version 3.3.5. URL: [http://ropengov.github.io/eurostat](http://ropengov.github.io/eurostat)*

### Nomenclature

- :rocket: [app.R](https://github.com/ordanovich/downloadEUROSTAT/blob/master/app.R): Shiny app combining **UI** and **server** parts.
- :earth_africa: [nuts.RData](https://github.com/ordanovich/downloadEUROSTAT/raw/master/nuts.RData): data frame with NUTS nomenclature and geographic codes.
- :bar_chart: [report.Rmd](https://github.com/ordanovich/downloadEUROSTAT/blob/master/report.Rmd): template markdown file to generate and download an HTML report through the application interface

The application itself (use `shiny::runApp()` from the cloned repository or open the <a href="http://193.146.75.235/sample-apps/final_apps/eurostat_download/"  rel="noopener noreferrer" target="_blank">online version</a> to preview the app) pursues the goal to allow users to consult the contents of the data base, retrieve and visualize the desired datasets in a quick and easy-to-manipulate manner. 
