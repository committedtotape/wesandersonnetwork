# Wes Anderson Actor Network

### Network visualisation of actors appearing in Wes Anderson films

This repository currently holds 2 versions (there may be more to come, who knows) of Wes Anderson network visualisations. I wanted to use the [wesanderson](https://github.com/karthik/wesanderson) colour palette package for a data viz of, well, something to do with Wes Anderson. This is what happened.

Files contained in this repo:

*  `Wes Anderson Film Network.R` - Original project to scrape Wes Anderson film data and generate a static visualisation of actors appearing in his films.  Built using the [tidygraph](https://github.com/thomasp85/tidygraph) and [ggraph](https://github.com/thomasp85/ggraph) packages by [Thomas Pedersen](https://github.com/thomasp85).
*  `wes_anderson_films.rds` - Dataset created in above script to save having to repeat web scrape for further Wes fun.
*  `app.R` - A Shiny app script to create an interactive visualisation.  Built using the `tidygraph` and [visNetwork](https://github.com/datastorm-open/visNetwork) packages. It uses the dataset above.
*  `styles.css` - Some CSS stylings for Shiny app.

A write-up of the static visualisation can be found on my blog:
[https://davidsmale.netlify.com/portfolio/wes-anderson-actor-network/](https://davidsmale.netlify.com/portfolio/wes-anderson-actor-network/). 

A write-up of the interactive visualisation hopefully coming soon...

Here's the Shiny App for your pleasure:
[https://committedtotape.shinyapps.io/wesandersonnetwork/](https://committedtotape.shinyapps.io/wesandersonnetwork/)

Forever indebted to [Karthik Ram's](https://github.com/karthik) [wesanderson](https://github.com/karthik/wesanderson) package.

Please use Wes Anderson colour palettes responsibly.

