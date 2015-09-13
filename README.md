# D3 JavaScript Network Graphs from R

This README includes information on set up and a number of advanced examples.
For more information see the package's [main page](http://christophergandrud.github.io/networkD3/).

## Installation

You can install **networkD3** from GitHub as follows:

```S
devtools::install_github("alexcb/rjson", subdir = "rjson")
devtools::install_github('christophergandrud/networkD3')
```

## Usage

Here are two examples of hierarchical edge bundling

### Parse hierarchical JSON data

```S
Flare <- rjson::fromJSON(file = system.file(file.path("data", "readme-flare-imports.json"), package = "networkD3"), simplify = FALSE)
```

dynamic radial layout uisng `clusterNetwork`:

```S
clusterNetwork(List = Flare)
```

static treemap layout using `treemapNetwork`:

```S
treemapNetwork(List = Flare)
```

Here's `chordNetwork` using an Uber transport matrix in JSON form:

```S
table <- read.csv(system.file(file.path("data", "cities.csv"), package = "networkD3"))
matrix <- JSONtoMatrix(file = system.file(file.path("data", "matrix.json"), package = "networkD3"))
chordNetwork(matrix = matrix, df = table)
```

### Saving to an external file

Use `saveNetwork` to save a network to stand alone HTML file:

```S
library(magrittr)

simpleNetwork(networkData) %>% saveNetwork(file = 'Net1.html')
```
