## cleanNLP: A Tidy Data Model for Natural Language Processing

**Author:** Taylor B. Arnold<br/>
**License:** [LGPL-2](https://opensource.org/licenses/LGPL-2.1)

[![CRAN Version](http://www.r-pkg.org/badges/version-ago/cleanNLP)](https://CRAN.R-project.org/package=cleanNLP) 

## Overview

The **cleanNLP** package is designed to make it as painless as possible
to turn raw text into feature-rich data frames. A minimal working example
of using **cleanNLP** consists of loading the package, setting up the NLP
backend, initializing the backend, and running the function `cnlp_annotate`.
The output is given as a list of data frame objects (classed as an
"cnlp_annotation"). Here is an example using the udpipe backend:

```{r}
library(cleanNLP)
cnlp_init_udpipe()

annotation <- cnlp_annotate(input = c(
        "Here is the first text. It is short.",
        "Here's the second. It is short too!",
        "The third text is the shortest."
))
lapply(annotation, head)
```
```
$token
  doc_id sid tid token token_with_ws lemma  upos xpos
1      1   1   1  Here         Here   here   ADV   RB
2      1   1   2    is           is     be   AUX  VBZ
3      1   1   3   the          the    the   DET   DT
4      1   1   4 first        first  first   ADJ   JJ
5      1   1   5  text          text  text  NOUN   NN
6      1   1   6     .            .      . PUNCT    .
                                                  feats tid_source relation
1                                          PronType=Dem          0     root
2 Mood=Ind|Number=Sing|Person=3|Tense=Pres|VerbForm=Fin          1      cop
3                             Definite=Def|PronType=Art          5      det
4                                Degree=Pos|NumType=Ord          5     amod
5                                           Number=Sing          1    nsubj
6                                                  <NA>          1    punct

$document
  doc_id
1      1
2      2
3      3
```

The `token` output table breaks the text into tokens, provides lemmatized
forms of the words, part of speech tags, and dependency relationships. Two
short case-studies are linked to from the repository to show sample usage of
the library:

- [State of the Union Addresses](https://statsmaths.github.io/cleanNLP/state-of-union.html)
- [Exploring Wikipedia Data](https://statsmaths.github.io/cleanNLP/wikipedia.html)

Please see the notes below, and the official package documentation on
[CRAN](https://cran.r-project.org/web/packages/cleanNLP/), for more options
to control the way that text is parsed.

## Installation

You can download the package from within R directly from CRAN:

```{r}
install.packages("cleanNLP")
```

After installation, you should be able to use the udpipe backend (as used
the minimal example and case-studies above; model files will be installed
automatically) or the stringi backend without any additional setup. **For most
users, we find that these out-of-the-box solutions are a good starting point.**
In order to use the two Python backends, you must install the associated
`cleannlp` python module. We recommend and support the Python 3.7 version of
[Anaconda Python](https://www.anaconda.com/distribution/#download-section).
After obtaining Python, install the module by running pip in a terminal:

```{py}
pip install cleannlp
```

Once installed, running the respective backend initialization functions will
provide further instructions for download the required models.

## API Overview

### V3

There have been numerous changes to the package in the newly released version 3.0.0.
These changes, while requiring some changes to existing code, have been carefully
designed to make the package easier to both install and use. The three most important
changes include:

- The object returned by `cnlp_annotate` is now a named list. Users can access its
elements with the dollar sign operator. Functions such as `cnlp_get_token`
and `cnlp_get_dependency` are no longer needed or included.
- The dependencies are now attached to the tokens table to make them easier to use.

If you are running into any issues with the package, first make sure you are using
updated materials (mostly available from links within this repository).

### Backends

The cleanNLP package is designed to allow users to make use of various NLP
annotation algorithms without having to worry (too much) about the output
format, which is standardizes at best as possible. There are three backends
currently available, each with their own pros and cons. They are:

- **stringi**: a fast parser that only requires the stringi package,
but produces only tokenized text
- **udpipe**: a parser with no external dependencies that produces
tokens, lemmas, part of speech tags, and dependency relationships. The
recommended starting point given its balance between ease of use and
functionality. It also supports the widest range of natural languages.
- **spacy**: based on the Python library, a more feature complete parser
that included named entity recognition and word embeddings. It does require
a working Python installation and some other set-up. Recommended for users
who are familiar with Python or plan to make heavy use of the package.

The final backend (spacy) require some additional setup,
namely installing Python and the associated Python library, as documented above.
To select the desired backend, simply initialize the model prior to running the
annotation.

```{r}
cnlp_init_stringi(locale="en_GB")
cnlp_init_udpipe(model_name="english")
cnlp_init_spacy(model_name="en")
```

The code above explicitly sets the default/English model. You can use a
different model/language when starting the model. For udpipe the models will
be downloaded automatically. For spacy the following helper
functions are available:

```{r}
cnlp_download_spacy(model_name="en")
```

Simply change the model name or language code to download alternative models.

## Citation

If you make use of the toolkit in your work, please cite the following paper.

```
@article{,
  title   = "A Tidy Data Model for Natural Language Processing Using cleanNLP",
  author  = "Arnold, Taylor B",
  journal = "R Journal",
  volume  = "9",
  number  = "2",
  year    = "2017"
}
```

Please, however, note that the library has evolved since the paper was published.
For specific help with the package's API please check the updated documents
linked to from this site.

## Note

Please note that this project is released with a
[Contributor Code of Conduct](CONDUCT.md). By participating in this project
you agree to abide by its terms.
