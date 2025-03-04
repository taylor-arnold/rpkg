{ pkgs ? import <nixpkgs> {}, displayrUtils }:

pkgs.rPackages.buildRPackage {
  name = "cleanNLP";
  version = displayrUtils.extractRVersion (builtins.readFile ./DESCRIPTION); 
  src = ./.;
  description = ''
  tables. Users may make use of the 'udpipe' back end with no external dependencies, a Python back
  end with 'spaCy' <https://spacy.io> or the Java back end 'CoreNLP'
  <http://stanfordnlp.github.io/CoreNLP/>. Exposed annotation tasks include
  tokenization, part of speech tagging, named entity recognition, entity linking, sentiment
  analysis, dependency parsing, coreference resolution, and word embeddings. Summary
  statistics regarding token unigram, part of speech tag, and dependency type frequencies
  are also included to assist with analyses.'';
  propagatedBuildInputs = with pkgs.rPackages; [ 
    dplyr
    stringi
    Matrix
    rJava
    jsonlite
 ];

}
