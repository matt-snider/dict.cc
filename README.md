# dict.cc
A command line tool for accessing dict.cc written in Haskell.


## Usage
To look up German translations for an English word:
```bash
$ dict-cc dictionary
Englisch                                   Deutsch
=========                                 ========
ling. publ. dictionary         1275 Wörterbuch {n}
dictionary                          66 Lexikon {n}
dictionary                      41 Verzeichnis {n}
```

To choose the source and destination languages:
```bash
$ dict-cc --from es --to de diccionario
Spanisch                                   Deutsch
=========                                 ========
ling. edit. diccionario {m}      94 Wörterbuch {n}
```


## Building
Clone the repository and build with [stack](https://docs.haskellstack.org/en/stable/README/):

```bash
$ git clone https://github.com/matt-snider/dict.cc
$ cd dict.cc
$ stack build
```
