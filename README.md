# dict.cc
A command line tool for accessing dict.cc written in Haskell.

## Usage
To look up German translations for an English word:
```bash
$ dict-cc dictionary
Englisch                                                      Deutsch
===========                                                ==========
ling. publ. dictionary                        Wörterbuch {n} [1324 ✓]
dictionary                                         Lexikon {n} [70 ✓]
dictionary                        Diktionär {n} {m} [veraltet] [66 ✓]
```

To choose the source and destination languages:
```bash
$ dict-cc --from es --to de diccionario
Spanisch                                             Deutsch
===========                                       ==========
ling. edit. diccionario {m}           Wörterbuch {n} [119 ✓]
edu. loc. diccionario {m} de bolsillo  Taschenwörterbuch {n}
```

To filter based on word type (e.g. noun or verb):
```bash
$ dict-cc --verb run
Englisch                                                    Deutsch
============                                             ===========
to run                                               laufen [3148 ✓]
to run                                               rennen [1494 ✓]
to run sth. [manage, lead]                      etw. leiten [1108 ✓]
# ...

$ dict-cc --noun run
Englisch                                                                               Deutsch
============                                                                        ===========
run                                                                            Lauf {m} [124 ✓]
run [sequence, cycle]                                                          Ablauf{m} [37 ✓]
cloth. run [in stockings, tights]                                          Laufmasche{f} [25 ✓]
transp. travel run                                                              Fahrt{f} [23 ✓]
# ...
```

## Configuration
Adding a YAML file `.dict-cc` at [`XDG_CONFIG_HOME`](https://wiki.archlinux.org/index.php/XDG_Base_Directory_support) (typically ~/.config) allows
you to configure the default options for `--to`, `--from` and `--limit`.

For example to search by default for translations from Spanish to German, and limit to 10 results:
```yaml
defaults:
  from: es
  to: de
  limit: 10
```

Now:
```bash
$ dict-cc diccionario
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
