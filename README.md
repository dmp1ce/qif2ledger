qif2ledger
==========

The purpose of the program is to convert a QIF file to a compatible ledger-cli formated file.  I created this program to convert a QIF file that was exported by iBank.  It is specific the iBank export format, so I'm not sure of the program works on other types of QIF files.

INSTALL
-------

- `cabal-dev configure`
- `cabal-dev install`
- `cabal-dev build`

RUN
---

`./qif2ledger < qif_file.qif > qif_file_converted.ledger`

NOTES
-----

- The QIF file must be formated with UNIX line breaks instead of the ^M characters.
