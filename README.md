# Planting Dates Chart Generator

[![Build Status](https://travis-ci.org/Southern-Exposure-Seed-Exchange/Planting-Dates-Charts.svg?branch=master)](https://travis-ci.org/Southern-Exposure-Seed-Exchange/Planting-Dates-Charts)

This is a script that takes excel workbooks and uses them to generate SVG
charts of planting dates.

The workbooks should be named `ZONES 5-6.xlsx` and `ZONES 7-8.xlsx` & should
have the data in `Sheet1`, with a header row, one plant per row, and columns
for the planting & harvest start/end dates(repeating if multiple bars are to be
shown).

```
stack build
stack run
inkscape "Zones 7 & 8 Planting Dates-part-one.svg"
```

TODO:

* What to do about spring/fall planting dates overlapping?
* Show harvest dates? In different color? With legend?


## License

GPL-3.0
