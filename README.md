# Planting Dates Chart Generator

[![Build Status](https://travis-ci.org/Southern-Exposure-Seed-Exchange/Planting-Dates-Charts.svg?branch=master)](https://travis-ci.org/Southern-Exposure-Seed-Exchange/Planting-Dates-Charts)

This is a script that takes an excel workbook and uses it to generate an SVG
chart of planting dates.

The workbook should have the data in `Sheet1`, with a header row, one plant per
row, and columns for the planting & harvest start/end dates(repeating if
multiple bars are to be shown).

```
stack build
stack run "Zones 7-8.xlsx"
inkscape plants-chart-part-one.svg
```

TODO:

* Add titles w/ zone numbers(add Part One, Part Two as sub-titles)
* What to do about spring/fall planting dates overlapping?
* Show harvest dates? In different color? With legend?


## License

GPL-3.0
