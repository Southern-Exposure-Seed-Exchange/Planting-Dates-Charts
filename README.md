# Planting Dates Chart Generator

This is a script that takes an excel workbook and uses it to generate an SVG
chart of planting dates.

The workbook should have the data in `Sheet1`, with a header row, one plant per
row, and columns for the planting & harvest start/end dates(repeating if
multiple bars are to be shown).

```
stack build
stack run "Zones 7-8.xlsx"
inkscape plants-chart.svg
```


## License

GPL-3.0
