# Simple utility to extract CSV transaction information from PDF files

Currently only tested with HSBC UK

```
cabal install hsbc-pdf
```

Then
* `hsbc-pdf current PATH_TOPDF` to extract current account info
* `hsbc-pdf credit PATH_TOPDF` to extract current account info
