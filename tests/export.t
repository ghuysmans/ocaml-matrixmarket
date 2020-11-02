Export edge labels
  $ ./export.exe
  %%MatrixMarket matrix coordinate integer symmetric
  3 3 3
  2 1 1
  3 2 1
  3 1 1

Drop edge labels
  $ ./export.exe pattern
  %%MatrixMarket matrix coordinate pattern symmetric
  3 3 3
  2 1
  3 2
  3 1
