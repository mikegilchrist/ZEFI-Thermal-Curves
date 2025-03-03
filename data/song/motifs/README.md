# Description

- More detailed data than previous
  - previous: number of songs per period
  - current: number of motifs/song and details about length of motifs
- CSV file created from xlsx file to ease viewing of contents from commandline 
- Did various fixes of typos such as
  - replace 2011 with 2022 for years
  - Fix typos in male ID
  - Fix errors in date column
    - `$ sed -i 's|T244,6/24/2022|T244,6/25/2022|' song.by.motif.data.csv 

# 2025-02-04

- Found ~ 30 entries where the time start and stop differences seemed erroneously long.
- Sent file with those values to Tara
# 2025-02-16

- Got revised values for 30 outlined only.
- Need to integerated it back into the current dataframe.

