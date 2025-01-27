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
