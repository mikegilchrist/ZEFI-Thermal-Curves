# 2025-04-18

- Still examining and comparing song-motif data.
- Not sure what the question we want to address is.
- There are 81 trials where the # of motifs differ by more than 10 between the original motif only dataset and the second, motif and song dataset.


```{r}
> different_by_10_count <- which(abs(data$motif_count.x - data$motif_count.y) > 10)
> data[different_by_10_count, cols_keep] |> 
+     group_by(male, date) |>
+     arrange(male, .by_group = TRUE) |> print(n=150)
# A tibble: 81 × 7
# Groups:   male, date [81]
   male  date       motif_count.x motif_count.y motif_misc motif_count_all song_count
   <chr> <date>             <dbl>         <int>      <dbl>           <dbl>      <dbl>
 1 T229  2022-02-17           114            85         29             114         28
 2 T229  2022-02-19           198           159          0             159         51
 3 T229  2022-02-21           315           227         91             318         83
 4 T229  2022-05-05            95            62         18              80         20
 5 T229  2022-05-09           321           262         48             310         85
 6 T229  2022-06-24           105            75         26             101         29
 7 T229  2022-06-26           133            28          6              34          7
...
78 T258  2022-07-12            74            57         14              71         24
79 T260  2022-07-13            74            60         13              73         24
80 T260  2022-07-15            65            54         12              66         22
81 T260  2022-07-19           198           143         38             181         61
> length(different_by_10_count)
[1] 81
> ## These rows are for T237 which, according to HSPi-Round-1-Heat-Trials.csv, have missing count data
> x_zero_y_not_zero_rows <- which(data$motif_count.x==0 & data$motif_count.y>0)
> data[x_zero_y_not_zero_rows,]
# A tibble: 5 × 58
# Groups:   male, date [5]
  male  round trial_round date       motif_count.x motif_count.y motif_prop temp_mean humidity_mean chamber
  <chr> <dbl>       <dbl> <date>             <dbl>         <int>      <dbl>     <dbl>         <dbl> <fct>  
1 T237      1           1 2022-02-23             0            67          0      40.8          20.4 2      
2 T237      1           2 2022-02-25             0            74          0      45.2          16.6 2      
3 T237      1           3 2022-02-27             0            89          0      35.8          17.8 2      
4 T237      1           4 2022-03-01             0           118          0      27.3          20.9 2      
5 T237      1           5 2022-03-03             0            73          0      42.6          13.9 2      
# ℹ 48 more variables: ...
> length(x_zero_y_not_zero_rows)
[1] 5
>
> ## Notes in merged.motif.data_2025-03-03.csv indicate T231 & T235 did not sing during trial.
> ## This is especially strange given the insanely high count for T235 on 2022-06-26 of 425 
> ## > T231,2/15/2022,NA,NA,NA,0,NA,"did not sing during 30 min trial, sang before only"
> ## > T235,6/26/2022,NA,NA,NA,0,NA,Did not sing
> y_zero_x_not_zero_rows <- which(data$motif_count.y==0 & data$motif_count.x>0)
> data[y_zero_x_not_zero_rows,]
# A tibble: 2 × 58
# Groups:   male, date [2]
  male  round trial_round date       motif_count.x motif_count.y motif_prop temp_mean humidity_mean chamber
  <chr> <dbl>       <dbl> <date>             <dbl>         <int>      <dbl>     <dbl>         <dbl> <fct>  
1 T231      1           2 2022-02-15             7             0    0.00529      45.0          8.75 2      
2 T235      3           2 2022-06-26           425             0    0.182        30.7         36.2  3      
# ℹ 48 more variables: ...
> length(y_zero_x_not_zero_rows)
[1] 2
>
```

I suspect this entry is due to a typo (133 vs 33)
> T229  2022-06-26           133            34               28          7                6          1.19

```{r}
> data$error_large <- FALSE
> data[error_large_rows, "error_large"] <- TRUE
> tmp <- data[error_large_rows, ] |> 
+     arrange(error_motif_xy)
> tmp |> print(n=40)
# A tibble: 12 × 10
# Groups:   male, date [12]
   male  date       motif_count.x motif_count.y song_motif_count song_count misc_motif_count error_motif_xy
   <chr> <date>             <dbl>         <dbl>            <int>      <dbl>            <dbl>          <dbl>
 1 T247  2022-06-10           117            86               74         16               12          0.305
 2 T235  2022-05-09            38            27               20          7                7          0.338
 3 T244  2022-06-29           125            82               82         17                0          0.415
 4 T229  2022-06-26           133            34               28          7                6          1.19 
 5 T231  2022-02-15             7             0                0          0                0          2    
 6 T235  2022-06-26           425             0                0          0                0          2    
 7 T237  2022-02-23             0            71               67         16                4          2    
 8 T237  2022-02-25             0            78               74         19                4          2    
 9 T237  2022-02-27             0            95               89         23                6          2    
10 T237  2022-03-01             0           123              118         36                5          2    
11 T237  2022-03-03             0            77               73         21                4          2    
12 T258  2022-07-22             0             1                0          0                1          2    
# ℹ 2 more variables: not_equal <lgl>, error_large <lgl>
```
  - `y_zero_x_not_zero_rows`
- Wondering if we want to try and use AI to resolve some of these issues since there's lots of observations.
