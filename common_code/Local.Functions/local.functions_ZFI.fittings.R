

which_tbl_row <- function(filter_male = FALSE, x0_flag = "individual", y0_flag = "individual", disp_flag = "uniform_1", disp_value = 0.01, model = "two_piece", tbl = fit_tbl) {
  which( tbl$filter_male %in% filter_male & 
           tbl$x0_flag %in% x0_flag &
           tbl$y0_flag %in% y0_flag &
           tbl$disp_flag %in% disp_flag &
           tbl$disp_value %in% disp_value &
           tbl$model %in% model
        # tbl$sampling_dist %in% sampling &
        )
}

clean_var_names <- function(fit) {
  fit %>% setNames(gsub("b_", "", names(.)) %>%
                     gsub("(x0|s0|y0|disp)_male(T[0-9]{3})", "\\2_\\1", .) %>%
                     gsub("__", "_", .) %>%
                     gsub("r_male_(x0|s0|y0|disp)\\[(T[0-9]{3}),Intercept\\]", "\\2_\\1_r", .) %>%
                     gsub("\\.", " ", .))
}

## Generate data for plotting expected values
generate_epred_data <- function(fit_brms = NULL, group = c(disp_group, male), ndraws = 3000, ncores = 4) {

    grps <- NULL

    ngrps <- ngrps(fit_brms)
    ifelse(length(ngrps) ==0 | length(group) == 0, grouping <-FALSE, grouping <- TRUE)

    data_fit_brms <- fit_brms$data
    ## Code for converting argument group into values for group_by()
    ## Based on: https://stackoverflow.com/a/68866579/5322644
    if(grouping) {
        grp_lst <- as.list(substitute(group))
        print(grp_lst)
        if(length(grp_lst)> 1) grp_lst <- grp_lst[-1]
        grps <- purrr::map_chr(grp_lst, rlang::as_string)
        print(grps)
    }
    
        ## Create grid of x values for epred/predictions
#    groupings <- names(data_fit_brms) 
    data_grid <- data_fit_brms %>%
        group_by(across(all_of(grps))) %>%
        #group_by(grps) %>%
        data_grid(x = seq_range(c(20, 46), n = 51))


  ## add expected values
  data_epred <- data_grid %>%
    add_epred_draws(object = fit_brms, ndraws = ndraws, cores = ncores)

  return(data_epred)
}

## Generate data for plotting range of predicted values
generate_pred_data <- function(fit_brms = NULL, ndraws = 1000, ncores = 4) {

  data_fit_brms <- fit_brms$data
  
  data_grid <- data_fit_brms %>%
    group_by(disp_group, male) %>%
    data_grid(x = seq_range(c(20, 45.9), n = 51)) %>%  #, .model = fit_brms) %>% 
    ungroup() 

  ## add simulated values
  data_pred <- data_grid %>%
    add_predicted_draws(object = fit_brms, ndraws = ndraws, ncores = ncores )

  return(data_pred)
}


plot_epred<- function(data_epred, male_vec, desc, data_obs = NULL) {
  ## Plot
  ## Only plot specified males
  data_epred_tmp <- data_epred %>%
    filter(male %in% male_vec)
  plot_tmp <- ggplot(data = data_epred_tmp, aes(x = x, y = .epred)) + #, color = male)) +
    ## Combine Scatter Plots and Model vs Data Plots
    stat_lineribbon(aes(y = .epred), .width = c(.95, .8, 0.5, 0.25), color = "#08519C") +
    scale_fill_brewer(palette = "Greys") 

  #scale_color_manual(values = colors_male) +
  #scale_color_brewer(palette = "Set2") +

  if(!is.null(data_obs)){

    data_obs_tmp <- data_obs %>% filter(male %in% male_vec)    
    plot_tmp <- plot_tmp +
      geom_point(data = data_obs_tmp,
                 aes(x = x, y = y), color = "red")
    title <- "Expected Values & Data vs. Temp"
    y_max <- max(data_obs_tmp$y)*1.1
  } else {

    title <- "Expected Values vs. Temp"
    y_max <- NA
  }

  plot_tmp <- plot_tmp +
    ylim(0, y_max) +
    facet_wrap(vars(male)) +
    labs(title = title, subtitle = desc)
  
  return(plot_tmp)
}

plot_pred <- function(data_pred, male_vec, desc, data_obs = NULL) {
  ## Plot
  ## Only plot specified males
  data_pred_tmp <- data_pred %>%
    filter(male %in% male_vec)
  plot_tmp <- ggplot(data = data_pred_tmp, aes(x = x, y = .prediction)) + #, color = male)) +
    ## Combine Scatter Plots and Model vs Data Plots
    stat_lineribbon(aes(y = .prediction), .width = c(.95, .8, .5, 0.25), color = "#08519C") +
    scale_fill_brewer(palette = "Greys", direction =  -1)

  #scale_color_manual(values = colors_male) +
  #scale_color_brewer(palette = "Set2") +

  if(!is.null(data_obs)){

    data_obs_tmp <- data_obs %>% filter(male %in% male_vec)    
    plot_tmp <- plot_tmp +
      geom_point(data = data_obs_tmp,
                 aes(x = x, y = y), color = "red")
    title <- "Predicted Values & Data vs. Temp"
    y_max <- max(data_obs_tmp$y)*1.1
  } else {
    ymax <- NA
    title <- "Predicted Values vs. Temp"
  }


  plot_tmp <- plot_tmp +
    ylim(0, y_max) +
    facet_wrap(vars(male)) +
    labs(title = title, subtitle = desc)
  
  return(plot_tmp)
}

is_stanfit <- function(fit) if_else(class(fit) == "stanfit",  TRUE, FALSE)
is_brmsfit <- function(fit) if_else(class(fit) == "brmsfit",  TRUE, FALSE)
is_fit <- function(fit) if_else((is_stanfit(fit) | is_brmsfit(fit)), TRUE, FALSE)

verify_stanfit <- function(fit) if(!is_stanfit(fit)) stop(paste("fit must be of class `stanfit`, not ", class(fit), "."))
verify_brmsfit <- function(fit) if(!is_brmsfit(fit)) stop(paste("fit must be of class `brmsfit`, not ", class(fit), "."))
verify_fit <- function(fit) if(!is_stanfit(fit) & !is_brmsfit(fit)) stop(paste("fit must be of class `stanfit` or `brmsfit`, not ", class(fit), "."))

## Code derived from: https://discourse.mc-stan.org/t/select-chains-and-iterations-post-sampling-in-brms/6822/8
keep_chains <- function(fit, chains_to_keep) {

  verify_fit(fit)
  
  if(is_brmsfit(fit)) {
    sim = fit$fit@sim
  } else {
    sim = fit@sim
  }
  
  sim$samples <- sim$samples[chains_to_keep]
  
  ## Update the meta-info
  sim$chains = length(chains_to_keep)
  sim$warmup2 = sim$warmup2[chains_to_keep]
  
  ## Add the modified sim back to fit
  if(is_brmsfit(fit)) {
    fit$fit@sim <- sim
  } else {
    fit@sim <- sim
  }
  return(fit)
}


drop_chains <- function(fit, chains_to_drop) {
  # brm_fit is the output of brms::brm
  return(keep_chains(fit, -chains_to_drop))
}

# works on single brms/stan fit object.size

fit_to_tibble <- function(fit) {
    verify_fit(fit)
   
    if(is_brmsfit(fit)) fit <- fit$fit

    fit_tibble <- as.array(fit) %>%
        melt(.) %>%
        as_tibble() %>%
        mutate(chains = as.character(chains)) %>%
        mutate(
            chains = sub("chain:", "", chains) %>%
                as.integer()
        )
    return(fit_tibble)
}


calc_fit_lp_stats <- function(fit) {
  ## Convert to tibble if necessary
    if(is_tibble(fit)) {
        fit_tibble <- fit
    } else {
        fit_tibble <- fit_to_tibble(fit)
    }

    lp_tbl <- fit_tibble %>% filter(parameters == "lp__")

  ## Calc stats and arrange so fit with highest mean is at top 
  lp_stats <- lp_tbl %>%
    group_by(chains) %>%
    summarize(mean = mean(value), sd = sd(value), n = n(), se = sd/sqrt(n) )

  return(lp_stats)
}


## Takes in tibble, returns a tibble

keep_chains_top <- function(fit, n_chains_max, se_factor = 4, verbose = FALSE, best_only = TRUE) {

  ## make sure correct object is passed
  verify_fit(fit)
  
  fit_tibble <- fit_to_tibble(fit)
  ## Calc stats and arrange so fit with highest mean is at top 
  lp_stats <- calc_fit_lp_stats(fit_tibble) %>%
      arrange(desc(mean))
  if(!best_only) {
    if(verbose) print(lp_stats)
    # Take top n_chains_max chains
    chains_top <- lp_stats[1:n_chains_max, "chains"] %>% unlist() %>% unname()
    if(verbose) print(paste0("Keeping ", n_chains_max, " top fitting chains out of ", nrow(lp_stats)))
    
  } else {
    ## Only use chains which match best
    ## Use chain with greatest mean lp__ value and calculate SE on mean
    mean_max <- lp_stats[[1, "mean"]]
    se <- lp_stats[[1, "se"]]
    ## Use a larger threshold to avoid 1/20 being dropped due to type 1 errors
    threshold <- mean_max - se_factor * se
    if(verbose) print(paste0("Threshold = ", threshold))
    ## Flag chains with matching means to max
    lp_stats <- lp_stats %>%
      mutate(match_max = mean > threshold)
    
    if(verbose) print(lp_stats)

    ## return to original order
    lp_stats <- lp_stats %>%
      arrange(chains)
    
    chains_top <- filter(lp_stats, match_max) %>%
      pull(chains) %>% unlist()
    #%>% # don't need anymore since chains is now numeric
    #sub("chain:", "", .) %>%
    #as.numeric()
    
    n_chains_top <- length(chains_top)
    n_chains <- unique(fit_tibble$chains) %>% length()

    if(verbose) {
      if(n_chains != n_chains_top) {
        print(paste("Chains Best:", paste(chains_top, collapse = ","), "(", n_chains_top, "out of", n_chains, ")"))
        print("Dropping other chains")
      }

      print("Chains Best vs. Chains Max Post Dropping")
      print(paste0("n_chains_top = ", n_chains_top,
                   ", n_chains_max = ", n_chains_max)
            )
    }
    if(n_chains_top > n_chains_max) {
      ## just take the first n target Chains
      if(verbose) {
        print(paste0("Dropping ", n_chains_top - n_chains_max, " extraneous chains"))
      }
      chains_top <- chains_top[1:n_chains_max]
    } else {
      if(verbose) {
        print(paste0("No extraneous chains to drop, have ", n_chains_top, " chains"))
      }
    }
  }
  
  ## Keep only desired chains
  fit_top <- keep_chains(fit, chains_top)

  return(fit_top)
}

add_column_safely <- function(tbl, names_col) {
  for(tmp in names_col) {
    ## note syntax for using value of variable to dynamically assign column name
    if(!(tmp %in% names(tbl))) tbl <- tbl %>%  add_column("{tmp}" := list(list()))
  }
  return(tbl)
}

## SYNTAX NOTE: When using the same naem for the argument and default value, you need to tell
## R your default is defined in the parent environment, not the local one of the
## function
## f=parent.frame()$f
last_plot_save <- function(file_prefix = parent.frame()$file_prefix,
                           desc = desc_filename,
                           output_dir = parent.frame()$output_dir,
                           scale = 1,
                           width = 8,
                           height = 10,
                           dpi = 300) {
  filename <- paste0(file_prefix, "_", desc_filename, ".pdf")
  ## print(output_dir)
  path <- file.path(output_dir, "figures")
  ggsave(filename = filename, path = path,
         plot = last_plot(),
         width = width,
         height = height,
         scale = scale,
         dpi= dpi,
         units = "in",
         bg = "white")
}
