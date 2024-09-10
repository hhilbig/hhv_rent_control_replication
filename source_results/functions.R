# Add line break

add_linebreak <- function(string, min_length = 10, add_multiple_linebreaks = F) {
    if (nchar(string) > min_length) {
        if (!add_multiple_linebreaks) {
            l <- nchar(string)
            find_space <- str_locate_all(string, " |\\-") %>%
                .[[1]] %>%
                data.frame() %>%
                pull(start) %>%
                .[which.min(abs(. -
                    (nchar(string) / 2)))]
            substr(string, find_space, find_space) <- "\n"
            string
        } else {
            find_space <- str_locate_all(string, " |\\-") %>%
                .[[1]] %>%
                data.frame() %>%
                slice(-1) %>%
                pull(1)
            for (i in find_space) {
                substr(string, i, i) <- "\n"
            }
            string
        }
    } else {
        string
    }
}

# Add line break - vector

add_linebreak_vector <- function(string, ...) {
    sapply(string, function(s) add_linebreak(s, ...))
}

# Tidy RD output

tidy_rd <- function(model, se_nr) {
    est <- model$coef[se_nr]
    se <- model$se[se_nr]
    low <- model$ci[se_nr, 1]
    high <- model$ci[se_nr, 2]
    zstat <- model$z[se_nr]
    pval <- model$pv[se_nr]
    h_left <- model$bws[1, 1]
    h_right <- model$bws[1, 2]
    b_left <- model$bws[2, 1]
    b_right <- model$bws[2, 2]
    p <- model$p
    n <- sum(model$N_h)
    smry <- tibble(
        estimate = est, std.error = se, statistic = zstat,
        p.value = pval, conf.low = low, conf.high = high, p = p,
        n = n, bw_left_h = h_left, bw_right_h = h_right, bw_left_b = b_left,
        bw_right_b = b_right
    )
    return(smry)
}

# String filter


str_filter <- function(string, pattern, neg = F) {
    if (neg == F) {
        string %>% .[str_detect(., pattern)]
    } else {
        string %>% .[!str_detect(., pattern)]
    }
}

# MAke binary

make_binary <- function(v, method = "median", q = 0.75) {
    if (!(method %in% c("mean", "median", "quantile"))) {
        stop("Unknown method")
    }
    if (!method == "quantile") {
        cutoff <- ifelse(method == "mean", mean(v, na.rm = T),
            median(v, na.rm = T)
        )
        b <- ifelse(v > cutoff, 1, 0)
    } else {
        cutoff <- quantile(v, probs = q, na.rm = T)
        b <- ifelse(v > cutoff, 1, 0)
    }
    b
}

# Tidy feols

tidy_feols <- function(model, ...) {
    if (!is.null(names(model))) {
        tidy_feols_single(model, ...)
    } else {
        lapply(model, function(x) {
            tidy_feols_single(x, ...)
        }) %>% reduce(bind_rows)
    }
}

# Tidy feols single

tidy_feols_single <- function(
    model, add_glance = T, add_dv_stats = T, add_conf_90 = T,
    add_first_rhs_stats = T) {
    if (!names(model)[1] == "nobs") {
        cat("Looks like this model uses more than one outcome;\nPlease use tidy_feols instead")
    } else {
        n <- model$nobs
        m_tidy <- broom::tidy(model, conf.int = F) %>% mutate(conf.low = estimate -
            qnorm(0.975) * std.error, conf.high = estimate +
            qnorm(0.975) * std.error)
        if (n > 0) {
            dv_val <- model.matrix(model, type = "lhs")
        } else {
            dv_val <- NA
        }
        out <- m_tidy %>% mutate(n = n)
        if (add_conf_90) {
            out <- out %>% mutate(conf.low90 = estimate - qnorm(0.95) *
                std.error, conf.high90 = estimate + qnorm(0.95) *
                std.error)
        }
        dv_mean <- dv_val %>% mean(na.rm = T)
        dv_sd <- dv_val %>% sd(na.rm = T)
        dv_min <- dv_val %>% min(na.rm = T)
        dv_max <- dv_val %>% max(na.rm = T)
        if (is.nan(dv_mean)) {
            dv_mean <- NA
        }
        if (is.nan(dv_sd)) {
            dv_sd <- NA
        }
        if (is.nan(dv_min)) {
            dv_min <- NA
        }
        if (is.nan(dv_max)) {
            dv_max <- NA
        }
        if (add_dv_stats) {
            out <- out %>% mutate(
                dv_mean = dv_mean, dv_sd = dv_sd,
                dv_min = dv_min, dv_max = dv_max
            )
        }
        if (add_first_rhs_stats) {
            if (n > 0) {
                rhs <- model.matrix(model, type = "rhs")
                if (!all(rhs[, 1] == 1)) {
                    first_rhs <- rhs[, 1]
                } else {
                    first_rhs <- rhs[, 2]
                }
                first_rhs_mean <- first_rhs %>% mean(na.rm = T)
                first_rhs_sd <- first_rhs %>% sd(na.rm = T)
                first_rhs_min <- first_rhs %>% min(na.rm = T)
                first_rhs_max <- first_rhs %>% max(na.rm = T)
            } else {
                first_rhs_mean <- NA
                first_rhs_sd <- NA
                first_rhs_min <- NA
                first_rhs_max <- NA
            }
            out <- out %>% mutate(
                first_rhs_mean = first_rhs_mean,
                first_rhs_sd = first_rhs_sd, first_rhs_min = first_rhs_min,
                first_rhs_max = first_rhs_max
            )
        }
        fml <- model$fml
        dv_lab <- fml %>%
            as.character() %>%
            {
                .[2]
            }
        fml <- Reduce(paste, deparse(fml)) %>%
            str_squish() %>%
            str_replace_all("\"", "")
        out <- out %>% mutate(dv = dv_lab, fml = deparse(fml))
        fe_vars <- paste0(model$fixef_vars, collapse = ", ")
        out <- out %>% mutate(fixef_vars = fe_vars)
        if (add_glance) {
            g <- model %>%
                broom::glance() %>%
                dplyr::select(matches("squared|f_"))
            out <- out %>% mutate(rsq = g$r.squared, a_rsq = g$adj.r.squared)
            out %>% dplyr::select(term, dv, fml, everything())
        } else {
            out %>% dplyr::select(term, dv, fml, everything())
        }
    }
}

# Summarize vector

summarize_vec <- function(vector) {
    mean_v <- mean(vector, na.rm = T)
    median_v <- median(vector, na.rm = T)
    sd_v <- sd(vector, na.rm = T)
    n_obs <- length(vector[!is.na(vector)])
    min_v <- min(vector, na.rm = T)
    max_v <- max(vector, na.rm = T)
    data.frame(cbind(mean_v, median_v, sd_v, n_obs, min_v, max_v))
}
