## scientific_10 ---------------------------------------------------------------
scientific_10 <- function(x) {
  parse(text = gsub("e",  " %*% 10^", scales::scientific_format()(x)))
}

## Read Files -------------------------------------------------------------------
read_files <- function(file = NA, path = NA, header = TRUE , ...) {
  ## file <- "filename.csv"
  
  if (is.na(path)) {
    file_path <- file 
  } else {
    file_path <- paste0(path, file) 
  }
  
  file_path <- stringr::str_to_lower(file_path)
  
  dot <- 
    data.frame(
      dot = stringr::str_split(file_path, pattern = "")[[1]],
      num = 1:nchar(file_path)
    )
  
  num_dot <- max(dot[dot$dot == ".", "num"])
  
  suffix <-  stringr::str_sub(file_path, num_dot, nchar(file_path))
  
  if (suffix == ".csv") {
    readr::read_csv(file_path, col_names = header) 
  } else if (suffix == ".tsv") {
    readr::read_tsv(file_path, col_names = header)
  } else if (suffix == ".xlsx") {
    readxl::read_xlsx(file_path, col_names = header)
  } else if (suffix == ".xls") {
    readxl::read_xls(file_path, col_names = header)
  } else {
    cat("file isn't csv, tsv, xls or xlsx")
  }
}

## Pr and En digits ------------------------------------------------------------
digit_fa <- function(x = NA, fa = TRUE) {
  ## x <- 5468
  digit <- data.frame(
    "en" = as.character(0:9),
    "fa" = c("۰", "۱", "۲", "۳", "۴", "۵", "۶", "۷", "۸", "۹")
    )
  
  x <- as.character(x)
  x <- strsplit(x, split = "")
  x <- x[[1]]
  
  digit_fa  <- c()
  
  if (fa) {
    for (i in seq_along(x)) {
      # i = 1
      digit_fa[i] <- 
        ifelse(x[i] %in% digit$en, digit$fa[digit$en == x[i]], x[i])
    }
  } else {
      for (i in seq_along(x)) {
      # i = 1
      digit_fa[i] <- 
        ifelse(x[i] %in% digit$fa,  digit$en[digit$fa == x[i]], x[i])
      }
  }
  
  digit_fa <- paste0(digit_fa, collapse = "")
  
  return(digit_fa)
}


## Jalali to gorgian -----------------------------------------------------------
date_gr_to_jl <- function(date = Sys.Date()) {
  
  date <- as.Date(date)
  
  ## Convert Gregorian Date to Jalali Date
  jalali <-
    ConvCalendar::as.OtherDate(date, format = "%d%b%Y", calendar = "persian")

  ## Convert English Digits to Persian Digits
  jalali <- paste(jalali$year, jalali$month, jalali$day, sep = "-")

  return(jalali)
}

## My Table Style --------------------------------------------------------------
my_table_style <- function(dt) {
  dt <- kableExtra::kable_classic(dt)
  dt <- kableExtra::kable_styling(
    kable_input = dt,
    latex_options = "HOLD_position", full_width = TRUE, html_font = "IRANSans",
    bootstrap_options = c("striped", "hover", "condensed", "responsive")
  )

  return(dt)
}

## scale plot ------------------------------------------------------------------
scale_plot <- 
  function(
    data = NULL,
    parameter = NA, 
    unit = NA, 
    label = NA,
    breaks_10 = 4,
    important = FALSE
    )  {
  
  p <- 
    ggplot(data, aes(x = scale)) +
    geom_text(aes(x = 1, y = 1, label = "mohsenebrahimy.ir"), 
              hjust = 0, color = "grey", size = 7/.pt, alpha = 0.5) +
    geom_text(
      aes(
        y = 2,
        label = sprintf("%s = %s", material, scale),
        color = important
        ),
      family = "IRANSans(FaNum)", angle = 90, size = 6/.pt
      ) +
    scale_color_manual(values = c("FALSE" = "black", "TRUE" = "red")) +
    labs(
      x = sprintf("%s (%s)", label, unit)
    ) +
    theme_classic(base_family = "IRANSans(FaNum)", base_size = 7) +
    theme(
      legend.position = "none",
      axis.title.y = element_blank(),
      axis.line.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.x = element_text(color ="black")
    ) +
    scale_x_log10(labels = scientific_10, breaks = 10^seq(-60, 60, breaks_10)) +
    scale_y_continuous(limits = c(1, 3))
  
  ggsave(
    filename = sprintf("./img/chap-1/scale-of-%s.png", parameter), 
    plot = p, bg = "white", width = 185, height = 60, units = "mm"
    )
}

## popup -----------------------------------------------------------------------
show_answer <- function(answer = NA, text = "جواب") {
  show_answer <- 
    paste0(
      "<div class='popup' onclick='show_answer()'>",
      text,
      "<span class='popuptext' id='answer'>",
      answer,
      "</span></div>"
    )
  
  return(show_answer)
}