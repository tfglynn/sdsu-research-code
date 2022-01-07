##################################################
### Loading datasets ###
##################################################

datapath <- function(p) here::here("data", p)

#' Wisconsin breast cancer data set
#' @export
cancer_dataset <-
  read_csv(datapath("breast cancer/breast-cancer-wisconsin.data"),
           na = "?",
           col_types = cols(), # Silence column spec message
           col_names = c("id", "thickness", "size_uniformity",
                         "shape_uniformity", "adhesion", "size",
                         "bare_nuclei", "bland_chromatin",
                         "normal_nuclei", "mitoses", "malignant")) %>%
  # 2 for benign, 4 for malignant
  mutate(class = factor(if_else(malignant == 4, "malignant", "benign"))) %>%
  select(-malignant) %>%
  drop_na

#' King-rook vs king-pawn data set
#' @export
chess_dataset <-
  read_csv(datapath("chess/kr-vs-kp.data"),
           col_types = cols(),
           col_names = c("bkblk", "bknwy", "bkon8", "bkona", "bkspr", "bkxbq",
                         "bkxcr", "bkxwp", "blxwp", "bxqsq", "cntxt", "dsopp",
                         "dwipd", "hdchk", "katri", "mulch", "qxmsq", "r2ar8",
                         "reskd", "reskr", "rimmx", "rkxwp", "rxmsq", "simpl",
                         "skach", "skewr", "skrxp", "spcop", "stlmt", "thrsk",
                         "wkcti", "wkna8", "wknck", "wkovl", "wkpos", "wtoeg",
                         "won")) %>%
  mutate(across(everything(), factor)) %>%
  rename(class = won)

#' Thyroid disease data set
#' @export
hypothyroid_dataset <-
  read_csv(datapath("hypothyroid/hypothyroid.data"),
           na = "?",
           col_types = cols(),
           col_names = c("hypothyroid", "age", "sex", "on_thyroxine",
                         "query_on_thyroxine", "on_antithyroid_medication",
                         "thyroid_surgery", "query_hypothyroid",
                         "query_hyperthyroid", "pregnant", "sick", "tumor",
                         "lithium", "goitre", "TSH_measured", "TSH",
                         "T3_measured", "T3", "TT4_measured", "TT4",
                         "T4U_measured", "T4U", "FTI_measured", "FTI",
                         "TBG_measured", "TBG")) %>%
  mutate(sex = replace_na(sex, "X"),
         across(ends_with("measured"), . %>% `==`(., "y")),
         across(where(~ !is.double(.x)), factor),
         across(where(is.double), . %>% replace_na(., mean(., na.rm = TRUE)))) %>%
  rename(class = hypothyroid)

#' Mushroom data set
#' @export
mushroom_dataset <-
  read_csv(datapath("mushroom/agaricus-lepiota.data"),
           na = "?",
           col_types = cols(),
           col_names = c("edible", "cap_shape", "cap_surface", "cap_color",
                         "bruises", "odor", "gill_attachment", "gill_spacing",
                         "gill_size", "gill_color", "stalk_shape", "stalk_root",
                         "stalk_surface_above_ring", "stalk_surface_below_ring",
                         "stalk_color_above_ring", "stalk_color_below_ring",
                         "veil_type", "veil_color", "ring_number", "ring_type",
                         "spore_print_color", "population", "habitat")) %>%
  mutate(stalk_root_NA = is.na(stalk_root),
         stalk_root = replace_na(stalk_root, "x"),
         across(where(~ !is.double(.x)), factor)) %>%
  rename(class = edible) %>%
  select(-veil_type) # only one factor

#' Data sets used in the Kohavi replication experiments
#' @export
my_dataset_list <-
  list(list(name = "cancer",
            tibble = cancer_dataset,
            sample_size = 50),
       list(name = "chess",
            tibble = chess_dataset,
            sample_size = 200),
       list(name = "hypothyroid",
            tibble = hypothyroid_dataset,
            sample_size = 200),
       list(name = "mushroom",
            tibble = mushroom_dataset,
            sample_size = 200))

#' @export
readable <- function(fname) {
  unname(file.access(fname, mode = 4) == 0)
}

#' @export
savepath <- function(p) {
  if (missing(p)) {
    here::here("saved")
  } else {
    here::here("saved", p)
  }
}

#' @export
save_data <- function(obj, fname) {
  ensure_dir_exists(savepath())
  saveRDS(obj, savepath(fname))
}

#' @export
read_saved_data <- function(fname) {
  readRDS(savepath(fname))
}
