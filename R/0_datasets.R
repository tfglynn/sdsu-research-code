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

commonest <- function(x) x[[which.max(tabulate(x))]]

#' Soybean (large) data set
#' @export
soybean_dataset <-
  read_csv(datapath("soybean large/soybean-large.data"),
           na = "?",
           col_types = cols(),
           col_names = c("class", "date", "plant_stand", "precip", "temp",
                         "hail", "crop_hist", "area_damaged", "severity",
                         "seed_tmt", "germination", "plant_growth", "leaves",
                         "leafspots_halo", "leafspots_marg", "leafspot_size",
                         "leaf_shread", "leaf_malf", "leaf_mild", "stem",
                         "lodging", "stem_cankers", "canker_lesion",
                         "fruiting_bodies", "external_decay", "mycelium",
                         "int_discolor", "sclerotia", "fruit_pods",
                         "fruit_spots", "seed", "mold_growth", "seed_discolor",
                         "seed_size", "shriveling", "roots")) %>%
  mutate(across(everything(), factor),
         across(everything(), . %>% replace_na(., commonest(.))))

#' Vehicle silhouettes data set
#' @export
# The data files have trailing spaces that cause `read_delim` to emit warnings.
# To prevent this, we strip the spaces first.
vehicle_dataset <-
  list.files(datapath("vehicle/"),
             pattern = "dat$",
             full.names = TRUE) %>%
  lapply(. %>%
         readLines %>%
         trimws(which = "right") %>%
         paste(sep = "\n") %>%
         I %>%
         read_delim(delim = " ",
                    col_types = cols(),
                    col_names = c("compactness", "circularity",
                                  "distance_circularity", "radius_ratio",
                                  "axis_aspect_ratio", "length_aspect_ratio",
                                  "scatter_ratio", "elongatedness",
                                  "axis_rectangularity",
                                  "length_rectangularity",
                                  "scaled_variance_along_major_axis",
                                  "scaled_variance_along_minor_axis",
                                  "scaled_radius_of_gyration",
                                  "skewness_about_major_axis",
                                  "skewness_about_minor_axis",
                                  "kurtosis_about_minor_axis",
                                  "kurtosis_about_major_axis", "hollows_ratio",
                                  "class"))) %>%
  bind_rows %>%
  mutate(class = factor(class))

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
            sample_size = 200),
       list(name = "soybean",
            tibble = soybean_dataset,
            sample_size = 100),
       list(name = "vehicle",
            tibble = vehicle_dataset,
            sample_size = 100))

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
