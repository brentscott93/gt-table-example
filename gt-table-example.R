#load R packages
library(data.table)
library(gt)
library(ggplot2)

# read in data
# parameter tables
base_pars <- fread("base-pars.csv")
hypercube_pars <- fread("hypercube-pars.csv")

# predicted force-pCa fit lines from Fibersim
base_lines <- fread("base-lines.csv")
sample_lines <- fread("sample-lines.csv")

# ktr values from FiberSim
base_ktr <- fread("base-ktr-points.csv")
sample_ktr <- fread("sample-ktr-points.csv")

# merge two paramater tables, calculating new values for shared columns
# divides the hypercube values by the base model

# columns to merge (all of them except the id)
merging_cols <- names(base_pars)[2:17]

# table merge data.table style
pars_dif <-
  hypercube_pars[base_pars,
                 on = "id",
                 lapply(
                   setNames(merging_cols, merging_cols),
                   function(x){
                     if(x != "viscosity*") {
                       get(x) / get(paste0("i.", x))
                     } else {
                       get(x) - get(paste0("i.", x))
                     }
                   }
                 ),
                 nomatch=0L,
                 by=.EACHI
                 ]

#split the id column which was a file path to get just the sample
# this will split the /path/to/file/name in the id column by the "/" and put the text values in new columns
pars_dif[, c("f1", "f2", "f3", "id", "si", "1", "filename") := tstrsplit(id, "/", fixed = TRUE)]

# remove the columns that you dont want to show in the graph
pars_dif <- pars_dif[, -c("f1", "f2", "f3", "si", "1", "filename",
                          "pps_drx_1", "pps_drx_2", "pps_drx_3", "pps_post",
                          "post_drx_3", "post_drx_4")]

# add two new columns to the data.table where the graph columns will go
# these columns should hold the ID of the samples
# we define plotting functions below that will take the ID values, filter the data, and plot just the one sample

# add the columns
# the := is the "walrus" operator and modifies the table in place, no need for the "carrot" <-
pars_dif[, "Force-pCa" := id]
pars_dif[, "ktr-pCa" := id]

# plotting functions to pass to gt() table function using ggplot2 package
plot_force_pca <- function(pick){
ggplot()+
  geom_line(data = sample_lines[id == pick], aes(x_fit, y_fit), color = "red", linewidth = 3)+
  geom_line(data = base_lines, aes(x_fit, y_fit), color = "black", linewidth = 3)+
  scale_x_reverse()+
  theme_void()+
  theme(legend.position = "none")
}

plot_ktr_pca <- function(pick){
ggplot()+
  geom_point(data = sample_ktr[id == pick], aes(pCa, ktr), color = "red", size = 7)+
  geom_line(data = sample_ktr[id == pick], aes(pCa, ktr), color = "red", linewidth = 3)+
  geom_point(data = base_ktr, aes(pCa, ktr), color = "black", size = 7)+
  geom_line(data = base_ktr, aes(pCa, ktr), color = "black", linewidth = 3)+
  scale_x_reverse()+
  theme_void()
}

# make the actual table which is done with the gt package
# this will generate an HTML file with the table
gt(pars_dif[order(-drx_pps)],
   id = "tab")|>
  tab_spanner(
    label = "m_kinetics",
    columns = 2:8
  ) |>
  tab_spanner(
    label = "thick_params",
    columns = 9
  ) |>
  tab_spanner(
    label = "thin_params",
    columns = 10
  ) |>
  tab_spanner(
    label = "lattice_params",
    columns = 11
  ) |>
  fmt_number(decimals = 2)|>
  # the text transform will draw the plots in the columns
  # it takes the cell values, the sample ID name, and passes the cell value into the supplied function
  # which we defined above with the plot_force_pca and plot_ktr_pca
  text_transform(
    locations = cells_body(columns = "Force-pCa"),
    fn = function(column) {
      lapply(column, plot_force_pca) |>
        ggplot_image(height = px(50), aspect_ratio = 2)
    }
  ) |>
  text_transform(
    locations = cells_body(columns = "ktr-pCa"),
    fn = function(column) {
      lapply(column, plot_ktr_pca) |>
        ggplot_image(height = px(50), aspect_ratio = 2)
    }
  ) |>
  # we can then map color onto the cells with data_color here
  data_color(
    columns = 2:10,
    ## method = "numeric",
    direction = "column",
    palette =  c("white", "blue"),
    domain = c(0, 3)
  )|>
  # last we can float the header column when you scroll down the page with CSS since its an HTML table
  opt_css(css =
            "
 .cell-output-display {
      overflow-x: unset !important;
    }
    div#tab {
      overflow-x: unset !important;
      overflow-y: unset !important;
    }
    #tab .gt_col_heading {
      position: sticky !important;
      top: 0 !important;
    }
    "
 )
