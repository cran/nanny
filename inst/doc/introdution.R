## ---- echo=FALSE, include=FALSE-----------------------------------------------
library(knitr)
knitr::opts_chunk$set(cache = TRUE, warning = FALSE,
                      message = FALSE, cache.lazy = FALSE)

library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)
library(magrittr)
library(nanny)

my_theme = 	
	theme_bw() +
	theme(
		panel.border = element_blank(),
		axis.line = element_line(),
		panel.grid.major = element_line(size = 0.2),
		panel.grid.minor = element_line(size = 0.1),
		text = element_text(size=12),
		legend.position="bottom",
		aspect.ratio=1,
		strip.background = element_blank(),
		axis.title.x  = element_text(margin = margin(t = 10, r = 10, b = 10, l = 10)),
		axis.title.y  = element_text(margin = margin(t = 10, r = 10, b = 10, l = 10))
	)


## ---- eval=FALSE--------------------------------------------------------------
#  
#  devtools::install_github("stemangiola/nanny")
#  

## -----------------------------------------------------------------------------
mtcars_tidy = 
	mtcars %>% 
	as_tibble(rownames="car_model") %>% 
	mutate_at(vars(-car_model,- hp, -vs), scale) %>%
	gather(feature, value, -car_model, -hp, -vs)

mtcars_tidy

## ----mds, cache=TRUE----------------------------------------------------------
mtcars_tidy_MDS =
  mtcars_tidy %>%
  reduce_dimensions(car_model, feature, value, method="MDS", .dims = 3)


## ----plot_mds, cache=TRUE-----------------------------------------------------
mtcars_tidy_MDS %>% subset(car_model)  %>% select(contains("Dim"), everything())

mtcars_tidy_MDS %>%
	subset(car_model) %>%
  GGally::ggpairs(columns = 4:6, ggplot2::aes(colour=factor(vs)))



## ----pca, cache=TRUE, message=FALSE, warning=FALSE, results='hide'------------
mtcars_tidy_PCA =
  mtcars_tidy %>%
  reduce_dimensions(car_model, feature, value, method="PCA", .dims = 3)

## ----plot_pca, cache=TRUE-----------------------------------------------------

mtcars_tidy_PCA %>% subset(car_model) %>% select(contains("PC"), everything())

mtcars_tidy_PCA %>%
	 subset(car_model) %>%
  GGally::ggpairs(columns = 4:6, ggplot2::aes(colour=factor(vs)))

## ----tsne, cache=TRUE, message=FALSE, warning=FALSE, results='hide'-----------
mtcars_tidy_tSNE =
	mtcars_tidy %>% 
	reduce_dimensions(car_model, feature, value, method = "tSNE")

## -----------------------------------------------------------------------------
mtcars_tidy_tSNE %>%
	subset(car_model) %>%
	select(contains("tSNE"), everything()) 

mtcars_tidy_tSNE %>%
	subset(car_model) %>%
	ggplot(aes(x = `tSNE1`, y = `tSNE2`, color=factor(vs))) + geom_point() + my_theme

## ----rotate, cache=TRUE-------------------------------------------------------
mtcars_tidy_MDS.rotated =
  mtcars_tidy_MDS %>%
	rotate_dimensions(`Dim1`, `Dim2`, .element = car_model, rotation_degrees = 45, action="get")

## ----plot_rotate_1, cache=TRUE------------------------------------------------
mtcars_tidy_MDS.rotated %>%
	ggplot(aes(x=`Dim1`, y=`Dim2`, color=factor(vs) )) +
  geom_point() +
  my_theme

## ----plot_rotate_2, cache=TRUE------------------------------------------------
mtcars_tidy_MDS.rotated %>%
	ggplot(aes(x=`Dim1 rotated 45`, y=`Dim2 rotated 45`, color=factor(vs) )) +
  geom_point() +
  my_theme

## ----cluster, cache=TRUE------------------------------------------------------
mtcars_tidy_cluster = mtcars_tidy_MDS %>%
  cluster_elements(car_model, feature, value, method="kmeans",	centers = 2, action="get" )

## ----plot_cluster, cache=TRUE-------------------------------------------------
 mtcars_tidy_cluster %>%
	ggplot(aes(x=`Dim1`, y=`Dim2`, color=cluster_kmeans)) +
  geom_point() +
  my_theme

## ----SNN, cache=TRUE, message=FALSE, warning=FALSE, results='hide', include=FALSE, eval=FALSE, echo=FALSE----
#  mtcars_tidy_SNN =
#  	mtcars_tidy_tSNE %>%
#  	cluster_elements(car_model, feature, value, method = "SNN")

## ----SNN_plot, cache=TRUE, include=FALSE, eval=FALSE, echo=FALSE--------------
#  mtcars_tidy_SNN %>%
#  	subset(car_model) %>%
#  	select(contains("tSNE"), everything())
#  
#  mtcars_tidy_SNN %>%
#  	subset(car_model) %>%
#  	ggplot(aes(x = `tSNE1`, y = `tSNE2`, color=cluster_SNN)) + geom_point() + my_theme
#  

## ----drop, cache=TRUE---------------------------------------------------------
mtcars_tidy_non_redundant =
	mtcars_tidy_MDS %>%
  remove_redundancy(car_model, feature, value)

## ----plot_drop, cache=TRUE----------------------------------------------------
mtcars_tidy_non_redundant %>%
	subset(car_model) %>%
	ggplot(aes(x=`Dim1`, y=`Dim2`, color=factor(vs))) +
  geom_point() +
  my_theme


## ----drop2, cache=TRUE, include=FALSE, eval=FALSE, echo=FALSE-----------------
#  mtcars_tidy_non_redundant =
#  	mtcars_tidy_MDS %>%
#    remove_redundancy(
#    	car_model, feature, value,
#    	method = "reduced_dimensions",
#    	Dim_a_column = `Dim1`,
#    	Dim_b_column = `Dim2`
#    )

## ----plot_drop2, cache=TRUE, include=FALSE, eval=FALSE, echo=FALSE------------
#  mtcars_tidy_non_redundant %>%
#  	subset(car_model) %>%
#  	ggplot(aes(x=`Dim1`, y=`Dim2`, color=factor(vs))) +
#    geom_point() +
#    my_theme
#  

## -----------------------------------------------------------------------------
mtcars_tidy_non_rectangular = mtcars_tidy %>% slice(-1)

## -----------------------------------------------------------------------------
mtcars_tidy_non_rectangular %>% fill_missing(car_model, feature, value, fill_with = 0)

## -----------------------------------------------------------------------------
mtcars_tidy_non_rectangular %>% mutate(vs = factor(vs)) %>% 
	impute_missing( car_model, feature, value,  ~ vs) %>%
	
	# Print imputed first
	arrange(car_model != "Mazda RX4" | feature != "mpg")

## -----------------------------------------------------------------------------
mtcars_tidy_permuted = 
	mtcars_tidy %>%
	permute_nest(car_model, c(feature,value))

mtcars_tidy_permuted

## -----------------------------------------------------------------------------
mtcars_tidy %>%
	combine_nest(car_model, value)

## -----------------------------------------------------------------------------
mtcars_tidy_permuted %>%
	
	# Summarise mpg
	mutate(data = map(data, ~ .x %>% filter(feature == "mpg") %>% summarise(mean(value)))) %>%
	unnest(data) %>%
	
	# Lower triangular
	lower_triangular(car_model_1, car_model_2,  `mean(value)`)

## -----------------------------------------------------------------------------
mtcars_tidy %>%
	keep_variable(car_model, feature, value, top=10)

## -----------------------------------------------------------------------------
mtcars_tidy %>%
	select(car_model, feature, value) %>%
	spread(feature, value) %>%
	as_matrix(rownames = car_model) %>%
	head()

## -----------------------------------------------------------------------------
mtcars_tidy %>%
	subset(car_model)


## -----------------------------------------------------------------------------
mtcars_tidy %>% nest_subset(data = -car_model)


## ---- cache=TRUE--------------------------------------------------------------
  mtcars_tidy

## ---- cache=TRUE--------------------------------------------------------------
  mtcars_tidy %>%
    reduce_dimensions(
    	car_model, feature, value, 
    	method="MDS" ,
    	.dims = 3,
    	action="add"
    )

## ---- cache=TRUE--------------------------------------------------------------
  mtcars_tidy %>%
    reduce_dimensions(
    	car_model, feature, value, 
    	method="MDS" ,
    	.dims = 3,
    	action="get"
    )

## ---- cache=TRUE--------------------------------------------------------------
  mtcars_tidy %>%
    reduce_dimensions(
    	car_model, feature, value, 
    	method="MDS" ,
    	.dims = 3,
    	action="only"
    )

