#' @title marrow.gen
#' @description Generate a hyperframe of bone marrow representations from exported Qupath annotations and points.
#'
#' @import spatstat
#' @import sf
#' @import readr
#' @import tcltk
#' @import spatstat.geom
#'
#' @return A hyperframe
#'
#' @export
#'
#'
#' @examples
#' marrow.gen()
marrow.gen <- function() {

  # library(sf)
  # library(spatstat)
  # library(spatstat.geom)
  # library(readr)
  # library(tcltk)

  #Remove hyperframe from global environment if it already exists.

  if (exists("hf", where = .GlobalEnv)) {
    rm("hf", pos = .GlobalEnv)
  }


  print("Welcome to Callum's super fancy point pattern importer for analysis of ckit cells in bone marrow, remember, for this script to work you must have exported separate window, endothelial cell, adipocyte .geojson files from Qupath, AND you must have exported all the CKIT and DAPI classified cells AS POINTS from Qupath. Please wait...")

  Sys.sleep(1)

  print("Set working directory. Data will be read and saved here:")

  wd <- tk_choose.dir(caption = "Choose working directory")

  setwd(wd)


  repeat {

    #Import files
    print("Select window .geojson file")
    win_path <- tk_choose.files(caption = "Select window.geojson")
    print("Select endo .geojson file")
    endo_path <- tk_choose.files(caption = "Select endo.geojson")
    print("Select peri .geojson file")
    peri_path <- tk_choose.files(caption = "Select peri.geojson")
    print("Select points tsv file")
    points_path <- tk_choose.files(caption = "Select points file")

    #1. Polygons

    # Process window file(s)
    #win_file <- win_path
    #endo_file <- endo_path
    #peri_file <- peri_path

    #Generate sf data

    ann_win <- st_read(win_path)
    ann_endo <- st_read(endo_path)
    ann_peri <- st_read(peri_path)

    print("Generating enclosing window element...")

    #Generate outer bounding box

    bbox <- st_bbox(ann_win)
    #bbox <- st_as_sfc(bbox)
    #bbox <- st_bbox(bbox[[1]])

    #Define bounds of enclosing window

    xmin <- bbox[[1]]
    xmax <- bbox[[3]]
    ymin <- bbox[[2]]
    ymax <- bbox[[4]]

    #Generate window

    W <- owin(xrange = c(xmin, xmax), yrange = c(ymin, ymax))

    # Generate adipocyte and vascular annotations

    print("Generating vascular and adipocytic polygons...")

    st_crs(ann_peri) <- NA
    peri <- as.owin(ann_peri$geometry)
    peri[["xrange"]] <- c(xmin, xmax)
    peri[["yrange"]] <- c(ymin, ymax)

    st_crs(ann_endo) <- NA
    endo <- as.owin(ann_endo$geometry)
    endo[["xrange"]] <- c(xmin, xmax)
    endo[["yrange"]] <- c(ymin, ymax)

    #plot(peri)
    #plot(endo)


    #2. POINTS

    # Process points files(s)

    print("Importing points file...")

    points <- read_delim(points_path, delim = "\t",
                         escape_double = FALSE, col_types = cols(...4 = col_skip()),
                         trim_ws = TRUE)

    #Change double positive cells to simply CKIT for analysis.

    points$class[points$class == "CKIT: DAPI"] <- "CKIT"

    #View(points)

    #Split point data based on location along tibia.
    #dt_points <- subset(points, x < 20000)
    #mt_points <- subset(points, x >= 20000 & x <= 30000)
    #pt_points <- subset(points, x > 30000)

    #If you want to view the points then de-hash the following:

    #Create objects of the x, y coordinates and staining marks (m)

    x <- points$x
    y <- points$y
    m <- points$class

    #Set observation window
    #pt_win <- owin(xrange = c(xmin, xmax), yrange = c(ymin, ymax))

    #Create point pattern object using above objects and window generated before that.
    print("Creating point pattern...")
    pp <- ppp(x, y, window = W, marks = m)

    #Factorise the marks for the point patterns

    pf <- factor(pp$marks)
    marks(pp) <- pf

    #Change units

    unitname(pp) <- "pixels"

    #Create cell class objects
    ckit <- pp[pp$marks == "CKIT"]


    print("Building hyperframe...")

    #Create factor group

    if(!exists("groups")) {
      groups <- c()
    }

    groups <- c(groups, readline(prompt = "Name the sample group:"))


    #Create solist

    # Sample <- solist(W, endo, peri, pp, ckit)


    #Add to hyperframe

    row <- rbind(solist(W, endo, peri, pp, ckit))

    if (!exists("hf")) {
        hf <- as.hyperframe(row, stringsAsFactors = TRUE)
        } else {
        new_hf <- as.hyperframe(row, stringsAsFactors = TRUE)
        hf <- rbind(hf, new_hf)
      }

    print("Hyperframe built")

    # Ask the user if they want to restart

    flush.console()
    answer <- readline(prompt="Do you want to add another sample set? (y/n): ")

    #Check the user's input
    if (tolower(answer) != "y") {
        break
    }
  }


  names(hf) <- c("win", "vasc", "adipo", "cells", "ckit")

  #Rescale the hyperframe to micrometres

  for (cn in names(hf)){
      hf[[cn]] <- solapply(hf[[cn]], rescale, s = 2.41370987, unitname = "\u03bcm")
    }

  #Add on the group factors

  groups <- hyperframe(groups, stringsAsFactors = TRUE)
  hf$group <- cbind(groups)

  nr <- nrow(hf)
  rownames(hf) <- as.character(c(1:nr))

  hf <<- hf

  save <- readline(prompt = "Do you want to save your hyperframe? (y/n):")

  if(save == "y"){

    hf_name <- readline(prompt = "Please name your hyperframe:")
    hf_name <- paste0(hf_name, "rds")
    saveRDS(hf, file = hf_name)
    print("Hyperframe has been saved in the working directory")

  }

}
