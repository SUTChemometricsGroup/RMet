require(gWidgets2)
require(gWidgets2RGtk2)
require(RGtk2)
require(ggplot2)


options ( guiToolkit="RGtk2" )
options(warn=-1)
memory.limit(size = 90000)
options ( guiToolkit="RGtk2" )
options(warn=-1)
memory.limit(size = 90000)

# source code
#rstudioapi::getSourceEditorContext()$path

window00 <- gwindow( "RMet" , visible  = TRUE  )

w00_lyt1 <- glayout(cont = window00)

w00_lyt1[1:50,1:50,anchor = c(0,0)] <- image001 <- gimage(filename = "C:/Program Files (x86)/RMet/RMet/Other/Pics/best_ini.png")

window22 <- gwindow("Preprocessing & segmentation" , visible = FALSE)

getToolkitWidget(window22)$maximize()

w00_lyt1[20:24,35:49,anchor = c(0,0)] <- load_lab <- glabel("Loading...")
font(load_lab) <- list(weight = "bold", color = "Blue" , size = 16 , background = "black")

window2 <- gwindow ( "Preprocessing & segmentation" , visible = FALSE)
window3 <- gwindow ( "Determining number of metabolites" , visible = FALSE, width = 1200,height = 650 )
getToolkitWidget(window3)$maximize()

getToolkitWidget(window2)$maximize()
#load(file = "C:/Program Files (x86)/RMet/RMet/Other/Pics/TIC.Rdata")
window8 <- gwindow ( "Identification of important metabolites" , visible = FALSE, width = 1200,height = 650 )
getToolkitWidget(window8)$maximize()


window4 <- gwindow ( "Data compression" , visible = FALSE )
getToolkitWidget(window4)$maximize()

window7 <- gwindow( "classifivation" , visible  = FALSE  )
getToolkitWidget(window7)$maximize()

window5 <- gwindow ( "Interface" , visible = FALSE )
getToolkitWidget(window5)$maximize()


window6 <- gwindow ( "Pure metabolites profiles" , visible = FALSE, width = 1200,height = 650 )
getToolkitWidget(window6)$maximize()

window0 <- gwindow ( "RMet" , visible = FALSE )
window <- gwindow ( "Importing data" , visible = FALSE )
window2 <- gwindow ( "Preprocess & segmentation" , visible = FALSE, width = 1200,height = 650 )
#getToolkitWidget(window0)$maximize()
getToolkitWidget(window0)
getToolkitWidget(window)$maximize()
getToolkitWidget(window2)$maximize()
#getToolkitWidget(window00)$maximize()





require(fields)
require(RNetCDF)
require(hash)
require(abind)
require(rgl)
require(wavelets)
require(ALS)
require(MASS)
require(mixOmics)
require(Matrix)
#require(corpcor)
require(irlba)

w00_lyt1[19:24,35:49,anchor = c(0,0)] <- but001 <- gbutton("Start Analysis" )
font(but001) <- list(weight = "bold", color = "#235DE3" , size = 16 )

w00_lyt1[1:3,48:50,anchor = c(0,0)] <- but002 <- gbutton("About" )
font(but002) <- list(weight = "bold", color = "#235DE3" , size = 12 )

addHandlerClicked(but001, function(h,...){
  visible(window0) <<- TRUE
  visible(window00) <<- FALSE
  w0_lyt <- glayout(cont = window0)
  w0_lyt[1,1:135 , anchor = c(0,0)] <- rmet_lab <- glabel("RMet workflow")
  font(rmet_lab) <- list(weight = "bold", color = "blue",size = 16)
  w0_lyt[1,115:135 , anchor = c(0,0)] <- help_but <- gbutton("Help")
  font(help_but) <- list(weight = "bold", color = "blue",size = 10)
  
  
  w0_lyt[2:63 , 5:43] <- gimage(filename = "C:/Program Files (x86)/RMet/RMet/Other/Pics/blue_back3.png")
  w0_lyt[33 , 44:49] <-  gimage(filename = "C:/Program Files (x86)/RMet/RMet/Other/Pics/arrow_right.png" )
  
  w0_lyt[2:63 , 50:88] <- gimage(filename = "C:/Program Files (x86)/RMet/RMet/Other/Pics/red_back3.png")
  
  w0_lyt[33 , 89:94] <-  gimage(filename = "C:/Program Files (x86)/RMet/RMet/Other/Pics/arrow_right.png" )
  w0_lyt[2:63 , 95:133] <- gimage(filename = "C:/Program Files (x86)/RMet/RMet/Other/Pics/green_back3.png")
  
  
  w0_lyt[2:63,5:43,anchor = c(0,0)] <- fr_lyt1 <-glayout(cont = w0_lyt)
  w0_lyt[2:63,50:88,anchor = c(0,0)] <- fr_lyt2 <-glayout(cont = w0_lyt)
  w0_lyt[2:63,95:133,anchor = c(0,0)] <- fr_lyt3 <-glayout(cont = w0_lyt)
  
  
  
  fr_lyt1[1,1:39,anchor = c(0,0)] <- intro_label <- glabel ( "--------- Step 1: Import & Preprocess ---------" , cont = fr_lyt1 )
  font(intro_label) <- list(weight = "bold", color = "black",size = 12)
  
  fr_lyt3[1,1:39,anchor = c(0,0)] <- intro_label <- glabel ( "-------------- Step 3: Interpretation --------------" , cont = fr_lyt1 )
  font(intro_label) <- list(weight = "bold", color = "black",size = 12)
  
  
  fr_lyt2[1,1:39,anchor = c(0,0)] <- intro_label <- glabel ( "------------------ Step 2: Analysis ----------------" , cont = fr_lyt1 )
  font(intro_label) <- list(weight = "bold", color = "black",size = 12)
  
  fr_lyt1[6:12,3:36,anchor = c(0,0)] <- button_img1 <- gbutton ( "1)Data import" , cont = fr_lyt1 )
  font(button_img1) <- list(weight = "bold", color = "blue" , size = 12)
  
  fr_lyt1[23:29,3:36,anchor = c(0,0)] <- button_img2 <- gbutton ( "2)Preprocess & Segmentation" , cont = fr_lyt1 )
  font(button_img2) <- list( weight = "bold",color = "blue" , size = 12)
  enabled(button_img2) <- FALSE
  
  fr_lyt1[39:45,3:36,anchor = c(0,0)] <- button_img3 <- gbutton ( "3)Determining number of metabolites" , cont = fr_lyt1)
  font(button_img3) <- list( weight = "bold",color = "blue" , size = 12)
  enabled(button_img3) <- FALSE
  
  fr_lyt1[54:60,3:36,anchor = c(0,0)] <- button_img4 <- gbutton ( "4)Data compression" , cont = fr_lyt1 )
  font(button_img4) <- list( weight = "bold",color = "blue" , size = 12)
  enabled(button_img4) <- FALSE
  
  fr_lyt2[12:18,3:36,anchor = c(0,1)] <- button_img5 <- gbutton ( "5)Building model" , cont = fr_lyt2 )
  font(button_img5) <- list( weight = "bold",color = "red" , size = 12)
  
  
  
  fr_lyt2[29:35,3:36,anchor = c(0,0)] <- button_img6 <- gbutton ( "6)Pure metabolites' profiles" , cont = fr_lyt2 )
  font(button_img6) <- list( weight = "bold",color = "red" , size = 12)
  enabled(button_img6) <- FALSE
  
  fr_lyt2[44:50,3:36,anchor = c(0,-1)] <- button_img7 <- gbutton ( "7)Metabolites classification" , cont = fr_lyt2 )
  font(button_img7) <- list( weight = "bold",color = "red" , size = 12)
  enabled(button_img7) <- FALSE
  
  #fr_lyt3[24,3:36,anchor = c(0,1)] <- button_img8 <- gbutton ( "8)Important metabolites determination" , cont = fr_lyt3 )
  #font(button_img8) <- list( weight = "bold",color = "dark gray" , size = 12)
  
  
  fr_lyt3[29:35,3:36,anchor = c(0,1)] <- button_img8 <- gbutton ( "8)Idenfication of important metabolites" , cont = fr_lyt3 )
  font(button_img8) <- list( weight = "bold",color = "dark green" , size = 12)
  
  
  addHandlerChanged(button_img1 , handler = function(h,...){
    visible(window0) <- FALSE
    visible(window) <- TRUE
    plot(0,type = "n",xlab = "1st column time points", ylab = "2nd column time points", main = "Total Ion Chromatogram")
    win_type[] <<- c( "GC*GC-MS" ,"GC-MS")
    win_format[] <<- c( ".cdf" ,".Rdata", ".csv")
    
  })
  
  addHandlerChanged(button_img2 , handler = function(h,...){
    visible(window0) <- FALSE
    visible(window2) <<- TRUE
    plot(0,type = "n",xlab = "1st column time points", ylab = "2nd column time points", main = "Total Ion Chromatogram")    
  })
  
  addHandlerChanged(button_img3 , handler = function(h,...){
    visible(window0) <- FALSE
    svalue(win5_comp_numb) <<- svalue(comp_spin)
    dev.off()
    visible(window3) <<- TRUE
    plot(0)
    win3_table[] <<- s_tab
    win3_data_combo[] <<- s_tab
  })
  
  addHandlerChanged(button_img4 , handler = function(h,...){
    visible(window0) <- FALSE
    svalue(win5_comp_numb) <<- svalue(comp_spin)
    dev.off()
    visible(window3) <<- FALSE
    visible(window4) <<- TRUE
    matplot(0 , type = "l" , xlab = "Retention time" , ylab = "Intensity" , main = "Chromatogram of compressed data")
    if(win3_tab_2 == "  ")
      data_for_comp[] <<- s_tab
    else
      data_for_comp[] <<- win3_tab_2
    
  })
  
  addHandlerChanged(button_img5 , handler = function(h,...){
    visible(window0) <- FALSE
    dev.off()
    visible(window4) <<- FALSE
    visible(window5) <<- TRUE
    win5_aug_combo[] <<- s_tab
    print(s_tab)
    matplot(0 , main = "Mass spectra" , xlab = "m/z" , ylab = "Intensity")
  })
  
  addHandlerChanged(button_img6 , handler = function(h,...){
    visible(window0) <- FALSE
    visible(window6) <<- TRUE
    plot(0,type = "n",xlab = "Retention time", ylab = "Intensity", main = "Chromatogram")
    
    prof_combo[] <<- s_tab
    win6_comp_combo[] <<- c(1: svalue(win5_comp_numb))
    
  })
  
  addHandlerChanged(button_img7 , handler = function(h,...){
    visible(window0) <- FALSE
    dev.off()
    visible(window7) <<- TRUE
    plot(0)
    
  })
  
  addHandlerChanged(button_img8 , handler = function(h,...){
    visible(window0) <- FALSE
    dev.off()
    visible(window8) <<- TRUE
    plot(0 , main = "VIP plot")
  })
  
  
  
})




##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
# Best data opening

lyt <- glayout ( cont = window , spacing = 5)

right <- c(1,0) 
left <- c(-1,0)
lyt [2,2 , anchor =  c(0,0) ] <- imp_lab <- glabel ( "Import data:" , cont = lyt )
font(imp_lab) <- list( weight = "bold",color = "Blue" , size = 10)

lyt [2,5:10 , anchor =  c(0,0) ] <- glabel ( "Data category:" , cont = lyt )
lyt[2,11:40] <- win_cate <- gcombobox ( c ( "Raw data" ,"MCR-results"))

lyt [2,41:55 , anchor =  c(0,0) ] <- glabel ( "Data type:" , cont = lyt )

lyt[2,56:80] <- win_type <- gcombobox (c( "GC*GC-MS" ,"GC-MS"))

lyt [2,81 :90 , anchor =  c(0,0) ] <- glabel ( "Data format:" , cont = lyt )
lyt[2,91:110] <- win_format <- gcombobox (c( ".cdf" ,".Rdata", ".csv"))

lyt [2, 115:130 , anchor =  c(0,0) ] <- search_button <- gbutton ( "Browse & import" , cont = lyt  )
enabled(search_button) <- FALSE
font(search_button) <- list( weight = "bold",color = "Blue" , size = 10)
lyt [2, 131:300 , anchor = right ] <- cdf_gedit <- gedit( initial.msg  = "Data directory..." , cont = lyt  )

#lyt [2:100, 1:9  ] <- lyt_1 <- glayout(container = lyt) 

s_tab <- c("No imported data")
vir <<- TRUE
lyt[3 , 1:18,anchor = c(0,0)  ] <- win_imp_lab <- glabel( "------ imported data ------")
font(win_imp_lab) <- list(weight = "bold", color = "Blue" , size = 10 )
lyt[3 , 19:36,anchor = c(0,0)  ] <- win_imp_info <- glabel( "----- Data information -----")
font(win_imp_info) <- list(weight = "bold", color = "Blue" , size = 10)

lyt[4:180 , 2:18  ] <- info_tabel <- gtable(s_tab )

name_tab1 <- c("Type:","Category:","Size:", "Format:","Dimensions:","Mod. time:","Frequency:" ,"Min mass val. ","Max mass val." , "1st col. TPs:" , "2nd col. TPs:" )
name_tab2 <- c(" "," ", " ", " ", " " )
lyt[4:70 , 19:29 , anchor = c(0,0)] <- table_info <- gtable(items = name_tab1 )
lyt[4:70 , 29:37 , anchor = c(0,0)] <- table_info2 <- gtable(items = name_tab2)

lyt[71:72 , 19:36,anchor = c(0,0)  ] <- win_imp_vis <- glabel( "---- Data visualization ----")
font(win_imp_vis) <- list(weight = "bold", color = "Blue" , size = 10)

lyt[73:77 , 19:28,anchor = c(0,0)  ] <- "Plot dimensions:"
lyt[73:77 , 29:36,anchor = c(0,0)  ] <- plot_combo <- gcombobox(c("2D   ","3D    " ))
lyt[78:83 , 19:36,anchor = c(0,0)  ] <- plot_but <- gbutton("Plot data")

lyt[85:90 , 19:36,anchor = c(0,0)  ] <- win_imp_sav <- glabel( "----------- Saving -----------")
font(win_imp_sav) <- list(weight = "bold", color = "Blue" , size = 10 )

lyt[91:96 , 19:36,anchor = c(0,0)  ] <- data_save_but <- gbutton("Save data as .Rdata ")
lyt[97:102 , 19:36,anchor = c(0,0)  ] <- TIC_save_but <- gbutton("Save plot")


addHandlerChanged(TIC_save_but, function(h,...){
  
  pic_dir <<- gfile( text = "Select a name to save data" ,initial.dir = getwd() ,type = "save" , cont = lyt )
  dev.print(png , filename = paste(pic_dir , ".png",sep = "")) 
  dev.off()
})




lyt[105:110 , 19:36,anchor = c(0,0)  ] <- win_step <- glabel( "------------ Step ------------")
font(win_step) <- list(weight = "bold", color = "Blue" , size = 10 )

lyt[ 115:135 , 19:36 , anchor = c(0,0) ] <- next_button_1 <- gbutton ( "Next")

lyt[ 145:165 , 19:36 , anchor = c(0,0) ] <- back_button_1 <- gbutton ( "Home")

lyt[3:180,40:300]<- plot_view <- ggraphics()



second_col_hash <- hash()
data_type_hash <- hash()
freq_hash <- hash()
mod_t_hash <- hash()
min_mass_hash <- hash()
max_mass_hash <- hash()
cat_hash <- hash()
main_data_hash <- hash()
first_col_hash <- hash()

ini_data_directory <- c

addHandlerChanged(data_save_but , function(h,...){
  print(svalue(info_tabel))
  saving_dir <<- gfile( text = "Select a name to save data" ,initial.dir = getwd() ,type = "save" , cont = lyt )
  saving_dat <- get(svalue(info_tabel)) 
  save(saving_dat , file = paste(saving_dir , ".Rdata",sep = "") )
  
})


addHandlerClicked ( search_button , handler = function ( h ,... ) {
  
  if ((svalue(win_type) == "GC-MS") & ( svalue( win_cate) == "Raw data")){
    input_data_dir <<- gfile( text = "Select a .cdf file ..." ,initial.dir = getwd() ,type = "open" , cont = lyt , multi = TRUE )
    svalue(cdf_gedit) <<- "Uploading data..."
    enabled(search_button) <<- FALSE
    
    
    for (ii in input_data_dir){
      
      #   visible(window_inter) <<- FALSE
      
      #import_counter <<- import_counter + 1
      file_dir <<- ii
      split_dir <<- unlist(strsplit(file_dir,".",fixed = TRUE))
      #data_type <<- split_dir[length(split_dir)]
      split_name <- unlist(strsplit(file_dir,"\\",fixed = TRUE))
      data_name <<- split_name[length(split_name)]
      data_type <<- unlist(strsplit(data_name,".",fixed = TRUE))[length(unlist(strsplit(data_name,".",fixed = TRUE)))]
      data_name <<- gsub(".cdf","",data_name)
      
      if (data_type == "cdf"){
        
        
        raw1 <<- open.nc(file_dir, write=FALSE, share=FALSE, prefill=TRUE)
        data1 <<- read.nc(raw1, unpack=TRUE)
        
        time_range <<- data1$mass_range_max[1]
        point_count <- data.matrix(data1$point_count)
        mass_values <- data.matrix(data1$mass_values)
        intenity_values <- data.matrix(data1$intensity_values)
        cdf_row <- nrow(point_count) +1 - 1
        value_row <- nrow(mass_values) +1 -1
        scan_index <- data.matrix(data1$scan_index)
        raw_in_mat <- matrix(rep(0,time_range * cdf_row),nrow = cdf_row,ncol = time_range)
        holder <- 0
        for (i in c(1:(cdf_row-1))){
          raw_in_mat[i,mass_values[(scan_index[i]+1):scan_index[i+1]]] <- intenity_values[(scan_index[i]+1):scan_index[i+1]]
          
        }  
        raw_in_mat[cdf_row,mass_values[(scan_index[cdf_row]+1):value_row]] <- intenity_values[(scan_index[cdf_row]+1):value_row]
        raw_in_mat <- raw_in_mat
        
        second_col_hash[[data_name]] <<- 1
        first_col_hash[[data_name]] <<- 1
        #      main_data_hash[[r_data_name]] <<- raw_in_mat
        #      main_data_hash <<- main_data_hash
        assign(data_name,raw_in_mat, envir = .GlobalEnv)
        
        if (vir == TRUE){
          s_tab <<- c(data_name)
          vir <<- FALSE
        }else{
          if (data_name %in% s_tab)
            gmessage ( "This file is aleardy uploaded" , title = "Existing data" )
          else
            s_tab <<- append(s_tab,data_name)
        }
        
        
      }
      
      
    }
    svalue(cdf_gedit) <<- "Browse & import"  
    enabled(search_button) <<- TRUE
    font(search_button) <<- list( weight = "bold",color = "Blue" , size = 10)
    info_tabel[] <<- s_tab
    win22_dat_tab[] <<- s_tab
    win22_data_combo[] <<- s_tab
    
  }
  
  
  if ((svalue(win_type) == "GC*GC-MS") & ( svalue( win_cate) == "Augment") ){
    input_data_dir <<- gfile( text = "Select a .cdf file ..." ,initial.dir = getwd() ,type = "open" , cont = lyt , multi = TRUE )
    
    
    svalue(cdf_gedit) <<- "Uploading data..."
    enabled(search_button) <<- FALSE
    
    for (ii in input_data_dir){
      
      
      #import_counter <<- import_counter + 1
      file_dir <<- ii
      split_dir <<- unlist(strsplit(file_dir,".",fixed = TRUE))
      #data_type <<- split_dir[length(split_dir)]
      split_name <- unlist(strsplit(file_dir,"\\",fixed = TRUE))
      data_name <<- split_name[length(split_name)]
      data_type <<- unlist(strsplit(data_name,".",fixed = TRUE))[length(unlist(strsplit(data_name,".",fixed = TRUE)))]
      aug_fir <<- as.numeric(svalue(aug_first_tp))
      aug_sec <<- as.numeric(svalue(aug_second_tp))
      #      first_col_tp <<- (freq * mod_t)
      
      if (data_type == "Rdata"){
        
        r_data_name <<- gsub(".Rdata","",data_name)
        
        second_col_hash[[r_data_name]] <<- aug_sec
        first_col_hash[[r_data_name]] <<- aug_fir
        
        data_type_hash[[r_data_name]] <<- svalue(win_type)
        freq_hash[[r_data_name]] <<- " " 
        mod_t_hash[[r_data_name]] <<- " "
        cat_hash[[r_data_name]] <<- svalue(win_cate)
        #     main_data_hash[[r_data_name]] <<- get(load(file = file_dir))
        #      main_data_hash <<- main_data_hash
        
        assign(r_data_name,get(load(file = file_dir)), envir = .GlobalEnv)
        #       real_first_col <- (dim(get(r_data_name))[1] / first_col_tp)
        #        first_col_hash[[r_data_name]] <<- real_first_col
        
        #  x <- get(load(file = file_dir))
        #  assign(paste(r_data_name),x, envir = .GlobalEnv)
        
        if (vir == TRUE){
          s_tab <<- c(r_data_name)
          vir <<- FALSE
        }
        else{
          if (r_data_name %in% s_tab)
            gmessage ( "This file is aleardy uploaded" , title = "Existing data" )
          else
            s_tab <<- append(s_tab,r_data_name)
        }
        info_tabel[] <<- s_tab
        win2_dat_tab[] <<- s_tab
        win2_data_combo[] <<- s_tab
        #svalue(win2_data_combo) <<- s_tab[1]
        
        font(search_button) <<- list( weight = "bold",color = "Blue" , size = 10)
        
        
        
        
      }
      
      
      
    }
    svalue(cdf_gedit) <<- "Browse & import"  
    enabled(search_button) <<- TRUE
    font(search_button) <<- list( weight = "bold",color = "Blue" , size = 10)
    
  }
  
  if ((svalue(win_type) == "GC*GC-MS") & ( svalue( win_cate) == "Raw data") ){
    
    input_data_dir <<- gfile( text = "Select a .cdf file ..." ,initial.dir = getwd() ,type = "open" , cont = lyt , multi = TRUE )
    svalue(cdf_gedit) <<- "Uploading data..."
    enabled(search_button) <<- FALSE
    
    
    for (ii in input_data_dir){
      svalue(cdf_gedit) <<- "Uploading data..."
      
      #import_counter <<- import_counter + 1
      file_dir <<- ii
      split_dir <<- unlist(strsplit(file_dir,".",fixed = TRUE))
      #data_type <<- split_dir[length(split_dir)]
      split_name <- unlist(strsplit(file_dir,"\\",fixed = TRUE))
      data_name <<- split_name[length(split_name)]
      data_type <<- unlist(strsplit(data_name,".",fixed = TRUE))[length(unlist(strsplit(data_name,".",fixed = TRUE)))]
      
      
      if (data_type == "Rdata"){
        
        r_data_name <<- gsub(".Rdata","",data_name)
        
        second_col_hash[[r_data_name]] <<- first_col_tp
        data_type_hash[[r_data_name]] <<- svalue(win_type)
        freq_hash[[r_data_name]] <<- freq
        mod_t_hash[[r_data_name]] <<- mod_t
        cat_hash[[r_data_name]] <<- svalue(win_cate)
        #     main_data_hash[[r_data_name]] <<- get(load(file = file_dir))
        #      main_data_hash <<- main_data_hash
        
        assign(r_data_name,get(load(file = file_dir)), envir = .GlobalEnv)
        real_first_col <<- (dim(get(r_data_name))[1] / first_col_tp)
        first_col_hash[[r_data_name]] <<- real_first_col
        
        #  x <- get(load(file = file_dir))
        #  assign(paste(r_data_name),x, envir = .GlobalEnv)
        
        if (vir == TRUE){
          s_tab <<- c(r_data_name)
          vir <<- FALSE
        }
        else{
          if (r_data_name %in% s_tab)
            gmessage ( "This file is aleardy uploaded" , title = "Existing data" )
          else
            s_tab <<- append(s_tab,r_data_name)
        }
        info_tabel[] <<- s_tab
        win2_dat_tab[] <<- s_tab
        win2_data_combo[] <<- s_tab
        #svalue(win2_data_combo) <<- s_tab[1]
        
        
        
        
      }
      
      if (data_type == "cdf"){
        raw1 <- open.nc(file_dir, write=FALSE, share=FALSE, prefill=TRUE)
        data1 <- read.nc(raw1, unpack=TRUE)
        second_col_hash[[data_name]] <<- first_col_tp
        data_type_hash[[data_name]] <<- svalue(win_type)
        freq_hash[[data_name]] <<- freq
        mod_t_hash[[data_name]] <<- mod_t
        min_mass_hash[[data_name]] <<- data1$mass_range_min[1]
        max_mass_hash[[data_name]] <<- data1$mass_range_max[1]
        cat_hash[[data_name]] <<- svalue(win_cate)
        
        svalue(cdf_gedit) <- "Reconstructing cdf"
        time_range <- data1$mass_range_max[1]
        point_count <- data.matrix(data1$point_count)
        mass_values <- data.matrix(data1$mass_values)
        intenity_values <- data.matrix(data1$intensity_values)
        cdf_row <- nrow(point_count) +1 - 1
        value_row <- nrow(mass_values) +1 -1
        scan_index <- data.matrix(data1$scan_index)
        raw_in_mat <- matrix(rep(0,time_range * cdf_row),nrow = cdf_row,ncol = time_range)
        holder <- 0
        for (i in c(1:(cdf_row-1))){
          raw_in_mat[i,mass_values[(scan_index[i]+1):scan_index[i+1]]] <- intenity_values[(scan_index[i]+1):scan_index[i+1]]
          
        }  
        raw_in_mat[cdf_row,mass_values[(scan_index[cdf_row]+1):value_row]] <- intenity_values[(scan_index[cdf_row]+1):value_row]
        raw_in_mat <- raw_in_mat
        #      main_data_hash[[r_data_name]] <<- raw_in_mat
        #      main_data_hash <<- main_data_hash
        assign(data_name,raw_in_mat, envir = .GlobalEnv)
        first_col_hash[[data_name]] <<- dim(raw_in_mat)[1] / first_col_tp
        if (vir == TRUE){
          s_tab <<- c(data_name)
          vir <<- FALSE
        }
        else{
          if (data_name %in% s_tab)
            gmessage ( "This file is aleardy uploaded" , title = "Existing data" )
          else
            s_tab <<- append(s_tab,data_name)
        }
        info_tabel[] <<- s_tab
        
        win2_dat_tab[] <<- s_tab
        win2_data_combo[] <<- s_tab
        
        
      }
      
    }
    
    svalue(cdf_gedit) <<- input_data_dir[1]  
    enabled(search_button) <<- TRUE
    font(search_button) <<- list( weight = "bold",color = "Blue" , size = 10)
    
    
  }
  
  
})




addHandlerChanged(win_type, handler = function(h,...){
  if (svalue(win_type) == "GC-MS"){
    name_tab1 <- c("Type:","Category:","Size:", "Format:","Dimensions:","Ret. times:","Min mass val.:" ,"Max mass val.:" )
    table_info[] <<- name_tab1
  }
  
  if (svalue(win_type) == "GC*GC-MS"){
    name_tab1 <- c("Type:","Category:","Size:", "Format:","Dimensions:","Mod. time:","Frequency:" ,"Min mass val. ","Max mass val." , "1st col. TPs:" , "2nd col. TPs:" )
    table_info[] <<- name_tab1
    
    
    
    if(svalue(win_cate) == "Raw data"){
      
      window_inter <<- gwindow ( "Raw data information" , visible = TRUE , width = 250 , height = 100 )
      lyt_inter <- glayout ( cont = window_inter , spacing = 5)
      lyt_inter[1,1 , anchor = c(-1,0)] <- "Modulation time:"
      lyt_inter[1,2, anchor = c(0,0)] <- modu_edit <<- gedit(width = 25 , cont = lyt_inter)
      lyt_inter[2,1, anchor = c(-1,0)] <- "Detector frequency:"
      lyt_inter[2,2, anchor = c(0,0)] <- freq_edit <<- gedit(width = 25 , cont = lyt_inter)
      lyt_inter[3 ,1:2, anchor = c(0,0)] <- mod_feq_but <<- gbutton("Input" , cont = lyt_inter)
      
      addHandlerClicked(mod_feq_but, function(h,...){
        
        freq <<- as.numeric(svalue(freq_edit))
        mod_t <<- as.numeric(svalue(modu_edit))
        first_col_tp <<- (freq * mod_t)
        visible(window_inter) <<- FALSE
        
        
        
      })
    }
    
    
    if(svalue(win_cate) == "Augment"){
      
      window_inter <<- gwindow ( "Data information" , visible = TRUE , cont = window , width = 50 , height = 50)
      lyt_inter <- glayout ( cont = window_inter , spacing = 5)
      lyt_inter[1,1 , anchor = c(-1,0)] <- "First column time points:"
      lyt_inter[1,2, anchor = c(0,0)] <- aug_first_tp <<- gedit(width = 25 , cont = lyt_inter)
      lyt_inter[2,1, anchor = c(-1,0)] <- "Second column time points:"
      lyt_inter[2,2, anchor = c(0,0)] <- aug_second_tp <<- gedit(width = 25 , cont = lyt_inter)
      lyt_inter[3 ,1:2, anchor = c(0,0)] <- aug_fc_but <<- gbutton("Input" , cont = lyt_inter)
      
      
      addHandlerClicked(aug_fc_but, function(h,...){
        aug_fir <<- as.numeric(svalue(aug_first_tp))
        aug_sec <<- as.numeric(svalue(aug_second_tp))
        visible(window_inter) <<- FALSE
      })
      
      
    }
    
  }
  
  
})



addHandlerChanged(win_format , function(h ,...){
  enabled(search_button) <<- TRUE
})




addHandlerClicked(info_tabel, function(h,...){
  ind_tab <<- svalue ( info_tabel , index = TRUE )
  
  if (svalue(win_type) == "GC*GC-MS"){
    if (any(ind_tab)){
      sel_name <- s_tab[ind_tab]
      cdf_check <- grepl(".cdf",sel_name)
      name_tab2[1] <<- data_type_hash[[sel_name]]
      name_tab2[7] <<- freq_hash[[sel_name]]
      name_tab2[6] <<- mod_t_hash[[sel_name]]
      name_tab2[11] <<- second_col_hash[[sel_name]]
      name_tab2[2] <<- cat_hash[[sel_name]]
      
      if (cdf_check == FALSE)
        name_tab2[4] <<- ".Rdata"
      else{
        name_tab2[4] <<- ".cdf"
        name_tab2[8] <<- min_mass_hash[[sel_name]]
        name_tab2[9] <<- max_mass_hash[[sel_name]]
        
      }
      sel_dim <<- dim(get(eval(sel_name)))
      name_tab2[5] <<- paste(as.character(sel_dim[1]),as.character(sel_dim[2]),sep = " * "   )
      sel_size <<- object.size(get(eval(sel_name)))
      if (sel_size < 1000){
        sel_size <- round(sel_size,1)
        name_tab2[3] <<- paste(as.character(sel_size),"B")
      }
      #name_tab2[10] <<- (sel_dim[1] / second_col_hash[[sel_name]])
      name_tab2[10] <<- first_col_hash[[sel_name]]
      
      if ((sel_size >= 1000) & (sel_size < 1000000)){
        sel_size <- round((sel_size/1000),1)
        name_tab2[3] <<- paste(as.character(sel_size),"kB")
      }
      if ((sel_size >= 1000000) & (sel_size < 1000000000)){
        sel_size <- round((sel_size/1000000),1)
        name_tab2[3] <<- paste(as.character(sel_size),"Mb")
      }
      
      if ((sel_size >= 1000000000) & (sel_size < 1000000000000)){
        sel_size <- round((sel_size/1000000000),1)
        name_tab2[3] <<- paste(as.character(sel_size),"GB")
      }
      
      
      table_info2[] <- name_tab2 
    }
    
    
  }
  
  
})







addHandlerClicked(plot_but,function(h,...){
  #svalue(plot_but) <- "Plotting..."
  enabled(plot_but) <- FALSE
  table_index <<- svalue ( info_tabel , index = TRUE )
  plot_name <- s_tab[table_index]
  if (any(table_index)){
    #plot_cat <- cat_hash[[plot_name]]
    plot_cat <<- "Raw data"
    if(plot_cat == "Raw data")
      if (svalue(plot_combo) == "2D   ")
        if (svalue(win_type) == "GC*GC-MS"){
          #matplot(get(s_tab[table_index]), type = "l" , xlab = "Retention time", ylab = "Intensity", main = "Chromatogram")  
          
          tic_name <- s_tab[table_index]
          tic <- get(tic_name)
          
          #    data_row_sum <- rowSums(real_dat_tic)
          #    TIC_data <- matrix(data_row_sum, nrow =  second_col_hash[[tic_name]] , ncol = dim(real_dat_tic)[1] / second_col_hash[[tic_name]])
          #    tic <- TIC_data
          
          #print(tic_name)
          #tic <<-  main_data_hash[[tic_name]]
          
          #  tic <- get(tic_name, envir = parent.frame(n = 1) )
          
          sel_dim <<- dim(tic)
          first_col_tp <<- first_col_hash[[tic_name]]
          data_row_sum <- rowSums(tic)
          TIC_data <- matrix(data_row_sum, nrow =   , ncol = dim(tic)[1] / second_col_hash[[tic_name]])
          tic <- TIC_data
          
          #layout(t(1:2),widths=c(1,10))
          #x=0:10
          #par(mar=c(4,1.5,2,2.5))
          min_TIC <<- round(min(tic))
          maximum_TIC <<- round(max(tic))
          step_TIC <<- (maximum_TIC - min_TIC)/200
          cb_range <<- seq(min_TIC, maximum_TIC , step_TIC)
          #image(y=cb_range,z=t(cb_range), col = tim.colors(200), axes=FALSE, main="Intensity", cex.main=.8)
          #axis(2,cex.axis=0.8,mgp=c(0,.5,0))
          
          #par(mar=c(1,1,1,1))
          #par(mar=c(5,3.8,1.5,2))
          image(c(1:first_col_hash[[tic_name]]),c(1:second_col_hash[[tic_name]]),t(tic),col = tim.colors(200),xlab = "1st column time points",ylab = "2nd column time points", axes = TRUE,main = "Total Ion Chromatogram")
          grid()
          
          
          
          
          
        }else{
          matplot( rowSums(get(s_tab[table_index])) , type = "l" , xlab = "Retention time", ylab = "Intensity", main = "Chromatogram")  
          
        }
    
    
    else{
      flex <<- get(s_tab[table_index])
      
      sec_col <<- second_col_hash[[s_tab[table_index]]]
      
      if (svalue(win_type) == "GC*GC-MS"){
        sel_dim <<- dim(flex)
        fir_col <<- (sel_dim[1] / sec_col)
        
        data_row_sum <<- rowSums(flex)
        TIC_data <<- matrix(data_row_sum, nrow =  sec_col , ncol = fir_col)
        tic <<- TIC_data
        
        
        
        max_main_data <- max(tic)
        max_plot_height <- 200
        ratio <- max_main_data / max_plot_height
        edit_TIC_data <<- tic / ratio
        
        y <- edit_TIC_data
        x <- (1:nrow(tic))
        z <- (1:ncol(tic))
        ylim <- range(y)
        ylen <- ylim[2] - ylim[1] + 1
        colorlut <- tim.colors(round(ylen)) # height color lookup table
        col <- colorlut[ y - ylim[1] + 1 ] # assign colors to heights for each point
        
        #r3dDefaults$windowRect <- c(0,50, 700, 700 ) 
        par3d(windowRect = c(20, 30, 600, 600))
        rgl.viewpoint(theta = 310 , phi = 30, zoom = 1  )
        rgl.bg( color = c("#CBFFF9"))
        
        rgl.surface(x, z, y, color = col, back = "lines"   )
        axes3d()
        title3d(main = "3D-TIC", ylab = "Intensity" , xlab = "2nd column time points", zlab = "1st column time points")
        M <- par3d("userMatrix")
        play3d( par3dinterp(time = (0:2)*0.75, userMatrix = list(M,
                                                                 rotate3d(M, pi/2, 0, 1, 0),
                                                                 rotate3d(M, pi/2, 0, 0,1 ) ) ), duration = 3)
        
        #svalue(plot_but) <- "Plot data"
        enabled(plot_but) <- TRUE
        
        
      }else{
        tic <<- t(flex)
        
        
        
        max_main_data <- max(tic)
        max_plot_height <- 1000
        ratio <- max_main_data / max_plot_height
        edit_TIC_data <<- tic / ratio
        
        y <- edit_TIC_data
        x <- (1:nrow(tic))
        z <- (1:ncol(tic))
        ylim <- range(y)
        ylen <- ylim[2] - ylim[1] + 1
        colorlut <- tim.colors(round(ylen)) # height color lookup table
        col <- colorlut[ y - ylim[1] + 1 ] # assign colors to heights for each point
        
        #r3dDefaults$windowRect <- c(0,50, 700, 700 ) 
        par3d(windowRect = c(20, 30, 600, 600))
        rgl.viewpoint(theta = 310 , phi = 30, zoom = 1  )
        rgl.bg( color = c("#CBFFF9"))
        
        rgl.surface(x, z, y, color = col, back = "lines"   )
        axes3d()
        title3d(main = "3D-TIC", ylab = "Intensity" , xlab = "Retention time", zlab = "m/z")
        M <- par3d("userMatrix")
        play3d( par3dinterp(time = (0:2)*0.75, userMatrix = list(M,
                                                                 rotate3d(M, pi/2, 0, 1, 0),
                                                                 rotate3d(M, pi/2, 0, 0,1 ) ) ), duration = 3)
        
        #svalue(plot_but) <- "Plot data"
        enabled(plot_but) <- TRUE
      }
      
      
      
      
      #    for (i in c(1:sec_col)){
      #      for (j in c(1:fir_col)){
      #        if (tic[i,j] > max_tic){
      #          tic[i,j] <- max_tic
      #        }
      #      }
      #    }
      #    tic <<- tic
      
      
      
    }
    
  }
  
  #svalue(plot_but) <- "Plot data"
  enabled(plot_but) <- TRUE
})

#win2_dat_tab <- c()
#win2_data_combo <- c()

addHandlerChanged ( next_button_1 , handler = function ( h ,... ) {
  dev.off()
  if (svalue(win_type) == "GC*GC-MS"){
    visible(window) <<- FALSE
    visible(window2) <<- TRUE
    plot(0,type = "n",xlab = "1st column time points", ylab = "2nd column time points", main = "Total Ion Chromatogram")
  }else{
    visible(window) <<- FALSE
    visible(window22) <<- TRUE
    plot(0,type = "n",xlab = "Retention time", ylab = "Intensity", main = "Total Ion Chromatogram")
    
  }
  
  #  op_select[] <<- op_select_data
})

addHandlerChanged ( back_button_1 , handler = function ( h ,... ) {
  visible(window) <<- FALSE
  visible(window0) <<- TRUE
  plot(0,type = "n",xlab = "1st column time points", ylab = "2nd column time points", main = "Total Ion Chromatogram")
  
})



##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
# Best TIC plot GC


lyt22 <- glayout(container = window22 , spacing = 5)
lyt22[1:2,1:5,anchor = left ] <- win22_sel_mat <- glabel("Select a matrix :", cont = lyt22)
size(win22_sel_mat) <- c(85,25)


lyt22 [1:2,6:12] <- win22_data_combo <- gcombobox ( s_tab )
size(win22_data_combo) <- c(150,25)
lyt22 [3,1:12, anchor = c(0,0)] <- view_button22 <- gbutton ( "View/Start" , cont = lyt22  )

#matplot( rowSums(get(s_tab[table_index])) , type = "l" , xlab = "Retention time", ylab = "Intensity", main = "Chromatogram")

#size(view_button) <- c(150,25)
#lyt22 [7,1:6, anchor = c(0,0) ] <- win22_zoom <- glabel("Margin increase step:", container = lyt22) 
#lyt22 [7,7:12, anchor = c(-1,0) ] <- zoom_spin22 <- gspinbutton(from= 1, to = 100, by = 1, value = 2 , cont = lyt22)
#size(zoom_spin) <- c(15,20)
#lyt22 [8,1:6 ] <- l_but <- gbutton("Left",cont = lyt22_3)
#lyt22 [8,7:12 ] <- r_but <- gbutton("Right",cont = lyt22_3)
lyt22 [4,1:12, anchor = c(0,0) ] <- range_lab <- glabel("Select segment retetion time range:", container = lyt22) 
font(range_lab) <- list( weight = "bold", color = "blue")

lyt22 [5,1:2,anchor = c(0,0)] <- from_lablx22 <- glabel("From:")
lyt22 [5,3:6] <- x1_spin2 <- gspinbutton(from = 1 , to = 1, by = 1 , value = 1,cont = lyt22)
lyt22 [5,7:8,anchor = c(0,0)] <- to_lablx22 <- glabel("To:")
lyt22 [5,9:12] <- x2_spin2 <- gspinbutton(from = 1 , to = 1, by = 1 , value = 1,cont = lyt22)
lyt22 [6,1:12] <- view_but22 <- gbutton("Plot selected RTs")
lyt22 [7,1:12, anchor = c(0,0)] <- win22_lable1 <- glabel( "Select data to impose segmentation:", cont = lyt22)
font(win22_lable1) <- list( weight = "bold", color = "blue")
win22_tab <- s_tab
lyt22 [8:140,1:12, anchor = c(0,0)] <- win22_dat_tab <- gtable(win22_tab , multiple = TRUE)

lyt22 [141:150,1:12, anchor = c(0,0)] <- f_seg_but2 <- gbutton( "Impose", cont = lyt22 )
lyt22 [151:160,1:6, anchor = c(0,0)] <- prep_back_but2 <- gbutton( "Back", cont = lyt22 )
lyt22 [151:160,7:12, anchor = c(0,0)] <- prep_next_but2 <- gbutton( "Next", cont = lyt22 )



addHandlerChanged ( f_seg_but2 , handler = function ( h ,... ) {
  
  name_list <- c()
  seg_x21 <- svalue(x1_spin2)
  seg_x22 <- svalue(x2_spin2)
  data_win22_dat_tab <<- svalue(win22_dat_tab)
  
  for (i in data_win22_dat_tab){
    
    assign(paste("seg",i, sep = "_"), get(i)[c(seg_x21:seg_x22),]  , envir = .GlobalEnv)
    
    name_list[length(name_list) + 1 ] <- paste(paste("seg",i, sep = "_"))
    second_col_hash[[paste("seg",i, sep = "_")]] <<- 1
    first_col_hash[[paste("seg",i, sep = "_")]] <<- 1
    
    s_tab[length(s_tab) + 1] <<- paste("seg",i, sep = "_")
    s_tab<<-s_tab[-which(s_tab==i)]
    assign(i , NULL, envir = .GlobalEnv)
    win22_dat_tab[] <<- s_tab
    win22_data_combo[] <<- s_tab
    
  }
  
  
  
})



addHandlerChanged ( prep_next_but2 , handler = function (h ,... ) {
  svalue(win5_comp_numb) <<- svalue(comp_spin)
  dev.off()
  visible(window22) <<- FALSE
  visible(window3) <<- TRUE
  plot(0)
  win3_table[] <<- s_tab
  win3_data_combo[] <<- s_tab
  
})

addHandlerChanged(prep_back_but2 , function(h,...){
  dev.off()  
  visible(window22) <<- FALSE
  
  visible(window) <<- TRUE
  plot(0,type = "n",xlab = "1st column time points", ylab = "2nd column time points", main = "Total Ion Chromatogram")
  
})


size(f_seg_but2) <- c(9,40)
size(prep_back_but2) <- c(9,30)
size(prep_next_but2) <- c(9,30)

font(f_seg_but2) <- list(size = 9,  weight = "bold", color = "blue")
lyt22 [1:160,13:340]<- TIC_graph2 <-ggraphics(cont = lyt22 )


addHandlerChanged(x1_spin2 , function(h,...){
  matplot(tic_gc[svalue(x1_spin2):svalue(x2_spin2)], type = "l" , xlab = "Retention time", ylab = "Intensity", main = "Chromatogram")
})

addHandlerChanged(x2_spin2 , function(h,...){
  matplot(tic_gc[svalue(x1_spin2):svalue(x2_spin2)], type = "l" , xlab = "Retention time", ylab = "Intensity", main = "Chromatogram")
})



addHandlerChanged(view_button22 , function(h,...){
  
  tic_gc <<- rowSums(get(svalue(win22_data_combo)))
  matplot( tic_gc , type = "l" , xlab = "Retention time", ylab = "Intensity", main = "Chromatogram")
  x2_spin2[] <<- seq(1 , length(tic_gc) ,1)
  svalue(x2_spin2) <<- length(tic_gc)
  x1_spin2[] <<- seq(1 , length(tic_gc) ,1)
  svalue(x1_spin2) <<- 1
})

addHandlerChanged(view_but22 , function(h,...){
  matplot(tic_gc[svalue(x1_spin2):svalue(x2_spin2)], type = "l" , xlab = "Retention time", ylab = "Intensity", main = "Chromatogram")
  
})


if (FALSE){
  
  ########################################33
  for (j in c(seg_y1 : seg_y2)){
    segmented_mat <- c(segmented_mat , c((seg_x1+400*(j-1)):(seg_x2+400*(j-1))))
  }
  
  assign(paste(seg_order,i, sep = "_"), get(i)[segmented_mat,]  , envir = .GlobalEnv)
  
  name_list[length(name_list) + 1 ] <- paste(seg_order,i, sep = "_")
  second_col_hash[[paste(seg_order,i, sep = "_")]] <<- delta_y
  first_col_hash[[paste(seg_order,i, sep = "_")]] <<- delta_x
  
  s_tab[length(s_tab) + 1] <<- paste(seg_order,i, sep = "_")
  s_tab<<-s_tab[-which(s_tab==i)]
  assign(i , NULL, envir = .GlobalEnv)
  win2_dat_tab[] <<- s_tab
  win2_data_combo[] <<- s_tab
}





##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
# Best TIC plot


lyt2 <- glayout(container = window2 , spacing = 5)

lyt2[1,1:2,anchor = left ] <- win2_sel_mat <- glabel("Select a matrix :", cont = lyt2)
size(win2_sel_mat) <- c(85,20)

lyt2 [1,3:4] <- win2_data_combo <- gcombobox ( s_tab )
size(win2_data_combo) <- c(80,20)
lyt2 [2,1:2, anchor = c(-1,0)] <- win2_max_mat <- glabel("INT Ceiling:", cont = lyt2)
size(win2_sel_mat) <- c(85,20)


addHandlerClicked (win2_data_combo, function(h,...){
  if(visible(window2)){
    
    enabled(max_intensity) <<- TRUE
    enabled(view_but2) <<- TRUE
    
    #  assign(tic_name , svalue(win2_data_combo) , envir = .GlobalEnv)
    
    tic_name <<- svalue(win2_data_combo)
    tic <<- get(tic_name)
    
    #    data_row_sum <- rowSums(real_dat_tic)
    #    TIC_data <- matrix(data_row_sum, nrow =  second_col_hash[[tic_name]] , ncol = dim(real_dat_tic)[1] / second_col_hash[[tic_name]])
    #    tic <- TIC_data
    
    #print(tic_name)
    #tic <<-  main_data_hash[[tic_name]]
    
    #  tic <- get(tic_name, envir = parent.frame(n = 1) )
    
    sel_dim <<- dim(tic)
    first_col_tp <<- first_col_hash[[tic_name]]
    x1_spin[] <<- seq(1,first_col_tp , by = 1)
    x2_spin[] <<- seq(1,first_col_tp , by = 1)
    y1_spin[] <<- seq(1,second_col_hash[[tic_name]] , by = 1)
    y2_spin[] <<- seq(1,second_col_hash[[tic_name]] , by = 1)
    svalue(x1_spin) <<- 1
    svalue(x2_spin) <<- first_col_tp
    svalue(y1_spin) <<- 1
    svalue(y2_spin) <<- second_col_hash[[tic_name]]
    
    
    
    
    if (svalue(win_type) == "GC*GC-MS"){
      data_row_sum <- rowSums(tic)
      TIC_data <- matrix(data_row_sum, nrow =  second_col_hash[[tic_name]] , ncol = dim(tic)[1] / second_col_hash[[tic_name]])
      tic <- TIC_data
      
    }
    
    
    
    
    max_ceil <<- floor(max(tic)/1000) * 1000
    min_ceil <<- ceiling(min(tic)/1000) * 1000
    step_ceil <<- (max_ceil - min_ceil) / 100
    value_ceil <<- min_ceil + 50 * step_ceil
    
    max_intensity[] <<- seq(min_ceil,max_ceil , by = step_ceil)
    svalue(max_intensity) <<- value_ceil
    
    enabled(view_button) <<- TRUE
    enabled(x1_spin) <<- TRUE
    enabled(x2_spin) <<- TRUE
    enabled(y1_spin) <<- TRUE
    enabled(y2_spin) <<- TRUE
    enabled(zoom_in_but) <<- TRUE
    enabled(zoom_out_but) <<- TRUE
    enabled(u_but) <<- TRUE
    enabled(d_but) <<- TRUE
    enabled(l_but) <<- TRUE
    enabled(r_but) <<- TRUE
    
    
  }
  
})

#max_ceil = floor(max(tic)/1000) * 1000
#min_ceil = ceiling(min(tic)/1000) * 1000
#step_ceil = (max_ceil - min_ceil) / 100
#value_ceil = min_ceil + 50 * step_ceil
#gslider(from = min_ceil, to = max_ceil, by = step_ceil, value = value_ceil, handler = TIC_plot)

#TIC_plot <- function(...){
#   max_tic <<- svalue(max_intensity)

#}

lyt2 [2:3,3:4, anchor = c(0,1)] <- max_intensity <- gslider(from = 1, to = 100, by = 1, value = 50)
max_tic <- svalue(max_intensity)


#max_intensity[] <- seq(min_ceil, max_ceil, step_ceil)
#size(max_intensity) <- c(80,25)
lyt2 [3,1, anchor = c(0,0)] <- view_button <- gbutton ( "View/Start" , cont = lyt2  )
#size(view_button) <- c(100,25)
lyt2 [4,1,anchor =c(-1,0)] <- win2_op_lable <- glabel("Operation :", cont = lyt2)
size(win2_op_lable) <- c(60,20)

op_select_data <- c ( "Segmentation" ,"Horizonal remove", "Vertical remove")

lyt2 [4,2:4] <- op_select <- gcombobox ( c ( "Segmentation" ,"Horizonal remove", "Vertical remove"), cont = lyt2 )
size(op_select) <- c(80,20)


lyt2 [5,1:2,anchor = c(-1,0)] <- win2_seg_label1 <- glabel("# of segments:", container = lyt2)
size(win2_seg_label1) <- c(40,20)
#lyt2 [5,1,anchor = c(-1,0)] <- "# of seg.:"
lyt2 [5,3] <- seg_numb <- gspinbutton(from = 1, to = 10, value = 1,cont = lyt2)
size(seg_numb) <- c(35,20)

#lyt2 [5,3,anchor = c(-1,0)] <- "Selected:"
seg_list <- c("Seg1")
lyt2 [5,4] <- sel_seg <- gcombobox ( seg_list, cont = lyt2 , editable = FALSE )
size(sel_seg) <- c(55,20)
lyt2 [6,1:4] <- crop_but <- gbutton("Select seg. borders by mouse",cont = lyt2)

lyt2 [7,1:3, anchor = c(-1,0) ] <- win2_zoom <- glabel("Zoom & scroll step:", container = lyt2) 

lyt2 [7,4, anchor = c(-1,0) ] <- zoom_spin <- gspinbutton(from= 1, to = 100, by = 1, value = 2 , cont = lyt2)
size(zoom_spin) <- c(15,20)


lyt2 [8,1:4 ] <- lyt2_3 <- glayout(cont = lyt2)

lyt2_3 [1,1:11 ] <- zoom_in_but <- gbutton("Zoom in",cont = lyt2_3)
lyt2_3 [1,12:22  ] <- zoom_out_but <- gbutton("Zoom out",cont = lyt2_3)

#lyt2 [9,1:4 ] <- lyt2_2 <- glayout(cont = lyt2)
lyt2_3 [2,1:6 ] <- u_but <- gbutton("Up",cont = lyt2_3)
lyt2_3 [2,6:11 ] <- d_but <- gbutton("Down",cont = lyt2_3)
lyt2_3 [2,12:17 ] <- l_but <- gbutton("Left",cont = lyt2_3)
lyt2_3 [2,17:22 ] <- r_but <- gbutton("Right",cont = lyt2_3)


lyt2 [9,1:4 ] <- lyt2_4 <- glayout(cont = lyt2)
#lyt2 [10,4 ] <- view_but_2 <- gbutton("View", cont = lyt2)
lyt2_4 [1,1,anchor = c(-1,0)] <- win2_x_lable <- glabel("X :")
lyt2_4 [1,2] <- x1_spin <- gspinbutton(from = 1 , to = 1, by = 1 , value = 1,cont = lyt2_4)
lyt2_4 [1,3,anchor = c(0,0)] <- to_lablx <- glabel("to")
lyt2_4 [1,4] <- x2_spin <- gspinbutton(from = 1 , to = 1, by = 1 , value = 1,cont = lyt2_4)

lyt2_4 [2,1,anchor = c(-1,0)] <- win2_y_lable <- glabel("Y :")
lyt2_4 [2,2] <- y1_spin <- gspinbutton(from = 1 , to = 1, by = 1 , value = 1,cont = lyt2_4)
lyt2_4 [2,3,anchor = c(0,0)] <- to_lably <- glabel("to")
lyt2_4 [2,4] <- y2_spin <- gspinbutton(from = 1 , to = 1, by = 1 , value = 1,cont = lyt2_4)
lyt2_4 [1:2,5:11] <- view_but2 <- gbutton("view")
size(view_but2) <- c(45,40)

lyt2 [10,1:4, anchor = c(0,0)] <- win2_lable1 <- glabel( "Select data to impose segmentation:", cont = lyt2)
font(win2_lable1) <- list( weight = "bold", color = "blue")
win2_tab <- s_tab
lyt2 [11:120,1:4, anchor = c(0,0)] <- win2_dat_tab <- gtable(win2_tab , multiple = TRUE)
lyt2 [121:130,1:4, anchor = c(0,0)] <- f_seg_but <- gbutton( "Impose", cont = lyt2 )
lyt2 [131:140,1:2, anchor = c(0,0)] <- prep_back_but <- gbutton( "Back", cont = lyt2 )
lyt2 [131:140,3:4, anchor = c(0,0)] <- prep_next_but <- gbutton( "Next", cont = lyt2 )

addHandlerChanged(prep_back_but , function(h,...){
  dev.off()  
  visible(window2) <<- FALSE
  
  visible(window) <<- TRUE
  plot(0,type = "n",xlab = "1st column time points", ylab = "2nd column time points", main = "Total Ion Chromatogram")
  
})


size(f_seg_but) <- c(10,40)
font(f_seg_but) <- list(size = 9,  weight = "bold", color = "blue")
lyt2 [1:140,7:320]<- TIC_graph <-ggraphics(cont = lyt2 )
#plot(seq(1,100,2),type = "n",xlab = "1st column time points", ylab = "2nd column time points", main = "Total Ion Chromatogram")




addHandlerChanged ( view_button , handler = function ( h ,... ) {
  
  max_tic <<- svalue(max_intensity)
  print(max_tic)
  
  
  if(visible(window2)){
    dat_f_tic <<- svalue (win2_data_combo)
    sec_col <<- second_col_hash[[dat_f_tic]]
    real_dat_tic <<- get(dat_f_tic)
    sel_dim <<- dim(real_dat_tic)
    fir_col <<- first_col_hash[[ dat_f_tic]]
    
    if (svalue(win_type) == "GC*GC-MS"){
      data_row_sum <<- rowSums(real_dat_tic)
      TIC_data <<- matrix(data_row_sum, nrow =  sec_col , ncol = fir_col)
      tic <<- TIC_data  
    }
    else
      tic <<- t(real_dat_tic)
    
    
    
    
    
    
    for (i in c(1:sec_col)){
      for (j in c(1:fir_col)){
        if (tic[i,j] > max_tic){
          tic[i,j] <- max_tic
        }
      }
    }
    tic <<- tic
    
    layout(t(1:2),widths=c(1,10))
    x=0:10
    par(mar=c(4,1.5,2,2.5))
    min_TIC <<- round(min(tic))
    maximum_TIC <<- round(max(tic))
    step_TIC <<- (maximum_TIC - min_TIC)/200
    cb_range <<- seq(min_TIC, maximum_TIC , step_TIC)
    image(y=cb_range,z=t(cb_range), col = tim.colors(200), axes=FALSE, main="Intensity", cex.main=.8)
    axis(2,cex.axis=0.8,mgp=c(0,.5,0))
    
    
    par(mar=c(5,3.8,1.5,2))
    image(c(1:fir_col),c(1:sec_col),t(tic),col = tim.colors(200),xlab = "1st column time points",ylab = "2nd column time points", axes = TRUE,main = "Total Ion Chromatogram")
    grid()
    
  }
  
  enabled(op_select) <<- TRUE
  enabled(sel_seg) <<- TRUE
  enabled(crop_but) <<- TRUE
  enabled(seg_numb) <<- TRUE
  
})

op_value <<- svalue(op_select)

addHandlerChanged(op_select, function(h,...){
  
  op_value <<- svalue(op_select)
  enabled(op_select) <<- FALSE
  if (op_value == "Horizonal remove"){
    svalue(crop_but) <<- "Select coords by mouse"
    enabled(l_but) <- FALSE
    enabled(r_but) <- FALSE
    enabled(u_but) <- TRUE
    enabled(d_but) <- TRUE
    #svalue(zoom_in_but) <- "Widen"
    #svalue(zoom_out_but) <- "Narrow"
    svalue(win2_zoom) <- "Height change step:"
    
    enabled(win2_seg_label1) <- FALSE
    enabled(seg_numb) <- FALSE
    enabled(sel_seg) <- FALSE
    enabled(win2_x_lable) <- FALSE
    enabled(to_lablx) <- FALSE
    enabled(x1_spin) <- FALSE
    enabled(x2_spin) <- FALSE
    enabled(win2_y_lable) <- TRUE
    enabled(to_lably) <- TRUE
    enabled(y1_spin) <- TRUE
    enabled(y2_spin) <- TRUE
    #svalue(f_seg_but) <- "Operate"
    font(f_seg_but) <- list(size = 10,  weight = "bold", color = "blue")
    svalue(win2_lable1) <- "Select data to impose removing:"
    font(win2_lable1) <- list( weight = "bold", color = "blue")
    enabled(crop_but) <<- TRUE
    
  }
  
  if (op_value == "Vertical remove"){
    svalue(crop_but) <<- "Select coords by mouse"
    
    enabled(l_but) <- TRUE
    enabled(r_but) <- TRUE
    enabled(u_but) <- FALSE
    enabled(d_but) <- FALSE
    #svalue(zoom_in_but) <- "Widen"
    #svalue(zoom_out_but) <- "Narrow"
    svalue(win2_zoom) <- "Width change step:"
    enabled(crop_but) <<- TRUE
    
    enabled(win2_seg_label1) <- FALSE
    enabled(seg_numb) <- FALSE
    enabled(sel_seg) <- FALSE
    enabled(win2_y_lable) <- FALSE
    enabled(to_lably) <- FALSE
    enabled(y1_spin) <- FALSE
    enabled(y2_spin) <- FALSE
    enabled(win2_x_lable) <- TRUE
    enabled(to_lablx) <- TRUE
    enabled(x1_spin) <- TRUE
    enabled(x2_spin) <- TRUE
    #svalue(f_seg_but) <- "Remove selected coords"
    font(f_seg_but) <- list(size = 10,  weight = "bold", color = "blue")
    svalue(win2_lable1) <- "Select data to impose removing:"
    font(win2_lable1) <- list( weight = "bold", color = "blue")
    
  }
  
  
  if (op_value == "Segmentation"){
    enabled(sel_seg) <<- TRUE
    enabled(seg_numb) <<- TRUE
    svalue(crop_but) <<- "Select seg. borders by mouse"
    enabled(crop_but) <<- TRUE
    enabled(l_but) <- TRUE
    enabled(r_but) <- TRUE
    enabled(u_but) <- TRUE
    enabled(d_but) <- TRUE
    #svalue(zoom_in_but) <- "Zoom in"
    #svalue(zoom_out_but) <- "Zoom out"
    svalue(win2_zoom) <- "Zoom & scroll step::"
    
    enabled(win2_seg_label1) <- TRUE
    enabled(seg_numb) <- TRUE
    enabled(sel_seg) <- TRUE
    enabled(win2_y_lable) <- TRUE
    enabled(to_lably) <- TRUE
    enabled(y1_spin) <- TRUE
    enabled(y2_spin) <- TRUE
    enabled(win2_x_lable) <- TRUE
    enabled(to_lablx) <- TRUE
    enabled(x1_spin) <- TRUE
    enabled(x2_spin) <- TRUE
    #svalue(f_seg_but) <- "Segment"
    font(f_seg_but) <- list(size = 9,  weight = "bold", color = "blue")
    svalue(win2_lable1) <- "Select data to impose segmentation:"
    font(win2_lable1) <- list( weight = "bold", color = "blue")
    
  }
})



addHandlerChanged(seg_numb, function(h,...){
  seg_list2 <<- c()
  number_seg <<- svalue( seg_numb)
  
  for (i in c(1:number_seg)){
    seg_list2 <<- append(seg_list2, paste("Seg",i,sep = ""))
  }
  #svalue(sel_seg)
  print(seg_list2)
  sel_seg[] <<- seg_list2
  #update(sel_seg)
  
})



addHandlerChanged ( crop_but , handler = function ( h ,... ) {
  op_value <<- svalue(op_select)
  
  if(op_value == "Segmentation"){
    m1 <- locator(n = 1, type = "n")
    x1 <<- round(m1[[1]])
    y1 <<- round(m1[[2]])
    
    if(x1 < 1)
      x1 <<- 1
    if (y1 > sec_col)
      y1 <<- sec_col
    
    
    svalue(x1_spin) <<- x1
    svalue(y2_spin) <<- y1
    
    x1_mat <- matrix(c(x1,x1,0,y1) , nrow = 2, ncol = 2)
    y1_mat <- matrix(c(x1,fir_col,y1,y1), nrow = 2, ncol = 2)
    lines(x1_mat, col = "red" , lwd = 2)
    lines(y1_mat, col = "red" , lwd = 2)
    
    m2 <- locator(n=1 , type = "n")
    
    
    if((round(m2[[1]]) > x1)&(round(m2[[2]]) < y1)){
      x2 <<- round(m2[[1]])
      y2 <<- round(m2[[2]])
      
      if (x2 > fir_col)
        x2 <<- fir_col
      if( y2 < 1)
        y2 <<- 1
      
      
      svalue(x2_spin) <<- x2
      svalue(y1_spin) <<- y2
      
      x2_mat <- matrix(c(x2,x2,y2,sec_col) , nrow = 2, ncol = 2)
      y2_mat <- matrix(c(0,x2,y2,y2), nrow = 2, ncol = 2)
      lines(x2_mat, col = "red" , lwd = 2)
      lines(y2_mat, col = "red" , lwd = 2)
    }
    else{
      gmessage ( "Second vertex of rectangle is not acceptable" , title = "segmentation error" )
      x2 <<- x2
      y2 <<- y2
    }
    
    for (i in c(1:sec_col)){
      for (j in c(1:fir_col)){
        if (tic[i,j] > max_tic){
          tic[i,j] <- max_tic
        }
      }
    }
    
    
    
    t_tic <- t(tic)
    new_tic <- t_tic[x1:x2 , y2:y1]
    
    
    layout(t(1:2),widths=c(1,10))
    x=0:10
    par(mar=c(4,1.5,2,2.5))
    min_TIC <- round(min(tic))
    max_TIC <- round(max(tic))
    step_TIC <- (max_TIC - min_TIC)/200
    cb_range <- seq(min_TIC, max_TIC , step_TIC)
    image(y=cb_range,z=t(cb_range), col = tim.colors(200), axes=FALSE, main="Intensity", cex.main=.8)
    axis(2,cex.axis=0.8,mgp=c(0,.5,0))
    
    
    par(mar=c(5,3.8,1.5,2))
    image(c(x1:x2),c(y2:y1),new_tic,col = tim.colors(200),xlab = "1st column time points",ylab = "2nd column time points", main = "Total Ion Chromatogram",axes = TRUE)
    grid()
    
    
  }
  
  if(op_value == "Horizonal remove"){
    
    ini_y1 <<- svalue(y1_spin)
    ini_y2 <<- svalue(y2_spin)
    
    x1 <<- svalue(x1_spin)
    x2 <<- svalue(x2_spin)
    m1 <- locator(n = 1, type = "n")
    y1 <<- round(m1[[2]])
    
    
    
    if (y1 < 1)
      y1 <<- 1
    
    y1_mat <- matrix(c(x1,x2,y1,y1), nrow = 2, ncol = 2)
    lines(y1_mat, col = "red" , lwd = 2)
    
    m2 <- locator(n = 1, type = "n")
    y2 <<- round(m2[[2]])
    if (y2 < 1)
      y2 <<- 1
    
    y2_mat <- matrix(c(x1,x2,y2,y2), nrow = 2, ncol = 2)
    lines(y2_mat, col = "red" , lwd = 2)
    
    #ini_y1 <<- svalue()
    
    
    if(y2 <= y1){
      y2 <- y2
    }  
    else {
      y2_fix <- y2
      y2 <<- y1
      y1 <<- y2_fix
    }
    
    
    
    svalue(y2_spin) <<- y1
    svalue(y1_spin) <<- y2
    
    for (i in c(1:sec_col)){
      for (j in c(1:fir_col)){
        if (tic[i,j] > max_tic){
          tic[i,j] <- max_tic
        }
      }
    }
    
    
    t_tic <- t(tic)
    
    print(x1)
    print(x2)
    print(y1)
    print(y2)
    
    new_tic <- t_tic[x1:x2 , y2:y1]
    
    
    layout(t(1:2),widths=c(1,10))
    x=0:10
    par(mar=c(4,1.5,2,2.5))
    min_TIC <- round(min(tic))
    max_TIC <- round(max(tic))
    step_TIC <- (max_TIC - min_TIC)/200
    cb_range <- seq(min_TIC, max_TIC , step_TIC)
    image(y=cb_range,z=t(cb_range), col = tim.colors(200), axes=FALSE, main="Intensity", cex.main=.8)
    axis(2,cex.axis=0.8,mgp=c(0,.5,0))
    
    
    par(mar=c(5,3.8,1.5,2))
    image(c(x1:x2),c(y2:y1),new_tic,col = tim.colors(200),xlab = "1st column time points",ylab = "2nd column time points", main = "Total Ion Chromatogram",axes = TRUE)
    grid()
    
    
    
  }
  
  if(op_value == "Vertical remove"){
    ini_x1 <<- svalue(x1_spin)
    ini_x2 <<- svalue(x2_spin)
    
    y1 <<- svalue(y2_spin)
    y2 <<- svalue(y1_spin)
    
    
    m1 <- locator(n = 1, type = "n")
    x1 <<- round(m1[[1]])
    
    if (x1 < 1)
      x1 <<- 1
    
    
    x1_mat <- matrix(c(x1,x1,y2,y1) , nrow = 2, ncol = 2)
    lines(x1_mat, col = "red" , lwd = 2)
    
    m2 <- locator(n = 1, type = "n")
    x2 <<- round(m2[[1]])
    
    if (x2 > ini_x2)
      x2 <<- ini_x2
    
    x2_mat <- matrix(c(x2,x2,y2,y1) , nrow = 2, ncol = 2)
    lines(x1_mat, col = "red" , lwd = 2)
    if(x1 < x2)
      x1 <- x1
    else{
      x1_fix <<- x1
      x1 <- x2
      x2 <<- x1_fix
    }
    svalue(x2_spin) <<- x2
    svalue(x1_spin) <<- x1
    
    
    for (i in c(1:sec_col)){
      for (j in c(1:fir_col)){
        if (tic[i,j] > max_tic){
          tic[i,j] <- max_tic
        }
      }
    }
    
    
    t_tic <- t(tic)
    new_tic <- t_tic[x1:x2 , y2:y1]
    
    
    layout(t(1:2),widths=c(1,10))
    x=0:10
    par(mar=c(4,1.5,2,2.5))
    min_TIC <- round(min(tic))
    max_TIC <- round(max(tic))
    step_TIC <- (max_TIC - min_TIC)/200
    cb_range <- seq(min_TIC, max_TIC , step_TIC)
    image(y=cb_range,z=t(cb_range), col = tim.colors(200), axes=FALSE, main="Intensity", cex.main=.8)
    axis(2,cex.axis=0.8,mgp=c(0,.5,0))
    
    
    par(mar=c(5,3.8,1.5,2))
    image(c(x1:x2),c(y2:y1),new_tic,col = tim.colors(200),xlab = "1st column time points",ylab = "2nd column time points", main = "Total Ion Chromatogram",axes = TRUE)
    grid()
    
    
  }
  
  
})

addHandlerChanged(view_but2 , function(h,...){
  x1 <<- svalue(x1_spin) 
  y1 <<- svalue(y2_spin) 
  x2 <<- svalue(x2_spin) 
  y2 <<- svalue(y1_spin) 
  
  for (i in c(1:sec_col)){
    for (j in c(1:fir_col)){
      if (tic[i,j] > max_tic){
        tic[i,j] <- max_tic
      }
    }
  }
  
  t_tic <- t(tic)
  new_tic <- t_tic[x1:x2 , y2:y1]
  
  
  layout(t(1:2),widths=c(1,10))
  x=0:10
  par(mar=c(4,1.5,2,2.5))
  min_TIC <- round(min(tic))
  max_TIC <- round(max(tic))
  step_TIC <- (max_TIC - min_TIC)/200
  cb_range <- seq(min_TIC, max_TIC , step_TIC)
  image(y=cb_range,z=t(cb_range), col = tim.colors(200), axes=FALSE, main="Intensity", cex.main=.8)
  axis(2,cex.axis=0.8,mgp=c(0,.5,0))
  
  
  par(mar=c(5,3.8,1.5,2))
  image(c(x1:x2),c(y2:y1),new_tic,col = tim.colors(200),xlab = "1st column time points",ylab = "2nd column time points", main = "Total Ion Chromatogram",axes = TRUE)
  grid()
  
})  



addHandlerChanged(zoom_in_but , function(h,...){
  
  for (i in c(1:sec_col)){
    for (j in c(1:fir_col)){
      if (tic[i,j] > max_tic){
        tic[i,j] <- max_tic
      }
    }
  }
  zoom_value <- svalue(zoom_spin)
  
  if(op_value == "Segmentation"){
    if (((x1 + zoom_value) < (x2 - zoom_value))&((y2 + zoom_value)<(y1 - zoom_value))){
      x1 <<- x1 + zoom_value
      x2 <<- x2 - zoom_value
      y2 <<- y2 + zoom_value
      y1 <<- y1 - zoom_value
      svalue(x1_spin) <<- x1
      svalue(y2_spin) <<- y1
      svalue(x2_spin) <<- x2
      svalue(y1_spin) <<- y2
      
      t_tic <- t(tic)
      new_tic <- t_tic[x1:x2 , y2:y1]
      layout(t(1:2),widths=c(1,10))
      x=0:10
      par(mar=c(4,1.5,2,2.5))
      min_TIC <- round(min(tic))
      max_TIC <- round(max(tic))
      step_TIC <- (max_TIC - min_TIC)/200
      cb_range <- seq(min_TIC, max_TIC , step_TIC)
      image(y=cb_range,z=t(cb_range), col = tim.colors(200), axes=FALSE, main="Intensity", cex.main=.8)
      axis(2,cex.axis=0.8,mgp=c(0,.5,0))
      
      
      par(mar=c(5,3.8,1.5,2))
      image(c(x1:x2),c(y2:y1),new_tic,col = tim.colors(200),xlab = "1st column time points",ylab = "2nd column time points", main = "Total Ion Chromatogram",axes = TRUE)
      grid()
    }
    else
      gmessage ( "Zooming in with selected step is not possible" , title = "zooming error" )
  }
  else{
    
    if(op_value == "Horizonal remove"){
      if ((y2 - zoom_value) > 0)
        y2 <<- y2 - zoom_value
      else
        y2 <<- 1
      if((y1 + zoom_value) <= nrow(tic))
        y1 <<- y1 + zoom_value
      else
        y1 <<- nrow(tic)
    }
    
    if (op_value == "Vertical remove"){
      if((x1 - zoom_value) > 0)
        x1 <<- x1 - zoom_value
      else
        x1 <<- 1
      if((x2 + zoom_value) <= ncol(tic))
        x2 <<- x2 + zoom_value
      else
        x2 <<- ncol(tic)
    }
    
    svalue(x1_spin) <<- x1
    svalue(y2_spin) <<- y1
    svalue(x2_spin) <<- x2
    svalue(y1_spin) <<- y2
    
    t_tic <- t(tic)
    new_tic <- t_tic[x1:x2 , y2:y1]
    layout(t(1:2),widths=c(1,10))
    x=0:10
    par(mar=c(4,1.5,2,2.5))
    min_TIC <- round(min(tic))
    max_TIC <- round(max(tic))
    step_TIC <- (max_TIC - min_TIC)/200
    cb_range <- seq(min_TIC, max_TIC , step_TIC)
    image(y=cb_range,z=t(cb_range), col = tim.colors(200), axes=FALSE, main="Intensity", cex.main=.8)
    axis(2,cex.axis=0.8,mgp=c(0,.5,0))
    
    
    par(mar=c(5,3.8,1.5,2))
    image(c(x1:x2),c(y2:y1),new_tic,col = tim.colors(200),xlab = "1st column time points",ylab = "2nd column time points", main = "Total Ion Chromatogram",axes = TRUE)
    grid()
    
  }
})


addHandlerChanged(zoom_out_but , function(h,...){
  for (i in c(1:sec_col)){
    for (j in c(1:fir_col)){
      if (tic[i,j] > max_tic){
        tic[i,j] <- max_tic
      }
    }
  }
  zoom_value <- svalue(zoom_spin)
  
  if(op_value == "Segmentation"){
    if (((x1 - zoom_value) >0 )&((x2 + zoom_value)<= ncol(tic))&((y2 - zoom_value)>0)&((y1 + zoom_value)<= nrow(tic))){
      x1 <<- x1 - zoom_value
      x2 <<- x2 + zoom_value
      y2 <<- y2 - zoom_value
      y1 <<- y1 + zoom_value
      svalue(x1_spin) <<- x1
      svalue(y2_spin) <<- y1
      svalue(x2_spin) <<- x2
      svalue(y1_spin) <<- y2
      
      t_tic <- t(tic)
      new_tic <- t_tic[x1:x2 , y2:y1]
      layout(t(1:2),widths=c(1,10))
      x=0:10
      par(mar=c(4,1.5,2,2.5))
      min_TIC <- round(min(tic))
      max_TIC <- round(max(tic))
      step_TIC <- (max_TIC - min_TIC)/200
      cb_range <- seq(min_TIC, max_TIC , step_TIC)
      image(y=cb_range,z=t(cb_range), col = tim.colors(200), axes=FALSE, main="Intensity", cex.main=.8)
      axis(2,cex.axis=0.8,mgp=c(0,.5,0))
      
      
      par(mar=c(5,3.8,1.5,2))
      image(c(x1:x2),c(y2:y1),new_tic,col = tim.colors(200),xlab = "1st column time points",ylab = "2nd column time points", main = "Total Ion Chromatogram",axes = TRUE)
      grid()
    }
    else
      gmessage ( "Zooming out with selected step is not possible" , title = "zooming error" )
  }
  else{
    
    if(op_value == "Horizonal remove"){
      
      if ((y2 + zoom_value) < (y1 - zoom_value)){
        y2 <<- y2 + zoom_value
        y1 <<- y1 - zoom_value  
      }
      else
        gmessage ( "Select area can't be narrower" , title = "removing error" )
      
      
      
    }
    if(op_value == "Vertical remove"){
      
      if ((x1 + zoom_value) < (x2 - zoom_value)){
        x1 <<- x1 + zoom_value
        x2 <<- x2 - zoom_value  
      }
      else
        gmessage ( "Select area can't be narrower" , title = "removing error" )
      
      
      
    }
    
    
    svalue(x1_spin) <<- x1
    svalue(y2_spin) <<- y1
    svalue(x2_spin) <<- x2
    svalue(y1_spin) <<- y2
    
    t_tic <- t(tic)
    new_tic <- t_tic[x1:x2 , y2:y1]
    layout(t(1:2),widths=c(1,10))
    x=0:10
    par(mar=c(4,1.5,2,2.5))
    min_TIC <- round(min(tic))
    max_TIC <- round(max(tic))
    step_TIC <- (max_TIC - min_TIC)/200
    cb_range <- seq(min_TIC, max_TIC , step_TIC)
    image(y=cb_range,z=t(cb_range), col = tim.colors(200), axes=FALSE, main="Intensity", cex.main=.8)
    axis(2,cex.axis=0.8,mgp=c(0,.5,0))
    
    
    par(mar=c(5,3.8,1.5,2))
    image(c(x1:x2),c(y2:y1),new_tic,col = tim.colors(200),xlab = "1st column time points",ylab = "2nd column time points", main = "Total Ion Chromatogram",axes = TRUE)
    grid()
    
  }
  
  
})

addHandlerChanged( u_but, function(h,...){
  for (i in c(1:sec_col)){
    for (j in c(1:fir_col)){
      if (tic[i,j] > max_tic){
        tic[i,j] <- max_tic
      }
    }
  }
  zoom_value <- svalue(zoom_spin)
  if ((y1 + zoom_value)<= nrow(tic)){
    #x1 <<- x1 - zoom_value
    #x2 <<- x2 + zoom_value
    y2 <<- y2 + zoom_value
    y1 <<- y1 + zoom_value
    svalue(x1_spin) <<- x1
    svalue(y2_spin) <<- y1
    svalue(x2_spin) <<- x2
    svalue(y1_spin) <<- y2
    
    t_tic <- t(tic)
    new_tic <- t_tic[x1:x2 , y2:y1]
    layout(t(1:2),widths=c(1,10))
    x=0:10
    par(mar=c(4,1.5,2,2.5))
    min_TIC <- round(min(tic))
    max_TIC <- round(max(tic))
    step_TIC <- (max_TIC - min_TIC)/200
    cb_range <- seq(min_TIC, max_TIC , step_TIC)
    image(y=cb_range,z=t(cb_range), col = tim.colors(200), axes=FALSE, main="Intensity", cex.main=.8)
    axis(2,cex.axis=0.8,mgp=c(0,.5,0))
    
    
    par(mar=c(5,3.8,1.5,2))
    image(c(x1:x2),c(y2:y1),new_tic,col = tim.colors(200),xlab = "1st column time points",ylab = "2nd column time points", main = "Total Ion Chromatogram",axes = TRUE)
    grid()
  }
  else
    gmessage ( "movement with selected step is not possible" , title = "moving error" )
  
  
  
})

addHandlerChanged( d_but, function(h,...){
  for (i in c(1:sec_col)){
    for (j in c(1:fir_col)){
      if (tic[i,j] > max_tic){
        tic[i,j] <- max_tic
      }
    }
  }
  zoom_value <- svalue(zoom_spin)
  if ((y2 - zoom_value)> 0){
    #x1 <<- x1 - zoom_value
    #x2 <<- x2 + zoom_value
    y2 <<- y2 - zoom_value
    y1 <<- y1 - zoom_value
    svalue(x1_spin) <<- x1
    svalue(y2_spin) <<- y1
    svalue(x2_spin) <<- x2
    svalue(y1_spin) <<- y2
    
    t_tic <- t(tic)
    new_tic <- t_tic[x1:x2 , y2:y1]
    layout(t(1:2),widths=c(1,10))
    x=0:10
    par(mar=c(4,1.5,2,2.5))
    min_TIC <- round(min(tic))
    max_TIC <- round(max(tic))
    step_TIC <- (max_TIC - min_TIC)/200
    cb_range <- seq(min_TIC, max_TIC , step_TIC)
    image(y=cb_range,z=t(cb_range), col = tim.colors(200), axes=FALSE, main="Intensity", cex.main=.8)
    axis(2,cex.axis=0.8,mgp=c(0,.5,0))
    
    
    par(mar=c(5,3.8,1.5,2))
    image(c(x1:x2),c(y2:y1),new_tic,col = tim.colors(200),xlab = "1st column time points",ylab = "2nd column time points", main = "Total Ion Chromatogram",axes = TRUE)
    grid()
  }
  else
    gmessage ( "movement with selected step is not possible" , title = "moving error" )
  
  
  
})

addHandlerChanged( r_but, function(h,...){
  for (i in c(1:sec_col)){
    for (j in c(1:fir_col)){
      if (tic[i,j] > max_tic){
        tic[i,j] <- max_tic
      }
    }
  }
  zoom_value <- svalue(zoom_spin)
  if ((x2 + zoom_value)<= ncol(tic)){
    x1 <<- x1 + zoom_value
    x2 <<- x2 + zoom_value
    #y2 <<- y2 + zoom_value
    #y1 <<- y1 + zoom_value
    svalue(x1_spin) <<- x1
    svalue(y2_spin) <<- y1
    svalue(x2_spin) <<- x2
    svalue(y1_spin) <<- y2
    
    t_tic <- t(tic)
    new_tic <- t_tic[x1:x2 , y2:y1]
    layout(t(1:2),widths=c(1,10))
    x=0:10
    par(mar=c(4,1.5,2,2.5))
    min_TIC <- round(min(tic))
    max_TIC <- round(max(tic))
    step_TIC <- (max_TIC - min_TIC)/200
    cb_range <- seq(min_TIC, max_TIC , step_TIC)
    image(y=cb_range,z=t(cb_range), col = tim.colors(200), axes=FALSE, main="Intensity", cex.main=.8)
    axis(2,cex.axis=0.8,mgp=c(0,.5,0))
    
    
    par(mar=c(5,3.8,1.5,2))
    image(c(x1:x2),c(y2:y1),new_tic,col = tim.colors(200),xlab = "1st column time points",ylab = "2nd column time points", main = "Total Ion Chromatogram",axes = TRUE)
    grid()
  }
  else
    gmessage ( "movement with selected step is not possible" , title = "moving error" )
  
  
  
})

addHandlerChanged( l_but, function(h,...){
  for (i in c(1:sec_col)){
    for (j in c(1:fir_col)){
      if (tic[i,j] > max_tic){
        tic[i,j] <- max_tic
      }
    }
  }
  zoom_value <- svalue(zoom_spin)
  if ((x1 - zoom_value) >= 1){
    x1 <<- x1 - zoom_value
    x2 <<- x2 - zoom_value
    #y2 <<- y2 + zoom_value
    #y1 <<- y1 + zoom_value
    svalue(x1_spin) <<- x1
    svalue(y2_spin) <<- y1
    svalue(x2_spin) <<- x2
    svalue(y1_spin) <<- y2
    
    t_tic <- t(tic)
    new_tic <- t_tic[x1:x2 , y2:y1]
    layout(t(1:2),widths=c(1,10))
    x=0:10
    par(mar=c(4,1.5,2,2.5))
    min_TIC <- round(min(tic))
    max_TIC <- round(max(tic))
    step_TIC <- (max_TIC - min_TIC)/200
    cb_range <- seq(min_TIC, max_TIC , step_TIC)
    image(y=cb_range,z=t(cb_range), col = tim.colors(200), axes=FALSE, main="Intensity", cex.main=.8)
    axis(2,cex.axis=0.8,mgp=c(0,.5,0))
    
    
    par(mar=c(5,3.8,1.5,2))
    image(c(x1:x2),c(y2:y1),new_tic,col = tim.colors(200),xlab = "1st column time points",ylab = "2nd column time points", main = "Total Ion Chromatogram",axes = TRUE)
    grid()
  }
  else
    gmessage ( "movement with selected step is not possible" , title = "moving error" )
  
  
  
})


addHandlerClicked (win2_dat_tab , function(h,...){
  
  #  print(svalue(win2_dat_tab))
})

#addHandlerClicked(sel_seg, function(h,...){
#  print("Yeeah")
#})


addHandlerChanged ( f_seg_but , handler = function (h ,...){
  op_value <<- svalue(op_select)
  #  op_value2 <<- svalue(op_select)
  
  
  name_list <- c()
  
  if(op_value == "Segmentation"){
    name_list <- c()
    #svalue(f_seg_but) <<- "Wait"
    enabled(f_seg_but) <<- FALSE
    
    seg_x1 <- svalue(x1_spin)
    seg_x2 <- svalue(x2_spin)
    seg_y1 <- svalue(y1_spin)
    seg_y2 <- svalue(y2_spin)
    seg_order <- svalue(sel_seg)
    delta_y <- seg_y2 - seg_y1 + 1
    delta_x <- seg_x2 - seg_x1 + 1
    
    #  seg_func <- function(in_mat ) {
    
    
    # cube_data <- array(in_mat , dim = c(sec_col , fir_col ,dim(in_mat)[2]))
    #  sel_data <- array(in_mat , dim = c(sec_col , fir_col ,dim(in_mat)[2]))[c(seg_y1:seg_y2), c(seg_x1:seg_x2) ,]
    #   f_segment_dat <- array(array(in_mat , dim = c(sec_col , fir_col ,dim(in_mat)[2]))[c(seg_y1:seg_y2), c(seg_x1:seg_x2) ,] , c(delta_y * delta_x , dim(in_mat)[2]))
    #    
    #     return(f_segment_dat)
    #out_func
    #    }
    
    
    data_win2_dat_tab <<- svalue(win2_dat_tab)
    
    t1 <- proc.time()
    for (i in data_win2_dat_tab){
      #cube_data <- array(get(i) , dim = c(sec_col , fir_col ,dim(get(i))[2]))
      
      #sel_data <- cube_data[c(seg_y1:seg_y2), c(seg_x1:seg_x2) ,]
      #sel_data <- array(get(i) , dim = c(sec_col , fir_col ,dim(get(i))[2]))[c(seg_y1:seg_y2), c(seg_x1:seg_x2) ,]
      
      #f_segment_dat <- array(array(get(i) , dim = c(sec_col , fir_col ,dim(get(i))[2]))[c(seg_y1:seg_y2), c(seg_x1:seg_x2) ,] , c(delta_y * delta_x , dim(get(i))[2]))
      
      #f_segment_dat <- matrix(array(get(i) , dim = c(sec_col , fir_col ,dim(get(i))[2]))[c(seg_y1:seg_y2), c(seg_x1:seg_x2) ,] , nrow = delta_y * delta_x, ncol = dim(get(i))[2]   )
      
      #
      segmented_mat <<- c()
      
      for (j in c(seg_x1 : seg_x2)){
        segmented_mat <<- append(segmented_mat , c((seg_y1+400*(j-1)):(seg_y2+400*(j-1))))
      }
      
      #for (j in c(seg_x1 : seg_x2)){
      #  segmented_mat <<- c(segmented_mat , c((seg_y1+619*(j-1)):(seg_y2+619*(j-1))))
      #}
      
      
      assign(paste(seg_order,i, sep = "_"), get(i)[segmented_mat,]  , envir = .GlobalEnv)
      
      name_list[length(name_list) + 1 ] <- paste(seg_order,i, sep = "_")
      second_col_hash[[paste(seg_order,i, sep = "_")]] <<- delta_y
      first_col_hash[[paste(seg_order,i, sep = "_")]] <<- delta_x
      
      s_tab[length(s_tab) + 1] <<- paste(seg_order,i, sep = "_")
      s_tab<<-s_tab[-which(s_tab==i)]
      assign(i , NULL, envir = .GlobalEnv)
      win2_dat_tab[] <<- s_tab
      win2_data_combo[] <<- s_tab
      
    }
    t2 <- proc.time()
    print(t2 -t1)
    
    
    
    #res_list <- lapply(lapply(data_win2_dat_tab , get)  , seg_func)
    
    
    #lapply(seq_along(name_list), 
    #       function(x) {
    #         assign(name_list[x], res_list[[x]] , envir=.GlobalEnv)
    #       })
    
    
    svalue(win2_data_combo) <<- s_tab[1]
    
    enabled(f_seg_but) <<- TRUE
    #svalue(f_seg_but) <<- "Segment to selected coord"
    font(f_seg_but) <<- list(size = 9,  weight = "bold", color = "blue")
    
    
  }
  
  if(op_value == "Horizonal remove"){
    print("yeah")
    #svalue(f_seg_but) <<- "Wait"
    enabled(f_seg_but) <<- FALSE
    
    #zz <- array(c(z_t_1 , z_t_2) , dim = c(151,619,700))    
    seg_x1 <- svalue(x1_spin)
    seg_x2 <- svalue(x2_spin)
    seg_y1 <- svalue(y1_spin)
    seg_y2 <- svalue(y2_spin)
    
    
    seg_order <- svalue(sel_seg)
    delta_y <- seg_y2 - seg_y1 + 1
    delta_x <- seg_x2 - seg_x1 + 1
    
    
    data_win2_dat_tab <<- svalue(win2_dat_tab)
    
    for (i in data_win2_dat_tab){
      cube_data <- array(get(i) , dim = c(sec_col , fir_col ,dim(get(i))[2]))
      
      
      select_cord <<- c(ini_y1 : seg_y1)
      if (ini_y1 == seg_y1)
        select_cord <<- c() 
      
      if (seg_y2 != ini_y2)
        select_cord <<- append(select_cord , c(seg_y2 : ini_y2))
      
      
      sel_data <- cube_data[select_cord , c(seg_x1:seg_x2) ,]
      f_segment_dat <- matrix(sel_data , nrow =  dim(sel_data)[1] * dim(sel_data)[2]  , ncol = dim(sel_data)[3]) 
      #     f_sel_data1 <- matrix(sel_data1 , nrow =  dim(sel_data1)[1] * dim(sel_data1)[2] , ncol = dim(sel_data1)[3]) 
      #    f_sel_data2 <- matrix(sel_data2 , nrow =  dim(sel_data2)[1] * dim(sel_data2)[2] , ncol = dim(sel_data2)[3])   
      #    f_segment_dat <- rBind(f_sel_data1 , f_sel_data2)
      
      assign(paste("HRemove",i, sep = "_"), f_segment_dat, envir = .GlobalEnv)
      s_tab[length(s_tab) + 1] <<- paste("HRemove",i, sep = "_")
      win2_dat_tab[] <<- s_tab
      win2_data_combo[] <<- s_tab
      second_col_hash[[paste("HRemove",i, sep = "_")]] <<- dim(sel_data)[1]
      first_col_hash[[paste("HRemove",i, sep = "_")]] <<- dim(sel_data)[2]
      svalue(win2_data_combo) <<- s_tab[1]
    }
    
    enabled(op_select) <<- TRUE
    enabled(f_seg_but) <<- TRUE
    #svalue(f_seg_but) <<- "Remove"
    font(f_seg_but) <<- list(size = 9,  weight = "bold", color = "blue")
    enabled(x1_spin) <<- TRUE
    enabled(x2_spin) <<- TRUE
    enabled(win2_x_lable) <<- TRUE
    enabled(win2_y_lable) <<- TRUE
    
  }
  
  
  if(op_value == "Vertical remove"){
    print("yeah")
    #svalue(f_seg_but) <<- "Wait"
    enabled(f_seg_but) <<- FALSE
    
    #zz <- array(c(z_t_1 , z_t_2) , dim = c(151,619,700))    
    seg_x1 <- svalue(x1_spin)
    seg_x2 <- svalue(x2_spin)
    seg_y1 <- svalue(y1_spin)
    seg_y2 <- svalue(y2_spin)
    
    
    seg_order <- svalue(sel_seg)
    delta_y <- seg_y2 - seg_y1 + 1
    delta_x <- seg_x2 - seg_x1 + 1
    
    
    data_win2_dat_tab <<- svalue(win2_dat_tab)
    
    for (i in data_win2_dat_tab){
      cube_data <- array(get(i) , dim = c(sec_col , fir_col ,dim(get(i))[2]))
      
      
      select_cord <<- c(ini_x1 : seg_x1)
      if (ini_x1 == seg_x1)
        select_cord <<- c() 
      
      if (seg_x2 != ini_x2)
        select_cord <<- append(select_cord , c(seg_x2 : ini_x2))
      
      
      sel_data <- cube_data[c(seg_y1:seg_y2) , select_cord  ,]
      f_segment_dat <- matrix(sel_data , nrow =  dim(sel_data)[1] * dim(sel_data)[2]  , ncol = dim(sel_data)[3]) 
      #     f_sel_data1 <- matrix(sel_data1 , nrow =  dim(sel_data1)[1] * dim(sel_data1)[2] , ncol = dim(sel_data1)[3]) 
      #    f_sel_data2 <- matrix(sel_data2 , nrow =  dim(sel_data2)[1] * dim(sel_data2)[2] , ncol = dim(sel_data2)[3])   
      #    f_segment_dat <- rBind(f_sel_data1 , f_sel_data2)
      
      assign(paste("VRemove",i, sep = "_"), f_segment_dat, envir = .GlobalEnv)
      s_tab[length(s_tab) + 1] <<- paste("VRemove",i, sep = "_")
      win2_dat_tab[] <<- s_tab
      win2_data_combo[] <<- s_tab
      svalue(win2_data_combo) <<- s_tab[1]
      second_col_hash[[paste("VRemove",i, sep = "_")]] <<- dim(sel_data)[1]
      first_col_hash[[paste("VRemove",i, sep = "_")]] <<- dim(sel_data)[2]
    }
    
    enabled(op_select) <<- TRUE
    enabled(f_seg_but) <<- TRUE
    #svalue(f_seg_but) <<- "Remove"
    font(f_seg_but) <<- list(size = 9,  weight = "bold", color = "blue")
    enabled(y1_spin) <<- TRUE
    enabled(y2_spin) <<- TRUE
    
    enabled(win2_x_lable) <<- TRUE
    enabled(win2_y_lable) <<- TRUE
    
  }
  
  
  
  enabled(view_button) <<- FALSE
  enabled(u_but) <<- FALSE
  enabled(d_but) <<- FALSE
  enabled(r_but) <<- FALSE
  enabled(l_but) <<- FALSE
  enabled(view_but2) <<- FALSE
  enabled(zoom_in_but) <<- FALSE
  enabled(zoom_out_but) <<- FALSE
  enabled(zoom_spin) <<- FALSE
  
  enabled(x1_spin) <<- FALSE
  enabled(x2_spin) <<- FALSE
  enabled(y1_spin) <<- FALSE
  enabled(y2_spin) <<- FALSE
  enabled(crop_but) <<- FALSE
  enabled(sel_seg) <<- FALSE
  enabled(seg_numb) <<- FALSE
  enabled(max_intensity) <<- FALSE
  enabled(op_select) <<- FALSE
  
  
})



enabled(view_button) <- FALSE
enabled(u_but) <- FALSE
enabled(d_but) <- FALSE
enabled(r_but) <- FALSE
enabled(l_but) <- FALSE
enabled(view_but2) <- FALSE
enabled(zoom_in_but) <- FALSE
enabled(zoom_out_but) <- FALSE
enabled(zoom_spin) <- FALSE

enabled(x1_spin) <- FALSE
enabled(x2_spin) <- FALSE
enabled(y1_spin) <- FALSE
enabled(y2_spin) <- FALSE
enabled(crop_but) <- FALSE
enabled(sel_seg) <- FALSE
enabled(seg_numb) <- FALSE
enabled(max_intensity) <- FALSE
enabled(op_select) <- FALSE


##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
# Best Number of components


lyt3 <- glayout ( cont = window3 , spacing = 5)
lyt3[1,1:60,anchor = c(0,0) ] <- win3_aug_lbl <- glabel("Data augmentation", cont = lyt3)
font(win3_aug_lbl) <- list( size= 10, weight = "bold", color = "blue")
win3_tab_1 <- c("Seg1")
win3_tab_2 <- c("  ")
lyt3[2,1:40,anchor = c(-1,0) ] <- win3_sel_lbl <- glabel("Select Segments:", cont = lyt3)
lyt3[2,41:60,anchor = c(-1,0) ] <- win3_sel_lbl <- glabel("Created Augments:", cont = lyt3)

lyt3 [3:71,1:40, anchor = c(0,0)] <- win3_table <-gtable(win3_tab_1, cont = lyt3 , multiple = TRUE)
lyt3 [3:71,41:60, anchor = c(0,0)] <- win3_table2 <-gtable(win3_tab_2, cont = lyt3 , multiple = TRUE)

lyt3 [72,1:60, anchor = c(0,0)] <- lyt3_1 <- glayout ( cont = lyt3 , spacing = 5)
lyt3_1[1,1,anchor = c(0,0) ] <-  name_lbl <- glabel("Augment name:", cont = lyt3_1)
lyt3_1[1,2:20,anchor = c(0,0) ] <-  augment_name <- gedit(initial.msg = "Input a name...", cont = lyt3_1, width = 34)
lyt3 [73,1:60, anchor = c(0,0)] <- lyt3_2 <- glayout ( cont = lyt3 , spacing = 5)
lyt3_2[1,1,anchor = c(-1,0) ] <-  glabel("Type:")
lyt3_2[1,2:21,anchor = c(0,0) ] <-  svd_type <- gcombobox(c("Column wise", "Row wise"), cont = lyt3_2)
lyt3_2[1,22:39,anchor = c(0,0) ] <- aug_button <- gbutton("Creat Aug.", cont = lyt3)
lyt3_2[1,40:56,anchor = c(0,0) ] <- aug_save <- gbutton("Save Aug.", cont = lyt3)
font(aug_button) <- list(weight = "bold", size = 10)
font(aug_save) <- list(weight = "bold", size = 10)
lyt3[74,1:60,anchor = c(0,0) ] <- win3_aug_lbl4 <- glabel("------Determining number of metabolites------", cont = lyt3)
font(win3_aug_lbl4) <- list( size= 10, weight = "bold", color = "blue")
lyt3 [75:90,1:45, anchor = c(0,0)] <- lyt3_3 <- glayout ( cont = lyt3 , spacing = 5)

lyt3[75,46:60, anchor = c(0,0)] <- win3_table3 <-glabel("Eigen values", cont = lyt3)
win3_tab_3 <- c(" ")
lyt3[76:150,46:60, anchor = c(0,0)] <- win3_table3 <- gtable(win3_tab_3, cont = lyt3 , multiple = TRUE)

lyt3[91:160,1:45, anchor = c(0,0)] <- lyt3_5 <- glayout ( cont = lyt3 , spacing = 5)



lyt3_3[1,1,anchor = c(0,0) ] <-  name_lbl3 <- glabel("Select Augment:", cont = lyt3_1)
lyt3_3 [1,2] <- win3_data_combo <- gcombobox ( c ( "Aug1" ,"Aug2", "Aug3","Aug4"), cont = lyt3 )
size(win3_data_combo) <- c(130,20)
lyt3_3[2,1,anchor = c(0,0) ] <-  name_lbl3 <- glabel("Number of PCs:", cont = lyt3_1)
lyt3_3[2,2,anchor = c(0,0) ] <- svd_spin  <- gspinbutton(from = 1, to = 500, value = 50, by = 1)
lyt3_3[3,1:2,anchor = c(0,0) ] <- lyt_3_3_2 <- glayout ( cont = lyt3_3 , spacing = 5)

lyt_3_3_2[1,1:45,anchor = c(0,0) ] <- svd_plot <- gbutton("Plot")
font(svd_plot) <- list( size= 10, weight = "bold", color = "blue")
lyt_3_3_2[2,1:45,anchor = c(0,0) ] <- save_plot <- gbutton("Save plot")
#lyt3_3[4,1:2,anchor = c(0,0) ] <- plot_lable_lyt3 <- glabel("Input number of metabolites:")
#lyt3_3[5,1:2,anchor = c(0,0) ] <- lyt3_3_1 <- glayout (cont = lyt3 , spacing = 5)

#lyt3_3_1[1,5:24,anchor = c(0,0) ] <- comp_spin  <- gspinbutton(from = 1, to = svalue(svd_spin), value = round(svalue(svd_spin)/2), by = 1)
#lyt3_3_1[1,25:45,anchor = c(0,0) ] <- comp_confirm  <- gbutton("Confirm")
#font(comp_confirm) <- list( size= 8, weight = "bold", color = "blue")

lyt3_3[6,1:2,anchor = c(0,0) ] <- glabel("Range of components in plot:")


lyt3_5[1,1, anchor = c(0,0)] <- "From: "
lyt3_5[1,2, anchor = c(0,0)] <- first_comp <- gspinbutton(from = 1, to = svalue(svd_spin), by = 1 , value = 1, cont = lyt3_5)
size(first_comp) <- c(70,23)
lyt3_5[1,3, anchor = c(0,0)] <- "  To:   "
lyt3_5[1,4, anchor = c(0,0)] <- second_comp <- gspinbutton(from = 1, to = svalue(svd_spin), by = 1 , value = svalue(svd_spin), cont = lyt3_2)

lyt3_5[5:15,1:4, anchor = c(0,0)] <- range_view_but <- gbutton("view")

lyt3_5[16:20,1:4,anchor = c(0,0) ] <- plot_lable_lyt3 <- glabel("Input number of metabolites:")
font(plot_lable_lyt3) <- list( size= 10, weight = "bold", color = "blue")
lyt3_5[21:25,1:4,anchor = c(0,0) ] <- comp_spin  <- gspinbutton(from = 1, to = svalue(svd_spin), value = round(svalue(svd_spin)/2), by = 1)
lyt3_5[26:40,1:4,anchor = c(0,0) ] <- comp_confirm  <- gbutton("Confirm (next)")
font(comp_confirm) <- list( size= 10, weight = "bold", color = "blue")




lyt3_5[46:50,1:4,anchor = c(0,0) ] <- back_but_3 <- gbutton("Back", cont = lyt3)

size(second_comp) <- c(70,23)



lyt3[1:2,61,anchor = c(0,0) ] <- svd_zoom_in <- gbutton("Zoom in", cont = lyt3)
lyt3[3:129,61,anchor = c(0,0) ] <- vertical_slider <- gslider(from = 1, to = svalue(svd_spin), by = 1 , value = svalue(svd_spin), horizontal = FALSE)
lyt3[130:140,61,anchor = c(0,0) ] <- svd_zoom_out <- gbutton("Zoom out", cont = lyt3)
lyt3[1:140,62:330,anchor = c(0,0)] <- ggraphics(container = lyt3)

horizon_x1 <- 1 
horizon_x2 <- 50
lyt3[144:150,62:81,anchor = c(0,0) ] <- svd_left_but <- gbutton("Left", cont = lyt3)
lyt3[141:150,82:310,anchor = c(0,0) ] <- horizonal_slider1 <- gslider(from = horizon_x1, to = horizon_x2, by = 1, cont = lyt3)
lyt3[144:150,311:330,anchor = c(0,0) ] <- svd_right_but <- gbutton("Right", cont = lyt3)

size(back_but_3) <- c(20,42)


addHandlerChanged(back_but_3 , function(h,...){
  dev.off()
  visible(window3) <<- FALSE
  visible(window2) <<- TRUE
  plot(0,type = "n",xlab = "1st column time points", ylab = "2nd column time points", main = "Total Ion Chromatogram")
  
})



addHandlerChanged(svd_plot, function(h,...){
  
  enabled(svd_plot) <<- FALSE
  gmessage("Please wait, calculating eigenvalues may take several minutes..." , title = "Eigen value plot")
  
  svd_data <- get(svalue(win3_data_combo))   
  svd_num <- svalue(svd_spin)
  
  r_svd <- irlba( t(svd_data) , nv = 1 , nu = svd_num ,fastpath = TRUE  , tol = 1  )$d
  
  svd_ratio <- c()
  
  for (i in c(1:svd_num)){
    svd_ratio = append(svd_ratio,r_svd[i]/r_svd[i+1])
  }
  
  
  svd_ratio <- svd_ratio - 1
  
  svd_ratio <<- matrix(svd_ratio)
  win3_table3[] <<- svd_ratio
  
  ratio_sort <<- matrix(sort(svd_ratio, decreasing = TRUE))
  color_order <- c()
  color_range <- colorRampPalette(c("black", "white"))(svd_num)
  
  for (i in ratio_sort){
    color_order <- append(color_order, which(i == svd_ratio))
    
  }
  ini_color_range <- color_range
  color_list <<- color_range
  
  for ( i in c(1:svd_num)){
    color_list[color_order[i]] <<- ini_color_range[i]
    
  }
  
  
  svd1 <- 1
  svd2 <- svd_num
  svd_ratio <<- as.vector(svd_ratio[svd1:svd2])
  barplot(height =  svd_ratio,col =  color_list[svd1:svd2],
          beside = TRUE , axes = TRUE,space = 0.4,main = "Eigen value plot", names.arg=seq(svd1,svd2)
          , cex.names = 0.8,font = 1,ylab = "Eigen value",xlab = "Metabolite")
  
  enabled(svd_plot) <<- TRUE
})

slider_count <- 1

addHandlerChanged(vertical_slider, function(h,...){
  enabled(horizonal_slider1) <<- TRUE
  v_slider <- svalue(vertical_slider)
  svd_x1 <- svalue(first_comp)
  svd_x2 <- svalue(second_comp)
  delta_x <- svd_x2-svd_x1 + 1
  
  cat(v_slider)
  cat("   ")
  cat(delta_x)
  print("")
  
  if (v_slider == delta_x)
    delta_x <<- delta_x
  if (v_slider > delta_x){
    delta_change <- v_slider - delta_x
    if ((svd_x2 + delta_change) <= svalue(svd_spin)){
      svd_x2 <- svd_x2 + delta_change
    }
    else
    {
      delta_svd1 <- svalue(svd_spin) - delta_change
      svd_x2 <- svalue(svd_spin)
      svd_x1 <- svd_x1 - delta_change
    }
    svd_x1 <- svd_x1
    svd_x2 <- svd_x2
  }
  
  if ( v_slider < delta_x){
    delta_change <- delta_x - v_slider
    svd_x2 <- svd_x2 - delta_change
    
    svd_x2 <- svd_x2
    
  }
  
  horizon_x2 <<- (svalue(svd_spin) + svd_x1 - svd_x2)
  horizon_x1 <<- 1
  
  
  
  svalue(second_comp) <<- svd_x2
  svalue(first_comp) <<- svd_x1
  svd_ratio <- as.vector(svd_ratio[svd_x1:svd_x2])
  barplot(height =  svd_ratio,col =  color_list[svd_x1:svd_x2],
          beside = TRUE , axes = TRUE,space = 0.4,main = "Eigen value plot", names.arg=seq(svd_x1,svd_x2)
          , cex.names = 0.8,font = 1,ylab = "Eigen value",xlab = "Metabolite")
  
  svd_x22 <- svd_x2  
  svd_x11 <- svd_x1
  horizonal_slider1[] <<- c(1:(svalue(svd_spin)+ svd_x1-  svd_x2))
  svalue(horizonal_slider1) <<- svd_x1
  
  horizon_val <<- svd_x1
  
})

enabled(horizonal_slider1) <- FALSE

h_slider <- 1

addHandlerChanged(horizonal_slider1, function(h,...){
  
  #  coeff <<- 1
  h_change <- svalue(horizonal_slider1) - h_slider
  h_slider <<- svalue(horizonal_slider1)
  
  #  if (svalue(horizonal_slider1) < h_slider)
  #    coeff <<- -1
  svd_x1 <- svalue(first_comp)
  svd_x2 <- svalue(second_comp)
  print(h_change)
  svd_x1 <- svd_x1 + h_change
  svd_x2 <- svd_x2 + h_change
  svalue(second_comp) <<- svd_x2
  svalue(first_comp) <<- svd_x1
  svd_ratio <- as.vector(svd_ratio[svd_x1:svd_x2])
  barplot(height =  svd_ratio,col =  color_list[svd_x1:svd_x2],
          beside = TRUE , axes = TRUE,space = 0.4,main = "Eigen value plot", names.arg=seq(svd_x1,svd_x2)
          , cex.names = 0.8,font = 1,ylab = "Eigen value",xlab = "Metabolite")
  
  
})  


addHandlerChanged(win3_table3, function(h,...){
  svalue(comp_spin) <<- match(svalue(win3_table3),svd_ratio)
  
})






seg_mat_hash <- hash()

addHandlerChanged ( aug_save , handler = function ( h ,... ) {
  
  saving_dir <- gfile( text = "Select a name to save data" ,initial.dir = getwd() ,type = "save" , cont = lyt )
  print(svalue(win3_tab_2))
  saving_aug_dat <-  get(svalue(win3_tab_2)) 
  save(saving_aug_dat , file = paste(saving_dir , ".Rdata",sep = "") )
  
  
  
})


addHandlerChanged ( aug_button , handler = function ( h ,... ) {
  enabled(comp_confirm) <<- TRUE
  augmenting_type <<- svalue(svd_type)
  auging_sets <<- svalue(win3_table)
  seg_mat <<- length(auging_sets)
  augmented_data <- c()
  no_aug <- length(auging_sets)
  svalue(win5_samp_numb) <<- no_aug
  
  
  if (augmenting_type == "Column wise"){
    for (i in auging_sets){
      
      #      augmented_data <- rBind(augmented_data , get(i))
      current_sec_col <<-   first_col_hash[[i]]
    }
    
    augmented_data <- do.call("rBind" , lapply(auging_sets  , get) )
    
    assign(svalue(augment_name),augmented_data, envir = .GlobalEnv)
    s_tab[length(s_tab) + 1] <<- svalue(augment_name)
    
    if( length(win3_tab_2) == 1)
      win3_tab_2[1] <<- svalue(augment_name)
    else 
      win3_tab_2[length(win3_tab_2) + 1 ] <<- svalue(augment_name)
    
    
    #  first_col_hash[[svalue(augment_name)]] <<- first_col_hash
    seg_mat_hash[[svalue(augment_name)]] <<- (seg_mat * current_sec_col )
    
    
    win3_table2[] <<- win3_tab_2
    win3_data_combo[] <<- win3_tab_2
    first_col_hash[[svalue(augment_name)]] <<- first_col_hash[[auging_sets[1]]]
    second_col_hash[[svalue(augment_name)]] <<- second_col_hash[[auging_sets[1]]]
    
    #    win3_table[] <<- s_tab
    #    win3_data_combo <<- s_tab
    
  }
  
  
  
})



addHandlerChanged(comp_confirm , function(h,...){
  svalue(win5_comp_numb) <<- svalue(comp_spin)
  dev.off()
  visible(window3) <<- FALSE
  visible(window4) <<- TRUE
  matplot(0 , type = "l" , xlab = "Retention time" , ylab = "Intensity" , main = "Chromatogram of compressed data")
  if(win3_tab_2 == "  ")
    data_for_comp[] <<- s_tab
  else
    data_for_comp[] <<- win3_tab_2
  assign("Seg1_S1" , NULL, envir = .GlobalEnv)
  assign("Seg1_S2" , NULL, envir = .GlobalEnv)
  assign("Seg1_S3" , NULL, envir = .GlobalEnv)
  assign("Seg1_S4" , NULL, envir = .GlobalEnv)
  assign("Seg1_S5" , NULL, envir = .GlobalEnv)
  assign("Seg1_S6" , NULL, envir = .GlobalEnv)
  assign("Seg1_C1" , NULL, envir = .GlobalEnv)
  assign("Seg1_C2" , NULL, envir = .GlobalEnv)
  assign("Seg1_C3" , NULL, envir = .GlobalEnv)
  assign("Seg1_C4" , NULL, envir = .GlobalEnv)
  assign("Seg1_C5" , NULL, envir = .GlobalEnv)
  assign("Seg1_C6" , NULL, envir = .GlobalEnv)
  assign("real_dat_tic" , NULL, envir = .GlobalEnv)
  assign("sel_data_fc" , NULL, envir = .GlobalEnv)
  assign("tic" , NULL, envir = .GlobalEnv)
  
  first_compress <<- TRUE
  
  
  
})

#enabled(comp_confirm) <- FALSE

addHandlerChanged ( prep_next_but , handler = function ( h ,... ) {
  svalue(win5_comp_numb) <<- svalue(comp_spin)
  dev.off()
  visible(window2) <<- FALSE
  visible(window3) <<- TRUE
  plot(0)
  win3_table[] <<- s_tab
  win3_data_combo[] <<- s_tab
  
})


##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
# Best data compression



lyt4 <- glayout ( cont = window4 , spacing = 5)
lyt4[1,1:350,anchor = c(0,0)] <- intro_label4 <- glabel ( "Data compression" , cont = lyt4 )
font(intro_label4) <- list(weight = "bold", color = "black",size = 12)


lyt4[2,1:30,anchor = c(0,0) ] <-  "Select an agument to compress:"


sel_data_fc <- matrix(c(1:20) , nrow = 4 , ncol = 5)

lyt4[2,31:70,anchor = c(0,0) ] <-  data_for_comp <- gcombobox(c("Aug 1", "Aug 2"), cont = lyt4)
lyt4[2,75:85,anchor = c(0,0) ] <-  "Data size:"
lyt4[2,86:100,anchor = c(-1,0) ] <-  label_size <- glabel ( "...." , cont = lyt4 )


font(label_size) <- list(weight = "bold", color = "blue")



initial_size <- 0

if (initial_size < 1000){
  initial_size <- round(initial_size,2)
  svalue(label_size) <- paste(as.character(initial_size),"B")
}

if ((initial_size >= 1000) & (initial_size < 1000000)){
  initial_size <- round((initial_size/1000),2)
  svalue(label_size) <- paste(as.character(initial_size),"kB")
}

if ((initial_size >= 1000000) & (initial_size < 1000000000)){
  initial_size <- round((initial_size/1000000),2)
  svalue(label_size) <- paste(as.character(initial_size),"Mb")
}

if ((initial_size >= 1000000000) & (initial_size < 1000000000000)){
  initial_size <- round((initial_size/1000000000),2)
  svalue(label_size) <- paste(as.character(initial_size),"GB")
}

lyt4[2,101:120,anchor = c(0,0) ] <-  "Compression level:"
lyt4[2,121:130,anchor = c(0,0) ] <- compression_level  <- gspinbutton(from = 1, to = 10, value = 3, by = 1)

lyt4[2,135:165,anchor = c(0,0) ] <- "Data size after compression:"

lyt4[2,166:180,anchor = c(-1,0) ] <-  label_size2 <- glabel ( "...." , cont = lyt4 )

font(label_size2) <- list(weight = "bold", color = "blue")





addHandlerChanged(compression_level, function(h,...){
  
  compressed_size <<- object.size(get(svalue(data_for_comp))) / (2 ^(svalue(compression_level)))
  
  
  if (compressed_size < 1000){
    compressed_size <- round(compressed_size,2)
    svalue(label_size2) <<- paste(as.character(compressed_size),"B")
  }
  
  if ((compressed_size >= 1000) & (compressed_size < 1000000)){
    compressed_size <- round((compressed_size/1000),2)
    svalue(label_size2) <<- paste(as.character(compressed_size),"kB")
  }
  
  if ((compressed_size >= 1000000) & (compressed_size < 1000000000)){
    compressed_size <- round((compressed_size/1000000),2)
    svalue(label_size2) <<- paste(as.character(compressed_size),"Mb")
  }
  
  if ((compressed_size >= 1000000000) & (compressed_size < 1000000000000)){
    compressed_size <- round((compressed_size/1000000000),2)
    svalue(label_size2) <<- paste(as.character(compressed_size),"GB")
  }
  
})

lyt4[2,280:327,anchor = c(0,0) ] <-  compress_but <- gbutton("Compress")
font(compress_but) <- list(weight = "bold", color = "blue" , size = 10)
lyt4[2,328:350,anchor = c(0,0) ] <-  "Compressed Augments"

comp_data <- c(" ")
lyt4[3:180,328:350, anchor = c(0,0)] <- win4_table3 <- gtable(comp_data, cont = lyt4 , multiple = FALSE)
lyt4[181:185,328:350, anchor = c(0,0)] <- next_but4 <- gbutton("Next" , cont = lyt4)
lyt4[181:185,1:38, anchor = c(0,0)] <- back_but4 <- gbutton("Back" , cont = lyt4)

lyt4[3:180,1:326, anchor = c(0,0)] <- comp_graph <- ggraphics(container = lyt4)

addHandlerChanged(back_but4 , function(h,...){
  
  dev.off()
  visible(window4) <<- FALSE
  
  visible(window3) <<- TRUE
  plot(0)
  
  
})

addHandlerClicked (data_for_comp , function(h,...){
  
  sel_data_fc <<- get(svalue(data_for_comp))
  initial_size <<- object.size(sel_data_fc)
  
  
  if (initial_size < 1000){
    initial_size <- round(initial_size,2)
    svalue(label_size) <<- paste(as.character(initial_size),"B")
  }
  
  if ((initial_size >= 1000) & (initial_size < 1000000)){
    initial_size <- round((initial_size/1000),2)
    svalue(label_size) <<- paste(as.character(initial_size),"kB")
  }
  
  if ((initial_size >= 1000000) & (initial_size < 1000000000)){
    initial_size <- round((initial_size/1000000),2)
    svalue(label_size) <<- paste(as.character(initial_size),"Mb")
  }
  
  if ((initial_size >= 1000000000) & (initial_size < 1000000000000)){
    initial_size <- round((initial_size/1000000000),2)
    svalue(label_size) <<- paste(as.character(initial_size),"GB")
  }
  
  
  
  compressed_size <- object.size(sel_data_fc) / (2 ^(svalue(compression_level)))
  
  
  if (compressed_size < 1000){
    compressed_size <- round(compressed_size,2)
    svalue(label_size2) <<- paste(as.character(compressed_size),"B")
  }
  
  if ((compressed_size >= 1000) & (compressed_size < 1000000)){
    compressed_size <- round((compressed_size/1000),2)
    svalue(label_size2) <<- paste(as.character(compressed_size),"kB")
  }
  
  if ((compressed_size >= 1000000) & (compressed_size < 1000000000)){
    compressed_size <- round((compressed_size/1000000),2)
    svalue(label_size2) <<- paste(as.character(compressed_size),"Mb")
  }
  
  if ((compressed_size >= 1000000000) & (compressed_size < 1000000000000)){
    compressed_size <- round((compressed_size/1000000000),2)
    svalue(label_size2) <<- paste(as.character(compressed_size),"GB")
  }
  
})




addHandlerChanged(compress_but, function(h,...){
  
  assign("sel_data_fc" , NULL, envir = .GlobalEnv)
  assign("tic" , NULL, envir = .GlobalEnv)
  
  mydata <<- get(svalue(data_for_comp))
  
  assign(svalue(data_for_comp), NULL, envir = .GlobalEnv)
  
  now_fir_col <- seg_mat_hash[[svalue(data_for_comp)]]
  prog_c <- ncol(mydata)
  layout( c(1:5))
  #layout(3)
  plot.new()
  plot.new()
  par(mar=c(4,1.5,2,2.5))
  barplot( prog_c , horiz = TRUE , col = "white" , main = "Data compression is in progress..." , xaxt='n',yaxt='n',ylab='',xlab='' )
  
  n_col_mydata <- ncol(mydata)
  
  u_level <- svalue(compression_level)
  sel_filter <- paste("d",2*u_level,sep = "")
  cur_row <- mydata[,1]
  comp_cur_row <- dwt(cur_row , n.levels = u_level , filter = sel_filter )
  m_comp <- matrix(unlist(comp_cur_row@V[u_level])) 
  f_comp_data <- matrix(rep(0,nrow(m_comp)*n_col_mydata), ncol = n_col_mydata , nrow = nrow(m_comp))
  
  for (i in c(1:n_col_mydata)){
    print(i)
    
    cur_row <- mydata[,i]
    comp_cur_row <- dwt(cur_row , n.levels = u_level , filter = sel_filter )
    m_comp <- matrix(unlist(comp_cur_row@V[u_level]))  
    f_comp_data[,i ] <- m_comp
    par(mar=c(4,1.5,2,2.5))
    barplot( i , horiz = TRUE , col = "blue" , add = TRUE , main = "Data compression is in progress..." , xaxt='n'  ,yaxt='n',ylab='',xlab='' )
  }
  
  layout( c(1))
  par(mar=c(4,4,4,4))
  
  plot_comp_data <- rowSums(f_comp_data)
  plot_comp_data <- matrix(plot_comp_data)
  matplot(plot_comp_data , type = "l")
  
  plot_comp_data[plot_comp_data > 0] <- 0
  matplot(plot_comp_data , type = "l" , col = "red",add = TRUE )
  comp_data_name <<- paste("Comp", svalue(compression_level) , svalue(data_for_comp) , sep = "_")
  
  assign(comp_data_name ,f_comp_data, envir = .GlobalEnv)
  
  seg_mat_hash[[comp_data_name]] <<- now_fir_col
  
  if(comp_data[1] == " ")
    comp_data[1] <<- comp_data_name
  else
    comp_data[length(comp_data) + 1] <<- comp_data_name
  
  win4_table3[] <<- comp_data
  s_tab <<- c(s_tab ,comp_data)
  
  first_col_hash[[comp_data_name]] <<- first_col_hash[[svalue(data_for_comp)]]
  
})



addHandlerClicked(win4_table3 , function(h,...){
  view_data_name <- svalue(win4_table3)
  view_data <- get(view_data_name)
  
  plot_comp_data <- rowSums(view_data)
  plot_comp_data <- matrix(plot_comp_data)
  matplot(plot_comp_data , type = "l")
  
  plot_comp_data[plot_comp_data > 0] <- 0
  matplot(plot_comp_data , type = "l" , col = "red",add = TRUE )
  
  
})

#assign(r_data_name,get(load(file = file_dir)), envir = .GlobalEnv)

addHandlerChanged(next_but4 , function(h,...){
  
  dev.off()
  visible(window4) <<- FALSE
  visible(window5) <<- TRUE
  win5_aug_combo[] <<- s_tab
  print(s_tab)
  matplot(0 , main = "Mass spectra" , xlab = "m/z" , ylab = "Intensity")
  
})


##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
# Best model builder

sec_col_mat_list <- c()
fir_col_mat_list <- c()
spec_mat_list <- c()


lyt5 <- glayout ( cont = window5 , spacing = 5)

#lyt5[1,1:260,anchor = c(0,0)] <- win5_lab0 <- glabel ( "Pure components' concentration & spectal profiles"  )
#font(win5_lab0) <- list(weight = "bold", color = "blue",size = 14)


#lyt5[1,1:260,anchor = c(0,0)] <- win5_lab0 <- glabel ( "Pure components' concentration & spectal profiles"  )
#font(win5_lab0) <- list(weight = "bold", color = "blue",size = 14)

lyt5[1,1:40,anchor = c(0,0)] <- win5_lab6 <- glabel ( "____________________ Data ____________________"  )
font(win5_lab6) <- list(weight = "bold", color = "blue",size = 10)
lyt5[2,1:5,anchor = c(0,0)] <- win5_lab7 <- glabel ( "Select a matrix:"  )
lyt5[2,7:40,anchor = c(0,0)] <- win5_aug_combo <- gcombobox ( c ( "Aug 1" ) )
lyt5[3,1:11,anchor = c(0,0)] <- win5_aug_num_lab <- "Number of metabolites:"
lyt5[3,21:40,anchor = c(0,0)] <- win5_comp_numb  <- gspinbutton(from = 1, to = 1000, value = 50, by = 1)
lyt5[4,1:8,anchor = c(0,0)] <- win5_aug_num_lab <- "Number of samples:"
lyt5[4,21:40,anchor = c(0,0)] <- win5_samp_numb  <- gspinbutton(from = 1, to = 1000, value = 1, by = 1)
lyt5[5,1:40] <- win5_lab1 <- glabel ( "____________ Output data name ____________"  )
font(win5_lab1) <- list(weight = "bold", color = "blue",size = 10)
lyt5[6,1:5,anchor = c(0,0)] <- win5_lab6 <- glabel ( "Elution profiles:"  )

lyt5[6,7:40,anchor = c(0,0) ] <- win5_con_name1  <- gedit(initial.msg = "Input a name..." , width = 8)

lyt5[7,1:4,anchor = c(0,0)] <- win5_lab6 <- glabel ( "Pure spectra:"  )

lyt5[7,7:40,anchor = c(0,0) ] <- win5_con_name2  <- gedit(initial.msg = "Input a name..." , width = 8)

lyt5[8,1:40] <- win5_lab1 <- glabel ( "__________ Model configuration __________"  )
font(win5_lab1) <- list(weight = "bold", color = "blue",size = 10)



lyt5[9,1:9,anchor = c(0,0)] <- win5_lab2 <- glabel ( "Select the constrain(s):"  )
lyt5[10,1:9,anchor = c(0,0)] <- win5_lab2_1 <- glabel ( "AND:"  )
lyt5[11,1:9,anchor = c(0,0)] <- win5_lab2_2 <- glabel ( "AND:"  )



lyt5[9,10:40] <- win5_model_combo <- gcombobox ( c ( "Non negativity" ,"Unimodality" , "Closure" ,"None") )
lyt5[10,10:40] <- win5_model_combo2 <- gcombobox ( c ( "None","Non negativity" ,"Unimodality" , "Closure") )
lyt5[11,10:40] <- win5_model_combo3 <- gcombobox ( c ( "None","Non negativity" ,"Unimodality" , "Closure") )

lyt5[13,1:17,anchor = c(0,0)] <- win5_lab3 <- glabel ( "Maximum number of iterations:"  )

lyt5[13,20:40,anchor = c(0,0) ] <- win5_svd_spin  <- gspinbutton(from = 1, to = 1000, value = 300, by = 1)

lyt5[14,1:9,anchor = c(0,0)] <- win5_lab4 <- glabel ( "Convergence criterion:")

lyt5[14,12:40,anchor = c(0,0) ] <- win5_conv  <- gedit(text = "0.01" , width = 8)
#lyt5[15,1:6,anchor = c(0,0) ] <- win5_lab5 <- glabel ( "Performer system:")
#lyt5[15,12:40] <- win5_core <- gcombobox ( c ( "This PC" ,"Send to HPC") )

lyt5[15:16,1:40] <- run_but <- gbutton("Run")
#lyt5[17,1:40] <- export_but <- gbutton("Export the data & code to HPC")

#size(run_but) <- c(60,90)
font(run_but) <- list( weight = "bold" , color = "blue",size = 12)
#font(export_but) <- list( weight = "bold" , color = "blue",size = 10 )
#enabled(export_but) <- FALSE

lyt5[18,1:40] <- win5_lab11 <- glabel ( "_________________ Status _________________"  )
font(win5_lab11) <- list(weight = "bold", color = "blue",size = 10)

lyt5[21,1:4,anchor = c(0,0)] <- win5_lab22 <- glabel ("Converging?")
lyt5[20:22,5:20,anchor = c(0,0)] <- is_conv <- gimage("C:/Program Files (x86)/RMet/RMet/Other/Pics/tik3.png")
font(win5_lab22) <- list(weight = "bold", color = "blue",size = 10)

lyt5[24:30,1:40,anchor = c(0,0)] <- image_test <- gimage("C:/Program Files (x86)/RMet/RMet/Other/Pics/e_rotator1.png")
#lyt5[31,1 : 40 ,anchor = c(0,0)] <- win5_lab30 <- glabel("_________ Importing HPC output _________") 
#font(win5_lab30) <- list(weight = "bold", color = "blue",size = 10)

#lyt5[32:36,1 : 25 ,anchor = c(0,0)] <- hpc_but <- gbutton("Browse & import")
#font(hpc_but) <- list(weight = "bold", color = "blue",size = 10)
#lyt5[32,26 : 40 ,anchor = c(0,0)] <- win5_hpc_lab <- glabel(".............")

lyt5[1,41 : 220 ,anchor = c(0,0)] <- win5_lab13 <- glabel("-------------------------------------------------------- Progress track ------------------------------------------------------------")
font(win5_lab13) <- list(weight = "bold", color = "blue",size = 12)

lyt5[2,41 : 220 ,anchor = c(0,0)] <- win5_lab14 <- glabel(" ")
font(win5_lab14) <- list(weight = "bold", color = "blue",size = 12)

i <- 0
n_comp <- 5
svalue(win5_lab14) <- paste("Generating initial estimiation: ",round(100 * i / n_comp) , "%" ," Completed" )
lyt5[3:79,41 : 270 ,anchor = c(0,0)] <- ggraphics()


lyt5[80:90,261 : 300 ,anchor = c(0,0)] <- win5_next_but <- gbutton("Next")
lyt5[80:90,41 : 80 ,anchor = c(0,0)] <- win5_back_but <- gbutton("Back")


font(win5_back_but) <- list(weight = "bold", color = "blue",size = 10)
font(win5_next_but) <- list(weight = "bold", color = "blue",size = 10)

addHandlerChanged(win5_back_but , function(h,...){
  dev.off()
  visible(window5) <<- FALSE
  visible(window4) <<- TRUE
  matplot(0 , type = "l" , xlab = "Retention time" , ylab = "Intensity" , main = "Chromatogram of compressed data")
  if(win3_tab_2 == "  ")
    data_for_comp[] <<- s_tab
  else
    data_for_comp[] <<- win3_tab_2
  
})



lyt5[1,271 : 300 ,anchor = c(0,0)] <- win5_lab17 <- glabel("Model statistics")

lyt5[2,271 : 300 ,anchor = c(0,0)] <- win5_lab45 <- glabel("_________________")

lyt5[3,271 : 300 ,anchor = c(0,0)] <- win5_lab18 <- glabel("Noise level:")

lyt5[4,271 : 300 ,anchor = c(0,0)] <- win5_lab41 <- glabel("....... %")

lyt5[9,271 : 300 ,anchor = c(0,0)] <- win5_lab19 <- glabel("Lack of fit:")

lyt5[10,271 : 300 ,anchor = c(0,0)] <- win5_lab42 <- glabel("....... %")

lyt5[15,271 : 300 ,anchor = c(0,0)] <- win5_lab20 <- glabel("Initial RSS:")
lyt5[16,271 : 300 ,anchor = c(0,0)] <- win5_lab43 <- glabel("................")

lyt5[21,271 : 300 ,anchor = c(0,0)] <- win5_lab21 <- glabel("Final RSS:")
lyt5[22,271 : 300 ,anchor = c(0,0)] <- win5_lab44 <- glabel("................")


font(win5_lab18) <- list(weight = "bold", color = "blue",size = 10)

font(win5_lab17) <- list(weight = "bold", color = "blue",size = 11)
font(win5_lab41) <- list(weight = "bold", color = "blue",size = 11)
font(win5_lab42) <- list(weight = "bold", color = "blue",size = 11)
font(win5_lab43) <- list(weight = "bold", color = "blue",size = 11)
font(win5_lab44) <- list(weight = "bold", color = "blue",size = 11)
font(win5_lab45) <- list(weight = "bold", color = "blue",size = 11)


font(win5_lab19) <- list(weight = "bold", color = "blue",size = 11)
font(win5_lab20) <- list(weight = "bold", color = "blue",size = 11)
font(win5_lab21) <- list(weight = "bold", color = "blue",size = 11)




#addHandlerChanged(win5_core , function(h,...){
#  if (svalue(win5_core) == "This PC"){
#    enabled(run_but) <<- TRUE
#    enabled(export_but) <<- FALSE
#  }
  
#  if (svalue(win5_core) == "Send to HPC"){
#    enabled(run_but) <<- FALSE
#    enabled(export_but) <<- TRUE
    
#  }
  
#})



source <- "C:/Program Files (x86)/RMet/RMet/Other/Pics/"

start_stop <- TRUE
starter <- 1


saeed_als <- function (CList, PsiList, S = matrix(), WList = list(), thresh = 0.001, 
                       maxiter = 100, forcemaxiter = FALSE, optS1st = TRUE, x = 1:nrow(CList[[1]]), 
                       x2 = 1:nrow(S), baseline = FALSE, fixed = vector("list", 
                                                                        length(PsiList)), uniC = FALSE, uniS = FALSE, nonnegC = TRUE, 
                       nonnegS = TRUE, normS = 0, closureC = list()) 
{
  RD <- 10^20
  PsiAll <- do.call("rBind", PsiList)
  resid <- vector("list", length(PsiList))
  if (length(WList) == 0) {
    WList <- vector("list", length(PsiList))
    for (i in 1:length(PsiList)) WList[[i]] <- matrix(1, 
                                                      nrow(PsiList[[1]]), ncol(PsiList[[1]]))
  }
  W <- do.call("rBind", WList)
  for (i in 1:length(PsiList)) resid[[i]] <- matrix(0, nrow(PsiList[[i]]), 
                                                    ncol(PsiList[[i]]))
  for (j in 1:length(PsiList)) {
    for (i in 1:nrow(PsiList[[j]])) {
      resid[[j]][i, ] <- PsiList[[j]][i, ] - CList[[j]][i, 
                                                        ] %*% t(S * WList[[j]][i, ])
    }
  }
  initialrss <- oldrss <- sum(unlist(resid)^2)
  
  svalue(win5_lab14) <<-  paste("Initial RSS", initialrss)
  #    cat("Initial RSS", initialrss, "\n")
  iter <- 1
  starter <- 1
  b <- if (optS1st) 
    1
  else 0
  oneMore <- FALSE
  t1 <- proc.time()
  
  while (((RD > thresh || forcemaxiter) && maxiter >= iter) || 
         oneMore) {
    if (iter%%2 == b){ 
      S <- getS(CList, PsiAll, S, W, baseline, uniS, nonnegS, 
                normS, x2)
      matplot(S , main = "Mass spectra" , xlab = "m/z" , ylab = "Intensity" , type = "l")
    }
    else CList <- getCList(S, PsiList, CList, WList, resid, 
                           x, baseline, fixed, uniC, nonnegC, closureC)
    for (j in 1:length(PsiList)) {
      for (i in 1:nrow(PsiList[[j]])) {
        resid[[j]][i, ] <- PsiList[[j]][i, ] - CList[[j]][i, 
                                                          ] %*% t(S * WList[[j]][i, ])
      }
      
      if ((proc.time() - t1)[3] > 1){
        t1 <- proc.time()
        rot_num <- starter %% 8
        if (rot_num == 0)
          rot_num <- 8
        
        svalue(image_test) <<- paste(source,"rotator",rot_num , ".png", sep = "")
        starter <- starter + 1
      }
      
      
      
    }
    rss <- sum(unlist(resid)^2)
    RD <- ((oldrss - rss)/oldrss)
    oldrss <- rss
    typ <- if (iter%%2 == b) 
      "S"
    else "C"
    svalue(win5_lab14) <<-  paste("Iteration (opt. ", typ, "): ", iter, ", RSS: ", 
                                  rss, ", RD: ", RD, sep = "")
    iter <- iter + 1
    oneMore <- (normS > S && (iter%%2 != b) && maxiter != 
                  1) || (length(closureC) > 0 && (iter%%2 == b))
  }
  
  svalue(win5_lab14) <<- paste("Initial RSS / Final RSS =", initialrss, "/", rss, "=", 
                               initialrss/rss)
  
  return(list(CList = CList, S = S, rss = rss, resid = resid, 
              iter = iter))
}


addHandlerChanged(win5_aug_combo , function(h,...){
  
  svalue(win5_con_name1) <<- paste("copt",svalue(win5_aug_combo) , sep = "_")
  svalue(win5_con_name2) <<- paste("sopt",svalue(win5_aug_combo) , sep = "_")
})


addHandlerChanged(run_but, function(h,...){
  is_non <<- FALSE
  is_uni <<- FALSE
  is_clos <<- FALSE
  
  
  if ((svalue(win5_model_combo) == "Non negativity") | (svalue(win5_model_combo2) == "Non negativity") | (svalue(win5_model_combo3) == "Non negativity"))
    is_non <<- TRUE
  if ((svalue(win5_model_combo) == "Unimodality") | (svalue(win5_model_combo2) == "Unimodality") | (svalue(win5_model_combo3) == "Unimodality"))
    is_uni <<- TRUE
  if ((svalue(win5_model_combo) == "Closure") | (svalue(win5_model_combo2) == "Closure") | (svalue(win5_model_combo3) == "Closure"))
    is_non <<- TRUE
  
  
  
  n_comp <<- svalue(win5_comp_numb)
  mydata <-  get(svalue(win5_aug_combo)) 
  saved_data <- mydata
  #  n_comp <- as.integer(readline("input number of components: "))
  #  last_comp <- as.numeric(readline(("input number of components for last analysi: ")))
  last_comp <<- 1000
  
  most_dis <- function(mydata,ref_data){
    row_numb <<- nnrow <- as.integer(nrow(mydata)) + 1 -1 
    det_list <- c()
    for (i in c(1:row_numb)){
      
      s_data <- data.matrix(matrix(unlist(mydata[i,]),nrow = 1, byrow=T))
      r_data <- rBind(ref_data,s_data)  
      d_data <- r_data %*% t(r_data)
      d_det <- det(d_data)
      det_list[length(det_list)+1 ] <- d_det
      
    }
    
    sort_det_list <- sort(det_list,decreasing = TRUE, index.return = TRUE)
    #sort_det_list_val <- sort(det_list)
    sort_index <- sort_det_list$ix
    max_index <- which.max(det_list) + 1 - 1
    
    #max_spec <- data.matrix(matrix(unlist(mydata[max_index,]),nrow = 1, byrow=T))
    return(list(first = max_index, second = sort_index))
    
  }
  mid_ref <- data.matrix(matrix(unlist(colMeans(mydata)),nrow = 1, byrow=T))
  #main_func <- most_dis(mydata,mid_ref)
  #max_index <- main_func$first
  #sort_index0 <- main_func$second
  max_index_list = c()
  time_list = c()
  #sort2 <- length(sort_index0) + 1 - 1
  #sort1 <- as.integer(round(sort2 * 0.9)) +1 - 1
  #if (sort1 > 1000){
  #  mydata <- mydata[-sort_index0[sort1:sort2],]
  #opa2
  row_numb2 <- nnrow <- as.integer(nrow(mydata)) + 1 -1 
  del_p <- (last_comp/row_numb2)**(1/n_comp)
  
  for (i in c(1:n_comp)){
    #cat("\014")
    t1 <- proc.time()
    if (i == 1){
      ref_data <- mid_ref
      main_func <- most_dis(mydata, ref_data)
      max_index <- main_func$first
      max_index_list[length(max_index_list)+1 ] <- max_index
      ref_data <- data.matrix(matrix(unlist(mydata[max_index,]),nrow = 1, byrow=T))
      sort_index <- main_func$second
      sort2 <- length(sort_index) + 1 - 1
      sort1 <- as.integer(round(sort2 * del_p)) +1 - 1
      
      if (sort1 > 1000){
        mydata <- mydata[-sort_index[sort1 : (sort2-i)],]
      }
    }
    else {
      main_func <- most_dis(mydata, ref_data)
      max_index <- main_func$first
      max_index_list[length(max_index_list)+1 ] <- max_index
      ref_data <- rBind(ref_data, data.matrix(matrix(unlist(mydata[max_index,]),nrow = 1, byrow=T)))
      sort_index <- main_func$second
      sort2 <- length(sort_index) + 1 - 1
      sort1 <- as.integer(round(sort2 * del_p)) + 1 -1
      
      if (sort1 > 1000 ){
        mydata <- mydata[-sort_index[sort1:(sort2-i)],]
      }
    }
    
    svalue(win5_lab14) <- paste("Generating initial estimiation: ",round(100 * i / n_comp) , "%" ," Completed." )
    #    svalue(win5_lab9)  <<- "Generating initial estimiation:"
    #    svalue(win5_lab10)  <<- paste(round(100 * i / n_comp) , "%")
    #    svalue(win5_lab11)  <<- "Completed"
    #    font(win5_lab9) <<- list( weight = "bold" , color = "blue",size = 14)
    #    font(win5_lab10) <<- list(   color = "blue",size = 22)
    #    font(win5_lab11) <<- list( weight = "bold" , color = "blue",size = 14)
    
  }
  svalue(win5_lab14) <- paste("Initiating the MCR..." )
  
  max_index_data <- data.matrix(max_index_list)
  
  s_opa = c()
  
  row_numb3 <-as.integer(nrow(saved_data)) + 1 -1
  sum_list <<- c()
  ref_data <<- ref_data
  
  
  main_index <<- c()
  row_numb4 <- as.integer(nrow(ref_data)) +1 -1
  
  sum_row <- rowSums(saved_data)
  ref_row <- rowSums(ref_data)
  
  
  for ( i in ref_row){
    
    #  print(dim(ref_data))
    #    check_sum <- round(sum(ref_data[i,]),2)
    
    this_check <- which(sum_row == i)
    #  print(this_check)
    
    main_index[length(main_index) + 1] <- this_check
  }
  
  
  
  main_index_data <- data.matrix(main_index)
  
  
  for (j in main_index_data){
    s_opa <- rBind(s_opa,saved_data[j,])
  }
  
  s_opa <<- data.matrix(s_opa)
  #assign(paste("sopt",svalue(win5_aug_combo) , sep = "_"), s_opa, envir = .GlobalEnv)
  assign(svalue(win5_con_name2), s_opa, envir = .GlobalEnv)
  
  #  is_save <- readline("want to save opa result?(y/n) ")
  
  #  is_t_plot <- readline("want process time plot?(y/n) ")
  #  if (is_t_plot == "y"){
  #    plot(time_list)
  #  }
  
  
  #  raw_data <- mydata 
  s_estimate <- s_opa 
  n_mat <<- as.integer(seg_mat_hash[[svalue(win5_aug_combo)]]) 
  
  if (svalue(win_type) == "GC-MS"){
    if ((nrow(saved_data) %% first_col_hash[[svalue(win5_aug_combo)]]) != 0){
      cur_x <<- first_col_hash[[svalue(win5_aug_combo)]]
      cur_nrow <<- nrow(saved_data)
      excess_x <<- cur_nrow - floor(cur_nrow / cur_x) * cur_x
      
      if (excess_x %% 2 == 0){
        haf <- excess_x / 2
        del_x <- c(1 : haf)
        append( del_x , c(cur_nrow - haf + 1 :cur_nrow))
        saved_data <<- saved_data[ - del_x , ]
        
      }
      else{
        haf <- floor(excess_x / 2)
        del_x <- c(1 : haf + 1)
        append( del_x , c(cur_nrow - haf + 1 :cur_nrow))
        saved_data <<- saved_data[ - del_x , ]
      }
      
      
    }
    
  }else{
    if ((nrow(saved_data) %% (first_col_hash[[svalue(win5_aug_combo)]]) * n_mat) != 0){
      
      
      saved_data <<- saved_data[ C(1:nrow(saved_data) %/% n_mat * n_mat) , ]
      
    }
    
    
    
  }
  
  
  raw_c_ini <<- t(ginv(t(s_estimate)) %*% t(saved_data))
  n_mat <<- as.integer(seg_mat_hash[[svalue(win5_aug_combo)]])  
  #as.integer(readline("input Number of matrices: ")) + 1 -1
  
  data_row <- as.integer(nrow(raw_c_ini)) +1 - 1
  data_col <- as.integer(ncol(raw_c_ini)) +1 - 1
  s_row <<- as.integer(data_row / n_mat) +1 -1
  ini_esti_c_list <<- list()
  for (i in c(1:n_mat)){
    
    ini_numb <- (i-1)*s_row  + 1
    end_numb <- i * s_row
    ex_c_mydata <- raw_c_ini[ini_numb:end_numb,]
    
    ini_esti_c_list <<- append(ini_esti_c_list,list(ex_c_mydata))
    
  }
  
  
  mydata_list = list()
  
  for (i in c(1:n_mat)){
    
    ini_numb <- (i-1)*s_row  + 1
    end_numb <- i * s_row
    ex_mydata <- saved_data[ini_numb:end_numb,]
    mydata_list <- append(mydata_list,list(ex_mydata))
    
    
  }
  
  
  #svalue(win5_lab9) <<- " "
  #svalue(win5_lab10) <<- "  "
  #svalue(win5_lab11) <<- "  "
  #  t11 <- proc.time()
  test0 <- saeed_als(CList=ini_esti_c_list,S=matrix(1,nrow= dim(saved_data)[2],ncol= svalue(win5_comp_numb )),
                     PsiList=mydata_list,normS = 2,optS1st=TRUE,thresh = as.numeric(svalue(win5_conv))  , nonnegC = TRUE,nonnegS = TRUE,maxiter = svalue(win5_svd_spin) , uniC = is_uni , uniS = is_uni )
  #  t22 <- proc.time()
  #  print(t22 - t11)
  
  first_r_mcr_s <<- test0$S
  
  svalue(win5_lab14) <<- "Generating profiles...."
  num_seg <<- svalue(win5_samp_numb)
  
  c_real <- t(ginv(first_r_mcr_s) %*% t(saved_data))
  assign(svalue(win5_con_name1) , c_real , envir = .GlobalEnv)
  
  c_real_row <<- nrow(c_real)
  sample_row <- c_real_row / num_seg
  
  
  
  
  
  for ( k in c(1:num_seg)){
    
    uaug <- c_real[(c(1 + (k-1)*sample_row) : (k*sample_row) ) ,]
    c_nrow <- as.numeric(nrow(uaug)) 
    c_ncol <- as.numeric(ncol(uaug)) 
    
    
    nnmat <- n_mat / num_seg
    m_nrow <- c_nrow / nnmat
    #cut_edge <- c_nrow / num_seg
    
    #c_ncol
    u1 = c()
    u2 = c()
    a1 <<- c()
    a2 <<- c()
    
    
    for( j in c(1: c_ncol)){
      
      m <- matrix(uaug[,j] , nrow = m_nrow , ncol = nnmat)
      m_svd <- svd(m , nu = m_nrow , nv = nnmat)
      u_m <- m_svd$u
      u_v <- m_svd$v
      u_s <- m_svd$d
      u1 <<- cbind(u1 , u_m[,1])
      u2 <<- cbind(u2 , as.vector(t(u_v[1, ])))
      a1 <<- cbind(a1 ,as.vector( t(colMeans(t(m)))) )
      a2 <<- cbind(a2 ,as.vector( t(colMeans(m))) )
      #  u2 <- t(u2)
      
    }
    
    
    first_col_con <<- paste("First_col","S",k , svalue(win5_aug_combo) , sep = "_" )
    second_col_con <<- paste("Sec_col","S",k ,svalue(win5_aug_combo), sep = "_" )
    
    sec_col_mat_list[length(sec_col_mat_list) + 1] <<- second_col_con
    fir_col_mat_list[length(fir_col_mat_list) + 1] <<- first_col_con
    
    assign(first_col_con, a1, envir = .GlobalEnv)
    assign(second_col_con, a2, envir = .GlobalEnv)  
    
  }
  
  
  svalue(win5_lab14) <<- "Analysis is complete."
  
  
  for (t in fir_col_mat_list){
    s_tab[length(s_tab) + 1] <<- t
  }
  
  for (t in sec_col_mat_list){
    s_tab[length(s_tab) + 1] <<- t
  }
  
  s_tab[length(s_tab) + 1] <<- svalue(win5_con_name1)
  s_tab[length(s_tab) + 1] <<- svalue(win5_con_name2)
  
  s_tab <<- rev(s_tab)
  
})


addHandlerChanged(win5_next_but, function(h,...){
  # dev.off()
  visible(window5) <<- FALSE
  visible(window6) <<- TRUE
  plot(0,type = "n",xlab = "Retention time", ylab = "Intensity", main = "Chromatogram")
  
  prof_combo[] <<- s_tab
  win6_comp_combo[] <<- c(1: svalue(win5_comp_numb))
  
  
})


##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
# Best pure profiles

lyt6 <- glayout(container = window6 , spacing = 5)

lyt6[1:5,1:2,anchor = c(-1, 0) ] <- win6_lab1 <- glabel("Select a data:") 
font(win6_lab1) <- list( weight = "bold", color = "blue" , size  = 10)
lyt6[3 , 5:50] <- prof_combo <- gcombobox(s_tab)

lyt6[1:5 , 55:60, anchor = c(0,0)] <- prof_checkbox <- gcheckbox("view an specified metabolite" , checked = FALSE)
lyt6[1:5 , 62:65 , anchor = c(0,0)] <- win6_lab2 <- glabel("Metabolite:")
font(win6_lab2) <- list( weight = "bold", color = "blue" , size  = 10)
lyt6[3 , 66:84] <- win6_comp_combo <- gcombobox(c(1:100))

lyt6[1:5 , 86:88] <- win6_radio <- gradio(c("View in selected data" , "View in all samples"))

lyt6[1:5 , 94:97 , anchor = c(0,0)] <- win6_lab3 <- glabel("Select profile type:")
font(win6_lab3) <- list( weight = "bold", color = "blue" , size  = 10)
lyt6[3 , 99:200] <- win6_prof_type <- gcombobox(c("Second Col." , "First Col." , "Spectral"))

lyt6[3 , 210:250] <- win6_plot_but2 <- gbutton("Plot")
#size(win6_plot_but2) <- c(30 ,40)
font(win6_plot_but2) <- list( weight = "bold", color = "blue" , size  = 12)

lyt6[6 , 1:17 , anchor = c(0,0)] <- win6_plot_but1 <- gbutton("View data")
lyt6[6 , 21:50, anchor = c(0,0)] <- win6_save_but <- gbutton("Save data")
font(win6_plot_but1) <- list( weight = "bold", color = "blue" )
font(win6_save_but) <- list( weight = "bold", color = "blue" )



lyt6[7:180 , 1:250] <- win6_graphics <- ggraphics()
#plot(0,type = "n",xlab = "Retention time", ylab = "Intensity", main = "Chromatogram")

lyt6[6 , 55:60] <- win6_back_but <- gbutton("Back")

lyt6[6 , 210:250] <- win6_next_but <- gbutton("Next")

addHandlerChanged(win6_back_but , function(h,...){
  visible(window6) <<- FALSE
  dev.off()
  visible(window5) <<- TRUE
  win5_aug_combo[] <<- s_tab
  matplot(0 , main = "Mass spectra" , xlab = "m/z" , ylab = "Intensity")
  
  
})



enabled(win6_lab3) <- FALSE
enabled(win6_prof_type) <- FALSE
enabled(win6_plot_but2) <- FALSE
enabled(win6_comp_combo) <- FALSE
enabled(win6_radio) <- FALSE
enabled(win6_lab2) <- FALSE


addHandlerChanged(prof_checkbox , function(h, ...){
  
  if (svalue(prof_checkbox) == TRUE){
    enabled(win6_lab2) <<- TRUE
    enabled(win6_comp_combo) <<- TRUE
    enabled(win6_radio) <<- TRUE
    enabled(win6_plot_but2) <<- TRUE
  }
  
  if (svalue(prof_checkbox) == FALSE){
    enabled(win6_lab3) <- FALSE
    enabled(win6_prof_type) <- FALSE
    enabled(win6_plot_but2) <- FALSE
    enabled(win6_comp_combo) <- FALSE
    enabled(win6_radio) <- FALSE
    enabled(win6_lab2) <- FALSE
    
  }
  
})


addHandlerChanged(win6_radio , function(h, ...){
  if (svalue(win6_radio) == "View in all samples" ){
    enabled(win6_lab3) <<- TRUE
    enabled(win6_prof_type) <<- TRUE
    
    
  }
  else{
    enabled(win6_lab3) <<- FALSE
    enabled(win6_prof_type) <<- FALSE
    
    
  }
  
})




addHandlerChanged(win6_plot_but1 , function(h, ...){
  
  prof_n <- svalue(prof_combo)
  prof_n_spil <- strsplit(prof_n , "_" ,fixed = TRUE)
  
  if (prof_n_spil[[1]][1] == "sopt"){
    matplot(t(get(svalue(prof_combo))) , type = "l")
    sopt <<- get(svalue(prof_combo))
  }
  else
    matplot(get(svalue(prof_combo)) , type = "l")
  
  
  
})


addHandlerChanged(win6_next_but , function(h,...){
  dev.off()
  visible(window6) <<- FALSE
  visible(window7) <<- TRUE
  plot(0)
  win7_tab1[] <<- sec_col_mat_list
  win7_spin2[] <<- seq(1 , length(sec_col_mat_list) - 1 , 1)
  svalue(win7_spin2) <<- length(sec_col_mat_list ) - 1
  
})

addHandlerChanged(win6_plot_but2 , function(h,...){
  
  
  if (svalue(win6_radio) == "View in selected data" ){
    prof_n <- svalue(prof_combo)
    prof_n_spil <- strsplit(prof_n , "_" ,fixed = TRUE)
    if(prof_n_spil[[1]][1] == "sopt"){
      plot(get(svalue(prof_combo)[1])[svalue(win6_comp_combo ) ,] , type = "l" , add = TRUE,xlab = "m/z" , ylab = "Intensity")
    }else
      plot(get(svalue(prof_combo))[,svalue(win6_comp_combo )] , type = "l", xlab = "Retention time" , ylab = "Intensity")
  }else{
    if (svalue(win6_prof_type) == "Second Col."){
      sec_prof <- c()
      for (i in sec_col_mat_list){
        sec_prof <- cBind(sec_prof , get(i)[,svalue(win6_comp_combo )])
        
      }
      matplot(sec_prof ,type = "l" , xlab = "Retention time" , ylab = "Intensity" , main = "2nd chromatographic column elution profiles")      
    }
    if (svalue(win6_prof_type) == "First Col."){
      fir_prof <- c()
      for (i in fir_col_mat_list){
        fir_prof <- cBind(fir_prof , get(i)[,svalue(win6_comp_combo )], xlab = "Retention time" , ylab = "Intensity",main = "1st chromatographic column elution profiles")
        
      }
      matplot(fir_prof ,type = "l", xlab = "Retention time" , ylab = "Intensity") 
      
    }
    if (svalue(win6_prof_type) == "Spectral"){
      plot(get(svalue(prof_combo)[1])[svalue(win6_comp_combo ) ,] , type = "l" , add = TRUE,xlab = "m/z" , ylab = "Intensity")
      
    }
  }
  
  
  
})



##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
# Classification

lyt7 <- glayout(cont = window7)


lyt7[1 , 21:115 , anchor = c(0,0)] <- win7_lab0 <- glabel("Metabolites classification")

lyt7[1 , 140:160 , anchor = c(0,0)] <- win7_next_but <- gbutton("Next")
lyt7[1 , 5:20 , anchor = c(0,0)] <- win7_back_but <- gbutton("Back")

font(win7_next_but) <- list(weight = "bold", color = "blue" , size = 10)
font(win7_back_but) <- list(weight = "bold", color = "blue" , size = 10)

addHandlerChanged(win7_back_but , function(h,...){
  visible(window7) <<- FALSE
  dev.off()
  visible(window6) <<- TRUE
  plot(0,type = "n",xlab = "Retention time", ylab = "Intensity", main = "Chromatogram")
  
  prof_combo[] <<- s_tab
  win6_comp_combo[] <<- c(1: svalue(win5_comp_numb))
  
})

font(win7_lab0) <- list(weight = "bold", color = "blue" , size = 16)
lyt7[2,1:30 , anchor  = c(0,0)] <- win7_lab1 <- glabel("---------------------- Defining classes ----------------------")
font(win7_lab1) <- list(color = "blue" ,weight = "bold" )

lyt7[3, 2:10, anchor = c(0,0)] <-  win7_lab2 <- glabel("Number of classes:")
lyt7[3 , 11:20, anchor = c(0,0)] <- win7_spin1 <- gspinbutton(from = 1, to = 10, value = 1)
lyt7[4 , 1:10 , anchor = c(-1,0)] <- win7_lab3 <- glabel("Select a class:")
combo_list1 <- c("Class 1" , "Class 2")

lyt7[4 , 11:20] <- win7_combo1 <- gcombobox(combo_list1)




addHandlerChanged(win7_spin1, function(h,...){
  
  combo_list1 <<- c()
  spin_val <<- svalue(win7_spin1)
  
  for (i in c(1:spin_val)){
    combo_list1 <<- append(combo_list1, paste("Class",i,sep = " "))
  }
  
  
  win7_combo1[] <<- combo_list1
})

lyt7[5, 1:20 ,anchor = c(0,0)] <-  win7_lab4 <- glabel("Select samples to creat a class")

win7_tab1_con <- c("sample1", "sample2" , "sample3","sample4","sample5","sample6","sample7","sample8")
lyt7[6:80,2:20, anchor = c(0,0)] <- win7_tab1 <- gtable(win7_tab1_con , multiple = TRUE)
lyt7[3:4 , 21 : 30] <- win7_but1 <- gbutton("Creat class")
font(win7_but1) <- list(color = "blue" ,weight = "bold" , size = 10 )
size(win7_but1) <- c(20,40)
lyt7[5 , 21:30] <- "classes:"

win7_tab2_data <- c(" ")
win7_tab2_ft <- TRUE
lyt7[6:27 , 21:30] <- win7_tab2 <- gtable(win7_tab2_data , multiple = FALSE)
win7_tab2_ft <- TRUE
lyt7[28 , 21:30] <- "Class contens:"
win7_tab3_data <- c(" ")
lyt7[29:74 , 21:30] <- win7_tab3 <- gtable(win7_tab3_data , multiple = FALSE)
lyt7[75:80 , 21:30] <- reset_but <- gbutton("Reset")



lyt7[2 , 31:39] <- win7_lab8 <- glabel("-- Preprocessing --")
font(win7_lab8) <- list(color = "blue" ,weight = "bold" )


lyt7[2,43:132 , anchor  = c(0,0)] <- win7_lab5 <- glabel("--------------------------------------------------------------------------------- classification ---------------------------------------------------------------------------------")
font(win7_lab5) <- list(color = "blue" ,weight = "bold" )
lyt7[3 , 31:39] <- win7_combo2 <- gcombobox(c("Autoscale" , "Mean center", "Scale"))
lyt7[4 , 31:35] <- win7_lab6 <- glabel("# of LVs:")
lyt7[4 , 36:39] <- win7_spin2 <- gspinbutton( from = 1 , to = 100 , step = 1 , value = 1)
lyt7[5:6 , 31:39] <- win7_ini_mod <- gbutton("Classify")
font(win7_ini_mod) <- list(weight = "bold", color = "blue" , size = 11)

lyt7[7:8 , 31:39] <- win7_lab7 <- glabel("Captured variance:")
lyt7[9:27 , 31:39] <- win7_tab4 <- gtable(c(" "))
lyt7[3:74 , 40:160] <- ggraphics()
lyt7[28 , 31:39] <- win7_lab9 <- glabel("Statistics")
font(win7_lab9) <- list(color = "blue" ,weight = "bold" )
lyt7[29:74 , 31:39] <- win7_tab5 <- gtable(c("Model", "XYscores" ,"XScores" , "XVariance" ,"XCumvariance" , "YVariance", "YCumvariance" , "RMSE" , "XResiduals" , "YResiduals" , "Sensivity" , "Specificity" , "Perdiction" , "Performance"))

lyt7[75:80 , 31:39] <- win7_result_but <- gbutton("Plot")



is_first <- TRUE
class_numb <- 1
class_hash <- hash()

addHandlerChanged(win7_but1, function(h,...){
  if (is_first){
    fixed_win7_tab1 <<- win7_tab1[]
    changing_win7_tab1 <<- win7_tab1[]
    y_vector <<- rep(0 , length( win7_tab1[]))
    is_first <<- FALSE
  }
  
  win7_tab1_val <<- svalue(win7_tab1)
  
  for (k in win7_tab1_val){
    class_pos <<- which( fixed_win7_tab1 == k)
    y_vector[class_pos] <<- class_numb
    changing_win7_tab1 <<- changing_win7_tab1[changing_win7_tab1 != k] 
  }
  
  win7_tab1[] <<- changing_win7_tab1
  class_name <<- svalue(win7_combo1)
  
  class_hash[[class_name]] <<- win7_tab1_val
  
  class_numb <<- class_numb + 1
  if (win7_tab2_ft){
    win7_tab2_data <<- c(class_name)
    win7_tab2_ft <<- FALSE
  }
  else{
    win7_tab2_data[length(win7_tab2_data) + 1] <<- class_name
  }
  
  win7_tab2[] <<- win7_tab2_data
  win7_tab3[] <<- class_hash[[class_name]]
  
  
})


addHandlerChanged(win7_tab2 , function(h,...){
  
  #cur_class <<- svalue(win7_tab2)
  win7_tab3[] <<- class_hash[[svalue(win7_tab2)]]
})

addHandlerChanged(reset_but , function(h,...){
  
  win7_tab1[] <<- fixed_win7_tab1
  win7_tab2[] <<- c(" ")
  win7_tab3[] <<- c(" ")
  is_first <<- TRUE
  win7_tab2_ft <<- TRUE
  
})






win7_is_first <- TRUE


addHandlerChanged(win7_ini_mod , function(h,...){
  
  
  sec_col_are <- c()
  for (j in sec_col_mat_list){
    cur_col_area <- colSums(get(j))
    sec_col_are <- rBind(sec_col_are , cur_col_area)
  }
  
  x_class <<- matrix( sec_col_are , nrow = length(sec_col_mat_list))
  y_class <<- y_vector
  
  center_stat <- FALSE
  scale_stat <- FALSE
  
  if(svalue(win7_combo2) == "Autoscale"){
    center_stat <- TRUE
    scale_stat <- TRUE
  }
  
  if(svalue(win7_combo2) == "Mean center"){
    center_stat <- TRUE
    scale_stat <- FALSE
  } 
  
  if(svalue(win7_combo2) == "Scale"){
    center_stat <- FALSE
    scale_stat <- TRUE
  } 
  
  da_ans <<-plsda(scale(x_class,center = center_stat, scale = scale_stat),as.vector(y_class),scale = FALSE,ncomp = svalue(win7_spin2))
  #plotIndiv(da_ans, ind.names = TRUE, ellipse = TRUE, legend = TRUE)
  da_vip <<- vip(da_ans)
  da_vip_col_1 <<- matrix(da_vip)
  vip_list <- c()
  vip_col <- c()
  count <- 1
  for(i in da_vip_col_1){
    if (i > 1 ){
      vip_list = append(vip_list,count)
      vip_col = append(vip_col,"blue")
    }
    else
      vip_col = append(vip_col,"gray")
    count <- count + 1
  }
  win7_tab4[] <<- da_ans$explained_variance[[1]]
  plotIndiv(da_ans, ind.names = TRUE, ellipse = TRUE, legend = TRUE,col = c(2,4),probe = TRUE, ,cex = 5)
  
  #plotVar(da_ans)
  #barplot(height =  da_vip_col_1,
  #        beside = TRUE, col = vip_col ,
  #        ylim = c(0, 1.1), axes = TRUE,space = 0.2,sub = "VIP plot for Centered & Scaled PLS-DA --  Column 1 ", names.arg=seq(1,20)
  #        , cex.names = 0.8,font = 1)
  #print(vip_list)
  
  
})



#lyt7[46 , 31:42] <- win7_combo3 <- gcombobox("Bi plot")


#sec_col_are <- c()
#for (j in sec_col_mat_list){
#  cur_col_area <- colSums(get(j))
#  sec_col_are <- rBind(sec_col_are , cur_col_area)
#}



addHandlerChanged(win7_result_but , function(h,...){
  detach("package:mixOmics", unload=TRUE)
  require(mdatools)
  
  
  
  center_stat <- FALSE
  scale_stat <- FALSE
  
  if(svalue(win7_combo2) == "Autoscale"){
    center_stat <- TRUE
    scale_stat <- TRUE
  }
  
  if(svalue(win7_combo2) == "Mean center"){
    center_stat <- TRUE
    scale_stat <- FALSE
  } 
  
  if(svalue(win7_combo2) == "Scale"){
    center_stat <- FALSE
    scale_stat <- TRUE
  } 
  
  
  y_class2 <<- c()
  for (h in y_class){
    y_class2[length(y_class2) + 1] <<- toString(h)
  }
  mda_classify <- plsda( scale(x_class,center = center_stat, scale = scale_stat), y_class2 , ncomp = svalue(win7_spin2) )
  if (svalue(win7_tab5) == "Model")
    plot(mda_classify)
  if (svalue(win7_tab5) == "XYscores")
    plotXYScores(mda_classify)
  if (svalue(win7_tab5) == "Xscores")
    plotXScores(mda_classify)
  if (svalue(win7_tab5) == "XVariance")
    plotXVariance(mda_classify)
  if (svalue(win7_tab5) == "XCumvariance")
    plotXCumVariance(mda_classify)
  if (svalue(win7_tab5) == "YVariance")
    plotYVariance(mda_classify)
  if (svalue(win7_tab5) == "YCumvariance")
    plotYCumVariance(mda_classify)
  if (svalue(win7_tab5) == "RMSE")
    plotRMSE(mda_classify)
  if (svalue(win7_tab5) == "XResiduals")
    plotXResiduals(mda_classify)
  if (svalue(win7_tab5) == "YResiduals")
    plotYResiduals(mda_classify)
  if (svalue(win7_tab5) == "Sensivity")
    plotSensitivity(mda_classify)
  if (svalue(win7_tab5) == "Specificity")
    plotSpecificity(mda_classify)
  if (svalue(win7_tab5) == "Perdiction")
    plotPredictions(mda_classify)
  if (svalue(win7_tab5) == "Performance")
    plotPerformance(mda_classify)
  
  detach("package:mdatools", unload=TRUE)
  require(mixOmics)
})

addHandlerChanged(win7_next_but , function(h,...){
  dev.off()
  visible(window7) <<- FALSE
  visible(window8) <<- TRUE
  plot(0 , main = "VIP plot")
})

##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
# Best Identi

lyt8 <- glayout(cont = window8)

lyt8[1 , 2:20] <- win8_back_but <- gbutton("Back")
lyt8[1 , 160:180] <- win8_next_but <- gbutton("Main menu")

addHandlerChanged(win8_back_but, function(h,...){
  visible(window8) <<- FALSE
  dev.off()
  visible(window7) <<- TRUE
  plot(0)
  win7_tab1[] <<- sec_col_mat_list
  win7_spin2[] <<- seq(1 , length(sec_col_mat_list) - 1 , 1)
  svalue(win7_spin2) <<- length(sec_col_mat_list ) - 1
  
  
})

lyt8[1 , 21:159 , anchor = c(0,0)] <- win8_lab0 <- glabel("Identification of important metabolites")
font(win8_lab0) <- list(weight = "bold" , color = "blue" , size = 16)
lyt8[2:10 , 2:22] <- win8_but1 <- gbutton("View")
font(win8_but1) <- list(weight = "bold" , color = "blue" , size = 10)
lyt8[11:12 , 2:22] <- win8_lab1 <- glabel("Important metabolites")
font(win8_lab1) <- list(weight = "bold" , color = "blue" , size = 10)
lyt8[13:67 , 2:22] <- win8_tab1 <- gtable(c(" "))
lyt8[68:71 , 2:22] <- win8_lab2 <- glabel("Peak selection criterion:")
font(win8_lab2) <- list(weight = "bold" , color = "blue" , size = 10)

#lyt8[54:55 , 2:16] <- win8_lab3 <- glabel("(Peak INT./Maximum INT) > :")
lyt8[72:75 , 2:16] <- win8_lab3 <- glabel("Normalized Peak Intensity > :")

lyt8[76:79 , 2:22] <- win8_gedit <- gedit(text = "0.01")
lyt8[80:90 , 2:22] <- win8_but2 <- gbutton("Export spectra to NIST")
font(win8_but2) <- list(weight = "bold" , color = "blue" , size = 10)

lyt8[2:90 , 23: 180] <- ggraphics()

addHandlerChanged(win8_but1 , function(h,...){
  sec_col_are <- c()
  for (j in sec_col_mat_list){
    cur_col_area <- colSums(get(j))
    sec_col_are <- rBind(sec_col_are , cur_col_area)
  }
  
  x_class <<- matrix( sec_col_are , nrow = length(sec_col_mat_list))
  y_class <<- y_vector
  
  
  if(svalue(win7_combo2) == "Autoscale"){
    center_stat <<- TRUE
    scale_stat <<- TRUE
  }
  
  if(svalue(win7_combo2) == "Mean center"){
    center_stat <<- TRUE
    scale_stat <<- FALSE
  } 
  
  if(svalue(win7_combo2) == "Scale"){
    center_stat <<- FALSE
    scale_stat <<- TRUE
  } 
  lv_num <<- svalue(win7_spin2)
  da_ans <<-plsda(scale(x_class,center = center_stat, scale = scale_stat),as.vector(y_class),scale = FALSE,ncomp = lv_num )
  #plotIndiv(da_ans, ind.names = TRUE, ellipse = TRUE, legend = TRUE)
  da_vip <<- vip(da_ans)
  da_vip_col_1 <<- matrix(da_vip)
  vip_list <<- c()
  vip_col <- c()
  count <- 1
  for(i in da_vip[,lv_num]){
    if (i > 1 ){
      vip_list = append(vip_list,count)
      vip_col = append(vip_col,"blue")
    }
    else
      vip_col = append(vip_col,"gray")
    count <- count + 1
  }
  win7_tab4[] <<- da_ans$explained_variance[[1]]
  #plotIndiv(da_ans, ind.names = TRUE, ellipse = TRUE, legend = TRUE,col = c(2,4),probe = TRUE, ,cex = 5)
  
  
  barplot(height =  da_vip[,lv_num],
          beside = TRUE, col = vip_col , axes = TRUE,space = 0.2,xlab =   "Metabolite", ylab = "VIP score", main = "VIP scores plot", names.arg=seq(1,dim(da_vip)[1])
          , cex.names = 0.8,font = 1)
  abline(a = 1 ,b = 0, col = "red" , lwd = 3 , add = TRUE)
  #matplot(rep(1,dim(da_vip)[1]), type = "l" ,add = TRUE, col = "red",lwd = 2)
  win8_tab1[] <<- vip_list
  vip_list <<- vip_list
})


addHandlerChanged(win8_but2, function(h,...){
  
  nist_dir <<- gfile(type = c("selectdir"))
  #s_thresh <- svalue()
  s_thresh <- 0.01
  counter <- 0
  for(i in vip_list){
    text_path <- paste(nist_dir,"\\Metabolite_",i,".txt" , sep = "")
    j_list <- c()
    int_list <- c()
    sopt <- get(svalue(win5_con_name2))
    cur_sopt <<- sopt[i,]
    max_cur_sopt <- max(cur_sopt)
    for (j in c(1:dim(sopt)[2])){
      if (sopt[i,j] >= (s_thresh *max_cur_sopt)){
        j_list <- append(j_list , j)
        int_list <- append(int_list ,sopt[i,j] / max_cur_sopt )
        counter <- counter + 1
      }
    }
    
    sink(text_path)
    cat("Name:")
    cat("\n")
    cat("Formula:")
    cat("\n")
    cat("Num Peaks: ")
    cat(counter)
    cat("\n")
    for(k in c(1:length(j_list))){
      cat(j_list[k])
      cat(" ")
      cat(int_list[k])
      cat("\n")
    }
    sink()
  }
  
})

while(TRUE){
  xxyy <- 1
}




