# LEWIE-Lite shiny app  
# Summer 2023
# Original code by Mateusz Filipski
# Edited by Parth Chawla

library(shinydashboard)
library(tidyverse)
library(scales)
library(googlesheets4)
library(readxl)
library(ggplot2)
library(RColorBrewer)
library(markdown)
library(shinyjs)
library(emojifont)
# library(xlsx)



###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
################################ Preliminaries ################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################


# -----------------------------------------------------------------------------------------------------
# -----------------------------------------------------------------------------------------------------
# DEFINE THE SOURCE FILE FOR ALL THE DATA: EITHER GOOGLE SHEETS or FROM LOCAL FILE (unstar only one line at a time):
# -----------------------------------------------------------------------------------------------------
# -----------------------------------------------------------------------------------------------------
# sheet_location = "online"; sheet_path = "https://docs.google.com/spreadsheets/d/1bhuOwJv4b6DttBXEx2lI7uZk6WL0l9uio2e4dcco-F8/edit?usp=share_link"
sheet_location = "local"; sheet_path = "Madagascar-Ranomafana/LEWIE-Lite_NewInput_v15_Ranomafana.xlsx"
# -----------------------------------------------------------------------------------------------------
# -----------------------------------------------------------------------------------------------------




###############################################################################
###############################################################################
###############################################################################



## Make the data structures we need for SAM output
###############################################################################
sam_row_names <- c("Ag", "Tourism", "Nag", "Fish", "LMUSK", "LMSK", "LFUSK", "LFSK", "K", "Poor", "NonPoor", "Restaurants", 
                   "Lodges", "Tourists", "PA", "ComRevSh", "LocalG", "G", "ROW", "TotalExp")

make_samcol <- function(name, rownames){
    namevec <- rep(0, length(rownames))
    samcol <- as.data.frame(namevec)
    row.names(samcol) <- rownames
    colnames(samcol) <- as.character(substitute(name))
    samcol
}

reorder_sam_rows <- function(sam, names){
    sam_temp1 <- sam[match(names, sam$rowname),]
    rownames(sam_temp1) <- sam_temp1$rowname
    sam_temp2 <- sam_temp1[,-1]
    rownames(sam_temp2) <- sam_temp1[,1]
    sam_temp2
}

check_balance = function(M){
    rows = rowSums(M)
    cols = colSums(M)
    diffs = round(abs(rows-cols)/cols*100, 3)
    diffs
}

ras_oneiter = function(M, method = "mean"){ 
    y = colSums(M)
    x = rowSums(M)
    # A <- make_colshares_A(M)
    bal0 = check_balance(M)
    
    # Target row/col (here they are same)
    if (method=="mean"){
        xihat = (x+y)/2
        xjhat = (x+y)/2
    }
    if (method=="x"){
        xihat = x
        xjhat = x
    }
    if (method=="y"){
        xihat = y
        xjhat = y
    }
    diff = xihat - xjhat 
    # message('target diff')
    # print(diff)
    
    # Update the row: 
    ai1 = xihat / rowSums(M)
    # x1 <- ai1 * M
    # Use the sweep function to multiply matrix by vector along dim1
    x1 = sweep(M, MARGIN=1, ai1, `*`)
    bal1 = check_balance(x1)
    
    
    # display_check(ai1)
    # display_check(M)
    # display_check(x1)
    # display_check(x1sweep)
    # display_check(bal1)
    
    # Update the column: 
    bj1 = xjhat / colSums(x1)
    # Use the sweep function to multiply matrix by vector along dim2
    x2 = sweep(x1, MARGIN=2, bj1, `*`)
    bal2 = check_balance(x2)
    
    # display_check(bj1)
    # display_check(x1)
    # display_check(x2)
    # display_check(x2sweep)
    # display_check(bal2)
    
    # some checks
    # message('balance 0')
    # print(bal0)
    # message('balance 1')
    # print(bal1)
    # message('balance 2')
    # print(bal2)
    # 
    # Return x2
    x2
}

ras_loop_x = function(M, criterion){
    # Remove the last row and column
    M2 = M[-nrow(M), -ncol(M)]
    balance_vec = check_balance(M2)
    balance = sum(balance_vec)
    i = 1
    print(i) ; print(balance)
    while ((balance > criterion) & i < 50) {
        # CHOOSING THE METHOD MAKES A BIG DIFFERENCE IN THE RASING OUTCOME 'mean' vs 'x' will equate either to the mean of rows and cols or just to the col totals
        Mprime <- ras_oneiter(M2, method="x")
        balance_vec = check_balance(Mprime)
        balance = sum(balance_vec)
        M2 = Mprime
        i = i+1
        # print(i) ; print(balance)
    }
    # print(balance)
    message(paste0('MESSAGE: balanced the sam in ', i, ' iterations; diff = ', balance))
    # Mprime 
    
    # Put the row totals and col totals back in: 
    M3 <- M2
    M3["TotalExp",] <- colSums(M2)
    M3[,"TotalExp"] <- rowSums(M3)
    M3
}


# Function to calculate labor revenue shares between poor vs non-poor:  
make_labshares_poor = function(pop_poor, inc_poor, musk_poor, msk_poor, fusk_poor, fsk_poor, incprof_poor, increm_poor,
                               pop_nonpoor, inc_nonpoor, musk_nonpoor, msk_nonpoor, fusk_nonpoor, fsk_nonpoor, incprof_nonpoor, increm_nonpoor){
    suminc_divshares_poor = pop_poor * inc_poor / (musk_poor + msk_poor + fusk_poor + fsk_poor + incprof_poor + increm_poor)
    suminc_divshares_nonpoor = pop_nonpoor * inc_nonpoor / (musk_nonpoor + msk_nonpoor + fusk_nonpoor + fsk_nonpoor + incprof_nonpoor + increm_nonpoor)
    
    share_musk_poor = musk_poor * suminc_divshares_poor / 
        (musk_poor * suminc_divshares_poor + musk_nonpoor * suminc_divshares_nonpoor)
    
    share_msk_poor = msk_poor * suminc_divshares_poor / 
        (msk_poor * suminc_divshares_poor + msk_nonpoor * suminc_divshares_nonpoor)
    
    share_fusk_poor = fusk_poor * suminc_divshares_poor / 
        (fusk_poor * suminc_divshares_poor + fusk_nonpoor * suminc_divshares_nonpoor)
    
    share_fsk_poor = fsk_poor * suminc_divshares_poor / 
        (fsk_poor * suminc_divshares_poor + fsk_nonpoor * suminc_divshares_nonpoor)
    outvect = c(share_musk_poor, share_msk_poor, share_fusk_poor, share_fsk_poor)
    outvect
}


# Function to rescale the factor shares of a given activity, accounting for profit margin
# -> never mind, too tricky to deal with the input$ prefix. 


# Make all the columns for the sam (blank for now, get filled below)
sam_ag <- make_samcol(Ag, sam_row_names)
sam_nag <- make_samcol(Nag, sam_row_names)
sam_tourism <- make_samcol(Tourism, sam_row_names)
sam_fish <- make_samcol(Fish, sam_row_names)
sam_LMUSK <- make_samcol(LMUSK, sam_row_names)
sam_LMSK <- make_samcol(LMSK, sam_row_names)
sam_LFUSK <- make_samcol(LFUSK, sam_row_names)
sam_LFSK <- make_samcol(LFSK, sam_row_names)
sam_K <- make_samcol(K, sam_row_names)
sam_hhp <- make_samcol(Poor, sam_row_names)
sam_hhnp <- make_samcol(NonPoor, sam_row_names)
sam_restaurants <- make_samcol(Restaurants, sam_row_names)
sam_lodges <- make_samcol(Lodges, sam_row_names)
sam_tourists <- make_samcol(Tourists, sam_row_names)
sam_pa <- make_samcol(PA, sam_row_names)
sam_comrevsh <- make_samcol(ComRevSh, sam_row_names)
sam_G <- make_samcol(G, sam_row_names)
sam_ROW <- make_samcol(ROW, sam_row_names)
sam_totalExp <- make_samcol(TotalExp, sam_row_names)
### 






###############################################################################
## Load all the Questions and Default Values from Google Sheets
###############################################################################

# Function to get input either locally or from an input sheet on google drive
get_input_online_or_local <- function(address, sheet, range, mode = "online"){ 
    if (mode == "online"){
        print("reading online")
        # out = "nothing"
        out = read_sheet(ss = address, sheet = sheet, range = range)
    } else if (mode == "local") {
        out = read_excel(path = address, sheet = sheet, range = range)   
        print("reading locally")
    } else {
        print("must specify mode = 'online' or mode = 'local' (the default)")
        out = "error"
    }
    out
} 



# -----------------------------------------------------------------------------------------------------
## This loads all the values.  The source is defined at the very top of the file. - 
###############################################################################

## Note that any change in the spreadsheet layout needs to be reflected here in the Excel ranges below. 

# Disable authentication: 
gs4_deauth()

# Tourists:
gdata_touristStats    = get_input_online_or_local(sheet_path, sheet = "Tourists", range = "B2:G7", mode = sheet_location)
gdata_touristShares   = get_input_online_or_local(sheet_path, sheet = "Tourists", range = "B11:G17", mode = sheet_location)
# Ag:
gdata_AgShares      = get_input_online_or_local(sheet_path, sheet = "Ag", range = "B53:G68", mode = sheet_location)
# NonAg:
gdata_NagShares     = get_input_online_or_local(sheet_path, sheet = "Nonag", range = "B53:G68", mode = sheet_location)
# Tourism businesses:
gdata_TourismShares = get_input_online_or_local(sheet_path, sheet = "Tourism", range = "B53:G68", mode = sheet_location)
# Fish:
gdata_FishShares    = get_input_online_or_local(sheet_path, sheet = "Fishing", range = "B53:G68", mode = sheet_location)
# Restaurants:
gdata_RestShares    = get_input_online_or_local(sheet_path, sheet = "Restaurants", range = "B53:G68", mode = sheet_location)
# Lodges:
gdata_LodgesShares    = get_input_online_or_local(sheet_path, sheet = "Lodges", range = "B53:G68", mode = sheet_location)
# NatParks:
gdata_NPName        = get_input_online_or_local(sheet_path, sheet = "NatParkPA", range = "B54:D55", mode = sheet_location)
gdata_NPBudget      = get_input_online_or_local(sheet_path, sheet = "NatParkPA", range = "B58:G60", mode = sheet_location)
gdata_NPSpending    = get_input_online_or_local(sheet_path, sheet = "NatParkPA", range = "B63:G76", mode = sheet_location)

# Com Rev Sh.:
gdata_ComRevShShares    = get_input_online_or_local(sheet_path, sheet = "ComRevSh", range = "B58:G69", mode = sheet_location)

# Households (split into 2 tabs):
gdata_HHPc1 = get_input_online_or_local(sheet_path, sheet = "Households", range =  "B54:G58", mode = sheet_location)
gdata_HHPc2 = get_input_online_or_local(sheet_path, sheet = "Households", range =  "B59:G65", mode = sheet_location)
gdata_HHPc3 = get_input_online_or_local(sheet_path, sheet = "Households", range =  "B67:G80", mode = sheet_location)
gdata_HHNPc1 = get_input_online_or_local(sheet_path, sheet = "Households", range = "I54:N58", mode = sheet_location)
gdata_HHNPc2 = get_input_online_or_local(sheet_path, sheet = "Households", range = "I59:N65", mode = sheet_location)
gdata_HHNPc3 = get_input_online_or_local(sheet_path, sheet = "Households", range = "I67:N80", mode = sheet_location)



# Transform all the questions into Shiny Inputs:
#--------------------------------------------------
# This function will create numeric input fields based on the tables we import
myfunc_CreateNumericInput <- function(id, label = label, value=value, min = 0, max = 1, step = step) {
    numericInput(id, label, min = min, max = max, value = value, step = 0.1)
}

# These are shiny input tables (using purr mapping based on the function above)
inp_touristsStats <- pmap(gdata_touristStats, myfunc_CreateNumericInput)
inp_touristsShares <- pmap(gdata_touristShares, myfunc_CreateNumericInput)

inp_agShares <- pmap(gdata_AgShares, myfunc_CreateNumericInput)
inp_NagShares <- pmap(gdata_NagShares, myfunc_CreateNumericInput)
inp_TourismShares <- pmap(gdata_TourismShares, myfunc_CreateNumericInput)
gdata_NagShares
gdata_FishShares

inp_FishShares <- pmap(gdata_FishShares, myfunc_CreateNumericInput)

inp_RestShares <- pmap(gdata_RestShares, myfunc_CreateNumericInput)
gdata_RestShares
inp_LodgesShares <- pmap(gdata_LodgesShares, myfunc_CreateNumericInput)
inp_ComRevShShares <- pmap(gdata_ComRevShShares, myfunc_CreateNumericInput)

inp_NPBudget <- pmap(gdata_NPBudget, myfunc_CreateNumericInput)
inp_NPSpending <- pmap(gdata_NPSpending, myfunc_CreateNumericInput)


inp_HHPc1  <- pmap(gdata_HHPc1, myfunc_CreateNumericInput)
gdata_HHNPc1

inp_HHPc2  <- pmap(gdata_HHPc2, myfunc_CreateNumericInput)
inp_HHPc3  <- pmap(gdata_HHPc3, myfunc_CreateNumericInput)
inp_HHNPc1 <- pmap(gdata_HHNPc1, myfunc_CreateNumericInput)
inp_HHNPc2 <- pmap(gdata_HHNPc2, myfunc_CreateNumericInput)
inp_HHNPc3 <- pmap(gdata_HHNPc3, myfunc_CreateNumericInput)

# Now create the input fields for making "simulations":
# ------------------------------------------------------
# the names should match the intended SAM columns 
inp_sim_TouristSpending <- numericInput("sim_TouristSpending", "How much tourist spending ($) do you want to simulate?", min = 0, max = 10000000, value = 100, step = 0.01)
inp_sim_PASpending <- numericInput("sim_PASpending", "How much park spending ($) do you want to simulate?", min = 0, max = 10000000, value = 100, step = 0.01)
inp_sim_ComRevShSpending <- numericInput("sim_ComRevShSpending", "How much community spending ($) do you want to simulate?", min = 0, max = 10000000, value = 100, step = 0.01)
inp_sim_AgSpending <- numericInput("sim_AgSpending", "How much increase in local agricultural production ($) do you want to simulate?", min = 0, max = 10000000, value = 100, step = 0.01)
inp_sim_NagSpending <- numericInput("sim_NagSpending", "How much increase in local non-agricultural production ($) do you want to simulate?", min = 0, max = 10000000, value = 100, step = 0.01)
inp_sim_LFUSKSpending <- numericInput("sim_LFUSKSpending", "How much increase in earnings of low-skilled female workers ($) do you want to simulate?", min = 0, max = 10000000, value = 100, step = 0.01)
inp_sim_LMUSKSpending <- numericInput("sim_LMUSKSpending", "How much in earnings of low-skilled male workers ($) do you want to simulate?", min = 0, max = 10000000, value = 100, step = 0.01)
inp_sim_LFSKSpending <- numericInput("sim_LFSKSpending", "How much in earnings of skilled female workers ($) do you want to simulate?", min = 0, max = 10000000, value = 100, step = 0.01)
inp_sim_LMSKSpending <- numericInput("sim_LMSKSpending", "How much in earnings of skilled male workers ($) do you want to simulate?", min = 0, max = 10000000, value = 100, step = 0.01)

