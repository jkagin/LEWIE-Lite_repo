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

# library(xlsx) -- This library crashed my mac every time.  If you can get it to work, it can help to design nice reports. 



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
# If your sheet is a google sheet, use the first line (leave "online" as the location)
# If your sheet is stored locally, put the name in the second line (usually no need for full path because shiny apps use relative path) (leave "local" as the location)

# sheet_location = "online"; sheet_path = "https://docs.google.com/spreadsheets/d/1bhuOwJv4b6DttBXEx2lI7uZk6WL0l9uio2e4dcco-F8/edit?usp=share_link"
sheet_location = "local"; sheet_path = "LEWIE-Lite_NewInput_v19.xlsx"
# -----------------------------------------------------------------------------------------------------
# -----------------------------------------------------------------------------------------------------




###############################################################################
###############################################################################
###############################################################################



## Make the data structures we need for SAM output
###############################################################################

# These are the names of the SAM accounts, in the same order we put them in the excel
sam_row_names <- c("Ag", "Tourism", "Nag", "Fish", "LMUSK", "LMSK", "LFUSK", "LFSK", "K", "Poor", "NonPoor", "Restaurants", 
                   "Lodges", "Tourists", "PA", "ComRevSh", "LocalG", "G", "ROW", "TotalExp")

# This function creates a sam column with the right names:
make_samcol <- function(name, rownames){
    namevec <- rep(0, length(rownames))
    samcol <- as.data.frame(namevec)
    row.names(samcol) <- rownames
    colnames(samcol) <- as.character(substitute(name))
    samcol
}

# This one reorders rows in a sam
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

# This makes one iteration of the SAM-balancing RAS algorithm.  We will loop over this to balance the SAM
# The input is a SAM, and the output is the same SAM after one iteration of bi-proportional rescaling
ras_oneiter = function(M, method = "mean"){ 
    y = colSums(M)
    x = rowSums(M)
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
    # print(diff)
    
    # Update the row: 
    ai1 = xihat / rowSums(M)

    # Use the sweep function to multiply matrix by vector along dim1
    x1 = sweep(M, MARGIN=1, ai1, `*`)
    bal1 = check_balance(x1)
    
    # Update the column: 
    bj1 = xjhat / colSums(x1)
    # Use the sweep function to multiply matrix by vector along dim2
    x2 = sweep(x1, MARGIN=2, bj1, `*`)
    bal2 = check_balance(x2)
    
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

# This makes a loop of many iterations of the previous function
# The input is a SAM, and the output is a balanced SAM 
# (where balanced means the sum of row-column totals differences is smaller than the criterion)
ras_loop_x = function(M, criterion){
    # Remove the last row and column
    M2 = M[-nrow(M), -ncol(M)]
    balance_vec = check_balance(M2)
    balance = sum(balance_vec)
    i = 1
    print(i) ; print(balance)
    while ((balance > criterion) & i < 50) {
        # THERE ARE MORE THAN ONE WAY TO RAS A MATRIX
        # Choosing the method can make a big difference in the outcome of RAS. 
        # 'mean' vs 'x' will equate either to the mean of rows and cols or just to the col totals
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
sam_localG <- make_samcol(LocalG, sam_row_names)
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
## This loads all the values.  The path to the source is defined at the very top of this code file. - 
#######################################################################################################

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

# Local Government:
gdata_LocalG        = get_input_online_or_local(sheet_path, sheet = "Localgov", range = "B61:D74", mode = sheet_location)
gdata_LocalGBudget      = get_input_online_or_local(sheet_path, sheet = "Localgov", range = "B56:G57", mode = sheet_location)

# Households (split into 2 tabs):
gdata_HHPc1 = get_input_online_or_local(sheet_path, sheet = "Households", range =  "B54:G58", mode = sheet_location)
gdata_HHPc2 = get_input_online_or_local(sheet_path, sheet = "Households", range =  "B59:G65", mode = sheet_location)
gdata_HHPc3 = get_input_online_or_local(sheet_path, sheet = "Households", range =  "B67:G80", mode = sheet_location)
gdata_HHNPc1 = get_input_online_or_local(sheet_path, sheet = "Households", range = "I54:N58", mode = sheet_location)
gdata_HHNPc2 = get_input_online_or_local(sheet_path, sheet = "Households", range = "I59:N65", mode = sheet_location)
gdata_HHNPc3 = get_input_online_or_local(sheet_path, sheet = "Households", range = "I67:N80", mode = sheet_location)



# Transform all the questions into Shiny Inputs:
#--------------------------------------------------
# All these "inp_" parameters are tables that get turned into the tabs of the "Data" page in the app.
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
inp_FishShares <- pmap(gdata_FishShares, myfunc_CreateNumericInput)
inp_RestShares <- pmap(gdata_RestShares, myfunc_CreateNumericInput)
inp_LodgesShares <- pmap(gdata_LodgesShares, myfunc_CreateNumericInput)
inp_ComRevShShares <- pmap(gdata_ComRevShShares, myfunc_CreateNumericInput)

inp_NPBudget <- pmap(gdata_NPBudget, myfunc_CreateNumericInput)
inp_NPSpending <- pmap(gdata_NPSpending, myfunc_CreateNumericInput)

inp_LocalGBudget <- pmap(gdata_LocalGBudget, myfunc_CreateNumericInput)
inp_LocalG <- pmap(gdata_LocalG, myfunc_CreateNumericInput)

inp_HHPc1  <- pmap(gdata_HHPc1, myfunc_CreateNumericInput)
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
inp_sim_LocalGSpending <- numericInput("sim_LocalGSpending", "How much in additional local government spending ($) do you want to simulate?", min = 0, max = 10000000, value = 100, step = 0.01)


# Everything above this line is code needed for both the "UI" (user interface) and "SERVER" parts of the code.
# Everything below this line falls either under UI or SERVER 



###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
################################ UI ###########################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################

# The UI section creates the user interface. 
# There are no calculations made here.  Just arranging the elements for display on the page. 
# The "SERVER" part of the dashboard code is located further down. 

ui <- dashboardPage(
    # %%%%%%%%%%%%%%%%% Header %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    dashboardHeader(title = "LEWIE-lite Dashboard",
                    titleWidth = 250),
    
    # %%%%%%%%%%%%%%%%% SideBar %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    dashboardSidebar(
        collapsed = T,
        sidebarMenu(
            # Note: Create a "Welcome" tab to the top once I'm done coding, or move Dashboard up. 
            menuItem("Home",
                     tabName = "home",
                     icon = icon("home")
            ),
            menuItem("Simulations",
                     tabName = "simulations",
                     icon = icon("dashboard")
            ),
            menuItem("Data",
                     tabName = "data",
                     icon = icon("table")
            ),
            menuItem("SAMs", 
                     tabName = "sams",
                     icon = icon("table")
            ),
            menuItem("FAQ",
                     tabName = "faq",
                     icon = icon("book")
            )
        )
    ),
    # %%%%%%%%%%%%%%%%% Body %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    dashboardBody(
        tabItems(
            # ============================ Instructions Page ======================================
            tabItem("faq", 
                    fluidPage(
                        uiOutput("doc_to_display")
                    )
            ),
            # ============================ Data Page: (Where you can change the SAM) ======================================
            tabItem("data",
                    p("This page is where you see the data inputs", style = "font-size:25px"),
                    p("Data on this page is read directly from an input spreadsheet (Excel or Google Sheets).  If you were granted access to that spreadsheet, use it to guide your exploration of the data. 
                  If you were not given access to that spreadsheet, then this tab will have limited use for you.", style = "font-size:16px"),
                    p("Nevertheless, you can change these inputs to explore what happens to the multipliers - but those changes cannot be saved.", style = "font-size:16px"),
                    tabBox(
                        title = "Data Inputs", width = 12,
                        tabPanel("Tourists",
                                 fluidRow(
                                     column(4, inp_touristsStats), 
                                     column(4, inp_touristsShares)
                                 )
                        ),
                        tabPanel("Ag",
                                 fluidRow(
                                     column(8, inp_agShares)
                                 )
                        ),
                        tabPanel("NonAg",
                                 fluidRow(
                                     column(8, inp_NagShares)
                                 )
                        ),
                        tabPanel("Tourism",
                                 fluidRow(
                                     column(8, inp_TourismShares)
                                 )
                        ),
                        tabPanel("Fishing",
                                 fluidRow(
                                     column(8, inp_FishShares)
                                 )
                        ),
                        tabPanel("Restaurant",
                                 fluidRow(
                                     column(8, inp_RestShares)
                                 )
                        ),
                        tabPanel("Lodges",
                                 fluidRow(
                                     column(8, inp_LodgesShares)
                                 )
                        ),
                        tabPanel("NatParkPA",
                                 fluidRow(
                                     column(4, inp_NPBudget),
                                     column(4, inp_NPSpending)
                                 )
                        ), 
                        tabPanel("ComRevSh",
                                 fluidRow(
                                     column(8, inp_ComRevShShares)
                                 )
                        ), 
                        tabPanel("LocalGov",
                                 fluidRow(
                                     column(4, inp_LocalGBudget),
                                     column(4, inp_LocalG)
                                 )
                        ), 
                        tabPanel("Poor HH",
                                 fluidRow(
                                     column(4, inp_HHPc1),
                                     column(4, inp_HHPc2),
                                     column(4, inp_HHPc3)
                                 )
                        ),
                        tabPanel("Non-Poor HH",
                                 fluidRow(
                                     column(4, inp_HHNPc1),
                                     column(4, inp_HHNPc2),
                                     column(4, inp_HHNPc3)
                                 )
                        )
                    )
            ),
            
            
            # ============================ SAMS Page ======================================
            tabItem("sams", 
                    p("All the relevant matrices are here:", style = "font-size:25px"), 
                    fluidRow(
                        tabBox(width = 12,
                               title = ("SAMs etc."),
                               tabPanel("Multipliers", tableOutput("mult")),
                               tabPanel("Shares", tableOutput("matA")),
                               tabPanel("SAM_RASed", tableOutput("sam_RASed")),
                               tabPanel("SAM_preRAS", tableOutput("sam_preRAS")),
                               tabPanel("Hide the tables")
                        )
                    )
            ),
            # ============================ HOME Page with some key information  ======================================
            tabItem("home",
                    fluidPage(
                        h1(textOutput("park_name")),
                        p("Welcome to the LEWIE-Lite dashboard. This tool allows you to explore the impacts of tourism in Protected Areas on the local economy surrounding them.", style = "font-size:18px"),
                        p("On this page, you can see some general information about tourism in the Protected Area being studied. At the top, some basic statistics regarding the numbers of tourists and park revenues, 
                    along with some simple scenario calculations. 
                      Scrolling down, you will find the local-economy multipliers calculated using the LEWIE-Lite model.", style = "font-size:18px"),
                        p("If you open the side-bar menu, you will find a number of useful tabs.  Most importantly, the Simulations tab offers a range of options to simulate the local-economy impacts of tourism spending.", style = "font-size:18px"),
                        fluidRow(
                            box(width = 12, title = span("Overview of tourism in the park:", style = 'font-size:27px;color:rgb(63, 144, 210);'),
                                p("Total tourists coming to the park (annually):", style = "font-size:22px"),
                                # Tourists, multi-day tourists, length of stay, avg spending
                                # Note: the value boxes are defined in the server part of the code, with other calculations
                                fluidRow( 
                                    column(3, valueBoxOutput("valueBox_numtour", width = 12)),
                                    column(9)
                                ),
                                p("Details on arrivals in the park:", style = "font-size:22px"),
                                # Single-day, multi-day tourists, length of stay, avg spending
                                valueBoxOutput("valueBox_numtourSingle", width = 3),
                                valueBoxOutput("valueBox_numtourMulti", width = 3),
                                valueBoxOutput("valueBox_avgTouristLength", width = 3),
                                valueBoxOutput("valueBox_avgTouristSpending", width = 3),
                                # Overview of finances 
                                p("Park finances:", style = "font-size:22px"),
                                valueBoxOutput("valueBox_entryFee", width = 3),
                                valueBoxOutput("valueBox_totalEntryFees", width = 3),
                                valueBoxOutput("valueBox_budget", width = 3), 
                                valueBoxOutput("valueBox_parkWageBill", width = 3), 
                                # Only some parks have community revenue sharing, so make this small
                                p("Community Revenue-Sharing:", style = "font-size:22px"),
                                span(textOutput("community_revenue_sharing"), style = "font-size:18px"),
                                # Removed this top part: it distracts from LEWIE too much. 
                                # ),
                                # fluidRow(
                                # box(width = 12,
                                # p("Tourism scenarios:", style = "font-size:18px"), 
                                # p("Tourism scenario #1: increase total number of tourists", style = "font-size:18px"), 
                                # fluidRow(
                                #     column(3, sliderInput("pct_increase_tourists", "Percent Increase in total tourists", min = -100, max = 100, value = 10)),
                                #     column(9)
                                # ),
                                # p(textOutput("increased_tourist_number")),
                                # hr(),
                                # ),
                                # box(width = 12, 
                                # p("Tourism scenario #2: detailed increases in tourism stats", style = "font-size:18px"), 
                                hr(),
                                p("Entry fee calculator:", style = "font-size:22px"),
                                p("(The two input boxes below are not for simulations. They are just useful to calculate total entry fees. Note that many parks charge a range of different tariffs (for example by age), so these should be thought as the average fee per tourist.)" , style = "font-size:16px"),
                                span("Enter the number of additional (fee-paying) visitors you expect and the highlighted figures below will change:", style = "font-size:18px"),
                                fluidRow(
                                    column(4, numericInput("inc_touristsTotal_count", "Increase in total visitors (count):", value = 0, step = 1, min = -1000000, max = 1000000)),
                                    column(4, numericInput("inc_fee", "Increase in average park entry fee ($):", value = 0.00, step = 0.01, min = -100000, max = 100000))
                                ),
                                span(textOutput("detailed_tourist_increases"), style = "font-size:18px"),
                                span(textOutput("detailed_tourist_increases_result"), style = "font-size:18px")
                            ),
                        ),
                        fluidRow(
                            box(width = 12, title = span("Local-economy impacts of tourist spending (US$):", style = 'font-size:27px;color:rgb(63, 144, 210);'),
                                p("For every dollar of tourist spending, the total production multiplier is:",  style = "font-size:22px"),
                                fluidRow( 
                                    column(width = 4,
                                           valueBox(textOutput("totalmult"), width = 12, "Total Production Multiplier", icon = icon("gears"),  color = "maroon"),
                                    ),
                                    column(width = 4,
                                           p("Which can be split into:"),
                                           valueBox(textOutput("touractmult"), width = 12, "Tourism activities", icon = icon("sun"),  color = "orange"),
                                           valueBox(textOutput("nontouractmult"), width = 12, "Non-Tourism activities", icon = icon("briefcase"),  color = "orange")
                                    ),
                                    hr(),
                                    column(width = 4, 
                                           p("The production multiplier represents the total value of goods and services generated in the economy for every dollar spent by a tourist, 
                                    including all higher-order impacts, or ripple effects
                                    (e.g.: Tourist spends money at a restaurant, the cook spends some of those wages at a local shop, the shopkeeper spends some of those profits buying food from a farmer, etc.)."
                                             , style = "font-size:18px"),
                                    ),
                                ),
                                fluidRow(
                                    hr()
                                ),
                                p("For every dollar of tourist spending, the total income multiplier is:", style = "font-size:22px; color"),
                                
                                column(width = 4, 
                                       valueBox(textOutput("gdpmult"), width = 12, "Total Income Multiplier", icon = icon("money-bill-trend-up"),  color = "blue"),
                                ),
                                column(width = 4, 
                                       p("Which can be split into:"),
                                       valueBox(textOutput("labmult"),  width = 12, "Accruing to Labor", icon = icon("user"),  color = "teal"),
                                       valueBox(textOutput("capmult"),  width = 12, "Accruing to Capital", icon = icon("building"),  color = "teal")
                                ),
                                column(width = 4,
                                       p("Or, alternatively, can be split into:"),
                                       valueBox(textOutput("poormult"), width = 12, "Accruing to Poor Households", icon = icon("coins"),  color = "aqua"), 
                                       valueBox(textOutput("nonpoormult"), width = 12, "Accruing to NonPoor Households", icon = icon("money-bill"),  color = "aqua")
                                ),
                                hr(),
                                p("The income multiplier represents the total value of incomes that are generated in the economy for every dollar spent by a tourist, 
                                    including all ripple effects (income of the cook, and of the shopkeeper, and of the farmer, etc)."
                                  , width = 10, style = "font-size:18px"), 
                                p("The income multiplier is always smaller than the production multiplier, because it is a subset of it (like profits are a subset of revenue)."
                                  , width = 10, style = "font-size:18px"),
                                p("Note: Only boxes of the same color should be added together. The two teal boxes add up to the dark blue box. The two light-blue boxes also. 
                            The two orange boxes above sum up to the dark red Total Production Multiplier box.  
                            However, total production multiplier (dark red) and the income multiplier (dark blue) and can NOT be added together, because the former already contains the latter.", style = "font-size:18px"
                                ), 
                                p("Note: Numbers are rounded.  Please forgive discrepancies in the second decimal.")
                                
                            )
                        ),
                        # MULTIPLIERS NET OF PARK FEES
                        fluidRow(
                          box(width = 12, title = span("Local-economy impacts of tourist spending net of park fees (US$):", style = 'font-size:27px;color:rgb(63, 144, 210);'),
                              p("For every dollar of tourist spending outside of park fees, the total production multiplier is:",  style = "font-size:22px"),
                              fluidRow( 
                                column(width = 4,
                                       valueBox(textOutput("totalmult_npf"), width = 12, "Total Production Multiplier (net of park fees)", icon = icon("gears"),  color = "maroon"),
                                ),
                                column(width = 4,
                                       p("Which can be split into:"),
                                       valueBox(textOutput("touractmult_npf"), width = 12, "Tourism activities", icon = icon("sun"),  color = "orange"),
                                       valueBox(textOutput("nontouractmult_npf"), width = 12, "Non-Tourism activities", icon = icon("briefcase"),  color = "orange")
                                ),
                                hr(),
                                column(width = 4, 
                                       p("The production multiplier net of park fees represents the total value of goods and services generated in the economy for every dollar spent by a tourist, excluding park entry fees.", style = "font-size:18px"),
                                ),
                              ),
                              fluidRow(
                                hr()
                              ),
                              p("For every dollar of tourist spending outside of park fees, the total income multiplier is:", style = "font-size:22px; color"),
                              
                              column(width = 4, 
                                     # p("total mult")
                                     valueBox(textOutput("gdpmult_npf"), width = 12, "Total Income Multiplier (net of park fees)", icon = icon("money-bill-trend-up"),  color = "blue"),
                              ),
                              column(width = 4, 
                                     p("Which can be split into:"),
                                     valueBox(textOutput("labmult_npf"),  width = 12, "Accruing to Labor", icon = icon("user"),  color = "teal"),
                                     valueBox(textOutput("capmult_npf"),  width = 12, "Accruing to Capital", icon = icon("building"),  color = "teal")
                              ),
                              column(width = 4,
                                     p("Or, alternatively, can be split into:"),
                                     valueBox(textOutput("poormult_npf"), width = 12, "Accruing to Poor Households", icon = icon("coins"),  color = "aqua"), 
                                     valueBox(textOutput("nonpoormult_npf"), width = 12, "Accruing to NonPoor Households", icon = icon("money-bill"),  color = "aqua")
                              ),
                              hr(),
                              p("The income multiplier net of park fees represents the total value of incomes that are generated in the economy for every dollar spent by a tourist, excluding park entry fees.", width = 10, style = "font-size:18px")
                          )
                        ),
                        fluidRow(
                            downloadButton("report1", "Generate PDF Report", style = "margin-left: 20px"),
                            p('Generating the files can take up to 30 seconds. There is no need for multiple clicks.', style = "margin-left: 20px")
                        )
                    )
            ),
            # ============================ SIMULATIONS DASHBOARD ======================================
            tabItem("simulations",
                    h1("Simulations"),
                    p("On this tab you can run simulations using the LEWIE-Lite model. Scroll to the relevant section to run your simulations.", style = "font-size:18px"),
                    fluidRow(
                        box(width = 12, title = "Explore the local economy impacts of…",
                            p(HTML("1. <a href='#sim_TouristSpending'>Tourist Spending</a>"), style = "margin-left: 20px"),
                            p(HTML("2. <a href='#sim_PASpending'>Park Spending</a>"), style = "margin-left: 20px"),
                            p(HTML("3. <a href='#sim_ComRevShSpending'>Community Spending</a>"), style = "margin-left: 20px"),
                            p(HTML("4. <a href='#sim_AgSpending'>Agricultural Production</a>"), style = "margin-left: 20px"),
                            p(HTML("5. <a href='#sim_NagSpending'>Non-Agricultural Production</a>"), style = "margin-left: 20px"),
                            p(HTML("6. <a href='#sim_LFUSKSpending'>Wage Earnings (for 4 different labor groups)</a>"), style = "margin-left: 20px"),
                            p(HTML("7. <a href='#sim_LocalGSpending'>Local Government Spending</a>"), style = "margin-left: 20px")
                        )
                    ),
                    fluidRow(
                        
                        box(width = 12, title = span("Local Economy-wide Impacts of Tourist Spending (US$)", style = 'font-size:22px;color:rgb(63, 144, 210);'),
                            p('You may wish to evaluate different values of tourist spending: total tourist spending, tourist spending attributable to the PA, change in tourist spending you expect from this project, etc.'),
                            fluidRow(
                                column(4, inp_sim_TouristSpending)
                            ),
                            p("EFFECTS OF THIS TOURISM SPENDING ON...", style = "font-size: 12pt"),
                            fluidRow(
                                column(width=4, plotOutput("simTourists_totalprod")),
                                column(width = 4, plotOutput("simTourists_incomes")),
                                column(width=4, plotOutput("simTourists_labor"))
                            ),
                            hr(), 
                            
                            p("Calculation helpers:", style = "font-size:18px"),
                            fluidRow(
                                column(3, sliderInput("pct_increase_tourists", "Percent Increase in total tourists", min = -100, max = 100, value = 10), 
                                       sliderInput("pct_increase_avg_spending", "Percent Increase in average spending / tourist", min = -100, max = 100, value = 10)
                                ),
                                column(9, p(textOutput("tourists_baseline")), 
                                       p(textOutput("increased_tourist_number"))
                                )
                            ),
                            p(),
                            useShinyjs(),
                            actionButton("toTop1", "Back to top",
                                         class="btn btn-light",
                                         style="padding:4px; font-size:80%; float:right"),
                            
                            
                        )
                    ),
                    fluidRow(
                        box(width = 12, title = span("Local Economy-wide Impacts of Park Spending (US$)", style = 'font-size:22px;color:rgb(63, 144, 210);'),
                            p('Park spending is policy-determined, even though there are visitor fees in most places.'),
                            fluidRow(
                                column(4, inp_sim_PASpending)
                            ),
                            p("EFFECTS OF THIS PARK BUDGET ON...", style = "font-size: 12pt"),
                            fluidRow(
                                column(width=4, plotOutput("simPA_totalprod")),
                                column(width=4, plotOutput("simPA_incomes")),
                                column(width=4, plotOutput("simPA_labor"))
                            ),
                            p(),
                            useShinyjs(),
                            actionButton("toTop2", "Back to top",
                                         class="btn btn-light",
                                         style="padding:4px; font-size:80%; float:right")
                        )
                    ),
                    fluidRow(
                        box(width = 12, title = span("Local Economy-wide Impacts of Community Spending (US$)", style = 'font-size:22px;color:rgb(63, 144, 210);'),
                            p('Our simulations assume that part of PA visitor fees are shared with communities. We can also use this model to see the effects of giving additional money to communities near the park. For example, some NGOs support communities near PAs that may or may not happen if the PA did not exist.'),
                            fluidRow(
                                column(4, inp_sim_ComRevShSpending)
                            ),
                            p("EFFECTS OF THIS COMMUNITY SPENDING ON...", style = "font-size: 12pt"),
                            fluidRow(
                                column(width=4, plotOutput("simComRevSh_totalprod")),
                                column(width=4, plotOutput("simComRevSh_incomes")),
                                column(width=4, plotOutput("simComRevSh_labor"))
                            ),
                            p(),
                            useShinyjs(),
                            actionButton("toTop3", "Back to top",
                                         class="btn btn-light",
                                         style="padding:4px; font-size:80%; float:right")
                        )
                    ),
                    fluidRow(
                        box(width = 12, title = span("Local Economy-wide Impacts of Increased Agricultural Production (US$)", style = 'font-size:22px;color:rgb(63, 144, 210);'),
                            p('Tourist activities create demand for local agricultural products.  We can also use this model to see the effects of complementary
                          interventions to increase the demand for local agricultural products, for example, 
                          by enabling restaurants and lodges to source more food locally.'),
                            fluidRow(
                                column(4, inp_sim_AgSpending)
                            ),
                            p("EFFECTS OF THIS INCREASE IN LOCAL AGRICULTURAL PRODUCTION ON...", style = "font-size: 12pt"),
                            fluidRow(
                                column(width=4, plotOutput("simAg_totalprod")),
                                column(width=4, plotOutput("simAg_incomes")),
                                column(width=4, plotOutput("simAg_labor"))
                            ),
                            p(),
                            useShinyjs(),
                            actionButton("toTop4", "Back to top",
                                         class="btn btn-light",
                                         style="padding:4px; font-size:80%; float:right")
                        )
                    ),
                    fluidRow(
                        box(width = 12, title = span("Local Economy-wide Impacts of Increased Non-Agricultural Production (US$)", style = 'font-size:22px;color:rgb(63, 144, 210);'),
                            p('Tourist activities create demand for local non-agricultural products (crafts, retail, services, etc). We can also use this model to see the effects of complementary
                          interventions to increase the demand for local non-agricultural products, for example, 
                          by enabling restaurants and lodges to source local artifacts, processed goods, retail, construction services, etc.'),
                            fluidRow(
                                column(4, inp_sim_NagSpending)
                            ),
                            p("EFFECTS OF THIS INCREASE IN LOCAL NON-AGRICULTURAL PRODUCTION ON...", style = "font-size: 12pt"),
                            fluidRow(
                                column(width=4, plotOutput("simNag_totalprod")),
                                column(width=4, plotOutput("simNag_incomes")),
                                column(width=4, plotOutput("simNag_labor"))
                            ),
                            p(),
                            useShinyjs(),
                            actionButton("toTop5", "Back to top",
                                         class="btn btn-light",
                                         style="padding:4px; font-size:80%; float:right")
                        )
                    ),
                    
                    # == WAGES ==
                    fluidRow(
                        HTML('<style>
                                   #subbox {
                                   border = 0px;
                                   padding: 5px;
                                   box-shadow: red;
                                   border-bottom: red;
                                   }
                                   </style>'),
                        box(width = 12, title = span("Local Economy-wide Impacts of Wage Earnings (US$)", style = 'font-size:22px;color:rgb(63, 144, 210);'),
                            p('Tourist activities create demand for local labor. We can also use this model to see the effects of complementary
                          interventions to increase the employment of local workers, for example, through job training programs.'),
                            p('In the four following panels we track results for four types of workers: Low-Skilled Females, Low-Skilled Males, Skilled Females, Skilled Males.'),                        
                            # --- LFUSK
                            fluidRow(
                                box(width = 12, title = span("Low-skilled Female Earnings (US$)", style = 'font-size:18px;color:rgb(63, 144, 210);'),
                                    fluidRow(
                                        column(4, inp_sim_LFUSKSpending)
                                    ),
                                    p("EFFECTS OF THIS INCREASE IN EARNINGS TO LOW-SKILLED FEMALE WORKERS ON...", style = "font-size: 12pt"),
                                    fluidRow(
                                        column(width=4, plotOutput("simLFUSK_totalprod")),
                                        column(width=4, plotOutput("simLFUSK_incomes")),
                                        column(width=4, plotOutput("simLFUSK_labor"))
                                    ),
                                    p(),
                                    useShinyjs(),
                                    actionButton("toTop6", "Back to top",
                                                 class="btn btn-light",
                                                 style="padding:4px; font-size:80%; float:right"),
                                )
                            ),
                            # --- LMUSK
                            fluidRow(
                                box(width = 12,  title = span("Low-skilled Male Earnings (US$)", style = 'font-size:18px;color:rgb(63, 144, 210);'),
                                    fluidRow(
                                        column(4, inp_sim_LMUSKSpending)
                                    ),
                                    p("EFFECTS OF THIS INCREASE IN EARNINGS TO LOW SKILLED MALE WORKERS ON...", style = "font-size: 12pt"),
                                    fluidRow(
                                        column(width=4, plotOutput("simLMUSK_totalprod")),
                                        column(width=4, plotOutput("simLMUSK_incomes")),
                                        column(width=4, plotOutput("simLMUSK_labor"))
                                    ),
                                    p(),
                                    useShinyjs(),
                                    actionButton("toTop7", "Back to top",
                                                 class="btn btn-light",
                                                 style="padding:4px; font-size:80%; float:right")
                                )
                            ),
                            # --- FUSK
                            fluidRow(
                                box(width = 12, title = span("Skilled Female Earnings (US$)", style = 'font-size:18px;color:rgb(63, 144, 210);'),
                                    fluidRow(
                                        column(4, inp_sim_LFSKSpending)
                                    ),
                                    p("EFFECTS OF THIS INCREASE IN EARNINGS TO SKILLED FEMALE WORKERS ON...", style = "font-size: 12pt"),
                                    fluidRow(
                                        column(width=4, plotOutput("simLFSK_totalprod")),
                                        column(width=4, plotOutput("simLFSK_incomes")),
                                        column(width=4, plotOutput("simLFSK_labor"))
                                    ),
                                    p(),
                                    useShinyjs(),
                                    actionButton("toTop8", "Back to top",
                                                 class="btn btn-light",
                                                 style="padding:4px; font-size:80%; float:right")
                                ),
                            ),
                            #---- MUSK
                            fluidRow(
                                box(width = 12, id = "subbox", title = span("Skilled Male Earnings (US$)", style = 'font-size:18px;color:rgb(63, 144, 210);'),
                                    fluidRow(
                                        column(4, inp_sim_LMSKSpending)
                                    ),
                                    p("EFFECTS OF THIS INCREASE IN EARNINGS TO SKILLED MALE WORKERS ON...", style = "font-size: 12pt"),
                                    fluidRow(
                                        column(width=4, plotOutput("simLMSK_totalprod")),
                                        column(width=4, plotOutput("simLMSK_incomes")),
                                        column(width=4, plotOutput("simLMSK_labor"))
                                    ),
                                    p(),
                                    useShinyjs(),
                                    actionButton("toTop9", "Back to top",
                                                 class="btn btn-light",
                                                 style="padding:4px; font-size:80%; float:right")
                                )
                            )
                        )
                    ),
                    # -------- end of wages --------
                    fluidRow(
                        box(width = 12, title = span("Local Economy-wide Impacts of Local Government Spending (US$)", style = 'font-size:22px;color:rgb(63, 144, 210);'),
                            p('Local government spends taxes on local activities, which also creates multipliers.'),
                            fluidRow(
                                column(4, inp_sim_LocalGSpending)
                            ),
                            p("EFFECTS OF THIS INCREASE IN LOCAL GOVERNMENT SPENDING ON...", style = "font-size: 12pt"),
                            fluidRow(
                                column(width=4, plotOutput("simLocalG_totalprod")),
                                column(width=4, plotOutput("simLocalG_incomes")),
                                column(width=4, plotOutput("simLocalG_labor"))
                            ),
                            p(),
                            useShinyjs(),
                            actionButton("toTop10", "Back to top",
                                         class="btn btn-light",
                                         style="padding:4px; font-size:80%; float:right")
                        )
                    ),
                    fluidRow(
                        downloadButton("report2", "Generate PDF Report", style = "margin-left: 20px"),
                        p('Generating the files can take up to 30 seconds. There is no need for multiple clicks.', style = "margin-left: 20px")
                    )
            )
        )
    )
);




###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
################################ SERVER #######################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################

# This is the server part of the shiny code.  All calculations and reactive calculations are made here.  
# 




#########################################################



server <- function(input, output) { 
    
    # %%%%%%%%%%%%%%%%% Compute the SAM pre-RAS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    sam_preRAS <- reactive({
        
        
        # &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
        # &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
        # First rescale everything that needs to add up to 100: 
        # &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
        # &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
        # Rescale Ag
        ag_totalshares =  (input$ag_MwagesUnskilled_raw + input$ag_MwagesSkilled_raw + input$ag_FwagesUnskilled_raw + input$ag_FwagesSkilled_raw + 
                               input$ag_expLocalAg_raw +   input$ag_expTourism_raw + input$ag_expLocalFish_raw + input$ag_expServices_raw + input$ag_expLocalStores_raw + 
                               input$ag_expOutside_raw + input$ag_expLocalTaxes_raw + input$ag_expTaxes_raw)
        ag_scaleshares = (100 - input$ag_profitMargin)/ag_totalshares
        
        ag_MwagesUnskilled = input$ag_MwagesUnskilled_raw * ag_scaleshares
        ag_MwagesSkilled = input$ag_MwagesSkilled_raw * ag_scaleshares
        ag_FwagesUnskilled = input$ag_FwagesUnskilled_raw * ag_scaleshares
        ag_FwagesSkilled = input$ag_FwagesSkilled_raw * ag_scaleshares
        ag_expLocalAg = input$ag_expLocalAg_raw * ag_scaleshares
        ag_expTourism = input$ag_expTourism_raw * ag_scaleshares
        ag_expLocalFish = input$ag_expLocalFish_raw * ag_scaleshares
        ag_expServices = input$ag_expServices_raw * ag_scaleshares
        ag_expLocalStores = input$ag_expLocalStores_raw * ag_scaleshares
        ag_expOutside = input$ag_expOutside_raw * ag_scaleshares
        ag_expLocalTaxes = input$ag_expLocalTaxes_raw * ag_scaleshares
        ag_expTaxes = input$ag_expTaxes_raw * ag_scaleshares
        
        
        # browser()
        
        # Rescale Tourism
        tourism_totalshares = (
            input$tourism_MwagesUnskilled_raw +
                input$tourism_MwagesSkilled_raw +
                input$tourism_FwagesUnskilled_raw +
                input$tourism_FwagesSkilled_raw +
                input$tourism_expLocalAg_raw +
                input$tourism_expTourism_raw +
                input$tourism_expLocalFish_raw +
                input$tourism_expServices_raw +
                input$tourism_expLocalStores_raw +
                input$tourism_expOutside_raw +
                input$tourism_expLocalTaxes_raw +    
                input$tourism_expTaxes_raw )
        
        tourism_scaleshares = (100 - input$tourism_profitMargin)/tourism_totalshares
        
        tourism_MwagesUnskilled = input$tourism_MwagesUnskilled_raw * tourism_scaleshares
        tourism_MwagesSkilled = input$tourism_MwagesSkilled_raw * tourism_scaleshares
        tourism_FwagesUnskilled = input$tourism_FwagesUnskilled_raw * tourism_scaleshares
        tourism_FwagesSkilled = input$tourism_FwagesSkilled_raw * tourism_scaleshares
        tourism_expLocalAg = input$tourism_expLocalAg_raw * tourism_scaleshares
        tourism_expTourism = input$tourism_expTourism_raw * tourism_scaleshares
        tourism_expLocalFish = input$tourism_expLocalFish_raw * tourism_scaleshares
        tourism_expServices = input$tourism_expServices_raw * tourism_scaleshares
        tourism_expLocalStores = input$tourism_expLocalStores_raw * tourism_scaleshares
        tourism_expOutside = input$tourism_expOutside_raw * tourism_scaleshares
        tourism_expLocalTaxes = input$tourism_expLocalTaxes_raw * tourism_scaleshares
        tourism_expTaxes = input$tourism_expTaxes_raw * tourism_scaleshares
        
        
        # Rescale NonAg
        nag_totalshares = (
            input$nag_MwagesUnskilled_raw +
                input$nag_MwagesSkilled_raw +
                input$nag_FwagesUnskilled_raw +
                input$nag_FwagesSkilled_raw +
                input$nag_expLocalAg_raw +
                input$nag_expTourism_raw + 
                input$nag_expLocalFish_raw +
                input$nag_expServices_raw +
                input$nag_expLocalStores_raw +
                input$nag_expOutside_raw +
                input$nag_expLocalTaxes_raw + 
                input$nag_expTaxes_raw )
        
        nag_scaleshares = (100 - input$nag_profitMargin)/nag_totalshares
        
        nag_MwagesUnskilled = input$nag_MwagesUnskilled_raw * nag_scaleshares
        nag_MwagesSkilled = input$nag_MwagesSkilled_raw * nag_scaleshares
        nag_FwagesUnskilled = input$nag_FwagesUnskilled_raw * nag_scaleshares
        nag_FwagesSkilled = input$nag_FwagesSkilled_raw * nag_scaleshares
        nag_expLocalAg = input$nag_expLocalAg_raw * nag_scaleshares
        nag_expTourism = input$nag_expTourism_raw * nag_scaleshares
        nag_expLocalFish = input$nag_expLocalFish_raw * nag_scaleshares
        nag_expServices = input$nag_expServices_raw * nag_scaleshares
        nag_expLocalStores = input$nag_expLocalStores_raw * nag_scaleshares
        nag_expOutside = input$nag_expOutside_raw * nag_scaleshares
        nag_expLocalTaxes = input$nag_expLocalTaxes_raw * nag_scaleshares
        nag_expTaxes = input$nag_expTaxes_raw * nag_scaleshares
        
        
        # Rescale NatResources
        fish_totalshares = (
            input$fish_MwagesUnskilled_raw +
                input$fish_MwagesSkilled_raw +
                input$fish_FwagesUnskilled_raw +
                input$fish_FwagesSkilled_raw +
                input$fish_expLocalAg_raw +
                input$fish_expTourism_raw + 
                input$fish_expLocalFish_raw +
                input$fish_expServices_raw +
                input$fish_expLocalStores_raw +
                input$fish_expOutside_raw +
                input$fish_expLocalTaxes_raw +
                input$fish_expTaxes_raw)
        
        fish_scaleshares = (100 - input$fish_profitMargin)/fish_totalshares
        
        fish_MwagesUnskilled = input$fish_MwagesUnskilled_raw * fish_scaleshares
        fish_MwagesSkilled = input$fish_MwagesSkilled_raw * fish_scaleshares
        fish_FwagesUnskilled = input$fish_FwagesUnskilled_raw * fish_scaleshares
        fish_FwagesSkilled = input$fish_FwagesSkilled_raw * fish_scaleshares
        fish_expLocalAg = input$fish_expLocalAg_raw * fish_scaleshares
        fish_expTourism = input$fish_expTourism_raw * fish_scaleshares
        fish_expLocalFish = input$fish_expLocalFish_raw * fish_scaleshares
        fish_expServices = input$fish_expServices_raw * fish_scaleshares
        fish_expLocalStores = input$fish_expLocalStores_raw * fish_scaleshares
        fish_expOutside = input$fish_expOutside_raw * fish_scaleshares
        fish_expLocalTaxes = input$fish_expLocalTaxes_raw * fish_scaleshares
        fish_expTaxes = input$fish_expTaxes_raw * fish_scaleshares
        
        
        # Rescale Restaurants
        restaurants_totalshares = (
            input$restaurants_MwagesUnskilled_raw +
                input$restaurants_MwagesSkilled_raw +
                input$restaurants_FwagesUnskilled_raw +
                input$restaurants_FwagesSkilled_raw +
                input$restaurants_expLocalAg_raw +
                input$restaurants_expTourism_raw + 
                input$restaurants_expLocalFish_raw +
                input$restaurants_expServices_raw +
                input$restaurants_expLocalStores_raw +
                input$restaurants_expOutside_raw +
                input$restaurants_expLocalTaxes_raw +
                input$restaurants_expTaxes_raw )
        
        restaurants_scaleshares = (100 - input$restaurants_profitMargin)/restaurants_totalshares
        
        restaurants_MwagesUnskilled = input$restaurants_MwagesUnskilled_raw * restaurants_scaleshares
        restaurants_MwagesSkilled = input$restaurants_MwagesSkilled_raw * restaurants_scaleshares
        restaurants_FwagesUnskilled = input$restaurants_FwagesUnskilled_raw * restaurants_scaleshares
        restaurants_FwagesSkilled = input$restaurants_FwagesSkilled_raw * restaurants_scaleshares
        restaurants_expLocalAg = input$restaurants_expLocalAg_raw * restaurants_scaleshares
        restaurants_expTourism = input$restaurants_expTourism_raw * restaurants_scaleshares
        restaurants_expLocalFish = input$restaurants_expLocalFish_raw * restaurants_scaleshares
        restaurants_expServices = input$restaurants_expServices_raw * restaurants_scaleshares
        restaurants_expLocalStores = input$restaurants_expLocalStores_raw * restaurants_scaleshares
        restaurants_expOutside = input$restaurants_expOutside_raw * restaurants_scaleshares
        restaurants_expLocalTaxes = input$restaurants_expLocalTaxes_raw * restaurants_scaleshares
        restaurants_expTaxes = input$restaurants_expTaxes_raw * restaurants_scaleshares
        
        
        # Rescale Lodges
        lodges_totalshares = (
            input$lodges_MwagesUnskilled_raw +
                input$lodges_MwagesSkilled_raw +
                input$lodges_FwagesUnskilled_raw +
                input$lodges_FwagesSkilled_raw +
                input$lodges_expLocalAg_raw +
                input$lodges_expTourism_raw + 
                input$lodges_expLocalFish_raw +
                input$lodges_expServices_raw +
                input$lodges_expLocalStores_raw +
                input$lodges_expOutside_raw +
                input$lodges_expLocalTaxes_raw + 
                input$lodges_expTaxes_raw )
        
        lodges_scaleshares = (100 - input$lodges_profitMargin)/lodges_totalshares
        
        lodges_MwagesUnskilled = input$lodges_MwagesUnskilled_raw * lodges_scaleshares
        lodges_MwagesSkilled = input$lodges_MwagesSkilled_raw * lodges_scaleshares
        lodges_FwagesUnskilled = input$lodges_FwagesUnskilled_raw * lodges_scaleshares
        lodges_FwagesSkilled = input$lodges_FwagesSkilled_raw * lodges_scaleshares
        lodges_expLocalAg = input$lodges_expLocalAg_raw * lodges_scaleshares
        lodges_expTourism = input$lodges_expTourism_raw * lodges_scaleshares
        lodges_expLocalFish = input$lodges_expLocalFish_raw * lodges_scaleshares
        lodges_expServices = input$lodges_expServices_raw * lodges_scaleshares
        lodges_expLocalStores = input$lodges_expLocalStores_raw * lodges_scaleshares
        lodges_expOutside = input$lodges_expOutside_raw * lodges_scaleshares
        lodges_expLocalTaxes = input$lodges_expLocalTaxes_raw * lodges_scaleshares
        lodges_expTaxes = input$lodges_expTaxes_raw * lodges_scaleshares
        
        
        # Rescale Community Revenue Sharing
        comRevSh_totalshares = (
            input$comRevSh_MwagesUnskilled_raw +
                input$comRevSh_MwagesSkilled_raw +
                input$comRevSh_FwagesUnskilled_raw +
                input$comRevSh_FwagesSkilled_raw +
                input$comRevSh_expCapital_raw +    
                input$comRevSh_expLocalAg_raw +
                input$comRevSh_expTourism_raw + 
                input$comRevSh_expLocalFish_raw +
                input$comRevSh_expServices_raw +
                input$comRevSh_expLocalStores_raw +
                input$comRevSh_expOutside_raw )
        
        comRevSh_scaleshares = (100)/comRevSh_totalshares 
        
        comRevSh_MwagesUnskilled = input$comRevSh_MwagesUnskilled_raw * comRevSh_scaleshares
        comRevSh_MwagesSkilled = input$comRevSh_MwagesSkilled_raw * comRevSh_scaleshares
        comRevSh_FwagesUnskilled = input$comRevSh_FwagesUnskilled_raw * comRevSh_scaleshares
        comRevSh_FwagesSkilled = input$comRevSh_FwagesSkilled_raw * comRevSh_scaleshares
        comRevSh_expCapital = input$comRevSh_expCapital_raw * comRevSh_scaleshares
        comRevSh_expLocalAg = input$comRevSh_expLocalAg_raw * comRevSh_scaleshares
        comRevSh_expTourism = input$comRevSh_expTourism_raw * comRevSh_scaleshares
        comRevSh_expLocalFish = input$comRevSh_expLocalFish_raw * comRevSh_scaleshares
        comRevSh_expServices = input$comRevSh_expServices_raw * comRevSh_scaleshares
        comRevSh_expLocalStores = input$comRevSh_expLocalStores_raw * comRevSh_scaleshares
        comRevSh_expOutside = input$comRevSh_expOutside_raw * comRevSh_scaleshares
        
        
        
        # Rescale Local Government
        localG_totalshares = (
            input$localG_MwagesUnskilled_raw +
                input$localG_MwagesSkilled_raw +
                input$localG_FwagesUnskilled_raw +
                input$localG_FwagesSkilled_raw +
                input$localG_expCapital_raw +
                input$localG_expLocalAg_raw +
                input$localG_expTourism_raw + 
                input$localG_expLocalFish_raw +
                input$localG_expServices_raw +
                input$localG_expLocalStores_raw +
                input$localG_expOutside_raw ) 
        # input$localG_expLocalTaxes_raw +  # taxes don't pay taxes.
        # input$localG_expTaxes_raw 
        
        # LocalG has no profit margin, so the scaling is just inverse of total shares
        localG_scaleshares = 100/localG_totalshares
        
        localG_MwagesUnskilled = input$localG_MwagesUnskilled_raw * localG_scaleshares
        localG_MwagesSkilled = input$localG_MwagesSkilled_raw * localG_scaleshares
        localG_FwagesUnskilled = input$localG_FwagesUnskilled_raw * localG_scaleshares
        localG_FwagesSkilled = input$localG_FwagesSkilled_raw * localG_scaleshares
        localG_expCapital = input$localG_expCapital_raw * localG_scaleshares
        localG_expLocalAg = input$localG_expLocalAg_raw * localG_scaleshares
        localG_expTourism = input$localG_expTourism_raw * localG_scaleshares
        localG_expLocalFish = input$localG_expLocalFish_raw * localG_scaleshares
        localG_expServices = input$localG_expServices_raw * localG_scaleshares
        localG_expLocalStores = input$localG_expLocalStores_raw * localG_scaleshares
        localG_expOutside = input$localG_expOutside_raw * localG_scaleshares
        localG_expLocalTaxes = input$localG_expLocalTaxes_raw * localG_scaleshares
        localG_expTaxes = input$localG_expTaxes_raw * localG_scaleshares
        
        
        # Rescale household expenditure shares: that's already done below (within the amt calculations)
        
        
        
        
        # &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
        # Now make the SAM
        # &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
        
        
        # Tourist Spending Column: 
        #------------------------------------
        tourist_days <- input$tourists_popMultiDay * input$tourists_nbDays + input$tourists_popSingleDay
        tourist_nights <- input$tourists_popMultiDay * input$tourists_nbNights
        tourexp_lodging <- tourist_nights * input$tourists_roomPrice
        tourexp_localRestaurants <- tourist_days * input$tourists_expRestaurants
        
        sam_tourists["Nag",] <- tourist_days * (input$tourists_expRetShops +
                                                    input$tourists_expOther)
        sam_tourists["Tourism",] <- tourist_days * (input$tourists_expGuidesTours +
                                                        input$tourists_expSouvenirs)
        sam_tourists["Restaurants",] <- tourist_days * input$tourists_expRestaurants
        sam_tourists["Lodges", ] <- tourexp_lodging 
        
        # Should not matter which calculation you use : natural park and tourist tabs need to agree
        # sam_tourists["PA",] <- (input$tourists_popMultiDay + input$tourists_popSingleDay) * input$tourists_expParkEntry
        sam_tourists["PA",] <- input$natPark_entryFees
        
        
        # Tourism Column not same as tourists column : it's the specialized tourism shops sam_tourists["Tourism",]
        # #------------------------------------
        # sam_tourism_totalrev <-  (sam_tourists["Tourism",] + sam_lodges["Tourism",] + sam_restaurants["Tourism",] + sam_nag["Tourism",] +
        #                            sam_hhp["Tourism",] + sam_hhnp["Tourism",] + sam_pa["Tourism",]  + sam_tourism["Tourism",] + sam_comrevsh["Tourism", ])/
        #             (1-input$tourism_expTourism/100)
        # This can be taken straight from the other column
        sam_tourism_totalrev <- sam_tourists["Tourism",]
        
        
        
        sam_tourism["Ag",] <- sam_tourism_totalrev * tourism_expLocalAg/100
        sam_tourism["Nag",] <- sam_tourism_totalrev * (tourism_expServices + tourism_expLocalStores)/100
        sam_tourism["Tourism",] <- sam_tourism_totalrev * (tourism_expTourism)/100
        sam_tourism["Fish",] <- sam_tourism_totalrev * (tourism_expLocalFish)/100
        
        sam_tourism["LMUSK",] <- sam_tourism_totalrev * (tourism_MwagesUnskilled )/100 * input$tourism_shareWorkersLocal/100
        sam_tourism["LMSK",] <- sam_tourism_totalrev * ( tourism_MwagesSkilled)/100 * input$tourism_shareWorkersLocal/100
        sam_tourism["LFUSK",] <- sam_tourism_totalrev * (tourism_FwagesUnskilled )/100 * input$tourism_shareWorkersLocal/100
        sam_tourism["LFSK",] <- sam_tourism_totalrev * ( tourism_FwagesSkilled)/100 * input$tourism_shareWorkersLocal/100
        
        sam_tourism["LocalG",] <- sam_tourism_totalrev * (tourism_expLocalTaxes)/100
        sam_tourism["G",] <- sam_tourism_totalrev * (tourism_expTaxes)/100
        sam_tourism["ROW",] <- sam_tourism_totalrev * (tourism_expOutside)/100
        # K account used to be the residual, now computed directly
        # sam_tourism_allbutK <- sum(sam_tourism)
        # sam_tourism["K",] <- sam_tourism_totalrev - sam_tourism_allbutK
        sam_tourism["K",] <- sam_tourism_totalrev * (input$tourism_profitMargin)/100 * input$tourism_shareLocallyOwned/100
        
        
        # Lodges Column
        #------------------------------------
        rev_Lodging <- tourexp_lodging
        sam_lodges["Ag",] <-  rev_Lodging * lodges_expLocalAg/100
        sam_lodges["Nag", ] <- rev_Lodging * (lodges_expServices + lodges_expLocalStores)/100
        sam_lodges["Tourism", ] <- rev_Lodging * (lodges_expTourism/100)
        sam_lodges["Fish", ] <- rev_Lodging * (lodges_expLocalFish/100)
        
        sam_lodges["LMUSK", ] <- rev_Lodging * (lodges_MwagesUnskilled)/100 * input$lodges_shareWorkersLocal/100
        sam_lodges["LMSK", ] <- rev_Lodging * (lodges_MwagesSkilled)/100 * input$lodges_shareWorkersLocal/100
        sam_lodges["LFUSK", ] <- rev_Lodging * (lodges_FwagesUnskilled)/100 * input$lodges_shareWorkersLocal/100
        sam_lodges["LFSK", ] <- rev_Lodging * (lodges_FwagesSkilled)/100 * input$lodges_shareWorkersLocal/100
        
        sam_lodges["LocalG", ] <- rev_Lodging * lodges_expLocalTaxes/100
        sam_lodges["G", ] <- rev_Lodging * lodges_expTaxes/100
        sam_lodges["ROW", ] <- rev_Lodging * (lodges_expOutside/100 
                                              +  input$lodges_profitMargin/100 * (1-input$lodges_shareLocallyOwned/100)
                                              +  (lodges_MwagesUnskilled + lodges_MwagesSkilled + lodges_FwagesUnskilled + lodges_FwagesSkilled)/100 * (1 - input$lodges_shareWorkersLocal/100))
        
        
        
        # K account used to be the residual, but now it is the PROFIT SHARE
        # sam_lodges_allbutK <- sum(sam_lodges)
        # sam_lodges["K",] <- (rev_Lodging - sam_lodges_allbutK)* input$lodges_shareOwned / 100
        sam_lodges["K",] <- rev_Lodging * input$lodges_profitMargin/100 * input$lodges_shareLocallyOwned/100
        
        
        
        # Both household columns first (need it to fill out following columns)
        #======================================================================
        hhp_totalInc <- input$hhp_pcinc * input$hhp_pop
        hhnp_totalInc <- input$hhnp_pcinc * input$hhnp_pop
        
        hhp_sumshares <- input$hhp_expGroceryFood + input$hhp_expGroceryOther + input$hhp_expMarkets + input$hhp_expLocalFarms +
            input$hhp_expLocalFish + input$hhp_expTourism + input$hhp_expLocalProc + input$hhp_expRest + input$hhp_expOtherLocal + input$hhp_expOutside +
            input$hhp_expRent + input$hhp_expLocalTaxes + input$hhp_expTaxes
        hhnp_sumshares <- input$hhnp_expGroceryFood + input$hhnp_expGroceryOther + input$hhnp_expMarkets + input$hhnp_expLocalFarms +
            input$hhnp_expLocalFish + input$hhnp_expTourism + input$hhnp_expLocalProc + input$hhnp_expRest + input$hhnp_expOtherLocal + input$hhnp_expOutside +
            input$hhnp_expRent + input$hhnp_expLocalTaxes + input$hhnp_expTaxes 
        
        
        
        # SAM rows for Poor (using sumshares to rescale)
        sam_hhp["Ag",] <- hhp_totalInc*(input$hhp_expLocalFarms/hhp_sumshares)
        sam_hhp["Tourism",] <- hhp_totalInc*(input$hhp_expTourism/hhp_sumshares)
        sam_hhp["Nag",] <- hhp_totalInc*((input$hhp_expGroceryFood +
                                              input$hhp_expGroceryOther +
                                              input$hhp_expMarkets +
                                              input$hhp_expLocalProc +
                                              input$hhp_expOtherLocal)/hhp_sumshares)
        sam_hhp["Fish",] <- hhp_totalInc*(input$hhp_expLocalFish/hhp_sumshares)
        sam_hhp["Restaurants", ] <- hhp_totalInc*(input$hhp_expRest/hhp_sumshares)
        # Rents and taxes calculation: 
        sam_hhp["K", ] <- hhp_totalInc*(input$hhp_expRent/hhp_sumshares)
        sam_hhp["LocalG", ] <- hhp_totalInc*(input$hhp_expLocalTaxes/hhp_sumshares)
        sam_hhp["G", ] <- hhp_totalInc*(input$hhp_expTaxes/hhp_sumshares)
        # ROW account is the RESIDUAL
        sam_hhp_allbutrow <- sum(sam_hhp)
        sam_hhp["ROW", ] <- hhp_totalInc - sam_hhp_allbutrow
        
        
        # SAM rows for Non-Poor
        sam_hhnp["Ag",] <- hhnp_totalInc*input$hhnp_expLocalFarms/hhnp_sumshares
        sam_hhnp["Tourism",] <- hhnp_totalInc*(input$hhnp_expTourism/hhnp_sumshares)
        sam_hhnp["Nag",] <- hhnp_totalInc*(input$hhnp_expGroceryFood +
                                               input$hhnp_expGroceryOther +
                                               input$hhnp_expMarkets +
                                               input$hhnp_expLocalProc +
                                               input$hhnp_expOtherLocal)/hhnp_sumshares
        sam_hhnp["Fish",] <- hhnp_totalInc*input$hhnp_expLocalFish/hhnp_sumshares
        sam_hhnp["Restaurants", ] <- hhnp_totalInc*input$hhnp_expRest/hhnp_sumshares
        # Rents and taxes calculation: 
        sam_hhnp["K", ] <- hhnp_totalInc*(input$hhnp_expRent/hhnp_sumshares)
        sam_hhnp["LocalG", ] <- hhnp_totalInc*(input$hhnp_expLocalTaxes/hhnp_sumshares)
        sam_hhnp["G", ] <- hhnp_totalInc*(input$hhnp_expTaxes/hhnp_sumshares)
        # ROW account is the RESIDUAL
        sam_hhnp_allbutrow <- sum(sam_hhnp)
        sam_hhnp["ROW", ] <- hhnp_totalInc - sam_hhnp_allbutrow
        
        
        # # Restaurants Column
        # #------------------------------------
        rev_Restaurants <- tourexp_localRestaurants + sam_hhp["Restaurants",] + sam_hhnp["Restaurants",]
        sam_restaurants["Ag",] <-  rev_Restaurants * restaurants_expLocalAg/100
        sam_restaurants["Nag", ] <- rev_Restaurants * (restaurants_expServices + restaurants_expLocalStores)/100
        sam_restaurants["Tourism", ] <- rev_Restaurants * (restaurants_expTourism)/100
        sam_restaurants["Fish",] <-  rev_Restaurants * restaurants_expLocalFish/100
        
        sam_restaurants["LMUSK", ] <- rev_Restaurants * (restaurants_MwagesUnskilled)/100 * input$restaurants_shareWorkersLocal/100
        sam_restaurants["LMSK", ] <- rev_Restaurants * (restaurants_MwagesSkilled)/100 * input$restaurants_shareWorkersLocal/100
        sam_restaurants["LFUSK", ] <- rev_Restaurants * (restaurants_FwagesUnskilled)/100 * input$restaurants_shareWorkersLocal/100
        sam_restaurants["LFSK", ] <- rev_Restaurants * (restaurants_FwagesSkilled)/100 * input$restaurants_shareWorkersLocal/100
        
        sam_restaurants["LocalG", ] <- rev_Restaurants * restaurants_expLocalTaxes/100
        sam_restaurants["G", ] <- rev_Restaurants * restaurants_expTaxes/100
        sam_restaurants["ROW", ] <- rev_Restaurants * (restaurants_expOutside/100 
                                                       +  input$restaurants_profitMargin/100 * (1-input$restaurants_shareLocallyOwned/100)
                                                       +  (restaurants_MwagesUnskilled + restaurants_MwagesSkilled + restaurants_FwagesUnskilled + restaurants_FwagesSkilled)/100 * (1 - input$restaurants_shareWorkersLocal/100))
        
        # K account used to be RESIDUAL, now calculated directly 
        # sam_rest_allbutrow <- sum(sam_restaurants)
        # sam_restaurants["K", ] <- (rev_Restaurants - sam_rest_allbutrow) * input$restaurants_shareOwned / 100
        sam_restaurants["K", ] <- rev_Restaurants * input$restaurants_profitMargin/100 * input$restaurants_shareLocallyOwned/100
        
        # NOTE: Order matters here, because Non-Ag feeds into Ag totals 
        
        
        
        # # PA Column
        # #------------------------------------
        sam_pa["Ag",] <- input$natPark_totalBudget * input$natPark_expLocalAg/100
        sam_pa["Nag",] <-input$natPark_totalBudget * ( input$natPark_expServices + input$natPark_expLocalStores)/100
        sam_pa["Tourism",] <- input$natPark_totalBudget * input$natPark_expTourism/100
        sam_pa["Fish",] <- input$natPark_totalBudget * input$natPark_expLocalFish/100
        sam_pa["ROW",] <- input$natPark_totalBudget * input$natPark_expOutside/100
        sam_pa["LMUSK", ] <- input$natPark_totalBudget * input$natPark_MwagesUnskilled/100 * input$natPark_shareWorkersLocal/100 
        sam_pa["LMSK", ] <- input$natPark_totalBudget * input$natPark_MwagesSkilled/100 * input$natPark_shareWorkersLocal/100 
        sam_pa["LFUSK", ] <- input$natPark_totalBudget * input$natPark_FwagesUnskilled/100 * input$natPark_shareWorkersLocal/100 
        sam_pa["LFSK", ] <-  input$natPark_totalBudget * input$natPark_FwagesSkilled/100 * input$natPark_shareWorkersLocal/100 
        
        # If parks send fees to government and to community revenue sharing: 
        sam_pa["ComRevSh", ] <- input$natPark_expComRevSh
        sam_pa["G", ] <- max(0, sam_tourists["PA",] - input$natPark_totalBudget - input$natPark_expComRevSh)
        
        # browser()
        
        
        # # ComRevSh Column - budget comes from a share of PA  
        # #------------------------------------
        sam_comrevsh["Ag",] <- input$natPark_expComRevSh * comRevSh_expLocalAg/100
        sam_comrevsh["Nag",] <- input$natPark_expComRevSh * (comRevSh_expServices + comRevSh_expLocalStores)/100
        sam_comrevsh["Tourism",] <- input$natPark_expComRevSh * comRevSh_expTourism/100
        sam_comrevsh["Fish",] <- input$natPark_expComRevSh * comRevSh_expLocalFish/100
        sam_comrevsh["ROW",] <- input$natPark_expComRevSh * comRevSh_expOutside/100
        
        sam_comrevsh["LMUSK", ] <-  input$natPark_expComRevSh * comRevSh_MwagesUnskilled/100 
        sam_comrevsh["LMSK", ] <-   input$natPark_expComRevSh * comRevSh_MwagesSkilled/100
        sam_comrevsh["LFUSK", ] <-  input$natPark_expComRevSh * comRevSh_FwagesUnskilled/100
        sam_comrevsh["LFSK", ] <-   input$natPark_expComRevSh * comRevSh_FwagesSkilled/100
        
        
        
        # NonAg Column
        # #------------------------------------
        # =SUM(Tourists!C22,Lodges!C17,Restaurants!C17,SUM(Households!C25:D25)+NatParkPA!C23)/(1-SUM(C8:C9))
        
        # Revenue needs to be calculated as totel_rev = exog_rev / (1-share_endog_rev) 
        # Where the endogenous revenues are Ag, Nag, and NatRes (otherwise there would be circularity in the excel sheet)
        sam_nag_totalrev <-  (sam_tourists["Nag",] + sam_lodges["Nag",] + sam_restaurants["Nag",] +
                                  sam_hhp["Nag",] + sam_hhnp["Nag",] + sam_pa["Nag",]  + sam_tourism["Nag",] + sam_comrevsh["Nag", ])/
            (1- nag_expServices/100 - nag_expLocalStores/100 - nag_expLocalAg /100 - nag_expLocalFish/100)
        
        sam_nag["Ag",] <- sam_nag_totalrev * nag_expLocalAg/100
        sam_nag["Nag",] <- sam_nag_totalrev * (nag_expServices + nag_expLocalStores)/100
        sam_nag["Tourism",] <- sam_nag_totalrev * nag_expTourism/100
        sam_nag["Fish",] <- sam_nag_totalrev * (nag_expLocalFish)/100
        
        # shares of local workers don't get rescaled, straight from inputs$
        sam_nag["LMUSK",] <- sam_nag_totalrev * (nag_MwagesUnskilled )/100 * input$nag_shareWorkersLocal/100
        sam_nag["LMSK",] <- sam_nag_totalrev * (nag_MwagesSkilled)/100 * input$nag_shareWorkersLocal/100
        sam_nag["LFUSK",] <- sam_nag_totalrev * (nag_FwagesUnskilled )/100 * input$nag_shareWorkersLocal/100
        sam_nag["LFSK",] <- sam_nag_totalrev * ( nag_FwagesSkilled)/100 * input$nag_shareWorkersLocal/100
        
        
        sam_nag["LocalG",] <- sam_nag_totalrev * (nag_expLocalTaxes)/100
        sam_nag["G",] <- sam_nag_totalrev * (nag_expTaxes)/100
        # Don't forget to funnel the shares not locally owned to the outside.  
        sam_nag["ROW",] <- sam_nag_totalrev * (nag_expOutside/100 
                                               + input$nag_profitMargin/100 * (1-input$nag_shareLocallyOwned/100)
                                               + (nag_MwagesUnskilled + nag_MwagesSkilled + nag_FwagesUnskilled + nag_FwagesSkilled)/100 * (1 - input$nag_shareWorkersLocal/100))
        
        # K account used to be the residual, now computed directly. No rescaling for that. 
        # sam_nag_allbutK <- sum(sam_nag)
        # sam_nag["K",] <- sam_nag_totalrev - sam_nag_allbutK
        sam_nag["K",] <- sam_nag_totalrev * (input$nag_profitMargin)/100 * input$nag_shareLocallyOwned/100
        
        
        # Ag Column 
        # #------------------------------------
        # =(Restaurants!C16+Lodges!C16+SUM(Households!C24:D24)+NatParkPA!C22+Nonag!C16)/(1-C6)
        # Revenue needs to be calculated as totel_rev = exog_rev / (1-share_endog_rev) 
        # Where the endogenous revenues are Ag, Nag, and NatRes (otherwise there would be circularity in the excel sheet)
        sam_ag_totalrev <-  (sam_tourists["Ag",] + sam_lodges["Ag",] + sam_restaurants["Ag",]  +
                                 sam_hhp["Ag",] + sam_hhnp["Ag",] + sam_pa["Ag",]  + sam_tourism["Ag",] + sam_comrevsh["Ag", ])/
            (1 - ag_expLocalAg/100 - ag_expLocalFish/100 - ag_expServices/100 - ag_expLocalStores/100)
        # 
        sam_ag["Ag",] <- sam_ag_totalrev * ag_expLocalAg/100
        sam_ag["Nag",] <- sam_ag_totalrev * (ag_expServices + ag_expLocalStores)/100
        sam_ag["Tourism",] <- sam_ag_totalrev * (ag_expTourism)/100
        sam_ag["Fish",] <- sam_ag_totalrev * (ag_expLocalFish)/100
        sam_ag["LMUSK",] <- sam_ag_totalrev * (ag_MwagesUnskilled )/100 * input$ag_shareWorkersLocal/100
        sam_ag["LMSK",] <- sam_ag_totalrev * (ag_MwagesSkilled)/100 * input$ag_shareWorkersLocal/100
        sam_ag["LFUSK",] <- sam_ag_totalrev * (ag_FwagesUnskilled )/100 * input$ag_shareWorkersLocal/100
        sam_ag["LFSK",] <- sam_ag_totalrev * ( ag_FwagesSkilled)/100 * input$ag_shareWorkersLocal/100
        
        sam_ag["LocalG",] <- sam_ag_totalrev * (ag_expLocalTaxes)/100
        sam_ag["G",] <- sam_ag_totalrev * (ag_expTaxes)/100
        sam_ag["ROW",] <- sam_ag_totalrev * (ag_expOutside/100 
                                             +  input$ag_profitMargin/100 * (1-input$ag_shareLocallyOwned/100)
                                             + (ag_MwagesUnskilled + ag_MwagesSkilled + ag_FwagesUnskilled + ag_FwagesSkilled)/100 * (1 - input$ag_shareWorkersLocal/100))
        
        # K account used to be the residual, but now computed directly 
        # sam_ag_allbutK <- sum(sam_ag)
        # sam_ag["K",] <- sam_ag_totalrev - sam_ag_allbutK
        sam_ag["K",] <- sam_ag_totalrev * (input$ag_profitMargin)/100 * input$ag_shareLocallyOwned/100
        
        
        
        
        # Fish Column (Natural Resources)
        # #------------------------------------
        # =(SUM(Lodges!C18,Restaurants!C18,SUM(Households!C26:D26))+NatParkPA!C24+Nonag!C18)/(1-C7)
        # Revenue needs to be calculated as totel_rev = exog_rev / (1-share_endog_rev) 
        # Where the endogenous revenues are Ag, Nag, and NatRes (otherwise there would be circularity in the excel sheet)
        sam_fish_totalrev <-  (sam_lodges["Fish",] + sam_restaurants["Fish",] +
                                   sam_hhp["Fish",] + sam_hhnp["Fish",] + sam_pa["Fish",]  + sam_tourism["Fish",] + sam_comrevsh["Fish", ])/
            (1- fish_expLocalFish/100 - fish_expLocalAg/100 - fish_expServices/100 - fish_expLocalStores/100)
        
        sam_fish["Ag",] <- sam_fish_totalrev * fish_expLocalAg/100
        sam_fish["Nag",] <- sam_fish_totalrev * (fish_expServices + fish_expLocalStores)/100
        sam_fish["Fish",] <- sam_fish_totalrev * (fish_expLocalFish)/100
        
        sam_fish["LMUSK",] <- sam_fish_totalrev * (fish_MwagesUnskilled )/100 * input$fish_shareWorkersLocal/100
        sam_fish["LMSK",] <- sam_fish_totalrev * ( fish_MwagesSkilled)/100 * input$fish_shareWorkersLocal/100
        sam_fish["LFUSK",] <- sam_fish_totalrev * (fish_FwagesUnskilled )/100 * input$fish_shareWorkersLocal/100
        sam_fish["LFSK",] <- sam_fish_totalrev * ( fish_FwagesSkilled)/100 * input$fish_shareWorkersLocal/100
        
        sam_fish["LocalG",] <- sam_fish_totalrev * (fish_expLocalTaxes)/100
        sam_fish["G",] <- sam_fish_totalrev * (fish_expTaxes)/100
        sam_fish["ROW",] <- sam_fish_totalrev * (fish_expOutside/100 
                                                 +  input$fish_profitMargin/100 * (1-input$fish_shareLocallyOwned/100)
                                                 + (fish_MwagesUnskilled + fish_MwagesSkilled + fish_FwagesUnskilled + fish_FwagesSkilled)/100 * (1 - input$fish_shareWorkersLocal)/100)
        
        # K account used to be the residual, now computed directly
        # sam_fish_allbutK <- sum(sam_fish)
        # sam_fish["K",] <- sam_fish_totalrev - sam_fish_allbutK
        sam_fish["K",] <- sam_fish_totalrev * (input$fish_profitMargin)/100 * input$fish_shareLocallyOwned/100
        # browse()
        
        # Local G Column - (unlike national G, local G functions like an activity)
        # #---------------------------------------------------------------------
        # here all revenue is exogenous so we don't worry about circularity
        sam_localG_totalrev <-  (sam_ag["LocalG",] + sam_nag["LocalG",]  + sam_tourists["LocalG",] + sam_lodges["LocalG",] + sam_restaurants["LocalG",]  +
                                     sam_hhp["LocalG",] + sam_hhnp["LocalG",] + sam_pa["LocalG",]  + sam_tourism["LocalG",] + sam_comrevsh["LocalG", ])
        
        # (1 - localG_expLocalAg/100 - localG_expLocalFish/100 - localG_expServices/100 - localG_expLocalStores/100)
        # 
        sam_localG["Ag",] <- sam_localG_totalrev * localG_expLocalAg/100
        sam_localG["Nag",] <- sam_localG_totalrev * (localG_expServices + localG_expLocalStores)/100
        sam_localG["Tourism",] <- sam_localG_totalrev * (localG_expTourism)/100
        sam_localG["Fish",] <- sam_localG_totalrev * (localG_expLocalFish)/100
        sam_localG["LMUSK",] <- sam_localG_totalrev * (localG_MwagesUnskilled )/100 * input$localG_shareWorkersLocal/100
        sam_localG["LMSK",] <- sam_localG_totalrev * (localG_MwagesSkilled)/100 * input$localG_shareWorkersLocal/100
        sam_localG["LFUSK",] <- sam_localG_totalrev * (localG_FwagesUnskilled )/100 * input$localG_shareWorkersLocal/100
        sam_localG["LFSK",] <- sam_localG_totalrev * ( localG_FwagesSkilled)/100 * input$localG_shareWorkersLocal/100
        
        # sam_localG["LocalG",] <- sam_localG_totalrev * (localG_expLocalTaxes)/100
        # sam_localG["G",] <- sam_localG_totalrev * (localG_expTaxes)/100
        sam_localG["ROW",] <- sam_localG_totalrev * (localG_expOutside/100 
                                                     # +  input$localG_profitMargin/100 * (1-input$localG_shareLocallyOwned/100)
                                                     + (localG_MwagesUnskilled + localG_MwagesSkilled + localG_FwagesUnskilled + localG_FwagesSkilled)/100 * (1 - input$localG_shareWorkersLocal/100))
        
        # No K for localG, because no profit margins
        # sam_localG["K",] <- sam_localG_totalrev * (input$localG_profitMargin)/100 * input$localG_shareLocallyOwned/100
        
        
        
        
        # G Column - based simply on NatParks + the difference of other cols
        # #---------------------------------------------------------------------
        # Total park revenue = entry fees from tourists. 
        # Total park budget = provided
        # Funding from govt = budget - revenue
        # #---------------------------------------------------------------------
        
        # sam_G["PA",] <- input$natPark_totalBudget + input$natPark_expComRevSh - sam_tourists["PA",]
        # Tourist entry fees - park expenditures, if there is deficit)
        sam_G["PA",] <- max(0, input$natPark_totalBudget - sam_tourists["PA",])
        sam_g_colsum = sum(sam_G)
        sam_g_rowsum = sam_ag["G",] + sam_nag["G",] + sam_fish["G",] + sam_hhp["G",] + sam_hhnp["G",] +
            sam_lodges["G",] + sam_restaurants["G",] + sam_tourists["G",] + sam_pa["G",]  + sam_tourism["G",] + sam_comrevsh["G", ]
        # Government payment out, if there is surplus
        sam_G["ROW", ] = max(0, sam_g_rowsum-sam_g_colsum)
        
        
        
        # L Columns (4 of them)
        # ---------------------------------------------------------------------
        # l_rowsum = sam_ag["L",] + sam_nag["L",] + sam_fish["L",] + sam_hhp["L",] + sam_hhnp["L",] +
        # sam_lodges["L",] + sam_restaurants["L",] + sam_tourists["L",] + sam_pa["L",]
        lmusk_rowsum = sam_ag["LMUSK",] + sam_nag["LMUSK",] + sam_fish["LMUSK",] + sam_hhp["LMUSK",] + sam_hhnp["LMUSK",] +
            sam_lodges["LMUSK",] + sam_restaurants["LMUSK",] + sam_tourists["LMUSK",] + sam_pa["LMUSK",] + sam_tourism["LMUSK",] + sam_comrevsh["LMUSK", ] + sam_localG["LMUSK", ]
        
        lmsk_rowsum = sam_ag["LMSK",] + sam_nag["LMSK",] + sam_fish["LMSK",] + sam_hhp["LMSK",] + sam_hhnp["LMSK",] +
            sam_lodges["LMSK",] + sam_restaurants["LMSK",] + sam_tourists["LMSK",] + sam_pa["LMSK",] + sam_tourism["LMSK",] + sam_comrevsh["LMSK", ] + sam_localG["LMSK", ]
        
        lfusk_rowsum = sam_ag["LFUSK",] + sam_nag["LFUSK",] + sam_fish["LFUSK",] + sam_hhp["LFUSK",] + sam_hhnp["LFUSK",] +
            sam_lodges["LFUSK",] + sam_restaurants["LFUSK",] + sam_tourists["LFUSK",] + sam_pa["LFUSK",] + sam_tourism["LFUSK",] + sam_comrevsh["LFUSK", ] + sam_localG["LFUSK", ]
        
        lfsk_rowsum = sam_ag["LFSK",] + sam_nag["LFSK",] + sam_fish["LFSK",] + sam_hhp["LFSK",] + sam_hhnp["LFSK",] +
            sam_lodges["LFSK",] + sam_restaurants["LFSK",] + sam_tourists["LFSK",] + sam_pa["LFSK",] + sam_tourism["LFSK",] + sam_comrevsh["LFSK", ] + sam_localG["LFSK", ]
        
        labshares_poor = make_labshares_poor(input$hhp_pop, input$hhp_pcinc,
                                             input$hhp_incShare_MWagesUnskilled, input$hhp_incShare_MWagesSkilled,
                                             input$hhp_incShare_FWagesUnskilled, input$hhp_incShare_FWagesSkilled, 
                                             input$hhp_incShareProfits, input$hhp_incShareRemits,
                                             input$hhnp_pop, input$hhnp_pcinc, 
                                             input$hhnp_incShare_MWagesUnskilled, input$hhnp_incShare_MWagesSkilled,
                                             input$hhnp_incShare_FWagesUnskilled, input$hhnp_incShare_FWagesSkilled, 
                                             input$hhnp_incShareProfits, input$hhnp_incShareRemits)
        # labshares_poor
        # allwages_hhp = (input$hhp_pop*input$hhp_pcinc)*input$hhp_incShareWages/(input$hhp_incShareWages + input$hhp_incShareRemits + input$hhp_incShareProfits)
        # allwages_hhnp = (input$hhnp_pop*input$hhnp_pcinc)*input$hhnp_incShareWages/(input$hhnp_incShareWages + input$hhnp_incShareRemits + input$hhnp_incShareProfits)
        # wagshare_hhp = allwages_hhp / (allwages_hhp + allwages_hhnp)
        # wagshare_hhnp = allwages_hhnp / (allwages_hhp + allwages_hhnp)
        sam_LMUSK["Poor",] = lmusk_rowsum * labshares_poor[1]
        sam_LMSK["Poor",] = lmsk_rowsum * labshares_poor[2]
        sam_LFUSK["Poor",] = lfusk_rowsum * labshares_poor[3]
        sam_LFSK["Poor",] = lfsk_rowsum * labshares_poor[4]
        sam_LMUSK["NonPoor",] = lmusk_rowsum * (1-labshares_poor[1])
        sam_LMSK["NonPoor",] = lmsk_rowsum * (1-labshares_poor[2])
        sam_LFUSK["NonPoor",] = lfusk_rowsum * (1-labshares_poor[3])
        sam_LFSK["NonPoor",] = lfsk_rowsum * (1-labshares_poor[4])
        
        
        # K Column 
        # ---------------------------------------------------------------------
        k_rowsum = sam_ag["K",] + sam_nag["K",] + sam_fish["K",] + sam_hhp["K",] + sam_hhnp["K",] +
            sam_lodges["K",] + sam_restaurants["K",] + sam_tourists["K",] + sam_pa["K",] + sam_tourism["K",] + sam_comrevsh["K", ]
        hhp_incShareWages = input$hhp_incShare_MWagesUnskilled + input$hhp_incShare_MWagesSkilled + input$hhp_incShare_FWagesUnskilled  + input$hhp_incShare_FWagesSkilled 
        hhnp_incShareWages = input$hhnp_incShare_MWagesUnskilled + input$hhnp_incShare_MWagesSkilled + input$hhnp_incShare_FWagesUnskilled  + input$hhnp_incShare_FWagesSkilled 
        allrents_hhp = (input$hhp_pop*input$hhp_pcinc)*input$hhp_incShareProfits/(hhp_incShareWages + input$hhp_incShareRemits + input$hhp_incShareProfits)
        allrents_hhnp = (input$hhnp_pop*input$hhnp_pcinc)*input$hhnp_incShareProfits/(hhnp_incShareWages + input$hhnp_incShareRemits + input$hhnp_incShareProfits)
        rentshare_hhp = allrents_hhp / (allrents_hhp + allrents_hhnp)
        rentshare_hhnp = allrents_hhnp / (allrents_hhp + allrents_hhnp)
        sam_K["Poor",] = k_rowsum * rentshare_hhp
        sam_K["NonPoor",] = k_rowsum * rentshare_hhnp
        # sam_K["Poor",] = k_rowsum * rentshare_hhp
        # sam_K["NonPoor",] = rentshare_hhnp
        # sam_K["Restaurants",] = allrents_hhp
        # sam_K["Lodges",] = allrents_hhnp
        
        
        # ROW Column 
        # ---------------------------------------------------------------------
        sam_ROW["Poor", ] = (input$hhp_pop*input$hhp_pcinc)*input$hhp_incShareRemits/(hhp_incShareWages + input$hhp_incShareRemits + input$hhp_incShareProfits)
        sam_ROW["NonPoor", ] = (input$hhnp_pop*input$hhnp_pcinc)*input$hhnp_incShareRemits/(hhnp_incShareWages + input$hhnp_incShareRemits + input$hhnp_incShareProfits)
        # This is the first col total to get calculated - must be done BEFORE adding the "Total" row. 
        sam_ROW["Tourists", ] = sum(sam_tourists)
        
        
        # Putting it all together into a SAM 
        #------------------------------------
        # First merge all the columns together with the correct rownames
        sam_almost <- Reduce(function(x,y) merge(x = x, y = y, by = 'rowname', all.x=TRUE),
                             list(
                                 rownames_to_column(sam_ag),
                                 rownames_to_column(sam_tourism),
                                 rownames_to_column(sam_nag),
                                 rownames_to_column(sam_fish),
                                 rownames_to_column(sam_LMUSK),
                                 rownames_to_column(sam_LMSK),
                                 rownames_to_column(sam_LFUSK),
                                 rownames_to_column(sam_LFSK),
                                 rownames_to_column(sam_K),
                                 rownames_to_column(sam_hhp),
                                 rownames_to_column(sam_hhnp),
                                 rownames_to_column(sam_restaurants),
                                 rownames_to_column(sam_lodges), 
                                 rownames_to_column(sam_tourists), 
                                 rownames_to_column(sam_pa),
                                 rownames_to_column(sam_comrevsh),
                                 rownames_to_column(sam_localG),
                                 rownames_to_column(sam_G),
                                 rownames_to_column(sam_ROW)
                             ))
        # this re-orders the rows# Ordering the rows: 
        sam_almost_reordered <- reorder_sam_rows(sam_almost, sam_row_names)
        
        # Now add the sums of rows and columns
        sam_preRAS <- sam_almost_reordered
        sam_preRAS["TotalExp",] <- colSums(sam_almost_reordered)
        sam_preRAS[,"TotalExp"] <- rowSums(sam_almost_reordered)
        sam_preRAS
    })
    
    # %%%%%%%%%%%%%%%%% RAS the SAM %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    sam_RASed <- reactive({
        # Here's the RAS procedure 
        sam_pre = sam_preRAS()
        
        # All functions for RASing  are defined above. 
        sam_rased_x <- ras_loop_x(sam_pre, 0.02)
        
        sam_rased_x
    }) 
    
    
    # %%%%%%%%%%%%%%%%% To RAS or Not to RAS?  - just for debugging %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
    # In case you want to look at no-RAS results, you can change the comment
    # Obviously this will change the multipliers - it's just for debugging
    sam <- reactive({
        sam = sam_RASed()
        # sam = sam_preRAS()
        sam
    })
    
    
    
    # %%%%%%%%%%%%%%%%% COMPUTE THE MULTIPLIERS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    multout <- reactive({ 
        sam_shares <- sam() %>% 
            rownames_to_column  
        
        rownames(sam_shares) = rownames(sam()) 
        sam_shares <- sam_shares %>%
            filter(rowname !="TotalExp") 
        sam_shares2 <- as.data.frame(apply(sam_shares[,-1],2, function(x) x /sum(x)))
        
        
        # Extract only the endogenous accounts:
        sam_shares_endog <-sam_shares2 %>%
            rownames_to_column %>% 
            filter(rowname !="ROW" & rowname !="G") %>%
            select(-c(ROW, rowname, TotalExp, G))  
        rownames(sam_shares_endog) <- colnames(sam_shares_endog)
        
        A <- as.matrix(sam_shares_endog)
        I <- diag(dim(sam_shares_endog)[1])
        IminA <- I-A
        
        sam_mult <- solve(IminA)
        rownames(sam_mult) <- rownames(sam_shares_endog)
        colnames(sam_mult) <- colnames(sam_shares_endog)
        sam_mult
    })
    
    # this just repeats the above code to output a shares matrix - not used for calculation
    mat_A <- reactive({ 
        sam_shares <- sam() %>% 
            rownames_to_column  
        
        rownames(sam_shares) = rownames(sam()) 
        sam_shares <- sam_shares %>%
            filter(rowname !="TotalExp") 
        sam_shares2 <- as.data.frame(apply(sam_shares[,-1],2, function(x) x /sum(x)))
        
        
        # Extract only the endogenous accounts:
        sam_shares_endog <-sam_shares2 %>%
            rownames_to_column %>% 
            filter(rowname !="ROW" & rowname !="G") %>%
            select(-c(ROW, rowname, TotalExp, G))  
        rownames(sam_shares_endog) <- colnames(sam_shares_endog)
        
        A <- as.matrix(sam_shares_endog)
        A
    })
    
    
    
    # %%%%%%%%%%%%%%%%% Compute some outputs %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
    # Outputs for the top of the page:
    park_name <- function() {
        return(gdata_NPName$label)
    }
    output$park_name <- renderText(park_name())
    
    # Park stats: number of visitors, revenue, budget 
    output$park_stats <- renderText(paste("The park receives", input$tourists_popMultiDay, "and has a budget of", round(input$natPark_totalBudget), "."))
    # output$park_numtour <- renderText(input$touri)
    
    # Note: you can define the whole valuebox here instead of up in the UI section.  It seems to make nicer boxes. 
    
    #### Four boxes about tourists: Number, number multi-day, length of stay, spending
    numtour <- function() {
        return(input$tourists_popMultiDay + input$tourists_popSingleDay)
    }
    output$valueBox_numtour <- renderValueBox({
        valueBox(value = format(numtour(), big.mark = ","),
                 subtitle = "Total Number of Tourists", icon = icon("person"), color= "green")
    })
    
    numtourSingle <- function() {
        return(input$tourists_popSingleDay)
    }
    output$valueBox_numtourSingle <- renderValueBox({
        valueBox(value = format(numtourSingle(), big.mark = ","),
                 subtitle = "Single-day Visitors", icon = icon("sun"))
    })
    
    numtourMulti <- function() {
        return(input$tourists_popMultiDay)
    }
    output$valueBox_numtourMulti <- renderValueBox({
        valueBox(value = format(numtourMulti(), big.mark = ","),
                 subtitle = "Multi-day Visitors", icon = icon("bed"), color = "blue")
    })
    
    # Average length of stay
    avgTouristLength <- function() {
        # Assuming single-day tourists spend 1 day (though technically it could be just a few hours)
        tourists_avgLength <-  (input$tourists_nbNights*input$tourists_popMultiDay + input$tourists_popSingleDay) / (input$tourists_popMultiDay + input$tourists_popSingleDay)
        out <-round(tourists_avgLength, digits = 2)
        return(out)
    }
    output$valueBox_avgTouristLength <- renderValueBox({
        valueBox(value = format(avgTouristLength(), big.mark = ",", scientific = FALSE) ,
                 subtitle = "Average Length of Stay", icon = icon("calendar"), color = "maroon")
    })
    
    # Average tourist spending (calculating average lodging per night including those without nights):
    avgTouristSpending_bad <- function() {
        tourists_avgSpending <- input$tourists_expRetShops + input$tourists_expOther + input$tourists_expGuidesTours +
            input$tourists_expSouvenirs +  input$tourists_expRestaurants + input$tourists_expParkEntry + 
            # note: single day tourists spend 0 on lodging, but they are still in the denominator:
            (input$tourists_nbNights*input$tourists_popMultiDay) * input$tourists_roomPrice / (input$tourists_popMultiDay + input$tourists_popSingleDay)
        out <-scales::dollar(round(tourists_avgSpending, digits = 2))
        return(out)
    }
    
    avgTouristSpendingPerPerson <- function() {
        tourists_avgSpending <- (input$tourists_expRetShops + input$tourists_expOther + input$tourists_expGuidesTours +
                                     input$tourists_expSouvenirs +  input$tourists_expRestaurants)*input$tourists_nbDays + input$tourists_expParkEntry + 
            # note: single day tourists spend 0 on lodging, but they are still in the denominator:
            (input$tourists_nbNights*input$tourists_popMultiDay) * input$tourists_roomPrice / (input$tourists_popMultiDay + input$tourists_popSingleDay)
        out <-scales::dollar(round(tourists_avgSpending, digits = 2))
        return(out)
    }
    
    avgTouristSpendingPerPersonPerDay <- function() {
        tourists_dailyexpenses <- (input$tourists_expRetShops + input$tourists_expOther + input$tourists_expGuidesTours + input$tourists_expSouvenirs +  input$tourists_expRestaurants)
        tourists_totaldays <- input$tourists_nbDays*input$tourists_popMultiDay + input$tourists_popSingleDay 
        tourists_totalparkdays <- input$tourists_popMultiDay + input$tourists_popSingleDay
        tourists_totallodging <- input$tourists_nbNights*input$tourists_popMultiDay * input$tourists_roomPrice 
        tourists_totalexpenses <- tourists_dailyexpenses*tourists_totaldays + 
            input$tourists_expParkEntry * tourists_totalparkdays + 
            tourists_totallodging
        tourists_averagePerPersonPerDay <- tourists_totalexpenses / (input$tourists_nbDays * input$tourists_popMultiDay + input$tourists_popSingleDay) 
        
        out <-scales::dollar(round(tourists_averagePerPersonPerDay, digits = 2))
        return(out)
    }
    output$valueBox_avgTouristSpending <- renderValueBox({
        valueBox(value = format(avgTouristSpendingPerPerson(), big.mark = ",", scientific = FALSE) ,
                 subtitle = "Average Tourist Spending", icon = icon("wallet"), color = "orange")
    })
    
    
    ##### Four boxes about park finances: entry fee, total entry fees, total park budget, total wage spending
    
    entryFee <- function() {
        return(scales::dollar(round(input$tourists_expParkEntry,2)))
    }
    output$valueBox_entryFee <- renderValueBox({
        # valueBox(value = format(round(input$tourists_expParkEntry), big.mark=","),
        valueBox(value = format(entryFee(), big.mark=","),      
                 subtitle = "Park Entry Fee (average)", icon = icon("ticket"), color = "olive")
    })
    
    totalEntryFees <- function() {
        return(scales::dollar(round(input$natPark_entryFees)))
    }
    output$valueBox_totalEntryFees <- renderValueBox({
        valueBox(value = format(totalEntryFees(), big.mark=","),
                 subtitle = "Total Entry Fees", icon = icon("money-bill-trend-up"), color = "yellow")
    })
    
    budget <- function() {
        return(scales::dollar(round(input$natPark_totalBudget)))
    }
    output$valueBox_budget <- renderValueBox({
        valueBox(value = format(budget(), big.mark = ",", scientific = FALSE) ,
                 subtitle = "Total Park Budget", icon = icon("file-invoice-dollar"), color = "teal")
    })
    
    parkWageBill <- function() {
        return(scales::dollar(round(input$natPark_totalBudget * 
                                        (input$natPark_MwagesUnskilled + input$natPark_MwagesSkilled + input$natPark_FwagesUnskilled + input$natPark_FwagesSkilled)/100 * 
                                        input$natPark_shareWorkersLocal/100)))
    }
    output$valueBox_parkWageBill <- renderValueBox({
        valueBox(value = format(parkWageBill(), big.mark = ",", scientific = FALSE) ,
                 subtitle = "Park Spending on Local Wages", icon = icon("person-digging"), color = "light-blue")
    })
    
    output$community_revenue_sharing <- renderText({
        crshare <- round(input$natPark_expComRevSh/input$natPark_entryFees * 100)
        crs_dollars <- format(round(crshare/100 * input$natPark_entryFees, 2), big.mark = ",") 
        
        HTML(paste("This park redistributes", crshare , "% of entry fees as a Community Revenue Sharing scheme.  
                  This generates $", crs_dollars, "in funds for the surrounding communities. (Note that not all parks have this kind of explicit redistribution scheme.)"))
    })
    
    
    
    ##### Compute some stuff for the tourism scenarios
    ################################################################################
    output$detailed_tourist_increases <- renderText({
        # new_touristsSingle <- format(round(input$tourists_popSingleDay + input$inc_touristsSingle_count), big.mark = ",")
        # new_touristsMulti <- format(round(input$tourists_popMultiDay + input$inc_touristsMulti_count), big.mark = ",") 
        new_touristsTotal <- format(round(input$tourists_popSingleDay + input$tourists_popMultiDay + input$inc_touristsTotal_count), big.mark = ",")
        new_fee <- format(round(input$tourists_expParkEntry + input$inc_fee, 2))
        
        paste("With those changes you would have", new_touristsTotal , "tourists, paying on average $", new_fee, "in entry fees.")
    })
    
    output$detailed_tourist_increases_result <- renderText({
        
        # new_touristsSingle <- input$tourists_popSingleDay + input$inc_touristsSingle_count
        # new_touristsMulti <- input$tourists_popMultiDay + input$inc_touristsMulti_count 
        new_touristsTotal <- input$tourists_popSingleDay + input$tourists_popMultiDay + input$inc_touristsTotal_count 
        new_fee <- input$tourists_expParkEntry + input$inc_fee 
        
        new_total_fees <- format(round(new_fee*(new_touristsTotal)), big.mark = ",")
        fees_increase <- format(round(new_fee*(new_touristsTotal) - input$natPark_entryFees), big.mark = ",")
        HTML(paste("Under those conditions, you would get an increase of $", fees_increase , "in entry fees, for a total of $", new_total_fees, "total revenue from entry fees."))
    })
    
    #### These ones are for the "Calculation helpers" on the Simulations tab:
    output$tourists_baseline <- renderText({
        old_total <- input$tourists_popMultiDay + input$tourists_popSingleDay
        old_spending <- input$tourists_expRetShops + input$tourists_expOther + input$tourists_expGuidesTours +
            input$tourists_expSouvenirs +  input$tourists_expRestaurants + input$tourists_expParkEntry + 
            # note: single day tourists spend 0 on lodging, but they are still in the denominator:
            (input$tourists_nbNights*input$tourists_popMultiDay) * input$tourists_roomPrice / (input$tourists_popMultiDay + input$tourists_popSingleDay)
        
        # pct_change <- format(input$pct_increase_tourists, nsmall = 2)
        # nb_increase <- (input$tourists_popMultiDay + input$tourists_popSingleDay)*(input$pct_increase_tourists)/100
        # spending_increase <- format(old_spending*nb_increase, big.mark = ",") 
        # # new_total <- format((input$tourists_popMultiDay + input$tourists_popSingleDay)*(100+input$pct_increase_tourists)/100, big.mark=",")
        # fees_increase <- format((input$tourists_popMultiDay + input$tourists_popSingleDay)*(input$pct_increase_tourists)/100 * input$tourists_expParkEntry, big.mark =",")
        # new_total_fees <- format((input$tourists_popMultiDay + input$tourists_popSingleDay)*(100+input$pct_increase_tourists)/100 * input$tourists_expParkEntry, big.mark=",")
        paste("There are currently", format(old_total, big.mark = ","), "tourists, spending on average $", format(round(old_spending), big_mark = ",") , ".")
    })
    
    output$increased_tourist_number <- renderText({
        old_total <- input$tourists_popMultiDay + input$tourists_popSingleDay
        old_spending <- input$tourists_expRetShops + input$tourists_expOther + input$tourists_expGuidesTours +
            input$tourists_expSouvenirs +  input$tourists_expRestaurants + input$tourists_expParkEntry + 
            # note: single day tourists spend 0 on lodging, but they are still in the denominator:
            (input$tourists_nbNights*input$tourists_popMultiDay) * input$tourists_roomPrice / (input$tourists_popMultiDay + input$tourists_popSingleDay)
        
        pct_increase_tourists <- format(input$pct_increase_tourists, nsmall = 2)
        pct_increase_avg_spending <- format(input$pct_increase_avg_spending, nsmall = 2)
        # nb_increase <- (input$tourists_popMultiDay + input$tourists_popSingleDay)*(input$pct_increase_tourists)/100
        new_total <- old_total * (1+input$pct_increase_tourists/100)
        new_spending <- old_spending * (1+input$pct_increase_avg_spending/100)
        spending_increase <- format( new_total*new_spending - old_total*old_spending, big.mark = ",") 
        # new_total <- format((input$tourists_popMultiDay + input$tourists_popSingleDay)*(100+input$pct_increase_tourists)/100, big.mark=",")
        # fees_increase <- format((input$tourists_popMultiDay + input$tourists_popSingleDay)*(input$pct_increase_tourists)/100 * input$tourists_expParkEntry, big.mark =",")
        # new_total_fees <- format((input$tourists_popMultiDay + input$tourists_popSingleDay)*(100+input$pct_increase_tourists)/100 * input$tourists_expParkEntry, big.mark=",")
        paste("Changing the number of tourists by", pct_increase_tourists , "% and the average spending per tourists by", pct_increase_avg_spending, "% would result in $",  
              spending_increase,"increase in spending.")
    })
    
    
    
    
    
    
    # Average tourist spending (calculating average lodging per night including those without nights): 
    # output$tourists_avgSpending <- renderText({
    #     tourists_avgSpending <- input$tourists_expRetShops + input$tourists_expOther + input$tourists_expGuidesTours +
    #         input$tourists_expSouvenirs +  input$tourists_expRestaurants + input$tourists_expParkEntry + 
    #         (input$tourists_nbNights*input$tourists_popMultiDay) * input$tourists_roomPrice / (input$tourists_popMultiDay + input$tourists_popSingleDay)
    #     out <-round(tourists_avgSpending, digits = 2) 
    #     return(out)})
    # # output$tourists_avgDays <- renderText({tourists_avgDays})
    
    
    # Matrices: 
    output$sam_preRAS <- renderTable({sam_preRAS()}, rownames=TRUE, digits=0, striped = T)
    output$sam_RASed <- renderTable({sam_RASed()}, rownames=TRUE, digits=0, striped = T)
    output$matA <- renderTable({mat_A()}, rownames=TRUE, digits=2, striped = T)
    output$mult <- renderTable({multout()}, rownames=TRUE, digits=2, striped = T)
    
    # browser()
    
    # %%%%%%%%%%%%%%%%% These multiplier computations can be sped up, I'm sure %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    # Multiplier values to report in tiles
    # Create functions to pass to download function
    
    # Total production multiplier
    totalmult <- function() {
        rows_to_sum = c("Ag","Nag","Fish","Tourism", "Restaurants","Lodges")
        mults <- multout()
        total <- sum(mults[rownames(mults) %in% rows_to_sum,"Tourists"])
        scales::dollar(total)
    }
    
    output$totalmult <- renderText({
        totalmult()
    })
    
    # Of which: Tourism-related production multiplier
    touractmult <- function() {
        rows_to_sum = c("Tourism", "Restaurants","Lodges")
        mults <- multout()
        total <- sum(mults[rownames(mults) %in% rows_to_sum,"Tourists"])
        scales::dollar(total)
    }
    
    output$touractmult <- renderText({
        touractmult()
    })
    
    # Of which: Non-tourism-related production multiplier
    nontouractmult <- function() {
        rows_to_sum = c("Ag","Nag","Fish")
        mults <- multout()
        total <- sum(mults[rownames(mults) %in% rows_to_sum,"Tourists"])
        scales::dollar(total)
    }
    
    output$nontouractmult <- renderText({
        nontouractmult()
    })
    
    # Total income (GDP) multiplier
    gdpmult <- function() {
        rows_to_sum = c("Poor","NonPoor")
        mults <- multout()
        total <- sum(mults[rownames(mults) %in% rows_to_sum,"Tourists"])
        scales::dollar(total)
    }
    
    output$gdpmult <- renderText({
        gdpmult()
    })
    
    labmult <- function() {
        rows_to_sum = c("LMUSK", "LFUSK", "LMSK", "LFSK")
        mults <- multout()
        total <- sum(mults[rownames(mults) %in% rows_to_sum,"Tourists"])
        round(total,2)
        scales::dollar(total)
    }
    
    output$labmult <- renderText({
        labmult()
    }) 
    
    capmult <- function() {
        rows_to_sum = c("K")
        mults <- multout()
        total <- sum(mults[rownames(mults) %in% rows_to_sum,"Tourists"])
        round(total,2)
        scales::dollar(total)
    }
    
    output$capmult <- renderText({
        capmult()
    })
    
    poormult <- function() {
        rows_to_sum = c("Poor")
        mults <- multout()
        total <- sum(mults[rownames(mults) %in% rows_to_sum,"Tourists"])
        round(total,2)
        scales::dollar(total)
    }
    
    output$poormult <- renderText({
        poormult()
    }) 
    
    nonpoormult <- function() {
        mults <- multout()
        total <- mults["NonPoor","Tourists"]
        round(total,2)
        scales::dollar(total)
    }
    
    output$nonpoormult <- renderText({
        nonpoormult()
    }) 
    
    ## NET OF PARK FEES MULTIPLIERS ############################################
    
    # Average share of tourist spending going to park fees (used to adjust net of park fees multipliers):
    pf <- function() {
      tot_spending_per_tourist <- input$tourists_expRetShops + input$tourists_expOther + input$tourists_expGuidesTours +
                                    input$tourists_expSouvenirs +  input$tourists_expRestaurants + input$tourists_expParkEntry + 
                                    (input$tourists_nbNights*input$tourists_popMultiDay) * input$tourists_roomPrice / (input$tourists_popMultiDay + input$tourists_popSingleDay)
      pf <- input$tourists_expParkEntry / tot_spending_per_tourist
      return(pf)
    }
    
    totalmult_npf <- function() {
      rows_to_sum = c("Ag","Nag","Fish","Tourism", "Restaurants","Lodges")
      mults <- multout()
      total <- sum(mults[rownames(mults) %in% rows_to_sum,"Tourists"])
      total <- total*(1/(1-pf()))
      scales::dollar(total)
    }
    
    output$totalmult_npf <- renderText({
      totalmult_npf()
    })
    
    # Of which: Tourism-related production multiplier
    touractmult_npf <- function() {
      rows_to_sum = c("Tourism", "Restaurants","Lodges")
      mults <- multout()
      total <- sum(mults[rownames(mults) %in% rows_to_sum,"Tourists"])
      total <- total*(1/(1-pf()))
      scales::dollar(total)
    }
    
    output$touractmult_npf <- renderText({
      touractmult_npf()
    })
    
    # Of which: Non-tourism-related production multiplier
    nontouractmult_npf <- function() {
      rows_to_sum = c("Ag","Nag","Fish")
      mults <- multout()
      total <- sum(mults[rownames(mults) %in% rows_to_sum,"Tourists"])
      total <- total*(1/(1-pf()))
      scales::dollar(total)
    }
    
    output$nontouractmult_npf <- renderText({
      nontouractmult_npf()
    })
    
    # Total income (GDP) multiplier
    gdpmult_npf <- function() {
      rows_to_sum = c("Poor","NonPoor")
      mults <- multout()
      total <- sum(mults[rownames(mults) %in% rows_to_sum,"Tourists"])
      round(total,2)
      total <- total*(1/(1-pf()))
      scales::dollar(total)
    }
    
    output$gdpmult_npf <- renderText({
      gdpmult_npf()
    })
    
    labmult_npf <- function() {
      rows_to_sum = c("LMUSK", "LFUSK", "LMSK", "LFSK")
      mults <- multout()
      total <- sum(mults[rownames(mults) %in% rows_to_sum,"Tourists"])
      round(total,2)
      total <- total*(1/(1-pf()))
      scales::dollar(total)
    }
    
    output$labmult_npf <- renderText({
      labmult_npf()
    }) 
    
    capmult_npf <- function() {
      rows_to_sum = c("K")
      mults <- multout()
      total <- sum(mults[rownames(mults) %in% rows_to_sum,"Tourists"])
      round(total,2)
      total <- total*(1/(1-pf()))
      scales::dollar(total)
    }
    
    output$capmult_npf <- renderText({
      capmult_npf()
    })
    
    poormult_npf <- function() {
      rows_to_sum = c("Poor")
      mults <- multout()
      total <- sum(mults[rownames(mults) %in% rows_to_sum,"Tourists"])
      round(total,2)
      total <- total*(1/(1-pf()))
      scales::dollar(total)
    }
    
    output$poormult_npf <- renderText({
      poormult_npf()
    }) 
    
    nonpoormult_npf <- function() {
      mults <- multout()
      total <- mults["NonPoor","Tourists"]
      round(total,2)
      total <- total*(1/(1-pf()))
      scales::dollar(total)
    }
    
    output$nonpoormult_npf <- renderText({
      nonpoormult_npf()
    }) 

    ############################################################################
    
    # Sam multipliers
    output$mult_barplot <- renderPlot({
        rows_to_plot = c("Ag","Nag","Fish","Restaurants","Lodges")
        mults <- multout()
        bar <- barplot(mults[rownames(mults) %in% rows_to_plot,"Tourists"])
        bar
    })
    
    
    # Total gdp: 
    output$gdp <- renderText({
        rows_to_sum = c("Poor","NonPoor")
        finc <- samout()
        total <- sum(finc[rownames(finc) %in% rows_to_sum,"K"]) + sum(finc[rownames(finc) %in% rows_to_sum,"L"])
        scales::dollar(total)
    })
    
    
    
    
    # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    # %%%%%%%%%%%%%%%%% Compute some effects of tourist spending, in $ %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
    # Functions to make the plots
    # ---------------------------------
    
    make_production_multipliers_plot <- function(column, input_value){
        activity_types = c("Tourism-related Activities", "Non-Tourism-related Activities")
        rows_to_plot = c("Lodges", "Restaurants", "Tourism", "Ag", "Nag", "Fish")
        mults <- multout()
        totals <- data.frame(round(input_value*(mults[rownames(mults) %in% rows_to_plot, column]), digits=2))
        colnames(totals) <- "total_prod"
        totals$cats = rownames(totals)
        totals$activity_type <- with(totals, ifelse(cats=="Lodges" | cats=="Restaurants" | cats=="Tourism", "Tourism-related Activities", "Non-Tourism-related Activities"))
        # This refactoring makes sure the cats are displayed in the right order (not alphabetical)
        totals$cats <- factor(totals$cats, levels = rows_to_plot)
        totals$activity_type <- factor(totals$activity_type, levels = activity_types)
        bar <- ggplot(totals, aes(x = cats, y = total_prod, fill = cats)) +
            geom_bar(stat = "sum", width = 1) +
            xlab("") + ylab("Additional Production Value ($)") +
            geom_text(aes(label = total_prod), vjust = -0.2) +
            ggtitle(" ... ON PRODUCTION") +
            facet_wrap(~activity_type, strip.position = "bottom", scales = "free_x") +
            theme_bw() +
            theme(plot.title = element_text(hjust=0.5, size=14, face="bold", margin=margin(5, 0, 5, 0)),
                  axis.text.x = element_text(size=10),
                  axis.text.y = element_text(size=10),
                  axis.title.x = element_text(size=12, margin = margin(t=5, r=0, b=0, l=0)),
                  axis.title.y = element_text(size=12, margin = margin(t=0, r=10, b=0, l=0)),
                  axis.ticks.x = element_blank(),
                  axis.ticks.y = element_blank(),
                  legend.position = "none",
                  strip.background = element_blank(),
                  strip.placement = "outside",
                  strip.text = element_text(size=12))
        bar
    }
    
    make_income_multipliers_plot <- function(column, input_value){
        mults <- multout()
        inctotals <- data.frame(round(input_value*(mults[rownames(mults) %in% c("Poor", "NonPoor"), column]), digits=2))
        colnames(inctotals) <- "total_inc"
        inctotals$cats = rownames(inctotals)
        bar <- ggplot(inctotals, aes(x = cats, y=total_inc, fill=cats )) +
            geom_bar(stat = "sum", width = 1) +
            xlab("Households") + ylab("Additional Income ($)") + 
            geom_text(aes(label = total_inc), vjust = -0.2) +
            ggtitle("... ON INCOMES ") +
            theme_bw() +
            theme(plot.title = element_text(hjust=0.5, size=14, face="bold", margin=margin(5, 0, 5, 0)),
                  axis.text.x = element_text(size=10),
                  axis.text.y = element_text(size=10),
                  axis.title.x = element_text(size=12, margin = margin(t=5, r=0, b=0, l=0)),
                  axis.title.y = element_text(size=12, margin = margin(t=0, r=10, b=0, l=0)),
                  axis.ticks.x = element_blank(),
                  axis.ticks.y = element_blank(),
                  legend.position = "none")
        bar
    }
    
    make_labor_multipliers_plot <- function(column, input_value){
        rows_to_plot = c("LFUSK", "LMUSK", "LFSK", "LMSK")
        mults <- multout()
        labtotals <- data.frame(round(input_value*(mults[rownames(mults) %in% rows_to_plot, column]), digits=2))
        colnames(labtotals) <- "total_lab"
        labtotals$cats = rownames(labtotals)
        
        # Rename the categories
        labtotals$cats <- forcats::fct_recode(labtotals$cats,
                                              "Female Unskilled" = "LFUSK",
                                              "Male Unskilled" = "LMUSK",
                                              "Female Skilled" = "LFSK",
                                              "Male Skilled" = "LMSK")
        
        # Set the order of categories
        labtotals$cats <- factor(labtotals$cats, levels = c("Female Unskilled", "Male Unskilled", "Female Skilled", "Male Skilled"))
        
        bar <- ggplot(labtotals, aes(x = cats, y=total_lab, fill=cats )) +
            geom_bar(stat = "sum", width = 1) +
            xlab("Labor Categories") + ylab("Additional Labor Income ($)") + 
            geom_text(aes(label = total_lab), vjust = -0.2) +
            ggtitle("... ON LABOR INCOME ") +
            theme_bw() +
            theme(plot.title = element_text(hjust=0.5, size=14, face="bold", margin=margin(5, 0, 5, 0)),
                  axis.text.x = element_text(size=10),
                  axis.text.y = element_text(size=10),
                  axis.title.x = element_text(size=12, margin = margin(t=5, r=0, b=0, l=0)),
                  axis.title.y = element_text(size=12, margin = margin(t=0, r=10, b=0, l=0)),
                  axis.ticks.x = element_blank(),
                  axis.ticks.y = element_blank(),
                  legend.position = "none")
        # Adding geom_bracket requires a non-standard package
        # geom_bracket(aes(xmin = 1, xmax = 2, y = max(total_lab) + 2, label = "unskilled labor"),
        #              label.y.npc = 1.2, label.size = 4)
        bar
    }
    
    # One more graph type, but only for the first row: 
    make_ComPA_multipliers_plot <- function(column, input_value){
        rows_to_plot = c("ComRevSh", "PA")
        mults <- multout()
        inctotals <- data.frame(round(input_value*(mults[rownames(mults) %in% rows_to_plot, column]), digits=2))
        colnames(inctotals) <- "total_inc"
        inctotals$cats = rownames(inctotals)
        bar <- ggplot(inctotals, aes(x = cats, y=total_inc, fill=cats )) +
            geom_bar(stat="sum") +
            xlab("Households") + ylab("Additional Income ($)") +
            geom_text(aes(label = total_inc), vjust = -0.2) +
            ggtitle("... ON COMMUNITY AND PARK EARNINGS") +
            theme(plot.title = element_text(hjust=0.5, size=14, face="bold", margin=margin(5, 0, 5, 0)),
                  axis.text.x = element_text(size=10),
                  axis.text.y = element_text(size=10),
                  axis.title.x = element_text(size=12, margin = margin(t=5, r=0, b=0, l=0)),
                  axis.title.y = element_text(size=12, margin = margin(t=0, r=10, b=0, l=0)),
                  axis.ticks.x = element_blank(),
                  axis.ticks.y = element_blank(),
                  legend.position = "none")
        # return the bar plot
        bar
    }
    
    
    # output$sim_totallab <- renderPlot({
    #     mults <- multout()
    #     labtotals <- data.frame(round(input$sim_TouristSpending*(mults[rownames(mults) %in% c("LMUSK", "LMSK", "LFUSK", "LFSK"),"Tourists"]), digits=2))
    #     colnames(labtotals) <- "total_lab"
    #     labtotals$cats = c("LMUSK", "LMSK", "LFUSK", "LFSK")
    #     bar <- ggplot(labtotals, aes(x = cats, y=total_lab, fill=cats )) +
    #         geom_bar(stat="sum") +
    #         xlab("Labor Categories") + ylab("Additional Labor Income ($)") + 
    #         geom_text(aes(label = total_lab), vjust = -0.2) +
    #         ggtitle("... ON LABOR INCOME ") + 
    #         theme(plot.title = element_text(hjust = 0.5), 
    #               legend.position = "none")
    #     bar
    
    # make_production_multipliers_plot3 <- function(column, input_value){
    #     rows_to_plot = c("Ag","Nag", "Restaurants","Lodges")
    #     mults <- multout()
    #     totals <- data.frame(round(input_value*(mults[rownames(mults) %in% rows_to_plot, column]), digits=2))
    #     colnames(totals) <- "total_prod"
    #     totals$cats = rows_to_plot
    #     bar <- ggplot(totals, aes(x = cats, y=total_prod, fill=cats )) +
    #         geom_bar(stat="sum") +
    #         xlab("Categories") + ylab("Additional Production Value ($)") +
    #         geom_text(aes(label = total_prod), vjust = -0.2) +
    #         ggtitle(" ... ON PRODUCTION") +
    #         theme(plot.title = element_text(hjust = 0.5),
    #               legend.position = "none")
    #     bar
    # }
    
    # spending_test <- reactive({input$sim_TouristSpending})
    # output$simCom_totalprod <- make_production_multipliers_plot("ComRevSh", spending_test())
    
    
    # Production plots need to be split into two sub-plots: Tourism Activities vs Non-Tourism activities
    
    # Make all the simulation PRODUCTION plots to output
    output$simTourists_totalprod <- renderPlot({
        make_production_multipliers_plot("Tourists", input$sim_TouristSpending)
    })
    output$simPA_totalprod <- renderPlot({
        make_production_multipliers_plot("PA", input$sim_PASpending)
    })
    output$simComRevSh_totalprod <- renderPlot({
        make_production_multipliers_plot("ComRevSh", input$sim_ComRevShSpending)
    })
    output$simAg_totalprod <- renderPlot({
        make_production_multipliers_plot("Ag", input$sim_AgSpending)
    })
    output$simNag_totalprod <- renderPlot({
        make_production_multipliers_plot("Nag", input$sim_NagSpending)
    })
    output$simLFUSK_totalprod <- renderPlot({
        make_production_multipliers_plot("LFUSK", input$sim_LFUSKSpending)
    })
    output$simLMUSK_totalprod <- renderPlot({
        make_production_multipliers_plot("LMUSK", input$sim_LMUSKSpending)
    })
    output$simLFSK_totalprod <- renderPlot({
        make_production_multipliers_plot("LFSK", input$sim_LFSKSpending)
    })
    output$simLMSK_totalprod <- renderPlot({
        make_production_multipliers_plot("LMSK", input$sim_LMSKSpending)
    })
    output$simLocalG_totalprod <- renderPlot({
        make_production_multipliers_plot("LocalG", input$sim_LocalGSpending)
    })
    
    
    # Functions to pass to download to create PRODUCTION plots in PDF report
    reportplot_prod1 <- function() {
        make_production_multipliers_plot("Tourists", input$sim_TouristSpending)
    }
    reportplot_prod2 <- function() {
        make_production_multipliers_plot("PA", input$sim_PASpending)
    }
    reportplot_prod3 <- function() {
        make_production_multipliers_plot("ComRevSh", input$sim_ComRevShSpending)
    }
    reportplot_prod4 <- function() {
        make_production_multipliers_plot("Ag", input$sim_AgSpending)
    }
    reportplot_prod5 <- function() {
        make_production_multipliers_plot("Nag", input$sim_NagSpending)
    }
    reportplot_prod6 <- function() {
        make_production_multipliers_plot("LFUSK", input$sim_LFUSKSpending)
    }
    reportplot_prod7 <- function() {
        make_production_multipliers_plot("LMUSK", input$sim_LMUSKSpending)
    }
    reportplot_prod8 <- function() {
        make_production_multipliers_plot("LFSK", input$sim_LFSKSpending)
    }
    reportplot_prod9 <- function() {
        make_production_multipliers_plot("LMSK", input$sim_LMSKSpending)
    }
    reportplot_prod10 <- function() {
        make_production_multipliers_plot("LocalG", input$sim_LocalGSpending)
    }
    
    # Make all the simulation INCOME plots to output 
    output$simTourists_incomes <- renderPlot({
        make_income_multipliers_plot("Tourists", input$sim_TouristSpending)
    })
    output$simPA_incomes <- renderPlot({
        make_income_multipliers_plot("PA", input$sim_PASpending)
    })
    output$simComRevSh_incomes <- renderPlot({
        make_income_multipliers_plot("ComRevSh", input$sim_ComRevShSpending)
    })
    output$simAg_incomes <- renderPlot({
        make_income_multipliers_plot("Ag", input$sim_AgSpending)
    })
    output$simNag_incomes <- renderPlot({
        make_income_multipliers_plot("Nag", input$sim_NagSpending)
    })
    output$simLFUSK_incomes <- renderPlot({
        make_income_multipliers_plot("LFUSK", input$sim_LFUSKSpending)
    })
    output$simLMUSK_incomes <- renderPlot({
        make_income_multipliers_plot("LMUSK", input$sim_LMUSKSpending)
    })
    output$simLFSK_incomes <- renderPlot({
        make_income_multipliers_plot("LFSK", input$sim_LFSKSpending)
    })
    output$simLMSK_incomes <- renderPlot({
        make_income_multipliers_plot("LMSK", input$sim_LMSKSpending)
    })
    output$simLocalG_incomes <- renderPlot({
        make_income_multipliers_plot("LocalG", input$sim_LocalGSpending)
    })
    
    
    # Functions to pass to download to create INCOME plots in PDF report
    reportplot_inc1 <- function() {
        make_income_multipliers_plot("Tourists", input$sim_TouristSpending)
    }
    reportplot_inc2 <- function() {
        make_income_multipliers_plot("PA", input$sim_PASpending)
    }
    reportplot_inc3 <- function() {
        make_income_multipliers_plot("ComRevSh", input$sim_ComRevShSpending)
    }
    reportplot_inc4 <- function() {
        make_income_multipliers_plot("Ag", input$sim_AgSpending)
    }
    reportplot_inc5 <- function() {
        make_income_multipliers_plot("Nag", input$sim_NagSpending)
    }
    reportplot_inc6 <- function() {
        make_income_multipliers_plot("LFUSK", input$sim_LFUSKSpending)
    }
    reportplot_inc7 <- function() {
        make_income_multipliers_plot("LMUSK", input$sim_LMUSKSpending)
    }
    reportplot_inc8 <- function() {
        make_income_multipliers_plot("LFSK", input$sim_LFSKSpending)
    }
    reportplot_inc9 <- function() {
        make_income_multipliers_plot("LMSK", input$sim_LMSKSpending)
    }
    reportplot_inc10 <- function() {
        make_income_multipliers_plot("LocalG", input$sim_LocalGSpending)
    }
    
    # Make all the simulation LABOR INCOME plots to output 
    output$simTourists_labor <- renderPlot({
        make_labor_multipliers_plot("Tourists", input$sim_TouristSpending)
    })
    output$simPA_labor <- renderPlot({
        make_labor_multipliers_plot("PA", input$sim_PASpending)
    })
    output$simComRevSh_labor <- renderPlot({
        make_labor_multipliers_plot("ComRevSh", input$sim_ComRevShSpending)
    })
    output$simAg_labor <- renderPlot({
        make_labor_multipliers_plot("Ag", input$sim_AgSpending)
    })
    output$simNag_labor <- renderPlot({
        make_labor_multipliers_plot("Nag", input$sim_NagSpending)
    })
    output$simLFUSK_labor <- renderPlot({
        make_labor_multipliers_plot("LFUSK", input$sim_LFUSKSpending)
    })
    output$simLMUSK_labor <- renderPlot({
        make_labor_multipliers_plot("LMUSK", input$sim_LMUSKSpending)
    })
    output$simLFSK_labor <- renderPlot({
        make_labor_multipliers_plot("LFSK", input$sim_LFSKSpending)
    })
    output$simLMSK_labor <- renderPlot({
        make_labor_multipliers_plot("LMSK", input$sim_LMSKSpending)
    })
    output$simLocalG_labor <- renderPlot({
        make_labor_multipliers_plot("LocalG", input$sim_LocalGSpending)
    })
    
    # Functions to pass to download to create LABOR INCOME plots in PDF report
    reportplot_linc1 <- function() {
        make_labor_multipliers_plot("Tourists", input$sim_TouristSpending)
    }
    reportplot_linc2 <- function() {
        make_labor_multipliers_plot("PA", input$sim_PASpending)
    }
    reportplot_linc3 <- function() {
        make_labor_multipliers_plot("ComRevSh", input$sim_ComRevShSpending)
    }
    reportplot_linc4 <- function() {
        make_labor_multipliers_plot("Ag", input$sim_AgSpending)
    }
    reportplot_linc5 <- function() {
        make_labor_multipliers_plot("Nag", input$sim_NagSpending)
    }
    reportplot_linc6 <- function() {
        make_labor_multipliers_plot("LFUSK", input$sim_LFUSKSpending)
    }
    reportplot_linc7 <- function() {
        make_labor_multipliers_plot("LMUSK", input$sim_LMUSKSpending)
    }
    reportplot_linc8 <- function() {
        make_labor_multipliers_plot("LFSK", input$sim_LFSKSpending)
    }
    reportplot_linc9 <- function() {
        make_labor_multipliers_plot("LMSK", input$sim_LMSKSpending)
    }
    reportplot_linc10 <- function() {
        make_labor_multipliers_plot("LocalG", input$sim_LocalGSpending)
    }
    
    # Make last plot, just for the first row
    output$simTourists_ComPA <- renderPlot({ 
        make_ComPA_multipliers_plot("Tourists", input$sim_TouristSpending)
    })
    
    # Functions to pass to download to create last plot in PDF report
    reportplot_earn1 <- function() {
        make_ComPA_multipliers_plot("Tourists", input$sim_TouristSpending)
    }
    
    
    output$report1 <- downloadHandler(
        filename = "report1.pdf",
        content = function(file) {
            # Copy the report file to a temporary directory before processing it, in
            # case we don't have write permissions to the current working dir (which
            # can happen when deployed).
            tempReport <- file.path(tempdir(), "report1.Rmd")
            file.copy("report1.Rmd", tempReport, overwrite = TRUE)
            
            # Set up parameters to pass to Rmd document
            params <- list(totalmult = totalmult, gdpmult = gdpmult, labmult = labmult,
                           capmult = capmult, poormult = poormult, nonpoormult = nonpoormult,
                           touractmult=touractmult, nontouractmult=nontouractmult,
                           park_name=park_name, numtour=numtour, numtourSingle=numtourSingle,
                           numtourMulti=numtourMulti, avgTouristLength=avgTouristLength,
                           avgTouristSpending=avgTouristSpending, entryFee=entryFee,
                           totalEntryFees=totalEntryFees, budget=budget, parkWageBill=parkWageBill)
            
            # Knit the document, passing in the `params` list, and eval it in a
            # child of the global environment (this isolates the code in the document
            # from the code in this app).
            rmarkdown::render(tempReport, output_file = file,
                              params = params,
                              envir = new.env(parent = globalenv())
            )
        }
    )
    
    output$report2 <- downloadHandler(
        filename = "report2.pdf",
        content = function(file) {
            # Copy the report file to a temporary directory before processing it, in
            # case we don't have write permissions to the current working dir (which
            # can happen when deployed).
            tempReport <- file.path(tempdir(), "report2.Rmd")
            file.copy("report2.Rmd", tempReport, overwrite = TRUE)
            
            # Set up parameters to pass to Rmd document
            params <- list(prod1 = reportplot_prod1, prod2 = reportplot_prod2, prod3 = reportplot_prod3,
                           prod4 = reportplot_prod4, prod5 = reportplot_prod5, prod6 = reportplot_prod6,
                           prod7 = reportplot_prod7, prod8 = reportplot_prod8, prod9 = reportplot_prod9,
                           prod10 = reportplot_prod10,
                           inc1 = reportplot_inc1, inc2 = reportplot_inc2, inc3 = reportplot_inc3,
                           inc4 = reportplot_inc4, inc5 = reportplot_inc5, inc6 = reportplot_inc6,
                           inc7 = reportplot_inc7, inc8 = reportplot_inc8, inc9 = reportplot_inc9,
                           inc10 = reportplot_inc10,
                           linc1 = reportplot_linc1, linc2 = reportplot_linc2, linc3 = reportplot_linc3,
                           linc4 = reportplot_linc4, linc5 = reportplot_linc5, linc6 = reportplot_linc6,
                           linc7 = reportplot_linc7, linc8 = reportplot_linc8, linc9 = reportplot_linc9,
                           linc10 = reportplot_linc10,
                           earn1 = reportplot_earn1, sim_TouristSpending = input$sim_TouristSpending,
                           sim_PASpending = input$sim_PASpending,
                           sim_ComRevShSpending = input$sim_ComRevShSpending,
                           sim_AgSpending = input$sim_AgSpending,
                           sim_NagSpending = input$sim_NagSpending,
                           sim_LFUSKSpending = input$sim_LFUSKSpending,
                           sim_LMUSKSpending = input$sim_LMUSKSpending,
                           sim_LFSKSpending = input$sim_LFSKSpending,
                           sim_LMSKSpending = input$sim_LMSKSpending,
                           sim_LocalGSpending = input$sim_LocalGSpending)
            
            # Knit the document, passing in the `params` list, and eval it in a
            # child of the global environment (this isolates the code in the document
            # from the code in this app).
            rmarkdown::render(tempReport, output_file = file,
                              params = params,
                              envir = new.env(parent = globalenv())
            )
        }
    )
    
    output$doc_to_display <- renderUI({
        includeHTML("instructions.html")
    })
    
    observeEvent(input$toTop1, {
        shinyjs::runjs("window.scrollTo(0, 0)")
    })
    observeEvent(input$toTop2, {
        shinyjs::runjs("window.scrollTo(0, 0)")
    })
    observeEvent(input$toTop3, {
        shinyjs::runjs("window.scrollTo(0, 0)")
    })
    observeEvent(input$toTop4, {
        shinyjs::runjs("window.scrollTo(0, 0)")
    })
    observeEvent(input$toTop5, {
        shinyjs::runjs("window.scrollTo(0, 0)")
    })
    observeEvent(input$toTop6, {
        shinyjs::runjs("window.scrollTo(0, 0)")
    })
    observeEvent(input$toTop7, {
        shinyjs::runjs("window.scrollTo(0, 0)")
    })
    observeEvent(input$toTop8, {
        shinyjs::runjs("window.scrollTo(0, 0)")
    })
    observeEvent(input$toTop9, {
        shinyjs::runjs("window.scrollTo(0, 0)")
    })
    observeEvent(input$toTop10, {
        shinyjs::runjs("window.scrollTo(0, 0)")
    })
    
    
    # output$download1.Excel <- downloadHandler(
    # 
    #   filename = function() { paste("data.xlsx")},
    #   
    #   content = function(file){
    #     Results_Workbook <- createWorkbook(type='xlsx')
    # 
    #     mults <- multout()
    #     sheet.1 <- createSheet(Results_Workbook, sheetName = "Multipliers")
    #     addDataFrame(mults, sheet=sheet.1, startRow=1, startColumn=1,row.names=TRUE)
    #     
    #     sam <- sam_RASed()
    #     sheet.2 <- createSheet(Results_Workbook, sheetName = "SAM (RASed)")
    #     addDataFrame(sam, sheet=sheet.2, startRow=1, startColumn=1,row.names=TRUE)
    #     
    #     prod1 = reportplot_prod1()
    #     inc1 = reportplot_inc1()
    #     linc1 = reportplot_linc1()
    #     earn1 = reportplot_earn1()
    #     sheet.3 <- createSheet(Results_Workbook, sheetName = "Tourism Spending")
    #     ggsave("prod1.jpeg", prod1, device="jpeg", dpi = 80)
    #     ggsave("inc1.jpeg", inc1, device="jpeg", dpi = 80)
    #     ggsave("linc1.jpeg", linc1, device="jpeg", dpi = 80)
    #     ggsave("earn1.jpeg", earn1, device="jpeg", dpi = 80)
    #     addPicture(file = paste0(getwd(),"/prod1.jpeg"),sheet=sheet.3,scale=1,startRow=2,startColumn=2)
    #     addPicture(file = paste0(getwd(),"/inc1.jpeg"),sheet=sheet.3,scale=1,startRow=2,startColumn=12)
    #     addPicture(file = paste0(getwd(),"/linc1.jpeg"),sheet=sheet.3,scale=1,startRow=2,startColumn=22)
    #     # addPicture(file = paste0(getwd(),"/earn1.jpeg"),sheet=sheet.3,scale=1,startRow=2,startColumn=32)
    #     
    #     prod2 = reportplot_prod2()
    #     inc2 = reportplot_inc2()
    #     linc2 = reportplot_linc2()
    #     sheet.4 <- createSheet(Results_Workbook, sheetName = "Park Spending")
    #     ggsave("prod2.jpeg", prod2, device="jpeg", dpi = 80)
    #     ggsave("inc2.jpeg", inc2, device="jpeg", dpi = 80)
    #     ggsave("linc2.jpeg", linc2, device="jpeg", dpi = 80)
    #     addPicture(file = paste0(getwd(),"/prod2.jpeg"),sheet=sheet.4,scale=1,startRow=2,startColumn=2)
    #     addPicture(file = paste0(getwd(),"/inc2.jpeg"),sheet=sheet.4,scale=1,startRow=2,startColumn=12)
    #     addPicture(file = paste0(getwd(),"/linc2.jpeg"),sheet=sheet.4,scale=1,startRow=2,startColumn=22)
    #     
    #     prod3 = reportplot_prod3()
    #     inc3 = reportplot_inc3()
    #     linc3 = reportplot_linc3()
    #     sheet.5 <- createSheet(Results_Workbook, sheetName = "Community Spending")
    #     ggsave("prod3.jpeg", prod3, device="jpeg", dpi = 80)
    #     ggsave("inc3.jpeg", inc3, device="jpeg", dpi = 80)
    #     ggsave("linc3.jpeg", linc3, device="jpeg", dpi = 80)
    #     addPicture(file = paste0(getwd(),"/prod3.jpeg"),sheet=sheet.5,scale=1,startRow=2,startColumn=2)
    #     addPicture(file = paste0(getwd(),"/inc3.jpeg"),sheet=sheet.5,scale=1,startRow=2,startColumn=12)
    #     addPicture(file = paste0(getwd(),"/linc3.jpeg"),sheet=sheet.5,scale=1,startRow=2,startColumn=22)
    #     
    #     prod4 = reportplot_prod4()
    #     inc4 = reportplot_inc4()
    #     linc4 = reportplot_linc4()
    #     sheet.6 <- createSheet(Results_Workbook, sheetName = "Increased Agricultural Production")
    #     ggsave("prod4.jpeg", prod4, device="jpeg", dpi = 80)
    #     ggsave("inc4.jpeg", inc4, device="jpeg", dpi = 80)
    #     ggsave("linc4.jpeg", linc4, device="jpeg", dpi = 80)
    #     addPicture(file = paste0(getwd(),"/prod4.jpeg"),sheet=sheet.6,scale=1,startRow=2,startColumn=2)
    #     addPicture(file = paste0(getwd(),"/inc4.jpeg"),sheet=sheet.6,scale=1,startRow=2,startColumn=12)
    #     addPicture(file = paste0(getwd(),"/linc4.jpeg"),sheet=sheet.6,scale=1,startRow=2,startColumn=22)
    #     
    #     prod5 = reportplot_prod5()
    #     inc5 = reportplot_inc5()
    #     linc5 = reportplot_linc5()
    #     sheet.7 <- createSheet(Results_Workbook, sheetName = "Increased Non-Agricultural Production")
    #     ggsave("prod5.jpeg", prod5, device="jpeg", dpi = 80)
    #     ggsave("inc5.jpeg", inc5, device="jpeg", dpi = 80)
    #     ggsave("linc5.jpeg", linc5, device="jpeg", dpi = 80)
    #     addPicture(file = paste0(getwd(),"/prod5.jpeg"),sheet=sheet.7,scale=1,startRow=2,startColumn=2)
    #     addPicture(file = paste0(getwd(),"/inc5.jpeg"),sheet=sheet.7,scale=1,startRow=2,startColumn=12)
    #     addPicture(file = paste0(getwd(),"/linc5.jpeg"),sheet=sheet.7,scale=1,startRow=2,startColumn=22)
    #     
    #     prod6 = reportplot_prod6()
    #     inc6 = reportplot_inc6()
    #     linc6 = reportplot_linc6()
    #     sheet.8 <- createSheet(Results_Workbook, sheetName = "Low-Skilled Female Earnings")
    #     ggsave("prod6.jpeg", prod6, device="jpeg", dpi = 80)
    #     ggsave("inc6.jpeg", inc6, device="jpeg", dpi = 80)
    #     ggsave("linc6.jpeg", linc6, device="jpeg", dpi = 80)
    #     addPicture(file = paste0(getwd(),"/prod6.jpeg"),sheet=sheet.8,scale=1,startRow=2,startColumn=2)
    #     addPicture(file = paste0(getwd(),"/inc6.jpeg"),sheet=sheet.8,scale=1,startRow=2,startColumn=12)
    #     addPicture(file = paste0(getwd(),"/linc6.jpeg"),sheet=sheet.8,scale=1,startRow=2,startColumn=22)
    #     
    #     prod7 = reportplot_prod7()
    #     inc7 = reportplot_inc7()
    #     linc7 = reportplot_linc7()
    #     sheet.9 <- createSheet(Results_Workbook, sheetName = "Low-Skilled Male Earnings")
    #     ggsave("prod7.jpeg", prod7, device="jpeg", dpi = 80)
    #     ggsave("inc7.jpeg", inc7, device="jpeg", dpi = 80)
    #     ggsave("linc7.jpeg", linc7, device="jpeg", dpi = 80)
    #     addPicture(file = paste0(getwd(),"/prod7.jpeg"),sheet=sheet.9,scale=1,startRow=2,startColumn=2)
    #     addPicture(file = paste0(getwd(),"/inc7.jpeg"),sheet=sheet.9,scale=1,startRow=2,startColumn=12)
    #     addPicture(file = paste0(getwd(),"/linc7.jpeg"),sheet=sheet.9,scale=1,startRow=2,startColumn=22)
    #     
    #     prod8 = reportplot_prod8()
    #     inc8 = reportplot_inc8()
    #     linc8 = reportplot_linc8()
    #     sheet.10 <- createSheet(Results_Workbook, sheetName = "Skilled Female Earnings")
    #     ggsave("prod8.jpeg", prod8, device="jpeg", dpi = 80)
    #     ggsave("inc8.jpeg", inc8, device="jpeg", dpi = 80)
    #     ggsave("linc8.jpeg", linc8, device="jpeg", dpi = 80)
    #     addPicture(file = paste0(getwd(),"/prod8.jpeg"),sheet=sheet.10,scale=1,startRow=2,startColumn=2)
    #     addPicture(file = paste0(getwd(),"/inc8.jpeg"),sheet=sheet.10,scale=1,startRow=2,startColumn=12)
    #     addPicture(file = paste0(getwd(),"/linc8.jpeg"),sheet=sheet.10,scale=1,startRow=2,startColumn=22)
    #     
    #     prod9 = reportplot_prod9()
    #     inc9 = reportplot_inc9()
    #     linc9 = reportplot_linc9()
    #     sheet.11 <- createSheet(Results_Workbook, sheetName = "Skilled Male Earnings")
    #     ggsave("prod9.jpeg", prod9, device="jpeg", dpi = 80)
    #     ggsave("inc9.jpeg", inc9, device="jpeg", dpi = 80)
    #     ggsave("linc9.jpeg", linc9, device="jpeg", dpi = 80)
    #     addPicture(file = paste0(getwd(),"/prod9.jpeg"),sheet=sheet.11,scale=1,startRow=2,startColumn=2)
    #     addPicture(file = paste0(getwd(),"/inc9.jpeg"),sheet=sheet.11,scale=1,startRow=2,startColumn=12)
    #     addPicture(file = paste0(getwd(),"/linc9.jpeg"),sheet=sheet.11,scale=1,startRow=2,startColumn=22)
    #     
    #     saveWorkbook(Results_Workbook,file)
    #   } 
    # 
    # )
    
}



shinyApp(ui, server)
