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

## Make the data structures we need for SAM output
###############################################################################
sam_row_names <- c("Ag", "Tourism", "Nag", "Fish", "LMUSK", "LMSK", "LFUSK", "LFSK", "K", "Poor", "NonPoor", "Restaurants", 
                   "Lodges", "Tourists", "PA", "ComRevSh", "G", "ROW", "TotalExp")

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
# -----------------------------------------------------------------------------------------------------
# Input all the questions as tables FROM GOOGLE SHEETS or FROM LOCAL FILE (unstar only one at a time):
# -----------------------------------------------------------------------------------------------------
# -----------------------------------------------------------------------------------------------------
# sheet_location = "online"; sheet_path = "https://docs.google.com/spreadsheets/d/1bhuOwJv4b6DttBXEx2lI7uZk6WL0l9uio2e4dcco-F8/edit?usp=share_link"
sheet_location = "local"; sheet_path = "LEWIE-Lite_NewInput_v07.xlsx"


# Disable authentication: 
gs4_deauth()

# Tourists:
gdata_touristStats    = get_input_online_or_local(sheet_path, sheet = "Tourists", range = "B2:G7", mode = sheet_location)
gdata_touristShares   = get_input_online_or_local(sheet_path, sheet = "Tourists", range = "B11:G17", mode = sheet_location)
# Ag:
gdata_AgShares      = get_input_online_or_local(sheet_path, sheet = "Ag", range = "B51:G65", mode = sheet_location)
# NonAg:
gdata_NagShares     = get_input_online_or_local(sheet_path, sheet = "Nonag", range = "B51:G65", mode = sheet_location)
# Tourism businesses:
gdata_TourismShares = get_input_online_or_local(sheet_path, sheet = "Tourism", range = "B51:G65", mode = sheet_location)
# Fish:
gdata_FishShares    = get_input_online_or_local(sheet_path, sheet = "Fishing", range = "B51:G65", mode = sheet_location)
# Restaurants:
gdata_RestShares    = get_input_online_or_local(sheet_path, sheet = "Restaurants", range = "B51:G65", mode = sheet_location)
# Lodges:
gdata_LodgesShares    = get_input_online_or_local(sheet_path, sheet = "Lodges", range = "B51:G65", mode = sheet_location)
# NatParks:
gdata_NPBudget      = get_input_online_or_local(sheet_path, sheet = "NatParkPA", range = "B52:G54", mode = sheet_location)
gdata_NPSpending    = get_input_online_or_local(sheet_path, sheet = "NatParkPA", range = "B57:G70", mode = sheet_location)

# Com Rev Sh.:
gdata_ComRevShShares    = get_input_online_or_local(sheet_path, sheet = "ComRevSh", range = "B58:G69", mode = sheet_location)

# Households (split into 2 tabs):
gdata_HHPc1 = get_input_online_or_local(sheet_path, sheet = "Households", range =  "B54:G58", mode = sheet_location)
gdata_HHPc2 = get_input_online_or_local(sheet_path, sheet = "Households", range =  "B59:G65", mode = sheet_location)
gdata_HHPc3 = get_input_online_or_local(sheet_path, sheet = "Households", range =  "B67:G78", mode = sheet_location)
gdata_HHNPc1 = get_input_online_or_local(sheet_path, sheet = "Households", range = "I54:N58", mode = sheet_location)
gdata_HHNPc2 = get_input_online_or_local(sheet_path, sheet = "Households", range = "I59:N65", mode = sheet_location)
gdata_HHNPc3 = get_input_online_or_local(sheet_path, sheet = "Households", range = "I67:N78", mode = sheet_location)



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
inp_FishShares <- pmap(gdata_FishShares, myfunc_CreateNumericInput)
inp_RestShares <- pmap(gdata_RestShares, myfunc_CreateNumericInput)
inp_LodgesShares <- pmap(gdata_LodgesShares, myfunc_CreateNumericInput)
inp_ComRevShShares <- pmap(gdata_ComRevShShares, myfunc_CreateNumericInput)

inp_NPBudget <- pmap(gdata_NPBudget, myfunc_CreateNumericInput)
inp_NPSpending <- pmap(gdata_NPSpending, myfunc_CreateNumericInput)


inp_HHPc1  <- pmap(gdata_HHPc1, myfunc_CreateNumericInput)
inp_HHPc2  <- pmap(gdata_HHPc2, myfunc_CreateNumericInput)
inp_HHPc3  <- pmap(gdata_HHPc3, myfunc_CreateNumericInput)
inp_HHNPc1 <- pmap(gdata_HHNPc1, myfunc_CreateNumericInput)
inp_HHNPc2 <- pmap(gdata_HHNPc2, myfunc_CreateNumericInput)
inp_HHNPc3 <- pmap(gdata_HHNPc3, myfunc_CreateNumericInput)

# Now create the input fields for making "simulations":
# ------------------------------------------------------
# the names should match the intended SAM columns 
inp_sim_TouristSpending <- numericInput("sim_TouristSpending", 
    "How much Tourist spending ($) do you want to simulate?", min = 0, max = 10000000, value = 100, step = 0.01)
inp_sim_PASpending <- numericInput("sim_PASpending", "How much Park spending ($) do you want to simulate?", min = 0, max = 10000000, value = 100, step = 0.01)
inp_sim_ComRevShSpending <- numericInput("sim_ComRevShSpending", "How much Community spending ($) do you want to simulate?", min = 0, max = 10000000, value = 100, step = 0.01)
inp_sim_AgSpending <- numericInput("sim_AgSpending", "How much increase in local Agricultural production ($) do you want to simulate?", min = 0, max = 10000000, value = 100, step = 0.01)
inp_sim_NagSpending <- numericInput("sim_NagSpending", "How much increase in local Non-Agricultural production ($) do you want to simulate?", min = 0, max = 10000000, value = 100, step = 0.01)
inp_sim_LFUSKSpending <- numericInput("sim_LFUSKSpending", "How much increase in earnings of Low-skilled Female workers ($) do you want to simulate?", min = 0, max = 10000000, value = 100, step = 0.01)
inp_sim_LMUSKSpending <- numericInput("sim_LMUSKSpending", "How much in earnings of Low-skilled Male workers ($) do you want to simulate?", min = 0, max = 10000000, value = 100, step = 0.01)
inp_sim_LFSKSpending <- numericInput("sim_LFSKSpending", "How much in earnings of Skilled Female workers ($) do you want to simulate?", min = 0, max = 10000000, value = 100, step = 0.01)
inp_sim_LMSKSpending <- numericInput("sim_LMSKSpending", "How much in earnings of Skilled Male workers ($) do you want to simulate?", min = 0, max = 10000000, value = 100, step = 0.01)




# # -----------------------------------------------------------------------------------------------------
# # -----------------------------------------------------------------------------------------------------
# # Functions to make some output plots:
# # -----------------------------------------------------------------------------------------------------
# # -----------------------------------------------------------------------------------------------------
# make_production_multipliers_plot <- function(sam_mults, value_inputed, column){
#     plot <- renderPlot({
#         rows_to_plot = c("Ag","Nag", "Restaurants","Lodges")
#         mults <- sam_mults
#         totals <- data.frame(round(value_inputed*(mults[rownames(mults) %in% rows_to_plot, column]), digits=2))
#         colnames(totals) <- "total_prod"
#         totals$cats = rows_to_plot
#         bar <- ggplot(totals, aes(x = cats, y=total_prod, fill=cats )) +
#             geom_bar(stat="sum") +
#             xlab("Categories") + ylab("Additional Production Value ($)") + 
#             geom_text(aes(label = total_prod), vjust = -0.2) +
#             ggtitle(" ... ON PRODUCTION") +
#             theme(plot.title = element_text(hjust = 0.5), 
#                   legend.position = "none")
#         bar
#     })
# }




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



ui <- dashboardPage(
    # %%%%%%%%%%%%%%%%% Header %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    dashboardHeader(title = "LEWIE-lite Dashboard",
                    titleWidth = 250),
    
    # %%%%%%%%%%%%%%%%% SideBar %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    dashboardSidebar(
        collapsed = T,
        sidebarMenu(
            # Note: Create a "Welcome" tab to the top once I'm done coding, or move Dashboard up. 
            menuItem("Dashboard",
                     tabName = "dashboard",
                     icon = icon("dashboard")
            ),
            menuItem("Data",
                     tabName = "data",
                     icon = icon("table")
            ),
            menuItem("Instructions",
                     tabName = "instructions",
                     icon = icon("book")
            )
        )
    ),
    # %%%%%%%%%%%%%%%%% Body %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    dashboardBody(
        tabItems(
            # ============================ Instructions Tab ======================================
            tabItem("instructions", 
                fluidPage(
                  uiOutput("doc_to_display")
                )
            ),
            # ============================ Data Tab: (Where you can change the SAM) ======================================
            tabItem("data",
                p("This page is where you change the data and the SAM if you need to", style = "font-size:25px"),
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
                ), 
                fluidRow(
                    tabBox(width = 12,
                       title = ("SAM"),
                       tabPanel("SAM_RASed", tableOutput("sam_RASed")),
                       tabPanel("SAM_preRAS", tableOutput("sam_preRAS")),
                       tabPanel("Multipliers", tableOutput("mult")),
                       tabPanel("Hide the tables")
                    )
                )
            ),
            
            # ============================ RESULTS DASHBOARD ======================================
            tabItem("dashboard",
                # fluidRow(
                #     infoBox("Some output", textOutput("Output"), icon = icon("dollar-sign"))
                # ),
                # fluidRow(
                #     box(width = 12, title = "For each dollar of tourist spending:",
                #         fluidRow(
                #             valueBox(textOutput("nothing"), "Total GDP multiplier", icon = icon("gears"),  color = "aqua"),
                #         )
                #     )
                # ),
                p("Local-economy impacts of tourist spending (US$)", style = "font-size:25px"),
                fluidRow(
                    box(width = 12, title = "For every dollar of tourist spending (multipliers):",
                        fluidRow(
                            valueBox(textOutput("totalmult"), "Total Production Multiplier", icon = icon("gears"),  color = "aqua"),
                            valueBox(textOutput("poormult"), "Accruing to Poor Households", icon = icon("wallet"),  color = "red"),
                            valueBox(textOutput("labmult"), "Accruing to Labor", icon = icon("user"),  color = "orange")
                        ),
                        fluidRow(
                            valueBox(textOutput("gdpmult"), "Total Income Multiplier", icon = icon("coins"),  color = "green"),
                            valueBox(textOutput("nonpoormult"), "Accruing to NonPoor Households", icon = icon("sack-dollar"),  color = "red"),
                            valueBox(textOutput("capmult"), "Accruing to Capital", icon = icon("building"),  color = "orange")
                        )
                    )
                ),
                fluidRow(
                  box(width = 12, title = "Explore the local economy impacts of…",
                      p(HTML("1. <a href='#sim_TouristSpending'>Tourist Spending</a>"), style = "margin-left: 20px"),
                      p(HTML("2. <a href='#sim_PASpending'>Park Spending</a>"), style = "margin-left: 20px"),
                      p(HTML("3. <a href='#sim_ComRevShSpending'>Community Spending</a>"), style = "margin-left: 20px"),
                      p(HTML("4. <a href='#sim_AgSpending'>Agricultural Production</a>"), style = "margin-left: 20px"),
                      p(HTML("5. <a href='#sim_NagSpending'>Non-Agricultural Production</a>"), style = "margin-left: 20px"),
                      p(HTML("6. <a href='#sim_LFUSKSpending'>Low-Skilled Female Earnings</a>"), style = "margin-left: 20px"),
                      p(HTML("7. <a href='#sim_LMUSKSpending'>Low-Skilled Male Earnings</a>"), style = "margin-left: 20px"),
                      p(HTML("8. <a href='#sim_LFSKSpending'>Skilled Female Earnings</a>"), style = "margin-left: 20px"),
                      p(HTML("9. <a href='#sim_LMSKSpending'>Skilled Male Earnings</a>"), style = "margin-left: 20px")
                  )
                ),
                fluidRow(
                    box(width = 12, title = "Local Economy-wide impact of tourist spending",
                        p('You may wish to evaluate different values of tourist spending: total tourist spending, tourist spending attributable to the PA, change in tourist spending you expect from this project, etc.'),
                        fluidRow(
                            column(4, inp_sim_TouristSpending)
                        ),
                        p('EFFECTS OF THIS TOURISM SPENDING ON...'),
                        fluidRow(
                            # column(width=4, plotOutput("sim_totalprod")),
                            column(width=3, plotOutput("simTourists_totalprod")),
                            # column(width=4, plotOutput("sim_totalinc")), 
                            column(width = 3, plotOutput("simTourists_incomes")),
                            column(width=3, plotOutput("simTourists_labor")),
                            column(width=3, plotOutput("simTourists_ComPA"))
                        ),
                        p(),
                        useShinyjs(),
                        actionButton("toTop1", "Back to top",
                                     class="btn btn-light",
                                     style="padding:4px; font-size:80%; float:right")
                    )
                ),
                #simPark_totalprod
                fluidRow(
                    box(width = 12, title = "Local-economy impacts of Park spending (US$)",
                        p('Park spending is policy-determined, even though there are visitor fees in most places.'),
                        fluidRow(
                            column(4, inp_sim_PASpending)
                        ),
                        p('EFFECTS OF THIS PARK BUDGET ON...'),
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
                    box(width = 12, title = "Local-economy impacts of Community spending (US$)",
                        p('Our simulations assume that part of PA visitor fees are shared with communities. We can also use this model to see the effects of giving additional money to communities near the park. For example, some NGOs support communities near PAs that may or may not happen if the PA did not exist.'),
                        fluidRow(
                            column(4, inp_sim_ComRevShSpending)
                        ),
                        p('EFFECTS OF THIS COMMUNITY SPENDING ON...'),
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
                    box(width = 12, title = "Local-economy impacts of increased Agricultural production (US$)",
                        p('Tourist activities create demand for local agricultural products.  We can also use this model to see the effects of complementary
                          interventions to increase the demand for local agricultural products, for example, 
                          by enabling restaurants and lodges to source more food locally.'),
                        fluidRow(
                            column(4, inp_sim_AgSpending)
                        ),
                        p('EFFECTS OF THIS INCREASE IN LOCAL AGRICULTURAL PRODUCTION ON...'),
                        fluidRow(
                            # p('blank'),
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
                    box(width = 12, title = "Local-economy impacts of increased Non-Agricultural production (US$)",
                        p('Tourist activities create demand for local non-agricultural products. We can also use this model to see the effects of complementary
                          interventions to increase the demand for local non-agricultural products, for example, 
                          by enabling restaurants and lodges to source local artifacts or processed goods.'),
                        fluidRow(
                            column(4, inp_sim_NagSpending)
                        ),
                        p('EFFECTS OF THIS INCREASE IN LOCAL NON-AGRICULTURAL PRODUCTION ON...'),
                        fluidRow(
                            # p('blank')
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
                fluidRow(
                    box(width = 12, title = "Local-economy impacts of Low-skilled Female earnings (US$)",
                        p('Tourist activities create demand for local labor. We can also use this model to see the effects of complementary 
                          interventions to increase the employment of local workers, for example, through job training programs.'),
                        fluidRow(
                            column(4, inp_sim_LFUSKSpending)
                        ),
                        p('EFFECTS OF THIS INCREASE IN EARNINGS TO LOW SKILLED FEMALE WORKERS ON...'),
                        fluidRow(
                            # p('blank')
                            column(width=4, plotOutput("simLFUSK_totalprod")),
                            column(width=4, plotOutput("simLFUSK_incomes")),
                            column(width=4, plotOutput("simLFUSK_labor"))
                        ),
                        p(),
                        useShinyjs(),
                        actionButton("toTop6", "Back to top",
                                     class="btn btn-light",
                                     style="padding:4px; font-size:80%; float:right")
                    )
                ),
                fluidRow(
                    box(width = 12, title = "Local-economy impacts of Low-Skilled Male earnings (US$)",
                        p('Tourist activities create demand for local labor. We can also use this model to see the effects of complementary 
                          interventions to increase the employment of local workers, for example, through job training programs.'),
                        fluidRow(
                            column(4, inp_sim_LMUSKSpending)
                        ),
                        p('EFFECTS OF THIS INCREASE IN EARNINGS TO LOW SKILLED MALE WORKERS ON...'),
                        fluidRow(
                            # p('blank')
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
                fluidRow(
                    box(width = 12, title = "Local-economy impacts of Skilled Female earnings (US$)",
                        p('Tourist activities create demand for local labor. We can also use this model to see the effects of complementary 
                          interventions to increase the employment of local workers, for example, through job training programs.'),
                        fluidRow(
                            column(4, inp_sim_LFSKSpending)
                        ),
                        p('EFFECTS OF THIS INCREASE IN EARNINGS TO SKILLED FEMALE WORKERS ON...'),
                        fluidRow(
                            # p('blank')
                            column(width=4, plotOutput("simLFSK_totalprod")),
                            column(width=4, plotOutput("simLFSK_incomes")),
                            column(width=4, plotOutput("simLFSK_labor"))
                        ),
                        p(),
                        useShinyjs(),
                        actionButton("toTop8", "Back to top",
                                     class="btn btn-light",
                                     style="padding:4px; font-size:80%; float:right")
                    )
                ),
                fluidRow(
                    box(width = 12, title = "Local-economy impacts of Skilled Male earnings (US$)",
                        p('Tourist activities create demand for local labor. We can also use this model to see the effects of complementary 
                          interventions to increase the employment of local workers, for example, through job training programs.'),
                        fluidRow(
                            column(4, inp_sim_LMSKSpending)
                        ),
                        p('EFFECTS OF THIS INCREASE IN EARNINGS TO SKILLED MALE WORKERS ON...'),
                        fluidRow(
                            # p('blank')
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
                ),
                fluidPage(
                  downloadButton("report", "Generate report"),
                  p('This can take up to 30sec to generate. There is no need for multiple clicks.')
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
            input$ag_expOutside_raw + input$ag_expTaxes_raw)
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
                input$fish_expTaxes_raw )
        
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
        lodges_expTaxes = input$lodges_expTaxes_raw * lodges_scaleshares
        
        
        # Rescale household expenditure shares: that's already done below.  
        
        # Rescale 
        
        
        
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
        
        # Changed the way we calculate the entries 
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
                input$hhp_expLocalFish + input$hhp_expLocalProc + input$hhp_expRest + input$hhp_expOtherLocal + input$hhp_expOutside +
                input$hhp_expRent + input$hhp_expTax
        hhnp_sumshares <- input$hhnp_expGroceryFood + input$hhnp_expGroceryOther + input$hhnp_expMarkets + input$hhnp_expLocalFarms +
            input$hhnp_expLocalFish + input$hhnp_expLocalProc + input$hhnp_expRest + input$hhnp_expOtherLocal + input$hhnp_expOutside +
            input$hhnp_expRent + input$hhnp_expTax 
        
        

        # SAM rows for Poor (rescaled shares)
        sam_hhp["Ag",] <- hhp_totalInc*(input$hhp_expLocalFarms/hhp_sumshares)
        sam_hhp["Nag",] <- hhp_totalInc*((input$hhp_expGroceryFood +
                                               input$hhp_expGroceryOther +
                                               input$hhp_expMarkets +
                                               input$hhp_expLocalProc +
                                               input$hhp_expOtherLocal)/hhp_sumshares)
        sam_hhp["Fish",] <- hhp_totalInc*(input$hhp_expLocalFish/hhp_sumshares)
        sam_hhp["Restaurants", ] <- hhp_totalInc*(input$hhp_expRest/hhp_sumshares)
        # Rents and taxes calculation: 
        sam_hhp["K", ] <- hhp_totalInc*(input$hhp_expRent/hhp_sumshares)
        sam_hhp["G", ] <- hhp_totalInc*(input$hhp_expTax/hhp_sumshares)
        # ROW account is the RESIDUAL
        sam_hhp_allbutrow <- sum(sam_hhp)
        sam_hhp["ROW", ] <- hhp_totalInc - sam_hhp_allbutrow
        

        # SAM rows for Non-Poor
        sam_hhnp["Ag",] <- hhnp_totalInc*input$hhnp_expLocalFarms/hhnp_sumshares
        sam_hhnp["Nag",] <- hhnp_totalInc*(input$hhnp_expGroceryFood +
                                                 input$hhnp_expGroceryOther +
                                                 input$hhnp_expMarkets +
                                                 input$hhnp_expLocalProc +
                                                 input$hhnp_expOtherLocal)/hhnp_sumshares
        sam_hhnp["Fish",] <- hhnp_totalInc*input$hhnp_expLocalFish/hhnp_sumshares
        sam_hhnp["Restaurants", ] <- hhnp_totalInc*input$hhnp_expRest/hhnp_sumshares
        # Rents and taxes calculation: 
        sam_hhnp["K", ] <- hhnp_totalInc*(input$hhnp_expRent/hhnp_sumshares)
        sam_hhnp["G", ] <- hhnp_totalInc*(input$hhnp_expTax/hhnp_sumshares)
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
        
        
        
        
        # # ComRevSh Column - budget comes from a share of PA  
        # #------------------------------------
        sam_comrevsh["Ag",] <- input$natPark_expComRevSh/100 * input$comRevSh_expLocalAg
        sam_comrevsh["Nag",] <- input$natPark_expComRevSh/100 * (input$comRevSh_expServices + input$comRevSh_expLocalStores)
        sam_comrevsh["Tourism",] <- input$natPark_expComRevSh/100 * input$comRevSh_expTourism
        sam_comrevsh["Fish",] <- input$natPark_expComRevSh/100 * input$comRevSh_expLocalFish
        sam_comrevsh["ROW",] <- input$natPark_expComRevSh/100 * input$comRevSh_expOutside
        
        sam_comrevsh["LMUSK", ] <-  input$natPark_expComRevSh/100 * input$comRevSh_MwagesUnskilled 
        sam_comrevsh["LMSK", ] <-   input$natPark_expComRevSh/100 * input$comRevSh_MwagesSkilled
        sam_comrevsh["LFUSK", ] <-  input$natPark_expComRevSh/100 * input$comRevSh_FwagesUnskilled
        sam_comrevsh["LFSK", ] <-   input$natPark_expComRevSh/100 * input$comRevSh_FwagesSkilled

        
        
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
        
        sam_fish["G",] <- sam_fish_totalrev * (fish_expTaxes)/100
        sam_fish["ROW",] <- sam_fish_totalrev * (fish_expOutside/100 
                                                 +  input$fish_profitMargin/100 * (1-input$fish_shareLocallyOwned/100)
                                                 + (fish_MwagesUnskilled + fish_MwagesSkilled + fish_FwagesUnskilled + fish_FwagesSkilled)/100 * (1 - input$fish_shareWorkersLocal)/100)
        
        # K account used to be the residual, now computed directly
        # sam_fish_allbutK <- sum(sam_fish)
        # sam_fish["K",] <- sam_fish_totalrev - sam_fish_allbutK
        sam_fish["K",] <- sam_fish_totalrev * (input$fish_profitMargin)/100 * input$fish_shareLocallyOwned/100
        
        
        # G Column - based simply on NatParks + the difference of other cols
        # #---------------------------------------------------------------------
        # Total park revenue = entry fees from tourists. 
        # Total park budget = provided
        # Funding from govt = budget - revenue
        # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        
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
                    sam_lodges["LMUSK",] + sam_restaurants["LMUSK",] + sam_tourists["LMUSK",] + sam_pa["LMUSK",] + sam_tourism["LMUSK",] + sam_comrevsh["LMUSK", ]
        
        lmsk_rowsum = sam_ag["LMSK",] + sam_nag["LMSK",] + sam_fish["LMSK",] + sam_hhp["LMSK",] + sam_hhnp["LMSK",] +
            sam_lodges["LMSK",] + sam_restaurants["LMSK",] + sam_tourists["LMSK",] + sam_pa["LMSK",] + sam_tourism["LMSK",] + sam_comrevsh["LMSK", ]
        
        lfusk_rowsum = sam_ag["LFUSK",] + sam_nag["LFUSK",] + sam_fish["LFUSK",] + sam_hhp["LFUSK",] + sam_hhnp["LFUSK",] +
            sam_lodges["LFUSK",] + sam_restaurants["LFUSK",] + sam_tourists["LFUSK",] + sam_pa["LFUSK",] + sam_tourism["LFUSK",] + sam_comrevsh["LFUSK", ]
        
        lfsk_rowsum = sam_ag["LFSK",] + sam_nag["LFSK",] + sam_fish["LFSK",] + sam_hhp["LFSK",] + sam_hhnp["LFSK",] +
            sam_lodges["LFSK",] + sam_restaurants["LFSK",] + sam_tourists["LFSK",] + sam_pa["LFSK",] + sam_tourism["LFSK",] + sam_comrevsh["LFSK", ]
        
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
        
        # All functions for rasing defined above. 
        sam_rased_x <- ras_loop_x(sam_pre, 0.02)
        
        sam_rased_x
    }) 
    
    
    # %%%%%%%%%%%%%%%%% To RAS or Not to RAS? %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
    # Just pick either pre-RAS or RASed for the main display, which will also change the multipliers 
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
    

    
    
    # %%%%%%%%%%%%%%%%% Compute some outputs %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    output$sam_preRAS <- renderTable({sam_preRAS()}, rownames=TRUE, digits=0, striped = T)
    output$sam_RASed <- renderTable({sam_RASed()}, rownames=TRUE, digits=0, striped = T)
    output$mult <- renderTable({multout()}, rownames=TRUE, digits=2, striped = T)
    
    
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
    
    # Total income (GDP) multiplier
    gdpmult <- function() {
      rows_to_sum = c("Poor","NonPoor")
      mults <- multout()
      total <- sum(mults[rownames(mults) %in% rows_to_sum,"Tourists"])
      total
      scales::dollar(total)
    }
    
    output$gdpmult <- renderText({
      gdpmult()
    })
    
    labmult <- function() {
      rows_to_sum = c("LMUSK", "LFUSK", "LMSK", "LFSK")
      mults <- multout()
      total <- sum(mults[rownames(mults) %in% rows_to_sum,"Tourists"])
      round(total,3)
      scales::dollar(total)
    }
    
    output$labmult <- renderText({
      labmult()
    }) 
    
    capmult <- function() {
      rows_to_sum = c("K")
      mults <- multout()
      total <- sum(mults[rownames(mults) %in% rows_to_sum,"Tourists"])
      round(total,3)
      scales::dollar(total)
    }
    
    output$capmult <- renderText({
      capmult()
    })
    
    poormult <- function() {
      rows_to_sum = c("Poor")
      mults <- multout()
      total <- sum(mults[rownames(mults) %in% rows_to_sum,"Tourists"])
      round(total,3)
      scales::dollar(total)
    }
    
    output$poormult <- renderText({
      poormult()
    }) 
    
    nonpoormult <- function() {
      mults <- multout()
      total <- mults["NonPoor","Tourists"]
      round(total,3)
      scales::dollar(total)
    }
    
    output$nonpoormult <- renderText({
      nonpoormult()
    }) 
    
    
    
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
    
    # These plots are made with functions now - should be able to comment out. 
    
    # Increase in production, total 
    # output$sim_totalprod <- renderPlot({
    #     rows_to_plot = c("Ag","Nag", "Restaurants","Lodges")
    #     mults <- multout()
    #     totals <- data.frame(round(input$sim_TouristSpending*(mults[rownames(mults) %in% rows_to_plot,"Tourists"]), digits=2))
    #     colnames(totals) <- "total_prod"
    #     totals$cats = rows_to_plot
    #     # now the actual plot
    #     bar <- ggplot(totals, aes(x = cats, y=total_prod, fill=cats )) +
    #              geom_bar(stat="sum") +
    #              xlab("Categories") + ylab("Additional Production Value ($)") + 
    #              geom_text(aes(label = total_prod), vjust = -0.2) +
    #              ggtitle(" ... ON PRODUCTION") +
    #             theme(plot.title = element_text(hjust = 0.5), 
    #                   legend.position = "none")
    #     bar
    #     # totals
    # })
    
    # These are now
    # output$sim_totalinc <- renderPlot({
    #     mults <- multout()
    #     inctotals <- data.frame(round(input$sim_TouristSpending*(mults[rownames(mults) %in% c("Poor", "NonPoor"),"Tourists"]), digits=2))
    #     colnames(inctotals) <- "total_inc"
    #     inctotals$cats = c("Poor", "NonPoor")
    #     bar <- ggplot(inctotals, aes(x = cats, y=total_inc, fill=cats )) +
    #         geom_bar(stat="sum") +
    #         xlab("Households") + ylab("Additional Income ($)") + 
    #         geom_text(aes(label = total_inc), vjust = -0.2) +
    #         ggtitle("... ON INCOMES ") + 
    #         theme(plot.title = element_text(hjust = 0.5), 
    #              legend.position = "none")
    #     bar
    #     # inctotals
    # })
    # 
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
    #     # inctotals
    # })
    # 
    # 
    # %%%%%%%%%%%%%%%%% Compute some more effects of park spending, in $ %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    # Increase in production, total 
    # output$simPark_totalprod <- renderPlot({
    #     rows_to_plot = c("Ag","Nag", "Restaurants","Lodges")
    #     mults <- multout()
    #     totals <- data.frame(round(input$sim_TouristSpending*(mults[rownames(mults) %in% rows_to_plot,"PA"]), digits=2))
    #     colnames(totals) <- "total_prod"
    #     totals$cats = rows_to_plot
    #     # bar <- barplot((mults[rownames(mults) %in% rows_to_plot,"Tourists"])*input$sim_TouristSpending)
    #     bar <- ggplot(totals, aes(x = cats, y=total_prod, fill=cats )) +
    #         geom_bar(stat="sum") +
    #         xlab("Categories") + ylab("Additional Production Value ($)") + 
    #         geom_text(aes(label = total_prod), vjust = -0.2) +
    #         ggtitle("Effects of tourism spending on production") +
    #         theme(plot.title = element_text(hjust = 0.5), 
    #               legend.position = "none")
    #     bar
    #     # totals
    # })
    
    # Try a server function for plotting
    # make_production_multipliers_plot <- function(column, input_value){
    #     plot <- renderPlot({
    #         rows_to_plot = c("Ag","Nag", "Restaurants","Lodges")
    #         mults <- multout()
    #         totals <- data.frame(round(input_value*(mults[rownames(mults) %in% rows_to_plot, column]), digits=2))
    #         colnames(totals) <- "total_prod"
    #         totals$cats = rows_to_plot
    #         bar <- ggplot(totals, aes(x = cats, y=total_prod, fill=cats )) +
    #             geom_bar(stat="sum") +
    #             xlab("Categories") + ylab("Additional Production Value ($)") +
    #             geom_text(aes(label = total_prod), vjust = -0.2) +
    #             ggtitle(" ... ON PRODUCTION") +
    #             theme(plot.title = element_text(hjust = 0.5),
    #                   legend.position = "none")
    #         bar
    #     })
    # }
    
    # Functions to make the plots
    # ---------------------------------
    
    # NOTE: The request was to CLEARLY SEPARATE tourism-related activities from non-tourism-related activities. 
    # This doesn't really do that, but gets a bit closer.  
    # To do this properly, we might need to split into two plots, or somehow add brackets with text over the bars?
    make_production_multipliers_plot <- function(column, input_value){
        rows_to_plot = c("Tourism", "Restaurants", "Lodges", "Ag","Nag", "Fish")
        mults <- multout()
        totals <- data.frame(round(input_value*(mults[rownames(mults) %in% rows_to_plot, column]), digits=2))
        colnames(totals) <- "total_prod"
        totals$cats = rownames(totals)
        # This refactoring makes sure the cats are displayed in the right order (not alphabetical)
        totals$cats <- factor(totals$cats, levels = rows_to_plot)
        bar <- ggplot(totals, aes(x = cats, y=total_prod, fill=cats )) +
            geom_bar(stat="sum") +
            xlab("Tourism-related Activities  vs.  Non-Tourism-related Activities") + ylab("Additional Production Value ($)") +
            geom_text(aes(label = total_prod), vjust = -0.2) +
            ggtitle(" ... ON PRODUCTION") +
            theme(plot.title = element_text(hjust = 0.5),
                  legend.position = "none")
        # return the bar plot:
        bar
    }
    
    
    make_income_multipliers_plot <- function(column, input_value){
        mults <- multout()
        inctotals <- data.frame(round(input_value*(mults[rownames(mults) %in% c("Poor", "NonPoor"), column]), digits=2))
        colnames(inctotals) <- "total_inc"
        inctotals$cats = rownames(inctotals)
        bar <- ggplot(inctotals, aes(x = cats, y=total_inc, fill=cats )) +
            geom_bar(stat="sum") +
            xlab("Households") + ylab("Additional Income ($)") + 
            geom_text(aes(label = total_inc), vjust = -0.2) +
            ggtitle("... ON INCOMES ") + 
            theme(plot.title = element_text(hjust = 0.5), 
                  legend.position = "none")
        # return the bar plot
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
            geom_bar(stat="sum") +
            xlab("Labor Categories") + ylab("Additional Labor Income ($)") + 
            geom_text(aes(label = total_lab), vjust = -0.2) +
            ggtitle("... ON LABOR INCOME ") + 
            theme(plot.title = element_text(hjust = 0.5), 
                  legend.position = "none") 
            # Adding geom_bracket requires a non-standard package
            # geom_bracket(aes(xmin = 1, xmax = 2, y = max(total_lab) + 2, label = "unskilled labor"),
            #              label.y.npc = 1.2, label.size = 4)
        # return the bar plot
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
            theme(plot.title = element_text(hjust = 0.5),
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

    # Make last plot, just for the first row
    output$simTourists_ComPA <- renderPlot({ 
        make_ComPA_multipliers_plot("Tourists", input$sim_TouristSpending)
    })
    
    # Functions to pass to download to create last plot in PDF report
    reportplot_earn1 <- function() {
      make_ComPA_multipliers_plot("Tourists", input$sim_TouristSpending)
    }
    
    
    output$report <- downloadHandler(
      filename = "report.pdf",
      content = function(file) {
        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).
        tempReport <- file.path(tempdir(), "report.Rmd")
        file.copy("report.Rmd", tempReport, overwrite = TRUE)
        
        # Set up parameters to pass to Rmd document
        params <- list(prod1 = reportplot_prod1, prod2 = reportplot_prod2, prod3 = reportplot_prod3,
                       prod4 = reportplot_prod4, prod5 = reportplot_prod5, prod6 = reportplot_prod6,
                       prod7 = reportplot_prod7, prod8 = reportplot_prod8, prod9 = reportplot_prod9,
                       inc1 = reportplot_inc1, inc2 = reportplot_inc2, inc3 = reportplot_inc3,
                       inc4 = reportplot_inc4, inc5 = reportplot_inc5, inc6 = reportplot_inc6,
                       inc7 = reportplot_inc7, inc8 = reportplot_inc8, inc9 = reportplot_inc9,
                       linc1 = reportplot_linc1, linc2 = reportplot_linc2, linc3 = reportplot_linc3,
                       linc4 = reportplot_linc4, linc5 = reportplot_linc5, linc6 = reportplot_linc6,
                       linc7 = reportplot_linc7, linc8 = reportplot_linc8, linc9 = reportplot_linc9,
                       earn1 = reportplot_earn1,
                       totalmult = totalmult, gdpmult = gdpmult, labmult = labmult,
                       capmult = capmult, poormult = poormult, nonpoormult = nonpoormult,
                       sim_TouristSpending = input$sim_TouristSpending,
                       sim_PASpending = input$sim_PASpending,
                       sim_ComRevShSpending = input$sim_ComRevShSpending,
                       sim_AgSpending = input$sim_AgSpending,
                       sim_NagSpending = input$sim_NagSpending,
                       sim_LFUSKSpending = input$sim_LFUSKSpending,
                       sim_LMUSKSpending = input$sim_LMUSKSpending,
                       sim_LFSKSpending = input$sim_LFSKSpending,
                       sim_LMSKSpending = input$sim_LMSKSpending)
        
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

  }
    
    

    
    
    
shinyApp(ui, server)
