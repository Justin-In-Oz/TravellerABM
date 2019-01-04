# this is to create a spec trade function
# this is drawn from pages 42 to 44 of GDW's 
# Traveller LBB2 pub 1977

# inputs required
purchWorldPop <- 7



# create the Trade and Spcutlation Table
tradeGood <- vector(mode = "character", length = 36)
basePrice <- vector(mode = "numeric", length = 36)

tradeGood <- c("Textiles",
               "Polymers",
               "Liquor",
               "Wood",
               "Crystals",
               "Radioactives",
               "Steel",
               "Copper",
               "Aluminium",
               "Tin",
               "Silver",
               "Special Alloys",
               "Petrochemicals",
               "Grain",
               "Meat",
               "Spices",
               "Fruit",
               "Pharmaceuticals",
               "Gems",
               "Firearms",
               "Ammunition",
               "Blades",
               "Tools",
               "Body Armour",
               "Aircraft",
               "Air/Raft",
               "Computers",
               "ATV",
               "AFV",
               "Farm Machinery",
               "Electronics Parts",
               "Mechanical Parts",
               "Cybernetic Parts",
               "Computer Parts",
               "Machine Tools",
               "Vacc Suits")

# initialise vectors for the purchase dice modifiers
agrWorldPurchDM <- vector(mode = "numeric", length = 36)
nonAgrWorldPurchDM <- vector(mode = "numeric", length = 36)
indWorldPurchDM <- vector(mode = "numeric", length = 36)
nonIndWorldPurchDM <- vector(mode = "numeric", length = 36)
richWorldPurchDM <- vector(mode = "numeric", length = 36)
poorWorldPurchDM <- vector(mode = "numeric", length = 36)

# initialise vectors for the purchase dice modifiers
agrWorldResaleDM <- vector(mode = "numeric", length = 36)
nonAgrWorldResaleDM <- vector(mode = "numeric", length = 36)
indWorldResaleDM <- vector(mode = "numeric", length = 36)
nonIndWorldResaleDM <- vector(mode = "numeric", length = 36)
richWorldResaleDM <- vector(mode = "numeric", length = 36)
poorWorldResaleDM <- vector(mode = "numeric", length = 36)

# initialise the vector for the quantity determination
qtyDice <- vector(mode = "numeric", length = 36)
qtyMult <- vector(mode = "numeric", length = 36)

# Purchase DMs 
{
# set the agrWorldPurch Mods
agrWorldPurchDM[1]  <- -7 # 11 Textiles
agrWorldPurchDM[3]  <- -4 # 13 Liquor
agrWorldPurchDM[4]  <- -6 # 14 Wood
agrWorldPurchDM[14] <- -2 # 32 Grain
agrWorldPurchDM[15] <- -2 # 33 Meat
agrWorldPurchDM[16] <- -2 # 34 Spices
agrWorldPurchDM[17] <- -2 # 35 Fruit

# set the Non Agricultural world Purch mods
nonAgrWorldPurchDM[1]  <- -5 # 11 Textiles
nonAgrWorldPurchDM[5]  <- -3 # 15 Crystals
nonAgrWorldPurchDM[13] <- -4 # 31 Petrochemicals
nonAgrWorldPurchDM[14] <-  1 # 32 Grain
nonAgrWorldPurchDM[15] <-  2 # 33 Meat
nonAgrWorldPurchDM[16] <-  3 # 34 Spices
nonAgrWorldPurchDM[17] <-  1 # 35 Fruit
nonAgrWorldPurchDM[18] <- -3 # 36 Pharmaceuticals
nonAgrWorldPurchDM[36] <- -5 # 66 Vacc Suits

# set the Industrial world Purch mods
indWorldPurchDM[2]  <- -2 # 12 Polymers
indWorldPurchDM[5]  <-  4 # 15 Crystals
indWorldPurchDM[6]  <-  7 # 16 Radioactives
indWorldPurchDM[7]  <- -2 # 21 Steel
indWorldPurchDM[8]  <- -3 # 22 Copper
indWorldPurchDM[9]  <- -3 # 23 Aluminium
indWorldPurchDM[10] <- -3 # 24 Tin
indWorldPurchDM[11] <-  5 # 25 Silver
indWorldPurchDM[12] <- -3 # 26 Special Alloys
indWorldPurchDM[13] <-  1 # 31 Petrochemicals
indWorldPurchDM[14] <-  2 # 32 Grain
indWorldPurchDM[15] <-  3 # 33 Meat
indWorldPurchDM[16] <-  2 # 34 Spices
indWorldPurchDM[17] <-  2 # 35 Fruit
indWorldPurchDM[18] <-  4 # 36 Pharmaceuticals
indWorldPurchDM[19] <-  4 # 41 Gems
indWorldPurchDM[20] <- -3 # 42 Firearms
indWorldPurchDM[21] <- -3 # 43 Ammunition
indWorldPurchDM[22] <- -3 # 44 Blades
indWorldPurchDM[23] <- -3 # 45 Tools
indWorldPurchDM[24] <- -1 # 46 Body Armour
indWorldPurchDM[25] <- -4 # 51 Aircraft
indWorldPurchDM[26] <- -3 # 52 Air/Raft
indWorldPurchDM[27] <- -2 # 53 Computers
indWorldPurchDM[28] <- -2 # 54 ATV
indWorldPurchDM[29] <- -5 # 55 AFV
indWorldPurchDM[30] <- -5 # 56 Farm Machinery
indWorldPurchDM[31] <- -4 # 61 Electronics Parts
indWorldPurchDM[32] <- -5 # 62 Mechanical Parts
indWorldPurchDM[33] <- -4 # 63 Cybernetic Parts
indWorldPurchDM[34] <- -5 # 64 Computer Parts
indWorldPurchDM[35] <- -5 # 65 Machine Tools
indWorldPurchDM[36] <- -3 # 66 Vacc Suits

# set the Non-Industrial world Purch mods
nonIndWorldPurchDM[1]  <- -3 # 11 Textiles
nonIndWorldPurchDM[6]  <- -3 # 16 Radioactives
nonIndWorldPurchDM[12] <-  5 # 26 Special Alloys
nonIndWorldPurchDM[13] <- -5 # 31 Petrochemicals
nonIndWorldPurchDM[19] <- -8 # 41 Gems

# set the Rich world Purch mods
richWorldPurchDM[2]  <- -3 # 12 Polymers
richWorldPurchDM[6]  <-  5 # 16 Radioactives
richWorldPurchDM[7]  <- -1 # 21 Steel
richWorldPurchDM[8]  <- -2 # 22 Copper
richWorldPurchDM[9]  <- -2 # 23 Aluminium
richWorldPurchDM[10] <- -2 # 24 Tin
richWorldPurchDM[11] <- -1 # 25 Silver
richWorldPurchDM[12] <- -2 # 26 Special Alloys
richWorldPurchDM[20] <- -2 # 42 Firearms
richWorldPurchDM[21] <- -2 # 43 Ammunition
richWorldPurchDM[22] <- -2 # 44 Blades
richWorldPurchDM[23] <- -2 # 45 Tools
richWorldPurchDM[24] <- -3 # 46 Body Armour
richWorldPurchDM[25] <- -3 # 51 Aircraft
richWorldPurchDM[26] <- -2 # 52 Air/Raft
richWorldPurchDM[27] <- -2 # 53 Computers
richWorldPurchDM[28] <- -2 # 54 ATV
richWorldPurchDM[29] <- -2 # 55 AFV
richWorldPurchDM[30] <- -2 # 56 Farm Machinery
richWorldPurchDM[31] <- -3 # 61 Electronics Parts
richWorldPurchDM[32] <- -3 # 62 Mechanical Parts
richWorldPurchDM[33] <- -1 # 63 Cybernetic Parts
richWorldPurchDM[34] <- -3 # 64 Computer Parts
richWorldPurchDM[35] <- -4 # 65 Machine Tools
richWorldPurchDM[36] <- -1 # 66 Vacc Suits

# set the Poor world Purch mods
poorWorldPurchDM[2]  <-  2 # 12 Polymers
poorWorldPurchDM[7]  <-  1 # 21 Steel
poorWorldPurchDM[8]  <-  1 # 22 Copper
poorWorldPurchDM[9]  <-  1 # 23 Aluminium
poorWorldPurchDM[10] <-  1 # 24 Tin
poorWorldPurchDM[11] <-  2 # 25 Silver
poorWorldPurchDM[18] <-  3 # 36 Pharmaceuticals
poorWorldPurchDM[19] <- -3 # 41 Gems
poorWorldPurchDM[20] <-  3 # 42 Firearms
poorWorldPurchDM[21] <-  3 # 43 Ammunition
poorWorldPurchDM[22] <-  3 # 44 Blades
poorWorldPurchDM[23] <-  3 # 45 Tools
poorWorldPurchDM[24] <-  3 # 46 Body Armour
poorWorldPurchDM[29] <-  4 # 55 AFV

} # End of Purchase DMs

# Resale DMs
{
# set the Agricultural world Resale mods
agrWorldResaleDM[1]  <- -6 # 11 Textiles
agrWorldResaleDM[3]  <- -3 # 13 Liquor
agrWorldResaleDM[4]  <- -6 # 14 Wood
agrWorldResaleDM[14] <- -2 # 32 Grain
agrWorldResaleDM[15] <- -2 # 33 Meat
agrWorldResaleDM[16] <- -2 # 34 Spices
agrWorldResaleDM[17] <- -2 # 35 Fruit
agrWorldResaleDM[30] <-  5 # 56 Farm Machinery

# set the Non-Agricultural world Resale mods
nonAgrWorldResaleDM[1]  <-  1 # 11 Textiles
nonAgrWorldResaleDM[5]  <- -3 # 15 Crystals
nonAgrWorldResaleDM[13] <- -4 # 31 Petrochemicals
nonAgrWorldResaleDM[18] <-  1 # 36 Pharmaceuticals
nonAgrWorldResaleDM[29] <- -2 # 55 AFV
nonAgrWorldResaleDM[30] <- -8 # 56 Farm Machinery
nonAgrWorldResaleDM[33] <-  2 # 63 Cybernetic Parts
nonAgrWorldResaleDM[34] <-  2 # 64 Computer Parts
nonAgrWorldResaleDM[35] <-  2 # 65 Machine Tools

# set the Industrial world Resale mods
indWorldResaleDM[2]  <- -2 # 12 Polymers
indWorldResaleDM[3]  <-  1 # 13 Liquor
indWorldResaleDM[4]  <-  1 # 14 Wood
indWorldResaleDM[5]  <-  3 # 15 Crystals
indWorldResaleDM[6]  <-  6 # 16 Radioactives
indWorldResaleDM[7]  <- -2 # 21 Steel
indWorldResaleDM[8]  <- -3 # 22 Copper
indWorldResaleDM[9]  <- -3 # 23 Aluminium
indWorldResaleDM[10] <- -3 # 24 Tin
indWorldResaleDM[11] <-  5 # 25 Silver
indWorldResaleDM[12] <- -3 # 26 Special Alloys
indWorldResaleDM[13] <-  3 # 31 Petrochemicals
indWorldResaleDM[15] <-  2 # 33 Meat
indWorldResaleDM[17] <-  3 # 35 Fruit
indWorldResaleDM[18] <-  5 # 36 Pharmaceuticals
indWorldResaleDM[19] <-  4 # 41 Gems
indWorldResaleDM[20] <- -2 # 42 Firearms
indWorldResaleDM[21] <- -2 # 43 Ammunition
indWorldResaleDM[22] <- -2 # 44 Blades
indWorldResaleDM[23] <- -2 # 45 Tools
indWorldResaleDM[24] <- -2 # 46 Body Armour

# set the Non-Industrial world Resale mods
nonIndWorldResaleDM[6]  <- -3 # 16 Radioactives
nonIndWorldResaleDM[9]  <-  4 # 23 Aluminium
nonIndWorldResaleDM[12] <-  4 # 26 Special Alloys
nonIndWorldResaleDM[13] <- -5 # 31 Petrochemicals
nonIndWorldResaleDM[19] <- -2 # 41 Gems
nonIndWorldResaleDM[25] <-  2 # 51 Aircraft
nonIndWorldResaleDM[26] <-  2 # 52 Air/Raft
nonIndWorldResaleDM[27] <-  2 # 53 Computers
nonIndWorldResaleDM[28] <-  2 # 54 ATV
nonIndWorldResaleDM[31] <-  2 # 61 Electronics Parts
nonIndWorldResaleDM[32] <-  3 # 62 Mechanical Parts
nonIndWorldResaleDM[33] <-  4 # 63 Cybernetic Parts
nonIndWorldResaleDM[34] <-  3 # 64 Computer Parts
nonIndWorldResaleDM[35] <-  3 # 65 Machine Tools
nonIndWorldResaleDM[36] <-  2 # 66 Vacc Suits

# set the Rich world Resale mods
richWorldResaleDM[1]  <-  3 # 11 Textiles
richWorldResaleDM[2]  <-  3 # 12 Polymers
richWorldResaleDM[3]  <-  2 # 13 Liquor
richWorldResaleDM[4]  <-  2 # 14 Wood
richWorldResaleDM[5]  <-  3 # 15 Crystals
richWorldResaleDM[6]  <- -4 # 16 Radioactives
richWorldResaleDM[7]  <- -1 # 21 Steel
richWorldResaleDM[8]  <- -1 # 22 Copper
richWorldResaleDM[9]  <- -1 # 23 Aluminium
richWorldResaleDM[10] <- -1 # 24 Tin
richWorldResaleDM[11] <- -1 # 25 Silver
richWorldResaleDM[12] <- -1 # 26 Special Alloys
richWorldResaleDM[16] <-  2 # 34 Spices
richWorldResaleDM[18] <-  4 # 36 Pharmaceuticals
richWorldResaleDM[19] <-  8 # 41 Gems
richWorldResaleDM[20] <- -1 # 42 Firearms
richWorldResaleDM[21] <- -1 # 43 Ammunition
richWorldResaleDM[22] <- -1 # 44 Blades
richWorldResaleDM[23] <- -1 # 45 Tools
richWorldResaleDM[24] <-  1 # 46 Body Armour
richWorldResaleDM[29] <-  1 # 55 AFV

# set the Poor world Resale mods
poorWorldResaleDM[7]  <-  3 # 21 Steel
poorWorldResaleDM[15] <-  1 # 33 Meat
poorWorldResaleDM[16] <-  3 # 34 Spices
poorWorldResaleDM[17] <-  2 # 35 Fruit
poorWorldResaleDM[20] <-  3 # 42 Firearms
poorWorldResaleDM[21] <-  3 # 43 Ammunition
poorWorldResaleDM[22] <-  3 # 44 Blades
poorWorldResaleDM[23] <-  3 # 45 Tools
poorWorldResaleDM[24] <-  4 # 46 Body Armour
poorWorldResaleDM[25] <-  1 # 51 Aircraft
poorWorldResaleDM[26] <-  1 # 52 Air/Raft
poorWorldResaleDM[27] <-  1 # 53 Computers
poorWorldResaleDM[28] <-  1 # 54 ATV
poorWorldResaleDM[30] <-  4 # 56 Farm Machinery
poorWorldResaleDM[31] <-  4 # 61 Electronic Parts
poorWorldResaleDM[36] <-  1 # 66 Vacc Suit
} # End of Resale DMs

testDF <- data.frame(agrWorldPurchDM, 
                     nonAgrWorldPurchDM, 
                     indWorldPurchDM, 
                     nonIndWorldPurchDM, 
                     richWorldPurchDM, 
                     poorWorldPurchDM, 
                     agrWorldResaleDM, 
                     nonAgrWorldResaleDM, 
                     indWorldResaleDM, 
                     nonIndWorldResaleDM, 
                     richWorldResaleDM, 
                     poorWorldResaleDM, 
                     row.names = tradeGood)
