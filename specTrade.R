# this is to create a spec trade function
# this is drawn from pages 42 to 44 of GDW's 
# Traveller LBB2 pub 1977

# inputs required
purchWorldPop <- 7



# create the Trade and Spcutlation Table
tradeGood <- vector(mode = "character", length = 36)
basePrice <- vector(mode = "numeric", length = 36)

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
qtyMuly <- vector(mode = "numeric", length = 36)