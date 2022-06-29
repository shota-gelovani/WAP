#### WAP ####
ab$like_CDU_CSU <- as.numeric(ab$pv19) # pv19 is a response to the feeling thermometer item (0-10 in this case)
ab$like_SPD <- as.numeric(ab$pv20)
ab$like_FDP <- as.numeric(ab$pv21)
ab$like_Gruenen <- as.numeric(ab$pv22) 
ab$like_Linke <- as.numeric(ab$pv23)
ab$like_AfD <- as.numeric(ab$pv24)
ab <- ab[complete.cases(ab[51:56]),] # columns from like_CDU_CSU to like_AfD

# Calculating vp for each party
# vp is the share of votes in the total votes received by all parties included in the analysis
totalvotes <- 15317344 + 9539381 + 5878115 + 4999449 + 4297270 + 4158400
ab$vp_CDU_CSU <- 15317344/totalvotes
ab$vp_SPD <- 9539381/totalvotes
ab$vp_AfD <- 5878115/totalvotes
ab$vp_FDP <- 4999449/totalvotes
ab$vp_Linke <- 4297270/totalvotes
ab$vp_Gruenen <- 4158400/totalvotes

ab$mean_affect <- na.omit(ab$vp_CDU_CSU * ab$like_CDU_CSU + ab$vp_SPD * ab$like_SPD + ab$vp_AfD * ab$like_AfD + ab$vp_FDP * ab$like_FDP + ab$vp_Linke * ab$like_Linke + ab$vp_Gruenen * ab$like_Gruenen)

ab$WAP_CSU_CDU <- ab$vp_CDU_CSU * (ab$like_CDU_CSU - ((ab$mean_affect)))^2
ab$WAP_SPD <- ab$vp_SPD * (ab$like_SPD - ((ab$mean_affect)))^2
ab$WAP_AfD <- ab$vp_AfD * (ab$like_AfD - ((ab$mean_affect)))^2
ab$WAP_FDP <- ab$vp_FDP * (ab$like_FDP - ((ab$mean_affect)))^2
ab$WAP_Linke <- ab$vp_Linke * (ab$like_Linke - ((ab$mean_affect)))^2
ab$WAP_Gruenen <- ab$vp_Gruenen * (ab$like_Gruenen - ((ab$mean_affect)))^2

ab$WAP <- sqrt(rowSums(ab[,c(64:69)])) # columns from WAP_CSU_CDU to WAP_Gruenen