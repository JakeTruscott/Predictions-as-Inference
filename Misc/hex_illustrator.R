################################################################################
# Hex Image Developer
# Jake S. Truscott
# Predictions as Inference Package (R)
# Updated March 2024
################################################################################

################################################################################
#Load Packages
################################################################################
library(rpart.plot); library(hexSticker); library(randomForest); library(tikzDevice); library(png)

################################################################################
# Create Hex
# Load Image from Misc File
################################################################################

imgurl <- 'Misc/rf_with_tree.png'

hexSticker::sticker(imgurl,
                    package="PAI",
                    p_size=25,
                    p_y = 1.55,
                    p_color = 'gray5',
                    p_fontface = 'bold',
                    s_x=1,
                    h_fill = 'honeydew3',
                    h_color = 'olivedrab4',
                    s_y=.65,
                    s_width=.75,
                    filename="Misc/PAI_hex.png")


