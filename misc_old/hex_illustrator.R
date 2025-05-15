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

ggplot(aes)


################################################################################
# Create Hex
# Load Image from Misc File
################################################################################

imgurl <- 'Misc/rf2.png'

hexSticker::sticker(imgurl,
                    package="Predictions as Inference",
                    p_size= 11,
                    p_y = 0.65,
                    p_color = 'gray5',
                    p_fontface = 'bold',
                    s_x=1,
                    h_fill = 'honeydew3',
                    h_color = 'olivedrab4',
                    s_y=1.3,
                    s_width=.7,
                    filename="Misc/PAI_hex.png")


