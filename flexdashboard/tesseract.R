# https://shiring.github.io/text_analysis/2017/07/17/ocr_tesseract

# install.packages("tesseract")
library(tesseract)

# install.packages("EBImage")
# library(EBImage) # install from Bioconductor!

# color.image <- readImage("study_r/flexdashboard/pic/receipt.png")
# # bw.image <- channel(color.image,"gray")
# writeImage(bw.image,file="/Users/shiringlander/Documents/Github/Blog_posts_prep/ocr/beispiel_scan_bw.png")
# 
# color.image <- readImage("/Users/shiringlander/Documents/Github/Blog_posts_prep/ocr/beispiel_scan_2.jpg")
# bw.image <- channel(color.image,"gray")=====
# writeImage(bw.image,file="/Users/shiringlander/Documents/Github/Blog_posts_prep/ocr/beispiel_scan_2_bw.jpg")

# install.packages("magick")
library(magick)
image <- image_read("flexdashboard/pic/receipt.png")

# install.packages("tidyverse")
library(tidyverse)

image_bearb <- image %>%
  image_scale("x2000") %>%                        # rescale
  image_background("white", flatten = TRUE) %>%   # set background to white
  image_trim() %>%                                # Trim edges that are the background color from the image.
  image_noise() %>%                               # Reduce noise in image using a noise peak elimination filter
  image_enhance() %>%                             # Enhance image (minimize noise)
  image_normalize() %>%                           # Normalize image (increase contrast by normalizing the pixel values to span the full range of color values).
  image_contrast(sharpen = 1)                  # increase contrast
  # image_deskew(treshold = 40)                     # deskew image -> creates negative offset in some scans

image_browse(image_bearb)

image_write(image_bearb, path = "flexdashboard/pic/receipt_scan_bearb.png", format = "png")

whitelist <- "1234567890-.,;:qwertzuiopüasdfghjklöäyxcvbnmQWERTZUIOPÜASDFGHJKLÖÄYXCVBNM@ß€!$%&/()=?+"

text <- ocr("flexdashboard/pic/receipt_scan_bearb.png",
            engine = tesseract(language = "eng", 
                               options = list(tessedit_char_whitelist = whitelist)))

library(tidytext)
# library(SnowballC)
# 
# openthes <- data.frame(words = read_lines("/Users/shiringlander/Documents/Projekte/OCR_Provinzial/German_dict/OpenThesaurus-Textversion/openthesaurus.txt", skip = 18)) %>%
#   mutate(words = as.character(words)) %>%
#   unnest_tokens(word, words) %>%
#   mutate(word = wordStem(word, language = "deu")) %>%
#   distinct()

text_df <- data.frame(text = read.delim(textConnection(text),    # make text into dataframe
                                        header = FALSE, 
                                        sep = "\n", 
                                        strip.white = TRUE)) %>%
  mutate(text = as.character(V1)) %>%
  unnest_tokens(word, text) %>%                                      # separate words
  mutate(word = wordStem(word, language = "eng"))                    # use word stem

