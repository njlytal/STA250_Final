# dogcat_misc.R

# Contains miscellaneous data conversion code not
# present in the R file submitted to Gauss for conversion

# ****** Initial Image Conversion ******

library(jpeg)

# Due to differences in dimensions, we should STANDARDIZE
# the image sizes by rescaling to a common size (250 x 250)

# Shell code to do this
# Rescaled images to 250x250
convert dog.*.jpg -resize 250x250\! dog.rescaled.jpg

convert cat.*.jpg -resize 250x250\! cat.rescaled.jpg
# WARNING: This does NOT work when applied to all 15000
# jpgs at once...a memory leak occurs. Up to 1000 seems
# to work without a problem though.

# SOLUTION: shell script as follows:

for f in cat.*.jpg
do
echo "Processing $f"
newname=${f/cat/rescaled.cat}
echo "Converting to: $newname"
convert $f -resize 250x250\! $newname
echo "done."
done

for f in dog.*.jpg
do
echo "Processing $f"
newname=${f/dog/rescaled.dog}
echo "Converting to: $newname"
convert $f -resize 250x250\! $newname
echo "done."
done


# dogcat_sim.R contains functions not shown here


# ****** Converting Complete Jobs into Usable Form ******

# B = number of jobs
# files.per = files contained in each job
# cols = number of columns in data (12800 here)
csvComboAlt = function(dir,B,files.per, cols)
{
    output = matrix(NA,B*files.per,cols)
    for(i in 0:(B-1))
    {
        file = paste0(dir,"hsvfeat.job.", i, ".csv")
        data = read.csv(file, quote="\"", header = FALSE)
        
        
        output[(1+(i*files.per)):((i+1)*(files.per)),]=
        matrix(as.integer(unlist(data)),nrow=files.per, byrow = F)
    }
    # Turns matrix of doubles into one of integers to save space
    output
}

start = proc.time()
x = csvComboAlt(directory,25,100,12800)
time = proc.time() - start

