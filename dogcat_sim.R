# dogcat_sim.R
# This job array comprises 250 jobs of 100 files each.

HSVConvert = function(file)
{
    jpg = readJPEG(file) # Imports JPEG in RGB format
    
    # Orders RGB values into a matrix
    x = matrix(jpg, nrow = dim(jpg[,,1])[1]*dim(jpg[,,1])[2],
               ncol = 3, byrow = FALSE)
    x = t(x) # Now in correct format (3x250^2)
    
    # Converts to HSV format
    x.hsv = rgb2hsv(x)
    x.hsv = t(x.hsv)
    x.hsv[,3] = x.hsv[,3]*255 # Naturally ranges 0 - 1/255 otherwise
    
    # Each pixel is listed in "order", want to recompose the picture
    hsv.array = array(x.hsv, c(250,250,3))
    
    # This is the original structure of the RGB data,
    # now in HSV form
    hsv.array  
}

# x is a single row of the pixel matrix
# idx.
outer3 = function(x, idx.h, idx.s, idx.v) 
{
    h = x[idx.h]
    s = x[idx.s]
    v = x[idx.v]
    as.vector(outer(as.vector(outer(h,s)),v)) # returns outer product
}

# This function takes an individual 250x250 jpeg and extracts
# all the hue, saturation, and value (HSV) features in the
# form of a vector.
# col = # of column strips
# row = # of row strips
# t = # of table divisions for H, S, and V respectively
# N = # pixels on a side for the picture
# jpeg = The file in question
# NOTE: Assumes square image, but could probably modify
# to allow for different picture side length, so long as
# they are multiples of col & row respectively.
HSVfeatures = function(col=5, row=5, t=c(8,8,8), N=250, jpeg)
{
    data = HSVConvert(jpeg) # Convert a JPEG into HSV values
    cells = numeric(0) # vector to contain HSV values
    pixel.h = numeric(0)
    pixel.s = numeric(0)
    pixel.v = numeric(0)
    # MUST define pixel to contain values
    
    # This code defines the intervals that each cell
    # contains. For col = 10, this produces a
    # 25x10 matrix, with each column representing the values
    # for a single cell.
    col.start = seq(1,N,N/col)
    col.end = seq((N/col),N,N/col)
    col.ints = matrix(0,N/row,col)
    for(i in 1:col)
    {
        # Each column of col.ints contains the values contained
        # in each start-end range pair
        # EX: If N = 5, col.ints[,j] = 1:50
        col.ints[,i] = col.start[i]:col.end[i]
    }
    row.start = seq(1,N,N/row)
    row.end = seq((N/row),N,N/row)
    row.ints = matrix(0,N/col,row)
    for(j in 1:row)
    {
        # Each column of row.ints contains the values contained
        # in each start-end range pair
        # EX: If N = 5, row.ints[,j] = 1:50
        row.ints[,j] = row.start[j]:row.end[j]
    }
    
    
    # full_start = proc.time() # Used for testing speed
    
    # The features vector we ultimately want for a single picture
    # We need values for each of the three features (H, S, and V,
    # divided into t table entries each), for each cell
    # (col*row different cells exist)
    # (250/col vertical strips and 250/row horizontal strips)
    pic.features = rep(NA,prod(t)*col*row)
    
    block.ctr = 1       # Counting variable
    hsv.ncomb = prod(t) # Number of HSV table value combinations
    
    for(c in 1:col)
    {
        for(r in 1:row)
        {
            # Selects the group of vector indices of length 512
            # to represent the current cell
            block.idx <- c((1 + ((block.ctr-1)*hsv.ncomb)):(hsv.ncomb*block.ctr))
            block.ctr = block.ctr + 1
            # For one row of the cell vector
            # Considers row r, column c, and slice s
            # t[s] is used to determine the table sizes

            # End result: 2500x512 vector: each column is a combination of
            # table values.
            
            start = proc.time()
            pixel.h <- matrix(NA,nrow=(N/row)*(N/col),ncol=t[1])
            pixel.s <- matrix(NA,nrow=(N/row)*(N/col),ncol=t[2])
            pixel.v <- matrix(NA,nrow=(N/row)*(N/col),ncol=t[3])
            
            # Next task: Create a SINGLE pixel matrix for h, s, and v
            # combined, which will be sum(t) columns
            pixel <- matrix(NA,nrow=(N/row)*(N/col),ncol=sum(t))
            
            idx.h <- c(1:t[1])
            idx.s <- c((t[1]+1):(t[1]+t[2]))
            idx.v <- c((t[1]+t[2]+1):sum(t))
            
            idx <- 1 
            nsub_r <- (N/row)
            nsub_c <- (N/col)
            if (abs(nsub_r - as.integer(nsub_r))>.Machine$double.eps){
                stop("'N/row' must be an integer")
            }
            if (abs(nsub_c - as.integer(nsub_c))>.Machine$double.eps){
                stop("'N/col' must be an integer")
            }
            
            for(pr in 1:nsub_r)
            {
                for(pc in 1:nsub_c)
                {
                    # t[1:3] = C.h,C.s,C.v
                    pixel[idx,idx.h] = table(cut(data[row.ints[pc,r],col.ints[pr,c],1],
                                                 seq(0,1,1/t[1])))
                    pixel[idx,idx.s] = table(cut(data[row.ints[pc,r],col.ints[pr,c],2],
                                                 seq(0,1,1/t[2])))
                    pixel[idx,idx.v] = table(cut(data[row.ints[pc,r],col.ints[pr,c],3],
                                                 seq(0,1,1/t[3])))
                    idx <- idx+1
                    # CHANGE: Need to count through pixel by pixel, so 1:N/col, 1:N/row
                    # cat("Finished pr=",pr,",pc=",pc,"...\n")
                }
            }
            result = apply(pixel, 1, outer3, idx.h = idx.h, idx.s = idx.s, idx.v = idx.v)
            # applies by row to the whole pixel matrix
            # yields 62500x512 matrix
            # Take column sums: if any are >1, reduce to 1.
            # pic.features will be a numeric(12800) vector of HSV features
            pic.features[block.idx] = as.numeric(rowSums(result) > 0)
            
            # Result: vector of length prod(t) (here, 8^3 = 512)
            # 
            time = proc.time() - start
            # This gives a 250^2 x 8 grid of table values for each pixel variable
            # Now we need to overlay all three grids to pinpoint which combinations
            # of the three yield at least one "hit"
            
            cat("Finished r=",r,",c=",c,"...\n",sep="") # Monitors progress
            
        }
        
    }
    pic.features
}
########################################################################################
########################################################################################
## Handle batch job arguments:

# 1-indexed version is used now.
args <- commandArgs(TRUE)

cat(paste0("Command-line arguments:\n"))
print(args)

####
# sim_start ==> Lowest simulation number to be analyzed by this particular batch job
###

#######################
sim_start <- 0
length.datasets <- 25000
batch.size = 100 # Can alter to test
jobs = length.datasets/batch.size
jobint = as.integer(jobs)
if(abs(jobs-jobint) > .Machine$double.eps)
{
    stop("Number of images not divisible by number of jobs.")
}
#######################

if (length(args)==0){
    sinkit <- FALSE
    sim_num <- sim_start + 1
    set.seed(1162977)
} else {
    # Sink output to file?
    sinkit <- TRUE
    # Decide on the job number, usually start at 1000:
    # NEED TO CHANGE THIS TO BE LENGTH batch.size
    sim_num <- sim_start + as.numeric(args[1]) - 1
    # Set a different random seed for every job number!
    set.seed(649*sim_num + 1162977)
}

# This identifies picture names properly to be included in
# a list of files to convert

if (sim_num < (jobs/2))
{
    animal = "cat"
    img.list = (sim_num*batch.size):(((sim_num+1)*batch.size) - 1)
} else {
    animal = "dog"
    img.list = ((sim_num-(jobs/2))*batch.size):(((sim_num+1-(jobs/2))*batch.size) - 1)
}

########################################################################################
########################################################################################

# Identifies all jpg files in the directory
library("jpeg")

# infile consists of all files numbered according to "num" and
# identified according to their animal
infile = lapply(img.list, FUN=function(x) paste0("rescaled.", animal, ".", x, ".jpg"))

N=250
grid=c(8,8,8)
col=5
row=5

results.mat = matrix(NA,batch.size,prod(grid)*col*row)

for(i in 1:batch.size)
{
    results.mat[i,] = HSVfeatures(jpeg=infile[[i]], col=col,row=row,t = grid, N=N) 
    cat("Finished image",i,"\n")
}

#################################################

outdir <- "data/"
outfile_data <- paste0(outdir,"hsvfeat.job.", sim_num, ".csv")

# We save the data through this code
write.table(results.mat,file=outfile_data,sep=",",quote=FALSE,row.names=FALSE,col.names=FALSE)
