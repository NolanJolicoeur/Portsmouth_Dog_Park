library(sqldf)
    ## Concepts
    ## 1. Put all the data files to be processed in a single folder
    ## 2. read the file names
    
    # files = grep("._a\\.csv", dir(), value = T)
    
    ## 3. Create empty dataframe to be populated with data from the files
    
    # df <- data.frame(Doubles=double(), #col named 'Doubles' with double (real number) data type
    #                  Integers=integer(),
    #                  Factors=factor(),
    #                  Logicals=logical(),
    #                  Characters=character())
    
    ## 4. check df is a dataframe 
    # stopifnot(class(df) == 'data.frame')
    # 
    ## 5. check there are 5 columns in df
    # stopifnot(length(colnames(df)) == 5)


# Example
    # 1. get some toy data to populate an empty dataframe
    # first load the data and split into 3 groups

data(mtcars)
df1 = mtcars[1:10,]
df2 = mtcars[11:20,]
df3 = mtcars[21:32,]

# toydir = 'toyDatafiles'
# dir.create(toydir)
# setwd(toydir)

# I will save these to files to simulate getting them from raw data files
tblnames = c('tbl1.csv','tbl2.csv','tbl3.csv')
tbls = list(df1, df2, df3) # must use a list object to make an iterable of dfs

# create data files
for(i in 1:length(tblnames)){
    write.csv(tbls[i],tblnames[i])
}

# read in first table
tbl = read.csv('tbl1.csv')

# get column names
nms = colnames(tbl)

# make empty df
df <- data.frame(matrix(ncol = length(nms), nrow = 0))
colnames(df) = nms

# populate df with data in tables
for (t in dir()){
    otherdf = read.csv(t)
    df = sqldf("
               SELECT * FROM df
               UNION
               SELECT * FROM otherdf
               ")
}

# make rownames
rownames(df) <- df[,1]
df[,1] = NULL

# compare df to mtcars to ensure data was loaded successfully
stopifnot(prod(mtcars[ order(row.names(mtcars)),] == df)==1)
