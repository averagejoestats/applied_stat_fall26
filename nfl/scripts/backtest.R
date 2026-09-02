
source("../R/funs.R")

for(j in 2015:2024){

    dat <- read_and_format_raw_season_data( j )
    m1 <- fit_bt01( dat )
    
    dat <- read_and_format_raw_season_data( j+1)
    m2 <- fit_bt01( dat )
    
    params <- data.frame( c1 = m1$coefficients )
    rownames(params) <- names( m1$coefficients )
    
    params$c2 <- NA
    params[ names( m2$coefficients ), "c2" ] <- m2$coefficients
    #print(params)
    
    print( c(j, cor( params[2:nrow(params),] )[1,2] ) )
}


vv <- seq(0,1.0,length.out=11)
years <- 2015:2024

scores <- matrix(NA, length(years), length(vv) )

for( j in 1:length(years) ){
    dat1 <- read_and_format_raw_season_data(years[j])
    m1 <- fit_bt01( dat1 )
    dat2 <- read_and_format_raw_season_data(years[j]+1)
    for(k in 1:length(vv)){
        dat2 <- predict_bt01( dat2, m1, vv[k] )
        scores[j,k] <- score_probs( dat2 )
    } 
}

plot(NA, type = "l", xlim = range(vv), ylim = range(scores) )
for(j in 1:nrow(scores)){
    lines( vv, scores[j,], type = "o" )
}
