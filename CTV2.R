Citytrees <- read_delim("Desktop/Citytrees.csv", 
                        ";", escape_double = FALSE, col_types = cols(Tno = col_double(), 
                                                                     `dbh (m)` = col_double(), `Clearance Ht (m)` = col_double(), 
                                                                     `% Variation` = col_double()), locale = locale(decimal_mark = ","), 
                        trim_ws = TRUE)

plot(Citytrees$'Ht (m)', Citytrees$'Crown Diameter (m)')
