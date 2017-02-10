#Sudoku.R
# get puzzle input

upload_puzzle <- function(dir="C:/Personal",filename="sudoku.csv"){
  setwd(dir)
  sudoku_puzzle <- as.matrix(read.table(filename,sep=","),9)
  colnames(sudoku_puzzle) <-1:9
  return(sudoku_puzzle)
}

solve_sudoku<- function(puzzle = sudoku_puzzle){

  sudoku_solution <- puzzle
  
  #Copy puzzle to another multi dimensional array
  sudoku <- array(rep(NA,721),c(9,9,9))
  
  for(i in 1:81){
    
  sudoku <- update_grid(grid=sudoku,solution=sudoku_solution)
  sudoku <- fill_grid(grid=sudoku)
  sudoku_solution <- update_solution(grid=sudoku,solution=sudoku_solution)
  
  }
  
  return(sudoku_solution)
}

update_grid <- function(grid=sudoku,solution = sudoku_solution){
  
  #Fill places where numbers are not possible
  
  for(i in 1:9) {
    for(j in 1:9){
      for(k in 1:9){
        if(!is.na(solution[i,j]) && solution[i,j]==k){
          grid[i,j,k] <-k
          grid[i,-j,k] <-0 #All columns in that row
          grid[-i,j,k] <-0 #All rows in that column
          grid[i,j,-k] <-0 #Other numbers for that position
          
          # In the 3x3 grid
          
          rowvar1 <- switch(i%%3+1,-2,1,-1)
          rowvar2 <- switch(i%%3+1,-1,2,1)
          
          colvar1 <- switch(j%%3+1,-2,1,-1)
          colvar2 <- switch(j%%3+1,-1,2,1)
          
          grid[i,j+colvar1,solution[i,j]] <- 0
          grid[i,j+colvar2,solution[i,j]] <- 0
          grid[i+rowvar1,j,solution[i,j]] <- 0
          grid[i+rowvar1,j+colvar1,solution[i,j]] <- 0
          grid[i+rowvar1,j+colvar2,solution[i,j]] <- 0
          grid[i+rowvar2,j,solution[i,j]] <- 0
          grid[i+rowvar2,j+colvar1,solution[i,j]] <- 0
          grid[i+rowvar2,j+colvar2,solution[i,j]] <- 0
        }
      }
    }
  }
  
  return(grid)
}

fill_grid <- function(grid=sudoku){
  
  x <- 0
  
  #Create summary data structure to find the total number of NAs for each number
  
  summary <- array(rep(0,9*27),c(9,27))
  rownames(summary) <- 1:9
  colnames(summary)<-c(paste('r',1:9,sep=''),paste('c',1:9,sep=''),paste('g',1:9,sep=''))
  
  for(k in 1:9){
    for(i in 1:9){
      summary[k,i]<-sum(is.na(grid[i,,k]))
    }
  }
  
  for(k in 1:9){
    for(j in 1:9){
      summary[k,j+9]<-sum(is.na(grid[,j,k]))
    }
  }
  
  for(k in 1:9){
    summary[k,19]<-sum(is.na(grid[1:3,1:3,k]))
    summary[k,20]<-sum(is.na(grid[1:3,4:6,k]))
    summary[k,21]<-sum(is.na(grid[1:3,6:9,k]))
    summary[k,22]<-sum(is.na(grid[4:6,1:3,k]))
    summary[k,23]<-sum(is.na(grid[4:6,4:6,k]))
    summary[k,24]<-sum(is.na(grid[4:6,7:9,k]))
    summary[k,25]<-sum(is.na(grid[7:9,1:3,k]))
    summary[k,26]<-sum(is.na(grid[7:9,4:6,k]))
    summary[k,27]<-sum(is.na(grid[7:9,7:9,k]))
  }
  
  
  
  #Single unoccupied cell in a row (for any one number k)
  for(k in 1:9){
    for(i in 1:9){
      if(sum(is.na(grid[i,,k]))==1) {
        na_column <- which(is.na(grid[i,,k]))
        grid[i,,k][na_column]<-k
        print(paste('Updated row number',i,', column number',na_column,'with',k))
        x<-1
      }
      if(x>0) break
    }
    if(x>0) break
  }
  
    #Single unoccupied cell in a column (for any one number k)
    if(x==0){
      for(k in 1:9){
        for(j in 1:9){
          if(sum(is.na(grid[,j,k]))==1) {
            na_row <- which(is.na(grid[,j,k]))
            grid[,j,k][na_row]<-k 
            print(paste('Updated column number',j,',row number',na_row,'with',k))
            x<-1
          }
          if(x>0) break
        }  
        if(x>0) break
      }
    }
  
  #Single value possible in a cell (across all numbers)
  if(x==0){
    for(i in 1:9){
      for(j in 1:9){
        if(sum(is.na(grid[i,j,]))==1) {
          na_number <- which(is.na(grid[i,j,]))
          grid[i,j,][na_number]<- na_number
          print(paste('Updated row number',i,'column number',j,'with',na_number))
          x<-1
        }
        if(x>0) break
      }  
      if(x>0) break
    }
  }
  
  return(summary)
}

update_solution <- function(grid = sudoku, solution = sudoku_solution){
  
  for(i in 1:9){
    for(j in 1:9){
      for(k in 1:9){
        if(!is.na(grid[i,j,k]) && grid[i,j,k]>0) 
          solution[i,j] <- grid[i,j,k]
      }
    }
  }
  
  return(solution)
}


