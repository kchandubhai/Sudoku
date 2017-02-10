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
  
  for(i in 1:400){
    
    if(sum(is.na(sudoku_solution))==0) {break}
    ##print(i)
    sudoku <- update_grid(grid=sudoku,solution=sudoku_solution)
    sudoku <- fill_grid(grid=sudoku)
    sudoku_solution <- update_solution(grid=sudoku,solution=sudoku_solution)
    ##print(sudoku_solution)
    
  }
  
  return(sudoku_solution)
}

update_grid <- function(grid=sudoku,solution = sudoku_solution){
  
  #Fill places where numbers are not possible
  #grid <- array(rep(NA,721),c(9,9,9))
  
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
  
  # Create cell level summary
  cellsum <- array(rep(0,81),c(9,9))
  for(i in 1:9){
    if(x>0) break
    for(j in 1:9){
      if(x>0) break
      cellsum[i,j] <- sum(is.na(grid[i,j,]))
    }
  }
  
  #print(cellsum)
  
  #Create summary data structure to find the total number of NAs for each number
  
  summary <- array(rep(0,9*27),c(9,27))
  rownames(summary) <- 1:9
  colnames(summary)<-c(paste('r',1:9,sep=''),paste('c',1:9,sep=''),paste('g',1:9,sep=''))
  
  for(k in 1:9){
    if(x>0) break
    for(i in 1:9){
      if(x>0) break
      summary[k,i]<-sum(is.na(grid[i,,k]))
    }
  }
  
  for(k in 1:9){
    if(x>0) break
    for(j in 1:9){
      if(x>0) break
      summary[k,j+9]<-sum(is.na(grid[,j,k]))
    }
  }
  
  for(k in 1:9){
    if(x>0) break
    summary[k,19]<-sum(is.na(grid[1:3,1:3,k]))
    summary[k,20]<-sum(is.na(grid[1:3,4:6,k]))
    summary[k,21]<-sum(is.na(grid[1:3,7:9,k]))
    summary[k,22]<-sum(is.na(grid[4:6,1:3,k]))
    summary[k,23]<-sum(is.na(grid[4:6,4:6,k]))
    summary[k,24]<-sum(is.na(grid[4:6,7:9,k]))
    summary[k,25]<-sum(is.na(grid[7:9,1:3,k]))
    summary[k,26]<-sum(is.na(grid[7:9,4:6,k]))
    summary[k,27]<-sum(is.na(grid[7:9,7:9,k]))
  }
  
  #print(summary)
  
  #Single unoccupied cell in a row or column or grid (for any one number k)
  
  for(k in 1:9){
    if(x>0) break
    for(p in 1:27){
      if(x>0) break
      
      #print(paste('Going through cells k',k,', p',p))
      
      if(summary[k,p]==1){
        
        ##print('Going through cells with 1 in summary')
        
        if(p<10){
          i<-p
          na_column <- which(is.na(grid[i,,k]))
          grid[i,,k][na_column]<-k
          print(paste('Updated row number',i,', column number',na_column,'with',k))
          x<-1
          break
        }
        
        if(p>9 && p<19){
          j <- p-9
          na_row <- which(is.na(grid[,j,k]))
          grid[,j,k][na_row]<-k 
          print(paste('Updated column number',j,',row number',na_row,'with',k))
          x<-1  
          break
        }
        
        if(p>18){
          row_range <- switch((p-18),1:3,1:3,1:3,4:6,4:6,4:6,7:9,7:9,7:9)
          col_range <- switch((p-18),1:3,4:6,7:9,1:3,4:6,7:9,1:3,4:6,7:9)
          na_row <- which(is.na(grid[row_range,col_range,k]),arr.ind = T)[1]+switch((p-18),0,0,0,3,3,3,6,6,6)
          na_column <- which(is.na(grid[row_range,col_range,k]),arr.ind = T)[2]+switch((p-18),0,3,6,0,3,6,0,3,6)
          grid[na_row,na_column,k]<-k 
          print(paste('Updated column number',na_column,',row number',na_row,'with',k))
          x<-1  
          break
        }
      } 
      
      if(x>0) break
    }
    if(x>0) break
  }  
  
  if(x==0){
    for(i in 1:9){
      if(x>0) break
      for(j in 1:9){
        if(x>0) break
        if(sum(is.na(grid[i,j,]))==1) {
          na_number <- which(is.na(grid[i,j,]))
          grid[i,j,][na_number]<- na_number
          print(paste('Updated row number',i,'column number',j,'with',na_number))
          x<-1
          break
        }
        if(x>0) break
      }  
      if(x>0) break
    }
  }
  
  if(x==0){
    for(i in 1:9){
      if(x>0) break
      for(j in 1:9){
        if(x>0) break
        
        y<-0
        
        if(cellsum[i,j]>1){
          n <- cellsum[i,j]
          
          ##print('========================================')
          #print(paste('i:',i,'j:',j,'n:',n))
          ##print('cellsum[i,j] greater than 1')
          
          row_index <- which(cellsum[i,]==n)
          col_index <- which(cellsum[,j]==n)
          
          #print(paste('row_index:',row_index))
          #print(paste('col_index:',col_index))
          
          row_begin<-i+switch(i%%3+1,-2,0,-1)
          row_end<-i+switch(i%%3+1,0,2,1)
          
          col_begin <- j+switch(j%%3+1,-2,0,-1)
          col_end<-j+switch(j%%3+1,0,2,1)
          
          #print(paste('row_begin:',row_begin))
          #print(paste('row_end:',row_end))
          
          #print(paste('col_begin:',col_begin))
          #print(paste('col_end:',col_end))
          
          grid_index <- which(cellsum[row_begin:row_end,col_begin:col_end]==n)
          ##print('grid_index')
          ##print(grid_index)
          
          if(length(row_index)>=n){
            ##print('inside row_index if')
            for(l in 1:ncol(combn(row_index,n))){
              if(x>0) break
              index <- combn(row_index,n)[,l]
              
              ##print('index')
              #print(paste(index,collapse=","))
              
              vacant_cells <- sum(is.na(sudoku[i,,]))
              #print(paste('vacant cells :', vacant_cells))
              
              if(n*n<vacant_cells){
                ##print('inside vacant cells condition')
                numbers_list <- list()
                for(m in 1:n){
                  if(x>0) break
                  numbers_list <- c(numbers_list,list(which(is.na(grid[i,index[m],]))))
                  #print(paste('Inside loop m :', m))
                  ##print(numbers_list)
                  if(m>1){
                    if(!identical(numbers_list[[m-1]],numbers_list[[m]])) {
                      y<- 1
                      break
                    }
                  }
                }
                # Assumption here is all elements in the list are equal if y==0
                if(y==0){
                  ##print('Assumption here is all elements in the list are equal if y==0')
                  if(sum(is.na(grid[i,-index,numbers_list[[1]]]))>0){
                    grid[i,-index,numbers_list[[1]]]<-0
                    print(paste('Made 0 for all columns except',index,'in row',i,', for numbers',numbers_list[[1]]))
                    x<-1  
                    break
                  }
                  
                }
              }
            }
          }
          
          if(length(col_index)>=n){
            
            ##print('inside col_index if')
            for(l in 1:ncol(combn(col_index,n))){
              
              if(x>0) break
              y<- 0
              
              index <- combn(col_index,n)[,l]
              
              #print(paste(index,collapse=","))
              
              vacant_cells <- sum(is.na(sudoku[,j,]))
              #print(paste('vacant cells :', vacant_cells))
              
              if(n*n<vacant_cells){
                numbers_list <- list()
                for(m in 1:n){
                  if(x>0) break
                  numbers_list <- c(numbers_list,list(which(is.na(grid[index[m],j,]))))
                  #print(paste('Inside loop m :', m))
                  ##print(numbers_list)
                  if(m>1){
                    if(!identical(numbers_list[[m-1]],numbers_list[[m]])) {
                      ##print('Not identical!')
                      y<- 1
                      break
                    }
                  }
                }
                
                # Assumption here is all elements in the list are equal if y==0
                if(y==0){
                  ##print('Assumption here is all elements in the list are equal if y==0')
                  ##print('sum(is.na(grid[-index,j,numbers_list[[1]]]))')
                  ##print(sum(is.na(grid[-index,j,numbers_list[[1]]])))
                  
                  
                  
                  if(sum(is.na(grid[-index,j,numbers_list[[1]]]))>0){
                    grid[-index,j,numbers_list[[1]]]<-0
                    print(paste('Made 0 for all rows except',index,'in column',j,', for numbers',numbers_list[[1]]))
                    x<-1 
                    break
                  }
                  
                }
              }
            }
          }
          
          if(length(grid_index)>=n){
            #print(grid_index)
			for(l in 1:ncol(combn(grid_index,n))){
              if(x>0) break
              
              index <- combn(grid_index,n)[,l]
              
              #print(paste('i :',i,'j :',j, 'n :', n))
              print(paste(index,collapse=","))
              
              #print(paste('row_begin, row_end, col_begin, col_end',row_begin, row_end, col_begin, col_end))
              
              vacant_cells <- sum(is.na(sudoku[row_begin:row_end,col_begin:col_end,]))
              #print(paste('vacant cells :', vacant_cells))
              
              if(n*n<vacant_cells){
                numbers_list <- list()
                rownum<- rep(NA,n)
                colnum<- rep(NA,n)
                for(m in 1:n){
                  if(x>0) break
                  #print(paste('m:',m))
                  rownum[m]<- row_begin+switch(index[m],0,1,2,0,1,2,0,1,2)
                  colnum[m]<- col_begin+switch(index[m],0,0,0,1,1,1,2,2,2)
                  numbers_list <- c(numbers_list,list(which(is.na(grid[rownum[m],colnum[m],]))))
                  #print(paste('Inside loop m :', m))
                  ##print(numbers_list)
                  
                  if(m>1){
                    if(!identical(numbers_list[[m-1]],numbers_list[[m]])) {
                      y<- 1
                      break
                    }
                  }
                }
                # Assumption here is all elements in the list are equal if y==0
                if(y==0){
                  
                  vacant_index <- setdiff(1:9,index)
                  
                  ##print('vacant_index')
                  ##print(vacant_index)
                  
                  rownum<- rep(NA,length(vacant_index))
                  colnum<- rep(NA,length(vacant_index))
                  
					#print(rownum)
					#print(vacant_index)
                  for(m in 1:length(vacant_index)){
                    if(x>0) break
					#print(m)
                    rownum[m]<- row_begin+switch(vacant_index[m],0,1,2,0,1,2,0,1,2)
                    colnum[m]<- col_begin+switch(vacant_index[m],0,0,0,1,1,1,2,2,2)
                    if(sum(is.na(grid[rownum[m],colnum[m],as.array(numbers_list[[1]])]))>0){
                      grid[rownum[m],colnum[m],as.array(numbers_list[[1]])]<-0
                      print(paste('Made 0 for',as.array(numbers_list[[1]]),'in row',rownum[m],', column',colnum[m]))
                      x<-1 
                      break
                    }
                  }
                }
              }
            }
          }
          
        }
      }
    }
  }
  
  if(x==0){
    
    for(k in 1:9){
      if(x>0) break
      for(p in 1:27){
        if(x>0) break
        
        y<-0
        
        if(summary[k,p]>1){
          
          n <- summary[k,p]
          print(paste('k :',k,'p :',p, 'n :', n))
          #Check for pointing pair in a row
          if(p<10){
            if(n<4){
              na_range <- which(is.na(grid[p,,k]))
              #print(na_range)
              
              is_grid_same = 1
              
              #print('checking for NAs in same grid')
              
              row_mod <- ifelse(p<4,1,ifelse(p>6,3,2))		  
              
              grid_num <- list()				  
              
              for(q in 1:n){
                col_mod <- ifelse(na_range[q]<4,1,ifelse(na_range[q]>6,3,2))
                grid_num <- c(grid_num,list(c(row_mod,col_mod)))
				print(paste('q:',q))
				#print(grid_num)
                if(q>1){
                  if(!identical(grid_num[[q-1]],grid_num[[q]])){
                    #print('setting is_grid_same to 0')
                    is_grid_same = 0
					break
                  }
                }                    
              }
              if(is_grid_same==1){
			  #print('is_grid_same is 1.. going ahead with updates')
			  row_begin<-p+switch(p%%3+1,-2,0,-1)
              row_end<-p+switch(p%%3+1,0,2,1)
              
              col_begin <- na_range[1]+switch(na_range[1]%%3+1,-2,0,-1)
              col_end<-na_range[1]+switch(na_range[1]%%3+1,0,2,1)
              
              if(sum(is.na(grid[row_begin:row_end,col_begin:col_end,k]))>n){
                grid[row_begin:row_end,col_begin:col_end,k]<- 0
                grid[p,na_range,k]<-NA
                print(paste('Made 0 for',k,'in all numbers of the grid except row',p,'columns',na_range))
                x<-1
                break
              }
			  }             
              
            }
          }
          
          #Check for pointing pair in a column
          if(p>9 && p<19){
            if(n<4){
              na_range <- which(is.na(grid[,p-9,k]))
              #print(na_range)
              
              is_grid_same = 1
              
              #print('checking for NAs in same grid')
              
              col_mod <- ifelse((p-9)<4,1,ifelse((p-9)>6,3,2))
              
              grid_num <- list()				  
              
              for(q in 1:n){
                row_mod <- ifelse(na_range[q]<4,1,ifelse(na_range[q]>6,3,2))
                grid_num <- c(grid_num,list(c(row_mod,col_mod)))
				print(paste('q:',q))
				#print(grid_num)
                if(q>1){
                  if(!identical(grid_num[[q-1]],grid_num[[q]])){
                    #print('setting is_grid_same to 0')
                    is_grid_same = 0
					break
                  }
                }                    
              }
              
			  if(is_grid_same==1){
			    #print('is_grid_same is 1.. going ahead with updates')
				row_begin<-na_range[1]+switch(na_range[1]%%3+1,-2,0,-1)
				row_end<-na_range[1]+switch(na_range[1]%%3+1,0,2,1)
              
				col_begin <- (p-9)+switch((p-9)%%3+1,-2,0,-1)
				col_end<-(p-9)+switch((p-9)%%3+1,0,2,1)
              
              if(sum(is.na(grid[row_begin:row_end,col_begin:col_end,k]))>n){
                grid[row_begin:row_end,col_begin:col_end,k]<- 0
                grid[na_range,(p-9),k]<-NA
                print(paste('Made 0 for',k,'in all numbers of the grid except rows',na_range,'column',(p-9)))
                x<-1
                break
              }
              }
            }
          }
          #Check for pointing pair in a grid
          if(p>18){
            row_range <- switch((p-18),1:3,1:3,1:3,4:6,4:6,4:6,7:9,7:9,7:9)
            col_range <- switch((p-18),1:3,4:6,7:9,1:3,4:6,7:9,1:3,4:6,7:9)
            
            #Check for NAs in the same row or columns in a grid to eliminate other candidates
            # in the same row or column in other grids
            if(n<4){
              na_range <- which(is.na(grid[row_range,col_range,k]),arr.ind=T)
              is_row_same = 1
              is_col_same = 1
              
              #print('checking for NAs in same row or column')
              #print(na_range)
              
              row_num <- na_range[1,][1]
              col_num <- na_range[1,][2]
              
              #print(paste('row_num',row_num))
              #print(paste('col_num',col_num))
              
              for(q in 1:n){
                if(x>0) break
                print(paste('rownum for q',q,'is',na_range[q,][1]))
                print(paste('colnum for q',q,'is',na_range[q,][2]))
                if(row_num != na_range[q,][1]){
                  #print('setting is_row_same to 0')
                  is_row_same = 0
                }
                if(col_num != na_range[q,][2]){
                  #print('setting is_col_same to 0')
                  is_col_same = 0
                }
              }
              
              i <- row_num+switch((p-18),0,0,0,3,3,3,6,6,6)
              j <- col_num+switch((p-18),0,3,6,0,3,6,0,3,6)
              
              
              print(paste('i,j :', i,',',j))
              ##print(row_range)
              ##print(col_range)
              
              #print(paste('is_row_same:',is_row_same))
              #print(paste(sum(is.na(grid[i,-col_range,k]))))
              #print(paste('is_col_same:',is_col_same))
              #print(paste(sum(is.na(grid[-row_range,j,k]))))
              
              
              if(is_row_same ==1 && sum(is.na(grid[i,-col_range,k]))>0){
                
                grid[i,-col_range,k] <- 0
                #print('NAs in same row in grid!')
                print(paste('Updated to 0 all columns not between',col_range[1],'and',col_range[3],', in row',i,'for number',k))
                x<-1
                break
              }
              if(is_col_same ==1 && sum(is.na(grid[-row_range,j,k]))>0){
                
                grid[-row_range,j,k] <- 0
                #print('NAs in same column in grid!')
                print(paste('Updated to 0 all rows not in',row_range,', in column',j,'for number',k))
                x<-1
                break
              }
            }
          }
          
          all_index <- which(summary[,p]==n)
          
          #print(all_index)
          print(paste(all_index,collapse=","))              
          
          if(length(all_index)>=n){
            for(l in 1:ncol(combn(all_index,n))){
              if(x>0) break
              index <- combn(all_index,n)[,l]
              
              print(paste(index,collapse=","))
              
              if(p<10){
                i<-p
                
                vacant_cells <- sum(is.na(sudoku[i,,]))
                #print(paste('vacant cells :', vacant_cells))
                
                if(n*n<vacant_cells){
                  numbers_list <- list()
                  for(m in 1:n){
                    if(x>0) break
                    numbers_list <- c(numbers_list,list(which(is.na(grid[i,,index[m]]))))
                    print(paste('Inside loop m :', m))
                    #print(numbers_list)
                    if(m>1){
                      if(!identical(numbers_list[[m-1]],numbers_list[[m]])) {
                        y<- 1
                        break
                      }
                    }
                  }
                  # Assumption here is all elements in the list are equal if y==0
                  if(y==0){
					#print('Assumption here is all elements in the list are equal')
                    if(sum(is.na(grid[i,-numbers_list[[1]],index]))>0){
                      grid[i,-numbers_list[[1]],index]<-0
                      print(paste('Made 0 for',index,'in row',i,', columns not in',numbers_list[[1]]))
                      x<-1  
                      break
                    }
					#Update for hidden pair
					if(sum(is.na(grid[i,numbers_list[[1]],-index]))>0){
                      grid[i,numbers_list[[1]],-index]<-0
                      print(paste('Made 0 for other than',index,'in row',i,', columns',numbers_list[[1]]))
                      x<-1  
                      break
                    }                    
                  }
                }
              }
              
              if(p>9 && p<19){
                j <- p-9
                
                vacant_cells <- sum(is.na(sudoku[,j,]))
                
                #print(paste('vacant cells :', vacant_cells))
                
                if(n*n<vacant_cells){
                  index <- which(summary[,p]==n)
                  #print(paste('index:',index))
                  numbers_list <- list()
                  for(m in 1:n){
                    if(x>0) break
                    numbers_list <- c(numbers_list,list(which(is.na(grid[,j,index[m]]))))
                    
                    #print(paste('Inside loop m :', m))
                    ##print(numbers_list)
                    
                    if(m>1){
                      if(!identical(numbers_list[[m-1]],numbers_list[[m]])) {
                        y<- 1
                        break
                      }
                    }
                  }
                  # Assumption here is all elements in the list are equal if y==0
                  if(y==0){                    
                    if(sum(is.na(grid[-numbers_list[[1]],j,index]))>0){
                      grid[-numbers_list[[1]],j,-index]<-0
                      print(paste('Made 0 for ',index,'in rows not in',numbers_list[[1]],', column ',j))
                      x<-1  
                      break
                    }
					#Update for hidden pair
					if(sum(is.na(grid[numbers_list[[1]],j,-index]))>0){
                      grid[numbers_list[[1]],j,-index]<-0
                      print(paste('Made 0 for other than',index,'in row',numbers_list[[1]],', columns',j))
                      x<-1  
                      break
                    }
                    
                  }
                }
              }
              
              if(p>18){                
                
                vacant_cells <- sum(is.na(sudoku[row_range,col_range,]))
                
                #print(paste('k :',k,'p :',p, 'n :', n, 'vacant cells :', vacant_cells))
                
                if(n*n<vacant_cells){
                  
                  numbers_list <- list()
                  for(m in 1:n){
                    if(x>0) break
                    numbers_list <- c(numbers_list,list(which(is.na(grid[row_range,col_range,index[m]]))))
                    #print(paste('Inside loop m :', m))
                    ##print(numbers_list)
                    
                    if(m>1){
                      if(!identical(numbers_list[[m-1]],numbers_list[[m]])) {
                        y<- 1
                        break
                      }
                    }
                  }
                  # Assumption here is all elements in the list are equal if y==0
                  if(y==0){
                    
                    ##print(row_range)
                    ##print(col_range)
                    ##print('index:')
                    ##print(index)
                    vacant_index <- which(is.na(grid[row_range,col_range,index[1]]))
                    
                    ##print('vacant index')
                    ##print(vacant_index)
                    if(!identical(numbers_list[[1]],vacant_index)){
                      index_to_update <- setdiff(1:9,numers_list[[1]])
                      for(m in 1:length(index_to_update)){
                        if(x>0) break
                        row_num <- index_to_update[m]+switch((p-18),0,0,0,3,3,3,6,6,6)
                        col_num <- index_to_update[m]+switch((p-18),0,3,6,0,3,6,0,3,6)  
                        
                        ##print(row_num)
                        ##print(col_num)
                        
                        grid[row_num,col_num,index]<-0
                      }
                      print(paste('Made 0 for ',index,'in row',row_num,', column',col_num))                    
                    }
					#Update for hidden pair
					if(identical(numbers_list[[1]],vacant_index)){                      
                      for(m in 1:length(numbers_list[[1]])){
                        #print(numbers_list[[1]])
                        row_num <- switch(numbers_list[[1]][m],1,2,3,1,2,3,1,2,3)+switch((p-18),0,0,0,3,3,3,6,6,6)
                        col_num <- switch(numbers_list[[1]][m],1,1,1,2,2,2,3,3,3)+switch((p-18),0,3,6,0,3,6,0,3,6)  
                        
                        #print(row_num)
                        #print(col_num)
                        if(sum(is.na(grid[row_num,col_num,-index]))>0){
							grid[row_num,col_num,-index]<-0
							x<-1
							print(paste('Made 0 for other than',index,'in row',row_num,', column',col_num))                    							
						}                        
                      }
					  if(x>0){						
						break
					  }                      
                    }
                  }
                }
              }
            }
          }
          
        }
      }        
    } 
	#search for naked triples
	
	for(var in 1:9){
		if(x>0) break	
		
		combs <- combn(9,3)
		##print('combs')
		##print(combs)		
		print(paste('var:',var))
		#In rows
		for(m in 1:ncol(combs)){		    
			if(x>0) break
			comb_possible <- 1
			loop_complete <- 0
		    #print('row check')
			row_naked_numbers <- vector()		
		    row_index <- combs[,m]
			##print('new combination')
			##print(row_index)
			for(q in 1:3){
			    if(comb_possible==0) break	
				##print(row_index[q])
				if(sum(is.na(grid[var,row_index[q],]))>3 || sum(is.na(grid[var,row_index[q],]))==0) {
					comb_possible <- 0
					break				
				}
				row_naked_numbers<- union(row_naked_numbers,which(is.na(grid[var,row_index[q],])))
				##print(row_naked_numbers)
				if(length(row_naked_numbers)>3) {
					comb_possible <- 0
					break				
				}
				if(q==3) loop_complete <- 1
			}
			if(loop_complete ==1 && length(row_naked_numbers)==3 && sum(is.na(grid[var,-row_index,row_naked_numbers]))>0){
				#print(row_index)
				#print(row_naked_numbers)
				#print('naked row triple found')
				grid[var,-row_index,row_naked_numbers]<-0
				x<-1
				print(paste('Updated to 0 for',row_naked_numbers,'in row',var,'columns other than',row_index))
				break
			}	
		}
		
		#In columns
		for(m in 1:ncol(combs)){
			if(x>0) break
			#print('col check')
			comb_possible <- 1
			loop_complete <- 0
			col_naked_numbers <- vector()
		    col_index <- combs[,m]
			if(var==5) {
				print('new combination')
				print(col_index)			
			}
			for(q in 1:3){
			    if(comb_possible==0) break	
				if(var==5) print(col_index[q])
				if(sum(is.na(grid[col_index[q],var,]))>3 || sum(is.na(grid[col_index[q],var,]))==0) {
					comb_possible <- 0
					break				
				}
				col_naked_numbers<- union(col_naked_numbers,which(is.na(grid[col_index[q],var,])))
				if(length(col_naked_numbers)>3) {
					comb_possible <- 0
					break				
				}
				if(var==5) print(col_naked_numbers)
				if(q==3) loop_complete <- 1
			}
			if(loop_complete ==1 && length(col_naked_numbers)==3 && sum(is.na(grid[-col_index,var,col_naked_numbers]))>0){
				print('naked column triple found')
				grid[-col_index,var,col_naked_numbers]<-0
				x<-1
				print(paste('Updated to 0 for',col_naked_numbers,'in row other than',col_index,'column',var))
				break
			}	
		}
		
		#In Grids
		
		for(m in 1:ncol(combs)){
		    if(x>0) break
			#print('grid check')
			comb_possible <- 1
			loop_complete <- 0
			grid_naked_numbers <- vector()
		    grid_index <- combs[,m]
			row_adder <- switch(var,0,0,0,3,3,3,6,6,6)
			col_adder <- switch(var,0,3,6,0,3,6,0,3,6)
			
			row_num_vector<-vector()
			col_num_vector<-vector()
				
			
			for(q in 1:3){
			    if(comb_possible==0) break	
				row_num <- row_adder + switch(grid_index[q],1,1,1,2,2,2,3,3,3)
				col_num <- col_adder + switch(grid_index[q],1,2,3,1,2,3,1,2,3)
				if(sum(is.na(grid[row_num,col_num,]))>3 || sum(is.na(grid[row_num,col_num,]))==0) {
					comb_possible <- 0
					break				
				}
				grid_naked_numbers<- union(grid_naked_numbers,which(is.na(grid[row_num,col_num,])))
				if(length(grid_naked_numbers)>3) {
					comb_possible <- 0
					break				
				}
				if(q==3) loop_complete <- 1
			}
			
			vacant_index <- setdiff(1:9,grid_index)
			#print(paste('vacant_index:',vacant_index))
			rownum<- rep(NA,length(vacant_index))
            colnum<- rep(NA,length(vacant_index))
            				
			if(loop_complete ==1 && length(grid_naked_numbers)==3){
				for(m in 1:length(vacant_index)){
					rownum[m]<- row_adder+switch(vacant_index[m],1,1,1,2,2,2,3,3,3)
					colnum[m]<- col_adder+switch(vacant_index[m],1,2,3,1,2,3,1,2,3)
					if(sum(is.na(grid[rownum[m],colnum[m],grid_naked_numbers]))>0){
						grid[rownum[m],colnum[m],grid_naked_numbers]<-0
						print('naked grid triple found')
						print(grid_index)
						print(grid_naked_numbers)
						print(rownum)
						print(colnum)				
						x<-1
						print(paste('Updated to 0 for',grid_naked_numbers,'in rows',rownum[m],'columns',colnum[m]))						
					}
				}
                if(x>0) break				
			}	
		}		
	}
	
	#Search for X wing
	if(x==0){
		for(i in 1:9){
		for(j in 1:9){
			if(x==1) break
			grid_possible <- 0
			q<-z<-p<-l<-0
			
			num <- which(is.na(grid[i,j,]))
			print(paste('i,j:',i,j))
			print(paste('num:',num))
			if(length(num)>0){
			i1<-i
			j1<-j
			for(q in 1:length(num)){
				print('in for loop with q')
				if(x==1 || grid_possible ==1) break				
				print(paste('num:',num[q]))
				cols <- setdiff(which(is.na(grid[i1,,num[q]])),j1)
                print(paste('cols:',cols))
				if(length(cols)>0){					
					for(z in 1:length(cols)){
						print('Inside if of cols > 1')
						print(grid_possible)
						if(grid_possible==1) break
						rows <- setdiff(which(is.na(grid[,cols[z],num[q]])),i1)
						print(z)
						print(cols)
						print(cols[z])
						print(paste('cols[z]:',cols[z]))
						print(paste('rows:',rows))
						if(length(rows)>0){
							for(p in 1:length(rows)){
								if(grid_possible==1) break
								cols2 <- setdiff(which(is.na(grid[rows[p],,num[q]])),cols[z])
								print(paste('rows[p]:',rows[p]))
								print(paste('cols2:',cols2))				
								if(length(cols2)>0){
									for(l in 1:length(cols2)){
										if(grid_possible==1) break
										if(cols2[l]==j1){
											print('found x grid!')											
											grid_possible <- 1
											row_index <- c(i1,rows[p])
											col_index <- c(j1,cols[z])
											print(row_index)
											print(col_index)
										}
									}
								}
							}
						}
					}
				}										
				if(grid_possible==1 && (sum(is.na(grid[row_index,setdiff(1:9,col_index),num[q]]))+sum(is.na(grid[setdiff(1:9,row_index),col_index,num[q]])) >0)){
					grid[row_index,setdiff(1:9,col_index),num[q]] <- 0
					grid[setdiff(1:9,row_index),col_index,num[q]] <- 0
					x <- 1					
					print(paste('X grid identified! Set 0 for',num[q],'in other than x grid row numbers of',row_index,'and column index of',col_index))
					break
				}
			}
			}
			
			
			i1 <- i
			j1 <- j
			
			
		}
	}
	}
	
		
		
}  
  return(grid)
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


