library(tidyverse)
setwd("C:\\Users\\pistachio\\Projects\\advent-of-code")


# Day 1 -------------------------------------------------------------------

#input01 <- c(1, -2, 3, 1)  # test case

input01 <- read.table("./data/input-2018-Q01-1.txt")
input01 <- input01$V1

stack <- 0
val <- 0
i <- 0

repeat {
  val <- val + input01[i %% length(input01) + 1] 
  if (val %in% stack) {
    break
  }
  stack <- c(stack, val)
  i <- i + 1
}
val


# Day 2 -------------------------------------------------------------------

input02 <- read.table("./data/input-2018-Q02-1.txt", stringsAsFactors = FALSE)
input02 <- input02$V1

# first problem
has_count <- function(array, count) {
  return(count %in% table(array))
}
list_has2 <- lapply(strsplit(input02, split=""), FUN=has_count, count=2)
list_has3 <- lapply(strsplit(input02, split=""), FUN=has_count, count=3)
Reduce("+", list_has2) * Reduce("+", list_has3)

# second problem
input_list <- strsplit(input02, split="")
for (i in 1:(length(input02)-1)) {
  for (j in (i+1):length(input02)) {
    if (sum(input_list[[i]] != input_list[[j]]) == 1) break
  }
  if (sum(input_list[[i]] != input_list[[j]]) == 1) break
}
paste0(input_list[[i]][input_list[[i]] == input_list[[j]]], collapse="")


# Day 3 -------------------------------------------------------------------

# # test case
# input03 <- data.frame(Left = c(1, 3, 5),
#                       Top = c(3, 1, 5),
#                       Width = c(4, 4, 2),
#                       Height = c(4, 4, 2)) %>%
#   mutate(x = Left + Width,
#          y = Top + Height)

input03 <- read_table("./data/input-2018-Q03-1.txt", col_names = FALSE)
input03 <- input03 %>% 
  separate(X1, c("ID", "Info"), " @ ") %>%
  separate(Info, c("Position", "Size"), ": ") %>%
  separate(Position, c("Left", "Top"), ",") %>%
  separate(Size, c("Width", "Height"), "x") %>%
  select(-ID) %>%
  mutate_if(is.character, as.numeric) %>%
  mutate(x = Left + Width,
         y = Top + Height)

# first problem
cloth_matrix <- matrix(0, nrow = max(input03$x), ncol = max(input03$y))
for(i in 1:nrow(input03)) {
  cloth_matrix[(input03[i,]$Left+1):(input03[i,]$Left + input03[i,]$Width), (input03[i,]$Top + 1):(input03[i,]$Top + input03[i,]$Height)] <- 
    cloth_matrix[(input03[i,]$Left+1):(input03[i,]$Left + input03[i,]$Width), (input03[i,]$Top + 1):(input03[i,]$Top + input03[i,]$Height)] + 1
}
sum(cloth_matrix > 1)

# second problem
good_id <- 0
for(i in 1:nrow(input03)) {
  if(sum(cloth_matrix[(input03[i,]$Left+1):(input03[i,]$Left + input03[i,]$Width), (input03[i,]$Top + 1):(input03[i,]$Top + input03[i,]$Height)]) == input03[i,]$Width * input03[i,]$Height) {
    good_id <- i
    break
  }
}
good_id


# Day 4 -------------------------------------------------------------------
# calculated problem elsewhere


# Day 5 -------------------------------------------------------------------

# first problem

input05 <- strsplit("dabAcCaCBAcCcaDA", split="")[[1]]  # test case

input05 <- read.table("./data/input-2018-Q05-1.txt", stringsAsFactors = FALSE)
input05 <- strsplit(input05$V1, split="")[[1]]

# first problem
find_length <- function(input) {
  final <- input
  i <- 1
  while (i <= length(final) - 1 ) {
    if (final[i] %in% LETTERS) {
      if (final[i+1] == letters[which(LETTERS == final[i])]) {
        final <- final[-(i:(i+1))]
        i <- max(1, i - 1)
      } else {
        i <- i + 1
      }
    } else {  # current letter is lower case
      if (final[i+1] == LETTERS[which(letters == final[i])]) {
        final <- final[-(i:(i+1))]
        i <- max(1, i - 1)
      } else {
        i <- i + 1
      }
    }
  }
  return(length(final))  
}
find_length(input05)

# second problem
best <- 99999999
for (i in 1:26) {
  i_length <- find_length(input05[(input05 != LETTERS[i] & input05 != letters[i])])
  best <- min(best, i_length)
}


# Day 6 -------------------------------------------------------------------

# # test case
# input06 <- data.frame(X = c(1, 1, 8, 3, 5, 8),
#                       Y = c(1, 6, 3, 4, 5, 9)) %>%
#   mutate(border = ifelse(X == min(X) | X == max(X) | Y == min(Y) | Y == max(Y), 1, 0))

input06 <- read.table("./data/input-2018-Q06-1.txt", sep= ",", stringsAsFactors = FALSE) %>% 
  mutate_if(is.character, as.numeric) %>%
  rename(X = V1, Y = V2) %>%
  mutate(border = ifelse(X == min(X) | X == max(X) | Y == min(Y) | Y == max(Y), 1, 0))

# first problem
grid <- expand.grid((min(input06$X)-500):(max(input06$X)+500), 
                    (min(input06$Y)-500):(max(input06$Y)+500))
for (i in 1:nrow(input06)) {
  varname <- paste0("C", i)
  grid[[varname]] <- with(grid, abs(Var1 - input06[i,]$X) + abs(Var2 - input06[i,]$Y))
}

find_min <- function(array) {
  mins <- which(array == min(array))
  return(ifelse(length(mins) > 1, 0, mins))
}
grid$Assignment <- apply(grid[3:ncol(grid)], 1, find_min)

border <- which(input06$border == 1)  
pvt <- table(grid %>% filter(! (Assignment %in% border)) %>% select(Assignment))
pvt[pvt <5500] %>% max

# second problem
grid <- expand.grid(min(input06$X):max(input06$X), 
                    min(input06$Y):max(input06$Y))

find_distance <- function(xypos) {
  return(sum(abs(xypos[1] - input06$X) + abs(xypos[2] - input06$Y)))
}
grid$Distance <- apply(grid[1:2], 1, find_distance)
grid %>% filter(Distance < 10000) %>% nrow()


# Day 7 -------------------------------------------------------------------



