# read relevant files
pokemon <- read.csv('Pokemon.csv', stringsAsFactors = T,head = T, row.names = 1)

# gather input from user on pokemon and what berries they are using
v.p <- readline(prompt="Enter Pokemon number: ")
v.berry <- readline(prompt="Berry (None = 1, Razz = 2, Silver = 3, Golden = 4): ")

# do preliminary variable assignment
v.p <- as.integer(v.p)
v.berry <- as.integer(v.berry)
berry = mod[v.berry]
BCR <- pokemon[v.p,1]
flee_chance <- pokemon[v.p,2]

# DEFINE FUNCTIONS
# finds the probability of catching the pokemon on a given throw
probCatch <- function(BCR,ball,berry,throw) {
  p <- 1-(1-(BCR/1.52312768))^(1.7*ball*berry*throw)
  return(p)
}

# calculates the number of throws before 80% chance that a catch is made
throwsCatch <- function(pcatch) {
  return(qgeom(.8, pcatch))
}

# calculates the number of throws before 50% chance of fleeing
throwsFlee <- function(p) {
  return(qgeom(.5, p))
}

# CREATES VECTORS FOR PROBABILITY OF CATCH:
Pball <- c(probCatch(BCR,1,berry,1)[1,1],
           probCatch(BCR,1,berry,1.15)[1,1],
           probCatch(BCR,1,berry,1.5)[1,1],
           probCatch(BCR,1,berry,1.85)[1,1]) #[normal, nice, great, excellent]

Gball <- c(probCatch(BCR,1.5,berry,1)[1,1],
           probCatch(BCR,1.5,berry,1.15)[1,1],
           probCatch(BCR,1.5,berry,1.5)[1,1],
           probCatch(BCR,1.5,berry,1.85)[1,1]) #[normal, nice, great, excellent]

Uball <- c(probCatch(BCR,2,berry,1)[1,1],
           probCatch(BCR,2,berry,1.15)[1,1],
           probCatch(BCR,2,berry,1.5)[1,1],
           probCatch(BCR,2,berry,1.85)[1,1]) #[normal, nice, great, excellent]

# creates table for catch probability
catch_cbind <- cbind(Pball,Gball,Uball)
catchTab <- catch_cbind
colnames(catchTab) <- c('POKEBALL', 'GREATBALL', 'ULTRABALL')
rownames(catchTab) <- c('NORMAL', 'NICE', 'GREAT', 'EXCELLENT')
catchTab <- as.table(catchTab)

# CREATES VECTORS FOR NUMBER OF THROWS BEFORE CATCH
throwsP <- c(throwsCatch(probCatch(BCR,1,berry,1)[1,1]),
             throwsCatch(probCatch(BCR,1,berry,1.15)[1,1]),
             throwsCatch(probCatch(BCR,1,berry,1.5)[1,1]),
             throwsCatch(probCatch(BCR,1,berry,1.85)[1,1])) #[normal, nice, great, excellent]

throwsG <- c(throwsCatch(probCatch(BCR,1.5,berry,1)[1,1]),
             throwsCatch(probCatch(BCR,1.5,berry,1.15)[1,1]),
             throwsCatch(probCatch(BCR,1.5,berry,1.5)[1,1]),
             throwsCatch(probCatch(BCR,1.5,berry,1.85)[1,1])) #[normal, nice, great, excellent]

throwsU <- c(throwsCatch(probCatch(BCR,2,berry,1)[1,1]),
             throwsCatch(probCatch(BCR,2,berry,1.15)[1,1]),
             throwsCatch(probCatch(BCR,2,berry,1.5)[1,1]),
             throwsCatch(probCatch(BCR,2,berry,1.85)[1,1])) #[normal, nice, great, excellent]

# creates table for number of throws before catch
throwsmtx <- cbind(throwsP, throwsG, throwsU)
throwsTab <- throwsmtx
colnames(throwsTab) <- c('Pokeball', 'Greatball', 'Ultraball')
rownames(throwsTab) <- c('Normal', 'Nice', 'Great', 'Excellent')
throwsTab <- as.table(throwsTab)

# determines the number of throws before fleeing
flee_num <- throwsFlee(flee_chance)

print(rownames(pokemon)[v.p])
print("CATCH PROBABILITY ON A GIVEN THROW")
print(catchTab)
print('————————————————————————————————————————————————————————————————————')
print("NUMBER OF THROWS BEFORE 80% CHANCE OF CATCHING")
print(throwsTab)
print('————————————————————————————————————————————————————————————————————')
print("NUMBER OF THROWS BEFORE 50% CHANCE OF FLEEING")
print(flee_num)