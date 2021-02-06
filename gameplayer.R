## game player


# packages needed ---------------------------------------------------------

library(pacman)
p_load(dplyr, magrittr, stringr, gtools, stringi)


# winning edges -----------------------------------------------------------

w1<-c("0,1", "1,1", "2,1")
w2<-c("0,0", "1,1", "2,2")
w3<-c("0,2", "1,1", "2,0")
w4<-c("1,0", "1,1", "1,2")


# status-occupied/free function -------------------------------------------

free_slots<-function(position)
{
    stopifnot(length(position)==6)
    
    total_slots<-c("0,0", "1,0", "2,0", "0,1",
                   "1,1", "2,1", "0,2", "1,2", "2,2")
    
    x<-integer()
    for (i in 1:length(total_slots))
    {
        if (sum(str_count(position, total_slots[i])) != 1)
        {
            x[i] <- i
        }
    }
    
    x <- x %>% na.omit() %>% as.integer()
    #print(x)
    
    a_slots<-total_slots[x]
    #message("The free slots are: ")
    return(a_slots)
    
}

## testing the free slots function
#free_slots(position=c("0,0","1,0","2,0","0,2","1,2","2,2"))
#b<-c("0,0","1,0","2,0","2,1","0,2","1,2")

# distance function between places
#dist(matrix(c(2,0,0,2), nrow=2, byrow=T), method="euclidean") %>% as.numeric() %>% round(2)
#m<-str_extract_all(a, "[012]") %>% unlist() %>% as.integer() # vector


# which position(s) is/are viable to a point -------------------------------------


viable_to<-function(point, blocks)
{
    # acceptable distances
    ac_dist<-c(1, 1.41)

    # obtaining free slots
    free<-free_slots(blocks)
    #print(paste("free slots are: ", free))
    
    # matrix for dist calculation
    mat<-list()
    
    for (i in seq_along(free))
    {
        mat[[i]]<-c(point, free[i])
    }
    
    m<-list() # to be matrix list
    d<-vector() # distance holder
    
    for (i in seq_along(mat))
    {
        m[[i]]<-str_extract_all(mat[[i]], "[012]") %>%
            unlist() %>%
            as.integer() # vector

        d[i]<-dist(matrix(m[[i]], nrow=2, byrow=T), method="euclidean") %>%
            as.numeric() %>%
            round(2)
    }
    viable_points <- vector()
    
    # this two loops can be vectorizable using is.element()
    for (i in seq_along(d))
    {
        for (j in seq_along(ac_dist))
        {
            if (d[i]==ac_dist[j])
            {
                # print(paste(point, "You can move to: ", free[i]))
                viable_points[i] <- free[i]
            }
        }
    }
    
    # print(paste("viable points are: ", viable_points))
    
    viable_points <- viable_points %>% na.omit() %>% as.character()
    
    return(viable_points)
    #print(mat)
    #print(m)
    #print(d)
}

# testing viable to function
viable_to(point="2,2", blocks=c("0,0","1,0","2,0","0,2","1,2","2,2"))


# creating a class for game session --------------------------------------------

setClass("game-session", representation(
    players="list",
    opponents="list",
    winning_edge="list"
))

setValidity("game-session",
            function(object)
            {
                length(object@players)==3 # three players
                length(object@opponents)==3 # three opponents
                length(object@winning_edge)==4 # four winning edges
                #length(unique(object@players==object@opponents))==1
            })

s1<-new("game-session",
        players=list(p1="0,0",p2="1,0",p3="2,0"),
        opponents=list(o1="0,2",o2="1,2",o3="2,2"),
        winning_edge=list(w1,w2,w3,w4))

# testing object
validObject(s1)


# plot method for game session --------------------------------------------


plotgame <- function(`game-session`)
{
    ## plot area setting
    plot(x=0,y=0,xlim=c(0,2), ylim=c(0,2), axes=F,
         main=paste("GAME SESSION"),
         sub="black-players || red-opponents", xlab="", 
         ylab="", col="white")
    #axis(1, at=seq(0,2,1));axis(2, at=seq(0,2,1))
    #axis(3, at=seq(0,2,1));axis(4, at=seq(0,2,1))
    box(lwd=2)
    abline(h=1);abline(v=1)
    
    lines(c(0,2),c(0,2), type="l", col="green", lwd=2)
    lines(c(0,2),c(2,0), type="l", col="green", lwd=2)
    lines(c(0,1),c(1,0), type="l", col="green", lwd=2)
    lines(c(1,2),c(0,1), type="l", col="green", lwd=2)
    lines(c(0,1),c(1,2), type="l", col="green", lwd=2)
    lines(c(1,2),c(2,1), type="l", col="green", lwd=2)
    
    
    ## players
    p<-`game-session`@players %>% unlist() %>% str_extract_all("[012]")
    o<-`game-session`@opponents %>% unlist() %>% str_extract_all("[012]")
    
    p_vec<-list()
    o_vec<-list()
    for (i in seq_along(p))
    {
        p_vec[[i]]<-p[[i]] %>% as.integer()
        o_vec[[i]]<-o[[i]] %>% as.integer()
    }
    
    for (i in seq_along(p_vec))
    {
        points(x=p_vec[[i]][1], y=p_vec[[i]][2], pch=15, cex=4, col="blue");
        #text(x=p_vec[[i]][1], y=p_vec[[i]][2]
        #     , labels=paste(p_vec[[i]][1],",",p_vec[[i]][2]), col="white",pos=4, cex=.8, adj=.4)
        
        points(x=o_vec[[i]][1], y=o_vec[[i]][2], pch=20, cex=4, col="red");
        #text(x=o_vec[[i]][1], y=o_vec[[i]][2], 
        #    labels=paste(o_vec[[i]][1],",",o_vec[[i]][2]), col="red",pos=4, cex=1, adj=.4)
    }
}

## testing plot
plotgame(s1)


# moving players and opponents in a game ----------------------------------

game_move<-function(current_point,next_point, game_session)
{
    ## all players
    all<-c(game_session@players, game_session@opponents)
    ## free slots
    free<-free_slots(all)

    for (i in seq_along(all))
    {
        if (next_point==all[[i]])
        {
            stop("THE SLOT CONTAINS A PLAYER!")
        }
    }
    
    ## checking viable slots of current position
    v_p<-viable_to(point=current_point, blocks=all)

    new_p<-list()
    new_o<-list()
    loc_vec <- vector()
    
    #for (i in seq_along(v_p))
    #{
    #    if (length(table(next_point == v_p))>1)
    #    {
    #        stop("YOU CANNOT MOVE TO THAT SLOT")
    #    }
    #}
    
    for (i in seq_along(v_p))
    {
        if (next_point == v_p[i])
        {
            for (i in seq_along(all))
            {
                if (current_point==all[[i]])
                {
                    loc_vec[i] <- i
                }
                else
                {
                    loc_vec[i] <- NA
                }
            }
        }
        #else
        #{
        #    stop("YOU CANNOT MOVE TO THAT SLOT!")
        #}
    }

    vec <- loc_vec %>% na.omit %>% as.integer()
    
    ## making changes to game session
    all[[vec]] <- next_point
    
    new_p <- list(p1=all[[1]], p2=all[[2]], p3=all[[3]])
    new_o <- list(o1=all[[4]], o2=all[[5]], o3=all[[6]])
    
    g_sess<-new("game-session",
            players=new_p,
            opponents=new_o,
            winning_edge=list(w1,w2,w3,w4))

    return(g_sess)

}

# testing gamemove function
s4<-game_move(current_point = "2,2", next_point = "2,1", game_session = s1)
plotgame(s4)



# suggestion engine function, attach/defend -------------------------------

suggestion_engine<-function(game_session)
{
    # cheking if a winner already exists
    exist.winner <- winner.exist(game_session)
    if (is.null(exist.winner) == F)
    {
        return(c(game_session, NA, NA))
    }
    
    # here the winner doesnt exist
    else
    {
        # analyzing proximity to win/defend
        analyze_edges <- function(game_session)
        {
            # winning edges
            w <- game_session@winning_edge
            
            # players
            p <- game_session@players %>%
                unlist() %>%
                unname()
            
            # opponents
            o <- game_session@opponents %>%
                unlist() %>%
                unname()
            
            # final playing parameters
            current <- NULL
            desired <- NULL
            
            # winning permutations list
            wp <- list()
            l <- length(w[[1]])
            
            for (i in seq_along(w))
            {
                wp[[i]] <- permutations(length(w[[i]]), length(w[[i]]), w[[i]])
            }
            #print(paste("length of wp is: ",length(wp)))
            
            # win pattern vector for opponent
            wpv <- vector()
            
            for (i in seq_along(wp))
            {
                for (j in 1:nrow(wp[[i]]))
                {
                    if (sum(str_count(wp[[i]][j,], o))==2)
                    {
                        wpv[i] <- i
                        #print(i)
                    }
                }
            }
            
            # win pattern vector for player/computer
            wpv_p <- vector()
            
            for (i in seq_along(wp))
            {
                for (j in 1:nrow(wp[[i]]))
                {
                    if (sum(str_count(wp[[i]][j,], p))==2)
                    {
                        wpv_p[i] <- i
                        #print(i)
                    }
                }
            }
            
            message("Securable winning edges are: ")
            wpv_p <- wpv_p %>% na.omit() %>% as.integer()
            
            # print(wpv_p)
            print(case_when(
                wpv_p == 1 ~ "Horizontal winning edge",
                wpv_p == 2 ~ "left-base diagonal winning edge",
                wpv_p == 3 ~ "right-based diagonal winning edge",
                wpv_p == 4 ~ "Vertical winning edge",
            ))
            
            message("Winning edges to be captured by opponent are: ")
            wpv <- wpv %>% na.omit() %>% as.integer()
            
            ## naming the winning edges
            # print(wpv)
            print(case_when(
                wpv == 1 ~ "Horizontal winning edge",
                wpv == 2 ~ "left-base diagonal winning edge",
                wpv == 3 ~ "right-based diagonal winning edge",
                wpv == 4 ~ "Vertical winning edge",
            ))
            
            ## cheking for free slots on a captured winning edge
            free <- free_slots(c(o,p))
            
            # message list holds messages on directions when there is no securable viable point
            messagelist <- list()
            
            
            # checking lengths of winning edges to be captured.
            #print(paste("length of wpv_o:",length(wpv)," length of wpv_p: ",length(wpv_p)))
            
            ## i am making an assumption that wpv/wpv_p can only have a max of 2
            ## at no time can a side take three winning edges at a go
            
            # attack engine
            if (length(wpv_p) >= 1)
            {
                for (i in 1:length(wpv_p))
                {
                    for (k in 1:3)
                    {
                        if (sum(str_count(free, w[[wpv_p[i]]][k])) == 1)
                        {
                            print(paste("Free slot for a win move on edge: ", wpv_p[i], "is: ", w[[wpv_p[i]]][k]))
                        }
                    }
                    # checking if the remaining player can actually fill in that freeslot for win
                    for (q in seq_along(p))
                    {
                        # locating a player not on the viable winning edge
                        #print(w[[wpv_p[i]]]) # initially was : print(w[[wpv[i]]])
                        if (sum(str_count(w[[wpv_p[i]]], p[q])) == 0)
                        {
                            #print(paste("The missing link is: ", p[q]))
                            
                            # the viable point to the missing player
                            vp <- viable_to(point=p[q], blocks=c(p,o))
                            
                            ## trying to find if the viable point of missing player slot
                            ## is on the winning edge
                            for (m in seq_along(vp))
                            {
                                if (sum(str_count(w[[wpv_p[i]]], vp[m])) != 0)
                                {
                                    print(paste("The missing link is: ", p[q]))
                                    print(paste("The player can secure winning edge: ", wpv_p[i] ,", please cover point: ", vp[m]))
                                    message(paste("Consider moving: ",p[q]," to point: ", vp[m]))
                                    
                                    # current
                                    current <- p[q]
                                    #next
                                    desired <- vp[m]
                                    
                                }
                            }
                        }
                        #
                    }
                }
            }
            
            # defend engine
            else
            {
                
                ## this whole loop just figures out which winning edges are worth
                ## securing from opponents and which are not
                # remove the #* comments to test code
                for (j in seq_along(wpv))
                {
                    for (k in 1:3)
                    {
                        if (sum(str_count(free, w[[wpv[j]]][k])) == 1)
                        {
                            print(paste("Free slot to be taken by enemy on edge", wpv[j], "is: ", w[[wpv[j]]][k]))
                            
                            for (i in seq_along(o))
                            {
                                # locating opponent not on the viable winning edge
                                if (sum(str_count(w[[wpv[j]]], o[i])) == 0)
                                {
                                    print(paste("The missing link is: ", o[i]))
                                    
                                    # the viable point to the missing opponent
                                    vp <- viable_to(point=o[i], blocks=c(p,o))
                                    
                                    ## trying to find if the viable point of missing opponent slot
                                    ## is on the winning edge
                                    for (q in seq_along(vp))
                                    {
                                        if (sum(str_count(w[[wpv[j]]], vp[q])) != 0)
                                        {
                                            print(paste("The opponent can secure winning edge: ", wpv[j] ,", please cover point: ", vp[q]))
                                            
                                            ## trying to block critical point on a winning edge
                                            for (n in seq_along(p))
                                            {
                                                vpp <- viable_to(point=p[n], blocks=c(p,o))
                                                if (sum(str_count(vpp, vp[q])) > 0 && length(vpp) != 0)
                                                {   # when length of vpp is 0, it means blocked from any slots
                                                    print(paste("CONSIDER MOVING POINT", p[n], "to: ", vp[q]))
                                                    
                                                    # current
                                                    current <- p[n]
                                                    #next
                                                    desired <- vp[q]
                                                }
                                            }
                                        }
                                        else if (sum(str_count(w[[wpv[j]]], vp[q])) == 0)
                                        {   # when the opponent is far from missing slot on a critical winning edge
                                            print(paste("The opponent missing", o[i], "cannot secure edge", wpv[j]))
                                        }
                                    }
                                    
                                }
                            }
                        }
                        
                    }
                }
            }
            
            
            pp <- p
            
            for (i in 1:length(pp))
            {
                if (pp[i] != "1,1" && 
                    length(viable_to(point=pp[i], blocks=c(p,o))) != 0)
                {
                    pp <- ifelse(pp=="1,1", NA, pp) %>% na.omit() %>% as.character()
                    break
                }
                else
                {
                    pp <- p
                }
            }
            
            #pp <- ifelse(pp=="1,1", NA, pp) %>% na.omit() %>% as.character()
            
            
            
            vp_vec <- vector()
            for (s in 1:length(pp))
            {
                l <- length(viable_to(point = pp[s], blocks = c(p,o)))
                if (l != 0)
                {
                    vp_vec[s] <- s
                }
                
            }
            vp_vec <- vp_vec %>% na.omit()
            print(paste("Movable point are: ", pp[vp_vec]))
            
            chosen <- sample(length(vp_vec), 1)
            viable_point <- viable_to(point=pp[vp_vec[chosen]], blocks=c(p,o))
            message(paste("optional::consider moving point", 
                          pp[vp_vec[chosen]], "to: ", 
                          sample(viable_point, 1)))
            
            # cheking if current and desired are already packed
            if (length(current) == 0 & length(desired) == 0)
            {
                # current
                current <- pp[vp_vec[chosen]]
                #next
                desired <- sample(viable_point, 1) 
            }
            else
            {}
            
            print("Current and desired are below")
            print(current)
            print(desired)
            
            mover <- game_move(current_point = current, 
                               next_point = desired, 
                               game_session = game_session)
            
            return(c(mover, current, desired))
        }
        
        g_s <- analyze_edges(game_session = game_session)
        #plotgame(g_s)
        return(g_s)
    }
    
}


startgame <- function(game_session)
{
    ## intializing the game
    message("Lets toss a coin, choose 1 for head, 2 for tail: ")
    s <- readline(prompt="What is your take: ")
    
    space <- c("HEAD", "TAIL")
    rand <- sample(2,1,prob = c(.5,.5))
    #print(paste("The outcome is: ", space[rand]))
    
    if (s==rand) {print("You win the toss, start!")}

    else {print("You lose, My turn !")}
    
    ## setting parameters
    s1<-new("game-session",
            players=list(p1="0,0",p2="1,0",p3="2,0"),
            opponents=list(o1="0,2",o2="1,2",o3="2,2"),
            winning_edge=list(w1,w2,w3,w4))
    
    game_session <- s1
    
    w <- game_session@winning_edge
    
    p <- game_session@players %>%
        unlist() %>%
        unname()
    
    o <- game_session@opponents %>%
        unlist() %>%
        unname()
    
    if (s!=rand) # i won the toss
    {
        # any random player to move to centre
        s <- p[sample(3,1)]

        game_session<-game_move(current_point = s, next_point = "1,1", game_session = game_session)
        plotgame(game_session)
        
    }
    else{}
    
    return(game_session)
}



# Miscellaneous test game -------------------------------------------------

s1<-new("game-session",
        players=list(p1="0,0",p2="2,0",p3="1,0"),
        opponents=list(o1="0,2",o2="2,2",o3="1,2"),
        winning_edge=list(w1,w2,w3,w4))

plotgame(s1)

s1 <- startgame(s1)

s1 <- playlocate(s1)
# s1<-game_move(current_point = "2,2", next_point = "2,1", game_session = s1)
analyze_edges(s1)

s1 <- suggestion_engine(s1)[[1]]

s1 <- startgame(s1)


# an automatic playlocate function ----------------------------------------

playlocate <- function(game_session)
{
    # current location
    current <- locator(1)  %>% as.numeric() %>% round() %>% 
        as.character() %>% list() %>% stri_join_list(sep=",")
    #print(current)
    # desired location
    desired_psn <- locator(1)  %>% as.numeric() %>% round() %>% 
        as.character() %>% list() %>% stri_join_list(sep=",")
    #print(desired_psn)
    game_session <- game_move(current_point = current, next_point = desired_psn, 
                              game_session = game_session)
    
    plotgame(game_session)
    return(game_session)

}


# check if winner exists at a given moment/session ----------------------

winner.exist <- function(game_session)
{
    w <- game_session@winning_edge
    p_o <- list(game_session@players, game_session@opponents)
    
    for (i in 1:length(p_o))
    {
        for (j in 1:length(w))
        {
            status <- is.element(el=p_o[[i]], set=w[[j]])
            if (all(status) == T)
            {
                #print(paste("winner is: ", i))
                #print(paste("winning edge is: ",j))
                return(c(i, T))
            }
        }
    }
}


# two players for analysis ------------------------------------------------


# swapping players and opponents ------------------------------------------

swap_po <- function(game_session)
{
    p <- game_session@players
    o <- game_session@opponents
    
    # swap
    game_session@players <- o
    game_session@opponents <- p
    
    return(game_session)
}


# the two players playing -------------------------------------------------


# databank for game sessions ----------------------------------------------

df <- array(dim=c(50,2,55))


win_match <- vector()
game_list <- list()
for (g in 1:50)
{
    # game initialization
    s1<-new("game-session",
            players=list(p1="0,0",p2="2,0",p3="1,0"),
            opponents=list(o1="0,2",o2="2,2",o3="1,2"),
            winning_edge=list(w1,w2,w3,w4))
    #plotgame(s1)
    llp <- vector()
    llo <- vector()
    
    # game start
    # starts with players
    while(is.null(winner.exist(s1)))
    {
        # starter-player
        sgp <- suggestion_engine(s1)
        s1 <- sgp[[1]]
        #plotgame(s1)
        llp <- c(llp, sgp[[2]], sgp[[3]])
        
        # loser-opponent
        temp <- s1 %>% swap_po()
        #print(temp)
        sgo <- suggestion_engine(temp)
        temp <- sgo[[1]]
        llo <- c(llo, sgo[[2]], sgo[[3]])
        
        #print(temp)
        temp <- swap_po(temp)
        #print(temp)
        s1 <- temp
        #plotgame(s1)
        
    }
    df[1:length(llp),1,g] <- llp
    df[1:length(llo),2,g] <- llo
    
    
    rm(llo)
    rm(llp)
    
    win_match[g] <- winner.exist(s1)[1]
    message("WINNN *****************************")
    rm(temp)
    rm(s1)
}

#re arranging the experience to have winner at column 1 and loser at 2
for (i in 1:length(win_match))
{
    if (win_match[i] == 2)
    {
        p <- df[,,i][,1]
        o <- df[,,i][,2]
        
        df[,,i][,1] <- o
        df[,,i][,2] <- p
    }
}

exp_play <- function(game_session)
{
    # player as starter
    # a random experience is chosen
    s <- sample(length(win_match), 1)
    
    pp <- df[,,s][,1] %>% 
        na.omit() %>% 
        as.character()
    
    oo <- df[,,s][,2] %>% 
        na.omit() %>% 
        as.character()
    
    game_session<-game_move(current_point = pp[], 
                            next_point = "1,1", 
                            game_session = game_session)
    
    
}
