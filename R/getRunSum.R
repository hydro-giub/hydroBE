getRunSum <- function(x,w) {
    .Call('runSum',x,as.integer(w))
}
