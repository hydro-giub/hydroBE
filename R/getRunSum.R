getRunSum <- function(x,w) {
    .Call('runSum',as.double(x),as.integer(w))
}
