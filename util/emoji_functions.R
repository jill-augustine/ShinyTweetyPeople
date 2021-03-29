# https://www.hvitfeldt.me/blog/real-emojis-in-ggplot2/#:~:text=We%20don't%20have%20a,png%20of%20that%20emoji.
emoji_to_link <- function(x) {
    paste0("https://emojipedia.org/emoji/",x) %>%
        read_html() %>%
        html_nodes("tr td a") %>%
        .[1] %>%
        html_attr("href") %>%
        paste0("https://emojipedia.org/", .) %>%
        read_html() %>%
        html_node('div[class="vendor-image"] img') %>%
        html_attr("src")
}

# https://www.hvitfeldt.me/blog/real-emojis-in-ggplot2/#:~:text=We%20don't%20have%20a,png%20of%20that%20emoji.

link_to_img <- function(x, size = 25) {
    paste0("<img src='", x, "' width='", size, "'/>")
}

#' @param string A string containing 2+ non-space sequences separated by a space
#' @param presplit If `TRUE`, assumes string has already been through `str_split(string, ' ')`
#' @return 
#' A character vector of length L where L is the number of pairs found.
#' Each string is two non-space sequences separated by a space. 
return_pairs <- function(string, presplit = FALSE) {
    if (!presplit) {
        splitted <- str_split(string, ' ')[[1]]
    } else {
        splitted <- string
    }
    
    if (length(splitted) < 2) {
        rlang::abort('String must contain 2+ non-space sequences separated by a space.')
    } else if (length(splitted) == 2) {
        return(paste(splitted, collapse = ' '))
    } else {
        from_ <- 1:(length(splitted) - 1)
        to_ <- from_ + 1
        res <- purrr::map2(from_, to_,
                    function(.x, .y) paste(splitted[.x:.y], collapse = ' ')
        )
    }
    unlist(res)
}