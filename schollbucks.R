library(checkmate)
library(data.table)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(stringr)
library(telegram.bot)


readNames <- function() {
  count <- NA
  name <- NA

  repeat{
    #check condition
    if (!is.na(count)) {
      break
    }

    # get input 
    input <- readline(prompt=writeLines(c("<Name>-<Anzahl>", "Type \":\" to quit insert-mode!")))
    parsed <- stringr::str_match(input, "(.+)-(\\d+)")
    name <- parsed[[2]]
    count <- as.numeric(parsed[[3]])
  
    # ask wether user wants to abort
    if (is.na(count)) {
      if (readline(prompt = writeLines(c("Input mode stopped! Abort? [Y/N]"))) %in% c("Y", "y")) {
        break
      } else {
        next
      }
    }
  }
  
  return(
    list(name = name,
         count = count)
  )
}


construct_table <- function() {
  data <- data.table()
  repeat {
    new_row <- readNames()
    if (is.na(new_row$name)) {
      break
    } else {
      data <- rbind(data, new_row)
    }
  }
  return(data)
}


calculate_prices <- function(dt, cost = 0.2) {
  assertDataTable(dt)
  assertTRUE(dim(dt)[[1]] > 0)
  return(
    dt %>% mutate(costs = cost * count)
  )
}


write_to_file <- function(dt, path) {
  assertDataTable(dt)
  ifelse(!dir.exists(path), dir.create(path), FALSE)
  fwrite(dt, paste(file.path(path, "Schollbucks"), Sys.Date(), sep = "_"))
}


write_to_PDF <- function(dt, path) {
  assertDataTable(dt)
  
  ifelse(!dir.exists(path), dir.create(path), FALSE)
  
  file <- file.path(path, paste0("Rechnung_", Sys.Date(), ".pdf"))
  pdf(file)
  plot(0:10, type = "n", xaxt="n", yaxt="n", bty="n", xlab = "", ylab = "")
  text(3, 10, paste("Rechnung", Sys.Date()))
  grid.table(dt)
  dev.off()
  return(file)
}


plot_stat <- function(dt, path) {
  assertDataTable(dt)

  file <- file.path(path, paste0("stat_", Sys.Date(), ".png"))
  coffee <- dt
  coffee <- coffee[order(coffee$count, decreasing = T), ]
  coffee$fac <- factor(coffee$name, levels = coffee$name[order(coffee$count, decreasing = T)])
  ggplot(coffee, aes(x = fac, y = count)) +
    geom_bar(stat = "identity", fill = "#8b6245") +
    ggtitle(paste("Schollbucks Coffee", Sys.Date())) +
    ylab("Anzahl Kaffees") +
    xlab("Krasse Leute") +
    theme_dark() +
    theme(plot.title = element_text(size = 20, face = "bold")) +
    ggsave(file)
  return(file)
}


run_bot <- function(dt, path) {
  assertDataTable(dt)

  renv <- fread(".renv")
  TOKEN <- renv[Variables == "TOKEN",]$Assigned
  DEBUG <- as.logical(renv[Variables == "DEBUG",]$Assigned)

  bot <- Bot(token = TOKEN)
  updates <- bot$getUpdates()
  print(updates)
  if(length(updates) == 0){
    stop("ERROR: No groups can be found")
  }
  chat_id <- updates[[1L]]$from_chat_id()

  bot$sendMessage(chat_id,
                  text = paste("Kaffeekosten, Stand", Sys.Date()),
                  parse_mode = "Markdown"
  )
  bot$sendDocument(chat_id,
                   document = write_to_PDF(dt, path)
  )
  bot$sendPhoto(chat_id,
                photo = plot_stat(dt, path))
  print("sent")
}


run_Script <- function(
  costs = 0.2,
  bot_send = F,
  path = "~/.Schollbucks") {
  repeat{
    table <- construct_table()
    table <- calculate_prices(dt = table, cost = 0.2)
    print(table)
    if (readline(prompt="Is that correct? [Y/N]") %in% c("y", "Y")) {
      break
    }
  }
  write_to_file(table, path)
  if (bot_send) {
    run_bot(table, path)
  }
}