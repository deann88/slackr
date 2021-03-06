#' Output R expressions to a Slack channel/user
#'
#' Takes an `expr`, evaluates it and sends the output to a Slack
#' chat destination. Useful for logging, messaging on long compute tasks or
#' general information sharing.
#'
#' By default, everything but `expr` will be looked for in a "`SLACK_`"
#' environment variable. You can override or just specify these values directly instead,
#' but it's probably better to call [slackr_setup()] first.
#'
#' @param ... expressions to be sent to Slack
#' @param channel which channel to post the message to (chr)
#' @param username what user should the bot be named as (chr)
#' @param icon_emoji what emoji to use (chr) `""` will mean use the default
#' @param bot_user_oauth_token Slack bot user OAuth token
#' @return the response (invisibly)
#' @note You need a <https://www.slack.com> account and will also need to
#'       setup an API token <https://api.slack.com/>
#'       Also, you can pass in `as_user=TRUE`, the default, as part of the `...`
#'       parameters and the Slack API will post the message as your logged-in
#'       user account (this will override anything set in `username`).
#'       Passing `as_user=FALSE`, results in the Slack API posting the
#'       message as set in `username`
#' @seealso [slackr_setup()], [slackr_bot()], [dev_slackr()],
#'          [save_slackr()], [slackr_upload()]
#' @examples
#' \dontrun{
#' slackr_setup()
#' slackr("iris info", head(iris), str(iris))
#' }
#' @export
slackr <- function(...,
                   channel=Sys.getenv("SLACK_CHANNEL"),
                   username=Sys.getenv("SLACK_USERNAME"),
                   icon_emoji=Sys.getenv("SLACK_ICON_EMOJI"),
                   bot_user_oauth_token=Sys.getenv("SLACK_BOT_USER_OAUTH_TOKEN")) {

  if ((bot_user_oauth_token == "") | is.na(bot_user_oauth_token)) {
    stop("No token specified. Did you forget to call slackr_setup()?", call. = FALSE)
  }

  resp_ret <- ""

  if (!missing(...)) {

    # mimics capture.output

    # get the arglist
    args <- substitute(list(...))[-1L]

    # setup in-memory sink
    rval <- NULL
    fil <- textConnection("rval", "w", local = TRUE)

    sink(fil)
    on.exit({
      sink()
      close(fil)
    })

    # where we'll need to eval expressions
    pf <- parent.frame()

    # how we'll eval expressions
    evalVis <- function(expr) withVisible(eval(expr, pf))

    # for each expression
    for (i in seq_along(args)) {

      expr <- args[[i]]

      # do something, note all the newlines...Slack ``` needs them
      tmp <- switch(mode(expr),
                    # if it's actually an expresison, iterate over it
                    expression = {
                      cat(sprintf("> %s\n", deparse(expr)))
                      lapply(expr, evalVis)
                    },
                    # if it's a call or a name, eval, printing run output as if in console
                    call = ,
                    name = {
                      cat(sprintf("> %s\n", deparse(expr)))
                      list(evalVis(expr))
                    },
                    # if pretty much anything else (i.e. a bare value) just output it
                    integer = ,
                    double = ,
                    complex = ,
                    raw = ,
                    logical = ,
                    numeric = cat(sprintf("%s\n\n", as.character(expr))),
                    character = cat(sprintf("%s\n\n", expr)),
                    stop("mode of argument not handled at present by slackr"))

      for (item in tmp) if (item$visible) { print(item$value); cat("\n") }
    }

    on.exit()

    sink()
    close(fil)

    # combined all of them (rval is a character vector)
    output <- paste0(rval, collapse="\n")

    loc <- Sys.getlocale('LC_CTYPE')
    Sys.setlocale('LC_CTYPE','C')
    on.exit(Sys.setlocale("LC_CTYPE", loc))

    # resp <- POST(url="https://slack.com/api/chat.postMessage",
    #              body=list(token=bot_user_oauth_token,
    #                        channel=slackr_chtrans(channel),
    #                        username=username,
    #                        icon_emoji=icon_emoji,
    #                        as_user=TRUE,
    #                        text=sprintf("```%s```", output),
    #                        link_names=1))

    resp <-
      post_message(
        bot_user_oauth_token = bot_user_oauth_token,
        channel=slackr_chtrans(channel),
        username=username,
        emoji=icon_emoji,
        txt=sprintf("```%s```", output),
        link_names=1
    )
  }

  invisible(resp)

}

#' Sends a message to a slack channel.
#'
#' @param txt text message to send to Slack. If a character vector of length > 1
#'        is passed in, they will be combined and separated by newlines.
#' @param channel which channel to post the message to (chr)
#' @param username what user should the bot be named as (chr)
#' @param icon_emoji what emoji to use (chr) `""` will mean use the default
#' @param bot_user_oauth_token Slack bot user OAuth token
#' @return the response (invisibly)
#' @param ... other arguments passed to the Slack API `chat.postMessage` call
#' @note You need a <https://www.slack.com> account and will also need to
#'       setup an API token <https://api.slack.com/>
#'       Also, you can pass in `add_user=TRUE` as part of the `...`
#'       parameters and the Slack API will post the message as your logged-in
#'       user account (this will override anything set in `username`)
#' @seealso [slackr_setup()], [slackr_bot()], [dev_slackr()],
#'          [save_slackr()], [slackr_upload()]
#' @examples
#' \dontrun{
#' slackr_setup()
#' slackr_msg("Hi")
#' }
#' @export
slackr_msg <- function(txt="",
                       channel=Sys.getenv("SLACK_CHANNEL"),
                       username=Sys.getenv("SLACK_USERNAME"),
                       icon_emoji=Sys.getenv("SLACK_ICON_EMOJI"),
                       bot_user_oauth_token=Sys.getenv("SLACK_BOT_USER_OAUTH_TOKEN"),
                       ...) {

  if (bot_user_oauth_token == "") {
    stop("No token specified. Did you forget to call slackr_setup()?", call. = FALSE)
  }

  output <- paste0(txt, collapse="\n\n")

  loc <- Sys.getlocale('LC_CTYPE')
  Sys.setlocale('LC_CTYPE','C')
  on.exit(Sys.setlocale("LC_CTYPE", loc))

  z <-
    post_message(
      txt        = output,
      emoji = icon_emoji,
      channel    = slackr_chtrans(channel),
      bot_user_oauth_token = bot_user_oauth_token,
      username = username,
      link_names = 1,
      ...
    )

  invisible(z)
}
