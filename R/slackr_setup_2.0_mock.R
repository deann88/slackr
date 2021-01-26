slackr_setup_mock <- function(channel="#general",
                         username="slackr",
                         icon_emoji="",
                         incoming_webhook_url="",
                         bot_user_oauth_token="",
                         config_file="~/.slackr",
                         echo=FALSE,
                         save_workspace = TRUE,
                         cacheChannels = FALSE) {

  if (!missing(cacheChannels)) {
    warning('cacheChannels parameter is deprecated as of slackr 2.1.0. channels are now auto-cached with memoization')
  }

  if (file.exists(config_file)) {
    config <- read.dcf(
      config_file,
      fields = c("channel", "icon_emoji",
                 "username", "incoming_webhook_url", "bot_user_oauth_token")
    )

    channel <- config[,"channel"]
    username <- config[,"username"]
    icon_emoji <- config[,"icon_emoji"]
    incoming_url_prefix <- config[,"incoming_webhook_url"]
    bot_user_oauth_token <- config[,"bot_user_oauth_token"]
  }


  if (!grepl("?$", Sys.getenv("SLACK_INCOMING_URL_PREFIX"))) {
    incoming_url_prefix <- sprintf("%s?", incoming_url_prefix)
  }

  if (length(Sys.getenv("SLACK_CHANNEL"))==0) {
    channel <- '#general'
  }

  if (length(Sys.getenv("SLACK_USERNAME"))==0) {
    username <- 'slackr'
  }

  if (save_workspace) {
    workspace <- list(
      channel = channel,
      username = username,
      bot_user_oauth_token = bot_user_oauth_token,
      incoming_webhook_url = incoming_webhook_url,
      icon_emoji = icon_emoji
    )
  } else {

    if (bot_user_oauth_token == '') {
      stop("No config file found. Please specify your Slack bot OAuth token\n   with the bot_user_oauth_token argument in slackr_setup().")
    }

    Sys.setenv(SLACK_CHANNEL=channel)
    Sys.setenv(SLACK_USERNAME=username)
    Sys.setenv(SLACK_ICON_EMOJI=icon_emoji)
    Sys.setenv(SLACK_INCOMING_URL_PREFIX=incoming_webhook_url)
    Sys.setenv(SLACK_BOT_USER_OAUTH_TOKEN=bot_user_oauth_token)

  }

  if (echo) {
    print(toJSON(as.list(
      Sys.getenv(c("SLACK_CHANNEL", "SLACK_USERNAME",
                   "SLACK_ICON_EMOJI",
                   "SLACK_INCOMING_URL_PREFIX", "SLACK_BOT_USER_OAUTH_TOKEN")
      )),
      pretty=TRUE))
  }

  msg <- 'Successfully connected to Slack'

  if (save_workspace) {
    return(workspace)
  }

  return(invisible(NULL))
}


slackr_msg_mock <- function(txt="",
                            workspace = slack_workspace,
                            channel = '#general',
                            username = 'slackr',
                            icon_emoji = '',
                            bot_user_oauth_token = '',
                            ...) {

  if (is.null(workspace) | !exists('workspace')) {
    channel=Sys.getenv("SLACK_CHANNEL")
    username=Sys.getenv("SLACK_USERNAME")
    icon_emoji=Sys.getenv("SLACK_ICON_EMOJI")
    bot_user_oauth_token=Sys.getenv("SLACK_BOT_USER_OAUTH_TOKEN")
  } else {
    channel <- workspace$channel
    username <- workspace$username
    icon_emoji <- workspace$icon_emoji
    bot_user_oauth_token <- workspace$bot_user_oauth_token
  }

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

