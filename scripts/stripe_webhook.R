library(digest)
library(jsonlite)

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || is.na(x)) y else x
}

verify_stripe_signature <- function(payload, signature_header, secret, tolerance = 300) {
  if (!nzchar(signature_header)) {
    stop("Missing Stripe-Signature header")
  }

  if (!nzchar(secret)) {
    stop("Missing STRIPE_WEBHOOK_SECRET")
  }

  parts <- strsplit(signature_header, ",", fixed = TRUE)[[1]]
  kv <- strsplit(parts, "=", fixed = TRUE)
  values <- setNames(vapply(kv, `[`, character(1), 2), vapply(kv, `[`, character(1), 1))

  timestamp <- values[["t"]] %||% ""
  signatures <- unname(values[names(values) == "v1"])

  if (!nzchar(timestamp) || !length(signatures)) {
    stop("Invalid Stripe signature header")
  }

  if (abs(as.numeric(Sys.time()) - as.numeric(as.POSIXct(timestamp, origin = "1970-01-01", tz = "UTC"))) > tolerance) {
    stop("Stripe signature timestamp is outside tolerance")
  }

  signed_payload <- paste0(timestamp, ".", payload)
  expected <- digest::hmac(
    key = secret,
    object = signed_payload,
    algo = "sha256",
    serialize = FALSE
  )

  if (!any(tolower(signatures) == tolower(expected))) {
    stop("Stripe signature verification failed")
  }

  TRUE
}

append_event_log <- function(event) {
  log_path <- Sys.getenv("STRIPE_DONATION_LOG_PATH", unset = "stripe-donations.jsonl")
  dir.create(dirname(log_path), recursive = TRUE, showWarnings = FALSE)

  record <- list(
    logged_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    event_id = event$id %||% NA_character_,
    event_type = event$type %||% NA_character_,
    livemode = event$livemode %||% NA,
    checkout_session_id = event$data$object$id %||% NA_character_,
    payment_status = event$data$object$payment_status %||% NA_character_,
    amount_total = event$data$object$amount_total %||% NA,
    currency = event$data$object$currency %||% NA_character_,
    customer_email = event$data$object$customer_details$email %||% NA_character_,
    client_reference_id = event$data$object$client_reference_id %||% NA_character_,
    metadata = event$data$object$metadata %||% list()
  )

  cat(jsonlite::toJSON(record, auto_unbox = TRUE, null = "null"), "\n", file = log_path, append = TRUE)
}

run_webhook_server <- function() {
  if (!requireNamespace("plumber", quietly = TRUE)) {
    stop("Package 'plumber' is required to run the Stripe webhook server.")
  }

  pr <- plumber::pr()

  pr$handle("GET", "/stripe/health", function(req, res) {
    list(status = "ok", service = "stripe-webhook")
  })

  pr$handle("POST", "/stripe/webhook", function(req, res) {
    payload <- req$postBody %||% ""
    signature <- req$HTTP_STRIPE_SIGNATURE %||% ""
    secret <- Sys.getenv("STRIPE_WEBHOOK_SECRET")

    tryCatch(
      {
        verify_stripe_signature(payload, signature, secret)
        event <- jsonlite::fromJSON(payload, simplifyVector = FALSE)

        if (identical(event$type, "checkout.session.completed")) {
          append_event_log(event)
        }

        res$status <- 200
        list(received = TRUE, event_type = event$type %||% NA_character_)
      },
      error = function(e) {
        res$status <- 400
        list(received = FALSE, error = conditionMessage(e))
      }
    )
  })

  port <- as.integer(Sys.getenv("STRIPE_WEBHOOK_PORT", unset = "8010"))
  pr$run(host = "0.0.0.0", port = port)
}

if (identical(environment(), globalenv())) {
  run_webhook_server()
}
