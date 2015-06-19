(ns mvxcvi.withings
  "Client for the Withings API.

  See: http://oauth.withings.com/api"
  (:require
    [clj-http.client :as http]
    [clojure.tools.logging :as log]
    [oauth.client :as oauth]))


(def default-oauth-url
  "https://oauth.withings.com/account")


(def default-api-url
  "https://wbsapi.withings.net/v2")


(defn init-oauth
  "Initializes an OAuth 1.0 token for approval by the user. This can then be
  used to perform API requests."
  [access-key secret-key]
  (let [consumer (oauth/make-consumer
                   access-key
                   secret-key
                   "https://oauth.withings.com/account/request_token"
                   "https://oauth.withings.com/account/access_token"
                   "https://oauth.withings.com/account/authorize"
                   :hmac-sha1)
        request-token (oauth/request-token consumer nil)] ; no callback URI
    (println "Visit" (oauth/user-approval-uri consumer (:oauth_token request-token)) "to authorize the app")
    {:consumer consumer
     :access-token (oauth/access-token consumer request-token)}))


(defn oauth-credentials
  [consumer access-token method url params]
  (oauth/credentials
    consumer
    (:oauth_token access-token)
    (:oauth_token_secret access-token)
    method ; e.g. :POST
    url
    params))
