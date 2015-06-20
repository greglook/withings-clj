(ns mvxcvi.withings
  "Client for the Withings API.

  See: http://oauth.withings.com/api"
  (:require
    [clj-http.client :as http]
    [clojure.tools.logging :as log]
    [oauth.client :as oauth]))



;; ## API Protocol

(defprotocol Client
  "Protocol for reading data from the Withings API."

  (user-info
    [client]
    "Retrieve information about the authenticated user.")

  #_ ...)



;; ## OAuth Credentials

(def default-oauth-url
  "https://oauth.withings.com/account")


(defn oauth-consumer
  "Constructs a new OAuth 1.0 consumer using the given access and secret key."
  ([consumer-key consumer-secret]
   (oauth-consumer consumer-key consumer-secret default-oauth-url))
  ([consumer-key consumer-secret oauth-url]
   (oauth/make-consumer
     consumer-key
     consumer-secret
     (str oauth-url "/request_token")
     (str oauth-url "/access_token")
     (str oauth-url "/authorize")
     :hmac-sha1)))


(defn request-access!
  "POSTs to the request-token endpoint to retrieve a temporary token credential
  for the end-user to authorize. Returns a map containing the temporary token
  and a URL for the user to visit to authorize the consumer's access."
  ([consumer]
   (request-access! consumer nil))
  ([consumer callback-url]
   (let [temp-token (oauth/request-token consumer callback-url)
         authz-url (oauth/user-approval-uri consumer (:oauth_token temp-token))]
     {:temp-token temp-token
      :authz-url authz-url})))


(defn authorize-credentials!
  "POSTs to the access-token endpoint to trade the authorized temporary token
  credential for a long-term access token credential. This should be called
  after `request-access!` and the user authorization. Returns a map of
  credentials that can be serialized and used to authenticate API calls."
  [consumer temp-token]
  (assoc
    (oauth/access-token consumer temp-token)
    :consumer consumer))



;; ## HTTP Client

(def default-api-url
  "https://wbsapi.withings.net/v2")


(defn- api-request
  "Makes an authenticated request to the API."
  [client resource action params]
  (let [url (str (:api-url client) "/" resource)
        params (assoc params
                      :action action
                      :userid (get-in client [:credentials :user_id]))

        oauth-params (oauth/credentials
                       (get-in client [:credentials :consumer])
                       (get-in client [:credentials :oauth_token])
                       (get-in client [:credentials :oauth_token_secret])
                       :GET url params)]
    (http/get url
      {:headers {"Authorization" (oauth/authorization-header oauth-params)}
       :query-params params
       :as :json})))


(defrecord HTTPClient
  [api-url credentials]

  Client

  (user-info
    [this]
    (api-request this "user" "getbyuserid" nil))

  #_ ...)


(defn http-client
  "Constructs a new HTTP API client."
  ([credentials]
   (HTTPClient. default-api-url credentials))
  ([api-url credentials]
   (HTTPClient. api-url credentials)))
