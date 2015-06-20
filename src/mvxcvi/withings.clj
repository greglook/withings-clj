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

  (get-activity
    [client date]
    [client from-date to-date]
    "Get activity information on a specific day or between a range of days.")

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


(def status-codes
  "Map of Withings 'status' response codes to descriptive keywords."
  {0    :success                 ; Operation was successful
   247  :bad-userid              ; The userid provided is absent, or incorrect
   250  :not-authorized          ; The provided userid and/or Oauth credentials do not match
   342  :bad-oauth-sig           ; The signature  (using Oauth) is invalid
   601  :too-many-requests       ; Too Many Request
   2554 :bad-action              ; Wrong action or wrong webservice
   2555 :unknown-error           ; An unknown error occurred
   2556 :undefined-service})     ; Service is not defined


(defn- api-request
  "Makes an authenticated request to the API."
  [client resource action params]
  (let [url (str (:api-url client) "/" resource)
        query (assoc params
                     :action action
                     :userid (get-in client [:credentials :user_id]))

        oauth (oauth/credentials
                (get-in client [:credentials :consumer])
                (get-in client [:credentials :oauth_token])
                (get-in client [:credentials :oauth_token_secret])
                :GET url query)]
    (let [response (http/get url
                     {:query-params (merge query oauth)
                      :as :json})]
      (if (= 200 (:status response))
        (let [result (update-in (:body response)
                                [:status]
                                #(status-codes % %))]
          (if (= :success (:status result))
            (:body result)
            result))
        (throw (ex-info (str "Unsuccessful Withings response: "
                             (:status response))
                        response))))))


(defrecord HTTPClient
  [api-url credentials]

  Client

  (user-info
    [this]
    (api-request this "user" "getbyuserid" nil))


  (get-activity
    [this date]
    (api-request this "measure" "getactivity" {:date date}))


  (get-activity
    [this from-date to-date]
    (api-request this "measure" "getactivity" {:startdateymd from-date
                                               :enddateymd to-date}))

  #_ ...)


(defn http-client
  "Constructs a new HTTP API client."
  ([credentials]
   (HTTPClient. default-api-url credentials))
  ([api-url credentials]
   (HTTPClient. api-url credentials)))
