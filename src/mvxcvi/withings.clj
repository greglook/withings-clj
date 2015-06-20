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

  (body-measurements
    [client opts]
    "Get body measurements.")

  (activity-summary
    [client date]
    [client from-date to-date]
    "Get daily summaries of activity information on a specific day or between a
    range of days.")

  (activity-data
    [client opts]
    "Gets detailed time-series activity data.")

  (sleep-summary
    [client from-date to-date]
    "Get sleep summary.")

  (sleep-data
    [client opts]
    "Get detailed sleep measurements."))



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



;; ## HTTP API Requests

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
            (throw (ex-info (str "Unsuccessful Withings response: "
                                 (:status result))
                            (dissoc result :body)))))
        (throw (ex-info (str "Unsuccessful Withings response: "
                             (:status response))
                        response))))))



;; ## Data Conversion

(defn- convert-user-info
  [user-info]
  (-> user-info
      (update-in [:birthdate] #(java.util.Date. (* 1000 %)))
      (update-in [:gender] {0 :male, 1 :female})))


(defn- convert-activity
  [activity]
  activity)



;; ## HTTP Client Component

(defrecord HTTPClient
  [api-url credentials]

  Client

  (user-info
    [this]
    (->>
      (api-request this "user" "getbyuserid" nil)
      :users
      (mapv convert-user-info)))


  (body-measurements
    [this opts]
    (api-request this "measure" "getmeas" opts))


  (activity-summary
    [this date]
    (->>
      {:date date}
      (api-request this "measure" "getactivity")
      (convert-activity)
      (vector)))


  (activity-summary
    [this from-date to-date]
    (->>
      {:startdateymd from-date
       :enddateymd to-date}
      (api-request this "measure" "getactivity")
      :activities
      (mapv convert-activity)))


  ; TODO: activity-data


  (sleep-summary
    [this from-date to-date]
    (api-request this "sleep" "getsummary"
                 {:startdateymd from-date
                  :enddateymd to-date}))


  (sleep-data
    [this opts]
    (api-request this "sleep" "get" opts)))


(defn http-client
  "Constructs a new HTTP API client."
  ([credentials]
   (HTTPClient. default-api-url credentials))
  ([api-url credentials]
   (HTTPClient. api-url credentials)))
