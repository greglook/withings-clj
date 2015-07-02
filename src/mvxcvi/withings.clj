(ns mvxcvi.withings
  "Client for the Withings API.

  See: http://oauth.withings.com/api"
  (:require
    [clj-http.client :as http]
    (clj-time
      [coerce :as coerce-time]
      [core :as time]
      [format :as format-time])
    [meajure.core :as meajure]
    [oauth.client :as oauth]))


(def default-oauth-url
  "https://oauth.withings.com/account")


(def default-api-url
  "https://wbsapi.withings.net")


(def status-codes
  "Map of Withings 'status' response codes to descriptive keywords."
  {0    :success                 ; Operation was successful
   247  :bad-userid              ; The userid provided is absent, or incorrect
   250  :not-authorized          ; The provided userid and/or Oauth credentials do not match
   342  :bad-oauth-sig           ; The signature  (using Oauth) is invalid
   503  :invalid-params          ; Action parameters are incorrect
   601  :too-many-requests       ; Too Many Request
   2554 :bad-action              ; Wrong action or wrong webservice
   2555 :unknown-error           ; An unknown error occurred
   2556 :undefined-service})     ; Service is not defined


(def genders
  "Enumeration of gender codes to keyword names."
  {0 :male
   1 :female})


(def device-models
  "Enumeration of device model codes to keyword names."
  { 0 :user
    1 :body-scale
    4 :blood-pressure-monitor
   16 :pulse
   32 :aura})


(def measurement-categories
  "Enumeration of measurement category codes to keyword names."
  {1 :real
   2 :goal})


(def measurement-types
  "Enumeration of measurement type codes to a tuple of the keyword name and
  measurement units."
  { 1 [:weight :kg]
    4 [:height :m]
    5 [:lean-mass :kg]
    6 [:fat-ratio :percent]
    8 [:fat-mass :kg]
    9 [:blood-pressure-diastolic :mmHg]
   10 [:blood-pressure-systolic  :mmHg]
   11 [:heart-rate :bpm]
   54 [:SpO2 :percent]})


(def sleep-states
  "Enumeration of sleep state codes to keyword names."
  {0 :awake
   1 :light
   2 :deep
   3 :REM})



;; ## API Protocol

(defprotocol Client
  "Protocol for reading data from the Withings API."

  (user-info
    [client]
    "Retrieve information about the authenticated user.")

  (body-measurements
    [client opts]
    "Get body measurements. Additional options may be applied to filter the
    results:

    `:after`          Only return results following this datetime.
    `:before`         Only return results prior to this datetime.
    `:updated-since`  Return results updated after this datetime.
    `:type`           Measurement type keyword. See `measurement-types`.
    `:category`       Measurement category keyword. See `measurement-categories`.
    `:limit`          Limit the number of results returned.
    `:offset`         Offset into results returned.")

  (activity-summary
    [client date]
    [client start-date end-date]
    "Get daily summaries of activity information on a specific day or between a
    range of days. Times should be provided as date-times.")

  (activity-data
    [client after before]
    "Gets detailed time-series activity data.")

  (sleep-summary
    [client start-date end-date]
    "Get sleep summary data.")

  (sleep-data
    [client updated-since]
    [client start-date end-date]
    "Get detailed sleep measurements."))



;; ## Data Conversion

(defn- inst->ymd
  "Converts a datetime instant into a year-month-day string."
  [inst]
  (-> :year-month-day
      (format-time/formatters)
      (format-time/unparse inst)))


(defn- epoch->inst
  "Converts a Unix epoch timestamp (in seconds) into an inst."
  ([epoch]
   (coerce-time/from-long (* epoch 1000)))
  ([epoch tz]
   (time/to-time-zone (coerce-time/from-long (* epoch 1000)) tz)))


(defn- update-fields
  [data & fields]
  (reduce (fn [acc [k f]]
            (apply update-in acc [k] f))
          data (partition 2 fields)))


(defn- convert-user-info
  [data]
  (update-fields data
    :birthdate [epoch->inst]
    :gender    [genders]))


(defn- convert-body-measurement
  [measure]
  (let [amount (BigDecimal. (BigInteger/valueOf (:value measure)) (- (:unit measure)))]
    (if-let [[kw unit] (measurement-types (:type measure))]
      (array-map :type kw :value (meajure/make-unit amount unit 1))
      (array-map :type (:type measure) :value amount))))


(defn- convert-measurement-groups
  [tz group]
  (update-fields group
    :attrib   [{0 :certain, 1 :ambiguous, 2 :manual, 3 :creation}]
    :date     [epoch->inst tz]
    :category [measurement-categories]
    :measures [(partial mapv convert-body-measurement)]))


(defn- convert-activity-summary
  [data]
  (update-fields data
    :timezone  [time/time-zone-for-id]
    :distance  [meajure/make-unit :meter  1]
    :elevation [meajure/make-unit :meter  1]
    :intense   [meajure/make-unit :second 1]
    :moderate  [meajure/make-unit :second 1]
    :soft      [meajure/make-unit :second 1]))


(defn- convert-sleep-summary
  [data]
  (let [tz (time/time-zone-for-id (:timezone data))]
    (update-fields (dissoc data :timezone)
      :model     [device-models]
      :startdate [epoch->inst tz]
      :enddate   [epoch->inst tz]
      :modified  [epoch->inst tz]
      :data      [update-fields
                  :durationtosleep    [meajure/make-unit :second 1]
                  :lightsleepduration [meajure/make-unit :second 1]
                  :deepsleepduration  [meajure/make-unit :second 1]
                  :wakeupduration     [meajure/make-unit :second 1]])))


(defn- convert-sleep-data
  [data]
  (update-fields data
    :startdate [epoch->inst]
    :enddate   [epoch->inst]
    :state     [sleep-states]))



;; ## OAuth Functions

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
  (let [tokens (oauth/access-token consumer temp-token)]
    (assoc
      (dissoc tokens :oauth_token :oauth_token_secret :user_id)
      :consumer consumer
      :oauth-token (:oauth_token tokens)
      :oauth-token-secret (:oauth_token_secret tokens)
      :user-id (:user_id tokens))))


(defn- api-request
  "Makes an authenticated request to the API."
  [client resource action params]
  (let [url (str (:api-url client) "/" resource)
        query (assoc params
                     :action action
                     :userid (get-in client [:credentials :user-id]))

        oauth (oauth/credentials
                (get-in client [:credentials :consumer])
                (get-in client [:credentials :oauth-token])
                (get-in client [:credentials :oauth-token-secret])
                :GET url query)]
    (let [response (http/get url
                     {:query-params (merge query oauth)
                      ;:debug true
                      :as :json})]
      (if (= 200 (:status response))
        (let [result (update-in (:body response)
                                [:status]
                                #(status-codes % %))]
          (if (= :success (:status result))
            (:body result)
            (throw (ex-info (str "Unsuccessful Withings response: "
                                 (:status result) " - "
                                 (:error result "--"))
                            (dissoc result :body)))))
        (throw (ex-info (str "Unsuccessful Withings response: "
                             (:status response))
                        response))))))



;; ## HTTP Client Component

(defrecord HTTPClient
  [api-url credentials]

  Client

  (user-info
    [this]
    (->> (api-request this "v2/user" "getbyuserid" nil)
         (:users)
         (mapv convert-user-info)))


  (body-measurements
    [this opts]
    {:pre [(not (and (or (:after opts) (:before opts)) (:updated-since opts)))]}
    (let [query (cond-> (select-keys opts [:limit :offset])
                  (:after opts)
                    (assoc :startdate (coerce-time/to-epoch (:after opts)))
                  (:before opts)
                    (assoc :enddate (coerce-time/to-epoch (:before opts)))
                  (:updated-since opts)
                    (assoc :lastupdate (coerce-time/to-epoch (:updated-since opts)))
                  (:type opts)
                    (assoc :meastype (some (fn [[code [kw _]]]
                                             (when (= kw (:type opts))
                                               code))
                                           measurement-types))
                  (:category opts)
                    (assoc :category (some (fn [[code kw]]
                                             (when (= kw (:category opts))
                                               code))
                                           measurement-categories)))]
      (let [data (api-request this "measure" "getmeas" query)
            tz (time/time-zone-for-id (:timezone data))]
        (update-in data [:measuregrps] (partial mapv (partial convert-measurement-groups tz))))))


  (activity-summary
    [this date]
    (-> (api-request
          this "v2/measure" "getactivity"
          {:date (inst->ymd date)})
        (convert-activity-summary)))


  (activity-summary
    [this start-date end-date]
    (-> (api-request
          this "v2/measure" "getactivity"
          {:startdateymd (inst->ymd start-date)
           :enddateymd (inst->ymd end-date)})
        (update-in [:activities] (partial mapv convert-activity-summary))))


  ; FIXME: untested, need to sign up for API.
  (activity-data
    [this after before]
    (api-request
      this "v2/measure" "getintradayactivity"
      {:startdate (coerce-time/to-epoch after)
       :enddate (coerce-time/to-epoch before)}))


  (sleep-summary
    [this start-date end-date]
    (-> (api-request
          this "v2/sleep" "getsummary"
          {:startdateymd (coerce-time/to-epoch start-date)
             :enddateymd (coerce-time/to-epoch end-date)})
        (update-in [:series] (partial mapv convert-sleep-summary))))


  (sleep-data
    [this updated-since]
    (-> (api-request
          this "v2/sleep" "get"
          {:lastupdate (inst->ymd updated-since)})
        (update-in [:model] device-models)
        (update-in [:series] (partial mapv convert-sleep-data))))


  (sleep-data
    [this start-date end-date]
    (-> (api-request
          this "v2/sleep" "get"
          {:startdate (inst->ymd start-date)
           :enddate (inst->ymd end-date)})
        (update-in [:model] device-models)
        (update-in [:series] (partial mapv convert-sleep-data)))))


(defn http-client
  "Constructs a new HTTP API client."
  ([credentials]
   (HTTPClient. default-api-url credentials))
  ([api-url credentials]
   (HTTPClient. api-url credentials)))
