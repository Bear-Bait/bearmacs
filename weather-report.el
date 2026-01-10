;;; weather-report.el -*- lexical-binding: t; -*-

;;; weather-report.el --- David Lynch-inspired weather for Emacs
;;; Commentary:
;; Weather display using Open-Meteo API (no key required)
;; Perfect for emacsOS
;;
;; Usage:
;;   M-x weather-report
;;
;; In the weather buffer:
;;   r - refresh weather
;;   f - toggle 7-day forecast
;;   a - toggle ASCII art
;;   q - quit
;;
;;; Code:

(require 'url)
(require 'json)

;;; Customization
(defgroup goldensunshine nil
  "David Lynch-inspired weather in Emacs."
  :group 'applications
  :prefix "goldensunshine-")

(defcustom goldensunshine-location "Athens,NY,US"
  "Location for weather display."
  :type 'string)

(defcustom goldensunshine-location-name "Athens, NY"
  "Human-readable location name."
  :type 'string)

(defcustom goldensunshine-latitude 42.23
  "Latitude for Open-Meteo API."
  :type 'number)

(defcustom goldensunshine-longitude -73.79
  "Longitude for Open-Meteo API."
  :type 'number)

(defcustom goldensunshine-units "fahrenheit"
  "Temperature units: fahrenheit or celsius."
  :type '(choice (const "fahrenheit")
                 (const "celsius")))

(defcustom goldensunshine-api-key ""
  "API key (not needed for Open-Meteo)."
  :type 'string)

(defcustom goldensunshine-show-ascii-art t
  "Show ASCII art weather representations."
  :type 'boolean)

(defcustom goldensunshine-auto-theme nil
  "Automatically change theme based on weather (if enabled)."
  :type 'boolean)

(defcustom goldensunshine-update-interval 600
  "Update weather every N seconds (default 10 minutes)."
  :type 'number)

;;; Faces
(defface goldensunshine-title
  '((t (:height 1.8 :foreground "goldenrod" :weight bold)))
  "Face for the main title."
  :group 'goldensunshine)

(defface goldensunshine-temp
  '((t (:height 1.4 :foreground "light goldenrod" :weight semi-bold)))
  "Face for temperature display."
  :group 'goldensunshine)

(defface goldensunshine-description
  '((t (:height 1.2 :foreground "wheat" :slant italic)))
  "Face for weather description."
  :group 'goldensunshine)

(defface goldensunshine-ascii
  '((t (:height 1.0 :foreground "light sky blue")))
  "Face for ASCII art."
  :group 'goldensunshine)

;;; Variables
(defvar goldensunshine-weather-cache nil
  "Cached weather data from Open-Meteo.")

(defvar goldensunshine-update-timer nil
  "Timer for auto-updating weather.")

(defvar goldensunshine-buffer-name "*Golden Sunshine Weather Report*"
  "Name of the Golden Sunshine buffer.")

(defvar goldensunshine-show-forecast t
  "Whether to show the forecast section.")

;;; Mode Definition
(defvar goldensunshine-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'goldensunshine-quit)
    (define-key map (kbd "r") #'weather-report)
    (define-key map (kbd "f") #'goldensunshine-toggle-forecast)
    (define-key map (kbd "a") #'goldensunshine-toggle-ascii-art)
    map)
  "Keymap for Golden Sunshine mode.")

(define-derived-mode goldensunshine-mode special-mode "Weather Report"
  "Major mode for displaying Lynch-style weather reports.

\\{goldensunshine-mode-map}"
  (buffer-disable-undo)
  (setq truncate-lines t))

(defun goldensunshine-quit ()
  "Quit the Golden Sunshine buffer."
  (interactive)
  (quit-window t))

;;; WMO Weather Code Interpretation
(defun goldensunshine-interpret-wmo-code (code &optional short-form)
  "Interpret WMO weather CODE as human-readable description.
If SHORT-FORM is t, return emoji/short version."
  (let ((code-map '(
    ;; Clear sky
    (0 . ("☀️ Clear sky" "Clear"))
    (1 . ("🌤️ Mainly clear" "Mostly Clear"))
    (2 . ("⛅ Partly cloudy" "Partly Cloudy"))
    (3 . ("☁️ Overcast" "Overcast"))

    ;; Fog/Mist
    (45 . ("🌫️ Foggy" "Fog"))
    (48 . ("🌫️ Depositing rime fog" "Fog"))

    ;; Drizzle
    (51 . ("🌧️ Light drizzle" "Drizzle"))
    (53 . ("🌧️ Moderate drizzle" "Drizzle"))
    (55 . ("🌧️ Dense drizzle" "Heavy Drizzle"))

    ;; Rain
    (61 . ("🌧️ Slight rain" "Light Rain"))
    (63 . ("🌧️ Moderate rain" "Rain"))
    (65 . ("⛈️ Heavy rain" "Heavy Rain"))

    ;; Snow
    (71 . ("❄️ Slight snow" "Light Snow"))
    (73 . ("❄️ Moderate snow" "Snow"))
    (75 . ("❄️ Heavy snow" "Heavy Snow"))

    ;; Showers
    (80 . ("⛈️ Slight rain showers" "Showers"))
    (81 . ("⛈️ Moderate rain showers" "Heavy Showers"))
    (82 . ("⛈️ Violent rain showers" "Violent Showers"))
    (85 . ("⛈️ Slight snow showers" "Snow Showers"))
    (86 . ("⛈️ Heavy snow showers" "Heavy Snow Showers"))

    ;; Thunderstorm
    (95 . ("⚡ Thunderstorm" "Thunderstorm"))
    (96 . ("⚡ Thunderstorm with hail" "Thunderstorm + Hail"))
    (99 . ("⚡ Thunderstorm with severe hail" "Severe Thunderstorm"))
    )))
    (let ((description (cdr (assoc code code-map))))
      (if description
          (if short-form (cadr description) (car description))
        (if short-form "?" "Unknown")))))

;;; ASCII Art Weather Graphics
(defun goldensunshine-ascii-clear-sky ()
  "ASCII art for clear sky."
  "
       \\   |   /
        \\  |  /
      --- ☀️ ---
        /  |  \\
       /   |   \\
")

(defun goldensunshine-ascii-cloudy ()
  "ASCII art for cloudy weather."
  "
        .-~~~-.
    .- ~ ~.~.~ ~.-.
   /    ~ ~.~ ~ .  \\
  (   ~  ~ ~.~  ~   )
   \\  ~ ~.~ ~.~  ~ /
    `- .~ ~.~. ~ -'
")

(defun goldensunshine-ascii-rainy ()
  "ASCII art for rainy weather."
  "
        .-~~~-.
    .- ~ ~.~.~ ~.-.
   (   ~  ~ ~.~  ~  )
    `- .~ ~.~. ~ -'
      ' ' ' ' ' '
     ' ' ' ' ' '
")

(defun goldensunshine-ascii-thunderstorm ()
  "ASCII art for thunderstorm."
  "
        .-~~~-.
    .- ~ ~.~.~ ~.-.
   (   ~  ~ ~.~  ~  )
    `- .~ ~.~. ~ -'
       \\  /  \\
        \\/⚡ \\/
      ' ' ' ' ' '
")

(defun goldensunshine-ascii-snow ()
  "ASCII art for snowy weather."
  "
        .-~~~-.
    .- ~ ~.~.~ ~.-.
   (   ~  ~ ~.~  ~  )
    `- .~ ~.~. ~ -'
      *  *  *  *
     *  *  *  *  *
      *  *  *  *
")

(defun goldensunshine-ascii-fog ()
  "ASCII art for foggy weather."
  "
    ≋≋≋≋≋≋≋≋≋≋≋≋≋
      ≋≋≋≋≋≋≋≋≋
    ≋≋≋≋≋≋≋≋≋≋≋≋≋
      ≋≋≋≋≋≋≋≋≋
    ≋≋≋≋≋≋≋≋≋≋≋≋≋
")

(defun goldensunshine-ascii-hot ()
  "ASCII art for hot weather."
  "
     \\  \\  |  /  /
      \\ \\ | / /
    --- ☀️☀️☀️ ---
      / / | \\ \\
     /  /  |  \\  \\
        🔥🔥🔥
")

(defun goldensunshine-ascii-cold ()
  "ASCII art for cold weather."
  "
      ❄️    ❄️    ❄️
        ❄️  ❄️
    ❄️    ❄️    ❄️
      🥶🥶🥶
        ❄️  ❄️
      ❄️    ❄️    ❄️
")

(defun goldensunshine-get-ascii-art (code temp)
  "Get ASCII art based on weather CODE and TEMP."
  (let ((desc (downcase (goldensunshine-interpret-wmo-code code t))))
    (cond
     ;; Extreme temperatures first
     ((< temp 10) (goldensunshine-ascii-cold))
     ((> temp 95) (goldensunshine-ascii-hot))
     ;; Weather conditions
     ((string-match-p "thunder\\|storm" desc) (goldensunshine-ascii-thunderstorm))
     ((string-match-p "snow" desc) (goldensunshine-ascii-snow))
     ((string-match-p "rain\\|drizzle\\|shower" desc) (goldensunshine-ascii-rainy))
     ((string-match-p "fog\\|mist" desc) (goldensunshine-ascii-fog))
     ((string-match-p "cloud\\|overcast" desc) (goldensunshine-ascii-cloudy))
     ((string-match-p "clear" desc) (goldensunshine-ascii-clear-sky))
     ;; Default to clear
     (t (goldensunshine-ascii-clear-sky)))))

;;; Lynchian Phrases - Inspired by real David Lynch weather reports
(defun goldensunshine-get-lynch-phrase (temp desc)
  "Get a Lynchian weather phrase based on TEMP and DESC."
  (let ((phrases
         (cond
          ;; EXTREME COLD (< 10°F) - hint of bitterness with optimism
          ((< temp 10)
           '("Now this, friends, this is genuinely cold. Single digits. But golden sunshine is still golden sunshine."
             "Below ten degrees this morning and I have to say, that's asking a lot of a person. But we press on."
             "It is COLD out there today. The kind of cold that makes you question your life choices. But it's beautiful too."
             "Single digit temperatures, and you know what, I'm not going to pretend that's comfortable. Stay warm, friends."
             "Now this is what I call serious cold. The air hurts your face. But there's a certain clarity to it."
             "Below ten degrees and the world is frozen solid. Not ideal, friends, not ideal. But there's beauty in it."
             "This cold is no joke today. Even the golden sunshine feels cold. Bundle up out there."
             "Single digits this morning. I'm going to be honest with you, that's rough. But we're going to get through it together."
             "It's the kind of cold where you can see your breath freeze. Brutal, but also kind of magical."
             "Sub-ten degrees and I have some words for this weather, but I'll keep it positive. Golden sunshine in spirit, friends."
             "Now this cold, this is the real deal. The trees are probably wondering what they did wrong. Stay inside if you can."
             "Below ten degrees. Even the squirrels look unhappy. But tomorrow is another day, friends."))

          ;; EXTREME HEAT (> 95°F) - hint of "this is too much" with optimism
          ((> temp 95)
           '("Now this is what I call HOT. We're up there in the numbers today. Find some shade, friends."
             "Over ninety-five degrees and the golden sunshine is really, uh, asserting itself today."
             "It's going to be a scorcher, friends. The kind of heat where the air feels thick. Stay hydrated."
             "This heat is intense today. I'm not going to sugarcoat it. The sun is working overtime."
             "Ninety-five plus degrees and you know what, that's a lot of golden sunshine. Maybe too much golden sunshine."
             "The heat is really something today. Even the pavement is complaining. Stay cool out there."
             "This is serious heat, friends. The kind that makes you move a little slower. And that's okay."
             "Over ninety-five and the world is melting a little bit. But there's beauty in it, somewhere."
             "Now this heat, this is the kind that tests you. Drink water, find air conditioning, be kind to yourself."
             "It's extremely hot today. The golden sunshine has become the golden hammer. Pace yourself, friends."
             "Ninety-five plus degrees and I'll be honest, that's asking a lot. But we're tough. We can handle it."
             "This heat is no joke. The golden sunshine is working very hard today. Too hard, maybe. Stay safe."))

          ;; Clear sky phrases
          ((string-match-p "clear" desc)
           '("Blue skies and golden sunshine all along the way!"
             "A beautiful blue sky today, like a dream!"
             "Clear skies! Golden sunshine drenching the world today!"
             "The kind of day where the birds sing the sweetest melodies!"
             "Not a cloud in the sky! Golden sunshine everywhere!"
             "A perfect day in the making. Golden sunshine, friends!"
             "And once again we're gonna be seeing beautiful blue skies and golden sunshine all along the way."
             "The sky is so blue today, it's looking very bright out there."
             "A clear morning, very still, and the golden sunshine is just beautiful."
             "If you can believe it, another gorgeous clear day. Golden sunshine, friends!"
             "The future is looking very bright today. Blue skies everywhere!"))

          ;; Cloudy phrases
          ((string-match-p "cloud\\|overcast" desc)
           '("Some clouds today, but golden sunshine is on the horizon!"
             "Cloudy skies with moments of golden sunshine peeking through!"
             "The clouds are doing a beautiful dance today!"
             "Today the clouds are like paintings in the sky!"
             "A silver-lining kind of day, friends. Golden sunshine is there!"
             "Clouds bringing a mysterious quality to our day!"
             "A cloudy morning, very slight breeze blowing. But these clouds are going to burn away."
             "Some clouds with us today, but hopefully also some blue skies and golden sunshine."
             "These clouds are going to burn away, probably mid-morning, maybe stay till lunch."
             "Quite a breeze blowing right now, clouds moving through. Golden sunshine is coming."
             "Partly cloudy all along the way today, friends. But that's beautiful too."))

          ;; Rainy phrases
          ((string-match-p "rain\\|drizzle\\|shower" desc)
           '("Rain today, but golden sunshine is waiting to break through!"
             "A beautiful rain today. The trees and flowers are so happy!"
             "Rain falling like music on the leaves today!"
             "The rain brings life to everything. Golden sunshine tomorrow!"
             "Wonderful rain today, like nature's own symphony!"
             "Rain washing the world clean today, friends!"
             "We've got some rain moving through, but after that, golden sunshine all along the way."
             "A rainy morning, and you know what, the trees really needed this."
             "Rain today, and I was thinking about how beautiful the sound is on the roof."
             "Some rain with us, very still otherwise. The world is getting a good drink."
             "The rain should clear by this afternoon. Then we're looking at golden sunshine."))

          ;; Thunder phrases
          ((string-match-p "thunder" desc)
           '("Thunder and lightning, but golden sunshine will return!"
             "Amazing electricity in the air today. Nature's power show!"
             "Thunder rolling across the sky like cosmic drums!"
             "Lightning creating momentary daylight in the darkness!"
             "A magnificent thunderstorm brewing. Stay safe, friends!"
             "Thunder speaking in its ancient language today!"
             "Some thunder moving through, quite a light show. Nature is putting on a performance."
             "Thunderstorms today, and there's something exciting about that electricity in the air."
             "We've got some thunder, but it's clearing. Golden sunshine is on the other side."
             "A stormy morning, very dramatic. But the future is looking bright, friends."
             "Thunder today. I've always loved that sound, like the sky is talking to us."))

          ;; Snow phrases
          ((string-match-p "snow" desc)
           '("Snowflakes dancing down from the heavens today!"
             "A beautiful blanket of snow transforming our world!"
             "Each snowflake a perfect crystal miracle!"
             "The snow makes everything so peaceful and quiet!"
             "A winter wonderland today, friends. Golden sunshine in our hearts!"
             "The snow brings a magical quality to everything it touches!"
             "Snow falling this morning, very still, very quiet. It's like being inside a dream."
             "We've got snow today, and everything looks brand new out there."
             "A snowy morning. If you can believe it, each flake is completely unique."
             "Snow covering everything, and there's a golden quality to the light."
             "The snow is beautiful today. Bundle up and enjoy it, friends."))

          ;; Fog phrases
          ((string-match-p "fog\\|mist" desc)
           '("The fog creates a mysterious mood today!"
             "Fog like something from a beautiful dream!"
             "The world wrapped in a blanket of fog today!"
             "The fog transforms familiar things into mysterious shapes!"
             "A day where reality seems to shift in the mist!"
             "The fog bringing a cinematic quality to our day, friends!"
             "Some low clouds and fog clearing a little bit now, very still."
             "The fog is lifting and I think we're going to see some golden sunshine."
             "A foggy morning, but it's clearing. The world looks like a beautiful dream right now."
             "Fog burning off slowly. There's something magical about watching that happen."
             "The mist is creating quite a mood today. Very mysterious, very beautiful."))

          ;; Hot weather phrases (85-95°F)
          ((> temp 85)
           '("Beautiful golden sunshine and it's a hot one today!"
             "The kind of heat that makes you appreciate the shade of a mighty tree!"
             "A real scorcher today! Stay hydrated, friends!"
             "The sun sharing its incredible warmth with us today!"
             "Heat bringing the summer dreams alive!"
             "Hot golden sunshine bathing the world today!"
             "It's going to be a hot one this afternoon. Golden sunshine really making itself known."
             "Quite warm today, should be going up into the high numbers. Stay cool, friends."
             "A hot morning already, and it's only going to get warmer. But that's summer for you."
             "The heat is with us today. Find some shade and a cold drink."
             "Hot day ahead. The golden sunshine is really pouring down."))

          ;; Cold weather phrases (10-45°F)
          ((< temp 45)
           '("A crisp, cold day with golden sunshine in our hearts!"
             "The kind of cold that makes a cup of coffee even more special!"
             "A beautiful chill in the air today, friends!"
             "Cold weather that makes you feel so alive!"
             "The cold air carrying the breath of winter!"
             "A brisk day that brightens the cheeks and the spirit!"))

          ;; Default phrases
          (t
           '("Golden sunshine all along the way!"
             "Have a great day, filled with golden sunshine!"
             "A magical day unfolding before us!"
             "Today holds so many wonderful possibilities!"
             "A day to treasure, friends!"
             "The air is filled with golden possibilities today!")))))
    ;; Randomly select one phrase from the appropriate list
    (nth (random (length phrases)) phrases)))

;;; API Integration - Open-Meteo (NO API KEY REQUIRED)
(defun goldensunshine-fetch-weather (callback)
  "Fetch weather from Open-Meteo API and call CALLBACK with data.
No API key required - just works!"
  (let ((url (format
              "https://api.open-meteo.com/v1/forecast?latitude=%.2f&longitude=%.2f&current=temperature_2m,weather_code,relative_humidity_2m&daily=weather_code,temperature_2m_max,temperature_2m_min,sunrise,sunset&hourly=temperature_2m,weather_code&temperature_unit=%s&timezone=America/New_York&forecast_days=14"
              goldensunshine-latitude
              goldensunshine-longitude
              (if (string= goldensunshine-units "fahrenheit") "fahrenheit" "celsius"))))
    (url-retrieve url
                  (lambda (status)
                    (if (plist-get status :error)
                        (message "Weather Report: Failed to fetch weather")
                      (goto-char (point-min))
                      (re-search-forward "^$" nil t)
                      (let* ((json-object-type 'hash-table)
                             (json-array-type 'list)
                             (json-key-type 'string)
                             (data (json-read)))
                        (setq goldensunshine-weather-cache data)
                        (funcall callback data))))
                  nil t t)))

;;; Formatting Functions
(defun goldensunshine-get-greeting ()
  "Return a time-appropriate greeting."
  (let ((hour (string-to-number (format-time-string "%H"))))
    (cond
     ((< hour 1) "Good evening")
     ((< hour 5) "Hello night owls")
     ((< hour 12) "Good morning")
     ((< hour 17) "Good afternoon")
     ((< hour 21) "Good evening")
     (t "Good evening"))))

(defun goldensunshine-format-current (data)
  "Format current weather conditions from DATA in David Lynch style."
  (let* ((current (gethash "current" data))
         (temp (round (gethash "temperature_2m" current)))
         (code (gethash "weather_code" current))
         (humidity (gethash "relative_humidity_2m" current))
         (desc (goldensunshine-interpret-wmo-code code))
         (desc-short (goldensunshine-interpret-wmo-code code t))
         (unit-char (if (string= goldensunshine-units "fahrenheit") "°F" "°C"))
         (date-str (format-time-string "%A, %B %d, %Y"))
         (time-str (format-time-string "%I:%M %p"))
         (lynch-phrase (goldensunshine-get-lynch-phrase temp (downcase desc-short)))
         (ascii-art (when goldensunshine-show-ascii-art
                      (goldensunshine-get-ascii-art code temp))))
    (concat
     "\n"
     (propertize "    TODAY'S WEATHER REPORT\n" 'face 'goldensunshine-title)
     (propertize (format "    %s at %s\n\n" date-str time-str) 'face 'default)
     (when ascii-art
       (propertize ascii-art 'face 'goldensunshine-ascii))
     (propertize (format "\n    %s everyone!\n\n" (goldensunshine-get-greeting)) 'face 'default)
     (propertize (format "    Here in %s,\n" goldensunshine-location-name) 'face 'default)
     (propertize (format "    It's currently %d%s\n" temp unit-char) 'face 'goldensunshine-temp)
     (propertize (format "    %s (Humidity: %d%%)\n\n" desc humidity) 'face 'goldensunshine-description)
     (propertize (format "    %s\n\n" lynch-phrase) 'face 'default)
     (propertize "    Everyone have a great day!\n\n" 'face 'default))))

(defun goldensunshine-format-forecast (data)
  "Format multi-day forecast from DATA."
  (let* ((daily (gethash "daily" data))
         (times (gethash "time" daily))
         (codes (gethash "weather_code" daily))
         (temps-max (gethash "temperature_2m_max" daily))
         (temps-min (gethash "temperature_2m_min" daily))
         (sunrises (gethash "sunrise" daily))
         (sunsets (gethash "sunset" daily))
         (unit-char (if (string= goldensunshine-units "fahrenheit") "°F" "°C")))
    (concat "\n"
            (propertize "    7-Day Forecast\n" 'face 'goldensunshine-title)
            (propertize "    ═══════════════════════════════════════\n" 'face 'shadow)
            (mapconcat
             (lambda (i)
               (let ((time (nth i times))
                     (code (nth i codes))
                     (max-t (round (nth i temps-max)))
                     (min-t (round (nth i temps-min)))
                     (sunrise (nth i sunrises))
                     (sunset (nth i sunsets))
                     (desc (goldensunshine-interpret-wmo-code (nth i codes) t)))
                 (format "    %s  %s\n        High: %d%s  Low: %d%s\n        ☀️ Rise: %s  Set: %s\n"
                         time desc max-t unit-char min-t unit-char
                         (substring sunrise 11 16)
                         (substring sunset 11 16))))
             (number-sequence 0 6)
             "\n")
            (propertize "\n    ═══════════════════════════════════════\n" 'face 'shadow))))

;;; Display Functions
(defun goldensunshine-display-buffer (content)
  "Display weather CONTENT in a beautiful buffer."
  (with-current-buffer (get-buffer-create goldensunshine-buffer-name)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert content)
      (goldensunshine-mode)
      (goto-char (point-min)))
    (switch-to-buffer (current-buffer))))

(defun goldensunshine-toggle-ascii-art ()
  "Toggle ASCII art display and refresh."
  (interactive)
  (setq goldensunshine-show-ascii-art (not goldensunshine-show-ascii-art))
  (weather-report)
  (message "ASCII art %s" (if goldensunshine-show-ascii-art "enabled" "disabled")))

(defun goldensunshine-toggle-forecast ()
  "Toggle forecast display and refresh."
  (interactive)
  (setq goldensunshine-show-forecast (not goldensunshine-show-forecast))
  (weather-report)
  (message "Forecast %s" (if goldensunshine-show-forecast "enabled" "disabled")))

(defun goldensunshine-wttr ()
  "Fetch weather from wttr.in as alternative ASCII view."
  (interactive)
  (let ((url (format "https://wttr.in/%s?0FTq"
                     (url-encode-url goldensunshine-location-name))))
    (url-retrieve url
                  (lambda (status)
                    (if (plist-get status :error)
                        (message "Failed to fetch from wttr.in")
                      (goto-char (point-min))
                      (re-search-forward "^$" nil t)
                      (let ((content (buffer-substring (point) (point-max))))
                        (with-current-buffer (get-buffer-create "*wttr.in Weather*")
                          (let ((inhibit-read-only t))
                            (erase-buffer)
                            (insert "\n    Weather from wttr.in:\n\n")
                            (insert content)
                            (special-mode))
                          (display-buffer (current-buffer))))))
                  nil t t)))

;;;###autoload
(defun weather-report ()
  "Display the weather in David Lynch style using Open-Meteo API.
No API key required! Just open the weather and vibe.

In the weather buffer, press:
  r - refresh weather data
  f - toggle 7-day forecast
  a - toggle ASCII art display
  q - quit"
  (interactive)
  (goldensunshine-fetch-weather
   (lambda (data)
     (let ((content (concat (goldensunshine-format-current data)
                           (when goldensunshine-show-forecast
                             (goldensunshine-format-forecast data)))))
       (goldensunshine-display-buffer content)))))

;; Alias for backwards compatibility
(defalias 'goldensunshine 'weather-report)

;;;###autoload
(defun goldensunshine-one-line ()
  "Return a one-line weather summary for modeline use."
  (if goldensunshine-weather-cache
      (let* ((current (gethash "current" goldensunshine-weather-cache))
             (temp (round (gethash "temperature_2m" current)))
             (code (gethash "weather_code" current))
             (desc (goldensunshine-interpret-wmo-code code t))
             (unit-char (if (string= goldensunshine-units "fahrenheit") "F" "C")))
        (format "%s %d°%s" desc temp unit-char))
    "Weather: --"))

;;;###autoload
(defun goldensunshine-auto-update ()
  "Start auto-updating weather."
  (interactive)
  (when goldensunshine-update-timer
    (cancel-timer goldensunshine-update-timer))
  (setq goldensunshine-update-timer
        (run-with-timer 0 goldensunshine-update-interval #'weather-report))
  (message "Weather Report: Auto-update enabled"))

;;;###autoload
(defun goldensunshine-stop-auto-update ()
  "Stop auto-updating weather."
  (interactive)
  (when goldensunshine-update-timer
    (cancel-timer goldensunshine-update-timer)
    (setq goldensunshine-update-timer nil)
    (message "Weather Report: Auto-update disabled")))

(provide 'weather-report)
;;; weather-report.el ends here
