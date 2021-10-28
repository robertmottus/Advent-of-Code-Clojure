(ns year2020.day04.day4
  (:require [ysera.test :refer [is is= is-not]]
            [clojure.string :as str :only [index-of]]))

; https://adventofcode.com/2020/day/4

(declare destruct-passport-string)
(declare parse-int)
(declare passport-keys-present?)
(declare passport-values-valid?)

(defn valid-passport-part1? [passport-string]
  (passport-keys-present? passport-string))

(defn valid-passport-part2? [passport-string]
  (and
    (passport-keys-present? passport-string)
    (passport-values-valid? passport-string)
    ))

(defn passport-keys-present?
  {:doc  "
  The automatic passport scanners are slow because they're having trouble detecting which passports have all
  required fields. The expected fields are as follows:
    byr (Birth Year)
    iyr (Issue Year)
    eyr (Expiration Year)
    hgt (Height)
    hcl (Hair Color)
    ecl (Eye Color)
    pid (Passport ID)
    cid (Country ID)

  Each passport is represented as a sequence of key:value pairs separated by spaces or newlines.
  Passports are separated by blank lines.

  The third passport is interesting; the only missing field is cid, so it looks like data from North Pole Credentials,
  not a passport at all! Surely, nobody would mind if you made the system temporarily ignore missing cid fields.
  Treat this 'passport' as valid.
  "
   :test (fn []
           (is= (passport-keys-present? "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd\nbyr:1937 iyr:2017 cid:147 hgt:183cm") true)
           (is= (passport-keys-present? "iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884\nhcl:#cfa07d byr:1929") false)
           (is= (passport-keys-present? "hcl:#ae17e1 iyr:2013\neyr:2024\necl:brn pid:760753108 byr:1931\nhgt:179cm") true)
           (is= (passport-keys-present? "hcl:#cfa07d eyr:2025 pid:166559648\niyr:2011 ecl:brn hgt:59in") false)
           )}
  [passport-string]
  (let [
        passpost-map (destruct-passport-string passport-string)
        keys-without-cid (disj (set (keys passpost-map)) :cid)
        valid (= #{:ecl :pid :eyr :hcl :byr :iyr :hgt} keys-without-cid)
        ]
    valid
    )
  )

(defn passport-values-valid?
  {:doc  "
  You can continue to ignore the cid field, but each other field has strict rules about what values are valid for
  automatic validation:
    byr (Birth Year) - four digits; at least 1920 and at most 2002.
    iyr (Issue Year) - four digits; at least 2010 and at most 2020.
    eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
    hgt (Height) - a number followed by either cm or in:
    If cm, the number must be at least 150 and at most 193.
    If in, the number must be at least 59 and at most 76.
    hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
    ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
    pid (Passport ID) - a nine-digit number, including leading zeroes.
    cid (Country ID) - ignored, missing or not.
  "
   :test (fn []
           (is= (passport-values-valid? "eyr:1972 cid:100\nhcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926") false)
           (is= (passport-values-valid? "iyr:2019\nhcl:#602927 eyr:1967 hgt:170cm\necl:grn pid:012533040 byr:1946") false)
           (is= (passport-values-valid? "hcl:dab227 iyr:2012\necl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277") false)
           (is= (passport-values-valid? "hgt:59cm ecl:zzz\neyr:2038 hcl:74454a iyr:2023\npid:3556412378 byr:2007") false)

           (is= (passport-values-valid? "pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980\nhcl:#623a2f") true)
           (is= (passport-values-valid? "eyr:2029 ecl:blu cid:129 byr:1989\niyr:2014 pid:896056539 hcl:#a97842 hgt:165cm") true)
           (is= (passport-values-valid? "hcl:#888785\nhgt:164cm byr:2001 iyr:2015 cid:88\npid:545766238 ecl:hzl\neyr:2022") true)
           (is= (passport-values-valid? "iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719") true)
           )}
  [passport-strings]
  (let [passpost-map (destruct-passport-string passport-strings)
        {:keys [ecl pid eyr hcl byr iyr cid hgt]} passpost-map ]
    (let [
          byr-valid? (let [yr (read-string byr)] (and (<= 1920 yr) (<= yr 2002)))
          iyr-valid? (let [yr (read-string iyr)] (and (<= 2010 yr) (<= yr 2020)))
          eyr-valid? (let [yr (read-string eyr)] (and (<= 2020 yr) (<= yr 2030)))
          hgt-valid? (let [hgt-val (parse-int hgt)
                           hgt-cm-valid? (and (str/includes? hgt "cm") (<= 150 hgt-val) (<= hgt-val 193))
                           hgt-in-valid? (and (str/includes? hgt "in") (<= 59 hgt-val) (<= hgt-val 76)) ]
                       (or hgt-cm-valid? hgt-in-valid?))
          hcl-valid? (if(re-matches #"#[0-9a-f]{6}" hcl) true false)
          ecl-valid? (str/includes? ["amb" "blu" "brn" "gry" "grn" "hzl" "oth"] ecl)
          pid-valid? (if(re-matches #"[0-9]{9}" pid) true false)
          valid-part2 (and byr-valid? iyr-valid? eyr-valid? hgt-valid? hcl-valid? ecl-valid? pid-valid?)
          ]
      valid-part2
      ))
  )

(defn destruct-passport-string
  {:doc  "destructs the passport string"
   :test (fn []
           (is= (destruct-passport-string
                  "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd\nbyr:1937 iyr:2017 cid:147 hgt:183cm")
                {:byr "1937" :iyr "2017" :eyr "2020" :hgt "183cm" :hcl "#fffffd" :ecl "gry" :pid "860033327" :cid "147"})
           (is= (destruct-passport-string
                  "hcl:#ae17e1 iyr:2013\neyr:2024\necl:brn pid:760753108 byr:1931\nhgt:179cm")
                {:byr "1931" :iyr "2013" :eyr "2024" :hgt "179cm" :hcl "#ae17e1" :ecl "brn" :pid "760753108"})
           )}
  [passport-str]
  (let [
        passport-map (into {}
                           (as-> passport-str pp
                                 (str/split pp #"\s")
                                 (map #(str/split % #":") pp)
                                 (map #(vector (keyword (first %)) (str (second %))) pp)
                                 ))
        ]
    passport-map)
  )

(defn parse-int
  {:test
   (fn []
     (is= (parse-int "x1x") 1)
     (is= (parse-int "1.9") 1)
     (is= (parse-int ".1") 1)
     )} [s] (Integer. (re-find  #"\d+" s )))

(defn part1
  [passport-strings]
  (as-> passport-strings $
       (str/split $ #"\n\n")
       (map valid-passport-part1? $)
       (filter true? $)
       (count $)
       )
  )

(defn part2
  [passport-strings]
  (as-> passport-strings $
       (str/split $ #"\n\n")
       (map valid-passport-part2? $)
       (filter true? $)
       (count $)
       )
  )

(comment
  (def puzzle-input "hgt:176cm\niyr:2013\nhcl:#fffffd ecl:amb\nbyr:2000\neyr:2034\ncid:89 pid:934693255\n\nhcl:#b5c3db ecl:grn hgt:155cm pid:#baec97 iyr:2017\nbyr:1939\neyr:2020\n\npid:526669252 eyr:1972\nhgt:152cm ecl:dne byr:1960 hcl:z iyr:2023\n\neyr:2028 hcl:#c0946f hgt:73in byr:1926 ecl:#473aaf iyr:2016 pid:565318180\n\npid:472686027 ecl:oth iyr:2019\ncid:277 byr:1940\neyr:2030 hgt:170cm\nhcl:#62e117\n\necl:oth\niyr:2017\npid:938461813 hcl:#733820 byr:1959 hgt:159cm eyr:2022\n\niyr:2011 eyr:2021 hcl:z\necl:hzl byr:2002 pid:17324328 cid:140\nhgt:186cm\n\nbyr:2022 pid:3164234967 iyr:1984\nhgt:76cm hcl:6b3837\necl:#fa362b\neyr:2037\n\nhcl:z eyr:1945\npid:9247286687 hgt:75cm\niyr:1934 cid:326 ecl:zzz\nbyr:2005\n\nbyr:2005\necl:lzr\neyr:2021 pid:152cm\ncid:254 iyr:2020 hcl:z hgt:157cm\n\niyr:2020 eyr:2020 hcl:#18171d ecl:gry pid:914128753 hgt:168cm\nbyr:2002\n\nhcl:#7d3b0c hgt:160cm eyr:2020 iyr:2015\npid:054067854 ecl:brn byr:2023\n\nhcl:#cfa07d hgt:157cm\nbyr:1994 eyr:2027 pid:344443856\niyr:2016\n\npid:762423097\niyr:2014 hcl:#a97842 ecl:brn hgt:180cm byr:1927 eyr:2021\n\npid:6645616064 hcl:#ceb3a1 byr:2030\neyr:2032 hgt:158cm iyr:2012\necl:#e9619e\n\neyr:2022\necl:brn\nbyr:1986\nhgt:161cm cid:99 pid:288726584 hcl:#6b5442 iyr:2019\n\ncid:75\npid:117771843\nhgt:184cm byr:1937 ecl:brn\nhcl:#d88fd9\niyr:2015 eyr:2027\n\niyr:2016 hcl:#fffffd hgt:170cm eyr:2022 ecl:oth pid:629454113\nbyr:1952\n\nhcl:#c0946f iyr:2018 hgt:189cm\nbyr:1971 ecl:oth eyr:2029\npid:800207810\n\neyr:2022 hcl:#7d3b0c pid:969986413\nbyr:1978 iyr:2020 hgt:186cm\necl:gry\n\nhgt:171cm byr:1949 hcl:#341e13\necl:amb eyr:2030 pid:359107274 iyr:2013\n\npid:839751525 eyr:2024 byr:1921\niyr:2012 ecl:amb hcl:#b0ed6f hgt:154cm\n\npid:32592758\nbyr:2009\nhgt:107 iyr:2019 hcl:#866857\neyr:2036 ecl:amb\n\neyr:2040 hcl:#733820 cid:199\nbyr:2027\npid:7791792988 ecl:blu iyr:2026\nhgt:63cm\n\niyr:2011 cid:119 pid:344693475\necl:grn hgt:160cm eyr:2029 hcl:#346973 byr:1996\n\nhgt:161in byr:2025 cid:167 iyr:2024 eyr:2040 pid:034804648\nhcl:#efcc98 ecl:oth\n\necl:#ba14f0 iyr:1935\nhgt:60cm\nbyr:2003 eyr:1987\nhcl:8e509b pid:161cm\n\niyr:2018 pid:620508652 ecl:amb eyr:2023 hgt:183cm hcl:#a97842\nbyr:1967 cid:117\n\neyr:2022 ecl:amb\npid:476049089 iyr:2012\nhgt:165cm\nbyr:1955 hcl:#602927\n\nbyr:2014 hcl:z iyr:2029 cid:279 pid:28914607 hgt:75cm ecl:xry\n\nhgt:156cm eyr:2023 iyr:2011 ecl:oth hcl:#7d3b0c pid:561313217 byr:1952\n\niyr:2011 byr:1935\nhcl:#cfa07d ecl:oth pid:830614209\neyr:2028 hgt:173cm\n\niyr:2012 cid:210 eyr:2022\npid:652810130 hcl:#18171d ecl:grn byr:1960 hgt:152cm\n\neyr:2026 pid:815848563 hgt:75in iyr:2019 ecl:gry byr:1947\nhcl:#cfa07d\n\ncid:181 iyr:2012\neyr:2024 byr:1934 hcl:#c0946f\nhgt:165cm ecl:oth pid:232944581\n\ncid:135 iyr:2020\nbyr:1971 hcl:#733820 pid:531877857 hgt:179cm eyr:2027 ecl:amb\n\nbyr:1987 hcl:936807 eyr:2032 ecl:#4bec4a pid:605628619 cid:180 hgt:150in\niyr:2015\n\nhcl:b62ef0 ecl:#092141\npid:876635399 byr:1944 hgt:158cm iyr:2017 eyr:1924\n\niyr:2016 pid:7039815301 byr:2014 hgt:150 eyr:2032 ecl:blu hcl:z\n\nbyr:1979 eyr:2030 iyr:1978 hgt:63 pid:1554613758 hcl:z ecl:amb\n\nhgt:70cm hcl:e45897 iyr:2020 eyr:1977 ecl:dne pid:2878189427 byr:1973\n\niyr:2003\nhcl:#cfa07d\npid:260517078\nbyr:2030 hgt:175cm eyr:2020\necl:brn\n\npid:460604681 eyr:2022\ncid:138 iyr:2016 hgt:163cm\nbyr:1922\nhcl:#ceb3a1 ecl:oth\n\nhgt:167cm byr:2009 eyr:1975 cid:295 pid:174cm iyr:2029\nhcl:z\n\nhgt:67in ecl:grn\neyr:2023\ncid:122 pid:281246917 byr:1990 iyr:2011 hcl:#866857\n\necl:#ed7ddc byr:1922 cid:234 hcl:e61b1e iyr:1932 eyr:1996 pid:31344005 hgt:62cm\n\nbyr:1949\ncid:275 iyr:2017 ecl:grn\nhgt:164cm eyr:2027 hcl:#18171d\npid:751342937\n\necl:blu hgt:162cm\npid:432600613 byr:1923 eyr:2029 iyr:2011 hcl:#623a2f cid:315\n\niyr:2020\nhcl:#b2bb11 pid:055891584 ecl:grn\nhgt:67in\neyr:2029 byr:1937\n\niyr:2012\nhcl:#a97842 pid:325640714 ecl:blu hgt:185cm eyr:2024 byr:1971\n\nhcl:#b6652a pid:485327267\necl:brn hgt:155cm eyr:2028\niyr:2019\n\npid:902164867 hgt:77 cid:283 eyr:2027\niyr:2020 ecl:hzl byr:1935 hcl:#efcc98\n\necl:grn\nhcl:#ceb3a1 byr:1977 hgt:165cm\npid:850700221 eyr:2030\niyr:2012\n\nbyr:1989 ecl:brn eyr:2026 pid:919138357 iyr:2016\nhcl:#623a2f cid:319 hgt:161cm\n\niyr:2017\nbyr:1973 pid:293382118 hcl:#341e13 cid:143 ecl:hzl\nhgt:166cm eyr:2022\n\npid:517102798\nhcl:f9d9dd\neyr:1933 iyr:2019 hgt:164cm\nbyr:2017 ecl:utc\n\neyr:2023 pid:757868802 hcl:#18171d cid:244\nhgt:156cm\necl:blu iyr:2015 byr:1926\n\neyr:2022\niyr:2020\nhgt:158cm ecl:grn\nbyr:1988\npid:979194751 hcl:#888785\n\neyr:2039\npid:3867868142 byr:1936 ecl:dne iyr:2022 hcl:4b43b8\nhgt:115 cid:241\n\niyr:2015 eyr:2026\nhcl:#ceb3a1 pid:539099924\ncid:234\necl:brn\nbyr:1920 hgt:163cm\n\ncid:259 iyr:2020\npid:949453818 eyr:2022 hgt:181cm\nbyr:1997 ecl:blu hcl:#18171d\n\nbyr:2016\niyr:2012\necl:utc\nhgt:68in eyr:1993\npid:1542134802 hcl:486699\ncid:239\n\niyr:2018\nhgt:154cm ecl:brn byr:1970\neyr:2021 pid:581775861 hcl:#888785\n\niyr:2012\neyr:2027 hgt:67cm hcl:#efcc98 ecl:zzz pid:312104916 byr:2020\n\nhcl:#b6652a ecl:hzl eyr:2023 iyr:2012 pid:513268492\nhgt:159cm\n\nhgt:162in hcl:z\nbyr:2029\neyr:2023 ecl:#e2e7ab iyr:2016 pid:65979982\n\ncid:84 hgt:71in ecl:blu pid:982719716\neyr:2020 iyr:2014\n\neyr:2028 hgt:181cm\necl:hzl pid:255796693 hcl:#341e13 byr:1994 iyr:2011 cid:218\n\necl:blu\nbyr:2029 iyr:2017 pid:468504566 eyr:2020 hcl:z hgt:163cm\n\nhgt:158cm\neyr:2025 ecl:hzl cid:295 pid:601339484\nhcl:#7d3b0c byr:1991 iyr:2013\n\neyr:2028\niyr:2018 pid:2236240873\nhgt:172cm\necl:#0e337e hcl:#b6652a cid:108 byr:1930\n\necl:gry hcl:#888785\neyr:2020 pid:442479017 iyr:2016\n\niyr:2014 ecl:grn\ncid:313 eyr:2023\nhgt:183cm\nbyr:1976\npid:499580308 hcl:#53efe6\n\neyr:2034\ncid:235 hcl:8f3cf5\nbyr:2027\nhgt:161in pid:3259965094 ecl:xry iyr:2026\n\neyr:1978 byr:1925 iyr:2018 hgt:170cm ecl:#0c94e8\npid:562699017 hcl:#816949\n\neyr:2023 hcl:#866857 hgt:179cm\npid:785862442 iyr:2014 cid:165 ecl:amb byr:1939\n\nhgt:187cm\npid:64469711 ecl:gry eyr:2023 cid:225 hcl:#341e13 iyr:2011 byr:1958\n\nhgt:162cm byr:2028 ecl:#37e345\neyr:2037 hcl:19fb3d\niyr:2021\npid:#87921a\n\neyr:2027 hcl:#18171d\nbyr:2002 ecl:gry iyr:2014\npid:561506850 hgt:177cm\n\nhgt:64cm pid:#a92686\neyr:2029 cid:122\nbyr:2026\niyr:2017 hcl:z ecl:grn\n\neyr:2028 byr:2007 hgt:155cm ecl:#86fa1b hcl:#733820 pid:562889497\niyr:2019\n\npid:880698787\nbyr:1992\nhcl:#7d3b0c hgt:163cm ecl:hzl\niyr:2011 eyr:2021\n\neyr:2020 byr:1994 iyr:2011 hgt:186cm pid:841855425 hcl:#cfa07d ecl:gry\n\nbyr:1923 iyr:2015 ecl:amb pid:414655744\nhcl:#b6652a\nhgt:159cm\neyr:2026\n\nhgt:171cm ecl:amb pid:363065723 iyr:2020\ncid:66 hcl:#b6652a eyr:2021\nbyr:1960\n\neyr:2002\nhcl:2627b2 ecl:#1bf21d pid:168cm byr:2024 iyr:2020\nhgt:186in\n\niyr:2011 byr:1924 eyr:2024\nhcl:#b6652a ecl:brn\npid:794477411 hgt:162in\n\nhcl:z hgt:67cm\nbyr:2025\npid:582569979\niyr:2013\necl:oth eyr:2025\n\ncid:50 hcl:931e2c\nhgt:172in eyr:1994 iyr:2023\necl:#cd2204\nbyr:2015\npid:157cm\n\nhgt:173cm eyr:2028\necl:amb pid:569607283\nbyr:1942\niyr:2019\ncid:228\nhcl:#866857\n\ncid:109\necl:oth eyr:1933 byr:1982 pid:173cm hcl:#b6652a hgt:174cm\niyr:2023\n\ncid:69 hcl:#9ad05b pid:341135641\nbyr:1968 ecl:brn\niyr:2012 hgt:156cm\neyr:2020\n\nhgt:176cm\nbyr:1954 ecl:blu\neyr:2020\npid:478462637 iyr:2019\nhcl:#888785\n\niyr:2026 hgt:193in\nbyr:2018 pid:162cm hcl:605e7f eyr:1948 ecl:utc\n\nbyr:1962\neyr:2022 pid:445346117 iyr:2019 hgt:158cm hcl:#623a2f ecl:hzl\n\ncid:278 hgt:187cm eyr:2024 iyr:2016 byr:1964\necl:grn pid:450739552 hcl:#733820\n\necl:grn byr:2000 eyr:2023\npid:344489911 hcl:#7d3b0c iyr:2011 hgt:177cm\n\niyr:2015 hgt:180cm cid:190 hcl:#a97842 pid:359774842 eyr:2029 byr:2002 ecl:amb\n\neyr:2027 iyr:2015 ecl:hzl\npid:082733109\nbyr:1975 hgt:191cm cid:251 hcl:#888785\n\nhcl:#c0946f iyr:2015\nhgt:167cm byr:1990 ecl:amb pid:168cm eyr:2023\n\necl:gry eyr:2028\nbyr:1934 iyr:2013 hcl:#6b5442\npid:424412120 hgt:173cm\n\npid:273352568\neyr:2024\niyr:2013 byr:1926 hcl:#602927\necl:brn hgt:180cm\n\nhcl:#7d3b0c hgt:70in ecl:amb iyr:2019\nbyr:1937\neyr:2030 pid:309011548\n\necl:grn\nhgt:64in pid:796889811 hcl:#18171d\nbyr:1929 eyr:2027\n\necl:amb hcl:#888785\npid:412449028 cid:316 byr:1982\niyr:2019 eyr:2030 hgt:193cm\n\neyr:1927\nhcl:z hgt:158cm byr:1930\necl:lzr iyr:2018\ncid:197\npid:0906120002\n\necl:grn byr:1970 hgt:181cm\npid:376212702 eyr:2030 iyr:2017 cid:266 hcl:#f8b0f5\n\niyr:2018 hgt:73in pid:652356158 hcl:#c0946f\necl:grn byr:1973\n\ncid:170 hcl:#b6652a byr:2011\necl:gry iyr:2025 pid:#b6e567 hgt:67cm eyr:2016\n\nhgt:192cm ecl:amb eyr:2026 pid:201824712 hcl:#888785 byr:1966 iyr:2019\n\niyr:2013 byr:1995 eyr:2028 hcl:#b6652a ecl:brn cid:53 pid:705606447 hgt:176cm\n\nhcl:#341e13 byr:1951\nhgt:161cm pid:231973770 iyr:2015 ecl:hzl\neyr:2030\n\ncid:210 ecl:brn iyr:2017 eyr:2030\nhgt:176cm hcl:#efcc98\nbyr:1965\n\neyr:2020 hcl:#7d3b0c\npid:872088079 ecl:oth iyr:2017 byr:1920\nhgt:180cm\n\nhcl:#0b540c iyr:2019\nbyr:1938\nhgt:153cm ecl:gry pid:236785988\neyr:2020\n\neyr:2020 hgt:184cm iyr:2019\npid:673186642 ecl:oth byr:1977 hcl:#866857\n\neyr:2025\necl:gry hcl:#341e13 byr:1970 iyr:2010 pid:972122542 hgt:184cm\n\necl:grn byr:1992 hgt:71in\niyr:2014 cid:254 hcl:#fffffd pid:749733013\neyr:2026\n\ncid:98 ecl:amb eyr:2022\nhgt:169cm pid:022677680\nbyr:1937 iyr:2014 hcl:#e62c71\n\nhgt:192cm\niyr:2015\neyr:2028 ecl:oth pid:6000619833 hcl:#c0946f\nbyr:1930\n\nbyr:1938 hcl:#efcc98 hgt:178cm iyr:1953 eyr:2038\necl:brn pid:#cdc55a\n\nhgt:66in byr:1951 iyr:2016 hcl:#18171d\neyr:2027\necl:lzr pid:834188980\n\niyr:2012 eyr:2025\nhcl:#7d3b0c pid:330325803 cid:166 hgt:186cm byr:1938\necl:amb\n\niyr:2015 hcl:#602927 cid:268 eyr:2021\necl:amb hgt:186cm pid:318676962\n\nhcl:#3d6f3c iyr:2014 pid:665730784 cid:191 hgt:150cm byr:1981 ecl:oth eyr:2024\n\necl:grn hcl:#733820\neyr:2028 iyr:2010\nhgt:162cm byr:1944 pid:872962499\n\neyr:2028 byr:1974\necl:brn\niyr:2010 hcl:#18171d hgt:160cm\n\nhcl:#602927\nbyr:1959 eyr:2027 iyr:2016 ecl:brn hgt:169cm pid:078503025\n\nhcl:#623a2f pid:326300051 hgt:153cm\nbyr:1973 iyr:2012\necl:gry eyr:2026\n\nhgt:151cm\nbyr:1966 eyr:2029 pid:026952622 hcl:#18171d ecl:gry iyr:2010\n\nhcl:#7d3b0c byr:1974 pid:444713591 iyr:2017 eyr:2030\nhgt:165cm ecl:oth\n\niyr:2026 pid:184cm\necl:gmt hcl:z hgt:71cm\neyr:2029\n\ncid:310 hcl:#fffffd byr:1998\npid:450705840 iyr:2015\necl:grn eyr:2021 hgt:165cm\n\nbyr:1939 hcl:#623a2f ecl:gry hgt:69in pid:539812641 eyr:2027 iyr:2013\n\npid:207645014\niyr:2015\ncid:314 ecl:oth\nbyr:1942\neyr:2027 hgt:186cm hcl:#fffffd\n\necl:#fb7e3d eyr:2031 iyr:1956\nhgt:188 pid:160cm hcl:z byr:2027\n\nbyr:1972 iyr:2020 eyr:2026 hcl:#b6652a pid:289088329 hgt:65in ecl:gry\n\neyr:2027\nhgt:59cm\nbyr:2022\npid:938063769 ecl:zzz iyr:2028 hcl:23c762\n\nbyr:2004 hgt:74 iyr:2017\neyr:2040 ecl:blu pid:4611117799 cid:73 hcl:z\n\necl:brn byr:1962 cid:321\niyr:2019 eyr:2026\nhgt:159cm\nhcl:#667310 pid:439864945\n\niyr:2026 eyr:2039 pid:633263851 cid:321 ecl:lzr hgt:166cm\nbyr:2023 hcl:fc3c63\n\nbyr:1961 iyr:2010 ecl:blu\neyr:2023 pid:245858010\n\nhgt:193cm pid:821303249 eyr:2020 hcl:#6b5442 cid:130 byr:1946\n\neyr:2026 ecl:brn\nhcl:#733820 byr:1983 hgt:182cm pid:727380954 cid:188 iyr:2015\n\nhgt:152cm cid:206 iyr:2012 byr:1947 hcl:#888785 ecl:gry\npid:720312394 eyr:2023\n\nhgt:150cm ecl:gry pid:863712648\niyr:2019 cid:349 byr:1976 hcl:#602927 eyr:2022\n\nhgt:164in pid:953500867\neyr:2021\niyr:2014\nhcl:z cid:343 ecl:amb\n\nbyr:1981 pid:529710230 iyr:2013 eyr:2023\nhcl:#c0946f ecl:amb\nhgt:151cm\n\npid:706204190 hgt:154cm cid:317\nhcl:#602927 byr:1949 ecl:blu iyr:2010 eyr:2028\n\niyr:2019 hcl:#0219e6\npid:850093151 ecl:gry\neyr:2030\nbyr:1938 hgt:177cm\n\necl:brn hcl:#efcc98 eyr:2029 byr:1963\nhgt:185cm pid:611279647 iyr:2011\n\necl:blu eyr:2022 byr:1941 hgt:167cm\niyr:2012 hcl:#7d3b0c pid:415739564\ncid:193\n\neyr:2027 ecl:blu byr:1968 pid:479994566\nhcl:#733820 hgt:151cm\niyr:2011\n\npid:263729839 hgt:189cm eyr:2030 ecl:gry byr:2001 hcl:#602927\n\nbyr:1985\necl:amb pid:672663977 cid:139\nhgt:159cm hcl:#733820 iyr:2018 eyr:2020\n\nbyr:1998\nhcl:#cfa07d eyr:2023 pid:255046063 iyr:2011 ecl:blu hgt:177cm\n\necl:oth\nbyr:1980 pid:253747166 eyr:2029\nhcl:#6b5442 hgt:186cm\n\neyr:2030 hcl:#866857\nhgt:165cm\necl:amb\niyr:2017 pid:241240220 cid:164 byr:2001\n\nbyr:1994 hcl:#b6652a iyr:2015\npid:753831241\nhgt:175cm\neyr:2027 ecl:blu\n\nhcl:#b6652a pid:471594512\nbyr:1961 ecl:hzl hgt:175cm\niyr:2020 eyr:2025\n\nbyr:1987 pid:112366159\neyr:2028 hcl:22b2d7\nhgt:64in cid:222\necl:#b40dca iyr:2019\n\niyr:2015 hcl:e1ed55 hgt:160in ecl:utc byr:2015 eyr:2036\n\nbyr:1935\nhcl:#7d3b0c hgt:152cm ecl:gry\npid:160090332 iyr:2020 eyr:2020\n\npid:552779024 byr:1998 hgt:185cm ecl:gry eyr:2026 iyr:2013 hcl:#d46cd6\n\necl:oth pid:311860969\ncid:57\nhgt:60in\neyr:2026\nhcl:#ceb3a1\nbyr:1961 iyr:2011\n\neyr:2021 hgt:162cm cid:240\npid:259997995\nhcl:#efcc98\necl:gry byr:1962 iyr:2017\n\nhcl:#866857\niyr:2016\neyr:2029\necl:blu byr:1927 cid:249 pid:665324615 hgt:65in\n\nbyr:1931\ncid:331\nhgt:66in\necl:grn iyr:2020 hcl:#efcc98 eyr:2025 pid:175780921\n\nhgt:98\neyr:2040 ecl:blu byr:2029\niyr:1967 hcl:0d76e9\npid:#c9053a cid:296\n\npid:370918950\nhcl:#602927\nbyr:1938\nhgt:178cm iyr:2018 eyr:2030\necl:oth\n\nhgt:185cm\neyr:1984 ecl:oth pid:851080398\nhcl:z byr:2027 iyr:2017\n\npid:095078224 byr:1957 hcl:#45bcf4 ecl:#f643f9 hgt:63cm eyr:2036 iyr:1978\n\nhcl:z\neyr:2023 ecl:oth hgt:162cm\niyr:2016 byr:1938 pid:#fdcddf\n\nhcl:#341e13 iyr:2013 hgt:189cm\npid:982271041 ecl:brn\nbyr:1930 eyr:2030\n\neyr:2026\niyr:2012 hcl:#cfa07d cid:59 pid:105862717 ecl:blu\nhgt:159cm byr:1943\n\necl:hzl\npid:604172804 iyr:2016 hgt:174cm cid:79 eyr:2025\nhcl:#733820 byr:1994\n\niyr:2011 pid:452628771 ecl:gry hgt:182cm hcl:#623a2f\neyr:2023\nbyr:1986\n\nhcl:#341e13 iyr:2010 byr:1946 eyr:2021\ncid:350 pid:049684494 hgt:180cm\necl:grn\n\niyr:2020\nhgt:173cm pid:384503937\nbyr:1986\nhcl:#341e13\ncid:113\neyr:2025 ecl:amb\n\nhgt:180cm byr:1949\nhcl:#733820 iyr:2010 eyr:2030\ncid:123 pid:065609606 ecl:oth\n\niyr:2010 eyr:2028\npid:231750173\nhgt:63in ecl:brn\nbyr:1948 hcl:#18171d\n\niyr:2020 hcl:#623a2f\necl:gry\nbyr:1922 pid:961213634 eyr:2022 hgt:177cm\n\nhcl:#18171d eyr:2020 iyr:2014 byr:1983\npid:183568344 hgt:72in\necl:gry\n\neyr:2023 pid:102781246 ecl:brn\nhcl:#888785 byr:1929 hgt:167cm iyr:2010\n\npid:362873066 byr:1994 hcl:#de545f iyr:2018 hgt:177cm ecl:blu cid:86\neyr:2024\n\nhcl:842f2d iyr:1983\nbyr:1954 eyr:2037\necl:lzr pid:3915492573 hgt:166cm\n\necl:grn\nhcl:#fffffd iyr:2014\nhgt:173cm\nbyr:1939\npid:930650489\neyr:2025\n\neyr:2028 ecl:brn hcl:#7d3b0c hgt:166cm byr:1938 pid:992958531 iyr:2011\n\npid:101149939 eyr:2024 iyr:2018 hgt:165cm\necl:hzl\nhcl:#ceb3a1 cid:176\n\ncid:62\npid:651390352 hcl:#efcc98\niyr:2018\neyr:2027\necl:brn\nhgt:66in byr:1953\n\nhcl:#623a2f byr:1978\niyr:2013\nhgt:180cm eyr:2027 ecl:amb pid:836425641\n\npid:557464096 hgt:155cm ecl:blu cid:142 byr:1936 iyr:2010\nhcl:#cfa07d eyr:2027\n\necl:gry iyr:2024 hcl:#341e13 pid:442593279 cid:314 hgt:186cm byr:1960\neyr:2022\n\ncid:123 iyr:2014\nbyr:2000\npid:878733390 eyr:2021 ecl:hzl hgt:162cm\n\nbyr:1959 cid:259\npid:722895016\necl:brn iyr:2018 eyr:2027 hgt:185cm\n\npid:163697814 ecl:hzl\niyr:2013 byr:1932\nhgt:68in cid:286 eyr:2025 hcl:#efcc98\n\nbyr:1927\nhgt:72cm ecl:oth\neyr:2021 hcl:#99e959\npid:669724466 iyr:2010\n\nbyr:1943 iyr:2011 eyr:2024 pid:384419879 ecl:hzl hcl:#7d3b0c hgt:170cm\n\npid:137944386 ecl:gry\nbyr:1953 hcl:#733820 iyr:2013 eyr:2025 hgt:184cm\n\niyr:2017 eyr:2023 pid:288078785\nhgt:179cm\nbyr:1993 hcl:#602927 ecl:hzl\n\necl:brn\nhgt:187cm eyr:2024 byr:1971 iyr:2020 hcl:#b6652a pid:622975646\ncid:290\n\npid:371817422 ecl:blu byr:1964\niyr:2018\neyr:2021 cid:176\nhgt:153cm hcl:#888785\n\nbyr:2002\ncid:256 iyr:2014 eyr:2024 ecl:blu hcl:#18171d hgt:187cm\npid:050022911\n\nhgt:178cm pid:211144001 eyr:2027 iyr:2013\nbyr:1947\nhcl:#7d3b0c ecl:grn\n\neyr:2025 ecl:blu pid:046417901 byr:1950\niyr:2015 hgt:165cm\nhcl:#7d3b0c cid:128\n\necl:hzl eyr:2029\niyr:2015\nhgt:171cm hcl:#341e13\npid:561680375 byr:1997\n\nbyr:1948 iyr:2023 pid:17288381 hcl:6a34a3 ecl:#671ece eyr:2001\ncid:152\n\neyr:2036 hgt:141 iyr:1957 byr:1987 hcl:z\npid:86986187 ecl:utc\n\neyr:2024 hcl:#b6652a iyr:2017 ecl:blu byr:1988 cid:348 hgt:152cm pid:068684272\n\niyr:2011 pid:989825275\ncid:78 hcl:#341e13 byr:1983 ecl:blu hgt:158cm eyr:2020\n\necl:grn hgt:187cm eyr:2027 iyr:2015\nhcl:#866857 pid:240650427\nbyr:1940\ncid:91\n\nhcl:#888785 cid:185 byr:1925\nhgt:155cm iyr:2015 ecl:blu eyr:2027 pid:851697441\n\niyr:2016 ecl:oth pid:056439470 byr:1985 eyr:2026\nhgt:154cm hcl:#282539\n\necl:hzl hcl:#a97842\niyr:1944\npid:118846711 eyr:2026 byr:1922 hgt:185cm\n\niyr:2020 hcl:#733820\npid:854531642 hgt:165cm\necl:hzl eyr:2022\n\niyr:2014\nbyr:1957 hcl:#7fa674 hgt:189cm\neyr:2023 pid:740871941 ecl:brn\n\necl:amb cid:349 hgt:170cm\nbyr:1952 hcl:#ceb3a1 iyr:2020\neyr:2026\npid:730499325\n\neyr:2027 ecl:amb\nbyr:1975 pid:985687961\nhcl:z hgt:157cm\niyr:2013\ncid:133\n\necl:blu\nhgt:193cm iyr:2015 hcl:#10f2ba byr:1989 pid:939704495 eyr:2021\n\necl:oth eyr:2025 hgt:69in iyr:2014 cid:258 pid:477970733 byr:1984 hcl:#b6652a\n\nhcl:z byr:2013\necl:zzz\npid:1904741884 hgt:180 cid:138 eyr:1985 iyr:1935\n\neyr:2025\niyr:2026 hgt:190in pid:#43ca33\necl:#3e1ef1 hcl:#7d3b0c byr:2030\n\neyr:2029 hgt:191cm\nbyr:1986 hcl:#ceb3a1 cid:327 pid:795060714 iyr:2012 ecl:hzl\n\neyr:2025 iyr:2017 ecl:grn\nhcl:z\npid:8886398 hgt:174cm byr:2016\n\nhcl:#a97842\neyr:2021 ecl:grn iyr:2013 pid:565234133 byr:1998\nhgt:161cm\n\neyr:2029 hgt:163cm byr:1933 cid:86 iyr:2011\necl:grn\nhcl:#fffffd\npid:818769307\n\nhgt:190cm eyr:2030 hcl:#af5b5d iyr:2011 ecl:brn pid:359524299 byr:1969\n\necl:amb iyr:2011 eyr:2022\ncid:141\nbyr:1978 hgt:69in hcl:#fffffd pid:013006109\n\necl:blu hgt:164cm iyr:2019 eyr:2027 pid:899103430 hcl:#cfa07d\nbyr:1976\n\neyr:1938\necl:#a03c41 pid:708735698\niyr:2030\nhgt:184cm hcl:#b6652a byr:2013\n\necl:hzl byr:1997 hcl:#a97842 cid:60 pid:172cm\neyr:2023 hgt:161in iyr:1936\n\necl:hzl\nbyr:1938 pid:094889181\nhgt:162cm iyr:2020\neyr:2028\nhcl:#623a2f\n\nhgt:162cm cid:86\nhcl:#623a2f pid:738174580 ecl:brn byr:1980 eyr:2028 iyr:2014\n\nbyr:2007 hgt:150in hcl:z\neyr:2032\necl:#114f3b\niyr:2030 pid:5129772\n\necl:hzl iyr:2017\nhcl:#18171d\npid:696283412 byr:1976 hgt:168cm\neyr:2028\n\neyr:1922 ecl:#84b0d4 byr:2013 hcl:#ceb3a1 pid:150cm iyr:2030\nhgt:71cm\n\nhgt:164cm byr:1949 ecl:gry eyr:2026\nhcl:#623a2f\n\necl:oth\niyr:2013 hgt:166cm hcl:#50e385\npid:478852286\neyr:2030 byr:1930\n\ncid:129\niyr:2020 byr:1978 pid:907580992 eyr:1955\nhcl:#602927\necl:grn hgt:165cm\n\necl:blu iyr:2018 byr:1953\nhgt:177cm pid:126681706 eyr:2025 hcl:#c0946f\n\nbyr:1984 pid:275799917\necl:oth hcl:#623a2f cid:348 iyr:2020\nhgt:189cm eyr:2024\n\niyr:2016\necl:hzl byr:1954\nhcl:#623a2f pid:810508839 eyr:2026\nhgt:185cm\n\nbyr:1967\neyr:2021 hcl:#ceb3a1\npid:406634908 hgt:158cm iyr:2018 ecl:hzl\n\niyr:2019 eyr:2030 pid:995681076 hcl:#341e13\ncid:101 hgt:162cm ecl:blu byr:1925\n\neyr:2026 pid:272513479 hcl:#b6652a byr:1973 iyr:2016 ecl:amb hgt:182cm\n\npid:298704871 eyr:2024 hcl:#efcc98 byr:1959\niyr:2014 hgt:191cm ecl:grn\n\nhgt:193cm pid:750729809 ecl:oth\ncid:324\niyr:2011 hcl:#efcc98 byr:1954 eyr:2020\n\nbyr:1966 iyr:2019 eyr:2025 ecl:#2df4b6\nhgt:184cm pid:#fc17f4 cid:161 hcl:#602927\n\nbyr:1955 hcl:299464 ecl:amb\nhgt:157cm iyr:2017 eyr:2021\npid:239450987\n\nhgt:172cm\necl:hzl\npid:839804598\nhcl:#341e13 eyr:2030 byr:1964 iyr:2013\n\niyr:2018 hgt:152cm byr:1948 hcl:#623a2f pid:400121515\necl:blu\neyr:2020\n\ncid:296\necl:grn\nbyr:1960 iyr:2028 pid:#1f4b95 eyr:2033 hcl:#602927\nhgt:66cm\n\niyr:1933 ecl:#232e20 pid:#d03ca7\neyr:2030 hcl:598ed6 hgt:154in byr:2011\n\ncid:247 ecl:grn iyr:2014\nhgt:178cm\nbyr:1992 hcl:#602927 eyr:2021\npid:678964478\n\niyr:2010 pid:623705680\necl:hzl hgt:181cm byr:1980 hcl:#341e13 eyr:2028")

  (time (part1 puzzle-input)) ; 230
  (time (part2 puzzle-input)) ; 156
  )
