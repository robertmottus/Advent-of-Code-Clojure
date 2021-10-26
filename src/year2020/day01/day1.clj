(ns year2020.day01.day1
  (:require [ysera.test :refer [is is= is-not]]
            [clojure.string :as str :only [index-of]]))

; https://adventofcode.com/2020/day/1

(defn get-2-factors-and-multiply
  {:doc  "
  Find the two entries that sum to 2020 and then multiply those numbers together"
   :test (fn []
           (is= (get-2-factors-and-multiply [1000 10 1020]) 1020000)
           (is= (get-2-factors-and-multiply [1721 979 366 299 675 1456]) 514579)
           )}
  [nums]
  (let [n (count nums)]
    (last (for [i (range 0 n)
                j (range 0 n)
                :when (and (< i j) (= 2020 (+ (nth nums i) (nth nums j))))
                ]
            (* (nth nums i) (nth nums j))
            ))
    )
  )

(defn get-3-factors-and-multiply
  {:doc  "
  Find the three entries that sum to 2020 and then multiply those numbers together"
   :test (fn []
           (is= (get-3-factors-and-multiply [1000 10 1010]) 10100000)
           (is= (get-3-factors-and-multiply [1721 979 366 299 675 1456]) 241861950)
           )}
  [nums]
  (let [n (count nums)]
    (last (for [i (range 0 n)
                j (range 0 n)
                k (range 0 n)
                :when (and (< i j k) (= 2020 (+ (nth nums i) (nth nums j) (nth nums k))))
                ]
            (* (nth nums i) (nth nums j) (nth nums k))
            ))
    )
  )

(defn part1
  [nums]
  (->> nums
       (str/split-lines)
       (map read-string)
       (get-2-factors-and-multiply)
       )
  )

(defn part2
  [nums]
  (->> nums
       (str/split-lines)
       (map read-string)
       (get-3-factors-and-multiply)
       )
  )

(comment
  (def puzzle-input "1438\n781\n1917\n1371\n1336\n1802\n1566\n1878\n737\n1998\n1488\n1372\n1715\n1585\n1676\n1810\n1692\n1329\n1916\n1854\n1307\n1347\n1445\n1475\n1435\n1270\n1949\n1957\n1602\n1931\n1505\n1636\n1539\n1803\n1011\n1821\n1021\n1461\n1755\n1332\n1576\n1923\n1899\n1574\n1641\n1357\n1509\n1877\n1875\n1228\n1725\n1808\n1678\n1789\n1719\n1691\n1434\n1538\n2002\n1569\n1403\n1146\n1623\n1328\n1876\n520\n1930\n1633\n1990\n1330\n1402\n1880\n1984\n1938\n1898\n1908\n1661\n1335\n1424\n1833\n1731\n1568\n1659\n1554\n1323\n1344\n1999\n1716\n1851\n1313\n1531\n190\n1834\n1592\n1890\n1649\n1430\n1599\n869\n1460\n1009\n1771\n1818\n1853\n1544\n1279\n1997\n1896\n1272\n1772\n1375\n1373\n1689\n1249\n1840\n1528\n1749\n1364\n1670\n1361\n1408\n1828\n1864\n1826\n1499\n1507\n336\n1532\n1349\n1519\n1437\n1720\n1817\n1920\n1388\n1288\n1290\n1823\n1690\n1331\n1564\n1660\n1598\n1479\n1673\n1553\n1991\n66\n1571\n1453\n1398\n1814\n1679\n1652\n1687\n1951\n1334\n1319\n1605\n1757\n1517\n1724\n2008\n1601\n1909\n1286\n1780\n1901\n1961\n1798\n1628\n1831\n1277\n1297\n1744\n1946\n1407\n1856\n1922\n1476\n1836\n1240\n1591\n1572\n2000\n1813\n1695\n1723\n1238\n1588\n1881\n1850\n1298\n1411\n1496\n744\n1477\n1459\n1333\n1902")
  (time (part1 puzzle-input)) ; 1020099  (60 - 100 ms)
  (time (part2 puzzle-input)) ; 49214880  (3600 - 3800 ms)
  )
