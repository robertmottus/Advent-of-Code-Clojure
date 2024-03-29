(ns year2022.day03.day3
  (:require [ysera.test :refer [is=]]
            [clojure.string :as str :only [index-of]]))

; https://adventofcode.com/2022/day/3

(defn find-common-item-in-rucksack
  {:doc "A given rucksack always has the same number of items in each of its two compartments.
          Find the only item which is in both compartments. "
   :test (fn []
           (is= (find-common-item-in-rucksack "vJrwpWtwJgWrhcsFMMfFFhFp") \p)
           )}
  [items]
  (let [half (/ (count items) 2)
        compartment-1 (set (take half items))
        compartment-2 (set (drop half items))]
         (first (clojure.set/intersection compartment-1 compartment-2))
         ))

(defn find-common-item-in-group-of-three
  {:test (fn []
           (is= (find-common-item-in-group-of-three ["vJrwpWtwJgWrhcsFMMfFFhFp"
                                                      "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL"
                                                      "PmmdzqPrVvPwwTWBwg"]) \r)
           )}
  [groups-rucksacks]
  (->> groups-rucksacks
       (map set)
       (apply clojure.set/intersection)
         (first)
         ))

(defn item->priority
  {:doc  "a through z have priorities 1 through 26
          A through Z have priorities 27 through 52"
   :test (fn []
           (is= (item->priority \a) 1)
           (is= (item->priority \z) 26)
           (is= (item->priority \A) 27)
           (is= (item->priority \Z) 52)
           )}
  [c]
  (if (re-matches #"[a-z]" (str c) )
    (+  1 (- (int c) (int \a)))
    (+ 27 (- (int c) (int \A)))
    )
  )

(defn part1
  {:test (fn []
           (is= (part1 "vJrwpWtwJgWrhcsFMMfFFhFp") 16)
           (is= (part1 "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL") 38)
           (is= (part1 "vJrwpWtwJgWrhcsFMMfFFhFp\njqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL\nPmmdzqPrVvPwwTWBwg\nwMqvLMZHhHMvwLHjbvcjnnSBnvTQFn\nttgJtRGJQctTZtZT\nCrZsJsPPZsGzwwsLwLmpwMDw") 157)
           )}
  [rucksacks]
  (->> rucksacks
       (str/split-lines)
       (map find-common-item-in-rucksack)
       (map item->priority)
       (reduce +)
       )
  )

(defn part2
  {:test (fn []
           (is= (part2 "vJrwpWtwJgWrhcsFMMfFFhFp\njqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL\nPmmdzqPrVvPwwTWBwg\nwMqvLMZHhHMvwLHjbvcjnnSBnvTQFn\nttgJtRGJQctTZtZT\nCrZsJsPPZsGzwwsLwLmpwMDw") 70)
           )}
  [rucksacks]
  (->> rucksacks
       (str/split-lines)
       (partition 3)
       (map vec)
       (map find-common-item-in-group-of-three)
       (map item->priority)
       (reduce +)
       )
  )

(comment
  (def puzzle-input "QLFdFCdlLcVqdvFLnFLSSShZwptfHHhfZZZpSwfmHp\nrTJRjjbJTgzDJjdsRsfwtfNwtfmZpZNhmmzt\nbMdJjsjglnVMFCCc\nBZvZMBBBMTtZTgcCPdgtgQCrrV\nVspNDDpsGDGnRmRpRplQdrrPcPCjgDCcPQCQQj\nRVnsmspnpwFRlHGRwHHlnRSThSSvBTbFTZMqTMZMTZFh\nttffrVJWtWpgtQnZGVnNSLTHSZ\njRsjjmhdRcjcRsFcdwGLMZSnHMTSMNZN\nRjczlvjhPCcPjcvRpbglWplJBblqrGgq\nNwCWwdNQNDCwGpWNQwmJtZgPZrdJgLZcPhLddr\nblqpnFTqrLbcLPtV\nMnjqSSpqlFRvSqNDGHvWHQDwfWmm\njfLljlQhDLmlrMJQtJTJrQqQ\nNpBbjjsdMCgCCMrb\ndwspwGnSHHGsGzDDlFDjVWjfZWnP\nwQhTZwvpZFZdqWLnnwSrWC\nmfDmMFlDcPLdgDSCLCqg\nPmzclsMclMlFsHHsJZFHpT\nLfLJWNdJnBLfhndfWdZqcgDZgSqgCCSSSLDF\nbQVQmrrjPqQDZSZBCQ\nRRtGjVmRsPbPrrnNNpNHHnBnpHns\nPfbGNwGBwNcPTbGNQFBVjsjztVztVjHV\nhrdCJhmlJhZrLDRmghrmDrzqFsbgtbHqnznzznQtbjtz\nWdZdDJCDmrJmLZrLDLDZlClcSccwSPbNPfSNWfGNNWMGNc\nQwrnTSgqgFShSdfHPdbS\nBGdjmMmZMvfhvCZZPf\nBzGmzVGGGzmzGpVBtdnQqqdTQQDDqrpR\nPPRPwLQlLtbPmbwgJhrSssNlhhrgsZ\nfFTdFvTMNfzVnFqdnjgSSjjsSjhghpJs\ndvczcFzNTWVWMFLcLbQQwmbHLCLL\nHhLLDfMmTjsjmLmhsmmfZMjGtpGJtdcvnCWtZJcWGddttW\ngwrwwgzwgDpJddrJDr\nSBwBBbgVsVmRRhDM\nSZdmfdZRTQZTQgHVVGGRqZdVCjCcNCNcJRWcCBbJjCPCsNsc\nFnhzMMhDDFlzlnvpwMLrMDCsbcjNJbcJbBcBfPhCNbWj\nwzwnpDDnLvFnLlttLrzGgTVGQqZtTqSqSdfZTg\nFJJWWWrCGWdmzFlTVqqlMhmvVlNh\nbtpgtfjZjjhgggrNMThl\nDpwpfRZDZwwfwjsnjfsZnnnwGcCRCHcCCCLGHLWcrcFCWCHW\nljHHHBtjQthchhZpqqNt\nFTmJnPFwzlJPmzTgTgbFwJbMCpbMchhhqbhWCDMDhZcppM\nJwlFGwVGnFFGBGjdSsfdsG\nQsvpGhjGgswvjjwjjjvPpThJfLZCCLCSSLbFLStCFCSgtH\nmDrzdzMqqnMrDMmZnqnzNVRCStlCHFSCtJqlCLFCFLfJfJ\nmBDzNRWBDDMBpGsZcGZWGjPp\nSlLQhQsvvttFlWsWcfHHMTJfwSHwTfTf\nVZmmrRDRfpTHJcRf\njzBnDDgjPchWlsQsBW\nLTLVdTSLNTLnTSnrWvdwswsfmJwmwGsffH\nFbgCbRRppCpPbgMcZvCcGftGGltwHwGtplQQsfJw\nCRBMCvZZRgMgBbDCPcDrjLzWLVrSSzShSSNrBS\nhVJJjhjRVRZjQvDfBstsBVNBdwstHsld\npCTCcMqCThTFLFFPWcWSPHtwwdmcBHHmNtHdmwmwBl\nrMTCCWPLLPCMFhDnDrjzRrfDJD\npqMpCvMchvFNWSBdVNqQ\nzDRJJDGJJtNtmGRRWVdFWWVdSfjb\nDDJLmnJmzwGmGLTPhTCNpgcrpv\ncpPpbPWVprWcbJrrwpCwwdWrvNNFRqzNnChgqFzFnZvqFlzq\nfTtHLfSHSsNDGLSmsLvnhFqzhzlzDhhvRlFz\nmSMLHTQTmHMSfBSMTPdBJJNNVddrVrbjbJ\nzpCpBTnFgFbncznbblzdhRswdlJsLllJdw\nQqqmtWVPWvHDVmqDhjsljwRhlZldhRMQ\ntWSHDmVfmrtPHVgGRRbgTRpSgpTc\nssTbzFRtPRwTFZtvbPRMhndBqMMvMBHJnnHMMd\nWQVWzlGWVBqqdMQJMq\npVSpSSgLfjDzWrLGWWjDzzfLtbRFFNtPZRssspsNRZcRsNZb\njnPzzGlnnznWnzhvGnnpVFrZmVFcgjrrmZRFtj\nfsbgTdwdqBbfwCptVtdZRcrRCp\ngsMbgfHsBSwsGhhJWMLnWPPJ\nbpmbJpNbbbNGGmRJzJTsfdsvsNdglfhssCvC\nhWLwQjZjLhjHFFBLZldvvflrvtfjCrfjlT\nQWVQZZFDcDJJhJJc\nRmRghgRlNgfGGRmdGqhsgsZFZZpBvHpZppHcgH\ntbLCDLnLtSbbbjtPtMLtDPTvHHBpcHcsHvTcHsmZcF\nrSLrMJzDznzGmhNlVwdrRr\nvWjljMWcnSSpjmzbJVzJrTCmtGJV\nNZDDQLRqPJrPzrprTC\ngqDqqwpdHWhlgnjH\nccptcpstDvbNvHbLNRZZ\ndFjhdnjQFJlFCQSjgngJPJgWWrRNWNRtNCzrVbRzNVzZZL\nPhThFSPPSpsmTcqMwt\ncLcLlMhGMGcpGzslHFHFvnHlBDvWbT\nVHdQwqPJdPwjJPdPQRrmjjjQnTFrbvNWFFffDvbvvNDvbBNN\nRwRJCHmmdQJZZLzGphcCtz\nhVvFVjvjVWmFRQVZqTpqtwQpwpqZfp\ngvDlSBDJSlPLcLdDwzwtptqTTTzwcCCt\nJgrJGbLgvnWsmvVr\nrwmqqRqrnHQGmnjCCqCzdBzCBJBz\nhFLgbWWPWmvtLhPtgpcdjJdcBJdpJjDsgp\nlvfhSSPWtTNTTZZmfr\nbHDDssRHsjNMbJjJLQJsbTtGvSCzCGQCTzGvSqSBzT\nmmVrwhmmpfPStnTSBnhStG\npcwrptZcgFcmpgHRDjjZlDJsjbbD\nJJRrmFqJMdFFJMjjJcqGgzSCSHSCscPCHPHGZc\nVWpWptnvSmpPGCHC\nvQnDLBmbntvLBbnlldTQFlJlFFrNRd\nLPDftnHFQfwmBcBGmc\nCVqRsdqvdrlsCVsNvqwwSpTNSSDSDDBBTTSN\nlqlDRddjbblRbRqrlRRjsbvghHHnPQWjHWQWZHPHWZhFnP\nbwQsDcgsJqcsDpcQRQnpqtVSVvgSMMMfMvfVBVfdvM\nCGZFrHHPrTZNGGZZHmCZHlVfjfzjSfzBtBBNSBVjvntf\nChrCCLGrTlhJnhDncRbp\nnmFnhfTQjSzfjddZWsRRRFRFGl\nHDgCwgtQbZlHsrqHHr\ncJPCgCCPbpbgDMPvMQnjmnhTfmzLpQQmjz\nQFHSQdNMCSgcSgFtttPNFJpCpnTjZlbblpppZplZjz\nLqLsWMRRfrrWMmMGpbTnbppbZnTjpnmm\nfWBrMqWsGswGfGRMMwrLgtPPdNFBQPHNHNPPcFPP\ndngbSppJSSppbVMZQQMjqfQQgwcl\nTWmSWtvCRCWjwfjqQqMstq\nFhFvRzSTNmhHnVPhGhBJdBpB\ngcHPgzGmPPwTsSTsbwbdWD\nQjBLLfVhhBqqBFQLrLjVFlNpNDtsSWTDdNptdbqbdS\njCMFLVjFBFjJJLFFMVBFrLnvPzHRmHPGnGWWcCvzHRZm\nPDPqWWjhPpPbCsjwjTVbLT\nSrtCttGRMddSVwHFSs\nJtfvttmrGMRRJzJCqhqqqWQZhCNgJZ\nChrCVFQCVQlwQNwpQcmmcjmWBmddghjjdW\nsbDTZStTqqfSBggPmWjWWNsL\nTHqqSHDTtZTDTHZZbHTzRzFvlFCVprFprQVnCzNppQrz\nPdfWCwMWjPSrdgCMnnlGsGQvvpJZvFGnps\nDmBhVBLbbVqVBzTRLBRzzTLNNpRQNNZQZppZvlQpZvllvF\nzVDtVHBbbTbzDbrjgWjMPtMWPMlj\nJLsTTNDsgTMNvDQpLpGpLGNJShrfzCFnSnSrnfzCfTFhWrfw\nZcqrRddHZZVRfzWnVWCzWFnn\ntZHZtrHHPdRtdHlcccQggsplpJDNvMGNGMss\ncMCLfStfMTCjPMPcGzjftMbgsRNmRgmmGsmnJbNJbghJ\nQHVVWrFFWZNShHSgbSJm\nqZwwrrpqpZpZFvqrQdFlQVSwLBMBfTTLTjLBCcdTMzMftPPB\nSwsdBTvgvJLPNptpCpCmBDtn\nwffrzwGFWFNZWpjWZnNm\nzrfflbRwJPhbPbsS\nHjHHRtwjnjRblQRttHwQGvGWNNBWvqGzfTvfNN\nFmScCcrsdVZrpBrVcCVFzffvzzmWGLWgqWqgGWzW\nSFVSDDBdsdDSJhnjJltJbPtHRM\nFjGFVqWrzQFlQrZzGQzFLTvfwwTgMnvcnbRMLRdnfb\nCCttSNsSnRfgncSg\nCNspmDBPtPmJJNBJPNpDhQZVzQlhqrGZflfVjQFrQj\ndNNdHWcmdmPPptmmWHpPTFFjJPGrQsVsPQGGGJVDrVVGrS\nMhZlZhlgflgfnfDtjbjJGbtnVtGS\nLtZqlzhzqMZWHHLwdHmFWp\nllNRlfwWRwwLlwFNNgRrVCBjdjCVdjpWjtVWCD\nHTQqzPqzQPmhhmSPznSsssJtdnMZddtMCjprtMjCnBVnjZ\nPzHQmqsGSJPSmQqPbfwNcgNbNgNfBGwR\nlPdzlZPzQzMZQGQrTZvvpjHTTpfsTTZb\nzRShhtWRnqnqSNRnDTTHvfNJspNsLpTsjL\nhBVncVtDSnhDnDBBtGrlzwmmMlGmVrMdrP\nHPTZVHVPlHDPlfgnjJFdJdjPjSPqCS\nhLRRBhwGhqbtmsRSSSjjdMJjnJGSMj\nQrQtqrRrcQDgVglc\nZTwbbZdchZZjmVWHTrHWBVJtBB\nglslCDqLLDfGRqlsgLssfrCHBHFHmrHBBppFmCJWWp\nfRzvvvgGgNSNvmQbSQ\nqPGGPwCTqTzHCvPGqWdLFLssLpstLLspvd\nnjJchhcbjbDrbcLNlLrpWWrLLHgp\nDQhMMMJQMQJnVbbnRHSMPwZmGZPZRTRCwTZmZGwz\nzzGNfPbcgdPqLrqvWWVzMq\nDGmJtnJTJRhhJMhCQqCLCLrrLM\nZnHDtSZlTBHnBdccGSfGcwjjdb\nFpZDpQZDvMwZpCCMdCBPpJGPPLgJGGLffJJL\njlbswNrlPPJJfGlf\nbnNwqbHnNwRSrqhbdCcmHddQzddFDdvZ\ngbQQQngWPVVtvvPQNVNvWWSHGwDsCCmDtHSlmrssDmHs\nfqhMLFFMMZqZMRZqMjRMqLJSCdFlrrldsDrCsDSSHHGCSC\nMJRZLZLGMcTqczjNPzNnzPvWBVgNnP\ngqdbBffTvlRHbwLl\nnMMQJQpGdsFpQsJzNMRLLDlmLLmjFFmLjDRF\npzGMnVcMBfTdtBBV\nWSbfmrrrrWdbWmdfDSSStmHjtMtvCLVnqBHCVGtVGnMM\nlRcgFRZhJgnMLjvGgv\nlcvwTcFTplvwphzcTTJTbsdsPSPDdbmzmDSWPsSm\nbbdTjTQTQMsZNqqhJrZslg\njFGVjwfCPVGfwjCVqWhWZFgqWrglllNN\nPjfSPzRBjCCfSBCGBLznTndHcdMLbMmmdT\nwSVMJSVccdGwGnsgbVTTbRsCRNgN\nrHjhHLmrhPJrqjNTRDgBbbRRRs\nzqmPPqqpPLzltrMdJcZpfdpGWWJJ\nZhrBBJGrgJhGHttGGVPPcPPF\ncnzLqNssfRnpfWqsLfcfWQNMbMVPDtnDtbHFtMbPtVPFFM\njfqzCCLsWQLcjgldjmljmgTd\nwghGSSGZPVwgqtwtwCCtFFMM\nBvbspnBznvvWHWHHHbCQptQFQlFcqMClqLLq\nJWzzsJHWzfWjJrvMBWHBBGDmVDrVhZmmgSPSmZVVrh\nccRMJRsjjgJgcPCSCCVCwsSWVNzp\nWQQqnmrBWtqWqdSbVwwBSpbbCSBB\nQvDqmqqmgWPWjPvW\nmsqpjDWspRWwvFvDWWhnbbJfPzFQblJJPlnz\ngGGrMTgLVBsBBLdsVTrSCBffHQfdHhnbPPPPffndlbzh\nZCVsCGSScsLZpwNpqmZRqW\nPPsGmJPVPQPZmsQCVPJPnPCMDcTcdqDDTqvFhvnTjRDTDchq\nBdrtzNBLHStHrdrlwfNThvFhcvbDccThjbFBqq\nSSgdHNfSHHgzLHtLNWSPQQPMQVpmmppVCmQZCW\npPssrWWLdndHPJdd\nQNQFTLNBFTzzgjfGTjffFNZjCSGnHDnSDJHnDScttDCcDnmd\nFVzVLZwZZgswqqrbphbR\nVpWCZjCwWnppZpqnhNjjNZjFLtLzQJHdHLQRzWLRzRztHJ\nDMGPmPMgTSmsgQzRFbdHRLJgdn\ncsDMPMGDDvMSSPnDTvrDChhwljlqNNjchNCjNVcf\nWpGGmbSGpVWWpjMMTNdfCFNdFfRNwNSF\nJsQztzrvrJqsTTRbbvFBhhhv\ncLrDqLccsLqbDHGpZWDHgjGlZW\nQGMQJMmsJmMCmmqjsRvLvvdgvgVvDVdD\nBDcrcNbNppwTpzRdvvchhFvfFv\nplBBwWrbpQHDjGmGJl\nmzFlTdmSDzrPvCJqqDVVNC\nhfRmhgjRhnfwnRHcnhGGvPJQPvvfLfQvNLGv\nBhhnjMgRWghpwjRWMRjrZzdbSbsdstTrltdmMs\nbLLnbqjpvplnDvNlqpqBWJZSdPJCNdJJThhSPhTd\nHFwHHQMMFHGzGwRPPJPTWthTZtJSQr\nmfWMHFHWHmgmFcwGwwpbDljqjBDcDnLcVnlb\nwBrWBwSWRJMBwdZnPQPgFnwGVF\nfLjfbsvDDfvvqqGqZGqmPQgqTGGG\nvZLsjzjjZCzJWRNSBR\njTRbRHHqPqTRBHqdjhgvgghhZQdDvvgvhC\nWLWWzzFszsmNFGWSFmMrpghCtZvhlQNDgQCDgctC\nFJsLsSrDmsFSDLWrzJmmMsGqjRBVbJTBVPVBbBqRjPBjHn\nQbwwnDDQDcDfSbDbfhhrvrCtJMvJSCvvJh\nFWRjjLjmdZWdWNBFNWNlNQQrMGvvMGgssGvQRvrMJs\nBjWdlBpmdmBWFWdpWfPfpVnVwfHpqPQDbq\nSqrvlMldqvSWdGPTGzWpWpzpHP\ntRwmhtbsRRFsLwGGTVDHppTNdbVp\nFRCRQdCFtCLmBhCcmmQdhFdCvnfjffjZlZnjSnvfcSrrgMgn\nGQQtNJQWWcqPPhMMtwqD\nWpWLlBWZCvhjwMMZqDDP\nWgvmLVmHCbpppLgdllHddvCmFGzGnfsJJQJsJncSsccFVffF\nHcSsSlTTvvPPWWNMWWgPTPPbGbbrwJQbrrDphrHJJRpRhp\nztfLqqzmRwDGlLDb\nfdVtmqjdZBmSvjsPSWlTgv\nDPvDhhMRRMhRNDLPMNsbwHwrjgnddqddrWdPtHzr\npcBGSpcVBfJWCcmJGGwHtzgrrtwqzdrtrngG\nmllBlBZmMlQWRbQv\nSGZBSFMZllJWmzvfpp\nNTqbNrhHNHWgNqHrNhNQbbjHJLcnfnzLLnLmfcfccJcfQLcL\nHggbTNRRTHqqbVSGMSZVWDMDwVPs\nSBsSlvbPlFPvRlbPsMFZLgVLrLsJVgzrCJfVCH\njcNddNdGzZrVgNVJ\ntTGwdcmWGdtwQmwmwZdwSlhBPbhPTBFRhlhSMFMR\nRzStzTzzvvQvSHVvhVgBqMMFqhPM\nddlLLwNVLWLjbbLrjrbWrwmlhcFmBGgFMMPgBcGBBqPhggMs\ndLwdVCVWWdfNwNwLrWrbfbJNptzDDHRnHptHtznHTppnQCtR\nRzcfMBHLzpDQFmnDSWNB\ndbqjtjVqJZZGjPGJCPGbPndNNDglrmQmNSDgSlSSng\nhjCTqhCJbhVCGNvMcfvhfRLhvchz\nsDDqDMtqshJhPvhhCpSCCWlZHSWp\nbffRcbBGGTwGfGfbNjgSHZSgWwplHCClZZ\nRTQBbcnbRNmGbGTQLbmbJVqLllsDVMsPDVVvttMd\nnbLBjnqwgfRRBgBwnllbLlwScvPdZPcScZPcdFZJPvZPvcMZ\ntChQpphHrrHztssZdcDJcPZcMvWv\nhpTHVMQMtQtVpzBfwjfRnfwfnjVl\nljJlvvJQlrlcJcWpPzgthnPnzMgpgSpC\nsmtmZBmHZTVttHmqFqmzCSZSdndzShPNgPShgP\nbVqFHLqLqfHHFwbBLHcwDQrDrtjlQvGjlRQQ\npwhVsPvVVCFtmhPhzqGqqZMZvGTTTMlGWM\ndrrrrDfDRrNQdQdrRrBdjGWqWqWlGlGtlGbGZGBTLc\ntSDfgnHrdDtVSPSshJCSPh\nWlWlDqhglLhsdgrcbFdJJpPpdBbB\nZQZvSvzRMSzjZjvZmMMpbFPQFVBrVbPcpbJFLB\nSwGZmjvCRSMRjMzZvRnstHftNfswHsflLhNWHf\njsprCvGRQrtjCsQrGsrzvGHhgmHVmHZgggmMGVmhMbHm\nFFFdDSdwSffJWqqMzzMmDVbZ\nLLcdcfcfPwwBzdTTdtvlsrjCtvPvprnsjR\nMvtSqNSWMzjwzFTD\nZRPlcRpQszNgszNwVT\nbcZcrcPlcPLLLZllPlbcbLSBfWCvHvWWNSmSqNqfWN\nrNdZpMGnddgggwHwzRPCzDDD\nvcvhcTLhZLhLPCPHPDPPVvzH\nLTmBmthWBchWLttttFJFLlFnGJNsfpdjNsnMnMpnpZdssn\nZHWFCvqBDdqqqCTDHHBWrgppTMhhVpspMPQcSgQVPS\njblbGffntRwltfMQVrrQscphfg\nztJrGtbwGztbmtzzRGnRznWNNCWmHHdFHdFNWWHHqCqZ\nWGWSSZvVvqmrmzPm\nNgjtwFFlwDsFghNsMtlcjljcPqrQHcZzQznpQQprnqqzHQ\ntgMCwNhtgbdLZRbZCT\nPQSPQrSGZnGnVFhpVhRRlvLvBDRV\ntjctcjTMMpDTvFTlRD\nJCftsccFCcmsJJGZGGmPHnQrGwGS\nTrjRFFRnpnRCHNFSjSRrffJvJfzqQBsjqQqzzffd\nZtlgMDhZhgmGDLVZLlGtLPqdQQvvfBJJqzzBPdMzdd\nVlLDgLLDWtGZwgtRNTNrFTqCwqHTrr\nLpcDFDMMPjMLLjpcDGCHgHssGHWnbCBWBHvm\nQfZhrhVVdZThlZlfVvVzZrTbgQnBHsCCHgJBsCsJBHmBmn\nwwtvfZztlTVlhtrzzlLNpFFRjMPDpRcPFwRj\nVzZhhQHQJJWJSSFWDGclbmNPgglPgVGc\nddBTqCjjBCcrqrCRrwGPGmmDGmbpBGNpNNgg\nCRMjwsjwsLdLRrQFJSvMFMWZcHFW\nJgJJPvtrhRPQQzSRMQFFSF\nBLqsjsdLsMBqblnsGbBqVqdwSQSCSWwNFwczQWCNNwNCHn\nljqbpLbbdDlbDbqDDVtMttTTgpJJgThhJrJr\nnflndmjbSnlTQGwvWGPHGRGj\nNtstcMcDJMvwgHfFvDgR\nqqqpLrMsLLqLNNnzbrdlbZSrznfz\nttZCCFjNjnPVCFQPPFbbStrzqzqrrrcwtmJJ\ngTTMRMTWsTGGTddHTTbBzBLSmqbbJGzGmqqb\nHpgpMTvRhHHTRDhMsHdHDRhjJlVPJjNFJnnFpQQVfPCjnP\nVqJVQPpjQqPBbHwldmLfVVmd\ntMGvrzzDGCDDddwLbgLvLwcm\nTWDWCzTZDGMZtzWWtsFhbRRqRQRjhbNQBBTh\nzgLgLHnnzCCvnsHSsZBZBsTRdD\nrslllhJjcQNNGjpWJlSRTRdwBVSSNTPVSdPB\njGrGqjJfqccrfqGcGplrJpFvzggqmCtMzmsMnvMvvCgm")
  (time (part1 puzzle-input))  ; 7917  (20 ms)
  (time (part2 puzzle-input))  ; 2585  (10 ms)
  )
