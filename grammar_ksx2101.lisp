(defparameter *grammar*
  '((Sentence ->  (NounPhraseSingular VerbPhraseSingularPresent)  ; sentence 1,2
                  (ExpletivePhrase NounPhrasePlural)              ; sentence 3
                  (Sentence CoordinatingConjunction Sentence)     ; sentence 4
                  (SubordinatingConjunction Sentence VerbPhraseFuturePerfect) ; sentence 5
                  (NounPhrasePlural VerbPhrasePluralPresent) ; sentence 4
                  (NounPhraseSingular VerbPhraseSingularPast))
                  ;(NounPhrasePlural VerbPhrasePluralPast)
    
    (NounPhraseSingular ->  (Article NounSingular)  ;sentence 4
                            (Article AdjectivePhrase NounSingular)  ; sentence 1,2,4
                            (AbstractNoun)  ; sentence 1
                            (AdjectivePhrase AbstractNoun)  ; sentence 1
                            (NounPhraseSingular PrepPhrase)  ; sentence 1
                            (AttributiveNoun AbstractNoun) ; sentence 3
                            (Article AttributiveNoun NounSingular)) ; sentence 5

    (NounPhrasePlural ->  (AdjectivePhrase NounPlural) ; sentence 3
                          (NounPhrasePlural PrepPhrase)  ; sentence 3
                          (AdjectivePhrase NounPhrasePlural PrepPhrase) ; sentence 3
                          (NounPhrasePlural RelativeClause) ; sentence 3
                          (NounPlural) ; sentence 3
                          (NounPhrasePlural CoordinatingConjunction NounPhraseSingular) ; sentence 3
                          (AdjectivePhrase AttributiveNoun NounPlural) ; sentence 3
                          (Article AdjectivePhrase NounPlural)) ; sentence 5

    (VerbPhraseSingularPast ->  (IntransitiveVerbPast)
                                (IntransitiveVerbPast PrepPhrase)
                                (TransitiveVerbPast NounPhraseSingular)
                                (TransitiveVerbPast NounPhraseSingular PrepPhrase))

    (VerbPhraseSingularPresent -> 
                                  ;(TransitiveVerbSingularPresent NounPhrasePlural)
                                  ;(TransitiveVerbSingularPresent NounPhrasePlural PrepPhrase)

                                  (HelpingVerb InfinitiveVerb NounPhraseSingular)  ; sentence 1
                                  (HelpingVerb Adverb InfinitiveVerb NounPhraseSingular)  ; sentence 2
                                  (HelpingVerb Adverb LinkingVerb Adjective PrepPhrase) ; sentence 4
                                  (IntransitiveVerbSingularPresent)
                                  (IntransitiveVerbSingularPresent PrepPhrase)
                                  (TransitiveVerbSingularPresent NounPhraseSingular)
                                  (TransitiveVerbSingularPresent NounPhraseSingular PrepPhrase))

    (VerbPhraseFuturePerfect -> (HelpingVerb LinkingVerb IntransitiveVerbPast PrepPhrase))  ; sentence 5

    (VerbPhrasePluralPresent ->  (HelpingVerb LinkingVerb Adjective InfinitivePhrase))  ; sentence 4

    (ExpletivePhrase -> (Expletive BeingVerb))

    (AdjectivePhrase -> (Adjective) ; sentence 1
                        (Adjective AdjectivePhrase) ; lots of places
                        (Adjective CoordinatingConjunction AdjectivePhrase)) 

    (PrepPhrase ->  (Preposition NounPhraseSingular)  ; sentence 1
                    (Preposition NounPhrasePlural)   ; sentence 3
                    (Determiner Preposition NounPhrasePlural)) ; sentence 3

    (RelativeClause -> (RelativePronoun IntransitiveVerbPluralPresent InfinitivePhrase))

    (InfinitivePhrase ->  (InfinitiveParticle InfinitiveVerb Adverb)
                          (InfinitiveParticle InfinitiveVerb PersonalPronoun PrepPhrase))

; THESE ARE THE TERMINAL SYMBOLS (WORDS)

    (AbstractNoun -> education polarization care religion progress friendship culture service)
    (NounSingular -> man ball woman table narrative strategy intent asteroid eye prosecution penalty case monkey cat student worker)
    (NounPlural -> men balls women tables gains industries services weeks astronomers telescopes teachers companies industries)
    (RelativePronoun -> that)
    (AttributiveNoun -> health job death government)
    (PersonalPronoun -> it)

    (IntransitiveVerbPast -> laughed cried sneezed decided)
    (IntransitiveVerbSingularPresent -> laughs cries)
    (IntransitiveVerbPluralPresent -> tend)
    (TransitiveVerbPast -> challenged pushed defeated)
    (TransitiveVerbSingularPresent -> counters challenges pushes defeats)

    (InfinitiveVerb -> counter achieve pay seek see)
    (HelpingVerb -> must will should)
    (BeingVerb -> were)
    (LinkingVerb -> be)

    (Adverb -> hardly not well)
    (Article -> the a this)
    (Adjective -> higher prevailing flawed desired solid important several visible naked business professional few amateur able happy amazing awesome helpful powerful interesting)
    (CoordinatingConjunction -> and but)
    (SubordinatingConjunction -> whether when)
    (Preposition -> of in as to with at)
    (Determiner -> such)
    (Expletive -> there)
    (InfinitiveParticle -> to))
  "A grammar for a trivial subset of English.")

(defun targeted-sentence (rules)
  (apply-rules rules nil)
)

;list of rules using DFS order
;(THE MAN LIKED A WOMAN)
;(defparameter rules1 '((sentence 0) (noun-phrase 0) (Article 0) (Noun 0) (verb-phrase 0) (Verb 3) (noun-phrase 0) (Article 1) (Noun 2)))

;take a set of rules and the current sentence that has been generated so far
;here is what happens for the above example:
;when the first rule is called, the current sentence is empty and is rewritten to (noun-phase verb-phrase)
;second rule: (Article Noun verb-phrase)
;3: (THE Noun verb-phrase)
;4: (THE MAN verb-phrase)
;5: (THE MAN Verb noun-phrase)
;6: (THE MAN LIKED noun-phrase)
;7: (THE MAN LIKED Article Noun)
;8: (THE MAN LIKED A Noun)
;9: (THE MAN LIKED A WOMAN)

; MY RULES

(defparameter rules1 '((Sentence 0) (NounPhraseSingular 3) (AdjectivePhrase 0) (Adjective 0) (AbstractNoun 0) (VerbPhraseSingularPresent 0) (HelpingVerb 0)
    (InfinitiveVerb 0) (NounPhraseSingular 4) (NounPhraseSingular 1) (Article 0) (AdjectivePhrase 0) (Adjective 1) (NounSingular 4) (PrepPhrase 0)
    (Preposition 0) (NounPhraseSingular 2) (AbstractNoun 1)))

(defparameter rules2 '((Sentence 0) (NounPhraseSingular 1) (Article 2) (AdjectivePhrase 0) (Adjective 2) (NounSingular 5) (VerbPhraseSingularPresent 1)
    (HelpingVerb 1) (Adverb 0) (InfinitiveVerb 1) (NounPhraseSingular 1) (Article 0) (AdjectivePhrase 0) (Adjective 3) (NounSingular 6)))

(defparameter rules3 '((Sentence 1) (ExpletivePhrase 0) (Expletive 0) (BeingVerb 0) (NounPhrasePlural 1) (NounPhrasePlural 6) (AdjectivePhrase 0) (Adjective 4)
    (AttributiveNoun 1) (NounPlural 4) (PrepPhrase 1) (Preposition 1) (NounPhrasePlural 2) (AdjectivePhrase 0) (Adjective 6) (NounPhrasePlural 3) 
    (NounPhrasePlural 4) (NounPlural 5) (RelativeClause 0) (RelativePronoun 0) (IntransitiveVerbPluralPresent 0) (InfinitivePhrase 0) (InfinitiveParticle 0)
    (InfinitiveVerb 2) (Adverb 2) (PrepPhrase 2) (Determiner 0) (Preposition 2) (NounPhrasePlural 5) (NounPhrasePlural 0) (AdjectivePhrase 2) (Adjective 9)
    (CoordinatingConjunction 0) (AdjectivePhrase 0) (Adjective 10) (NounPlural 6) (CoordinatingConjunction 0) (NounPhraseSingular 5) (AttributiveNoun 0)
    (AbstractNoun 2)))

(defparameter rules4 '((Sentence 2) (Sentence 0) (NounPhraseSingular 0) (Article 0) (NounSingular 7) (VerbPhraseSingularPresent 2) (HelpingVerb 1) (Adverb 1) 
    (LinkingVerb 0) (Adjective 7) (PrepPhrase 0) (Preposition 3) (NounPhraseSingular 1) (Article 0) (AdjectivePhrase 0) (Adjective 8) (NounSingular 8)
    (CoordinatingConjunction 1) (Sentence 4) (NounPhrasePlural 0) (AdjectivePhrase 0) (Adjective 12) (NounPlural 8) (VerbPhrasePluralPresent 0) (HelpingVerb 2) 
    (LinkingVerb 0) (Adjective 13) (InfinitivePhrase 1) (InfinitiveParticle 0) (InfinitiveVerb 4) (PersonalPronoun 0) (PrepPhrase 1) (Preposition 4)
    (NounPhrasePlural 4) (NounPlural 9)))

(defparameter rules5 '((Sentence 3) (SubordinatingConjunction 0) (Sentence 0) (NounPhraseSingular 0) (Article 0) (NounSingular 9) (VerbPhraseSingularPresent 0) 
    (HelpingVerb 1) (InfinitiveVerb 3) (NounPhraseSingular 4) (NounPhraseSingular 6) (Article 0) (AttributiveNoun 2) (NounSingular 10) (PrepPhrase 0)
    (Preposition 1) (NounPhraseSingular 0) (Article 0) (NounSingular 11) (VerbPhraseFuturePerfect 0) (HelpingVerb 1) (LinkingVerb 0) (IntransitiveVerbPast 3)
    (PrepPhrase 1) (Preposition 1) (NounPhrasePlural 7) (Article 1) (AdjectivePhrase 0) (Adjective 11) (NounPlural 7)))

(defun apply-rules (rules sentence)
  (cond 
    ((null rules) sentence)
    ((null sentence) (apply-rules (rest rules) (elt (rule-rhs (assoc (car (car rules)) *grammar*)) (second (car rules)))))
    (t (let ((rule-to-rewrite (car (car rules))) (new-rule (elt (rule-rhs (assoc (car (car rules)) *grammar*)) (second (car rules)))))
      (apply-rules (rest rules) (rewrite-sentence nil sentence rule-to-rewrite new-rule)))))) 

;simply rewrites a sentence replacing the first occurence of the variable "rule-to-rewrite" in "sentence-next" by the symbols in "new-rule" 
;example: (rewrite-sentence nil '(THE MAN verb-phrase) 'verb-phrase '(Verb noun-phrase))
;returns (THE MAN Verb noun-phrase)
(defun rewrite-sentence (sentence-pre sentence-next rule-to-rewrite new-rule)
    (cond ((null sentence-next) sentence-pre)
    (t 
      (if (equal (car sentence-next) rule-to-rewrite)
      (append (append sentence-pre (if (listp new-rule) new-rule (list new-rule))) (rest sentence-next))
      (rewrite-sentence (append sentence-pre (list (car sentence-next))) (rest sentence-next) rule-to-rewrite new-rule)))))
      

(defun random-elt (list)
  (elt list
       (random (length list))))

(defun random-sentence (phrase)
  "Generate a random sentence or phrase"
  (cond ((listp phrase)
         (mappend #'random-sentence phrase))
        ((rewrites phrase)
         (random-sentence (random-elt (rewrites phrase))))
        (t (list phrase))))

(defun generate-tree (phrase)
  "Generate a random sentence or phrase,
  with a complete parse tree."
  (cond ((listp phrase)
         (mapcar #'generate-tree phrase))
        ((rewrites phrase)
         (cons phrase
               (generate-tree (random-elt (rewrites phrase)))))
        (t (list phrase))))

(defun mappend (fn list)
  "Append the results of calling fn on each element of list.
  Like mapcon, but uses append instead of nconc."
  (apply #'append (mapcar fn list)))

(defun rule-rhs (rule)
  "The right hand side of a rule."
  (rest (rest rule)))

(defun rewrites (category)
  "Return a list of the possible rewrites for this category."
  (rule-rhs (assoc category *grammar*)))




; MY FUNCTIONS

; generateValid returns a valid phrase described by the non_term input. Ex. if you
; call generateValid('sentence), the function will generate a random sentence and check 
; if it's valid using validp. If it's valid, then the sentence will be returned. If not,
; the function will keep calling random-sentence until a valid sentence is returned.

(defun generateValid (non_term)

  ; set found_valid to false
  (setq found_valid NIL)

  ; while we have not found a valid sentence
  (loop while (not found_valid) do

    ; set the current_phrase to (random-sentence non_term)
    (setq current_phrase (random-sentence non_term))

    ; if the current_phrase is valid, set found_valid to true
    (if (validp current_phrase)
      (setq found_valid t))
  )

  (setq return_val current_phrase)
)

; returns true is sentence is valid
(defun validp (sent)
  (and  (check-length sent)
        (check-depth sent)
        (check-for-repeats sent)
        (check-repeated-subordinating-conjunction sent)
        (check-coordinating-conjunction-count sent))
)

; returns true if sentence is valid; false if not
(defun check-length (sent)
  (<= (length sent) 40)
)

; couldn't figure out in time, so it just always returns true
(defun check-depth (sent)
  (= 1 1)
)

; returns true if sentence is valid; false if not. Certain types of words are allowed to be repeated
(defun check-for-repeats (sent)

  ; compile a list of permitted repeated words
  (setq permitted_words (rewrites 'Article))
  (setq permitted_words (append permitted_words (rewrites 'CoordinatingConjunction)))
  (setq permitted_words (append permitted_words (rewrites 'Preposition)))
  (setq permitted_words (append permitted_words (rewrites 'HelpingVerb)))
  (setq permitted_words (append permitted_words (rewrites 'LinkingVerb)))
  (setq permitted_words (append permitted_words (rewrites 'BeingVerb)))
  (setq permitted_words (append permitted_words (rewrites 'Expletive)))

  (setq found_repeated NIL)

  ; while the sentence has more than 1 word left, and no repeats have been found
  (loop while (and (not found_repeated) (> (length sent) 1)) do

    ; set the current word as the first word in the sentence
    (setq current_word (car sent))

    ; if current_word is not a permitted word
    (if (not (member current_word permitted_words))

      ; check if the current_word is repeated in the rest of the list
      (if (member current_word (cdr sent))
        ; if it is, set flag to true
        (setq found_repeated t))
    )

    ; remove the first word from the list, start while loop over
    (setq sent (cdr sent))
  )

  (setq return_value (not found_repeated))
)

; first rejection function of my own. Checks that subordinating 
; conjunctions never appear twice in a row.
(defun check-repeated-subordinating-conjunction (sent)

  ; set word_list as a list of all subordinating conjunctions
  (setq word_list (rewrites 'SubordinatingConjunction))

  ; set found_repeat as false
  (setq found_repeat NIL)

  ; while found_repeat is false and the sentence has 2+ words
  (loop while (and (not found_repeat) (>= (length sent) 2)) do

    ; set current_word and next_word as the first and second words
    (setq current_word (first sent))
    (setq next_word (second sent))

    ; if the current and next words are both subordinating conjunctions
    (if (and (member current_word word_list) (member next_word word_list))
      ; set found_repeat to true
      (setq found_repeat t))
    
    ; remove the first word from the sentence
    (setq sent (cdr sent))
  )

  (not found_repeat)
)

; second rejection function of my own. Checks that there are less 
; than 8 coordinating conjunctions in the sentence.
(defun check-coordinating-conjunction-count (sent)

  ; get the list of coordinating conjunctions
  (setq permitted_words (rewrites 'CoordinatingConjunction))
  (setq count 0)

  ; iterate through all the words in the input sentence
  (loop while (>= (length sent) 1) do

    ; set current word as the first word
    (setq current_word (first sent))

    ; check if the current word is in permitted_words
    (if (member current_word permitted_words)
      (setq count (+ 1 count)))

    (setq sent (cdr sent))
  )

  (< count 8)
)

; FUNCTIONS TO OUTPUT TO FILE

;You can use these functions to write sentences to a file. It will call your functions random-sentence and validp. If validp returns true it will add a + before 
;the function, and if it returns nil it will add a -.

;Call loop-run(N) with N the number of sentences that you want to write in your file. Also you can change the name of the file that you want to ouptput in the 
;function write-to-file

;This function will create the file if it doesn't exit, and will append the sentence at the end if the file already exists.


(defun write-to-file (sentence)
  (with-open-file (str "q1_ksx2101.txt"
                     :direction :output
                     :if-exists :append
                     :if-does-not-exist :create)
    (format str sentence)))

(defun run2 ()
  (let ((sent (random-sentence 'sentence)))
    (write-to-file (if (validp sent) (format nil "+ ~S ~%" sent) (format nil "- ~S ~%" sent)))))

(defun loop-run (N)
  (loop for i from 1 to N do (run2)))


;(random-sentence 'sentence)
