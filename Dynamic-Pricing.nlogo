;;Amineh Kamranzadeh
;;Kimbrough, Steven O. (2011). Oligopoly Bid Price model. http://opim.wharton.upenn.edu/~sok/AGEbook/nlogo/OligopolyBidPrice.nlogo University of Pennsylvania, Philadelphia, PA 19004, USA.
;ai20170428 It is good to have this here, but you shd also add (in more detail) to the `Info` tab.
;;ak01; Our main modification to the original model:
;;1) Added “reduce cost” and “decrease product value” strategies: In the original model, competitors reduce price below that of competitors in order to dominate the market.
;;Therefore, in each iteration the only tactic that firms can chose is to reduce price until the price equals marginal cost. In our model, we added to the list of strategies that competitors can choose from by checking competition and consumer preferences.
;;2) Added "first mover advantage": In the original model, competitors reduce price below that of competitors in order to dominate the market.
;;Therefore, in each iteration the only tactic that firms can chose is to reduce price until the price equals marginal cost. In our model, we added to the list of strategies that competitors can choose from by checking competition and consumer preferences.
;;3) Introduced a penalty to the profit function of firms with "Decrease Product Value" policy so that their quantity demanded decreases by the reduction value of their product value and
;;therefore gain less profit in each episode.
;;4) Added relevant monitors to track how firms cost changes based on the new introduced strategies and modifications, as well as the subsequent profits gained by each firm in each epoch for analysis in behaviorspace.
;ai20170428 Amineh, You should have added at the VERY least one experiment by now; see the project guidelines.
;ai20170428 Amineh & Robin: will you really not be writing any files to collect data,
;  except with BehaviorSpace?  (This is NOT required, but is usually useful.)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;DECLARATIONS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

globals [
  daWinner                      ;; rm01; the winning firm in each round
  current-price-firm-1          ;; (float): rm01;used to set values of other variables
  priceIntercept                ;; (float): rm01;Price intercept; the price where quantity is 0
  m-price                       ;; (float): rm01;Monopolist's price = (quantityIntercept + totalUnitProductionCost * dSlope) / (2 * dSlope)
  m-price-at0cost               ;; (float): rm01;Monopolist's price when the monopolist has zero costs
  m-quantity-at0cost            ;; (float): rm01;Monopolist's quantity given m-price as the monopolist's price
  m-price-firm0                 ;; (float): rm01;Monopoly price of firm 0
  m-price-firm1                 ;; (float): rm01;Monopoly price of firm 1
  firstMover                    ;; ak01; (added to the original model) to distinguish the firm with the lowest bid (winner) in the first episode
  quantityIntercept             ;; (float): rm01;Used to calculate the price when the quantity is zero
  episodeAbsoluteCount          ;; (float): rm01;Count of iterations
  avgWinningBid                 ;; (float): rm01;Running average of the winning bid value
  runningAverageBidList         ;; (list of float): rm01;Running list of average bids
  runningAverageBid             ;; (float): rm01;Running average bid value
  totalUnitProductionCost       ;; (float): rm01;Cost to produce
  ;ai20170428 When comments get long, put them on their own line.  (See the project guidelines.)
  productValue                  ;;  ak01; (added to the original model) (float) product value is a new variable added to firms' cost function so firms can decrease their cost through this variable: cost = cost * productValue
  version                       ;;
  episodeBids                   ;; (list of float): rm01;all price bids in current episode
]
;ai ;;;;;;;;;;; INTERFACE GLOBALS ;;;;;;;;;;;;;;;;;;;
  ;epsilonFirm0 ;ai20170428 this is also in a slider
;slider variable   ;epsilonFirm1            ;; rm01;The amount that firm 1 can fluctuate its preice by
;slider variable   ;deltaFirm0              ;; rm01;the range within which the price offer falls for firm 0
;slider variable   ;deltaFirm1              ;; rm01;the range within which the price offer falls for firm 1
;slider variable   ;kFirm0                  ;; rm01;the cost of production for firm 0
;slider variable   ;kFirm1                  ;; rm01;the cost of production for firm 1
;slider variable   ;initialPriceFirm0       ;; rm01;the initial price charged by firm 0
;slider variable   ;initialPriceFirm1       ;; rm01;the initial price charged by firm 1
;slider variable   ;epochLengthFirm0        ;; rm01;the length of an epoch (in episodes) for firm 0
;slider variable   ;epochLengthFirm1        ;; rm01;the length of an epoch (in episodes) for firm 1
;slider variable   ;reportEveryNEpisodes    ;; rm01;determines how often the variables of interest are reported in erms of episodes (ticks)
;slider variable   ;dSlope                  ;; rm01;slope of the demand curve
;slider variable   ;qIBase                  ;; rm01; X intercept, the quantity at which price approaches 0
;slider variable   ;numEpisodesToRun        ;; rm01; How many episodes to execute
;slider variable   ;variableFactor          ;; rm01; Allows for controlled fluctuations in price
;slider variable   ;runningAvgLength        ;; rm01; Used to modify runningAverageBidList
;slider variable   ;firstMoverAdvantage     ;; ak01; (added tot he original model) The magnitude of the first mover advantage
;slider variable   ;VDelta0                 ;; ak01; (added tot he original model) parameter used to decrease product value of firm 0
;slider variable   ;VDelta1                 ;; ak01; (added tot he original model) parameter used to decrease product value of firm 1
;slider variable   ;CDelta0                 ;; ak01; (added tot he original model) parameter used to decrease cost of firm 0
;slider variable   ;CDelta1                 ;; ak01; (added tot he original model) parameter used to decrease cost of firm 1
;slider variable   ;prodcutValue0           ;; ak01; (added tot he original model) the product value of firm 0
;slider variable   ;prodcutValue1           ;; ak01; (added tot he original model) the product value of firm 1
;ai20170428 You also need to document the switches!

breed [
  pAndAers pAndA                            ;; Probe-and-Adjust
]
turtles-own [
  ;ai FROM ORGINAL MODE
  priceBid               ;; rm01; The price bid by each individual firm
  totalReward            ;; rm01; The total reward gained by each firm via partidipation in the market
  bidsWon                ;; rm01; Running count of the bids won by each individual firm
  unitProductionCost     ;; rm01; The cost of each firm per unit of production
  ;ai ADDITIONS TO MODEL
  profit                 ;; rm01; added to original model, calculated profit of each winning firm at each epoch, to be used for behaviorspace analysis.
]
pAndAers-own [
  epsilon                              ;; rm01;amount price is increased or decreased by
  delta                                ;; rm01;defines the range within which the price offer falls
  cost                                 ;; rm01;cost of production
  epochLength                          ;; rm01;the length of an epoch, defined in episodes
  episodeCount                         ;; rm01;the current episode and how many have passed previously
  currentPrice                         ;; rm01;the current price for which sales are made
  upRewards                            ;; rm01;the net benefit lost by not winning an episode, based on p and q of winner
  downRewards                          ;; rm01;the net loss avoided by not winning an episode, based on p and q of winner
  VDelta                               ;; ak01; (added to the original model) Value delta is the parameter we use to decrease product value
  CDelta                               ;; ak01; (two update types added to the original model)Firms different profit policy, i.e. market return, reduce product value, reduce cost.
  kFirm                                ;; rm01;a variable that defines cost of production for each firm
  updateType                           ;; rm01;Update the intended returns method, i.e. market, individual, etc.
  isFirstMover                         ;; ak01; (added tot he original model)Binary variable equal to 1 if the firm is identified as the first mover
  ownUpRewards                         ;; ak01; profit gained by the firm by bidding high
  ownDownRewards                       ;; ak01; profit gained by the firm by bidding low
  runningAverageReward                 ;; rm01;the running average of the reward gained
  myEpisodeReward                      ;; rm01;what the pAndAer gets in the current episode
  myPricesBid industryPricesBid        ;; rm01;these are lists of prices bid during an epoch
  ;patience ;ai20170428 say sonmething about cutting this out of the model
  ]


;;;;;;;;;;;;;;;;;;;;;;;;;
;; setup  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

;ai20170428 Missing `startup` procedure.  (See code submission guidelines.)

;ai20170428 This is MUCH better than Kimbrough's setup!
;ai20170428 But reformat (here and below) for clearer presentation (especially, consistent indentation)
to Setup
  clear-all                                     ;; rm01;Clears all to start with a blank slate
if (daRandomSeed != "system clock")             ;; rm01;If the value of daRandomSeed does not equal the value returned by the system clock,
  [random-seed daRandomSeed]                    ;; rm01;set daRandomSeed equal to a random-seed value
setupGlobals                                    ;; rm01;Executes setupGlobals procedure
clear-all-plots                                 ;; rm01;Clears all preexisting plots
plot-quantity-price                             ;; rm01;This plots the demand curve, and the monopoly price and quantity for that demand curve, assuming 0 costs for the monopolist. ; note that this is hidden, so the user doesn't actually see it, by default
create-pAndAers numPandAFirms                   ;; rm01; Creates the agent firms in both a singular and plural context
setupTurtles                                    ;; rm01;Executes setupTurtles procedure
setupPandAers                                   ;; rm01;Executes setupPandAers procedure
  reset-ticks                                   ;; rm01;Resets tick counter
end ; of setup

;;;;;;;;;;;;;;;;;;;;;;;;;
;; setup subprocedures;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;

to setupGlobals
  set episodeAbsoluteCount 0                                                                  ;; rm01;Initializes the episode count to 0
set runningAverageBidList []                                                                  ;; rm01;Initializes the running average bidclist
set quantityIntercept qIBase                                                                  ;; rm01;Initializes the value of the uantity intercept to qIBase
set priceIntercept quantityIntercept / dSlope                                                 ;; rm01;the price when the quantity is 0
set m-price-at0cost quantityIntercept / (2 * dSlope)                                          ;; rm01;This is the monopolist's price when the monopolist has 0 costs. ; Otherwise, it is m-price = (quantityIntercept + k*dSlope)/(2*dSlope)
set m-quantity-at0cost (quantityIntercept - (dSlope * m-price-at0cost))                       ;; rm01;This is the monopolist's quantity given m-price as the monopolist's price
end

;ai20170428 There is still too much going on here.  Break up into subprocs.
to setupPandAers     ;;IMBUES FIRMS WITH NECESSARY CHARACTERSTICS FOR MODELING
ask pAndAers [
  ;ai20170428 Another example of a helpful comment. V  (Describes what's about to be done.)
  ;; rm01;First, I'll set ALL firms with the Firm1 parameter values.
  ;ai20170428 And another example of a helpful comment. V
  ;  (In this case, it is helpful to be reminded that epsilonFirm1 is a slider variable!)
  ;; rm01;Sets the amount that firm1 raises or lowers its price equal to the value designated in the interface slider
    set epsilon epsilonFirm1
     set delta deltaFirm1                                       ;; rm01;Sets the potential bounds of firm1s bid equal to the value designated in the interface slider
     set VDelta VDelta1                                         ;; ak01; (added to the original model) value delta is the parameter we use to decrease product value
     set CDelta CDelta1                                         ;; ak01; (added to the original model) Cost delta is the parameter we use to decrease cost
     set productValue productValue1                             ;; ak01; (added to the original model) product value is a new vaiable that we add which ranges from 1-10.
                                                                ;; ak01; (added to the original model) Firms can decrease their cost by decreasing their product value using VDelta: cost=kFirm + (1 - VDelta) * productValue
     set kFirm kFirm1                                           ;; ak01; (added to the original model)assigning cost of firm 1 value to firm 1
     set cost kFirm * (1) * productValue                        ;; ak01; (added to the original model) we modified the cost function assuming production cost is a function of product value
     set isFirstMover 0                                         ;; ak01; (added to the original model)initializes the firstmover attribute as false
     set currentPrice initialPriceFirm1                         ;; rm01;Sets the current price variable equal to the initial price of firm 1 as set in the interface slider
     set epochLength epochLengthFirm1                           ;; rm01;Sets the epoch length equal to the value designated in the epoch length of firm 1 interface slider
     set episodeCount 0                                         ;; rm01;Initializes episode count to 0
     set unitProductionCost kFirm1                              ;; rm01;Sets the unit productioncost equal to the unit cost of production for firm 1 as designated in the interface slider.
     set updateType updateTypeFirm1                             ;; rm01;Sets the update type to the update type of firm 1 as designated in the inerface tab
     set myEpisodeReward 0                                      ;; rm01;Initializes the my episode reward of firm 1 equal to 0
     set myPricesBid []                                         ;; rm01;Initializes the value for myPriceBid
     set industryPricesBid []                                   ;; rm01;Initializes the value for industryPricesBid

    ]
  ; Now do firm 0                                               ;; rm01;Set Firm 0 with the Firm0 values.
  ask pAndA 0 [set epsilon epsilonFirm0]                        ;; rm01;Sets the amount that firm1 raises or lowers its price equal to the value designated in the interface slider
  ask pAndA 0 [set delta deltaFirm0]                            ;; rm01;Sets the potential bounds of firm0s bid equal to the value designated in the interface slider
  ask pAndA 0 [set VDelta VDelta0 ]                             ;; ak01; (added to the original model) value delta is the parameter we use to decrease product value
  ask pAndA 0 [set CDelta CDelta0 ]                             ;; ak01; (added to the original model) Cost delta is the parameter we use to decrease cost
  ask pAndA 0 [set productValue productValue0 ]                 ;; ak01; (added to the original model)product value is a new vaiable that we add which ranges from 1-10.
                                                                ;; ak01; (added to the original model) rm01;Firms can decrease their cost by decreasing their product value using VDelta: cost=kFirm + (1 - VDelta) * productValue
  ask pAndA 0 [set currentPrice initialPriceFirm0]              ;; rm01;Sets the current price variable equal to the initial price of firm 0 as set in the interface slider
  ask pAndA 0 [set kFirm kFirm0 ]                               ;; rm01;assigning cost of firm 0 value to firm 0
  ask pAndA 0 [set cost kFirm * (1) * productValue]             ;; ak01; (added to the original model)we modified the cost function rm01; assuming production cost is a function of product value
  ;ai20170428 can you separate any ADDITIONS so that the reader can easily find them?
  ask pAndA 0 [set isFirstMover 0]                              ;; ak01; (added to the original model)initializes the firstmover attribute as false
  ask pAndA 0 [set epochLength epochLengthFirm0]                ;; rm01;Sets the epoch length equal to the value designated in the epoch length of firm 0 interface slider
  ask pAndA 0 [set episodeCount 0]                              ;; rm01;Initializes episode count to 0
  ask pAndA 0 [set unitProductionCost kFirm0]                   ;; rm01;Sets the unit productioncost equal to the unit cost of production for firm 0 as designated in the interface slider.
  ask pAndA 0 [set updateType updateTypeFirm0]                  ;; rm01;Sets the update type to the update type of firm 0 as designated in the inerface tab

  ;ai20170428 It *is* helpful to be reminded that `variableStarts` is an interface switch,
  ;  but it would be more helpful to be reminded what it means.  Possibly (?), break this out into
  ;  a separate proc.  (And, remember to include your initials with your comments--of course, you usually do!)
  if (variableStarts = True)                                    ;; If the variable starts switch on the interface tab is turned on,
   [ask pAndAers
     [set currentPrice (random-float 2 * (variableFactor * currentPrice)) + (currentPrice - (variableFactor * currentPrice))        ;; rm01;Sets the current price equal to the currentprice less the variable factor*the current price times either 1 or 0 based on the random-float value
      print (word "Starting currentPrice for agent "  self  " is "  currentPrice  ".")                                              ;; rm01;Displays starting currentPrice for a specigfic agent as the current price
   ]
   ] ; end of if (variableStarts = True)

  set m-price-firm0 (quantityIntercept + [unitProductionCost] of pAndA 0 * dSlope) / (2 * dSlope)                           ;; rm01;sets the monopoly price of firm 0 equal to the quantity intercept plus the unit production cost * the slope of the demand curve / 2 times the slope of the demand curve
  if (numPandAFirms > 1)                                                                                                    ;; rm01;If there are more than 1 firm in the market, (NOT A MONOPOLY)
    [set m-price-firm1 (quantityIntercept + [unitProductionCost] of pAndA 1 * dSlope) / (2 * dSlope)]                       ;; rm01;sets the monopoly price of firm 1 equal to the quantity intercept plus the unit production cost * the slope of the demand curve / 2 times the slope of the demand curve
  ;;;;;;;;;;
  ;ai20170428 It would be nice to break reporting set up into a separate proc.
  ;  (Also, it is odd to report to the command line instead of to a file...)
  if (reporting)                     ;; rm01;If the agents are reporting
    ;ai20170428  Note that these comments are not correct ...
    [write "episode"                 ;; rm01;Display the episode number
     write "ID"                      ;; rm01;Display the ID number
     write "breed"                   ;; rm01;Display the breed
     write "currentPrice"            ;; rm01;Display the agents current price
     write "bidsWon"                 ;; rm01;Display the amount of bids won
     write "totalReward"             ;; rm01;Display the totalReward of the agent
     print " "]                      ;; rm01;Print the values
  if (reporting)                     ;; rm01;If the agents are reporting
     [turtle-report]                 ;; rm01;Execute turtle-report procedure
  ; Now get the total unit production cost
  set totalUnitProductionCost 0      ;; rm01;Initializes totalUnitProductionCost to 0
  ask turtles
    [set totalUnitProductionCost (totalUnitProductionCost + unitProductionCost)]        ;; rm01;Increments the totalUnitProductionCost by the unitProductionCost
  set m-price (quantityIntercept + totalUnitProductionCost * dSlope) / (2 * dSlope)     ;; rm01;sets the monopoly price equal to the quantity intercept plus the unit production cost * the slope of the demand curve / 2 times the slope of the demand curve
  ;ai20170428 Does the turtle location have any significance?  (The View is hidden ...)
  ask pAndAers [setxy -4 who                                                            ;; rm01;Sets the XY coordinates of the turtles to (-4, the turtles who number)
     set upRewards []                                                                   ;; rm01;Initializes upRewards to an empty list
     set downRewards []                                                                 ;; rm01;Initializes downRewards to an empty list
     set ownUpRewards []                                                                ;; rm01;Initializes ownUpRewards to an empty list
     set ownDownRewards [] ]                                                            ;; rm01;Initializes ownDownRewards to an empty list
; ok, from the Information tab:
;|numPandAFirms
;If set to 1, we have the monopoly case and the firm uses the Firm0 parameter values.
;If set to 2, we have the duopoly case. Firm 0 uses the Firm0 parameter values and
;Firm 1 uses the Firm1 values. If set to 3 or more, Firm 0 uses the Firm0
;parameter values and all other firms use the Firm1 parameter values.
;(Later I can have a randomized option for parameter values.)
end

to setupTurtles
  ask turtles [set totalReward 0     ;; rm01; Initialize total reward value to 0
             set bidsWon 0]          ;; rm01; Initialize bids Won value to 0
end

;;;;;;;;;;;;;;;;;;;;
;; go ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;

to Go
  ;; rm01; (MODIFICATION OF ORIGINAL CODE) ; Broken downn into subprocedures ;ai20170428 Yea!
  ;; rm01;Increment tick counter ;ai20170428 move this comment (and say why you moved the code)
  if (ticks >= numEpisodesToRun) [stop]             ;; rm01; Stopping condition setby slider on interface tab.
  set episodeAbsoluteCount episodeAbsoluteCount + 1 ;; rm01;Increment the absolute episode count by 1 for each tick
  set episodeBids []                                ;; rm01;a list to hold all price bids in this episode
  perturbDemandCurve                                ;; rm01;Executes perturbDemand curve procedure
; 1. All of the turtles are asked to determine a bid price. Each records his own, in bidPrice.
  ask turtles [
    Set-PriceBid                                    ;; rm01;Sets the price bid of the turtles based on a random float value within the delta ranges set by the interface sliders
  ]
  takeWinningBid                                    ;; rm01;Execute takeWinningBid procedure to notate the winning turtle and do all subsequent reporting.
  tick
end ; of go


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; go subprocedures ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to perturbDemandCurve    ;; rm01; Allowsthe demand curve intercepts to vary based on a random value to simulate a random-walk procedure        ;; 0. Perturb the demand curve if we're doing random-walk-as
  if (random-walk-as)                                                                                           ;; rm01;If random-walk-as is true,
   [set quantityIntercept (random-float 2 * aDelta) - aDelta + quantityIntercept                                ;; rm01;Sets the quantity intercept value as either quantityintercept or quantityintercept - aDelta
   set priceIntercept quantityIntercept / dSlope                                                                ;; rm01;Sets priceIntercept equal to the quantityIntercept / dSlope
   ; This is the monopolist's price when the monopolist has 0 costs.
   ; Otherwise, it is m-price = (quantityIntercept + k*dSlope)/(2*dSlope)
   set m-price-at0cost quantityIntercept / (2 * dSlope)                                                         ;; rm01;Sets monopoly price at 0 costs equal to the quantityIntercept/(2*dSlope)
   ; This is the monopolist's quantity given m-price as the monopolist's price
   set m-quantity-at0cost (quantityIntercept - (dSlope * m-price))                                              ;; rm01;Sets the monopoly quantity at 0 costs equal to the quantityIntercept - (dSlope*m-price)
   set m-price (quantityIntercept + totalUnitProductionCost * dSlope) / (2 * dSlope)                            ;; rm01;Sets monopoly price equal to (quantity intercept + (totalunitproductioncost*dSLope))/(s*dSlope)
   set m-price-firm0 (quantityIntercept + [unitProductionCost] of pAndA 0 * dSlope) / (2 * dSlope)              ;; rm01;Sets monopoly price of firm 0 equal to (quantity Intercept + [unitProductionCost] of pAndA 0 * dSlope) / (2 * dSlope)
   if (numPandAFirms > 1)                                                                                       ;; rm01;If the number of Probe and adjust firms is greater than 1 (competition)
     [set m-price-firm1 (quantityIntercept + [unitProductionCost] of pAndA 1 * dSlope) / (2 * dSlope)]          ;; rm01;Sets monopoly price of firm 1 equal to (quantityIntercept + [unitProductionCost] of pAndA 1 * dSlope) / (2 * dSlope)]
   ] ; if of if (random-as)
end
to Set-PriceBid     ;; rm01; Allows agent firm to set price bid for current epoch and adds this bid value to all running list of the agent's bids
 if (is-pAndA? self) [                                                                  ;; rm01; If the agent is self,
  set priceBid max (list cost ((random-float 2 * delta) - delta + currentPrice))        ;; rm01;Sets priceBid equal to the maximum value of the list of cost times either currentPrice or currentPrice - Delta
  set episodeBids lput priceBid episodeBids                                             ;; rm01;Updates episode bids list to include priceBid at the end of the list
  set myPricesBid lput priceBid myPricesBid                                             ;; rm01;Updates my Prices bid list to include priceBid at the end of the list
  set episodeCount episodeCount + 1                                                     ;; rm01;Increments episodeCount by 1
 ]
end ; of set-priceBid

to takeWinningBid   ;; rm01; Based on all the relevant variable factors, the agent firm with the lowest price bid is signified as the winner (a la Bertrand Price Competition) and subsequently captures the entire market for the epoch
  ; 2. We figure out the lowest bid and take it.
  ; daWinner is the winning turtle, with the or a lowest bid.
  set daWinner min-one-of turtles [priceBid]                                                                    ;; rm01; Sets daWinner (winning turtle) as that with the lowest priceBid (the lowest bidding turtle captures all market demand)
let daWinnersID [who] of daWinner                                                                               ;; rm01; Sets daWinnersID equal to the WHO identifier of the daWinner turtle.
let daWinningBid [priceBid] of daWinner                                                                         ;; rm01; Sets daWinningBId equal to the price bid of the winning turtle
let daQuantityDemanded Quantity-Demanded(daWinningBid)                                                          ;; rm01; Sets daQuantityDemanded equal to the QUantity demanded times the winning bid
let daCost [cost] of daWinner                                                                                   ;; rm01; Sets daCost equal to the cost of the winning turtle
let tempReward [totalReward] of daWinner                                                                        ;; rm01; Sets the temporary reward equal to the otal reward of the winning turtle
ask daWinner [set totalReward (tempReward + (daWinningBid * daQuantityDemanded - daCost * daQuantityDemanded))] ;; rm01; sets total reward equal to the total reward plus the winningbid*quantitydemanded-(cost*quantitydemanded) (standard profit function)
ask daWinner [set bidsWon (bidsWon + 1)]                                                                        ;; rm01; Increment bids won counter by 1
ask daWinner [set profit ((daWinningBid * daQuantityDemanded) - (daCost * daQuantityDemanded))]                 ;; rm01; Asks the winner to record their profits from the winning epoch according to a typical linear profit function.


;;;;;;;;First-mover advantage ;;;;;;;;;;;;;;;;;;;;                           ;; ak01;we hypothesize that the firm which dominates the market in the first episode gains advantage by reducing its product value after the first episode
if (episodeAbsoluteCount = 1) [ask daWinner [set isFirstMover 1]]            ;; ak01;If it is currently the first episode, identify the winning agent as the first mover
if (episodeAbsoluteCount = 1) [set firstMover daWinner]                      ;; ak01;If it is currently the first episode, set the firstMover as the winning agent

; 4. All of the turtles observe the winning bid (daWinningBid)
; and the resulting quantity demanded (daQuantityDemanded).
; All of the turtles record the profit they would have had (or did have
; in the case of the winner) as either upProfits or downProfits, profits they
; got or could have gotten by bidding high or low.  More carefully put,
; each firm observes the winning bid and resulting demand, then calculates
; the reward/profit it would have had had it won with that bid. This reward
; is recorded in the list of upRewards or downRewards depending on whether
; daWinningBid is > or <= currentPrice. Now all this applies only to turtles
; of breed pAndA.
ask turtles [observe-and-record(daWinningBid)(daQuantityDemanded)] ;; rm01;Turtles observe and record the values of interet in determining future price bids
; 5. Plot monopoly price and winning bid price.
plot-prices(m-price)(daWinningBid)                                           ;; rm01;Plots monopoly price and winning price bid
; This completes the episode. We ask the turtles to do any
; appropriate housekeeping.
ask turtles [postpare-episode]                                               ;; rm01;Executes postpare-episode procedure
ifelse (episodeAbsoluteCount > 1)                                                                                ;; rm01;If the absolute count of episodes is greater than 1
  [set avgWinningBid (avgWinningBid * (episodeAbsoluteCount - 1) + daWinningBid) / episodeAbsoluteCount ]        ;; rm01;Set the average winning bid equal to (the current value of the average winning bid * the previous episode count * the winning bid) / the episode absolute count
  [set avgWinningBid daWinningBid]                                                                               ;; rm01;Otherwise, set the average winning bid equal to the winning nid

set runningAverageBidList lput daWinningBid runningAverageBidList                    ;; rm01;Updates running average bids list to include daWinningBid at the end of the list
if (length runningAverageBidList > runningAvgLength)                                 ;; rm01;If the length of the running average bid list is greater than the designated running average length
  [set runningAverageBidList but-first runningAverageBidList]                        ;; rm01;Updates the running average bid list to not include the first term in the list.  This is done to ;;;;;;;;;;;;;;;;;
set runningAverageBid mean runningAverageBidList                                     ;; rm01;Sets the running average bid to the mean of the running average bid list

if (episodeAbsoluteCount mod reportEveryNEpisodes = 0 and reporting)                 ;; rm01;If the episodeabsolutecount mod reportEveryNepisodes (episodeAbsoluteCount - (floor (episodeAbsoluteCount / reportEveryNEpisodes)) * reportEveryNEpisodes) equals 0 and the turtles are reporting,
  [turtle-report]                                                                    ;; rm01;Execute turtle-report procedure
end

to Observe-And-Record [price quantity]
  if (is-pAndA? self)
  [ifelse (updateType = "Decrease Product Value")                                ;; ak01;(added to the original model) we introduce a penalty to firms that reduce their product value
                                                                                 ;; ak01;(added to the original model) meaning that firms with "Decrease Product Value" policy gain less profit because their quantity demanded drops
    [let reward price * (quantity * VDelta) - cost * (quantity * VDelta)         ;; ak01;(added to the original model) rm01;Sets reward as price * quantity( subject to the penalty associated with a lower product value) minus cost * quantity( subject to the penalty associated with a lower product value) (standard profit function)
                                                                                 ;; ak01;(added to the original model) firm's quantity demanded decreases by the changes in their product value:(quantity * (1- VDelta)

 ifelse (price > currentPrice)                                                   ;; ak01;If the winning bid is greater than the currentPrice,
      [set upRewards lput reward upRewards]                                      ;; rm01;Update upRewards list by adding reward to the end of the list (You won the bid)
      [set downRewards lput reward downRewards]                                  ;; rm01;Otherwise, update downRewards list by adding reward to the end of the list (You lost the bid)
    ifelse (priceBid > currentPrice)                                             ;; ak01;If the firm's bid price is greater than the currentPrice,
      [ifelse (daWinner = self)                                                  ;; ak01;If the agent is the winner,
       [set ownUpRewards lput reward ownUpRewards]                               ;; ak01;the firm Update its profit list by adding profit to the end of the list
       [set ownUpRewards lput 0 ownUpRewards]                                    ;; ak01;Otherwise, update ownUpRewards list by adding 0 to the end of the list
       ]
      [ifelse (daWinner = self)                                                  ;; ak01;If the agent is the winner,
       [set ownDownRewards lput reward ownDownRewards]                           ;; ak01;Update ownDownRewards list by adding profit to the end of the list
       [set ownDownRewards lput 0 ownDownRewards]                                ;; ak01;Otherwise, update ownDownRewards list by adding 0 to the end of the list
       ]
    ]
    [let reward price * quantity - cost * quantity                   ;; ak01; if firm doesn't have a "Decrease Product Value" policy, rm01;Sets reward as price * quantity minus cost * quantity (standard profit function)
    ifelse (price > currentPrice)                                    ;; ak01;If the winning bid is greater than the currentPrice,
      [set upRewards lput reward upRewards]                          ;; rm01;Update upRewards list by adding reward to the end of the list (You won the bid)
      [set downRewards lput reward downRewards]                      ;; rm01;Otherwise, update downRewards list by adding reward to the end of the list (You lost the bid)
    ifelse (priceBid > currentPrice)                                 ;; ak01;If the firm's bid price is greater than the currentPrice,
      [ifelse (daWinner = self)                                      ;; rm01;If the agent is the winner,
       [set ownUpRewards lput reward ownUpRewards]                   ;; rm01;Update ownUpRewards list by adding reward to the end of the list (You won the bid)
       [set ownUpRewards lput 0 ownUpRewards]                        ;; rm01;Otherwise, update ownUpRewards list by adding 0 to the end of the list (You did not win the bid, therefore you get a reward of 0)
       ]
      [ifelse (daWinner = self)                                      ;; rm01;If the agent is the winner,
       [set ownDownRewards lput reward ownDownRewards]               ;; rm01;Update ownDownRewards list by adding reward to the end of the list (You won the bid)
       [set ownDownRewards lput 0 ownDownRewards]                    ;; rm01;Otherwise, update ownDownRewards list by adding 0 to the end of the list
       ]
    ]
    foreach episodeBids [ [?1] -> set industryPricesBid lput ?1 industryPricesBid ]          ;; rm01; Creates a list of a list of industryPricesBid
   ] ; end of if is-pAndA?
end ; of observe-and-record

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;PLOTS;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to plot-prices [monopolyPrice winningBid]   ;; rm01; Creates the main plot on the interface tab
set-current-plot "Current Bid Price"                              ;; rm01;Sets current plot as Current Bid Price
set-current-plot-pen "Winning Bid"                                ;; rm01;Sets current plot pen as Winning Bid
plot winningBid                                                   ;; rm01;Plots the values of WinningBid
; I no longer think this makes any sense, so I'm deleting it.
set-current-plot-pen "Monopoly Price at 0 Costs"                  ;; rm01;Sets current plot pen to Monopoly Price at 0 Costs
plot m-price-at0cost                                              ;; rm01;Plots monopoly price at 0 costs
set-current-plot-pen "Monopoly Price, Firm 0"                     ;; rm01;Sets current plot pento Monopoly Price, Firm 0
plot m-price-firm0                                                ;; rm01;Plots monopoly price of Firm 0
set-current-plot-pen "Monopoly Price, Firm(s) 1"                  ;; rm01;Sets current plot pento Monopoly Price, Firm(s) 1
plot m-price-firm1                                                ;; rm01;Plots monopoly price of firm 1
set-current-plot "Current Base Prices"                            ;; rm01;Sets current plot as Current Base Prices
let numToPlot min (list 10 numPandAFirms)                         ;; rm01;Set the numToPlot equal to the minimum value of a list containing 10 and the number of PandAFirms
foreach n-values numToPlot [ [?1] -> ?1 ]                         ;; rm01;For each of the values in the numToPlot list,create a list of lists
[ [?1] -> set-current-plot-pen (word "Agent"  ?1)                 ;; rm01; Creates plot pens for each of n agents
plot [currentPrice] of pAndA ?1                                   ;; rm01; Plots the current prices of the respective agent firms.
 ]
end ; of plot-prices
to Postpare-Episode   ;; rm01; One of the main segments of code.  Allows the agent firms to take into account the actions and outcomes of the other firm(s) and adjust their pricing strategy as necessary based on the update strategy signified on the interface tab chooser.
 ;;;;;;;; ak01; (Two new update policy added to the original : "Decrease Product Value", "Decrease Cost") ;;;;;;;;;;;;;;;;;;;;;
 if (is-pAndA? self) [
    let meanUp 0                                                                                ;; rm01;Set meanUp value equal to 0
    let meanDown 0                                                                              ;; rm01;Set meanDown value equal to 0
    if (episodeCount >= epochLength)                                                            ;; rm01;then update and reset
      [
      ; first, collect statistics
      set runningAverageReward (sum ownUpRewards + sum ownDownRewards) / epochLength            ;; rm01;set runningAverageBid as the sum of all ownUpRewards and ownDownRewards divided by the epoch length to give the average value
      if (updateType = "Market Returns") [                                                      ;; rm01;If the update type on the interface tab is designated as "Market Returns"
       ifelse (upRewards = [])                                                                  ;; rm01;If there is no recorded value for upRewards (empty list)
         [set meanUp 0]                                                                         ;; rm01;Set the meanUp value to 0, since there are no recorded values for upRewards
         [set meanUp mean upRewards]                                                            ;; rm01;Otherwise, set the meanUp value to the mean value of the recorded upRewards
       ifelse (downRewards = [])                                                                ;; rm01;If there is no recorded value for downRewards (empty list)
         [set meanDown 0]                                                                       ;; rm01;Set the meanDown value to 0, since there are no recorded values for downRewards
         [set meanDown mean downRewards]                                                        ;; rm01;Otherwise, set the meanDown value to the mean value of the recorded downRewards
       ifelse (meanUp > meanDown)                                                               ;; ak01; if, during the epoch, firm's bids above its current price returned on average more than its bids below the current price,
         [set currentPrice currentPrice + epsilon]                                              ;; ak01 ;then firm raises its price by epsilon;
         [                                                                                      ;; ak01;(added to the original model) if not, firm lowers its price (however, by how much the firm can lower the price depends on whether the firm is the first mover or not)
           ifelse isFirstMover = 1                                                              ;; ak01;(added to the original model) if the firm is the first mover, it lowers its cost by reducing its product value
                                                                                                ;; ak01;(multilpies product value by first mover advantage parameter) remember first mover advantage is a value between 0 and 1
         [set cost (kFirm * productValue * firstMoverAdvantage)                                 ;; rm01; Set the cost equal to the firm cost times the prodcut value times first mover advantage.
             set currentPrice max (list cost (currentPrice - epsilon))]                         ;; ak01;and lowers its current price by setting it equal to the maximum value of 'current price minus epsilon' or 'the cost'
       [set cost (kFirm  * productValue )                                                       ;; ak01;if the firm is not the first mover, it sets its cost equal to the firm cost times the product value
             set currentPrice max (list cost (currentPrice - epsilon))]                         ;; ak01; and lowers its current price by setting it equal to the maximum value of 'current price minus epsilon' or 'the cost'
                                                                                                ;; ak01; remember bid price can't go below cost: bid price >= cost . so the firm with lower cost bids lower price. This is the advantage the first-mover is given.
         ]
      ]
        if (updateType = "Decrease Product Value") [                                            ;; ; ak01;(added to the original model) rm01;If the update type on the interface tab is designated as "Decrease Product Value"
                                                                                                ;; ak01;(added to the original model) in "Decrease Product Value" policy, firms lower their cost by lowering their product value:(1 - VDelta) * productValue
       ifelse (ownUpRewards = [])                                                               ;; rm01;If there is no recorded value for ownUpRewards (empty list)
         [set meanUp 0]                                                                         ;; rm01;Set the meanUp value to 0, since there are no recorded values for ownUpRewards
         [set meanUp mean ownUpRewards]                                                         ;; rm01;Otherwise, set the meanUp value to the mean value of the recorded ownUpRewards
       ifelse (ownDownRewards = [])                                                             ;; rm01;If there is no recorded value for ownDownRewards (empty list)
         [set meanDown 0]                                                                       ;; rm01;Set the meanDown value to 0, since there are no recorded values for ownDownRewards
         [set meanDown mean ownDownRewards]                                                     ;; rm01;Otherwise, set the meanDown value to the mean value of the recorded ownDownRewards
       ifelse (meanUp > meanDown)                                                               ;; rm01;If the recorded value for meanUp is greater than the value of meanDown,
         [set currentPrice currentPrice + epsilon]                                              ;; rm01;Increment the current price by the value of epsilon (the change in price)
         [
           ifelse isFirstMover = 1                                                              ;; rm01;Otherwise, if first mover is on (equal to 1),
           [set cost (kFirm * (1 - VDelta) * productValue * firstMoverAdvantage)                ;; rm01;Set the cost equal to the firm cost times the change in product value times the prodcut value times first mover advantage.
             set currentPrice max (list cost (currentPrice - epsilon))]                         ;; rm01;and set the currentprice equal to the maximum value of cost times the current price less the change in price
       [set cost (kFirm * (1 - VDelta) * productValue )                                         ;; rm01;Set the cost equal to the firm cost times the change in product value times the prodcut value
             set currentPrice max (list cost (currentPrice - epsilon))]                         ;; rm01;and set the current price equal to the maximum value of cost times current price less the change in price
       ]
        ]
         if (updateType = "Decrease Cost") [                                                    ;; rm01;If the update type on the inerface tab is designated as "Decrease Cost"
                                                                                                ;; ak01;(added to the original model) in "Decrease cost" policy, firms lower their cost by setting cost as ( 1 - CDelta) * cost
      ifelse (ownUpRewards = [])                                                                ;; rm01;If there is no recorded value for ownUpRewards (empty list)
         [set meanUp 0]                                                                         ;; rm01;Set the meanUp value to 0, since there are no recorded values for ownUpRewards
         [set meanUp mean ownUpRewards]                                                         ;; ak01;Otherwise, set meanUp as the average return of the firm by bidding above the current price
       ifelse (ownDownRewards = [])                                                             ;; rm01;If there is no recorded value for ownDownRewards (empty list)
         [set meanDown 0]                                                                       ;; rm01;Set the meanDown value to 0, since there are no recorded values for ownDownRewards
         [set meanDown mean ownDownRewards]                                                     ;; ak01;Otherwise, set meanDown as the average return of the firm by bidding below the current price
       ifelse (meanUp > meanDown)                                                               ;; ak01; if, during the epoch, firm's bids above its current price returned on average more than its bids below the current price,
         [set currentPrice currentPrice + epsilon]                                              ;; rm01;Increment the current price by the value of epsilon (the change in price)
        [                                                                                       ;; ak01; if not, firm lowers its price (however, by how much the firm can lower the price depends on whether the firm is the first mover or not)
           ifelse isFirstMover = 1                                                              ;; ak01; if the firm is the first mover, it lowers its cost by reducing its product value
                                                                                                ;; ak01;(multilpies product value by first mover advantage parameter) remember first mover advantage is a value between 0 and 1
          [set cost (( 1 - CDelta) * kFirm * productValue * firstMoverAdvantage)                ;; rm01;Set the cost equal to the firm cost times the change in product cost times the product value times first mover advantage.
             set currentPrice max (list cost (currentPrice - epsilon))]                         ;; ak01;and lowers its current price by setting it equal to the maximum value of 'current price minus epsilon' or 'the cost'
       [set cost (( 1 - CDelta) * kFirm  * productValue )                                       ;; ak01;if the firm is not the first mover, it sets its cost equal to the firm cost times the change in cost ((1 - CDelta) times the product value
             set currentPrice max (list cost (currentPrice - epsilon))]                         ;; ak01; and lowers its current price by setting it equal to the maximum value of 'current price minus epsilon' or 'the cost'
                                                                                                ;; ak01; remember bid price can't go below cost: bid price >= cost.
        ]
         ]
       set upRewards []                                                                         ;; rm01;Set upRewards equal to an empty value
       set downRewards []                                                                       ;; rm01;Set downRewards equal to an empty value
       set ownUpRewards []                                                                      ;; rm01;Set ownUpRewards equal to an empty value
       set ownDownRewards []                                                                    ;; rm01;Set ownDOwnRewards equal to an empty value
       set myPricesBid []                                                                       ;; rm01;Set myPricesBid equal to an empty value
       set industryPricesBid []                                                                 ;; rm01;Set industryPricesBid equal to an empty value
       set episodeCount 0                                                                       ;; rm01; setsthe episode count to zero
      ] ; of if episodeCount = epochLength
  ]; end of if is-pAndA?
end ; of postpare-episode

;ai20170428 Be precise: here by "report" you mean "write to output".
;  It would probably be more useful to write to file instead.
to turtle-report  ;; rm01; Allows the turtles to report values of interest for the agent firms.
ask turtles [
write episodeAbsoluteCount          ;; rm01;Denote how many episodes passed
write who                           ;; rm01;Denotes the self of turtle
write [breed] of self               ;; rm01;Denotes the breed of self
write currentPrice                  ;; rm01;Denotes the current price of self turtle
write bidsWon                       ;; rm01;Denotes how many bids were won by the turtle
write totalReward                   ;; rm01;Denotes the total reward gained throughout the episodes
print " "                           ;; rm01;Prints the above values  ;ai20170428 No, j
]
end ; of turtle-report

to plot-quantity-price     ;; rm01; Creates the demand plot on the interface tab.  Allows for observation of the predetermined expected price.
set-current-plot "Demand and Monopoly Quantity & Price at 0 Costs"    ;; rm01;Sets the current plot as Demand and Monopoly Quantity & Price at 0 Costs, therefore the following commands will only affect this plot
clear-plot                                                            ;; rm01;Clears any preexisting plots
set-plot-y-range 0 priceIntercept                                     ;; rm01;Sets the y range of the plot between 0 and the price intercept value (STANDARD ECONOMIC PQ GRAPH)
set-plot-x-range 0 quantityIntercept                                  ;; rm01;Sets the x range of the plot between 0 and the quantity intercept value (STANDARD ECONOMIC PQ GRAPH)
set-current-plot-pen "demand-curve"                                   ;; rm01;Sets the currently plotting pen as the demand-curve
plot-pen-up                                                           ;; rm01;"Raises" the pen
plotxy 0 priceIntercept                                               ;; rm01;Moves the current plot pen to the point (0, priceIntercept)
plot-pen-down                                                         ;; rm01;"Lowers" the pen
plotxy quantityIntercept 0                                            ;; rm01;Moves the current plot pen to the point (quantityIntercept, 0), since the pen is down, this draws a downward sloping demand line.
set-current-plot-pen "monopoly-quantity"                              ;; rm01;Sets the current plot pen as monopoly quantity
plot-pen-up                                                           ;; rm01;"Raises" the pen
plotxy m-quantity-at0cost 0                                           ;; rm01;Moves the current plot pen to the point (m-quantity-at0cost, 0)
plot-pen-down                                                         ;; rm01;"Lowers" the pen
plotxy m-quantity-at0cost m-price-at0cost                             ;; rm01;Moves the current plot pen to the point (m-quantity-at0cost, m-price-at0cost), since the pen is down, this draws a vertical line from the x-axis to the demand curve
set-current-plot-pen "monopoly-price"                                 ;; rm01;Sets the current plot pen as monopoly-price
plot-pen-up                                                           ;; rm01;"Raises" the pen
plotxy 0 m-price-at0cost                                              ;; rm01;Moves the current plot pen to the point (0, m-price-at0cost)
plot-pen-down                                                         ;; rm01;"Lowers" the pen
plotxy m-quantity-at0cost m-price-at0cost                             ;; rm01;Moves the current plot pen to the point (m-quantity-at0cost, m-price-at0cost), since the pen is down, this draws a horizontal line from the y-axis to the demand curve
end


to debug
;; rm01;Allows us to transform the variable lexicon into plainspeak for observers of the model
;ai20170428: That ^ is getting closer to a helpful comment.  Add a little more detail.
let probe 0                                                                      ;; rm01;Initializes value of probe equal to 0
set probe (random-float 2 * deltaFirm0) - deltaFirm0 + current-price-firm-1      ;; rm01;Sets value of probe equal to a a random value dependent on the range of the price of firm 0 + the current price of firm 1
show (word "a = "  quantityIntercept)                                            ;; rm01;Displays "a= value of the quantity intercept"
show (word "price intercept = "  priceIntercept)                                 ;; rm01;Displays "price intercept = the value of the price intercept"
show (word "slope = " dSlope)                                                    ;; rm01;Displays "slope = the value of dSLope"
show (word "monopoly price = "  m-price)                                         ;; rm01;Displays "monopoly price = the value of m-price"
show (word "monopoly demand = "  (quantityIntercept - dSlope * m-price))         ;; rm01;Displays "monopoly demand = the quantityintercept - (dslope*m-price)"
show (word "current-price = " current-price-firm-1)                              ;; rm01;Displays "current price = the current price of firm 1"
show (word "probe = " probe)                                                     ;; rm01;Displays "probe = probe"
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;REPORTERS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;ai20170428: For the most part, your comments are just repeating the code.
;  Try to say precisely what this function is accomplishing.
;  Separately, it would be nice to cut back on the use of globals.
;  (Recall our classroom discussion of pure functions.)
;; New for oligopoly, price setting
to-report Quantity-Demanded [daPrice]                 ;; rm01;if the price is too high, demand is zero
  let bob  quantityIntercept - (dSlope * daPrice)     ;; rm01;Set bob equal to the quantty intercept - (dSLope*dPrice)
  let carol (list 0 bob)                              ;; rm01;Set carol as a list of 0 and the value for bob
  report max carol                                    ;; rm01;Choose the maximum value from the list, carol
end ; of Quantity-Demanded


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;TESTS

;ai20170428 Missing tests.  (See code submission guidelines.)
;ai20170428: This one test is a good idea, but it is not adequate test coverage.
to testGlobals    ;; rm01; added to the originial model
  if (episodeAbsoluteCount != 0)      [error "WRONG EPISODEABSOLUTECOUNT"]
  if (runningAverageBidList != [])    [error "WRONG RUNNINGAVERAGEBIDLIST"]
  if (quantityIntercept != qIBase)    [error "WRONG QUANTITYINTERCEPT"]
  if (priceIntercept != quantityIntercept / dSlope)           [error "WRONG MPRICEAT0COST"]
  if (m-price-at0cost != quantityIntercept / (2 * dSlope))    [error "WRONG MPRICEAT0COST"]
  if (m-quantity-at0cost != (quantityIntercept - (dSlope * m-price-at0cost)))  [error "WRONG MQUANTITYAT0COST"]
end


@#$#@#$#@
GRAPHICS-WINDOW
363
128
558
324
-1
-1
17.0
1
10
1
1
1
0
1
1
1
-5
5
-5
5
1
1
1
ticks
30.0

SLIDER
288
404
471
437
qIBase
qIBase
0
1000
222.0
1
1
NIL
HORIZONTAL

SLIDER
117
404
289
437
dSlope
dSlope
0
5
0.6
0.01
1
NIL
HORIZONTAL

BUTTON
2
10
68
43
Setup
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
2
239
174
272
initialPriceFirm0
initialPriceFirm0
0
300
222.0
1
1
NIL
HORIZONTAL

SLIDER
2
109
174
142
deltaFirm0
deltaFirm0
0
5
3.0
0.1
1
NIL
HORIZONTAL

SLIDER
2
43
174
76
epsilonFirm0
epsilonFirm0
0
1
0.7
0.1
1
NIL
HORIZONTAL

PLOT
516
469
964
646
Demand and Monopoly Quantity & Price at 0 Costs
Q, quantity
P, price
0.0
200.0
0.0
400.0
true
true
"" ""
PENS
"demand-curve" 1.0 0 -16777216 true "" ""
"monopoly-quantity" 1.0 0 -2674135 true "" ""
"monopoly-price" 1.0 0 -10899396 true "" ""

BUTTON
68
10
131
43
Go
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
175
10
991
347
Current Bid Price
NIL
NIL
0.0
10.0
197.0
203.0
true
true
"" ""
PENS
"Winning Bid" 1.0 0 -2674135 true "" ""
"Monopoly Price at 0 Costs" 1.0 0 -16777216 true "" ""
"Monopoly Price, Firm 0" 1.0 0 -11221820 true "" ""
"Monopoly Price, Firm(s) 1" 1.0 0 -5825686 true "" ""

SLIDER
514
437
686
470
aDelta
aDelta
0
1
0.3
0.01
1
NIL
HORIZONTAL

SWITCH
356
437
515
470
random-walk-as
random-walk-as
0
1
-1000

SLIDER
2
76
174
109
epsilonFirm1
epsilonFirm1
0
1.0
0.7
0.1
1
NIL
HORIZONTAL

SLIDER
2
142
174
175
deltaFirm1
deltaFirm1
0
5
3.0
0.1
1
NIL
HORIZONTAL

SLIDER
2
272
174
305
initialPriceFirm1
initialPriceFirm1
0
300
222.0
1
1
NIL
HORIZONTAL

CHOOSER
481
393
599
438
daRandomSeed
daRandomSeed
0 1 17 100 "system clock"
4

SLIDER
2
305
174
338
epochLengthFirm0
epochLengthFirm0
0
200
10.0
1
1
NIL
HORIZONTAL

SLIDER
2
338
174
371
epochLengthFirm1
epochLengthFirm1
0
200
31.0
1
1
NIL
HORIZONTAL

CHOOSER
502
347
640
392
numPandAFirms
numPandAFirms
1 2 3 4 5 6 8 10 12 14 16 18 20
1

SLIDER
2
174
174
207
kFirm0
kFirm0
0
100
10.0
1
1
NIL
HORIZONTAL

SLIDER
2
206
174
239
kFirm1
kFirm1
0
100
10.0
1
1
NIL
HORIZONTAL

SLIDER
1
370
174
403
reportEveryNEpisodes
reportEveryNEpisodes
0
100
1.0
1
1
NIL
HORIZONTAL

BUTTON
640
347
724
380
Run N Times
let bob numEpisodesTorun\nwhile [bob > 0]\n[;go\nset bob bob - 1\n]
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
599
396
792
429
numEpisodesToRun
numEpisodesToRun
10000
100000
100000.0
5000
1
NIL
HORIZONTAL

MONITOR
726
347
829
392
NIL
avgWinningBid
3
1
11

SWITCH
2
404
118
437
reporting
reporting
1
1
-1000

MONITOR
829
347
961
392
NIL
runningAverageBid
3
1
11

MONITOR
3
437
173
482
Monopoly Price for firm 0
(quantityIntercept + [unitProductionCost] of pAndA 0 * dSlope) / (2 * dSlope)
3
1
11

MONITOR
172
437
358
482
Monopoly Price for firm(s) 1
(quantityIntercept + [unitProductionCost] of pAndA 1 * dSlope) / (2 * dSlope)
3
1
11

CHOOSER
831
256
1082
301
updateTypeFirm0
updateTypeFirm0
"Market Returns" "Decrease Product Value" "Decrease Cost"
0

CHOOSER
831
301
1082
346
updateTypeFirm1
updateTypeFirm1
"Market Returns" "Decrease Product Value" "Decrease Cost"
0

PLOT
3
486
490
684
Current Base Prices
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"Agent0" 1.0 0 -11221820 true "" ""
"Agent1" 1.0 0 -5825686 true "" ""
"Agent2" 1.0 0 -7500403 true "" ""
"Agent3" 1.0 0 -13345367 true "" ""
"Agent4" 1.0 0 -10899396 true "" ""
"Agent5" 1.0 0 -2674135 true "" ""
"Agent6" 1.0 0 -955883 true "" ""
"Agent7" 1.0 0 -6459832 true "" ""
"Agent8" 1.0 0 -1184463 true "" ""
"Agent9" 1.0 0 -8630108 true "" ""

SWITCH
685
437
807
470
variableStarts
variableStarts
0
1
-1000

SLIDER
807
437
963
470
variableFactor
variableFactor
0
1.0
0.1
0.01
1
NIL
HORIZONTAL

MONITOR
175
346
330
391
Agent0 Epoch Ave. Reward
[runningAverageReward] of turtle 0
0
1
11

MONITOR
329
346
481
391
Agent1 Epoch Ave. Reward
[runningAverageReward] of turtle 1
0
1
11

MONITOR
491
647
978
692
NIL
version
3
1
11

MONITOR
990
58
1147
103
Firm 0's Current Price
[currentPrice] of turtle 0
3
1
11

MONITOR
1149
59
1306
104
Firm 1's Current Price
[currentPrice] of turtle 1
3
1
11

MONITOR
992
106
1149
151
Firm 2's Current Price
[currentPrice] of turtle 2
3
1
11

MONITOR
1151
106
1308
151
Firm 3's Current Price
[currentPrice] of turtle 3
3
1
11

SLIDER
767
701
939
734
productValue0
productValue0
0
10
5.0
1
1
NIL
HORIZONTAL

SLIDER
941
700
1113
733
productValue1
productValue1
0
10
5.0
1
1
NIL
HORIZONTAL

SLIDER
38
692
210
725
VDelta0
VDelta0
0
1
0.2
1
1
NIL
HORIZONTAL

SLIDER
220
696
392
729
VDelta1
VDelta1
0
1
0.2
1
1
NIL
HORIZONTAL

SLIDER
406
698
578
731
CDelta0
CDelta0
0
1
0.1
1
1
NIL
HORIZONTAL

SLIDER
588
701
760
734
CDelta1
CDelta1
0
1
0.1
1
1
NIL
HORIZONTAL

MONITOR
992
11
1147
56
Who was the first mover?
firstMover\n;; rm01; added to the originial model
17
1
11

MONITOR
1020
204
1133
249
Firm 0's Cost
[cost] of turtle 0
17
1
11

MONITOR
1151
204
1264
249
Firm 1's Cost
[cost] of turtle 1
17
1
11

SLIDER
832
223
1014
256
firstMoverAdvantage
firstMoverAdvantage
0
1
1.0
.1
1
NIL
HORIZONTAL

MONITOR
1149
11
1313
56
First Mover's Current Price
[currentPrice] of firstMover\n;; rm01; added to the originial model
3
1
11

SLIDER
790
396
962
429
runningAvgLength
runningAvgLength
0
100
50.0
1
1
NIL
HORIZONTAL

BUTTON
1346
10
1501
43
NIL
testGlobals
NIL
1
T
TURTLE
NIL
NIL
NIL
NIL
1

MONITOR
1307
152
1379
197
Bid Winner
daWinner\n;; rm01; added to the originial model
17
1
11

MONITOR
1151
153
1279
198
Bids Won by Firm 1
[bidsWon] of turtle 1\n;; rm01; added to the originial model
17
1
11

MONITOR
994
152
1122
197
Bids Won by Firm 0
[bidsWon] of turtle 0\n;; rm01; added to the originial model
17
1
11

MONITOR
1089
261
1261
306
Firm 0's Epoch Profit
[profit] of turtle 0\n;; rm01; added to the originial model
17
1
11

MONITOR
1268
260
1422
305
Firm 1's Epoch Profit
[profit] of turtle 1\n;; rm01; added to the originial model
17
1
11

@#$#@#$#@
## MISSING UPDATE

ai: Here is where you should give a description of what you changed.  You can keep Kimbrough's comments below your own, but you need to include your own comments as well.

## NOTE WELL

The documentation below is incomplete.  It is in draft form only.  What is here, however, is correct (or so I think).

## HOW TO RUN THE MODEL

1. Set values for the various sliders and switches (in green) on the Interface tab.

The default values should be OK, so you can skip this step at first.

2. Click the Setup button.

3. Click the Go button.

The Go button is an "always" button, so the run will continue until you stop it, which you can do by clicking the Go button again.

## WHAT DO YOU SEE WHEN YOU RUN THE MODEL?

Most prominently you see an active plot entitled "Current Bid Price".  The simulated market proceeds in episodes in which each participating firm offers to supply the entire demand of the market at a bid price.  The market selects the price of the lowest bidder, and a new episode begins.

What you see plotted are two things.  In black is the monopolist's price, given the (linear) demand curve specified via a number of sliders (discussed below), PROVIDING THE MONOPOLIST HAS 0 COSTS.  This is just: m-price = quantityIntercept / (2 * dSlope).
quantityIntercept is given initially by the qIBase slider on the Interface tab. dSlope is given by the dSlope slider.

In red on the plot is the lowest price bid in the episode.  Also plotted are the monopoly price of Firm0 (agent 0) given its costs (this is in cyan) and the monopoly price of Firm1 (agent 1) given its costs (this is in magenta).

In addition to the "Current Bid Price" plot there are several monitors that report information during a run ...

1. Monopoly Price for firm 0

This is the numerical value of the cyan line in the above plot.

2. Monopoly Price for firm(s) 1

This is the numerical value of the magenta line in the above plot.

3.  avgWinningBid

This is the running average over all the episodes in the run of the (price) value of the winning bid.

4. runningAverageBid

This is the average of the (price) value of the winning bid for the last N episodes. N is set by the slider runningAvgLength.  (At the inception of the run, before N episodes have been completed, runningAverageBid is just the average winnng price of the episodes that have been completed.)

## WHAT ARE THE UPDATETYPE CHOOSERS ABOUT?

On the Interface tab you will find two choosers for selecting update policies to be used by the agents.  "updateTypeFirm0" sets the policy for firm (agent) 0. "updateTypeFirm1" sets the policy for all higher-numbered firms (agents).

See the procedure Postpare-Episode for the implementation of these policies.  The procedure Observe-and-Record is also important.

In brief, "Own Returns" is a myopic and selfish policy. At the end of the agent's epoch the agent raises or lowers its current price (about which it bids randomly using Probe and Adjust) in the direction of its own returns exclusively. In particular, if the agent fails to win the bid during an episode, it records a 0 reward for that episode. Then, if its bids during the epoch about its current price returned on average more than its bids below the current price, then the agent raises its price; and similarly for lowering the price.

Using the "Market Returns" policy, the agent takes the industry view and records what it would have received had it won the bid. Then, if its bids during the epoch about its current price would have returned on average (had it won each of them) more than its bids below the current price, then the agent raises its price; and similarly for lowering the price.

Using the "Market Returns and Own Returns" policy, the agent first determines whether it has won any bids during the epoch. If not, it lowers its current price. If it has won some bids, then the agent applies the "Market Returns" policy.

## WHAT DO THE MONITORS REPORT?

The monitor labelled "Agent0 Epoch Ave. Reward" displays the average reward per episode received by firm 0 (agent 0) during the agent's previous epoch.  During an epoch, each agent records the reward it achieved for each episode. If the agent did not win the bid, the reward is 0, otherwise it is the usual: price * quantity - cost * quantity.  See the procedure Observe-and-Record.  At the end of an epoch, the agent averages its rewards over the episodes in the epoch and this is what is reported in the monitor for agent 0. See the procedure Postpare-Episode.

The monitor labelled "Agent1 Epoch Ave. Reward" works just like "Agent0 Epoch Ave. Reward", but for firm (agent) 1.

## WHAT DO THE SLIDERS, SWITCHES, AND CHOOSERS DO?

They set parameters, global variables, for the run of the model.

    numPandAFirms

This model, oligopolyBidPrice.nlogo, is about markets in which there are a few firms competing to supply the entire demand in a winner-take-all-by-episode market. This chooser lets the user set the number of firms in the market who will be using Probe and Adjust (explained below) to arrive at their bids.

Note that the firm-specific parameters, set by sliders to the left of the plot, all end in ...Firm0 or ...Firm1.  If there are two firms in the run, i.e., if numPandAFirms is set to 2, they are called firm 0 and firm 1, and are assigned the parameter values as indicated.  If there is one firm in the model, it gets the parameter values ...Firm0. Finally, if there are 3 or more firms in the model, they ALL get the parameter values ...Firm0.

/* more to do here*/

## WHAT IS IT?

This model explores how oligopolists might discover their prices in the face of, here, a linear demand function. The discovery process does not allow explicit coordination or collusion between the duopolists.  Instead, a main purpose of this model is to investigate the potential for tacit collusion by the duopolists, using information gained from repeated interactions in the market.

In this model, the firms adjust prices. During each market episode, each firm announces its unit price.  The market satisfies its entire demand at the lower of the two prices (or randomly in the case of a tie).  This is, then, a winner-take-all market during each episode.

Prices are being adjusted and price is a continuous variable. The firms in this program use what I call a "Probe and Adjust" model, and a very simple one at that.

    1.  set

 Each agent begins with a current value, here currentPriceFirmX (X = 1, 2), set by initialPriceFirmX in the Interface tab. Play is organized by epochs, each containing a number of episodes. In each episode, the agent probes its market by offering a price in the range [currentPriceFirmX - deltaFirmX, currentPriceFirmX + deltaFirmX]. The agent records the revenue it gets from the probe, classifying the revenue as coming from a price offered that is either above or below its current price. When the epoch is over (a number of episodes, set by epochLengthFirmX in the Interface tab), the agent determines whether on average it did better by offering more than its current price or less. Depending on the results, it increases its currentPriceFirmX by epsilonFirmX or decreases it by epsilonFirmX. Then a new epoch begins. Both deltaFirmX and epsilonFirmX are set in the Interface. The key point about this model is that neither agent has access to the demand function, except as it responds to prices placed to the market.

## MONOPOLY PRICE-SETTING

Introductory textbooks in microeconomics will tell a story roughly as follows. The market's demand, Q (think: quantity demanded), for our product is a linear function of its price, P:

(1)  Q(P) = Q = c - dSlope*P

Here, c is a constant, representing the quantity demanded when price is 0 and, since we are linear, the price point at which demand disappears when the price is too high (when dSlope*P = c). (We assume that c, dSlope > 0, so -dSlope < 0.)

Let us assume that the cost of production (for the monopolist) is k*Q

The profit, pi, made by the monopolist is P*Q (since the cost of production is 0, we need only account for revenue, which is defined here as P*Q).

(3) pi = P*Q = (a - dSlope*Q)*Q = a*Q - dSlope*Q**2

The monopolist will seek to maximize pi, which can be done by a simple exercise with the calculus.

(4) dpi/dQ = a - 2*dSlope*Q

Setting a - 2*dSlope*Q to zero and solving for Q yields

(5) Q = a/(2*dSlope)

So Q in (5) is Q*, the optimal quantity for the monopolist to put on the market.

(Checking that d**2 pi/dQ**2 = -2*dSlope < 0 verifies that we indeed have found a maximum.)

See, for example, _Intermediate_Microeconomics:_A_Modern_Approach_, 6th ed., by Hal. R. Varian. I draw explicitly on _Microeconomic_Theory_, 2nd ed., 1978, The Dryden Press, Hinsdale, Illinois, by Walter Nicholson. See "Appendix to Chapter 13: Models of Interdependent Output Decisions," pp. 389-398. Nicholson's is the more advanced text. My treatment generalizes his discussion.  Kreps's _A_Course_in_Microeconomic_Theory_, 1990, is excellent and more advanced.


## HOW IT WORKS

There are two key passages from the code. First this:

    ; 4. All of the turtles observe the winning bid (daWinningBid)
    ; and the resulting quantity demanded (daQuantityDemanded).
    ; All of the turtles record the profit they would have had (or did have
    ; in the case of the winner) as either upProfits or downProfits, profits they
    ; got or could have gotten by bidding high or low.  More carefully put,
    ; each firm observes the winning bid and resulting demand, then calculates
    ; the reward/profit it would have had had it won with that bid. This reward
    ; is recorded in the list of upRewards or downRewards depending on whether
    ; daWinningBid is > or <= currentPrice. Now all this applies only to turtles
    ; of breed pAndA.
    ask turtles [observe-and-record(daWinningBid)(daQuantityDemanded)(daWinner)]

and this:

    to Observe-And-Record [price quantity daWinner]
      if (is-pAndA? self) [
        let reward price * quantity - cost * quantity
        ifelse (price > currentPrice)
          [set upRewards lput reward upRewards]
          [set downRewards lput reward downRewards]
        ifelse (priceBid > currentPrice)
          [ifelse (daWinner = self)
           [set ownUpRewards lput reward ownUpRewards]
           [set ownUpRewards lput 0 ownUpRewards]
           ]
          [ifelse (daWinner = self)
           [set ownDownRewards lput reward ownDownRewards]
           [set ownDownRewards lput 0 ownDownRewards]
           ]
       ] ; end of if is-pAndA?
    end ; of observe-and-record

Second, is this:

    to Postpare-Episode
      if (is-pAndA? self) [
        let meanUp 0
        let meanDown 0
        if (episodeCount >= epochLength) ; then update and reset
          [if (updateType = "Market Returns") [
           ifelse (upRewards = [])
             [set meanUp 0]
             [set meanUp mean upRewards]
           ifelse (downRewards = [])
             [set meanDown 0]
             [set meanDown mean downRewards]
           ifelse (meanUp > meanDown)
             [set currentPrice currentPrice + epsilon]
             [set currentPrice currentPrice - epsilon]
           ] ; end of if (updateType = "Market Returns")
           if (updateType = "Own Returns") [
           ifelse (ownUpRewards = [])
             [set meanUp 0]
             [set meanUp mean ownUpRewards]
           ifelse (ownDownRewards = [])
             [set meanDown 0]
             [set meanDown mean ownDownRewards]
           ifelse (meanUp > meanDown)
             [set currentPrice currentPrice + epsilon]
             [set currentPrice max (list delta (currentPrice - epsilon))]
           ] ; end of if (updateType = "Own Returns")
           set upRewards []
           set downRewards []
           set ownUpRewards []
           set ownDownRewards []
           set episodeCount 0
          ] ; of if episodeCount = epochLength
       ] ; end of if is-pAndA?
    end ; of postpare-episode

Something to add would be an analog of the cautious policy in oligopolyBidQuantity.nlogo. Here, the firm needs to protect itself from others continually giving low bids.  So this needs to be added.

The runs, and the players' learning regimes, are organized into epochs, which consist of a number of episodes or rounds of play.  Each agent keeps its own epochLength. When an epoch begins, a probe-and-adjust player has a currentPrice which stays constant throughout the epoch.

After each episode (round) of play, the Postpare-Episode procedure is called for each agent.
For a given agent, its epoch continues until its episodeCount reaches the agent's epochLength. If the agent's epoch is in fact over, the code in Postpare-Episode comes into play for updating the agent's currentPrice and resetting the epoch accumulators.



## THINGS TO NOTICE

With two firms (agents), i.e. duopoly and the standard setting (see THINGS TO TRY) and both agents  using the policy of "Market Returns", the price converges to the monopoly price of the lower cost agent.

How robust is this result? What does it take to change it?

What does this suggest about business stragey?

## THINGS TO TRY

Here's a nice standard setting: kFirm0=10, kFirm1=5, epochLengthFirm0 = epochLengthFirm1 = 30. initialPriceFirm0 = 193, initialPriceFirm1 = 220. dSlope=0.5, qIBase=200. variableStarts=On.  variableFactor=0.10.
Try this with numPandAFirms = 1, 2, 3, 4, 5, 6, 8, 10. What happens?
Try varying epoch lengths. What happens? Be sure to mix up "Market Returns" and "Own Returns".

## EXTENDING THE MODEL

This section could give some ideas of things to add or change in the procedures tab to make the model more complicated, detailed, accurate, etc.

## NETLOGO FEATURES

This section could point out any especially interesting or unusual features of NetLogo that the model makes use of, particularly in the Procedures tab.  It might also point out places where workarounds were needed because of missing features.

## RELATED MODELS

This section could give the names of models in the NetLogo Models Library or elsewhere which are of related interest.

Created 2007-03-25 from oligopolyProbeAndAdjust.nlogo.

## CREDITS AND REFERENCES

This model was created and written by Steven O. Kimbrough: kimbrough at wharton.upenn.edu and http://opim.wharton.upenn.edu/~sok/.  The model is
freely downloadable at:
http://opim.wharton.upenn.edu/~sok/AGEbook/nlogo/OligopolyBidPrice.nlogo

Please give me credit if you use this model or program.

To refer to this model in academic publications, please use: Kimbrough, Steven O. (2011). Oligopoly Bid Price
model. http://opim.wharton.upenn.edu/~sok/AGEbook/nlogo/OligopolyBidPrice.nlogo University of Pennsylvania, Philadelphia, PA 19004, USA.
In other publications, please use: Copyright 2011 Steven O. Kimbrough. All
rights reserved.

Version information: $Id: OligopolyBidPrice.nlogo 4440 2014-11-12 04:53:59Z sok $
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

link
true
0
Line -7500403 true 150 0 150 300

link direction
true
0
Line -7500403 true 150 150 30 225
Line -7500403 true 150 150 270 225

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="experiment1" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>ticks &gt; 2000</exitCondition>
    <metric>runningAverageBid</metric>
    <enumeratedValueSet variable="numEpisodesToRun">
      <value value="5000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reportEveryNEpisodes">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="variableFactor">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="deltaFirm1">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="epsilonFirm0">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qIBase">
      <value value="222"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reporting">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="epochLengthFirm0">
      <value value="10"/>
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="runningAvgLength">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="daRandomSeed">
      <value value="&quot;system clock&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="epsilonFirm1">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dSlope">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="updateTypeFirm0">
      <value value="&quot;Market Returns s.t. Own Returns&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initialPriceFirm1">
      <value value="193"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kFirm1">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="aDelta">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="updateTypeFirm1">
      <value value="&quot;Market Returns s.t. Own Returns&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-walk-as">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numPandAFirms">
      <value value="2"/>
      <value value="4"/>
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kFirm0">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initialPriceFirm0">
      <value value="193"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="deltaFirm0">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="epochLengthFirm1">
      <value value="10"/>
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="variableStarts">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment2" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>ticks &gt; 8000</exitCondition>
    <metric>runningAverageBid</metric>
    <enumeratedValueSet variable="numEpisodesToRun">
      <value value="5000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reportEveryNEpisodes">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="variableFactor">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="deltaFirm1">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="epsilonFirm0">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qIBase">
      <value value="222"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reporting">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="epochLengthFirm0">
      <value value="30"/>
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="runningAvgLength">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="daRandomSeed">
      <value value="&quot;system clock&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="epsilonFirm1">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dSlope">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="patienceFirm1">
      <value value="0.9"/>
      <value value="1"/>
      <value value="1.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="updateTypeFirm0">
      <value value="&quot;Market Returns s.t. Own Returns&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initialPriceFirm1">
      <value value="193"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kFirm1">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="patienceFirm0">
      <value value="0.9"/>
      <value value="1"/>
      <value value="1.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="aDelta">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="updateTypeFirm1">
      <value value="&quot;Market Returns s.t. Own Returns&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-walk-as">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numPandAFirms">
      <value value="6"/>
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kFirm0">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initialPriceFirm0">
      <value value="193"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="deltaFirm0">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="epochLengthFirm1">
      <value value="30"/>
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="variableStarts">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="rm01;; Average Bid Price Market Returns" repetitions="4" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>avgWinningBid</metric>
    <metric>[currentPrice] of turtle 0</metric>
    <metric>[currentPrice] of turtle 1</metric>
    <enumeratedValueSet variable="numPandAFirms">
      <value value="2"/>
      <value value="3"/>
      <value value="4"/>
      <value value="5"/>
      <value value="6"/>
    </enumeratedValueSet>
    <steppedValueSet variable="firstMoverAdvantage" first="0" step="0.5" last="1"/>
    <enumeratedValueSet variable="numEpisodesToRun">
      <value value="5000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="updateTypeFirm0">
      <value value="&quot;Market Returns&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="updateTypeFirm1">
      <value value="&quot;Market Returns&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numEpisodesToRun">
      <value value="100000"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="rm01;; Average Bid Price Decrease Cost" repetitions="4" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>avgWinningBid</metric>
    <metric>[currentPrice] of turtle 0</metric>
    <metric>[currentPrice] of turtle 1</metric>
    <enumeratedValueSet variable="numPandAFirms">
      <value value="2"/>
      <value value="3"/>
      <value value="4"/>
      <value value="5"/>
      <value value="6"/>
    </enumeratedValueSet>
    <steppedValueSet variable="firstMoverAdvantage" first="0" step="0.5" last="1"/>
    <enumeratedValueSet variable="numEpisodesToRun">
      <value value="5000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="updateTypeFirm0">
      <value value="&quot;Decrease Cost&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="updateTypeFirm1">
      <value value="&quot;Decrease Cost&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numEpisodesToRun">
      <value value="100000"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="rm01;; Average Bid Decrease Product Value" repetitions="4" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>avgWinningBid</metric>
    <metric>[currentPrice] of turtle 0</metric>
    <metric>[currentPrice] of turtle 1</metric>
    <enumeratedValueSet variable="numPandAFirms">
      <value value="2"/>
      <value value="3"/>
      <value value="4"/>
      <value value="5"/>
      <value value="6"/>
    </enumeratedValueSet>
    <steppedValueSet variable="firstMoverAdvantage" first="0" step="0.5" last="1"/>
    <enumeratedValueSet variable="numEpisodesToRun">
      <value value="5000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="updateTypeFirm0">
      <value value="&quot;Decrease Cost&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="updateTypeFirm1">
      <value value="&quot;Decrease Cost&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numEpisodesToRun">
      <value value="100000"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
