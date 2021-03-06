(defnode start)
(defnode have-5)
(defnode have-10)
(defnode have-15)
(defnode have-20)
(defnode have-25)          
(defnode end)

(defarc start   nickel       have-5  "Clunk!")
(defarc start   dime         have-10 "Clink!")
(defarc start   quarter      have-25 "Ker-chunk!")
(defarc start   coin-return  start   "Nothing to return.")
(defarc have-5  nickel       have-10 "Clunk!")
(defarc have-5  dime         have-15 "Clink!")
(defarc have-5  coin-return  start   "Returned five cents.")
(defarc have-10 nickel       have-15 "Clunk!")
(defarc have-10 dime         have-20 "Clink!")
(defarc have-10 coin-return start   "Returned ten cents.")
(defarc have-15 nickel       have-20 "Clunk!")
(defarc have-15 dime         have-25 "Clink.")
(defarc have-15 gum-button   end     "Deliver gum.")
(defarc have-15 coin-return  start "Returned fifteen cents.")
(defarc have-20 nickel       have-25 "Clunk!")
(defarc have-20 dime         have-25 "Return nickel.")
(defarc have-20 gum-button   end  
                               "Deliver gum, nickel change.")
(defarc have-20 mint-button  end     "Deliver mints.")
(defarc have-20 coin-return  start  "Returned twenty cents.")
(defarc have-25 nickel       have-25 "Nickel returned.")
(defarc have-25 dime         have-25 "Dime returned.")
(defarc have-25 gum-button   end     "Deliver gum, dime change.")
(defarc have-25 mint-button  end     "Deliver mint, nickel change.")
(defarc have-25 chocolate-bar-button end "Deliver chocolate bar.")
(defarc have-25 coin-return start "Returned 25 cents.")