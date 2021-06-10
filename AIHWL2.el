; By Lauren Dennedy and Matthew Stanford
; gr5743 & gd9687

; If the plaintext does not work,
; go to this link: https://repl.it/@IvoInstructor/Connect-3#main.el

; For this assignment, we wrote our code in Emacs Lisp, a dialect of Lisp.
; In order to see the display of the functions, you must type them into the console.
; Like this: "(connect-3)" for example.
; No quotation marks.
; Note that the big green "play" button must be clicked first if using repl.it.

; General Overview:

; This connect-3 game lets the human player interact with an AI to
; play a modified version of connect-4.
; The player's turns are marked as "X" and the AI's turns are marked as "O"
; The player uses console I/O to make their turn. The AI uses the minimax
; algorith to make its turn.

; Game Elements:

; 1 (connect-3) - The starter function for the game. Handles all game instances while program is running.
; 2 (connect-3-game) - The main game loop. Is called each time a new game starts.
; 3 (print-board) - Formats the board to be printed on the console.
; 4 (update-board) - Updates the current game state by modifying the board argument.
; 5 (player-1-turn) - Handles console I/O for player 1's turn.
; 6 (player-2-turn) - Handles console I/O for player 2's turn, currently unused, see other notes section.

; AI Elements:

; 7 (get-moves) - Same as actions. Returns a list of integers for the columns that the AI can place a mark.
; 8 (simulate-turn) - Same as results. Creates a new list for the resulting game state based on an action.
; 9 (terminal-test) - Tests to see if the board is in a terminal state.
;10 (utility) - Returns a utility value based on the terminal state.
;11 (minimax) - Our minimax algorithm that uses all of these functions.
;12 (AI-turn) - A parent handler for the minimax algorithm to return a move back to the main game loop.

; State Representation/Encoding Scheme:

; Board states are represented as a list of characters. Each "spot" on the board can
; be either an "X", and "O", or " ". When the game starts, the board is initalized
; to a list of 16 space characters, or " ". An action is represented as an integer,
; which can be either 0, 1, 2, or 3, representing the index of which column to place
; a mark in. A function to return a list of possible actions given available spaces on
; the board is defined as (get-moves). Players take turns placing an "X" or an "O" into
; one column based on the action they chose.

; Issues:

; The game runs and the AI is able to formulate its turn in a reasonable time. The AI also uses all of
; the components of the minimax algorithm required. Minimax, Utility, Terminal-Test, Actions, and Results
; are all implemented. However, we had issues getting results from the AI that actually seem intelligent
; or strategic/competent. The AI chooses to fill the board in a seemingly "left-justified" manner.
; With more time and familiarity in Lisp/Elisp, we would debug our minimax algorithm to find out why
; this happens.

; Other Notes:

; There is a vestigial function for a "player-2-turn", to make the game a 2-player
; game. This is because we first started writing this project with the idea to make it a
; 2-player game first, so all of the game elements were working and interacting with eachother
; correctly. Then once we confirmed the game worked as a 2 player game, we switched player-2
; to be the AI. The game can still be run as a 2 player game if you uncomment the player-2 call
; inside (connect-3-game) and comment the call to (AI-turn) right after it.
; This code was left in for us to test certain functions and game win conditions while
; creating the AI. We decided to leave it in our final submission to show our working process.

; 1 (connect-3) - The starter function for the game. Handles all game instances while program is running.

(defun connect-3()

    (princ "Welcome to Connect-3!")
    (terpri)
    (princ "This is a modified version of Connect-4,")
    (terpri)
    (princ "where you have a board that is 4x4 spaces")
    (terpri)
    (princ "and you need 3 marks in a row to win.")
    (terpri)
    (princ "You are X's. You and the AI will take turns")
    (terpri)
    (princ "placing either an X or an O into a column")
    (terpri)
    (princ "on the board.")
    (terpri)

    (connect-3-game (list " " " " " " " " " " " " " " " " " " " " " " " " " " " " " " " "))

    (terpri)
    (princ "Game Over.")
    (terpri)

    (if (y-or-n-p "Would you like to play again?: ")
        (progn
            (princ "Game is restarting...")
            (terpri)
            (connect-3)
        )
        (progn
            (princ "Thank you for playing!")
            (terpri)
            (princ "Quitting Connect-3...")
            (terpri)
        )
    )
)

; 2 (connect-3-game) - The main game loop. Called each time a new game starts.

(defun connect-3-game(board)

    (setq continue-game 1)

    (catch 'result
        (while (= 1 continue-game)
            (progn
                (print-board board)
                (player-1-turn board)
                (if (terminal-test board "X")
                    (progn
                        (print-board board)
                        (princ "Player 1 wins!")
                        (terpri)
                        (setq continue-game 0)
                        (throw 'result t)
                    )
                )
                (print-board board)
                ;(player-2-turn board)
                (AI-turn board)
                (if (terminal-test board "O")
                    (progn
                        (print-board board)
                        (princ "AI wins!")
                        (terpri)
                        (setq continue-game 0)
                        (throw 'result t)
                    )
                )
            )
        )
    )
)

; 3 (print-board) - Formats the board to be printed on the console.

(defun print-board(board)
    (princ " 1 2 3 4 ")
    (terpri)
    (princ "╔═╦═╦═╦═╗")
    (terpri)
    (princ (format "║%1$s║%2$s║%3$s║%4$s║" (nth 0 board) (nth 1 board) (nth 2 board) (nth 3 board) ))
    (terpri)
    (princ "╠═╬═╬═╬═╣")
    (terpri)
    (princ (format "║%1$s║%2$s║%3$s║%4$s║" (nth 4 board) (nth 5 board) (nth 6 board) (nth 7 board) ))
    (terpri)
    (princ "╠═╬═╬═╬═╣")
    (terpri)
    (princ (format "║%1$s║%2$s║%3$s║%4$s║" (nth 8 board) (nth 9 board) (nth 10 board) (nth 11 board) ))
    (terpri)
    (princ "╠═╬═╬═╬═╣")
    (terpri)
    (princ (format "║%1$s║%2$s║%3$s║%4$s║" (nth 12 board) (nth 13 board) (nth 14 board) (nth 15 board) ))
    (terpri)
    (princ "╚═╩═╩═╩═╝")
    (terpri)
)

; 4 (update-board) - Updates the current game state by modifying the board argument.

(defun update-board(board player column-input)
    (setq index (+ 12 column-input))
    (if (string= " " (nth index board))
        (setf (nth index board) player)
        (update-board board player (- index 16))
    )
    board
)

; 5 (player-1-turn) - Handles console I/O for player 1's turn.

(defun player-1-turn(board)

    (terpri)
    (princ "It is player 1s turn (X)")
    (terpri)
    (princ "Which column do you want to place your piece in? 1-4: ")
    (setq player-1 (ignore-errors (read)))
    (terpri)

    ; Confirm input is a matching column
    (while (not (member player-1 '(1 2 3 4)))
        (progn
            (terpri)
            (princ "Please type a number between 1-4: ")
            (setq player-1 (ignore-errors (read)))
        )
    )

    ; Confirm input is not full
    (while (not (string= " " (nth (- player-1 1) board)))
        (progn
            (terpri)
            (princ "That column is full. Please choose another spot: ")
            (setq player-1 (ignore-errors (read)))

            (while (not (member player-1 '(1 2 3 4)))
                (progn
                    (terpri)
                    (princ "Please type a number between 1-4: ")
                    (setq player-1 (ignore-errors (read)))
                )
            )
        )
    )

    (terpri)
    (princ "You chose: ")
    (princ player-1)
    (terpri)
    (update-board board "X" (- player-1 1) )
)

; 6 (player-2-turn) - Handles console I/O for player 2's turn, currently unused, see other notes section.
; Turning the game back to a 2-player-game can be done in (connect-3-game).

(defun player-2-turn(board)

    (terpri)
    (princ "It is player 2s turn (X)")
    (terpri)
    (princ "Which column do you want to place your piece in? 1-4: ")
    (setq player-2 (ignore-errors (read)))

    ; Confirm input is a matching column
    (while (not (member player-2 '(1 2 3 4)))
        (progn
            (terpri)
            (princ "Please type a number between 1-4: ")
            (setq player-2 (ignore-errors (read)))
        )
    )

    ; Confirm input is not full
    (while (not (string= " " (nth (- player-2 1) board)))
        (progn
            (terpri)
            (princ "That column is full. Please choose another spot: ")
            (setq player-2 (ignore-errors (read)))

            (while (not (member player-2 '(1 2 3 4)))
                (progn
                    (terpri)
                    (princ "Please type a number between 1-4: ")
                    (setq player-2 (ignore-errors (read)))
                )
            )
        )
    )

    (terpri)
    (princ "You chose: ")
    (princ player-2)
    (terpri)
    (update-board board "O" (- player-2 1) )
)

; 7 (get-moves) - Same as actions. Returns a list of integers for the columns that the AI can place a mark.

(defun get-moves(board)
    (defun get-moves-helper(board i)
        (cond
            ((= i 4) nil)
            ((string= (car board) " ")
                (cons i (get-moves-helper (cdr board) (+ i 1))))
            (t (get-moves-helper (cdr board) (+ i 1)))
        )
    )
    (get-moves-helper board 0)
)

; 8 (simulate-turn) - Same as results. Creates a new list for the resulting game state based on an action.
;   It was required to be distinct from update-board, since it should not modify the original argument while
;   running through minimax. It still uses the original update-board function, just updates a copy and returns that instead.

(defun simulate-turn(board turn isX)
    (update-board (copy-tree board) (if isX "X" "O") turn)
)

; 9 (terminal-test) - Tests to see if the board is in a terminal state. Returns t or nil.

(defun terminal-test(board player)
    ; Possible Terminal States:
    ; 1. Whole board is filled (no spaces left)
    ; 2. 3 in a row, i, i+1, i+2
    ;       i is the first on the left
    ; 3. 3 in a column, i, i+4, i+8
    ;       i is the "top" element
    ; 4. 3 diagonal forward, i, i+5, i+10
    ;       i is the "top left" element
    ; 5. 3 diagonal backward, i, i+3, i+6
    ;       i is the "top right" element
    ; If none of these, returns nil (Not a terminal state)
   
    (catch 'result
        ; Case for if board is filled
        (if (not (member " " board))
            (throw 'result t)
        
            (progn
                ; Cases for 3 in a row
                (dotimes (i 16)
                    (if (= (mod i 4) 0)
                        (progn
                            (if (and 
                                    (string= (nth i board) (nth (+ i 1) board)) 
                                    (string= (nth (+ i 1) board) (nth (+ i 2) board))
                                    (string= (nth i board) player)
                                    (string= (nth (+ i 1) board) player)
                                    (string= (nth (+ i 2) board) player)
                                )
                                (throw 'result t)
                            )
                            (if (and 
                                    (string= (nth (+ i 1) board) (nth (+ i 2) board))  
                                    (string= (nth (+ i 2) board) (nth (+ i 3) board))
                                    (string= (nth (+ i 1) board) player)
                                    (string= (nth (+ i 2) board) player)
                                    (string= (nth (+ i 3) board) player)
                                )
                                (throw 'result t)
                            )
                        )
                    )
                )

                ; Cases for 3 in a column
                (dotimes (i 8)
                    (if (and 
                            (string= (nth i board) (nth (+ i 4) board))  
                            (string= (nth (+ i 4) board) (nth (+ i 8) board))
                            (string= (nth i board) player)
                            (string= (nth (+ i 4) board) player)
                            (string= (nth (+ i 8) board) player)
                        )
                        (throw 'result t)
                    )
                )

                ; Cases for 3 diagonal forwards
                (dotimes (i 6)
                    (if (= (mod i 4) 0)
                        (progn
                            (if (and 
                                    (string= (nth i board) (nth (+ i 5) board))  
                                    (string= (nth (+ i 5) board) (nth (+ i 10) board))
                                    (string= (nth i board) player)
                                    (string= (nth (+ i 5) board) player)
                                    (string= (nth (+ i 10) board) player)
                                )
                                (throw 'result t)
                            )
                            (if (and 
                                    (string= (nth (+ i 1) board) (nth (+ i 6) board))  
                                    (string= (nth (+ i 6) board) (nth (+ i 11) board))
                                    (string= (nth (+ i 1) board) player)
                                    (string= (nth (+ i 6) board) player)
                                    (string= (nth (+ i 11) board) player)
                                )
                                (throw 'result t)
                            )
                        )
                    )
                )

                ; Case for diagonal backwards
                (dotimes (i 5)
                    (if (= (mod i 4) 0)
                        (progn
                            (if (and 
                                    (string= (nth (+ i 2) board) (nth (+ i 5) board))  
                                    (string= (nth (+ i 5) board) (nth (+ i 8) board))
                                    (string= (nth (+ i 2) board) player)
                                    (string= (nth (+ i 5) board) player)
                                    (string= (nth (+ i 8) board) player)
                                )
                                (throw 'result t)
                            )
                            (if (and 
                                    (string= (nth (+ i 3) board) (nth (+ i 6) board))  
                                    (string= (nth (+ i 6) board) (nth (+ i 9) board))
                                    (string= (nth (+ i 3) board) player)
                                    (string= (nth (+ i 6) board) player)
                                    (string= (nth (+ i 9) board) player)
                                )
                                (throw 'result t)
                            )
                        )
                    )
                )
                ; If the program reaches this point, no solution/terminal state has been found
                ; and will return nil.

                (throw 'result nil)
            )
        )
    )
)

; 10 (utility) - Returns a utility value based on the terminal state.
;   Checks wins/losses for both "X" and "O"
;   Takes in the board, and a boolean for isX,
;   because the AI will use the Utility function for "swapping" turns
;   Returns 1 for win for the "perspective" of the simulated player
;   Returns -1 for the loss for the "perspective" of the simulated player
;   Returns 0 if neither

(defun utility(board)
    (if (terminal-test board "O") 1
        (if (terminal-test board "X") -1 0)
    )
)

; 11 (minimax) - Our minimax algorithm that uses all of the AI functions.
;   AI needs to use its own AI function/algorithm to simulate possible opponent turns
;   For this reason, we need a boolean as an argument for the AI to determine if its 
;   simulating "player1" (isX = true)
;   Or if its determining its own next move (isX = false)

(defun minimax(board isX)
    (let ((maxi -99) (mini 99) (current nil))
        (catch 'result
            (progn
                (if (or (terminal-test board "X") (terminal-test board "O"))
                    (throw 'result (utility board))
                )

                (if (not isX)
                    (progn
                        (setq maxi -99)
                        (dolist (turn (get-moves board))
                            (progn
                                (setq current (minimax (simulate-turn board turn nil) nil))
                                (setq maxi (max current maxi))
                            )
                        )
                        (throw 'result maxi)
                    )
                )
                
                (if (isX)
                    (progn
                        (setq mini 99)
                        (dolist (turn (get-moves board))
                            (progn
                                (setq current (minimax (simulate-turn board turn t) t))
                                (setq mini (min current mini))
                            )
                        )
                        (throw 'result mini)
                    )
                )
            )
        )
    )
)

; 12 (AI-turn) - A parent handler for the minimax algorithm to return a move back to the main game loop.

(defun AI-turn(board)
    (let ((moves (get-moves board)) (current nil) (best-move nil) (best-value -99))
        (dolist (move moves)
            (progn
                (setq current (minimax (simulate-turn board move nil) nil))
                (if (> current best-value)
                    (progn
                        (setq best-value current)
                        (setq best-move move)
                    )
                )
            )
        )
        (update-board board "O" best-move)
    )
)