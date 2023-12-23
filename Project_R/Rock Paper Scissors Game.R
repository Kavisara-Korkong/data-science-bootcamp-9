## HW 1
# 1st version: "Rock Paper Scissors Game" for 1 time playing until computer or user win

game <- function() {
    # Print the welcome message
    print("Hello, welcome to the Rock Paper Scissors game!")
    flush.console()

    # Get the username
    username <- readline("What's your name: ")

    # Print greeting meassage
    print(paste("Let's play Rock Paper Scissors!", username))
    flush.console()

    # create variables
    score <- 0
    while (score == 0) {
        hands <- c("rock", "paper", "scissors")
        com_hand <- sample(hands,1)
        user_hand <- tolower(readline("Your's choice (rock, paper, scissors): " ))

        # in case user type wrong
        if (user_hand != "rock" & user_hand != "paper" & user_hand != "scissors") {
            print(paste("There is no", user_hand))
        } else {
            print(paste("Computer's choice:", com_hand))
            # create condition
            if ((user_hand == "rock" & com_hand == "paper") | (user_hand == "paper" & com_hand == "scissors") | (user_hand == "scissors" & com_hand == "rock")) {
                score <- -1
                print("you lose")
            } else if ((user_hand == "rock" & com_hand == "scissors") | (user_hand == "paper" & com_hand == "rock") | (user_hand == "scissors" & com_hand == "paper")) {
                score <- 1
                print("you win")
            } else {
                score <- 0
                print("you draw")
                flush.console()
                print("Let's try again")
                flush.console()
            }
        }
    }
}

# 2nd version: "Rock Paper Scissors Game", break when score of computer or user reach 5

game_2 <- function() {
    # Print the welcome message
    print("Hello, welcome to the Rock Paper Scissors game!")
    flush.console()

    # Get the username
    username <- readline("What's your name: ")

    # Print greeting meassage
    print(paste("Let's play Rock Paper Scissors!", username))
    flush.console()

    # set score = 0
    score_user <- 0
    score_com <- 0

    # create variables
    hands <- c("rock", "paper", "scissors")

    while (score_user < 5 & score_com < 5) {
    com_hand <- sample(hands,1)
    user_hand <- tolower(readline("Your's choice (rock, paper, scissors): " ))

    # return value and update score
    if (user_hand != "rock" & user_hand != "paper" & user_hand != "scissors") {
        print(paste("There is no", user_hand))
    } else  {
        print(paste("Computer's choice:", com_hand))
        flush.console()

        # create condition
        if ((user_hand == "rock" & com_hand == "paper") | (user_hand == "paper" & com_hand == "scissors") | (user_hand == "scissors" & com_hand == "rock")) {
                score_com <- score_com + 1
                print("you lose")
                print(paste("Computer's score : ", score_com))
                print(paste("Your score : ", score_user))
                flush.console()
            } else if ((user_hand == "rock" & com_hand == "scissors") | (user_hand == "paper" & com_hand == "rock") | (user_hand == "scissors" & com_hand == "paper")) {
                score_user <- score_user + 1
                print("you win")
                print(paste("Computer's score : ", score_com))
                print(paste("Your score : ", score_user))
                flush.console()
            } else {
                print("you draw")
                print(paste("Computer's score : ", score_com))
                print(paste("Your score : ", score_user))
                flush.console()
            }
            }
    }

    # summary the result
    if (score_user == 5) {
        print("You win this round")
    } else if (score_com == 5) {
        print("You lose this round")
    }
}
