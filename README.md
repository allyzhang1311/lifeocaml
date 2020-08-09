
# Instructions

Live the life of your dreams (or your nightmares) with
the Game of LIFE! Here are some basic instructions to
get you started.

## Input syntax
- `spin`: Move forward a random (1-10) number of spaces.
  Note that if you pass a stop square, you will have to
  stop there.
- `pay` [`player_name` OR `bank`] [`amt`]: Transfer some amount
  money from yourself to another player or the bank.
- `take` [`player_name` OR `bank`] [`amt`]: Transfer some amount
  money from another player or the bank to yourself.
- `borrow` [`amt`]: Take out a loan from the bank. The bank only
  loans in $20,000 multiples, and charges $5,000 interest
  on each one.
- `return` [`amt`]: Pay back a loan. This can only be done
  in $25,000 multiples, for the reason given above.
- `insure home`: Buy home insurance. The cost depends
  on your house's value.
- `insure car`: Buy car insurance. Always $10,000.
- `invest` [`1-10`]: Buy stock in one of the ten spin numbers.
  Always $50,000. You can have at most 1 stock, unless you
  land on a special square. 
- `view` [`player_name`]: Prints information on given player.
- `view next`: Print your position and the next 10 squares.
- `view global`: View all players' positions and balances.
- `help`: Print this message.
- `done`: Finish your turn.
- `exit`: Quit the game.

## Gaining/losing money
- Passing a Payday square automatically adds your salary
to your balance.
- Landing on a LIFE tile square automatically adds a LIFE
tile to your list. These are worth money, but you won't know
how much until the game ends!
- When any player spins a number you have invested in,
you will automatically gain $10,000.
- If a square directs you to collect/pay, then input the
appropriate command to the bank, unless...
- If there is a career listed on the square, and if a
player has that career, pay them and not the bank!
- You can gain money with your career special effect.
Remember it, because it's up to YOU to invoke it!

## Winning
Once all players have retired, your LIFE tiles will be
revealed along with their cash rewards. The player with
the highest total amount of money wins!
