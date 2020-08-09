(* TYPES *)

type career = {
  name : string;
  effect : string;
  degree : bool;
}

type house = {
  name : string;
  desc : string;
  price : int;
  insurance : int;
}

type salary = {
  amt : int;
  taxes : int;
  degree : bool;
}

let get_career_name (c:career) = c.name
let get_house_name (c:house) = c.name
let save_of_salary (s) =
  ("{\n\"amt\": \"" ^ (string_of_int s.amt) ^ "\",\n"
   ^ "\"taxes\": \"" ^ (string_of_int s.taxes) ^ "\",\n"
   ^ "\"degree\": \"" ^ (string_of_bool s.degree) ^ "\"\n}")

type stock = int

type tile = {
  desc : string;
  amt: int;}

let save_of_tile t =
  ("{\n\"desc\": \"" ^ t.desc ^ "\",\n"   
   ^ "\"amt\": \"" ^ (string_of_int t.amt) ^ "\"\n}")

type family_mem = Spouse | Son | Daughter

let save_of_family f =
  match f with
  | Spouse -> "spouse"
  | Son -> "son"
  | Daughter -> "daughter"

type stop_square = 
  | Marriage
  | House
  | Job
  | Sell_house
  | Retire

type switch_square = 
  | Career
  | Salary

type square =
  | Start
  | Payday
  (* description, target career if any *)
  | Loss of string * string option
  | Gain of string
  | Life_tile of string
  | Baby of string * family_mem
  | Switch of string * switch_square
  | Stop of stop_square
  | Stock of string

type board = square array

let format_career fmt (career : career option) =
  match career with 
  | None -> ANSITerminal.(print_string [blue] "Career: none")
  | Some c ->
    ANSITerminal.(printf [blue] "Career: %s\nSpecial effect: %s"
                    c.name c.effect)


let format_house fmt (house : house option) =
  match house with 
  | None -> ANSITerminal.(print_string [red] "House: none")
  | Some h ->
    ANSITerminal.
      (printf [red]
         "House: %s\nDescription: %s\nPrice: $%d\nInsurance: $%d"
         h.name h.desc h.price h.insurance)


let format_salary fmt (salary : salary option) =
  match salary with 
  | None -> ANSITerminal.(print_string [green] "Salary: none")
  | Some s ->
    ANSITerminal.
      (printf [green]
         "Salary: $%d\nTaxes due: $%d" s.amt s.taxes)


let format_family_lst fmt lst =
  let rec helper lst acc_sp acc_so acc_d = 
    match lst with 
    | [] -> Format.fprintf fmt "Family: %d spouse, %d son(s), %d daughter(s)"
              acc_sp acc_so acc_d
    | h::t -> (match h with
        | Spouse -> helper t (acc_sp+1) acc_so acc_d
        | Son -> helper t acc_sp (acc_so+1) acc_d
        | Daughter -> helper t acc_sp acc_so (acc_d+1)
      )
  in helper lst 0 0 0

let format_tile fmt tile =
  Format.fprintf fmt "$%d: %s" tile.amt tile.desc

let format_stocks fmt stocks = 
  let rec print_list (intls:int list) =
    (match intls with
     |[] -> ANSITerminal.(print_string [] "");
     |h::t -> ANSITerminal.(printf [cyan] "%d" h);
       ANSITerminal.(printf [cyan] " "; print_list t;)) in
  ANSITerminal.(printf [cyan] "Stocks: "); print_list stocks

let format_square fmt sq =
  match sq with 
  | Start ->
    ANSITerminal.(printf [magenta] "The start of the game!\n")
  | Life_tile(str) ->
    ANSITerminal.
      (printf [magenta] "%s\nYou get a LIFE tile.\n" str)
  | Baby(str,fam) ->
    ANSITerminal.(printf [magenta] "%s\nCongratulations! Your baby has been
added to the family. You also get a LIFE tile.\n" str)
  | Loss(str,career) -> (
      match career with 
      | Some c ->
        ANSITerminal.(printf [blue] "%s\nCareer: %s\n" str c)
      | None -> ANSITerminal.(printf [blue] "%s\n" str)
    )
  | Gain(str) ->
    ANSITerminal.
      (printf [magenta] "%s\n" str)
  | Payday ->
    ANSITerminal.
      (printf [green] "Payday!\n")
  | Stock(str) -> (match str with
      |"boom" -> ANSITerminal.(printf [magenta] "\nThe stock market booms!\n")
      |"crash" -> ANSITerminal.(printf [blue] "\nThe stock market crashes!\n")
      |_ -> ();
    )
  | Switch(str, x) -> (
      match x with 
      | Career -> ANSITerminal.(printf [blue] "%s\n" str)
      | Salary ->
        ANSITerminal.(printf [magenta] "%s\nYou may OPTIONALLY switch your
salary card with another player.\n" str)
    )
  | Stop(x) -> (
      ANSITerminal.(
        printf [red] "STOP SQUARE! ");
      match x with
      | Marriage -> ANSITerminal.(printf [red] "You got married!
Congrats on finding that special someone ;)
Your spouse has been added to the family.
You also get a LIFE tile.\n")
      | House ->
        ANSITerminal.(printf [red] "It's finally time to buy a house!\n")
      | Job ->
        ANSITerminal.
          (printf [red] "Your job hunt is over!
Time to put that
degree to good use!\n")
      | Sell_house -> 
        ANSITerminal.
          (printf [red] "You may OPTIONALLY
sell your house and buy a new one.\n") 
      | Retire -> ANSITerminal. (printf [red] "After a fulfilling life, it's
finally time to retire. Congrats!
If you have anything left to do,
do it now.\n")
    )

let careers = [
  {name = "ARTIST";
   effect = "FINALLY I CAN EAT!
Collect $10,000 from any player
who buys your art, i.e. spins a 1.";
   degree = false;};
  {name = "ENTERTAINER";
   effect = "THE NEXT CARLY RAE JEPSEN!
If two 8s, 9s, or 10s are spun
in a row, collect $100,000 on
your turn.";
   degree = false;};
  {name = "POLICE OFFICER";
   effect ="OCAML LIVES MATTER!
Collect $5,000 from any player
who speeds, i.e. spins a 10.";
   degree = false;};
  {name = "ACCOUNTANT";
   effect ="YOUR W-4 IS MINE!
When a player lands on a 'taxes
due' square, collect an extra 30%
of their taxes from the bank on
your turn.";
   degree = false;};
  {name = "COMPUTER SCIENTIST";
   effect ="GET IN THE QUEUE!
Collect $5000 from any player who
begs you to debug their OCaml code,
i.e. spins a 3 or 5.";
   degree = true;};
  {name = "BUSINESSPERSON";
   effect ="SNAKE IT UP!
Collect $20000 from any player who
lands on an accident square
(except yourself, of course).";
   degree = true;};
  {name = "TEACHER";
   effect = "THANKS FOR THE CURVE!
Collect $10000 if your current
LIFE tile count is a multiple
of 4.";
   degree = false;};
  {name = "ATHLETE";
   effect = "NOT ANOTHER LAP!
Spin again if you don't like
the square you land on (you
can do this twice at most).";
   degree = false;};
  {name = "DOCTOR";
   effect = "HOSPITAL FEES!
After position #40, add $3000
every time you collect money
from the bank.";
   degree = true;};
]

let houses = [
  {name = "TUDOR";
   desc = "From Tutime Realty.
Tufloors, tubaths, a tucar garage.
Perfect for tupeople with tukids
or more.";
   price = 180000;
   insurance = 45000;};
  {name = "COZY CONDO";
   desc = "From Sandwich Inn Realty.
This beautiful 2-level condominium
comes complete with gourmet kitchen
and rooftop garden.";
   price = 100000;
   insurance = 25000;};
  {name = "DUTCH COLONIAL";
   desc = "From Wooden Shoe Realty.
8 spacious rooms w/ study, den.
Solar-heated, wood-burning stoves,
solid oak floors.";
   price = 120000;
   insurance = 30000;};
  {name = "LOG CABIN";
   desc = "From Rod N. Realty.
Rustic charm in a woodland setting.
Loft w/ skylight, stone fireplace.
Near Lake Ketcheefishee.";
   price = 80000;
   insurance = 20000;};
  {name = "VICTORIAN";
   desc = "From Blithering Heights Realty.
Library, parlor, servants' quarters,
marble fireplaces, wraparound porch.";
   price = 200000;
   insurance = 50000;};
  {name = "MOBILE HOME";
   desc = "From Deals on Wheels Realty.
Aluminum-sided little beauty!
Great location, lovely view. 
Trailer hitch included.";
   price = 60000;
   insurance = 15000;};
  {name = "FARMHOUSE";
   desc = "From Euell B. Milken Realty.
Located on 50 rolling acres!
Garbanzo bean crops, prizewinning
pigs & dairy cows. Spacious barn 
w/ silo.";
   price = 160000;
   insurance = 40000;};
  {name = "BEACH HOUSE";
   desc = "From High Winds Realty.
Only 50 yards from Monsoon Beach.
Sun deck, boat dock, hurricane
wall. Hurry while it lasts!";
   price = 140000;
   insurance = 35000;};
  {name = "SPLIT-LEVEL (LITERALLY)";
   desc = "From Faultline Realty.
Was one level before the quake.
Now a real fixer-upper for
adventurous folks!";
   price = 10000;
   insurance = 40000;};
]

let salaries = [
  {amt = 25000;
   taxes = 7000;
   degree = false;};
  {amt = 30000;
   taxes = 10000;
   degree = false;};
  {amt = 40000;
   taxes = 15000;
   degree = false};
  {amt = 45000;
   taxes = 18000;
   degree = false;};
  {amt = 50000;
   taxes = 20000;
   degree = false;};
  {amt = 55000;
   taxes = 22000;
   degree = false;};
  {amt = 60000;
   taxes = 25000;
   degree = true;};
  {amt = 70000;
   taxes = 30000;
   degree = true;};
  {amt = 80000;
   taxes = 35000;
   degree = true;};
  {amt = 90000;
   taxes = 45000;
   degree = true;};
]

let stocks = [1;2;3;4;5;6;7;8;9;10]

let tiles = [
  {desc = "Climb Mt. Everest";
   amt = 50000;};
  {desc = "Run for President";
   amt = 150000;};
  {desc = "Compose a Symphony";
   amt = 120000;};
  {desc = "Cure the Common Cold";
   amt = 150000;};
  {desc = "Discover New Planet";
   amt = 75000;};
  {desc = "Swim the English Channel";
   amt = 50000;};
  {desc = "Win Dance Contest";
   amt = 50000;};
  {desc = "Run World Record Mile";
   amt = 50000;};
  {desc = "Invent New Ice Cream Flavor";
   amt = 50000;};
  {desc = "Write Great American Novel";
   amt = 100000;};
  {desc = "Open Health Food Chain";
   amt = 75000;};
  {desc = "Design New Company Logo";
   amt = 50000;};
  {desc = "Pulitzer Prize";
   amt = 150000;};
  {desc = "Nobel Peace Prize";
   amt = 180000;};
  {desc = "Solution to Pollution";
   amt = 100000;};
  {desc = "Create New Teaching Method";
   amt = 75000;};
  {desc = "Find New Energy Source";
   amt = 100000;};
  {desc = "Save Endangered Species";
   amt = 100000;};
  {desc = "Humanitarian Award";
   amt = 75000;};
  {desc = "Family Horse Wins Derby";
   amt = 100000;};
  {desc = "Paint a Masterpiece";
   amt = 150000;};
  {desc = "Build New Functional Programming Language";
   amt = 103110;};
  {desc = "Build a Better Mousetrap";
   amt = 150000;};
  {desc = "Toy Invention Sells Big";
   amt = 50000;};
  {desc = "Invent New Sport";
   amt = 75000;};
  {desc = "Invent New Sport";
   amt = 75000;};
]

(* BOARD *)

let full_board : board = [|
  Start;
  Payday;
  Loss ("Rent apartment. Pay $5,000.", None);
  Gain ("Your great-great-aunt kicks the bucket.
Collect $10,000.");
  Life_tile ("Make friends at your new job. Hey there Jim.");
  Gain ("New employee bonus! Collect $5,000.");
  Loss ("Take time off for vacation.
Pay half of your pay day.", None);
  Payday;
  Life_tile ("Visit Mom and Dad.");
  Loss ("Ski accident. Meh, bobsledding is
better anyway. Pay $5,000.", Some "DOCTOR");
  Gain ("Win marathon! Collect $10,000.");
  Life_tile ("Visit a museum.");
  Life_tile ("Tutor undergraduates out of the
goodness of your heart.");
  Payday;
  Loss ("Car rolls away. Them Ithaca hills...
Pay $15,000 if not insured.", None);
  Stop(Marriage);
  Loss ("Wedding reception. Pay $10,000.", None);
  Life_tile("Happy honeymoon!");
  Loss ("Upgrade to a foldable-screen iPhone.
Pay $10,000.",
        Some "COMPUTER SCIENTIST");
  Loss ("Car rolls away. Seriously, Ithaca is no joke.
Pay $10,000 if not insured.", None);
  Loss ("Attend business seminar. Mmm, networking.
Pay $10,000.", Some "BUSINESSPERSON");
  Loss ("Night school. Pay $20,000.", Some "TEACHER");
  Payday;
  Loss ("Taxes due.", Some "ACCOUNTANT");
  Gain ("Win lottery! Wow, who knew you
had so many 'old friends'? Collect $50,000.");
  Life_tile("Visit in-laws.");
  Stop(House);
  Payday;
  Switch ("Lose your job and start new career. Thanks, Obama.", Career);
  Baby ("Baby boy!", Son);
  Loss ("Furnish baby room. Pay $5,000.", Some "BUSINESSPERSON");
  Baby("Baby girl!", Daughter);
  Gain ("Win talent show. Collect $10,000.");
  Payday;
  Loss ("Buy the top-secret Amazon Echo prototype.
Alexa, what's my GPA? Pay $5000.", Some "BUSINESSPERSON");
  Loss ("50-yard-line seats at the big game.
Pay $20,000.", Some "ATHLETE");
  Baby("Baby girl!", Daughter);
  Loss ("Attend Avengers: Endgame red carpet premiere.
Dab on all your friends. Pay $5,000.", Some "ENTERTAINER");
  Loss ("House flooded. Pay $40,000 if not insured.", None);
  Baby ("Adopt a girl!", Daughter);
  Loss ("Buy 4K TV. Oooooh shiny.", Some "BUSINESSPERSON");
  Life_tile ("Read a new book.");
  Gain ("Business bigwig notices you! Collect $50,000.");
  Loss ("Post bail for your sketchy cousin.
Pay $5,000.", Some "POLICE OFFICER");
  Payday;
  Loss ("Car stolen! Pay $15,000 if not insured.", None);
  Loss ("Family physicals. Pay $5,000.", Some "DOCTOR");
  Switch ("Headhunted by a hot new company!", Salary);
  Baby("Baby boy!", Son);
  Payday;
  Baby("Baby girl!", Daughter);
  Loss ("Tree falls on house. Shoulda moved to Phoenix...
Pay $15,000 if not insured.", None);
  Stock ("boom");
  Life_tile ("Return lost wallet.");
  Switch ("You got a pay raise. Ohhh yeah baby.", Salary);
  Life_tile ("Run for mayor.");
  Life_tile ("Vote!");
  Baby("Baby boy!", Son);
  Payday;
  Loss ("Build a fancy gaming PC and make insufferable
Reddit posts about it. Pay $5,000.", Some "COMPUTER SCIENTIST");
  Loss ("Night school. Pay $20,000.", Some "TEACHER");
  Life_tile ("Learn CPR.");
  Loss ("Art auction. Pay $20,000.", Some "ARTIST");
  Payday;
  Stock ("boom");
  Loss ("Taxes due.", Some "ACCOUNTANT");
  Loss ("Tennis camp. Pay $25,000.", Some "ATHLETE");
  Loss ("Help found a startup. This is the one, guys,
I mean it! Pay $25,000.",
        Some "COMPUTER SCIENTIST");
  Payday;
  Stock ("bust");
  Stop(Sell_house);
  Loss ("Day care. Pay $5,000 per child.", Some "TEACHER");
  Gain ("Write bestselling biography of Happy Dave. Collect $80,000.");
  Payday;
  Baby ("Adopt a girl!", Daughter);
  Loss ("Family portraits. Say cheese!
Pay $35,000.", Some "ARTIST");
  Switch ("Fancy new corner office just for you!", Salary);
  Loss ("Buy new SUV. Vroom vroom boys.
Pay $25,000.", Some "BUSINESSPERSON");
  Gain ("Find buried treasure! Collect $80,000.");
  Loss ("Fund Kathy Zoner memorial. Pay $15,000.", Some "POLICE OFFICER");
  Payday;
  Life_tile ("Join health club.");
  Loss ("Taxes due.", Some "ACCOUNTANT");
  Loss ("Invest in Broadway play. I dreamed a dreeeam...
Pay $15,000.", Some "ENTERTAINER");
  Life_tile ("Have a family game night.");
  Loss ("Donate to Johnson Museum of Art.
Pay $25,000.", Some "ARTIST");
  Life_tile ("Start a recycling initiative.");
  Gain ("You win 'Are You Smarter than a 3110 Student?' Collect $95,000.");
  Loss ("Summer school. Pay $5,000 per child.", Some "TEACHER");
  Life_tile ("Learn American Sign Language.");
  Loss ("Buy lakeside cabin. Pay $20,000.", None);
  Payday;
  Loss ("Burglar! Pay $50,000.", None);
  Gain ("Win Nobel Prize. No, seriously. Collect $100,000.");
  Loss ("Buy home gym. Pay $30,000.", Some "ATHLETE");
  Stock ("bust");
  Loss ("Tornado hits house! #justmidwestthings
Pay $125,000 if not insured.", None);
  Payday;
  Baby ("Adopt a boy!", Son);
  Loss ("Lifesaving operation. Pay $25,000.", Some "DOCTOR");
  Loss ("Taxes due.", Some "ACCOUNTANT");
  Loss ("Buy sailboat. Pay $30,000.", Some "BUSINESSPERSON");
  Loss ("Sponsor golf tournament. Welcome to the 1%. Pay $35,000.",
        Some "ATHLETE");
  Switch ("Mid-life crisis, start new career.
No it's NOT because of the comb-over.", Career);
  Baby ("Baby boy!", Son);
  Gain ("Spin again if not in the lead.");
  Payday;
  Loss ("Get Game of Thrones leaks from shady insiders. Pay $80,000.",
        Some "ENTERTAINER");
  Life_tile ("Help the homeless.");
  Gain ("Spin again if not in the lead.");
  Loss ("Get a good ol' nip-tuck. Pay $100,000.", Some "DOCTOR");
  Loss ("College. Urgh. Pay $50,000 per child.", Some "TEACHER");
  Payday;
  Loss ("Taxes due.", Some "ACCOUNTANT");
  Gain ("Spin again if not in the lead.");
  Life_tile("Visit war memorial.");
  Loss ("Sponsor postmodern art exhibit. It's deeeep man.
Pay $125,000.", Some "ARTIST");
  Life_tile("Go fishing.");
  Gain ("Spin again if not in the lead."); 
  Loss ("Hire jockey for your racehorse. Welcome
to the 1%, seriously. Pay $65,000.", Some "ATHLETE");
  Life_tile("Go hiking.");
  Payday;
  Life_tile ("Plant a tree.");
  Gain ("Spin again if not in the lead."); 
  Life_tile ("Support wildlife fund.");
  Loss ("Donate to Cornell CS. Gates who? Pay $45,000.",
        Some "COMPUTER SCIENTIST");
  Life_tile ("You're a grandparent!");
  Payday;
  Loss ("Throw party for Tony award winners. Pay $35,000.",
        Some "ENTERTAINER");
  Loss ("Invest in Theranos. Pay $45,000.", Some "COMPUTER SCIENTIST");
  Gain ("Pension. Hey kids, get off my lawn!
Collect $20,000 * your spin.");
  Stop(Retire);
|]

let college_branch = [|
  Start;  
  Gain ("Scholarship! Collect $25,000.");
  Loss ("Buy books and supplies. Pay $5,000.", None);
  Life_tile ("Make new friends. But keep the old!");
  Gain ("Part-time job cleaning pipettes in Baker.
Collect $5,000.");
  Loss ("Sign up for a sketchy MATH 1920
tutoring service. Pay $5,000.", None);
  Loss ("Spring break! You, uh, let yourself go a bit. 
Pay $5,000.", None);
  Life_tile ("Dean's List!");
  Loss ("Your pals crash your car. Wow thanks a lot guys.
Pay $5,000 if not insured.", None);
  Life_tile ("Graduation day!");
  Stop(Job);
|]

(* For demoing only. *)
let full_board = [|
  Start;
  Payday;
  Loss ("Rent apartment. Pay $5,000.", None);
  Gain ("Your great-great-aunt kicks the bucket.
Collect $10,000.");
  Life_tile ("Make friends at your new job. Hey there Jim.");
  Gain ("New employee bonus! Collect $5,000.");
  Loss ("Take time off for vacation.
Pay half of your pay day.", None);
  Payday;
  Life_tile ("Visit Mom and Dad.");
  Loss ("Ski accident. Meh, bobsledding is
better anyway. Pay $5,000.", Some "DOCTOR");
  Stop(Marriage);
  Stock("bust");
  Stock("boom");
  Loss ("Invest in Theranos. Pay $45,000.", Some "COMPUTER SCIENTIST");
  Switch ("Mid-life crisis, start new career.
No it's NOT because of the comb-over.", Career);
  Baby("Baby girl!", Daughter);
  Stock("boom");
  Gain ("Win lottery! Wow, who knew you
had so many 'old friends'? Collect $50,000.");
  Stock("bust");
  Stop(House);
  Stock("boom");
  Switch ("Headhunted by a hot new company!", Salary);
  Switch ("Corner office just for you!", Salary);
  Loss ("Attend business seminar. Mmm, networking.
Pay $10,000.", Some "BUSINESSPERSON");
  Switch ("You got fired! Time to hit the books.", Career);
  Stop(Sell_house);
  Gain ("Pension. Hey kids, get off my lawn!
Collect $20,000 * your spin.");
  Stop(Retire);
|]

let min_players : int = 2
let max_players : int = 5
let board_length = Array.length(full_board) - 1

(* FORMATTERS *)

let format_career fmt (career : career option) =
  match career with 
  | None -> ANSITerminal.(print_string [blue] "Career: none")
  | Some c ->
    ANSITerminal.(printf [blue] "Career: %s\nSpecial effect: %s"
                    c.name c.effect)


let format_house fmt (house : house option) =
  match house with 
  | None -> ANSITerminal.(print_string [red] "House: none")
  | Some h ->
    ANSITerminal.
      (printf [red]
         "House: %s\nDescription: %s\nPrice: $%d\nInsurance: $%d"
         h.name h.desc h.price h.insurance)


let format_salary fmt (salary : salary option) =
  match salary with 
  | None -> ANSITerminal.(print_string [green] "Salary: none")
  | Some s ->
    ANSITerminal.
      (printf [green]
         "Salary: $%d\nTaxes due: $%d" s.amt s.taxes)


let format_family_lst fmt lst =
  let rec helper lst acc_sp acc_so acc_d = 
    match lst with 
    | [] -> Format.fprintf fmt "Family: %d spouse, %d son(s), %d daughter(s)"
              acc_sp acc_so acc_d
    | h::t -> (match h with
        | Spouse -> helper t (acc_sp+1) acc_so acc_d
        | Son -> helper t acc_sp (acc_so+1) acc_d
        | Daughter -> helper t acc_sp acc_so (acc_d+1)
      )
  in helper lst 0 0 0

let format_tile fmt tile =
  Format.fprintf fmt "$%d: %s" tile.amt tile.desc

let format_stocks fmt stocks = 
  let rec print_list (intls:int list) =
    (match intls with
     |[] -> ()
     |h::t -> ANSITerminal.(printf [cyan] "#%d " h);
       ANSITerminal.(printf [cyan] " "; print_list t;)) in
  ANSITerminal.(printf [cyan] "Stocks: ");
  match stocks with 
  | [] -> ANSITerminal.(printf [cyan] "none")
  | _ -> print_list stocks

(** [format_stop_square fmt sq] outputs a textual representation of [sq],
    a stop square, on [fmt]. *)
let format_stop_square fmt sq =
  ANSITerminal.(
    printf [red] "STOP SQUARE! ");
  match sq with
  | Marriage -> ANSITerminal.(printf [red] "You got married!
Congrats on finding that special someone ;)
Your spouse has been added to the family.
You also get a LIFE tile.\n")
  | House ->
    ANSITerminal.(printf [red] "It's finally time to buy a house!\n")
  | Job ->
    ANSITerminal.
      (printf [red] "Your job hunt is over!
Time to put that
degree to good use!\n")
  | Sell_house -> 
    ANSITerminal.
      (printf [red] "You may OPTIONALLY
sell your house and buy a new one.\n") 
  | Retire -> ANSITerminal. (printf [red] "After a fulfilling life, it's
finally time to retire. Congrats!
If you have anything left to do,
do it now.\n")

let format_square fmt sq =
  match sq with 
  | Start ->
    ANSITerminal.(printf [magenta] "The start of the game!\n")
  | Life_tile(str) ->
    ANSITerminal.
      (printf [magenta] "%s\nYou get a LIFE tile.\n" str)
  | Baby(str,fam) ->
    ANSITerminal.(printf [magenta] "%s\nCongratulations! Your baby has been
added to the family. You also get a LIFE tile.\n" str)
  | Loss(str,career) -> (
      match career with 
      | Some c ->
        ANSITerminal.(printf [blue] "%s\nCareer: %s\n" str c)
      | None -> ANSITerminal.(printf [blue] "%s\n" str)
    )
  | Gain(str) ->
    ANSITerminal.
      (printf [magenta] "%s\n" str)
  | Payday ->
    ANSITerminal.
      (printf [green] "Payday!\n")
  | Stock(str) -> (
      match str with
      |"boom" -> ANSITerminal.(printf [magenta] "Stock market boom!
You gain a random new stock.\n")
      |"bust" -> ANSITerminal.(printf [blue] "Stock market crash!
You lose a random stock.\n")
      |_ -> ();
    )
  | Switch(str, x) -> (
      match x with 
      | Career -> ANSITerminal.(printf [blue] "%s\n" str)
      | Salary ->
        ANSITerminal.(printf [magenta] "%s\nYou may OPTIONALLY switch your
salary card with another player.\n" str)
    )
  | Stop(x) -> format_stop_square fmt x

let format_board fmt (position, college) =
  Format.fprintf fmt "These are the squares around you (-5 to +10).\n";
  let start_pos = if position < 5 then 0 else position - 5 in
  let end_pos = if position > (board_length - 10) then board_length
    else position + 10 in
  for x = start_pos to end_pos do
    let board =
      if college && x < (Array.length college_branch) 
      then college_branch else full_board
    in
    if x = position then Format.fprintf fmt "->";
    Format.fprintf fmt "%d. %a" x format_square (board.(x))
  done