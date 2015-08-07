%Agent for program 3, progress requirements to grab gold, move around some,
%and keep track of position.

:- dynamic([
	agent_loc/1,
	agent_direction/1,
	agent_inside/1,
	agent_status/1,		%Is the agent alive or dead?
	agent_gold_count/1,
	agent_arrow/1,
	agent_points/1,
	safe_squares/1,		%List of the squares that are already known as safe
	previous_loc/1,		%List of the squares that the agent has already been to
	move_count/1
	]).


% init_agent: Initializes the agent by retracting any values about the agent in the knowledge base
% and then asserting the proper values for the agent so that it is ready to start exploring.

init_agent:-

  retractall(agent_loc(_)),
  retractall(agent_direction(_)),
  retractall(agent_inside(_)),
  retractall(agent_status(_)),
  retractall(agent_gold_count(_)),
  retractall(agent_arrow(_)),
  retractall(agent_points(_)),
  retractall(safe_squares(_)),
  retractall(previous_loc(_)),
  retractall(move_count(_)),

  assert(agent_loc(square(1,1))),
  assert(agent_direction(0)),
  assert(agent_inside(yes)),
  assert(agent_status(alive)),
  assert(agent_gold_count(0)),
  assert(agent_arrow(1)),
  assert(agent_points(0)),
  assert(safe_squares([square(1,1)])),
  assert(previous_loc(square(1,1))).


% restart_agent: restarts the agent, and all of the information being kept track of, to what
% it would be at the starting point.

restart_agent:-

  retractall(agent_loc(_)),
  retractall(agent_direction(_)),
  retractall(agent_inside(_)),
  retractall(agent_status(_)),
  retractall(agent_gold_count(_)),
  retractall(agent_arrow(_)),
  retractall(agent_points(_)),
  retractall(safe_squares(_)),
  retractall(previous_loc(_)),

  assert(agent_loc(square(1,1))),
  assert(agent_direction(0)),
  assert(agent_inside(yes)),
  assert(agent_status(status)),
  assert(agent_gold_count(0)),
  assert(agent_arrow(1)),
  assert(agent_points(0)),
  assert(safe_squares([square(1,1)])),
  assert(previous_loc(square(1,1))).

% run_agent(Percept,Action):  has the agent attempt to grab gold, and then returns the action
% as Action based on the given percept.

%percept is in the format: [Stench,Breeze,Glitter,Bump,Scream,_,_]

%Given a percept from the simulator, decide on what the next move should be.
%un_agent(Percept, goforward).


%Things to be true if the agent should grab gold
%Always try to grab gold first
run_agent([_,_,yes,_,_,_,_], grab):-
	%agent_loc(square(X,Y)),
	%add_to_safe(square(X,Y)),
	agent_gold_count(Gold),
	Gold2 is (Gold + 1),
	retractall(agent_gold_count(Gold)),
	assert(agent_gold_count(Gold2)).
	
	
	%reverse the safe_squares list so that it can be followed to the starting point
	%safe_squares(List),
	%reverse(List,Rlist),
	%retractall(safe_squares(List)),
	%assert(safe_squares(Rlist)).


%Things to be true if the agent should move forward.
%True if there is no breeze,stench, glitter or bump
run_agent([no,no,no,no,_,_,_], goforward):-
	
	
	agent_loc(square(X,Y)),	
	agent_direction(Angle),
	next_location(X,Y,Angle,X1,Y1),
	retractall(agent_loc(square(X,Y))),
	assert(agent_loc(square(X1,Y1))),
	
	nl,write(square(X,Y)),nl,
	
	safe_squares(List),
	append(List,[square(X1,Y1)],Newlist),
	retract(safe_squares(List)),
	assert(safe_squares(Newlist)),
	
	
	nl,write('hi'),nl.
	


%run_agent([_,_,_,yes,_,_,_], Action):-
%		agent_direction(Angle),		
%		bump_choice(Angle, square(X,Y), Action).

%bump_choice(0,Action):-
	
run_agent([_,_,_,yes,_,_,_],turnleft):-


	agent_direction(Angle),
	Angle2 is (Angle + 90) mod 360,
	retractall(agent_direction(Angle)),
	assert(agent_direction(Angle2)).


run_agent([_,_,_,yes,_,_,_],turnright):-

	agent_loc(square(X,Y)),
	%(X < 8 ; (X = 8, agent_direction(D), (D = 180 ; D = 270))),
	agent_direction(Angle),
	Angle2 is (Angle + 270) mod 360,
	retractall(agent_direction(Angle)),
	assert(agent_direction(Angle2)).

	
%Things to be true if the agent should shoot
run_agent([yes,_,_,_,_,_,_], shoot):-
	agent_arrow(A),
	A > 0, !,
	A1 is (A-1),
	retractall(agent_arrow(A)),
	assert(agent_arrow(A1)).
		
%Climbs out of the cave if the agent has at least one piece of gold, is alive
%and is at the starting square (1,1).
run_agent([_],climb):-
	agent_gold_count(Gold),
	Gold > 0,
	agent_status(S),
	S = alive,
	agent_loc(square(X,Y)),
	X = 1,
	Y = 1.

%Picks a random action if confronted by a breeze
run_agent([_,yes,no,no,_,_,_],Action):-
	random_member(Action, [goforward, turnleft, turnright, goforward]).
	





%Things to be true if the agent should turn left
%Only aplicable for the random choosing from a breeze
%run_agent(Percept, turnleft):-




%Things to be true if the agent should turn right
%only applicable if the agent has run into a wall or is chosen from feeling a breeze
%run_agent(Percept, turnright):-

%Things to be true if the agent should climb
%run_agent(Percept, climb):-






%tracesteps(Action): The agent looks for the action that will result in the agent moving towards
%the last square it was in.

%tracesteps(Action, Curr(X,Y)):-


%addToSafe(Square(X,Y)): adds the given square to the list of known safe squares
add_to_safe(square(X1,Y1)):-

		%only add to list if square(X,Y) is not already in the safe_squares
		safe_squares(List),
		%not_member(square(X,Y),List),
		%nl,write('after not'),nl,
		append(List,[square(X1,Y1)],Newlist),
		retract(safe_squares(List)),
		assert(safe_squares(Newlist)).




% moveforward(Bump): Moves the agent forward based on the direction it was facing, as long as
% it doesn't run into a wall.

moveforward(no):-
	agent_direction(Angle),
	agent_loc(X,Y),
	next_location(X,Y,Angle,X1,Y1),
	retract(agent_loc(X,Y)),
	assert(agent_loc(X1,Y1)).

moveforward(yes).


% next_location(X,Y,Orientation,X1,Y1): returns new coordinates X1,Y1
% after turning left or right by 90 degrees

next_location(X,Y,0,X1,Y) :-
  X1 is (X + 1).
 

next_location(X,Y,90,X,Y1) :-
  Y1 is (Y + 1).


next_location(X,Y,180,X1,Y) :-
  X1 is (X - 1).


next_location(X,Y,270,X,Y1) :-
  Y1 is (Y - 1).
  



member(E, [E | Tail]).
member(E, [X | Tail]):-
		member(E, Tail).
		
		
not_member(_, []):- !.
not_member(X, [H | Tail]):-
	X \= H,
	not_member(X, Tail).


%append([], List, List).
%append([H1|T1], List2, [H1|CombTail]):-
%	append(T1, List2, CombTail).

%reverse([],[]).
%reverse([H1|T1], List2):-
%	append(T2,[H1],List2),
%	reverse(T1,T2).
