%Bin_Li bli4 796669
%Fillin Puzzle

% The mechanism is to convert puzzle file and wordlist file into two lists.
% Bind puzzle with Free Logical Variables and generate slots from the raw puzzle
% by rows and columns and then append them into together.
% After that calculate the how many words each slots could match.
% Pick the slot with least match and bind it with wordlist.
% Till the Slot list is empty, and the program will output the solved puzzle. 
:- ensure_loaded(library(clpfd)).

% main function-the entraince of the program.
main(PuzzleFile, WordlistFile, SolutionFile) :-
	read_file(PuzzleFile, Puzzle),
	read_file(WordlistFile, Wordlist),
	valid_puzzle(Puzzle),
	solve_puzzle(Puzzle, Wordlist, Solved),
	print_puzzle(SolutionFile, Solved).
	

% read file and keep data in list.
read_file(Filename, Content) :-
	open(Filename, read, Stream),
	read_lines(Stream, Content),
	close(Stream).

read_lines(Stream, Content) :-
	read_line(Stream, Line, Last),
	(   Last = true
	->  (   Line = []
	    ->  Content = []
	    ;   Content = [Line]
	    )
	;  Content = [Line|Content1],
	    read_lines(Stream, Content1)
	).

read_line(Stream, Line, Last) :-
	get_char(Stream, Char),
	(   Char = end_of_file
	->  Line = [],
	    Last = true
	; Char = '\n'
	->  Line = [],
	    Last = false
	;   Line = [Char|Line1],
	    read_line(Stream, Line1, Last)
	).

% output the solved puzzle into file.
print_puzzle(SolutionFile, Puzzle) :-
	open(SolutionFile, write, Stream),
	maplist(print_row(Stream), Puzzle),
	close(Stream).


print_row(Stream, Row) :-
	maplist(put_puzzle_char(Stream), Row),
	nl(Stream).

put_puzzle_char(Stream, Char) :-
	(   var(Char)
	->  put_char(Stream, '_')
	;   put_char(Stream, Char)
	).

% check the puzzle is valid.
valid_puzzle([]).
valid_puzzle([Row|Rows]) :-
	maplist(samelength(Row), Rows).

samelength([], []).
samelength([_|L1], [_|L2]) :-
	same_length(L1, L2).
	
% solve_puzzle function: 
% @params: Puzzle: list
%          Wordlist: list
% @return: Solved Puzzle 
% 1.convert puzzle row into variables, and then rows.
% 2.convert rows of variables into slots.
% 3.fill in the slots with words.

solve_puzzle(Puzzle, WordList, SolvedPuzzle) :-
    puzzle_log(Puzzle, SolvedPuzzle),
    puzzle_slots(SolvedPuzzle, Slots),
	filling(Slots, WordList).

	
% puzzle to variables function: 
% @params: Puzzle rows by '#' and  '_'
% @return: Rows made by free logical variables.
% convert puzzle row into variables.
	
puzzle_log([],[]).
puzzle_log(Originalpuzzle,Logicalpuzzle):-
	maplist(rows_var,Originalpuzzle,Logicalpuzzle).

% Convert rows of puzzle into slots.
rows_var([], []).
rows_var(Originalrow, Logicalrow) :-
    maplist(fillin_logicalvars, Originalrow, Logicalrow).

% Convert characters of a line into slots.
fillin_logicalvars(Eachchar, _):-
	Eachchar == '_'.
fillin_logicalvars(Eachchar, Eachchar).


% Generate slots from free-variabled and eliminate 
% the slots with invalid length(less than 2 characters).
puzzle_slots(SolvedPuzzle, Slot) :-
    slots_rows(SolvedPuzzle, SlotsRow),
    exclude(invalid, SlotsRow, NewSlotRow),
    transpose(SolvedPuzzle, TransposedPuzzle),
    slots_rows(TransposedPuzzle, SlotsColumn),
    exclude(invalid, SlotsColumn, NewSlotColumn),
    append(NewSlotRow, NewSlotColumn, Slot).

invalid(List) :-
    length(List, LengthList),
    LengthList < 2.

slots_rows([], []).
slots_rows([Row|RestRows], SlotsRows) :-
    slot_row(Row, [],RowSlot),
    append(RowSlot, MoreSlots, SlotsRows),
    slots_rows(RestRows, MoreSlots).

%  When the row is empty and the current slot is empty, 
%  And the predicate should stop and return a empty Slots.
slot_row([], [], []).
%  When the row is empty and the CurrentSlot is not empty,
%  The Slots should include the temperarily kept slot. 
slot_row([], CurrentSlot, [CurrentSlot]) :-
    CurrentSlot \= [].
	
% When encounter a '#', it's necessary to check whether the 
% current slot is more than 2 variables.
% If it is, the slot should be included.
% Otherwise, the slot should be dropped. 
slot_row([H|T], CurrentSlot, [CurrentSlot|RestSlots]) :-
    H == '#',
    slot_row(T, [], RestSlots).
slot_row([H|T], CurrentSlot, Slots) :-
    H \== '#',
    append(CurrentSlot, [H], NewSlot),
    slot_row(T, NewSlot, Slots).

% Filling the best slots with matchable word, and recursively filling
% the rest words till the the slot-list is empty. 
filling([], []).
filling([Slot|RestSlots], WordList) :-  
    matchingslot(Slot, WordList, 0,Matches),
    best_slot(RestSlots, WordList, Matches,  Slot, BestSelectedSlot),
    exclude(\=(BestSelectedSlot), WordList, MatchingWords),
    member(SelectedWord, MatchingWords),
    BestSelectedSlot = SelectedWord,
    include(\=(SelectedWord), WordList, Words_rest),
    exclude(==(BestSelectedSlot), [Slot|RestSlots], Slots_rest),
    filling(Slots_rest, Words_rest).


% Trasverse each slot and count how many words it matches
% The CurrentBestSlot and Least_match_count works as accumulators to
% keep the best slot with least matches.
best_slot([], _, _, BestSelectedSlot, BestSelectedSlot).
best_slot([Slot|RestSlots], WordList, Least_match_count, 
    CurrentBestSlot, BestSelectedSlot) :-
    matchingslot(Slot, WordList,0, Matches),
    (Matches < Least_match_count ->
        CurrentBestSlot1 = Slot,
        Least_match_count1 = Matches
    ;   CurrentBestSlot1 = CurrentBestSlot,
        Least_match_count1 = Least_match_count
    ),
    best_slot(RestSlots, WordList, Least_match_count1, 
        CurrentBestSlot1, BestSelectedSlot).

% Use tail-recurse to count how many words a slot could match.
% @Params Single Slot, Wordlist.
% @Number of word the slot could match.
matchingslot(_, [], Acc_match, Acc_match).
matchingslot(Slot, [Word|Words], Acc_match, Matches) :-
    (Slot \= Word ->
        Acc_match1 = Acc_match
    ;   Acc_match1 = Acc_match + 1
    ), matchingslot(Slot, Words, Acc_match1, Matches).




















