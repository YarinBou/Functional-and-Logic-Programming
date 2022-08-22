-module(hw5).
-export([create_indices/1]).
-export([ex_map/2]).
-export([fizzbuzz/1]).
-export([concurrent_filter/2, concurrent_helper/3]).
% Q1
create_indices(0) -> [0];
create_indices(N) -> create_indices(N - 1) ++ [N].
% Q2
ex_map(F, L) ->
    % The list start with zero index.
    ListOfIndexes = create_indices(length(L)-1),
    Fun = fun(Tuple) -> {Value, Index} = Tuple, F(Value, Index) end,
    lists:map(Fun, lists:zip(L,ListOfIndexes)).
% Q3
fizzbuzz(N) ->
    [_|T] = create_indices(N),
    Convertor = fun(Idx) ->
        % rem = Integer remainder of X/Y
            if
                %  If the index of the item is divisible by both 3 and 5, then the output will be the atom fizzbuzz.
                (Idx rem 3 == 0) and (Idx rem 5 == 0) -> fizzbuzz;
                % If the index of the item is divisible by 3 then the output will be the atom fizz.
                Idx rem 3 == 0 -> fizz;
                % If the index of the item is divisible by 5 then the output will be the atom buzz.
                Idx rem 5 == 0 -> buzz;
                % In all other cases, the output will be the index.
                true -> Idx
            end
        end,
    lists:map(Convertor, T).
% Q4
concurrent_filter(F, L) ->
    spawn(hw5, concurrent_helper, [F, L, self()]),
    receive
        ListOutput -> ListOutput
    end.
% base case of the recursion.
concurrent_helper(_, [], ParentPID) -> ParentPID ! [];
concurrent_helper(F, L, ParentPID) ->
    [H|T] = L,
    spawn(hw5, concurrent_helper, [F, T, self()]),
    % flag - does the first elem should be in the list according to the function.
    KeepHead = F(H),
    receive
        ListOutput ->
            if
                % add the element to the list
                KeepHead -> ParentPID ! ([H] ++ ListOutput);
                % del the element to the list
                not KeepHead -> ParentPID ! ListOutput
            end
    end.
